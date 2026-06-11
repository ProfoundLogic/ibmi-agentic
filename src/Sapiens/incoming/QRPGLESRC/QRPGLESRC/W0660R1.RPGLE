     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*Caller)
     H*****************************************************************
     H/TITLE W0660R1 - CALCULATED MANUAL PREMIUM SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM WILL DISPLAY ALL PAYROLL CLASS CODES SET UP FOR *
     H*  AN EMPLOYER IN A GIVEN FUND YEAR.  SELF-REPORTING ENTIRES    *
     H*  ENTERED ON A GIVEN MONTH AND YEAR IN THE FILE WASRP ARE USED *
     H*  ALONG WITH THE APPROPRIATE PAYROLL CLASS CODE RATES TO       *
     H*  CALCULATE THE SUM OF ALL MANUAL PREMIUM AMOUNTS FOR ALL      *
     H*  CLASS CODES IN THE MONTH AND YEAR REQUESTED.                 *
     H*                                                               *
     H*  CALLED BY:                                                   *
     H*   W0060R                                                      *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: KENNETH KENNEDY                                  *
     H*  CREATED:    12/11/92          PROJECT: WC186                 *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  18   ROLL UP REQUEST                                         *
     H*  19   ROLL DOWN REQUEST                                       *
     H*  20   INVALID ROLL                                            *
     H*  55   USED IN LOKUP TO DETERMINE IF CLASS CODE IS A DUPLICATE *
     H*  60   GENERAL USAGE: CHAIN, READ, ETC.                        *
     H*  61   USED TO INDICATE WHEN A FULL SCREEN HAS BEEN            *
     H*       INITIALIZED AND IS READY TO BE DISPLAYED.               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/17/24 C:344 Project: WC2952                                *
     H*                Expand scRT01 - scRT13 rate fields in display  *
     H*                file.                                          *
     H*                Changed all fields within the RT data structure*
     H*                to zoned 8.3.                                  *
     H*                Changed SRT to zoned 8.3.                      *
     H*                Changed XXAMRT to 8.3.                         *
     H*                                                               *
     H* 01/19/22 C:33  Project: WC2837                                *
     H*                Extend net rate conditional                    *
     H*                                                               *
     H* 04/07/17 C:062 Project 136WHF                                 *
     H*                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 06/06/14 C:302 Support Item 90979 WA099                       *
     H*                Use the calculated net rate only for           *
     H*                for client 136.                                *
     H*                                                               *
     H* 01/24/11 C:65  PROJECT: 139WPL/PRISS 117                      *
     H*                IF CREDIT EXPIRES ON SAME DAY IT IS EFFECTIVE  *
     H*                THEN DO NOT INCLUDE IT IN CALCULATION.         *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                ADD NEW HCN CREDIT INTO DISCOUNT FACTOR BEFORE *
     H*                MODIFIER AND PRINT A LINE ITEM IN THE DISCOUNT *
     H*                BOX.                                           *
     H*                                                               *
     H* 06/28/10 C:257 PROJECT: WC1912                                *
     H*                INSTALLMENT FEES ON S/R ACCOUNTS ->            *
     H*                CHECK THAT IF THE EXEMPT FLAG IS SET TO 'BLANK'*
     H*                AT THE EMPLOYER DETAIL LEVEL.                  *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                WHEN DEVIATIONS ARE SPLIT. DEV#2 CHANGES APPLY *
     H*                FOR (M,J,E,S,D) PREMIUM LEVELS AS IT IS FOR    *
     H*                DEV#1.                                         *
     H*                                                               *
     H* 03/12/10 C:235 PROJECT: 102WFC                                *
     H*                ADDITION CLASSES FOR LOGGING CALC              *
     H*                                                               *
     H* 02/25/10 C:65  PROJECT: 25WRC                                 *
     H*                EXPAND SCTA04 AND SCTA08 TO 6 DECIMAL PLACES   *
     H*                TO MATCH MONTHLY INTERIM OUTPUT FOR FULL SIZE  *
     H*                OF STABILIZATION FACTOR FIELD.                 *
     H*                                                               *
     H* 01/12/10 C:136 PROJECT: WC1838                                *
     H*                ADD LARGE DEDUCTIBLE PROCESSING                *
     H*                                                               *
     H* 01/14/10 C:191 PROJECT: 25WRC                                 *
     H*                BILLING ALOGRITHM RETROFIT                     *
     H*                - For CLT025,  use stabilization factor to cal *
     H*                  the amount of change for dev#2               *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records                               *
     H*                                                               *
     H* 03/02/09 C:65  PROJECT: 81WEI                                 *
     H*                Use optional headings for terrorism charges.   *
     H*                                                               *
     H* 01/26/09 C:136 SUPPORT:                                       *
     H*                CORRECT INFO WHEN PULLING IN THE PREMIUM       *
     H*                PAID DESC AND CTGY.                            *
     H*                                                               *
     H* 12/24/08 C:65  PROJECT: SUPPORT/WC1619                        *
     H*                USE THE REPORT DATE TO ACCESS THE SREFFD       *
     H*                PROGRAM TO ENSURE WE'RE PULLING RATES AS OF    *
     H*                CORRECT PERIOD ON THE POLICY.                  *
     H*                                                               *
     H* 11/05/08 C:136 PROJECT: 146WBM                                *
     H*                ADD S/R FEE                                    *
     H*                                                               *
     H* 09/23/08 C:136 SUPPORT: WA102 65290                           *
     H*                MAKE WC1627(NET RATE) BACWARDS COMPATIBLE FOR  *
     H*                CLIENTS THAT HAVE MODIFIED CODE AND HAVE NOT   *
     H*                IMPLEMENTED WC1627                             *
     H*                                                               *
     H* 09/15/08 C:189 PROJECT: WC1644                                *
     H*                Add an Option to exclude terrorism from LCM    *
     H*                                                               *
     H* 08/21/08 C:191 PROJECT: 102WEP                                *
     H*                BACK TO BASE  -                                *
     H*                 - S/R PAYROLL, CREATE LATE FEE                *
     H*                 - CALCULATE TERRORISM FOR LOGGING CLASSES     *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 01/30/08 C:136 SUPPORT WA136 62068                            *
     H*                CORRECT THE RATE THAT IS BEING PULLED IN BY    *
     H*                USING THE NEW SREFFD API                       *
     H*                                                               *
     H* 01/22/08 C:136  WA146 61959                                   *
     H*                -CORRECT RATE BEING PULLED IN FOR 'A' RATED    *
     H*                 POLICIES                                      *
     H*                -PERMIT ONLY PAYROLL CLASSES THAT ARE FLAGGED  *
     H*                 AS 'A' RATABLE TO BE PROCESSED AS 'A' RATED   *
     H*                 PAYROLLS PER GW                               *
     H*                                                               *
     H* 01/22/08  C:65  PROJECT: WC1627                               *
     H*                 PULL THE NET RATE FOR EACH PAYROLL CLASS      *
     F*                 FROM THE NEW NET FIELDS.                      *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                                                               *
     H* 07/30/07  C:65  PROJECT 71WGH (REMOVE SUPP#43064)             *
     H*                 REMOVE SUPPORT CHANGE FOR CLIENT 71 TO APPLY  *
     H*                 DEVIATION #1 AND #2 TO SAME PREMIUM FIGURE    *
     H*                 WHEN APPLIED TO MODIFIED PREMIUM.             *
     H*                                                               *
     H* 07/11/07 C:65  PROJECT: SUPP# 58761                           *
     H*                CORRECT PRIOR SUPPORT CHANGE FOR EXPANDING THE *
     H*                SIZE OF PREMIUM CALCULATION FIELD.  VALUE OF   *
     H*                PREMIUM CALC WAS NOT BEING PASSED OUT.         *
     H*                                                               *
     H* 07/06/07 C:65  PROJECT: SUPP# 58761                           *
     H*                ROUND THE TERRORISM PREMIUM TO WHOLE DOLLAR.   *
     H*                                                               *
     H* 06/06/07 C:136 SUPPORT: WA081 58542                           *
     H*                INCLUDE ONLY S/R BILLING CODE IN PREV BALANCE  *
     H*                                                               *
     H* 01/11/07 C:183 PROJECT: 139WGA                                *
     H*                IGNORE CATEGORY 'C' RECORDS WHERE A/R TYPE     *
     H*                IS 'B' OR 'A'. THIS APPLIES FOR REGULAR BI     *
     H*                LLING OR EXPENSE CONSTANT BILLINGS.            *
     H*                                                               *
     H* 11/15/06 C:136 SUPPORT: WA135 54711                           *
     H*                INCREASE ACCURACY OF CALCULATION DOWN TO THE   *
     H*                PENNY                                          *
     H*                                                               *
     H* 11/09/06 C:65  PROJECT: SUPP#55717                            *
     H*                SET REPORT DATE EQUAL TO POLICY EXPIRATION     *
     H*                DATE WHEN POLICY EXPIRES MID-MONTH.  ALSO,     *
     H*                TO MATCH MONTHLY INTERIM, ONLY USER AN EXP     *
     H*                MODIFIER IF ITS DATE IS LESS THAN REPORT DT.   *
     H*                                                               *
     H* 08/18/06 C:65  PROJECT: 136WBM/WC1373 - SUPP#54268            *
     H*                ALTER CODING TO EXCLUDE EXPENSE CONSTANT       *
     H*                BILLINGS FROM PRIOR BILLED A/R CHECK.          *
     H*                                                               *
     H* 07/26/06 C:136 PROJECT WC1324                                 *
     H*               -IMPROVE ACCURACY OF NET RATE CALCULATION BY    *
     H*                TWEAKING THE EXP MOD WEIGHTED AVERAGE CALC AND *
     H*                BY PROPERLY ACCOUNTING FOR A FLAT OR SPECIFIC  *
     H*                WAIVER CHARGE                                  *
     H*               -IMPROVE ACCURACY OF STANDARD CALCULATION BY    *
     H*                PROPERLY ACCOUNTING FOR A FLAT OR SPECIFIC     *
     H*                WAIVER CHARGE                                  *
     H*                                                               *
     H* 07/10/06 C:65  PROJECT 132WEW                                 *
     H*                CALCULATE THE AIRCRAFT REINSURANCE CHARGE IN   *
     H*                THE DISCOUNT AFTER MODIFIER CALCULATION.  IF   *
     H*                THE CHARGE IS A FLAT AMOUNT THEN ADD 1/12 TO   *
     H*                THE FINAL PREMIUM FIGURE.                      *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 04/26/06 C:132 PROJECT: 46WFL                                 *
     H*              - Bring 48 MODs for Self-Reporting Payroll Entry *
     H*                Discount back to base - NO change to Screens.  *
     H*              - Check "Late Flag" (WASRP/SRSTAT) for "L", if   *
     H*                not, Write an additional S/R Discount record   *
     H*                if WDF1P/F1DSCD code is not blank.             *
     H*                                                               *
     H* 04/13/06 C:132 PROJECT: WC1417                                *
     H*                Change "TRIA" ref to "Foreign Terrorism" in    *
     H*                SCR02, no changes in program code.             *
     H*                                                               *
     H* 03/02/06 C:136 PROJECT: 136WBM/WC1373                         *
     H*                IMPLEMENT CODE TO WRITE A/R ADJUSTMENT IF      *
     H*                PREVIOUSLY BILLED A/R EXISTS.                  *
     H*                                                               *
     H* 02/20/06 C:65  SUPPORT: #51496                                *
     H*                CLEAR OUT PREMIUM FIELD BEFORE TERRORISM CALC  *
     H*                TO PREVENT WAIVER FROM BEING ADDED IN TWICE.   *
     H*                                                               *
     H* 01/27/06 C:136 SUPPORT: WA136 51046                           *
     H*                CORRECT NET RATE FACTOR FOR SPLIT EXP MODS     *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC:  ADD COMMISSION MULTIPLIER    *
     H*                TO PREMIUM CALCULATION.                        *
     H*                                                               *
     H* 12/12/05 C:33  Seq#: WA109/50264                              *
     H*               -Use full key to get Agent info                 *
     H*               -Use seperate data structure for net rates      *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: WC1136                                *
     H*                ADD LOSS COST MULTIPLIER                       *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109DB}                                        *
     H*                CA flag rate increase. 0.2% for all 2002       *
     H*                 policies(anniv or eff dt) and a 4.9% increase *
     H*                 for the unexpired portion as of 1/1/2003.     *
     H*                                                               *
     H* 06/10/05 C:65  PROJECT: WC1288 (SUMMARIZE SELF-REPORTING)     *
     H*                WHEN CHECKING IF S/R RECORD HAS ALREADY BEEN   *
     H*                WRITTEN, CHECK A/R SUMMARY FLAG TO ENSURE      *
     H*                READING PROPER DIVISION FOR A/R.               *
     H*                                                               *
     H* 04/08/05 C:136 PROJECT: WC1288 (SUMMARIZE SELF-REPORTING)     *
     H*                WRITE RECORDS TO STATE REPORTING DIVISION,     *
     H*                ALSO INDICATE WHICH DIVISION INITIATED THE     *
     H*                'SR' RECORD.                                   *
     H*                                                               *
     H* 04/07/05 C:156PROJECT: 126WDK                                 *
     H*               PROJECT: 126WCD                                 *
     H*               MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.         *
     H*                                                               *
     H* 02/15/05 C:65 PROJECT: WC1167                                 *
     H*               IF NET PAYROLL RATES ARE USED THEN PRINT THE    *
     H*               APPROPRIATE RATES AND SKIP THE PREMIUM CALC     *
     H*               ON THE LAST SCREEN.                             *
     H*                                                               *
     H* 01/07/05 C:136 PROJECT: WC1250                                *
     H*                ADD DOMESTIC TERRORISM RATE WITH THE TERRORISM *
     H*                RATE WHEN CALCULATING THE TERRORISM CHARGE.    *
     H*                                                               *
     H* 11/23/04 C:65 PROJECT: SUPPORT#43064                          *
     H*               APPLY DEVIATION #2 AFTER DEV #1 ON MODIFIED     *
     H*               PREMIUM INSTEAD OF TO THE SAME FIGURE.          *
     H*                                                               *
     H* 02/26/04 C:136 SUPPORT: WA129 41471                           *
     H*                Reporting date not displayed for weekly        *
     H*                reporting cycle.                               *
     H*                                                               *
     H* 02/06/04 C:65 PROJECT: SUPPORT#39886/WC1120                   *
     H*               DO NOT USE THE ARD WHEN PULLING TERRORISM RATE  *
     H*               FOR NJ.  USE THE POLICY EFFECTIVE DATE ONLY.    *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP.  *
     H*                                                               *
     H* 10/10/03 C:65  PROJECT: SUPP/WC1123/WC1149                    *
     H*                ENSURE RATING DATE ARRAYS ARE NOT FILLED WITH  *
     H*                RATE SPLIT DATES WHEN THE SPLIT IS NOT VALID.  *
     H*                                                               *
     H* 10/07/03 C:65  PROJECT: WC1149                                *
     H*                IF AN FLORIDA POLICY HAS A PERIOD RATED BEFORE *
     H*                10/1/3 AND EXPIRES AFTER 10/1/3 THEN PULL      *
     H*                RATES AS OF 10/1/3 FOR OCTOBER AND FORWARD.    *
     H*                                                               *
     H* 10/08/03 C:136 SUPPORT: WA099 39421                           *
     H*                Fill out canx date field on screens.           *
     H*                                                               *
     H* 04/30/03 C:65  PROJECT: WC1123                                *
     H*                IF AN FLORIDA POLICY HAS A PERIOD RATED 1/1/3  *
     H*                TO 3/31/3, THEN PULL 4/1/3 RATES FOR ANY       *
     H*                MONTHS AFTER MARCH.                            *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                If @@TODT is passed, process calculation for   *
     H*                a weekly report. If vendor processing is       *
     H*                enabled, display frequency of report.          *
     H*                                                               *
     H* 01/29/03 C:65  PROJECT: WC744                                 *
     H*                ADD THE ARAP CHARGE TO THE DISCOUNT AFTER      *
     H*                MODIFIER FIGURE.  THE ARAP CHARGE IS CALCULATED*
     H*                WITHOUT ANY OF THE OTHER DISCOUNTS INCLUDED.   *
     H*                                                               *
     H* 01/17/03 C:65  PROJECT: WC1120                                *
     H*                CALCULATE THE TERRORISM RISK CHARGE AND ADD    *
     H*                TO THE TOTAL PREMIUM DUE AMOUNT.               *
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT WC1099                                  *
     H*               IF A PAYROLL CLASS CODE HAS AN EXTENSION WITH   *
     H*               A STAT CODE THEN PRINT THE HEADING TERRITORY    *
     H*               DIFF.  ADD ANY ADDITIONAL LOAD AMOUNTS FROM     *
     H*               STAT ENABLED CLASS CODES INTO THIS FIELD AND    *
     H*               INTO THE PREMIUM TOTAL FOR CALCULATION SCREEN.  *
     H*                                                               *
     H* 04/25/02  C:65  PROJECT CM045                                 *
     H*                 IMPLEMENT CHANGES TO INCLUDE WAIVER SURCHARGE *
     H*                 ON THE MONTHLY INTERIM.  IF THE WAIVER IS A   *
     H*                 BLANKET PERCENTAGE THEN IT CAN BE ADDED INTO  *
     H*                 THE BEFORE OR AFTER MODIFIER.  IF IT IS A     *
     H*                 FLAT AMOUNT OR SPECIFIC TO EACH WAIVER THEN   *
     H*                 1/12 OF THE ANNUAL AMOUNT WILL BE ADDED.      *
     H*                                                               *
     H* 11/15/01  C:136 SUPPORT# 29681                                *
     H*                 ENSURE THAT EACH RECORD WRITTEN TO A/R DETAIL *
     H*                 FILE (WMEAP) HAS A UNIQUE TIMESTAMP VALUE     *
     H*                                                               *
     H* 10/11/01  C:65  SUPPORT# 28939                                *
     H*                 ANNIVERSARY RATING DATE SHOULD BE USED AS     *
     H*                 LONG AS IT FALLS BEFORE THE POLICY EFF DATE,  *
     H*                 AND THE RATING DATE IN EFFECT DOES NOT. ALSO  *
     H*                 IF THE RATING DATE IN EFFECT IS EQUAL TO THE  *
     H*                 POLICY EXPIRATION DATE, DO NOT USE THE IT.    *
     H*                                                               *
     H* 09/24/01  C:65  PROJECT WC1035                                *
     H*                 EXPAND PAYROLL RATE FIELD BY 2 DIGITS.        *
     H*                                                               *
     H* 08/20/01 C:90  PROJECT: WC1041                                *
     H*                Expanded percentage from 3.2 to 5.3            *
     H*                                                               *
     H* 03/14/01 C:65  PROJECT WC1027                                 *
     H*                IF A CLASS CODE IS POPULATION RATED, SET ITS   *
     H*                PREMIUM TO 1/12 OF THE PREMIUM ON THE CLASS.   *
     H*                                                               *
     H* 12/18/00 C:65  PROJECT SUPPORT#25063                          *
     H*                CORRECT A DATE ISSUE WITH INCR LIMITS.         *
     H*                                                               *
     H* 11/30/00 C:65  PROJECT WC577                                  *
     H*                BLANK OUT THE TAX A/R CODE IF THERE IS NO TAX. *
     H*                                                               *
     H* 10/12/00 C:61  PROJECT WC1012                                 *
     H*              - Add F7=A/R Inquiry to SCR02.                   *
     H*              - Default 'Bill to A/R' = 'Y'                    *
     H*              - Info window if billing for entered posting     *
     H*                date has already been entered.                 *
     H*                                                               *
     H* 03/31/00 C:118 SUPPORT: WA091/22407                           *
     H*                CHECK FOR FEB. 29 BEING A VALID DATE, IF NOT   *
     H*                SUB 1 FROM XXDAY.  ALSO MADE DISPLAY FILE CHGS *
     H*                TO F-KEYS.                                     *
     H*                                                               *
     H* 01/03/00 C:65 PROJECT: SUPPORT#21112                          *
     H*                RESET THE STATE VARIABLE AFTER THE WC945 MODS  *
     H*                TO ALLOW CHAINING WITH STATE BASED FILES.      *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 08/12/99  C:65  PROJECT WC935                                 *
     H*                 ALLOW FOR 1ST EXP MOD TO BE ZERO, IN ORDER    *
     H*                 TO SKIP IT.                                   *
     H*                                                               *
     H*  04/14/99  C:87  PROJECT: SUPP WA102 #17412                   *
     H*                  Make sure the A/R tax billing code selected  *
     H*                  is active.  If none don't default.           *
     H*                                                               *
     H*  03/18/99  C:65  PROJECT: 48WOY                               *
     H*                  CHANGE DISPLAY OF TAX PERCENTAGE FOR 6.4     *
     H*                                                               *
     H*  03/03/99  C:87  PROJECT: WC882                               *
     H*                  Default A/R Billing code (SCBLCD) to F1SRBB. *
     H*                  Default A/R Tax Billing code to the code     *
     H*                  corresponding to the first record in the A/R *
     H*                  transaction code file that has a 'T' category*
     H*                  type and which has a type of 'F'.            *
     H*                                                               *
     H*  12/23/98  C:65  PROJECT: SUPPORT                             *
     H*                  FACTORS NOT SHOWING WHEN EFF FOR ONE DAY.    *
     H*                                                               *
     H*  12/03/98  C:65  PROJECT: WC705                               *
     H*                  ADD LATE FLAGS FOR BILLED AND PAID A/R RCDS. *
     H*                                                               *
     H*  09/24/98  C:65  PROJECT: WC705                               *
     H*                  ADD STATE TAX TO PREMIUM CALC, BILLED + PAID *
     H*                                                               *
     H*  09/11/98  C:65  PROJECT: SUPPORT                             *
     H*                  CHECK PAYROLLS USING ELPFDT, NOT EPEFDT.     *
     H*                                                               *
     H*  09/02/98  C:65  PROJECT: WC705                               *
     H*                  USE FACTOR AFTER SCH DB/CR WHEN APPLYING     *
     H*                  DEV #2 TO EXPERIENCE.                        *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*                                                               *
     H* 07/13/98  C:65  PROJECT: WC846                                *
     H*                 CHECK IF DISCOUNT IS APPLIED TO MOD MANUAL.   *
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H*  02/03/98  C:33  PROJECT: WC738                               *
     H*                  ALLOW ZERO BILLING/PAYMENT AMOUNTS           *
     H*****************************************************************
     FW0660F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDHEL     IF   E           K DISK
     FWASRL2    IF   E           K DISK
     FWASRL8    IF   E           K DISK
     F                                     RENAME(PSR01:PSR02)
     FWMEPL     IF   E           K DISK
     FWMEPL7    IF   E           K DISK
     F                                     RENAME(PEP01:PEP0L7)
     FWMEML     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWDELP     IF   E             DISK
     FWMEAL     O    E           K DISK
     FWMEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMAHL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWTLML1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMPXL1    IF   E           K DISK
      * EMPLOYER PREMIUM FILE
     FWDMPL     IF   E           K DISK
      * GROUP INSTALLMENT FEE TABLE
     FWMIFL     IF   E           K DISK
     FWMLDL1    IF   E           K DISK
     FWDHCL3    IF   E           K DISK
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

     D SED             S              7  0 DIM(200)
     D SCS             S              6    DIM(200)
     D SDS             S             15    DIM(200)
     D SPY             S             11  2 DIM(200)
     D***  Beg Chg  ***  06/17/24 *****************************************
     D***SRT             S              7  2 DIM(200)
     D SRT             S              8S 3 DIM(200)
     D***  End Chg  ***  06/17/24 *****************************************
     D SAM             S              9  2 DIM(200)
     D SRTN            S              7  2 DIM(200)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYE            S              6  0 DIM(2)
     D KEYC            S              5  0 DIM(2)
     D X0              S              1    DIM(5)
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     D CON             S             30    DIM(14) CTDATA PERRCD(1)
     D TMP             S              1    DIM(30)
     D NUM             S              1    DIM(5)
     D NUM6            S              1    DIM(6)
     D OPTNM           C                   CONST('Calculate S/R Premiu-
     D                                     m')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D AGTAPI          C                   CONST('WA002R')
     D SRLOGC          C                   CONST('SRLOGCLAS')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  ED                     1    104  0
     D                                     DIM(13)
     D  SCED01               001    008  0
     D  SCED02               009    016  0
     D  SCED03               017    024  0
     D  SCED04               025    032  0
     D  SCED05               033    040  0
     D  SCED06               041    048  0
     D  SCED07               049    056  0
     D  SCED08               057    064  0
     D  SCED09               065    072  0
     D  SCED10               073    080  0
     D  SCED11               081    088  0
     D  SCED12               089    096  0
     D  SCED13               097    104  0
     D                 DS
     D  CL                     1     78
     D                                     DIM(13)
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
     D  SCCL11                61     66
     D  SCCL12                67     72
     D  SCCL13                73     78
     D                 DS
     D  DS                     1    195
     D                                     DIM(13)
     D  SCDS01                 1     15
     D  SCDS02                16     30
     D  SCDS03                31     45
     D  SCDS04                46     60
     D  SCDS05                61     75
     D  SCDS06                76     90
     D  SCDS07                91    105
     D  SCDS08               106    120
     D  SCDS09               121    135
     D  SCDS10               136    150
     D  SCDS11               151    165
     D  SCDS12               166    180
     D  SCDS13               181    195
     D                 DS
     D  PY                     1    143  2
     D                                     DIM(13)
     D  SCPY01                 1     11  2
     D  SCPY02                12     22  2
     D  SCPY03                23     33  2
     D  SCPY04                34     44  2
     D  SCPY05                45     55  2
     D  SCPY06                56     66  2
     D  SCPY07                67     77  2
     D  SCPY08                78     88  2
     D  SCPY09                89     99  2
     D  SCPY10               100    110  2
     D  SCPY11               111    121  2
     D  SCPY12               122    132  2
     D  SCPY13               133    143  2
     D***  Beg Chg  ***  06/17/24 *****************************************
     D***              DS
     D***RT                     1     91  2
     D***                                   DIM(13)
     D***
     D***SCRT01                 1      7  2
     D***SCRT02                 8     14  2
     D***SCRT03                15     21  2
     D***SCRT04                22     28  2
     D***SCRT05                29     35  2
     D***SCRT06                36     42  2
     D***SCRT07                43     49  2
     D***SCRT08                50     56  2
     D***SCRT09                57     63  2
     D***SCRT10                64     70  2
     D***SCRT11                71     77  2
     D***SCRT12                78     84  2
     D***SCRT13                85     91  2
     D                 DS
     D  RT                     1    104S 3
     D                                     DIM(13)
     D  SCRT01                 1      8S 3
     D  SCRT02                 9     16S 3
     D  SCRT03                17     24S 3
     D  SCRT04                25     32S 3
     D  SCRT05                33     40S 3
     D  SCRT06                41     48S 3
     D  SCRT07                49     56S 3
     D  SCRT08                57     64S 3
     D  SCRT09                65     72S 3
     D  SCRT10                73     80S 3
     D  SCRT11                81     88S 3
     D  SCRT12                89     96S 3
     D  SCRT13                97    104S 3
     D***  End Chg  ***  06/17/24 ******************************************
     D                 DS
     D  RTN                    1     91  2
     D                                     DIM(13)
     D  SCNT01                 1      7  2
     D  SCNT02                 8     14  2
     D  SCNT03                15     21  2
     D  SCNT04                22     28  2
     D  SCNT05                29     35  2
     D  SCNT06                36     42  2
     D  SCNT07                43     49  2
     D  SCNT08                50     56  2
     D  SCNT09                57     63  2
     D  SCNT10                64     70  2
     D  SCNT11                71     77  2
     D  SCNT12                78     84  2
     D  SCNT13                85     91  2
     D                 DS
     D  AM                     1    117  2
     D                                     DIM(13)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D  SCAM12               100    108  2
     D  SCAM13               109    117  2
     D                 DS
     D  EPEFDT                 1      7  0
     D  X2YYMM                 1      5  0
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  XXRPDT                 1      7  0
     D  XXYYMM                 1      5  0
     D  XXDAY                  6      7  0
     D  XXRPYR                 1      3  0
     D  XXRPMN                 4      5  0
     D  XXRPYM                 1      5  0
     D                 DS
     D  X2CLSP                 1      6
     D  X2CLAS                 1      5
     D  X2SPEC                 6      6
     D                 DS
     D  XXCLSP                 1      6
     D  EPCLAS                 1      5  0
     D  EPSPEC                 6      6
     D                 DS
     D  HDG                    1    300
     D                                     DIM(10)
     D  SCHD01                 1     30
     D  SCHD02                31     60
     D  SCHD03                61     90
     D  SCHD04                91    120
     D  SCHD05               121    150
     D  SCHD06               151    180
     D  SCHD07               181    210
     D  SCHD08               211    240
     D  SCHD09               241    270
     D  SCHD10               271    300
     D                 DS
     D  TAM                    1    110  2
     D                                     DIM(10)
     D  SCTA01                 1     11  2
     D  SCTA02                12     22  2
     D  SCTA03                23     33  2
     D  SCTA04                34     44  6
     D  SCTA05                45     55  2
     D  SCTA06                56     66  4
     D  SCTA07                67     77  2
     D  SCTA08                78     88  6
     D  SCTA09                89     99  2
     D  SCTA10               100    110  2
     D                 DS
     D  ELPFDT                 1      7  0
     D  XXFPYM                 1      5  0
     D  XXFPYR                 1      3  0
     D  XXFPMO                 4      5  0
     D  XXFPDY                 6      7  0
     D                 DS
     D  ELPTDT                 1      7  0
     D  XXTPYM                 1      5  0
     D  XXTPYR                 1      3  0
     D  XXTPMO                 4      5  0
     D  XXTPDY                 6      7  0
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)
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
     D                 DS
     D  EX                     1    021  0
     D                                     DIM(3) DESCEND
     D  ELEMD1               001    007  0
     D  ELEMD2               008    014  0
     D  ELEMD3               015    021  0
     D                 DS
     D  EM                     1     15  4
     D                                     DIM(3)
     D  ELEXM1                 1      5  4
     D  ELEXM2                 6     10  4
     D  ELEXM3                11     15  4
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D***  Begin Add  ***  01/19/22  *********************
      /copy wPrEmplr
     D***  End   Add  ***  01/19/22  *********************

     IPEP0L7
     I              EPSTAT                      E9STAT
     I              EPEFDT                      E9EFDT
     I              EPEMPR                      E9EMPR
     I              EPEXT                       E9EXT
     I              EPSPEC                      E9SPEC

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE HARBOR EXPOSURE FILE
     C*
     C     HEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C*
     C     HEKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    X1ST              2
     C                   KFLD                    X1EFDT            7 0
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    X1ST
     C*
     C*  KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  KEY LIST FOR THE STAT FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  KEY LIST FOR THE SELF REPORTING FILE
     C*
     C     SRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXRPMY
     C                   KFLD                    XXCLAS
     C*
     C     SRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXRPMY
     C*
     C     SRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXRPDT
     C                   KFLD                    XXCLAS
     C*
     C     SRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXRPDT
     C*
     C*  KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    X1EFDT            7 0
     C                   KFLD                    XXCLAS
     C*
     C     EPKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKY7A        KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EPFYR
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPSPEC
      *
     C     EPKY7B        KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EPFYR
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPSPEC
     C                   KFLD                    XXRPDT
     C*
     C     MPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    X1EFDT
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*
     C*  KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE DISCOUNT MASTER FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELDISC
     C*
     C*  KEY LIST FOR THE LIMITS FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X1KEY
     C*
     C     TBKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXILEF            7 0
     C*
     C     LMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Employer Accounts
      *
     C     EAKY          KLIST
     C                   KFLD                    X3CO#
     C                   KFLD                    X3FND
     C                   KFLD                    X3EMP#
     C                   KFLD                    X3DIV
     C                   KFLD                    X3FYR
     C                   KFLD                    X3ARTY
     C                   KFLD                    X3PODT
     C*
     C* HCN CREDIT KEY
     C*
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     **
     **  SYSTEM CONTROL FILE
     **
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    PRRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    ELPFDT        X2EFDT
     C     *LIKE         DEFINE    EACO#         X3CO#
     C     *LIKE         DEFINE    EAFND         X3FND
     C     *LIKE         DEFINE    EAEMP#        X3EMP#
     C     *LIKE         DEFINE    EADIV         X3DIV
     C     *LIKE         DEFINE    EAFYR         X3FYR
     C     *LIKE         DEFINE    EAARTY        X3ARTY
     C     *LIKE         DEFINE    EAPODT        X3PODT
     C     *LIKE         DEFINE    EPARAT        XXARAT
     C     *LIKE         DEFINE    RRTERR        XXTERR
     C     *LIKE         DEFINE    XXFACT        XXFAC2
     C     *LIKE         DEFINE    EADIV         XSDIV
     C     *LIKE         DEFINE    ELENMP        XXSEXC
     C     *LIKE         DEFINE    EAAMT         XXBADJ
     C     *LIKE         DEFINE    ELEXM1        XXMOD
     C     *LIKE         DEFINE    ELEMPM        XXEMPM
     C     *LIKE         DEFINE    ELHCN         XXHCN
     C     *LIKE         DEFINE    ELHCN         SVHCN
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RPMY            6 0
     C                   PARM                    @@TODT            8 0
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
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM      ELCO#         ##CO#             3 0
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   MOVE      '0'           *IN49
     C     CLTID         IFEQ      '025'
     C                   MOVE      '1'           *IN49
     C                   ENDIF
     C     @@TODT        IFEQ      *ZERO
     C                   SETOFF                                       46
     C                   TIME                    DSDTTM
     C                   Z-ADD     @@RPMY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYYMM
     C                   MOVE      XXYYMM        X1                2 0
     C                   MOVE      MD(X1)        XXDAY
     C     XXRPMN        IFEQ      2
     C     XXRPYR        DIV       4             XXDIVR            2 0
     C                   MVR                     XXREM             2 0
     C     XXREM         IFNE      *ZERO
     C                   MOVE      28            XXDAY
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     XXYYMM        XXRPFM
     C                   MOVE      01            XXRPFM
     C                   ELSE
     C                   SETON                                        46
     C                   TIME                    DSDTTM
     C                   Z-ADD     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPDT
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        SCUSER
     C     @@RRN         CHAIN     WDELP                              60
     C                   Z-ADD     ELFYR         SCFYR
     C*
      *
      * SET REPORT DATE EQUAL TO POLICY EXPIRATION DATE IF POLICY
      * EXPIRES MID-MONTH.
      *
     C     XXRPDT        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        XXRPDT
     C                   ENDIF
      *
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF
     C*
     C     ELANDT        IFNE      *ZEROS
     C     ELANDT        ANDNE     ELPFDT
     C                   Z-ADD     ELANDT        XXANDT
     C                   ADD       1             XXANYR
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXANDT
     C                   ENDIF
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        X2EFDT
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
     C     *IN60         IFEQ      *OFF
     C     IFISFE        ANDNE     *ZERO
     C                   Z-ADD     IFISFE        SCISFE
     C                   Z-ADD     IFISFE        SCISFP
     C                   MOVE      IFIFBL        SCIFBL
     C                   MOVE      IFIFPD        SCIFPD
     C                   MOVE      *ON           *IN64
     C                   ELSE
     C                   Z-ADD     *ZERO         SCISFE
     C                   Z-ADD     *ZERO         SCISFP
     C                   MOVE      *BLANKS       SCIFBL
     C                   MOVE      *BLANKS       SCIFPD
     C                   MOVE      *OFF          *IN64
     C                   ENDIF
     C*
     C     ELARSM        IFEQ      'Y'
      *
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C                   LEAVE
     C                   ENDIF
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO
      *
     C                   ENDIF
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL23        IFEQ      'Y'
      *
     C                   SETON                                        47
     C                   MOVEL(P)  ELBSCD        X1KEY
     C     TBKY          CHAIN     PBS01                              60
     C                   SELECT
     C     BSFREQ        WHENEQ    'W'
     C                   MOVEL(P)  'Weekly'      SCFREQ
     C     BSFREQ        WHENEQ    'M'
     C                   MOVEL(P)  'Monthly'     SCFREQ
     C                   OTHER
     C                   MOVE      *BLANKS       SCFREQ
     C                   ENDSL
      *
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF
     C                   MOVE      *BLANK        LRGDFL            1
     C     WCFL43        IFEQ      'Y'
     C                   MOVEL(P)  ELBSCD        X1KEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        ANDEQ     'L'
     C     ELKEY         CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           LRGDFL            1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   ELSE
     C                   MOVE      *BLANKS       SCCONM
     C                   ENDIF
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   ELSE
     C                   MOVE      *BLANKS       SCFND
     C                   MOVE      *BLANKS       SCNAME
     C                   ENDIF
     C     F1KY          CHAIN     PF101                              60
     C*  Default A/R Billing code
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F1SRBB        SCBLCD
     C                   ENDIF
     C*
     C*  Default A/R tax billing code
     C                   MOVE      *BLANK        SCTXBC
     C                   MOVE      *OFF          FOUND             1
     C*
     C     TBKY1         CHAIN     PTT01                              60
     C     *IN60         DOWEQ     '0'
     C     FOUND         ANDEQ     *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     TTSTAT        ANDEQ     'A'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         SCTXBC
     C                   MOVE      *ON           FOUND
     C                   ENDIF
     C*
     C     TBKY1         READE     PTT01                                  60
     C                   ENDDO
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
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
     C                   MOVE      *OFF          *IN48
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN48
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*  GET AGENT RECORDS
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      ELPFDT        X2EFDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C                   MOVE      *OFF          *IN51
     C                   MOVEL     ELPFDT        X1YR              3 0
     C                   MOVEL     ELANDT        X1AYR             3 0
     C     XXST          IFEQ      'CA'
     C     X1YR          ANDEQ     102
     C     XXST          OREQ      'CA'
     C     X1AYR         ANDEQ     102
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C*
     C     *IN46         IFEQ      *OFF
     C                   Z-ADD     @@RPMY        SCRPMY
     C     XXRPFM        IFLT      ELPFDT
     C                   MOVE      ELPFDT        XXRPFM
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     @@TODT        SCTODT
     C                   Z-ADD     @@TODT        ##DT8
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DT8             8 0
     C                   PARM      6             ##PAST            5 0
     C                   PARM      *BLANK        ##ERR             1
      *
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPFM
      *
     C     XXRPFM        IFLT      ELPFDT
     C                   MOVE      ELPFDT        XXRPFM
     C                   ENDIF
     C*
     C* CALCULATE DAYS IN THE TIME PERIOD.
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C                   Z-ADD     XXRPFM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     XXRPDT        SRDTI
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
     C                   ADD       1             QQDAYS
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     QQERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        XXDAYS            5 0
      *
     C                   ENDIF
     C                   Z-ADD     *ZEROS        SCMAPR
     C                   Z-ADD     *ZEROS        XXPAYR           15 4
     C                   Z-ADD     1             XXPROR           12 9
     C                   MOVE      *OFF          *IN63
     C***  Begin Chg  ***  01/19/22  *********************
     C*****F1NETR        IFEQ      'Y'
     C*****ELPFDT        ANDGE     F1NETD
     C                   If        IsUseNetRate(ELco#:ELfnd:ELemp#:ELdiv:ELfyr)
     C***  End   Chg  ***  01/19/22  *********************
     C*
     C*
     C*  SUBTRACT EXP MOD CHG FROM NORMAL PREMIUM, GET CURRENT EXP MOD AND
     C*  MULTIPLY IT BACK INTO ELENMP SO THAT NET RATE REFLECT CURRENT EXP
     C*  MOD
     C*
     C                   MOVE      *ON           *IN63
     C     ELMAPR        IFNE      *ZERO
     C     ELEMPM        ANDNE     *ZERO
     C*
     C     MPKY          CHAIN     PMP01                              60
     C*
     C*  GET EXP MOD CHG IF NO HIT OR IF WDMPP/MPEXPA = 0
     C*
     C     *IN60         IFEQ      *ON
     C     MPEXPA        OREQ      0
     C*
     C     EPKEY2        CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
     C                   ADD       EPADLD        XXADLD           11 2
     C                   ENDIF
     C     EPKEY2        READE     PEP01                                  60
     C                   ENDDO
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      ELPFDT        X2EFDT
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   Z-ADD     0             X1PREM           11 2
     C                   Z-ADD     0             X2PREM           11 2
     C                   ADD       ELEXP1        X1PREM
     C                   ADD       ELEXP2        X1PREM
     C                   ADD       ELEXP3        X1PREM
     C                   ADD       ELESBJ        X2PREM
     C                   SUB       ELEDCS        X2PREM
     C                   SUB       ELEDCM        X2PREM
     C     RRDV1L        IFEQ      'J'
     C                   SUB       ELEDC2        X2PREM
     C                   ENDIF
     C     RRDV2L        IFEQ      'J'
     C                   SUB       ELEDV2        X2PREM
     C                   ENDIF
     C     ELWVFL        IFEQ      'J'
     C                   ADD       ELEWAV        X2PREM
     C                   ENDIF
     C     RRASFL        IFEQ      'J'
     C                   ADD       XXADLD        X2PREM
     C                   ENDIF
     C     X1PREM        IFNE      *ZERO
     C     X1PREM        SUB       X2PREM        MPEXPA
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXPRTT
     C     *IN46         IFEQ      *OFF
     C     XXRPDT        SUB       XXRPFM        XXDAYS
     C                   ELSE
     C                   Z-ADD     XXRPFM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##FDT8            8 0
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##TDT8            8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    ##FDT8
     C                   PARM                    ##TDT8
     C     XXDAYS        PARM                    ##DAYS
     C                   PARM                    ##ERR
     C                   ENDIF
     C*
     C                   ADD       1             XXDAYS
     C                   Z-ADD     XXRPFM        XXWKDT            7 0
     C     XXWKDT        DOWLE     XXRPDT
     C*
     C     EX(3)         IFNE      *ZERO
     C     XXWKDT        ANDGE     EX(3)
     C                   Z-ADD     EM(3)         XXMOD
     C                   ELSE
     C     EX(2)         IFNE      *ZERO
     C     XXWKDT        ANDGE     EX(2)
     C                   Z-ADD     EM(2)         XXMOD
     C                   ELSE
     C     EX(1)         IFEQ      0
     C     XXWKDT        ORLT      EX(1)
     C                   Z-ADD     1             XXMOD
     C                   ELSE
     C                   Z-ADD     EM(1)         XXMOD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXMOD         DIV(H)    XXDAYS        XXPROD           11 9
     C                   ADD(H)    XXPROD        XXPRTT           11 9
     C     *IN46         IFEQ      *OFF
     C                   ADD       1             XXWKDT
     C                   ELSE
     C                   Z-ADD     XXWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DT8             8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DT8             8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM      *BLANK        ##ERR             1
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWKDT
     C                   ENDIF
      *
     C                   ENDDO
     C*
     C*
     C                   Z-ADD(H)  XXPRTT        XXWK54            5 4
     C     XXWK54        IFEQ      *ZERO
     C                   Z-ADD     1             XXWK54
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN71
     C     ELWVPC        IFEQ      0
     C     ELEWAV        ANDNE     0
     C     ELEWAV        ORNE      0
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *ON           *IN71
      *
     C     *IN46         IFEQ      *OFF
     C     1             DIV(H)    12            XXDPRO            9 8
     C                   ELSE
     C     XXDAYS        DIV(H)    365           XXDPRO
     C                   ENDIF
      *
     C     ELWVFL        IFEQ      'M'
     C     ELWVFL        OREQ      'J'
     C     ELEMPM        SUB       ELEWAV        XXSEXC
     C                   SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR           12 9
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C     ELEWAV        MULT(H)   XXDPRO        XXEWAV           11 0
     C     ELWVFL        IFEQ      'M'
     C     XXEWAV        MULT(H)   XXPROR        XXEWAV
     C                   ELSE
     C     XXEWAV        MULT(H)   XXWK54        XXEWAV
     C                   ENDIF
     C                   ELSE
     C     ELEMPM        SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C                   ENDIF
      *
     C     ELWVFL        IFEQ      'E'
     C     ELWVFL        OREQ      'S'
     C     ELENMP        SUB       ELEWAV        ELENMP
     C     ELENMP        DIV(H)    ELEMPM        X1PROR           12 9
     C                   MULT(H)   X1PROR        XXPROR
     C     XXEWAV        MULT(H)   XXDPRO        XXEWAV
     C     ELEWAV        MULT(H)   X1PROR        XXEWAV
     C                   ELSE
     C     ELWVFL        IFEQ      'D'
     C     ELEWAV        MULT(H)   XXDPRO        XXEWAV
     C     ELENMP        SUB       ELEWAV        ELENMP
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C                   ELSE
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C     XXEWAV        MULT      X1PROR        XXEWAV
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     XXEWAV        SCWVAM
      *
     C                   ELSE
     C     ELEMPM        SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVE      *ZEROS        SED
     C                   MOVE      *BLANKS       SCS
     C                   MOVE      *BLANKS       SDS
     C                   MOVE      *ZEROS        SPY
     C                   MOVE      *ZEROS        SRT
     C                   MOVE      *ZEROS        SRTN
     C                   MOVE      *ZEROS        SAM
     C                   Z-ADD     *ZEROS        SCADLD
     C                   MOVE      *OFF          *IN62
     C                   Z-ADD     0             Y                 3 0
     C                   Z-ADD     XXYYMM        XXRPMY            5 0
     C     EPKEY2        CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C     EPEFDT        ANDLE     XXRPDT
      *
      *   GET LATEST RATE AS OF REPORT DATE FOR 'A' RATED POLICIES THAT ARE
      *   ANNIVERSARY RATED
      *
     C     ELSTAT        IFEQ      'Y'
     C     ELANDT        ANDNE     *ZERO
     C     EPKY7B        SETGT     PEP0L7
     C     EPKY7A        READPE    PEP0L7                                 60
     C                   ENDIF
      *
     C     EPSTAT        IFNE      'I'
     C     X2YYMM        ANDLE     XXYYMM
     C     EPCLAS        ANDNE     RRDCCD
     C     *IN46         ANDEQ     *OFF
     C                   Z-ADD     1             X                 3 0
     C     XXCLSP        LOOKUP    SCS(X)                                 55
     C*
     C*  DO NOT DUPLICATE A PAYROLL CLASS CODE WHEN DISPLAYING
     C*  SELF-REPORTING CALCULATED MANUAL PREMIUM
     C*
     C     *IN55         IFEQ      '0'
     C                   ADD       1             Y
     C                   Z-ADD     EPEFDT        SED(Y)
     C                   MOVE      XXCLSP        SCS(Y)
     C                   EXSR      SRPREM
     C                   MOVE      XXDESC        SDS(Y)
     C                   Z-ADD     XXAMPY        SPY(Y)
     C                   Z-ADD     XXAMRT        SRT(Y)
     C                   Z-ADD(H)  XNAMRT        SRTN(Y)
     C                   Z-ADD     XXAMPR        SAM(Y)
     C     PRPCF         IFEQ      'O'
     C     EPEMPR        DIV(H)    12            XXWK90            9 0
     C                   Z-ADD     XXWK90        SAM(Y)
     C                   Z-ADD     0             SPY(Y)
     C                   Z-ADD     0             SRT(Y)
     C                   Z-ADD     0             SRTN(Y)
     C                   Z-ADD     XXWK90        XXAMPR
     C                   ENDIF
     C                   ADD       XXAMPR        SCMAPR
     C                   ELSE
     C*
     C*  THE ONLY TIME A PAYROLL CLASS CODE SHOULD BE DUPLICATED
     C*  FOR AN EMPLOYER IS IF ANNIVERSARY RATING WAS IN EFFECT AND
     C*  A PAYROLL CLASS WAS SPLIT.  IN THIS CASE WE WANT TO SHOW
     C*  THE PROPER EFF DT, DESCRIPTION AND RATE FOR WHATEVER YEAR
     C*  AND MONTH THE SELF-REPORTING MANUAL PREMIUM IS BEING
     C*  CALCULATED.
     C*
     C                   Z-ADD     EPEFDT        SED(X)
     C                   END
     C                   END
     C*
     C     EPSTAT        IFNE      'I'
     C     EPEFDT        ANDLE     XXRPDT
     C     EPCLAS        ANDNE     RRDCCD
     C     *IN46         ANDEQ     *ON
     C                   Z-ADD     1             X                 3 0
     C     XXCLSP        LOOKUP    SCS(X)                                 55
     C*
     C*  DO NOT DUPLICATE A PAYROLL CLASS CODE WHEN DISPLAYING
     C*  SELF-REPORTING CALCULATED MANUAL PREMIUM
     C*
     C     *IN55         IFEQ      '0'
     C                   ADD       1             Y
     C                   Z-ADD     EPEFDT        SED(Y)
     C                   MOVE      XXCLSP        SCS(Y)
     C                   EXSR      SRPREM
     C                   MOVE      XXDESC        SDS(Y)
     C                   Z-ADD     XXAMPY        SPY(Y)
     C                   Z-ADD     XXAMRT        SRT(Y)
     C                   Z-ADD(H)  XNAMRT        SRTN(Y)
     C                   Z-ADD     XXAMPR        SAM(Y)
     C     PRPCF         IFEQ      'O'
      *
     C     XXDAYS        DIV       365           XXPRO            11 9
     C     EPEMPR        MULT      XXPRO         XXWK90
      *
     C                   Z-ADD     XXWK90        SAM(Y)
     C                   Z-ADD     0             SPY(Y)
     C                   Z-ADD     0             SRT(Y)
     C                   Z-ADD     0             SRTN(Y)
     C                   Z-ADD     XXWK90        XXAMPR
     C                   ENDIF
     C*
     C                   ADD       XXAMPR        SCMAPR
     C                   ELSE
     C*
     C*  THE ONLY TIME A PAYROLL CLASS CODE SHOULD BE DUPLICATED
     C*  FOR AN EMPLOYER IS IF ANNIVERSARY RATING WAS IN EFFECT AND
     C*  A PAYROLL CLASS WAS SPLIT.  IN THIS CASE WE WANT TO SHOW
     C*  THE PROPER EFF DT, DESCRIPTION AND RATE FOR WHATEVER YEAR
     C*  AND MONTH THE SELF-REPORTING MANUAL PREMIUM IS BEING
     C*  CALCULATED.
     C*
     C                   Z-ADD     EPEFDT        SED(X)
     C                   END
     C*
     C                   END
     C*
     C     EPKEY2        READE     PEP01                                  60
     C                   END
     C*
     C                   Z-ADD     1             Z                 3 0
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '0'           *IN60
     C                   EXSR      LOADB
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C     ELIFFL        IFEQ      'Y'
     C                   Z-ADD     *ZERO         SCISFE
     C                   ENDIF
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
      *
      *   F7=A/R Inquiry
      *
     C     *INKG         IFEQ      *ON
      *
     C     Q2RECF        IFEQ      'SCR02'
     C     @@RRN         CHAIN     WDELP                              60
     C                   MOVE      ELCO#         #1CO#             3
     C                   MOVE      ELFND         #1FND             3
     C                   MOVE      ELEMP#        #1EMP#            9
     C                   MOVE      ELDIV         #1DIV             5
     C                   MOVE      ELFYR         #1FYR             3
      *
     C                   CALL      'W0612R'                             99
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1EMP#            9
     C                   PARM                    #1DIV             5
     C                   PARM                    #1FYR             3
     C                   PARM      *ZEROS        #1DATE            8
     C                   PARM      *ZEROS        #1AM              9
     C                   PARM      *ZEROS        #1CHK#            7
     C                   PARM      *BLANK        #1RTC             1
      *
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*   COMMAND 10 = TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBLCD'
     C                   MOVE      *BLANKS       X1KEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    X1KEY
     C*
     C     X1KEY         IFNE      *BLANKS
     C                   MOVEL     X1KEY         SCBLCD
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     X1KEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN31
     C*
     C                   ELSE
     C*
     C     TBKY          CHAIN     PTT01                              60
     C  N60              MOVEL     TTDESC        SCBLDS
     C                   MOVE      '1'           *IN43
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCPYCD'
     C                   MOVE      *BLANKS       X1KEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    X1KEY
     C*
     C     X1KEY         IFNE      *BLANKS
     C                   MOVEL     X1KEY         SCPYCD
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     X1KEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN33
     C*
     C                   ELSE
     C*
     C     TBKY          CHAIN     PTT01                              60
     C  N60              MOVEL     TTDESC        SCPYDS
     C                   MOVE      '1'           *IN41
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCTXBC'
     C                   MOVE      *BLANKS       X1KEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    X1KEY
     C*
     C     X1KEY         IFNE      *BLANKS
     C                   MOVEL     X1KEY         SCTXBC
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     X1KEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN37
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN42
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCTXPC'
     C                   MOVE      *BLANKS       X1KEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    X1KEY
     C*
     C     X1KEY         IFNE      *BLANKS
     C                   MOVEL     X1KEY         SCTXPC
     C                   ENDIF
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     X1KEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN40
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN32
     C                   ENDIF
     C*
     C                   OTHER
     C*
     C                   ENDSL
     C*
     C* ENSURE DATES DISPLAY CORRECTLY
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*   COMMAND 15 = CALCULATE BILL
     C*
     C     *INKP         IFEQ      '1'
     C*
     C*   INITIALIZE WORK FIELDS
     C*
     C                   Z-ADD     0             XXPRCT            5 4
     C                   Z-ADD     0             XXPRC2            5 4
     C                   Z-ADD     0             XXPCT             5 4
     C                   Z-ADD     0             XXWK92           11 2
     C                   MOVE      '0'           *IN93
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*  INITIALIZE SCREEN ARRAYS
     C*
     C                   CLEAR                   HDG
     C                   CLEAR                   TAM
     C*
     C     *IN46         IFEQ      *OFF
     C     SRKY1         CHAIN     PSR01                              60
     C                   ELSE
     C     SRKY3         CHAIN     PSR02                              60
     C                   ENDIF
      *
     C     *IN60         DOWEQ     '0'
     C     SRSTAT        IFEQ      'L'
     C                   MOVE      '1'           *IN93
     C                   END
      *
     C     *IN46         IFEQ      *OFF
     C     SRKY1         READE     PSR01                                  60
     C                   ELSE
     C     SRKY3         READE     PSR02                                  60
     C                   ENDIF
     C*
     C                   END
     C*
     C*
     C*  LOAD SCREEN ARRAYS
     C*
     C                   Z-ADD     1             A                 3 0
     C*
     C*  MANUAL PREMIUM
     C*
     C     *IN63         IFEQ      *OFF
     C  N49              MOVEL     CON(1)        HDG(A)
     C   49              MOVEL     CON(12)       HDG(A)
     C                   Z-ADD     SCMAPR        TAM(A)
     C                   ADD       SCADLD        TAM(A)
     C                   Z-ADD     TAM(A)        SCBLAM
     C*
     C                   ADD       1             A
     C*
     C*  SET FIRST DAY OF MONTH IF NOT WEEKLY REPORTING
     C*
     C     *IN46         IFEQ      *OFF
     C                   Z-ADD     XXRPDT        XXRPFM            7 0
     C                   MOVE      01            XXRPFM
     C*
     C     XXRPYM        IFEQ      XXFPYM
     C                   MOVE      XXFPDY        XXRPFM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  INCREASED LIMITS
     C*
     C                   Z-ADD     *ZERO         LMPCT
     C*
     C     ELILCD        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     ELILCD        XXKEY
     C*
     C     ELILEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELILEF        XXILEF
     C                   ENDIF
     C*
     C     ELILEX        IFEQ      *ZERO
     C     ELI2EF        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELI2EF        XXILEX
     C                   ENDIF
     C                   ELSE
     C                   MOVE      ELILEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELILEX        ANDNE     ELILEF
     C     ELILEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELILEX        ANDEQ     *ZERO
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LMPCT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LMPCT         MULT      .01           XXPRCT
     C     XXPRCT        MULT(H)   SCBLAM        XXRND            11 0
     C                   Z-ADD     XXRND         TAM(A)
     C*
     C                   Z-ADD     0             LMPCT
     C     ELINL2        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     ELINL2        XXKEY
     C*
     C     ELI2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELI2EF        XXILEF
     C                   ENDIF
     C*
     C     ELI2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELI2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELI2EX        ANDNE     ELI2EF
     C     ELI2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELI2EX        ANDEQ     *ZERO
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LMPCT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LMPCT         MULT      .01           XXPRC2
     C     XXPRC2        MULT(H)   SCBLAM        XXRND            11 0
     C     XXPRC2        ADD       XXPRCT        XXPCT
     C     XXPCT         MULT      100           LMPCT
     C                   ADD       XXRND         TAM(A)
     C                   ADD       TAM(A)        SCBLAM
     C                   MOVEA     CON(A)        TMP
     C                   MOVE      X'22'         TMP(22)
     C                   MOVE      '%'           TMP(29)
     C                   MOVE      '.'           TMP(25)
     C                   MOVE      LMPCT         XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(23)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(24)
     C                   END
     C                   MOVE      NUM(3)        TMP(26)
     C                   MOVE      NUM(4)        TMP(27)
     C                   MOVE      NUM(5)        TMP(28)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C*  SUBTOTAL
     C*
     C                   MOVEL     CON(A)        HDG(A)
     C                   Z-ADD     SCBLAM        TAM(A)
     C                   ADD       1             A
     C*
     C*  CALCULATE DISCOUNT FACTOR BEFORE MODIFIER
     C*
     C                   Z-ADD     1             XXFACT           11 9
     C*
     C                   Z-ADD     1             XXFAC1           11 9
     C*
     C*  SET UP FLAG TO CALCULATE AMOUNT OF DISCOUNT TO APPLY TO FLAT CHARGE
     C*  OR SPECIFIC WAIVER. XXWFAC HOLDS THE DISCOUNT APPLIED TO THE CHARGE
     C*
     C                   Z-ADD     *HIVAL        XXWVFL
     C                   Z-ADD     1             XXWFAC           11 9
     C     ELWVPC        IFEQ      0
     C     ELEWAV        ANDNE     0
     C     ELEWAV        ORNE      0
     C     ELWVTP        ANDEQ     'S'
      *
      *  CONVERT PREMUIM LEVEL OF WAIVER TO A NUMBER USED TO DETERMINE WHAT
      *  FACTORS APPLY TO THE WAIVER CHARGE
      *
     C                   SELECT
     C     ELWVFL        WHENEQ    'M'
     C                   Z-ADD     1             XXWVFL            1 0
     C     ELWVFL        WHENEQ    'J'
     C                   Z-ADD     2             XXWVFL
     C     ELWVFL        WHENEQ    'E'
     C                   Z-ADD     3             XXWVFL
     C     ELWVFL        WHENEQ    'S'
     C                   Z-ADD     4             XXWVFL
     C     ELWVFL        WHENEQ    'D'
     C                   Z-ADD     5             XXWVFL
     C                   OTHER
     C                   Z-ADD     9             XXWVFL
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVTP        ANDEQ     'B'
     C     ELWVFL        ANDEQ     'M'
     C*
     C*  TO CALCULATE EFFECT OF WAIVER CHG ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE WAIVER CHG PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELWVPC        MULT      .01           XXFAC1
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFAC1
     C                   DIV(H)    XXPRO         XXFAC1
     C                   ENDIF
     C*
     C*  DEDUCTIBLE DISCOUNT
     C*
     C     ELDDCT        IFNE      *ZERO
     C     RRRDPR        ANDEQ     'M'
     C     LRGDFL        ANDNE     'Y'
     C*
     C     ELDCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDCEF        XXILEF
     C                   ENDIF
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELDCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C*
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELDDCT        MULT      -.01          XXFACT
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFACT
     C                   DIV(H)    XXPRO         XXFACT
     C                   ENDIF
     C     ELDD2P        IFNE      *ZERO
     C*
     C     EL2DEF        IFEQ      *ZERO
     C                   MOVE      ELDCEX        XXILEF
     C                   ELSE
     C                   MOVE      EL2DEF        XXILEF
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDNE     EL2DEF
     C     EL2DEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
     C*
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELDD2P        MULT      -.01          XXFAC2
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFAC2
     C                   DIV(H)    XXPRO         XXFAC2
     C*
     C                   ADD       XXFAC2        XXFACT
     C                   SUB       1             XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
      * MOVED ENDIF HERE SO THAT 2ND DED APPLIED TO MANUAL WOULD NOT BE
      * APPLIED UNLESS THERE IS A FIRST DED APPLIED TO MANUAL
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT MANUAL PREMIUM LEVEL
      *
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C     LDPLV1        ANDEQ     'M'
     C*
     C     LDEFD1        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD1        XXILEF
     C                   ENDIF
     C*
     C     LDEXP1        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP1        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDNE     LDEFD1
     C     LDEXP1        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT1
     C*
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     LDPCT1        MULT      -.01          XXFACT
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFACT
     C                   DIV(H)    XXPRO         XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C     LDPLV2        ANDEQ     'M'
     C*
     C     LDEFD2        IFEQ      *ZERO
     C                   MOVE      LDEXP1        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD2        XXILEF
     C                   ENDIF
     C*
     C     LDEXP2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDNE     LDEFD2
     C     LDEXP2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT2
     C*
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     LDPCT2        MULT      -.01          XXFAC2
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFAC2
     C                   DIV(H)    XXPRO         XXFAC2
     C*
     C                   ADD       XXFAC2        XXFACT
     C                   SUB       1             XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ADD       XXFAC1        XXFACT
     C                   SUB       1             XXFACT
     C*
     C*  HCN CREDIT
     C*
     C     ELHCN         IFNE      *ZERO
     C     ELHCN         ANDNE     1
     C*
     C                   Z-ADD     ELHCN         SVHCN
     C                   Z-ADD     1             XXHCN
     C                   MOVE      *BLANK        XXHCNF            1
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     SVHCN         ELHCN
     C*
     C     HCIN          IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      HCIN          XXILEF
     C                   ENDIF
     C*
     C     HCOUT         IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      HCOUT         XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     HCOUT         ANDNE     HCIN
     C     HCOUT         ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     HCOUT         ANDEQ     *ZERO
     C*
     C                   MOVE      'Y'           XXHCNF
     C                   SUB       1             ELHCN
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELHCN
     C*
     C                   ENDIF
     C*
     C                   ADD       ELHCN         XXHCN
     C*
     C                   ENDIF
     C*
     C     HCKY          READE     PHC01                                  60
     C                   ENDDO
     C*
     C     XXHCNF        IFEQ      'Y'
     C*
     C                   MULT(H)   XXHCN         XXFACT
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXHCN         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  MANAGED CARE
     C*
     C     ELASPM        IFNE      *ZERO
     C*
     C     ELMCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELMCEF        XXILEF
     C                   ENDIF
     C*
     C     ELMCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELMCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELMCEX        ANDNE     ELMCEF
     C     ELMCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELMCEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASPM
     C*
     C                   ENDIF
     C*
     C     ELASPM        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  SAFETY CREDIT
     C*
     C     ELASPS        IFNE      *ZERO
     C*
     C     ELSCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELSCEF        XXILEF
     C                   ENDIF
     C*
     C     ELSCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELSCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSCEX        ANDNE     ELSCEF
     C     ELSCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSCEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASPS
     C*
     C                   ENDIF
     C*
     C     ELASPS        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION - FROM SUBJECT
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'J'
     C     ELASP2        ORNE      *ZERO
     C     RRDV1L        ANDEQ     'M'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C*
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV1L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION 1 SECOND PERIOD - FROM SUBJECT
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'J'
     C     ELD1S2        ORNE      *ZERO
     C     RRDV1L        ANDEQ     'M'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C                   ENDIF
     C*
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV1L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - FROM SUBJECT
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C     ELDEV2        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDEQ     '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDEQ     '025'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDEQ     '136'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   EndSl
     C*
     C                   ENDIF
     C*
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxPro
     C                   Mult(H)   xxPro         xxFact
     C                   EndSl
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV2L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- FROM SUBJECT
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDNE     '025'
     C*
     C     ELD2S2        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDEQ     '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C     CLTID         IFNE      '025'
     C                   MULT(H)   XXPRO         ELD2S2
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CLTID         IFNE      '025'
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*FOR CLIENT 025
     C                   ELSE
     C                   MULT(H)   ELSTFC        XXFACT
     C                   Z-ADD     ELSTFC        XXPRO
     C                   ENDIF
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV2L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'J'
     C     ELWVTP        ANDEQ     'B'
     C*
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
      *
     C                   MOVE      'J'           XXPLV1            1
     C                   MOVE      'J'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
     C                   Z-ADD(H)  XXFACT        SCTA04
     C     SCBLAM        MULT(H)   SCTA04        XXRND
     C                   Z-ADD     XXRND         SCBLAM
     C                   MOVEL     CON(A)        HDG(A)
     C                   ADD       1             A
     C*
     C*  SUBTOTAL
     C*
     C                   MOVEL     CON(A)        HDG(A)
     C                   Z-ADD     SCBLAM        TAM(A)
     C                   ADD       1             A
     C*
     C*  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
     C*
     C     *LIKE         DEFINE    ELEXM1        XXEXMD
     C     *LIKE         DEFINE    ELARP1        XXARAP
     C                   Z-ADD     0             XXARAP
     C*
     C*  CALCULATE THE EXPERIENCE MODIFIER
     C*
     C     ELEXM1        IFNE      *ZERO
     C     ELEXM3        IFNE      0
     C     ELEMD3        ANDLT     XXRPDT
     C                   Z-ADD     ELEXM3        XXEXMD
     C                   ELSE
     C     ELEXM2        IFNE      0
     C     ELEMD2        ANDLT     XXRPDT
     C                   Z-ADD     ELEXM2        XXEXMD
     C                   ELSE
     C                   Z-ADD     ELEXM1        XXEXMD
     C                   END
     C                   END
     C*
     C*  CALCULATE THE ARAP MODIFIER
     C*
     C     ELEXM3        IFNE      0
     C     ELEMD3        ANDLE     XXRPDT
     C                   Z-ADD     ELARP3        XXARAP
     C                   ELSE
     C     ELEXM2        IFNE      0
     C     ELEMD2        ANDLE     XXRPDT
     C                   Z-ADD     ELARP2        XXARAP
     C                   ELSE
     C                   Z-ADD     ELARP1        XXARAP
     C                   END
     C                   END
     C*
     C     XXARAP        IFNE      *ZERO
     C                   SUB       1             XXARAP
     C                   ENDIF
     C*
     C                   Z-ADD     XXEXMD        SCTA06
     C     XXEXMD        MULT(H)   SCBLAM        XXRND
     C                   Z-ADD     XXRND         SCBLAM
     C                   MOVEL     CON(A)        HDG(A)
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXEXMD        XXWFAC
     C                   ENDIF
     C                   ENDIF
     C                   ADD       1             A
     C*
     C*  SUBTOTAL
     C*
     C                   MOVEL     CON(A)        HDG(A)
     C                   Z-ADD     SCBLAM        TAM(A)
     C                   ADD       1             A
     C*
     C*  CALCULATE DISCOUNT FACTOR AFTER MODIFIER
     C*
     C                   Z-ADD     1             XXFACT           11 9
     C*
     C     RRDCAP        IFEQ      ' '
     C                   EXSR      CALDBR
     C                   ENDIF
     C*
     C*  DEVIATION - APPLIED TO EXPERIENCE
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'E'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C*
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*  DEVIATION 1 SECOND PERIOD  APPLIED TO EXPERIENCE
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'E'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C*
     C                   ENDIF
     C*
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO EXPERIENCE
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDEQ     '025'
     C* Use Territory Rating for CLTID = '136'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   EndSl
     C*
     C                   ENDIF
     C*
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '071'
     C     svFact        Mult(H)   xxPro         s2Fact           11 9
     C     svFact        Sub       s2Fact        svFact
     C     xxFact        Sub       svFact        xxFact
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxPro
     C                   Mult(H)   xxPro         xxFact
     C                   EndSl
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- APPLIED TO EXPERIENCE
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C*
     C     CLTID         IFNE      '025'
     C                   MULT(H)   XXPRO         ELD2S2
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C                   ENDIF
     C*
     C                   ENDIF
     C     CLTID         IFNE      '025'
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C     CLTID         IFEQ      '071'
     C     SVFACT        MULT(H)   XXPRO         S2FACT           11 9
     C     SVFACT        SUB       S2FACT        SVFACT
     C     XXFACT        SUB       SVFACT        XXFACT
     C                   ELSE
     C                   MULT(H)   XXPRO         XXFACT
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C                   MULT(H)   ELSTFC        XXFACT
     C                   Z-ADD     ELSTFC        XXPRO
     C                   ENDIF
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C     RRDCAP        IFEQ      'Y'
     C                   EXSR      CALDBR
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'E'
     C     ELWVTP        ANDEQ     'B'
     C*
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
      *
     C                   MOVE      'E'           XXPLV1            1
     C                   MOVE      'E'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
     C*  CONTRACTORS CLASS
     C*
     C     ELASP1        IFNE      *ZERO
     C*
     C     ELCREF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELCREF        XXILEF
     C                   ENDIF
     C*
     C     ELCREX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELCREX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELCREX        ANDNE     ELCREF
     C     ELCREX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELCREX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP1
     C*
     C                   ENDIF
     C*
     C     ELASP1        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEDUCTIBLE DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDEDF           11 9
     C*
     C     ELDDCT        IFNE      *ZERO
     C     RRRDPR        ANDEQ     'S'
     C*
     C     ELDCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDCEF        XXILEF
     C                   ENDIF
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C*
     C                   ENDIF
     C*
     C     ELDDCT        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXFACT
     C*
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C     ELDD2P        IFNE      *ZERO
     C*
     C     EL2DEF        IFEQ      *ZERO
     C                   MOVE      ELDCEX        XXILEF
     C                   ELSE
     C                   MOVE      EL2DEF        XXILEF
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDNE     EL2DEF
     C     EL2DEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
     C*
     C                   ENDIF
     C*
     C     ELDD2P        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXFACT
     C*
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION - APPLIED TO STANDARD
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'S'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C*
     C                   MULT(H)   XXPRO         ELASP2
     C*
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION 1 SECOND PERIOD- APPLIED TO STANDARD
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'S'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C*
     C                   ENDIF
     C*
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO STANDARD
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDEQ     '025'
     C* Use Territory Rating for CLTID = '136'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   EndSl
     C*
     C                   ENDIF
     C*
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxPro
     C                   Mult(H)   xxPro         xxFact
     C                   EndSl
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C*  2ND DEVIATION 2ND PERIOD - APPLIED TO STANDARD
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C*
     C     CLTID         IFNE      '025'
     C                   MULT(H)   XXPRO         ELD2S2
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CLTID         IFNE      '025'
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*FOR CLIENT 025
     C                   ELSE
     C                   MULT(H)   ELSTFC        XXFACT
     C                   Z-ADD     ELSTFC        XXPRO
     C                   ENDIF
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C     SCBLAM        MULT      XXFACT        XXAMT3           15 4
      *
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'S'
     C     ELWVTP        ANDEQ     'B'
     C*
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
      *
     C                   MOVE      'S'           XXPLV1            1
     C                   MOVE      'S'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
     C*  DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDIS3            5 2
     C*
     C     ELASP3        IFNE      *ZERO
     C     ELDISC        ORNE      *BLANK
     C     ELEDC3        ANDNE     *ZERO
     C*
     C     ELASP3        IFNE      *ZERO
     C                   Z-ADD     ELASP3        XXDIS3
     C                   ENDIF
     C*
     C     ELDISC        IFNE      *BLANK
     C*
     C     RRDSPR        IFEQ      'M'
     C*
     C     RRDV1L        IFEQ      'S'
     C     ELSTPR        SUB       ELEDC2        XXRND
     C                   ELSE
     C                   Z-ADD     ELSTPR        XXRND
     C                   ENDIF
     C     RRDV2L        IFEQ      'S'
     C     XXRND         SUB       ELEDV2        XXRND
     C                   ENDIF
     C     ELWVFL        IFEQ      'S'
     C     XXRND         ADD       ELEWAV        XXRND
     C                   ENDIF
     C*
     C     XXRND         SUB       ELEDC3        XXRND2           11 0
     C     XXRND         IFNE      *ZERO
     C     XXRND2        DIV(H)    XXRND         XXPCNT
     C                   ELSE
     C                   Z-ADD     1             XXPCNT
     C                   ENDIF
     C*
     C     1             SUB       XXPCNT        XXPCNT
     C     XXPCNT        MULT      100           XXDIS3
     C*
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     ELSTPR        XXRND
     C                   ELSE
     C     RRDV1L        IFEQ      'S'
     C     ELSTPR        SUB       ELEDC2        XXRND
     C                   ELSE
     C                   Z-ADD     ELSTPR        XXRND
     C                   ENDIF
     C     RRDV2L        IFEQ      'S'
     C     XXRND         SUB       ELEDV2        XXRND
     C                   ENDIF
     C     ELWVFL        IFEQ      'S'
     C     XXRND         ADD       ELEWAV        XXRND
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRND         IFNE      *ZERO
     C     ELEDC3        DIV(H)    XXRND         XXPCNT
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPCNT            5 4
     C                   ENDIF
     C*
     C     XXPCNT        MULT      100           XXDIS3
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN93         IFEQ      *OFF
     C     XXDIS3        IFNE      *ZERO
     C     XXDIS3        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*  DEVIATION - APPLIED TO DISCOUNTED
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'D'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C*
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION 1 SECOND PERIOD- APPLIED TO DISCOUNTED
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'D'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C*
     C                   ENDIF
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO DISCOUNTED
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDEQ     '025'
     C* Use Territory Rating for CLTID = '136'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   EndSl
     C*
     C                   ENDIF
     C*
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxPro
     C                   Mult(H)   xxPro         xxFact
     C                   EndSl
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- APPLIED TO DISCOUNTED
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C*
     C     CLTID         IFNE      '025'
     C                   MULT(H)   XXPRO         ELD2S2
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CLTID         IFNE      '025'
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*FOR CLIENT 025
     C                   ELSE
     C                   MULT(H)   ELSTFC        XXFACT
     C                   Z-ADD     ELSTFC        XXPRO
     C                   ENDIF
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'D'
     C     ELWVTP        ANDEQ     'B'
     C*
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
      *
     C                   MOVE      'D'           XXPLV1            1
     C                   MOVE      'D'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
     C* AIRCRAFT REINSURANCE (ESTIMATED)
     C*
      *
     C     ELARCP        IFNE      *ZEROS
     C     ELARCP        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      6
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ADD       XXARAP        XXFACT
     C*
     C                   Z-ADD(H)  XXFACT        SCTA08
     C     SCBLAM        MULT(H)   SCTA08        XXRND
     C*
     C* IF A WAIVER SURCHARGE IS ON THE POLICY AND IT IS NOT A
     C* PERCENTAGE, THEN WE NEED TO ADD IN 1/12 OF THE CHARGE TO
     C* THE PREVIOUS BALANCE TO REFLECT IT AND PRINT A MESSAGE.
     C*
     C                   MOVE      *OFF          *IN71
     C     ELEWAV        IFNE      *ZERO
     C     ELWVPC        ANDEQ     *ZERO
     C     ELEWAV        ORNE      *ZERO
     C     ELWVTP        ANDEQ     'S'
     C*
     C     *IN46         IFEQ      *OFF
     C     ELEWAV        DIV(H)    12            XXPREM           11 0
     C                   ELSE
     C     XXDAYS        DIV       365           XXPRO
     C     ELEWAV        MULT(H)   XXPRO         XXPREM
     C                   ENDIF
     C*
     C     XXPREM        MULT(H)   XXWFAC        XXPREM
     C                   Z-ADD     XXPREM        SCWVAM
     C                   ADD       XXPREM        XXRND
     C                   MOVE      *ON           *IN71
     C*
     C                   ENDIF
     C*
     C* IF A REINSURANCE CHARGE IS ON THE POLICY AND IT IS NOT A
     C* PERCENTAGE, THEN WE NEED TO ADD IN 1/12 OF THE CHARGE TO
     C* THE PREVIOUS BALANCE TO REFLECT IT AND PRINT A MESSAGE.
     C*
     C                   MOVE      *OFF          *IN73
     C     ELEARC        IFNE      *ZERO
     C     ELARCP        ANDEQ     *ZERO
     C*
     C     *IN46         IFEQ      *OFF
     C     ELEARC        DIV(H)    12            XXPREM           11 0
     C                   ELSE
     C     XXDAYS        DIV       365           XXPRO
     C     ELEARC        MULT(H)   XXPRO         XXPREM
     C                   ENDIF
     C*
     C                   Z-ADD     XXPREM        SCARAM
     C                   ADD       XXPREM        XXRND
     C                   MOVE      *ON           *IN73
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   Z-ADD     8             A
     C  N49              MOVEL     CON(11)       HDG(A)
     C   49              MOVEL     CON(14)       HDG(A)
     C                   Z-ADD     SCMAPR        SCNETP
     C                   ADD       SCADLD        SCNETP
     C                   Z-ADD     SCNETP        SCBLAM
     C                   Z-ADD     SCNETP        XXRND
     C                   ADD       XXEWAV        XXRND
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C* CALCULATE TERRORISM RISK CHARGE AND ADD TO TOTAL PREMIUM AMT.
     C*
     C     X1EFDT        IFNE      *ZERO
     C     X1EFDT        ANDNE     X2EFDT
     C                   MOVE      X1EFDT        X2EFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        X2EFDT
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C                   MOVEL(P)  'Foreign'     XXTERH
     C                   CAT       'Terror':1    XXTERH
     C                   CAT       'ism:':0      XXTERH
     C*
     C     RRTERH        IFNE      *BLANKS
     C                   MOVEL(P)  RRTERH        XXTERH
     C                   CAT       ':':0         XXTERH
     C                   ENDIF
     C                   MOVE      *OFF          *IN72
     C                   Z-ADD     0             XXPREM
     C*
     C     RRTERR        IFNE      *ZERO
     C     RRDTRR        ORNE      *ZERO
     C     XXPAYR        DIV(H)    100           XXPREM
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   RRTERC        QQCLAS
     C                   MOVE(P)   X2EFDT        QQEFDT
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
     C     RRTERL        IFNE      'Y'
      *
     C     RRTERR        MULT(H)   XXLCM         RRTERR
     C     RRDTRR        MULT(H)   XXLCM         RRDTRR
     C                   ENDIF
     C                   Z-ADD(H)  RRTERR        XXTERR
     C                   ADD(H)    RRDTRR        XXTERR
     C                   MULT(H)   XXTERR        XXPREM
     C                   ENDIF
     C*
     C     XXPREM        IFNE      *ZERO
     C*
     C                   Z-ADD     XXPREM        SCTERR
     C                   ADD       XXPREM        XXRND
     C                   MOVE      *ON           *IN72
     C*
     C                   ENDIF
     C*
     C                   MOVE      ELPFDT        X2EFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C                   Z-ADD     XXRND         SCBLAM
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     CON(A)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF
     C*
     C*  PREMIUM DUE
     C*
     C  N49              MOVEL     CON(A)        HDG(A)
     C   49              MOVEL     CON(13)       HDG(A)
     C                   Z-ADD     SCBLAM        TAM(A)
     C                   EXSR      GETPBL
     C                   Z-ADD     X3AMT         SCPRVB
     C     SCBLAM        SUB       X3AMT         XXBADJ
     C     XXBADJ        IFGT      *ZERO
     C                   Z-ADD     XXBADJ        SCPYAM
     C                   ENDIF
      *
     C     CLTID         IFEQ      '046'
     C*
     C     *IN93         IFEQ      *OFF
     C     XXDIS3        ANDNE     *ZERO
     C     F1DSCD        ANDNE     *BLANKS
     C     XXDIS3        MULT      XXAMT3        XXAMT3
     C     XXAMT3        DIV(H)    100           XXAMT3
     C                   Z-ADD(H)  XXAMT3        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT3
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXAMT3
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C     RRTXPC        IFNE      *ZERO
     C                   ADD       1             A
     C*
     C     RRTXPC        MULT(H)   SCBLAM        XXAMT            15 4
     C     XXAMT         MULT(H)   .01           XXTAX            11 0
     C                   Z-ADD     XXTAX         TAM(A)
     C*
     C     XXBADJ        IFNE      *ZERO
     C     RRTXPC        MULT      XXBADJ        XXAMT
     C     XXAMT         MULT(H)   .01           XXTAX            11 0
     C                   ENDIF
     C*
     C     RRTXHD        IFNE      *BLANKS
     C                   MOVEA     RRTXHD        TMP
     C                   ELSE
     C                   MOVEA     CON(10)       TMP
     C                   ENDIF
     C*
     C                   MOVE      X'22'         TMP(21)
     C                   MOVE      '%'           TMP(29)
     C                   MOVE      '.'           TMP(24)
     C                   MOVE      RRTXPC        X6ASP1            6
     C                   MOVEA     X6ASP1        NUM6
     C*
     C     NUM6(1)       IFNE      '0'
     C                   MOVE      NUM6(1)       TMP(22)
     C                   ENDIF
     C*
     C     NUM6(2)       IFNE      '0'
     C     NUM6(1)       ORGT      '0'
     C                   MOVE      NUM6(2)       TMP(23)
     C                   ENDIF
     C*
     C                   MOVE      NUM6(3)       TMP(25)
     C                   MOVE      NUM6(4)       TMP(26)
     C                   MOVE      NUM6(5)       TMP(27)
     C                   MOVE      NUM6(6)       TMP(28)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C*
     C                   Z-ADD     XXTAX         SCTXBA
     C     XXTAX         IFGT      *ZERO
     C                   Z-ADD     XXTAX         SCTXPA
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANKS       SCTXBC
     C                   Z-ADD     0             SCTXBA
     C                   Z-ADD     0             SCTXPA
     C                   ENDIF
      *
     C                   Z-ADD     XXBADJ        SCBLAM
      *
     C                   MOVE      *BLANKS       SCTXPC
     C                   MOVE      'Y'           SCBLAR
     C                   MOVE      'N'           SCPYAR
     C                   MOVE      *BLANKS       SCPYCD
     C                   MOVE      *BLANKS       SCBLDS
     C                   MOVE      *BLANKS       SCPYDS
     C*
     C* CHECK IF DATE IS FEB. 29 AND LEAP YEAR
     C*
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1RPDT            8 0
     C*
     C                   Z-ADD     X1RPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1RPDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SUB       1             XXDAY
     C                   ENDIF
     C*
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT
     C                   Z-ADD     0             SCCHK#
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C     TOP           SUB       1             Z
     C                   MOVE      '0'           *IN60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C                   GOTO      TERM
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*  BILLING FLAG MUST BE 'Y' OR 'N'.
     C*  IF POSTING A BILL, EDIT BILLING ENTRY FIELDS
     C*
     C     SCBLAR        IFNE      'Y'
     C     SCBLAR        ANDNE     'N'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCBLAR        IFEQ      'Y'
     C*
     C*  AMOUNT CANNOT BE LESS THAN TO ZERO
     C*
     C*  BILLING CODE MUST BE A 'B' TYPE AND ACTIVE
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     SCBLCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      '1'
     C     TTTYPE        ORNE      'B'
     C     TTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE BILLING CODE DESCRIPTION IS BLANK LOAD THE DESCRIPTION
     C*  FROM THE TABLE FILE
     C*
     C     SCBLDS        IFEQ      *BLANKS
     C                   MOVEL     TTDESC        SCBLDS
     C                   END
     C     *LIKE         DEFINE    SCBLDS        XXBCDC
     C*
     C*  TAX BILLED AMOUNT CANNOT BE LESS THAN ZERO
     C*
     C*
     C*  BILLING CODE MUST BE A 'F' TYPE, 'T' CATEGORY, AND ACTIVE
     C*
     C     SCTXBC        IFNE      *BLANKS
     C     SCTXBA        ORNE      *ZERO
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     SCTXBC        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      '1'
     C     TTTYPE        ORNE      'F'
     C     TTCTYP        ORNE      'T'
     C     TTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVEL(P)  TTDESC        XXBCDC
     C                   ENDIF
     C*
     C*  VALIDATE THE POSTING DATE
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C     SCBLAT        IFNE      'Y'
     C     SCBLAT        ANDNE     *BLANK
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Display info window if billing transaction for this month
      *  has already been posted to the A/R.
      *
     C     X3AMT         IFNE      *ZERO
     C                   EXSR      INFWDW
     C                   ENDIF
     C*
     C                   END
     C*
     C*  PAYMENT FLAG MUST BE 'Y' OR 'N'.
     C*  IF POSTING A PAYMENT, EDIT PAYMENT ENTRY FIELDS
     C*
     C     SCPYAR        IFNE      'Y'
     C     SCPYAR        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     SCPYAR        IFEQ      'Y'
     C*
     C*  AMOUNT CANNOT BE LESS THAN OR EQUAL TO ZERO
     C*
     C     SCPYAM        CABLT     0             STRT                   26
     C*
     C*  PAYMENT CODE MUST BE A 'P' TYPE AND ACTIVE
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     SCPYCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      '1'
     C     TTTYPE        ORNE      'P'
     C     TTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE PAYMENT CODE DESCRIPTION IS BLANK LOAD THE DESCRIPTION
     C*  FROM THE TABLE FILE
     C*
     C     SCPYDS        IFEQ      *BLANKS
     C                   MOVEL     TTDESC        SCPYDS
     C                   END
     C     *LIKE         DEFINE    SCPYDS        XXPCDC
     C     *LIKE         DEFINE    EPEAPR        XXEAPR
     C*
     C*  TAX PAYMENT AMOUNT CANNOT BE LESS THAN ZERO
     C*
     C     SCTXPA        CABLT     0             STRT                   38
     C*
     C*  PAYMENT CODE MUST BE A 'Q' TYPE, 'T' CATEGORY, AND ACTIVE
     C*
     C     SCTXPC        IFNE      *BLANKS
     C     SCTXPA        ORNE      *ZERO
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     SCTXPC        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      '1'
     C     TTTYPE        ORNE      'Q'
     C     TTCTYP        ORNE      'T'
     C     TTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVEL(P)  TTDESC        XXPCDC
     C                   ENDIF
     C*
     C*  VALIDATE THE POSTING DATE
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*  EMPLOYER MUST HAVE ASSIGNED AGENT FOR THE FUND YEAR EFFECTIVE
     C*  AS OF THE SYSTEM DATE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT            6 0
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  55
     C     *IN55         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  55
     C                   END
     C     *IN55         IFEQ      '1'
     C     AHCO#         ORNE      XXCO#
     C     AHFND         ORNE      XXFND
     C     AHEMP#        ORNE      XXEMP#
     C     AHDIV         ORNE      XXDIV
     C     AHFYR         ORNE      XXFYR
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     SCPLAT        IFNE      'Y'
     C     SCPLAT        ANDNE     *BLANK
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORDS, GOTO THE
     C*  FIRST SCREEN
     C*
     C     SCBLAR        IFEQ      'Y'
     C*
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   ELSE
     C                   Z-ADD     XXDIV         EADIV
     C                   ENDIF
     C                   Z-ADD     XXDIV         EARDIV
     C                   MOVE      SCBLCD        EAARCD
     C                   MOVE      'B'           EAARTY
     C                   MOVEL     SCBLCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     SCBLDS        EADESC
     C                   Z-ADD     SCBLAM        EAAMT
      *
     C     CLTID         IFEQ      '046'
     C     EAAMT         ADD       XXAMT3        EAAMT
     C                   ENDIF
      *
     C                   MOVE      SCBLAT        EALATE
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   MOVE      ' '           EAEDIT
     C                   MOVE      ' '           EASTAT
     C                   MOVE      *BLANKS       EAUSER
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     0             EABUL1
     C*
     C                   EXSR      GETINV
     C                   WRITE     PEA01
      *
      *  WRITE INSTALLMENT FEE BILLING RECORD
      *
     C     SCISFE        IFNE      *ZERO
     C     ELIFFL        ANDEQ     *BLANKS
     C                   MOVE      SCIFBL        EAARCD
     C                   MOVE      'F'           EAARTY
     C                   MOVEL     SCIFBL        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     SCISFE        EAAMT
     C                   EXSR      GETINV
     C                   WRITE     PEA01
     C                   ENDIF
     C*
     C* WRITE DISCOUNT RECORD.
     C*
Q    C     CLTID         IFEQ      '046'
     C     XXAMT3        IFNE      *ZERO
     C*
     C                   MOVE      *BLANKS       X1KEY
     C                   MOVEL     F1DSCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  TTDESC        EADESC
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      *BLANKS       EACTYP
     C                   Z-SUB     XXAMT3        EAAMT
     C                   MOVE      F1DSCD        EAARCD
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   EXSR      GETINV
     C                   WRITE     PEA01
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
      *
     C     SCTXBC        IFNE      *BLANKS
     C                   MOVE      *BLANK        EALATE
     C                   MOVE      SCTXBC        EAARCD
     C                   MOVE      'F'           EAARTY
     C                   MOVE      'T'           EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     XXBCDC        EADESC
     C                   Z-ADD     SCTXBA        EAAMT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   EXSR      GETINV
     C                   WRITE     PEA01
     C                   ENDIF
     C                   END
     C*
     C     SCPYAR        IFEQ      'Y'
     C*
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   ELSE
     C                   Z-ADD     XXDIV         EADIV
     C                   ENDIF
     C                   Z-ADD     XXDIV         EARDIV
     C                   MOVE      SCPYCD        EAARCD
     C                   MOVE      'P'           EAARTY
     C                   MOVEL     SCPYCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVE      SCPLAT        EALATE
     C                   MOVEL     SCPYDS        EADESC
     C                   Z-ADD     SCPYAM        EAAMT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   MOVE      ' '           EAEDIT
     C                   MOVE      ' '           EASTAT
     C                   MOVE      *BLANKS       EAUSER
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     SCCHK#        EACHK#
     C*
     C                   EXSR      GETINV
     C                   WRITE     PEA01
      *
      *  WRITE INSTALLMENT FEE PAYMENT RECORD
      *
     C     SCISFE        IFNE      *ZERO
     C     ELIFFL        ANDEQ     *BLANKS
     C                   MOVE      SCIFPD        EAARCD
     C                   MOVE      'Q'           EAARTY
     C                   MOVEL     SCIFPD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     SCISFP        EAAMT
     C                   EXSR      GETINV
     C                   WRITE     PEA01
     C                   ENDIF
     C     SCTXPC        IFNE      *BLANKS
     C                   MOVE      *BLANK        EALATE
     C                   MOVE      SCTXPC        EAARCD
     C                   MOVE      'Q'           EAARTY
     C                   MOVE      'T'           EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     XXPCDC        EADESC
     C                   Z-ADD     SCTXPA        EAAMT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   EXSR      GETINV
     C                   WRITE     PEA01
     C                   ENDIF
     C                   END
      *
      * "Bill to A/R" Late Fee
      *
     C     SCBLAT        IFEQ      'Y'
     C     RRLATE        ANDEQ     'Y'
     C                   CLEAR                   PEA01
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C                   Z-ADD     XXDIV         EADIV
     C                   MOVE      RRLTCD        EAARCD
     C                   MOVE      'F'           EAARTY
     C                   MOVE      SCPLAT        EALATE
      *
     C                   MOVEL(P)  RRLTCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C  N60              MOVEL(P)  TTDESC        EADESC
     C                   MOVE      *BLANKS       EACTYP
      *
     C                   Z-ADD     RRLTAM        EAAMT
      *
     C                   ADD       1             DSTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
      *
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
      *
     C                   Z-ADD     SCCHK#        EACHK#
      *
     C                   EXSR      GETINV
     C                   WRITE     PEA01
     C                   ENDIF
     C*
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
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
     C                   ENDIF
     C*
     C                   ENDSR
     C*
      *
      *================================================================
      * GETPBL - Get 'B' type records written previously with same posting date
      *================================================================
     C     GETPBL        BEGSR
      *
     C                   Z-ADD     XXCO#         X3CO#
     C                   Z-ADD     XXFND         X3FND
     C                   Z-ADD     XXEMP#        X3EMP#
     C                   Z-ADD     XXDIV         X3DIV
     C                   Z-ADD     XXFYR         X3FYR
     C                   MOVE      'B'           X3ARTY
      *
     C                   Z-ADD     XXRPDT        X3PODT
      *
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         X3DIV
     C                   ENDIF
      *
     C                   Z-ADD     *ZERO         X3AMT            15 5
     C     EAKY          CHAIN     PEA02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDNE     'C'
     C     TTCTYP        ANDNE     'E'
     C     ELARSM        IFEQ      'Y'
     C     EARDIV        ANDEQ     XXDIV
     C     ELARSM        ORNE      'Y'
     C*
     C                   MOVEL(P)  EAARCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     EAARCD        ANDEQ     SCBLCD
     C                   ADD       EAAMT         X3AMT
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C     EAKY          READE     PEA02                                  60
     C                   ENDDO
      *
     C     X3AMT         IFNE      0
     C                   MOVE      *ON           *IN56
     C                   ENDIF
      *
      *  CHECK TO SEE IF AN INSTALLMENT FEE HAS ALREADY BEEN BILLED. IF SO,
      *  WRITE AN ADJUSTMENT. IF NO ADJUSTMNET IS TO BE WRITTEN, CONDITION
      *  THE DISPLAY SO THAT IT DOESN'T SHOW THE INSTALLMENT FEES BY TURNING
      *  *IN64 *OFF
      *
     C     SCISFE        IFNE      *ZERO
     C     ELIFFL        ANDEQ     *BLANKS
     C                   Z-ADD     *ZERO         XXISFE
     C                   MOVE      'F'           X3ARTY
     C     EAKY          CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
     C     ELARSM        IFEQ      'Y'
     C     EARDIV        ANDEQ     XXDIV
     C     ELARSM        ORNE      'Y'
     C     EAARCD        IFEQ      SCIFBL
     C                   ADD       EAAMT         XXISFE            7 2
     C                   ENDIF
     C                   ENDIF
     C     EAKY          READE     PEA02                                  60
     C                   ENDDO
     C     XXISFE        IFGE      SCISFE
     C                   Z-ADD     *ZERO         SCISFE
     C                   ELSE
     C                   SUB       XXISFE        SCISFE
     C                   ENDIF
     C     SCISFE        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN64
     C                   ELSE
     C                   Z-ADD     SCISFE        SCISFP
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *===============================================================
      *  INFWDW - DISPLAY WINDOW IF PREVIOUS BILLING EXISTS FOR PODT
      *===============================================================
     C     INFWDW        BEGSR
      *
     C     *INKL         DOWEQ     *OFF
     C     *INKF         ANDEQ     *OFF
     C                   EXFMT     WDW01
     C                   ENDDO
      *
     C                   SETOFF                                       KF
      *
     C     *INKL         IFEQ      *ON
     C                   SETOFF                                       KL
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDSR
     C*
     C     SRPREM        BEGSR
     D***  Beg Chg  ***  06/17/24 *****************************************
     C****************** Z-ADD     *ZEROS        XXAMRT            7 2
     C                   Z-ADD     *ZEROS        XXAMRT            8 3
     D***  End Chg  ***  06/17/24 *****************************************
     C                   Z-ADD     *ZEROS        XNAMRT            7 4
     C                   Z-ADD     *ZEROS        XXAMPY           11 2
     C                   Z-ADD     *ZEROS        XXAMPR            9 2
     C                   Z-ADD     EPCLAS        XXCLAS            5 0
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         Q1CO#             3
     C                   MOVE(P)   ELFND         Q1FND             3
     C                   MOVE(P)   ELFYR         Q1FYR             3
     C                   MOVE(P)   ELEMP#        Q1EMP#            9
     C                   MOVE(P)   ELDIV         Q1DIV             5
     C                   MOVE(P)   XXRPDT        Q1RPDT            7
     C                   MOVE(P)   'P'           Q1TYPE            1
     C                   MOVE(P)   XXCLAS        Q1CLAS            5
     C                   CALL      'SREFFD'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C                   PARM                    Q1RPDT
     C                   PARM                    Q1TYPE
     C                   PARM                    Q1CLAS
      *
     C                   MOVE      Q1RPDT        X1EFDT
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCO#         COMP      EPCO#                              6060
     C  N60PRCLAS        COMP      EPCLAS                             6060
     C  N60PRRTCD        COMP      XXRTCD                             6060
     C                   END
     C   60              MOVE      *BLANKS       PRDESC
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVEL     PRDESC        XXDESC           15
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       X1ST
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   MOVE(P)   EPEFDT        #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   XXCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X1EFDT
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        XXDESC
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        X1ST
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         #2CO#             3
     C                   MOVE(P)   ELFND         #2FND             3
     C                   MOVE(P)   ELFYR         #2FYR             3
     C                   MOVE(P)   ELEMP#        #2EMP#            9
     C                   MOVE(P)   ELDIV         #2DIV             5
     C                   MOVE(P)   EPEFDT        #2DATE            7
     C                   MOVE(P)   'P'           #2TYPE            1
     C                   MOVE(P)   XXCLAS        #2CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #2CO#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2EMP#
     C                   PARM                    #2DIV
     C                   PARM                    #2DATE
     C                   PARM                    #2TYPE
     C                   PARM                    #2CLAS
      *
     C                   MOVE      #2DATE        X1EFDT
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        XXDESC
     C                   ENDIF
     C     PXCODE        IFNE      *ZERO
     C     EPEXT         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN62
     C                   ENDIF
     C*
     C*  CHECK TO SEE IF EMPLOYER IS USING "A" RATING
     C*  AND IF THE CLASS CODE IS 'A' RATABLE
     C*
     C     ELSTAT        IFEQ      'Y'
     C     PRARFL        ANDEQ     'Y'
     C*
     C*  USE "A" RATE AS ENTERED THRU EMPLOYER PAYROLL MAINTENANCE
     C*
     C                   Z-ADD     EPARAT        XXARAT
     C*
     C                   ELSE
     C*
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   X1EFDT        QQEFDT
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
     C     PRRT          MULT(H)   XXLCM         PRRT
     C                   Z-ADD     PRRT          XXARAT
     C     EPSPEC        IFEQ      'F'
     C*
     C     PRLHCD        IFEQ      'N'
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     ELPFDT        X2EFDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     HEKY          SETGT     PHE01
     C     HEKY2         READPE    PHE01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXARAT        MULT      HELHNF        XXARAT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN51         IFEQ      *ON
     C                   MULT(H)   1.002         XXARAT
     C     X1EFDT        IFLT      1030101
     C     XXRPDT        ANDGT     1030101
     C                   MULT(H)   1.049         XXARAT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN63         IFEQ      *ON
     C     EPENTR        IFNE      *ZERO
     C     CLTID         ANDEQ     '136'
     C                   Z-ADD     EPENTR        XNARAT            7 4
     C                   ELSE
     C     XXARAT        MULT(H)   XXPROR        XNARAT            7 4
     C                   ENDIF
     C                   Z-ADD     XNARAT        XNAMRT
     C                   ENDIF
     C                   Z-ADD     XXARAT        XXAMRT
     C*
     C     *IN46         IFEQ      *OFF
     C     SRKEY         CHAIN     PSR01                              60
     C                   ELSE
     C     SRKY2         CHAIN     PSR02                              60
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XXWK11           13 4
     C                   Z-ADD     *ZEROS        XXTLSP           11 2
     C     *IN60         DOWEQ     '0'
     C     SRSPEC        IFEQ      EPSPEC
     C     PXCODE        IFNE      *ZERO
     C     EPEXT         ANDNE     *BLANKS
     C                   ADD       SRADLD        SCADLD
     C                   ENDIF
     C                   ADD       SRAMTP        XXTLSP
     C     PRPCF         IFEQ      'P'
     C                   ADD       SRAMTP        XXPAYR
     C     SRAMTP        DIV       100           X2WK11           13 4
     C                   ELSE
     C     CLTID         IFEQ      '102'
     C     PRLOGF        IFEQ      'Y'
     C                   CALL      SRLOGC
     C                   PARM                    PRCLAS
     C                   PARM      *OFF          QQFOND            1
     C                   PARM      *ZEROS        QQMPYR            7 5
     C                   PARM      *ZEROS        QQMRAT            7 5
     C                   PARM      *ZEROS        QQRPCL            5 0
     C     QQFOND        IFEQ      '1'
     C     SRAMTP        MULT(H)   QQMPYR        XXEAPR
     C     XXPAYR        ADD       XXEAPR        XXPAYR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     SRAMTP        X2WK11
     C                   END
     C                   ADD       X2WK11        XXWK11
     C                   END
      *
     C     *IN46         IFEQ      *OFF
     C     SRKEY         READE     PSR01                                  60
     C                   ELSE
     C     SRKY2         READE     PSR02                                  60
     C                   ENDIF
      *
     C                   END
      *
     C     XXWK11        MULT(H)   XXARAT        X1AMPR           11 4
     C     *IN63         IFEQ      *ON
     C     XXWK11        MULT(H)   XNARAT        X1AMPR
     C                   ENDIF
     C                   Z-ADD     XXTLSP        XXAMPY
     C                   Z-ADD(H)  X1AMPR        XXSAVE           11 0
     C                   Z-ADD     XXSAVE        XXAMPR
     C                   ENDSR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       ED
     C                   MOVE      *BLANKS       CL
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       PY
     C                   MOVE      *BLANKS       RT
     C                   MOVE      *BLANKS       RTN
     C                   MOVE      *BLANKS       AM
     C                   Z-ADD     *ZERO         I1                2 0
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     SED(Z)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ED(I1)
     C                   MOVEL     SCS(Z)        XXWK5A            5
     C                   MOVE      SCS(Z)        X2SPEC
     C                   MOVEA     XXWK5A        X0
     C                   Z-ADD     1             W                 3 0
     C     W             DOWLE     5
     C     X0(W)         ANDEQ     '0'
     C                   MOVE      ' '           X0(W)
     C                   ADD       1             W
     C                   END
     C                   MOVEA     X0            X2CLAS
     C                   MOVE      X2CLSP        CL(I1)
     C                   MOVE      SDS(Z)        DS(I1)
     C                   MOVE      SPY(Z)        PY(I1)
     C                   MOVE      SRT(Z)        RT(I1)
     C                   MOVE      SRTN(Z)       RTN(I1)
     C                   MOVE      SAM(Z)        AM(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     Z             TOP               3 0
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
     C                   ADD       1             Z
     C                   MOVE      '0'           *IN60
     C     Z             IFGT      Y
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      13                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     14            I1
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
     C                   SUB       1             Z
     C                   MOVE      '0'           *IN60
     C     Z             IFEQ      0
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     Z             TOP
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     1             Z
     C                   Z-ADD     1             TOP               3 0
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   ADD       1             Z
     C                   MOVE      '0'           *IN60
     C     Z             IFGT      Y
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      13                                   61
     C   61              GOTO      OVERB
     C     DONEB         TAG
     C                   ENDSR
     C*
     C*  SUBROUTINE TO PRORATE PREMIUM FACTORS
     C*
     C     SRPRO         BEGSR
     C*
     C                   MOVE      *ON           *IN74
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
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
     C                   ADD       1             QQDAYS
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     QQERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* PRORATE AGAINST THE NUMBER OF DAYS IN THE MONTH IF RPT FREQ NOT WEEKLY
     C*
     C     *IN46         IFEQ      *OFF
     C                   Z-ADD     XXRPMN        X                 3 0
     C                   Z-ADD     MD(X)         XXDAYS            5 0
     C     XXRPMN        IFEQ      2
     C     XXRPYR        DIV       4             XXDIVR            2 0
     C                   MVR                     XXREM             2 0
     C     XXREM         IFNE      *ZERO
     C                   Z-ADD     28            XXDAYS
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRPMN        IFEQ      XXFPMO
     C     XXRPYR        ANDEQ     XXFPYR
     C                   SUB       XXFPDY        XXDAYS
     C                   ADD       1             XXDAYS
     C*
     C     XXDAYS        IFEQ      *ZERO
     C                   ADD       1             XXDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*                                                AND XXRPYR = XXFPYR
     C     XXRPMN        IFEQ      XXTPMO
     C     XXRPYR        ANDEQ     XXTPYR
     C                   Z-ADD     XXTPDY        XXDAYS
     C                   ENDIF
     C*                                                AND XXRPYR = XXTPYR
     C                   ENDIF
     C     QQDAYS        DIV       XXDAYS        XXPRO            11 9
     C*
     C                   ENDSR
     C     CALDBR        BEGSR
     C*  SCHEDULE DB/CR
     C*
     C     ELDBCR        IFNE      *ZERO
     C*
     C     ELSDEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELSDEF        XXILEF
     C                   ENDIF
     C*
     C     ELSDEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELSDEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDNE     ELSDEF
     C     ELSDEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   SUB       1             ELDBCR
     C                   MULT(H)   XXPRO         ELDBCR
     C                   ADD       1             ELDBCR
     C*
     C                   ENDIF
     C*
     C                   MULT(H)   ELDBCR        XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   ELDBCR        XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '136'
     C     ELCMUL        ANDNE     *ZERO
     C                   MULT(H)   ELCMUL        XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   ELCMUL        XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     XXFACT        SVFACT           11 9
     C*
     C                   ENDSR
      *============================================================
      * APPLY LARGE DEDUCTIBLE DISCOUNT
      *============================================================
     C     APLGDD        BEGSR
      *
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C     LDPLV1        ANDEQ     XXPLV1
     C*
     C     LDEFD1        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD1        XXILEF
     C                   ENDIF
     C*
     C     LDEXP1        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP1        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDNE     LDEFD1
     C     LDEXP1        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT1
     C*
     C                   ENDIF
     C*
     C     LDPCT1        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C     LDPLV2        ANDEQ     XXPLV2
     C*
     C     LDEFD2        IFEQ      *ZERO
     C                   MOVE      LDEXP1        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD2        XXILEF
     C                   ENDIF
     C*
     C     LDEXP2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDNE     LDEFD2
     C     LDEXP2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT2
     C*
     C                   ENDIF
     C*
     C     LDPCT2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C                   ENDSR
      *============================================================
**
ROLL UP
ROLL DOWN
**
312931303130313130313031
**
Manual premium
Inc limits (1st/2nd)
Subtotal
Discount factor before mod
Subtotal
Experience modifier
Subtotal
Discount factor after mod
Total premium due
State Tax
Net premium
Manual contribution                                                        01/14
Total contribution due                                                     01/14
Net contribution                                                           01/14
