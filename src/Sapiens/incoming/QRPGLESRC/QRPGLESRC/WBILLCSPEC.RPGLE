     C*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/05/25 C:65  Support: 119669 (Sunz)                         *
     H*                Remove call to SREXCO - no need to loop over   *
     H*                every division in an API when WDMPP is going   *
     H*                to be updated later all the time.              *
     H*                                                               *
     H* 11/13/24 C:291 Project: WC2141                                *
     H*                Update the stock discount calculation to split *
     H*                up premium discount into buckets based on each *
     H*                periods total premium instead of number of    *
     H*                days in each period.                           *
     H*                                                               *
     H* 11/12/24 C:65  Support: 103922 (Amwins)                       *
     H*                Change premium after increased limits to be    *
     H*                unique for each assessment since premium may   *
     H*                not be the same as regular standard premium.   *
     H*                                                               *
     H* 11/11/24 C:300 Project: WC2985                                *
     H*                Apply Deductible Discount to Discounted Premium*
     H*                for W8010R and W7720R                          *
     H*                                                               *
     H* 09/24/24 C:65  Project: Phw#117596 (Prescient)                *
     H*                XXDDCT populated for large deductibles to      *
     H*                allow for proper exclusion on tax/assessments. *
     H*                Only process small deductible if code present. *
     H*                                                               *
     H* 08/14/24 C:291 Project: WC2974                                *
     H*                Calculate Balance to Increased Limit.          *
     H*                                                               *
     H* 04/18/24 C:291 Project: WC2961                                *
     H*                Apply Schedule DB/CR against Manual Premium    *
     H*                if the new dropdown option M (Manual) is       *
     H*                chosen on Statistical Report/Rating Info.      *
     H*                                                               *
     H* 12/01/23 C:65  Project: Phw#114374 (Vital)                    *
     H*                For AZ only process the schedule rating to     *
     H*                the premium before deviation if applied after  *
     H*                deviation.                                     *
     H*                                                               *
     H* 10/31/23 C:340 SUPPORT: 103922                                *
     H*                CORRECT SECOND DEDUCTIBLE CREDIT CALCULATION   *
     H*                                                               *
     H* 06/14/23 C:65  PROJECT: 150WFY (revision to 150WAR)           *
     H*                Change the target base for % of manual premium *
     H*                to be the premium before large deductible is   *
     H*                applied to modified premium for MI divisions.  *
     H*                                                               *
     H* 02/05/21 C:65  Project: Phw#94364 (THE)                       *
     H*                Process the coding for increased limits if     *
     H*                there is a valid code, not if there is a %.    *
     H*                                                               *
     H* 04/22/19 C:65  PROJECT: WC2610                                *
     H*                During SRMIL call, run calculations through    *
     H*                modified premium and then save value to WDILP. *
     H*                                                               *
     H* 08/03/17 C:65  PROJECT: 150WAR                                *
     H*                Save off premium before large deductible is    *
     H*                applied to discounted premium for % of manual. *
     H*                                                               *
     H* 04/07/17 C:62  PROJECT 136WHF                                 *
     H*                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 08/17/16 C:65  PROJECT: Phw# 42547 (THE)                      *
     H*                Clear out increased limits amounts from all    *
     H*                arrays when minimum is to be invoked.          *
      *
      * 10/20/15 C:302 PROJECT: WC2364
      *                Add non-ratable class code processing
     H*                                                               *
     H* 10/07/15 C:65  PROJECT: Phw# 37696                            *
     H*                Calculate DE deductible in the same manner as  *
     H*                done for PA under supp# 49203 - calculate on   *
     H*                manual premium + increased limits.             *
     H*                                                               *
     H* 07/24/13 C:65  Project: WC2037                                *
     H*                Change array used for annualized premium.      *
     H*                                                               *
     H* 03/29/13 C:65  Project: 114WVY Correction                     *
     H*                The coding added for the 2nd period deviations *
     H*                was causing the 1st period to be skipped for   *
     H*                all additional premium arrays.                 *
     H*                                                               *
     H* 03/28/13 C:65  Project: WC2037                                *
     H*                Revamp the unearned premium surcharge calc.    *
     H*                Build the unearned premium bucket prior to     *
     H*                stock discount calculation and apply the       *
     H*                remaining premium calculations.                *
     H*                                                               *
     H* 05/26/11 C:191 Project: WC2038                                *
     H*                Account for new assessment calculations flags  *
     H*                                                               *
     H* 01/06/11 C:65  PROJECT: 139WPL      Priss#83                  *
     H*                Ensure HCN factor is not populated into emplr  *
     H*                detail if no record exists on policy.          *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                Add new calculations for HCN credit for GIC.   *
     H*                This credit will be applied after subject      *
     H*                premium and will have multiple periods. Each   *
     H*                period for the credit will calculate and the   *
     H*                discount will be totaled for the total discount*
     H*                amount.                                        *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                WHEN DEVIATIONS ARE SPLIT. 2ND PERIOD DEVIATIO-*
     H*                NS CHANGES WILL APPLY FOR (M,J,E,S,D) PREMIUM  *
     H*                LEVELS AS IT IS FOR 1ST PERIOD DEVIATIONS.     *
     H*                                                               *
     H* 02/25/10 C:65  PROJECT: 25WRC                                 *
     H*                Remove all changes related to EARS and net     *
     H*                net premium as none of this was intended for   *
     H*                base design.  Correct premium levels for       *
     H*                2nd deviation for TASB coding as well.         *
     H*                                                               *
     H* 01/14/10 C:191 PROJECT: 25WRC                                 *
     H*                BILLING ALGORITHM RETROFIT                     *
     H*                - For CLT025,  use stabilization factor to cal *
     H*                  the amount of change for dev#2               *
     H*                                                               *
     H* 01/12/10 C:65  PROJECT: WC1838                                *
     H*                LARGE DEDUCTIBLES - APPLY THE LARGE DEDUCTIBLE *
     H*                AT VARIOUS PREMIUM LEVELS WHEN APPLICABLE.     *
     H*                CORRECTIONS:                                   *
     H*                FIX 2ND DEDUCTIBLE APPLIED TO MANUAL TO APPLY  *
     H*                TO ALL PREMIUM ARRAYS AFTER 1ST DEDUCTIBLE     *
     H*                HAS CALCULATED AND USING AN UNALTERRED ARRAY   *
     H*                FOR PA POLICIES.                               *
     H*                FIX 2ND DEDUCTIBLE APPLIED TO STANDARD TO      *
     H*                PROCESS THE TAX ARRAY.                         *
     H*                FIX THE SCHEDULE DB/CR CODING TO MOVE THE      *
     H*                SETTING OF SAVE ARRAYS TO OUTSIDE DCAP COND.   *
     H*                                                               *
     H* 04/23/09 C:136 SUPPORT: WA114 68176                           *
     H*                CORRECT SECOND DEDUCTIBLE CREDIT CALCULATION   *
     H*                                                               *
     H* 01/05/09  C:33  Project: WC1769                               *
     H*                -Change to make a subroutine from original     *
     H*                  calcs so that other subroutines can be added *
     H*                  in the future.                               *
     H*                -Add WAIV subroutine.                          *
     H*                                                               *
     H* 12/11/08  C:65  PROJECT: WC1769                               *
     H*                 ADD PROCESSING TO CALCULATE THE 6TH AND 7TH   *
     H*                 ASSESSMENTS.                                  *
     H*                                                               *
     H* 10/15/08 C:65  SUPPORT: SUPP# 65598 (WA045 67220)             *
     H*                POPULATE DEVIATION #1 AMOUNT FOR STD PREMIUM   *
     H*                AND PREVENT IT FROM BEING APPLIED TWICE AT     *
     H*                DISCOUNTED PREMIUM.                            *
     H*                                                               *
     H* 02/12/08 C:136 SUPPORT: WA045 67220                           *
     H*                REMOVE CLIENT SPECIFIC PROCESSING FOR AUDIT    *
     H*                DISCOUNT REDUCTION                             *
     H*                                                               *
     H* 01/08/08 C:65  PROJECT: SUPP#61542                            *
     H*                MOVE THE SETTING OF A1E, A2E, ETC ARRAYS TO    *
     H*                PROPER LOCATION AFTER MODIFIED PREMIUM.        *
     H*                Implemented by C:33 after code was deleted.    *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                Implemented by C:33 after code was deleted.    *
     H*                                                               *
     H* 11/22/07 C:65  PROJECT: WC1104/SUPP#61220                     *
     H*                CORRECT DEVIATIONS APPLIED TO MANUAL TO STORE  *
     H*                AND UPDATE PREMIUM ARRAYS CORRECTLY.           *
     H*                                                               *
     H* 04/06/07  C:65  PROJECT: WC1514                               *
     H*                 ADD THE SHORT RATE PENALTY PREMIUM INTO       *
     H*                 ALGORTHMN BASED ON NEW STATISTICAL FLAG.      *
     H*                                                               *
     H* 03/31/07  C:132 PROJECT: 45WFN                                *
     H*                 ADD IN ADDITIONAL CALLS TO SRADR IF CLIENT 45 *
     H*                                                               *
     H* 02/22/07  C:132 PROJECT: 45WFN                                *
      *                 Back to Base on Audits                        *
     C*                                                               *
     C* 09/15/06  C:180 PROJECT: 114WMF                               *
      *
     C* 05/03/06  C:132 PROJECT: 46WFL                                *
     C*                 -ADD CLT046 MOD for Self-Reporting Discount   *
     C*                  adding up the Discount Records.              *
     C*                                                               *
     C* 04/13/06  C:168 PROJECT: 140WAO                               *
     C*                 -ADD NEW AUDIT DISCOUNT REDUCTION ROUTINE.    *
     C*                 -IF SELF-RPT, EFFECTIVE AT LEAST 1 MONTH.     *
     H*                                                               *
     H* 04/10/06 C:136 SUPPORT: WA136 52134 (136WBH CLIENT SPECIFIC)  *
     H*                CURRENTLY PREMIUM IS ZEROED IF NO HIT ON       *
     H*                COMMISSION MOD FACTOR TABLE. CHANGE THIS       *
     H*                SO THAT MOD FACTOR HAS NO AFFECT IF NO HIT.    *
     H*                                                               *
     H* 02/27/06 C:136 SUPPORT: WA136 51626                           *
     H*                IMPLEMENT WC1298 TO CALCULATE NET RATES        *
     H*                PROPERLY.                                      *
     H*                                                               *
     H* 12/23/05 C:136 PROJECT: 129WKB (MA DIA ASSESSMENT BREAKOUT)   *
     H*                ALLOW MA RULES TO APPLY TO ASSESSMENT PREM     *
     H*                CALCULATION.                                   *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC CHANGES TO CREDIT/DEBIT FACTOR *
     H*                                                               *
     H* 10/28/05 C:65  PROJECT: SUPP# 49203                           *
     H*                FOR PA ONLY, DEDUCTIBLE DISCOUNT WILL BE       *
     H*                CALCULATED OFF MANUAL PREMIUM + INCR LIMITS.   *
     H*                ADDITIONALLY, WHEN CALCULATING PA ASSESSMENTS  *
     H*                ADD THE DEDUCTIBLE DISCOUNT BACK INTO THE      *
     H*                ASSESSMENT PREMIUM INSTEAD OF CALCULATING      *
     H*                WITHOUT IT.                                    *
     H*                                                               *
     H* 07/20/05 C:65  PROJECT: SUPP# 47194                           *
     H*                SET A FLAG FOR IF A POLICY INVOKES MINIMUM     *
     H*                INCR LIMITS AND IS NOT SHORT TERM FOR          *
     H*                CONCURRENCY.                                   *
     H*                                                               *
     H* 07/15/05 C:136 PROJECT 133WBS                                 *
     H*                ADD CODE FOR MAXIMUM WAIVER CHARGE             *
     H*                ADD CODE FOR 4TH AND 5TH ASSESSMENT WAIVER     *
     H*                PREMIUM LEVELS                                 *
     H*                                                               *
     H* 04/07/05 C:156PROJECT: 126WDK                                 *
     H*               PROJECT: 126WCD                                 *
     H*               MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.         *
     H*                                                               *
     H* 12/08/04 C:65 PROJECT: WC1180/WC870/SUPP#44911                *
     H*               DO NOT EXCLUDE SCHEDULE DB/CR FROM KY TAX CALC  *
     H*               IF THEY ARE CHARGING TAX ON DEDUCTIBLES.        *
     H*                                                               *
     H* 10/08/04 C:65 PROJECT: SUPP#43528                             *
     H*               REMOVE THE CX ON SAME DAY CHECK FOR INCREASED   *
     H*               LIMITS.  INCREASED LIMITS WILL CALC OVER ALL    *
     H*               DIVISIONS REGARDLESS TO ENSURE PROPER CALC.     *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                BILL VOLUNTARY LABOR CHARGE.                   *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                    *
     H*                Bill 4th and 5th assessments.                  *
     H*                                                               *
     H* 06/03/04 C:65 PROJECT: WC1180                                 *
     H*               CHECK THE BILL TAX ON DEDUCTIBLES FLAG FOR      *
     H*               INCLUDING DEDUCTIBLE DISCOUNT WITHIN TAX CALC.  *
     H*                                                               *
     H* 04/26/04 C:136 PROJECT: WC1169                                *
     H*                PROCESS OCCUPATIONAL DISEASE PAYROLL CLASS     *
     H*                                                               *
     H* 02/26/04 C:65  PROJECT: SUPPORT                               *
     H*                STORE AMOUNT OF PREMIUM BEFORE EXP MODIFIERS.  *
     H*                                                               *
     H* 01/15/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     C*****************************************************************
     C     BILLC         BEGSR
     C*
     C                   XFOOT(H)  MAN           XSMAN            11 0
     C     RRST          IFEQ      'MA'
     C                   Z-ADD     MAN           MANT
      *
     C     RR1IDV        IFNE      'Y'
     C                   Z-ADD     MAND          MAN1
     C                   ELSE
     C                   Z-ADD     MAN           MAN1
     C                   ENDIF
      *
     C     RR2IDV        IFNE      'Y'
     C                   Z-ADD     MAND          MAN2
     C                   ELSE
     C                   Z-ADD     MAN           MAN2
     C                   ENDIF
      *
     C     RR3IDV        IFNE      'Y'
     C                   Z-ADD     MAND          MAN3
     C                   ELSE
     C                   Z-ADD     MAN           MAN3
     C                   ENDIF
      *
     C     RR4IDV        IFNE      'Y'
     C                   Z-ADD     MAND          MAN4
     C                   ELSE
     C                   Z-ADD     MAN           MAN4
     C                   ENDIF
      *
     C     RR5IDV        IFNE      'Y'
     C                   Z-ADD     MAND          MAN5
     C                   ELSE
     C                   Z-ADD     MAN           MAN5
     C                   ENDIF
      *
     C     RR6IDV        IFNE      'Y'
     C                   Z-ADD     MAND          MAN6
     C                   ELSE
     C                   Z-ADD     MAN           MAN6
     C                   ENDIF
      *
     C     RR7IDV        IFNE      'Y'
     C                   Z-ADD     MAND          MAN7
     C                   ELSE
     C                   Z-ADD     MAN           MAN7
     C                   ENDIF
      *
     C                   ELSE
     C*
     C                   Z-ADD     MAN           MANT
     C                   Z-ADD     MAN           MAN1
     C                   Z-ADD     MAN           MAN2
     C                   Z-ADD     MAN           MAN3
     C                   Z-ADD     MAN           MAN4
     C                   Z-ADD     MAN           MAN5
     C                   Z-ADD     MAN           MAN6
     C                   Z-ADD     MAN           MAN7
     C                   ENDIF
     C*
     C* ADD SHORT RATE PENALTY PREMIUM TO SUBJECT PREMIUM.
     C*
     C     RRSRAF        IFEQ      'M'
     C     XXSRFL        ANDEQ     'Y'
     C     RRSRAF        OREQ      ' '
     C     XXSRFL        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C     MAN(X)        MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C     XXSRPR        SUB       MAN(X)        SRP(X)
     C                   Z-ADD     XXSRPR        MAN(X)
     C*
     C     MANT(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MANT(X)
     C*
     C     MAN1(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MAN1(X)
     C*
     C     MAN2(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MAN2(X)
     C*
     C     MAN3(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MAN3(X)
     C*
     C     MAN4(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MAN4(X)
     C*
     C     MAN5(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MAN5(X)
     C*
     C     MAN6(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MAN6(X)
     C*
     C     MAN7(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        MAN7(X)
     C*
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*
     C* APPLY DEVIATION #1 TO MANUAL PREMIUM.
     C*
     C                   Z-ADD     *ZERO         XXDIS2           11 0
     C*** BEG ADD *****  06/21/10  ************************************
     C                   Z-ADD     *ZERO         XXD1D2           11 0
     C                   Z-ADD     *ZERO         XXD2D2           11 0
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     RRDV1L        IFEQ      'M'
     C     ELASP2        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELASP2        XXPCT
     C                   Z-ADD     MAN           PRM
     C                   Z-ADD     ELDVEF        XXEFD
     C     ELDVEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELDVEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDC2
     C                   Z-ADD     PRM           MAN
     C                   Z-ADD(H)  XXDISA        XXDIS2
     C*** begin add ***  03/29/13  **************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     MANT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MANT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     MAN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN1
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     MAN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN2
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     MAN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN3
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     MAN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN4
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     MAN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN5
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     MAN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN6
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     MAN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD1S2        IFNE      0
     C                   Z-ADD     ELD1S2        XXPCT
     C                   Z-ADD     ELD1D2        XXEFD
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD1X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     MAN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           D1E2
     C                   Z-ADD     PRM           MAN
     C                   Z-ADD(H)  XXDISA        XXD1D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     MANT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MANT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     MAN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     MAN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     MAN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     MAN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     MAN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     MAN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN6
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     MAN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN7
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C* APPLY DEVIATION #2 TO MANUAL PREMIUM.
     C*
     C                   Z-ADD     *ZERO         XXDEV2           11 0
     C*** Begin Add *** 04/07/17 ***************************************
     C                   Z-add     *zeros        xxDev2BP         11 2
     C                   Z-add     *zeros        xxDev2AP         11 2
     C*** End   Add *** 04/07/17 ***************************************
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****RRDV2L        IFEQ      'M'
     C*****ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*****XXCLT#        ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C*****RRDV2L        OREQ      'M'
     C*****XXCLT#        ANDEQ     '025'
     C*****ELSTFC        ANDNE     *ZERO
     C     RRDV2L        IFEQ      'M'
     C     ELDEV2        IFNE      *ZERO
     C     ELSTFC        ORNE      *ZERO
     C     ELDEV2F       ORNE      *ZERO
     C*** End   Chg *** 04/07/17 ***************************************
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****XXCLT#        IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C**********         Z-ADD     ELDEV2        XXPCT
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C**********         ELSE
     C*****ELSTFC        MULT(H)   -100          XXPCT
     C**********         ADD       100           XXPCT
     C**********         ENDIF                                                  XXCLT# <> '025'
     C                   Select
     C                   When      ELstFc  <> 0
     C                   Eval      xxPct = %DecH(ELstFc*-100:12:9) + 100
     C                   When      ELdev2F <> 0
     C                   Eval      xxPct = %DecH(ELdev2F*-100:12:9) + 100
     C                   When      ELdev2  <> 0
     C                   Eval      xxPct = ELdev2
     C                   EndSl
     C*** End   Chg *** 04/07/17 ***************************************
     C*** END   ADD ****** 01/14/10 ************************************
     C*
     C                   Z-ADD     MAN           PRM
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate Before Premium
     C                   Eval      xxDev2BP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     ELD2EF        XXEFD
     C     ELD2EX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2EX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDV2
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate After Premium
     C                   Eval      xxDev2AP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     PRM           MAN
     C                   Z-ADD(H)  XXDISA        XXDEV2
     C*** begin add ***  03/29/13  **************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     MANT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MANT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     MAN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN1
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     MAN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN2
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     MAN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN3
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     MAN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN4
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     MAN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN5
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     MAN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN6
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     MAN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD2S2        IFNE      0
     C                   Z-ADD     ELD2S2        XXPCT
     C                   Z-ADD     ELD2D2        XXEFD
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     MAN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           D2E2
     C                   Z-ADD     PRM           MAN
     C                   Z-ADD(H)  XXDISA        XXD2D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     MANT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MANT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     MAN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     MAN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     MAN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     MAN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     MAN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     MAN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN6
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     MAN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           MAN7
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD2S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*** Begin Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  ELDEV2/STFC/DEV2F
     C*** End   Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C                   Z-ADD     MANT          STNT
     C                   Z-ADD     MAN1          STN1
     C                   Z-ADD     MAN2          STN2
     C                   Z-ADD     MAN3          STN3
     C                   Z-ADD     MAN4          STN4
     C                   Z-ADD     MAN5          STN5
     C                   Z-ADD     MAN6          STN6
     C                   Z-ADD     MAN7          STN7
     C*
     C                   Z-ADD     MAN           STN

      *** Beg Add ***** 04/18/24 *************************************
     C                   Z-ADD     0             XXDISD           11 0
      * Schedule DB/CR applied to Manual Premium
     C                   If        RRdcap = 'M'
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELSDEF        XXEFD
     C     ELSDEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELSDEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        XXDISD
     C                   Z-ADD     DIS           EDC
     C                   Z-ADD     PRM           STN
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C     RRRDTX        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF                                                  RRST IFNE
     C*
     C     RRST          IFNE      'MA'
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C*
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF                                                  RRST <> 'KY'
     C                   ENDIF                                                  RRST <> 'MA'
     C                   ENDIF                                                  ELDBCR IFNE
     C                   Endif
      *** End Add ***** 04/18/24 *************************************
     C*
     C* WAVIER OF SUBROGRATION - APPLIED TO MANUAL
     C*
     C     ELWVFL        IFEQ      'M'
     C     ELWVTP        ANDNE     *BLANK
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     MAN           PRM
     C                   Z-ADD     MANT          PRMT
     C                   Z-ADD     MAN1          PRM1
     C                   Z-ADD     MAN2          PRM2
     C                   Z-ADD     MAN3          PRM3
     C                   Z-ADD     MAN4          PRM4
     C                   Z-ADD     MAN5          PRM5
     C                   Z-ADD     MAN6          PRM6
     C                   Z-ADD     MAN7          PRM7
     C                   Z-ADD     0             PRML
     C*
     C                   EXSR      WAIV
     C*
     C                   Z-ADD     0             WAVL
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       WAV(X)        STN(X)
     C                   ADD       WAVT(X)       STNT(X)
     C*** Beg Add *** 10/31/23 *** 103922 ****************************
     C                   If        RRST <> 'MA'
     C*** End Add *** 10/31/23 *** 103922 ****************************
     C                   ADD       WAV1(X)       STN1(X)
     C                   ADD       WAV2(X)       STN2(X)
     C                   ADD       WAV3(X)       STN3(X)
     C                   ADD       WAV4(X)       STN4(X)
     C                   ADD       WAV5(X)       STN5(X)
     C                   ADD       WAV6(X)       STN6(X)
     C                   ADD       WAV7(X)       STN7(X)
     C*** Beg Add *** 10/31/23 *** 103922 ****************************
     C                   EndIf
     C*** End Add *** 10/31/23 *** 103922 ****************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C                   Z-ADD     MAN           STOC
     C*
     C*  CHECK PRORATE FOR LIMITS CODES
     C*
     C                   Z-ADD     0             XXMIN1
     C                   Z-ADD     0             XXMIN2
     C                   MOVE      *BLANK        XLIM1F            1
     C                   MOVE      *BLANK        XLIM2F            1
     C*
     C     ELILCD        IFNE      *BLANK
     C*
     C*   GET LIMIT PERCENTAGE & MINIMUM
     C*
     C                   Z-ADD     0             LPR
     C                   Z-ADD     *ZERO         LMPCT
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     ELILCD        XXKEY
     C                   Z-ADD     ELPFDT        XXEFDT
     C     LMKEY1        SETGT     PLM01
     C*** begin chg ***  02/05/21  *****************************************
     C*****              READP     PLM01                                  60
     C******IN60         DOWEQ     '0'
     C*****LMSTAT        ANDEQ     'I'
     C*****              READP     PLM01                                  60
     C*****              ENDDO                                                  *IN60 DOWEQ
     C******IN60         IFNE      '0'
     C*****XXCO#         ORNE      LMCO#
     C*****XXFND         ORNE      LMFND
     C*****XXST          ORNE      LMST
     C*****XXKEY         ORNE      LMKEY
     C*****XXEFDT        ORLT      LMEFDT
     C*****              Z-ADD     *ZERO         LMPCT
     C*****              END
     C     LMKEY2        READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKEY2        READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   If        *in60 = *off
     C*** end chg *****  02/05/21  *****************************************
     C*
     C*  GET INCREASED LIMIT APPLIED TO EACH RATING PERIOD.  NEED TO
     C*  PRORATE THE TIME THAT THE INCR LIMIT IS IN EFFECT DURING EACH
     C*  PERIOD SEPARATELY.
     C*
     C     *LIKE         DEFINE    ELILEF        XXILEF
     C     ELILEF        IFEQ      *ZERO
     C                   Z-ADD     ELPFDT        XXILEF
     C                   ELSE
     C                   Z-ADD     ELILEF        XXILEF
     C                   ENDIF
     C*
     C     *LIKE         DEFINE    ELILEX        XXILEX
     C     ELILEX        IFEQ      *ZERO
     C     ELI2EF        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXILEX
     C                   ELSE
     C                   Z-ADD     ELI2EF        XXILEX
     C                   ENDIF                                                  ELI2EF IFEQ
     C                   ELSE
     C                   Z-ADD     ELILEX        XXILEX
     C                   ENDIF
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C*
     C*** BEGIN CHG ****** 01/14/10 ************************************
     C**** X ********    ADD       1             Y                 1 0
     C     X             ADD       1             Y                 3 0
     C*** END   CHG ****** 01/14/10 ************************************
     C     XXILEF        IFLT      PDAT(Y)
     C     XXILEX        ANDGT     PDAT(X)
     C*
     C                   Z-ADD     1             FACT1             5 4
     C                   Z-ADD     PRO(X)        LPR(X)
     C*
     C*  IF INCR LIMIT IS EFFECTIVE ONLY DURING PART OF RATING PERIOD
     C*  NEED TO CALCULATE THE PRORATA FOR THIS PERIOD, IN ORDER TO GET
     C*  THE CORRECT MINIMUM AMT FOR LIMIT.
     C*
     C     XXILEF        IFGT      PDAT(X)
     C     XXILEX        ORLT      PDAT(Y)
     C*
     C     XXILEF        IFGT      PDAT(X)
     C                   Z-ADD     XXILEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ELSE
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ENDIF
     C     XXILEX        IFLT      PDAT(Y)
     C                   Z-ADD     XXILEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ELSE
     C                   Z-ADD     PDAT(Y)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ENDIF
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
     C     QQDAYS        IFLT      *ZERO
     C                   MULT      -1            QQDAYS
     C                   ENDIF
     C*
     C     XXLPDT        IFNE      0
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO
     C     XXLPDT        IFGE      X1FROM
     C     XXLPDT        ANDLT     X1TO
     C                   SUB       1             QQDAYS
     C                   END
     C                   END
     C*
     C     DYS(X)        IFNE      *ZERO
     C     QQDAYS        DIV(H)    DYS(X)        FACT1
     C                   END
     C*
     C     PDAYS         IFNE      *ZERO
     C     QQDAYS        DIV(H)    PDAYS         LPR(X)
     C                   END
     C*
     C                   END                                                    ELILEF > PDAT,X
     C*
     C*   CALCULATE PRORATA USED TO GET THE AMT OF PAYROLL SUBJECT TO
     C*   INCR LIMITS FOR THIS TIME PERIOD.
     C*
     C     MAN(X)        MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM(X)
     C     MANT(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LMT(X)
     C     MAN1(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM1(X)
     C     MAN2(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM2A(X)
     C     MAN3(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM3(X)
     C     MAN4(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM4(X)
     C     MAN5(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM5(X)
     C     MAN6(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM6(X)
     C     MAN7(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM7(X)
     C*
     C                   END                                                    ELILEF < PDAT,Y
     C*
     C                   ADD       1             X
     C                   END                                                    RDAT,X DOWNE 0
     C*
     C*  CHECK INCREASED LIMIT MINIMUM
     C*
     C*** begin del ***  02/05/21  *****************************************
     C*****LMPCT         IFNE      *ZEROS
     C*** end del *****  02/05/21  *****************************************
     C                   XFOOT     LPR           XXWK54            5 4
     C     XXWK54        MULT(H)   XLMIN1        XXFMI2           15 4
     C     XLMIN2        IFEQ      *ZERO
     C                   Z-ADD     XLMIN1        XXFMI2
     C                   ENDIF
     C                   Z-ADD     XXFMI2        XXFMIN           15 4
     C                   XFOOT     LM            XXWK92           15 4
     C*
     C* IF DIVISION IS CANCELLED AND ENTIRE POLICY IS NOT CANCELLED
     C* ON THE SAME DAY, THEN ONLY USE THE DIVISION'S PREMIUM FOR
     C* DETERMINING INCR LIMITS MINIMUM.  OTHER DIVISIONS WILL EXCLUDE
     C* THIS DIVISION IN DETERMINING MINIMUM.
     C*
     C                   MOVE      *BLANK        XXL1CX            1
     C*
     C     DSKY1         CHAIN     PIL01                              60
     C*
     C                   Z-ADD     XXCO#         ILCO#
     C                   Z-ADD     XXFND         ILFND
     C                   Z-ADD     XXEMP#        ILEMP#
     C                   Z-ADD     XXDIV         ILDIV
     C                   Z-ADD     XXFYR         ILFYR
     C*** begin add ***  04/22/19  *************************************
     C                   Move      xxSt          ILSt
     C*** end add *****  04/22/19  *************************************
     C*
     C     BILL          IFEQ      'E'
     C                   Z-ADD     XXWK92        ILEIN1
     C                   MOVE      'E'           ILSTAT
     C                   ELSE
     C                   Z-ADD     XXWK92        ILAIN1
     C                   MOVE      'A'           ILSTAT
     C                   ENDIF                                                  BILL IFEQ
     C     XXL1CX        IFEQ      'Y'
     C                   Z-ADD     0             ILEIN1
     C                   Z-ADD     0             ILAIN1
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ILDT
     C                   Z-ADD     DSTIME        ILTIME
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PIL01
     C                   ELSE
     C                   WRITE     PIL01
     C                   ENDIF
     C*
     C     XXL1CX        IFNE      'Y'
     C     DSKY          CHAIN     PIL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ILDIV         IFNE      XXDIV
     C*
     C     ILSTAT        IFEQ      'E'
     C                   ADD       ILEIN1        XXWK92
     C                   ELSE
     C                   ADD       ILAIN1        XXWK92
     C                   ENDIF                                                  BILL IFEQ
     C*
     C                   ENDIF
     C*
     C     DSKY          READE     PIL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   ENDIF                                                  XXL1CX IFNE
      //*** Beg Chg ***** 08/14/24 ****************************************
      ***  XXWK92        IFLT      XXFMIN
     C***                Z-ADD     0             LM
     C*** begin add ***  08/17/16  *****************************************
     C***                Z-ADD     0             LMT
     C***                Z-ADD     0             LM1
     C***                Z-ADD     0             LM2A
     C***                Z-ADD     0             LM3
     C***                Z-ADD     0             LM4
     C***                Z-ADD     0             LM5
     C***                Z-ADD     0             LM6
     C***                Z-ADD     0             LM7
     C*** end add *****  08/17/16  *****************************************
     C***  XXDIV         IFEQ      XLDIV1
     C***  ELCOFL        IFNE      'Y'
     C***                MOVE      'Y'           XLIM1F
     C***                ENDIF
     C***                Z-ADD     1             X
     C***  RDAT(X)       DOWNE     0
     C***  XLMIN1        MULT(H)   LPR(X)        XXFMI2           15 4
     C***                Z-ADD(H)  XXFMI2        LM(X)
     C***                Z-ADD(H)  XXFMI2        LMT(X)
     C***                Z-ADD(H)  XXFMI2        LM1(X)
     C***                Z-ADD(H)  XXFMI2        LM2A(X)
     C***                Z-ADD(H)  XXFMI2        LM3(X)
     C***                Z-ADD(H)  XXFMI2        LM4(X)
     C***                Z-ADD(H)  XXFMI2        LM5(X)
     C***                Z-ADD(H)  XXFMI2        LM6(X)
     C***                Z-ADD(H)  XXFMI2        LM7(X)
     C***  XLMIN2        IFEQ      *ZERO
     C***  XXWK54        IFNE      *ZERO
     C***  LPR(X)        DIV(H)    XXWK54        XXFACT            5 4
     C***                ELSE
     C***                Z-ADD     0             XXFACT
     C***                ENDIF
     C***  XLMIN1        MULT(H)   XXFACT        XXFMI2           15 4
     C***                Z-ADD(H)  XXFMI2        LM(X)
     C***                Z-ADD(H)  XXFMI2        LMT(X)
     C***                Z-ADD(H)  XXFMI2        LM1(X)
     C***                Z-ADD(H)  XXFMI2        LM2A(X)
     C***                Z-ADD(H)  XXFMI2        LM3(X)
     C***                Z-ADD(H)  XXFMI2        LM4(X)
     C***                Z-ADD(H)  XXFMI2        LM5(X)
     C***                Z-ADD(H)  XXFMI2        LM6(X)
     C***                Z-ADD(H)  XXFMI2        LM7(X)
     C***                ENDIF
     C***                ADD       1             X
     C***                ENDDO                                                  RDAT  <> 0
     C***                ENDIF                                                  EMSTRP IFEQ
     C***                ENDIF                                                  WK92 < FMIN
     C***                Z-ADD(H)  XXFMIN        XXMIN1           11 0
     C***                ADD       XXMIN1        XXHIGH
     C*** begin del ***  02/05/21  *****************************************
     C*****              END                                                    LMPCT <> 0
     C*** end del *****  02/05/21  *****************************************
     C****
       If XXWK92 < XXFMIN;
          XLBal1 = XLMin1 - xxWK92;
          If XXDIV = XLDIV1;
             If ELCOFL <> 'Y';
                XLIM1F = 'Y';
             Endif;
             X = 1;
             Dow RDAT(X) <> 0;
                If XLMIN2 = *Zero;
                   If XXWK54 <> *Zero;
                      Eval(h) XXFact = LPR(X)/XXWK54;
                   Else;
                      XXFact = 0;
                   Endif;
                   Eval(h) xxFMI2 = XLBal1 * XXFACT;
                   LMB(X) = LMB(X) + XXFMI2;
                   LMT(X) = LMT(X) + XXFMI2;
                   LM1(X) = LM1(X) + XXFMI2;
                   LM2A(X) = LM2A(X) + XXFMI2;
                   LM3(X) = LM3(X) + XXFMI2;
                   LM4(X) = LM4(X) + XXFMI2;
                   LM5(X) = LM5(X) + XXFMI2;
                   LM6(X) = LM6(X) + XXFMI2;
                   LM7(X) = LM7(X) + XXFMI2;
                Else;
                 //Eval(h) xxFMI2 = XLBal1 * LPR(X);
                   Eval(h) xxFMI2 = XXFMIN - xxWK92;
                   LMB(X) = LMB(X) + XXFMI2;
                   LMT(X) = LMT(X) + XXFMI2;
                   LM1(X) = LM1(X) + XXFMI2;
                   LM2A(X) = LM2A(X) + XXFMI2;
                   LM3(X) = LM3(X) + XXFMI2;
                   LM4(X) = LM4(X) + XXFMI2;
                   LM5(X) = LM5(X) + XXFMI2;
                   LM6(X) = LM6(X) + XXFMI2;
                   LM7(X) = LM7(X) + XXFMI2;
                Endif;
                X += 1;
             Enddo;
          Endif;
       ENDIF;
       Eval(h) XXMIN1 = XXFMIN;
       XXHigh = XXHigh + XXMin1;
      //*** End Chg ***** 08/14/24 *******************************************

     C*  ADD INCREASED LIMIT TO STANDARD
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C                   ADD       LM(X)         STN(X)
     C                   ADD       LMT(X)        STNT(X)
     C                   ADD       LM1(X)        STN1(X)
     C                   ADD       LM2A(X)       STN2(X)
     C                   ADD       LM3(X)        STN3(X)
     C                   ADD       LM4(X)        STN4(X)
     C                   ADD       LM5(X)        STN5(X)
     C                   ADD       LM6(X)        STN6(X)
     C                   ADD       LM7(X)        STN7(X)
      //*** Beg Add ***** 08/14/24 ****************************************
       STN(X) = STN(X) + LMB(X);
      //*** End Add ***** 08/14/24 ****************************************
     C                   ADD       1             X
     C                   END
     C*
     C*** begin add ***  02/05/21  *****************************************
     C                   Endif
     C*** end add *****  02/05/21  *****************************************
     C                   END                                                    ELILCD NE *BLANK
     C************************************************************
     C*  CHECK FOR 2ND LIMITS CODES
     C*
     C     ELINL2        IFNE      *BLANK
     C*
     C*   GET LIMIT PERCENTAGE & MINIMUM
     C*
     C                   Z-ADD     0             LPR
     C                   Z-ADD     *ZERO         LMPCT
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     ELINL2        XXKEY
     C                   Z-ADD     ELPFDT        XXEFDT
     C     LMKEY1        SETGT     PLM01
     C*** begin chg ***  02/05/21  *****************************************
     C*****              READP     PLM01                                  60
     C******IN60         DOWEQ     '0'
     C*****LMSTAT        ANDEQ     'I'
     C*****              READP     PLM01                                  60
     C*****              ENDDO                                                  *IN60 DOWEQ
     C******IN60         IFNE      '0'
     C*****XXCO#         ORNE      LMCO#
     C*****XXFND         ORNE      LMFND
     C*****XXST          ORNE      LMST
     C*****XXKEY         ORNE      LMKEY
     C*****XXEFDT        ORLT      LMEFDT
     C*****              Z-ADD     *ZERO         LMPCT
     C*****              END
     C     LMKEY2        READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKEY2        READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   If        *in60 = *off
     C*** end chg *****  02/05/21  *****************************************
     C*
     C*  GET INCREASED LIMIT APPLIED TO EACH RATING PERIOD.  NEED TO
     C*  PRORATE THE TIME THAT THE INCR LIMIT IS IN EFFECT DURING EACH
     C*  PERIOD SEPARATELY.
     C*
     C                   Z-ADD     ELI2EF        XXILEF
     C*
     C     ELI2EX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXILEX
     C                   ELSE
     C                   Z-ADD     ELI2EX        XXILEX
     C                   ENDIF
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C*
     C     X             ADD       1             Y
     C     XXILEF        IFLT      PDAT(Y)
     C     XXILEX        ANDGT     PDAT(X)
     C*
     C                   Z-ADD     1             FACT1             5 4
     C                   Z-ADD     PRO(X)        LPR(X)
     C*
     C*  IF INCR LIMIT IS EFFECTIVE ONLY DURING PART OF RATING PERIOD
     C*  NEED TO CALCULATE THE PRORATA FOR THIS PERIOD, IN ORDER TO GET
     C*  THE CORRECT MINIMUM AMT FOR LIMIT.
     C*
     C     XXILEF        IFGT      PDAT(X)
     C     XXILEX        ORLT      PDAT(Y)
     C*
     C     XXILEF        IFGT      PDAT(X)
     C                   Z-ADD     XXILEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ELSE
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ENDIF
     C     XXILEX        IFLT      PDAT(Y)
     C                   Z-ADD     XXILEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ELSE
     C                   Z-ADD     PDAT(Y)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ENDIF
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
     C     QQDAYS        IFLT      *ZERO
     C                   MULT      -1            QQDAYS
     C                   ENDIF
     C*
     C     XXLPDT        IFNE      0
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO
     C     XXLPDT        IFGE      X1FROM
     C     XXLPDT        ANDLT     X1TO
     C                   SUB       1             QQDAYS
     C                   END
     C                   END
     C*
     C     DYS(X)        IFNE      *ZERO
     C     QQDAYS        DIV(H)    DYS(X)        FACT1
     C                   END
     C*
     C     PDAYS         IFNE      *ZERO
     C     QQDAYS        DIV(H)    PDAYS         LPR(X)
     C                   END
     C*
     C                   END                                                    ELILEF > PDAT,X
     C*
     C*   CALCULATE PRORATA USED TO GET THE AMT OF PAYROLL SUBJECT TO
     C*   2ND INCR LIMITS FOR THIS TIME PERIOD.
     C*
     C     MAN(X)        MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM2(X)
     C     MANT(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM2T(X)
     C     MAN1(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM21(X)
     C     MAN2(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM22(X)
     C     MAN3(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM23(X)
     C     MAN4(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM24(X)
     C     MAN5(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM25(X)
     C     MAN6(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM26(X)
     C     MAN7(X)       MULT(H)   FACT1         XXFGRS           15 4
     C                   MULT(H)   LMPCT         XXFGRS
     C     XXFGRS        DIV(H)    100           LM27(X)
     C*
     C                   END                                                    ELILEF < PDAT,Y
     C*
     C                   ADD       1             X
     C                   END                                                    RDAT,X DOWNE 0
     C*
     C*  CHECK INCREASED LIMIT MINIMUM
     C*
     C*** begin del ***  02/05/21  *****************************************
     C*****LMPCT         IFNE      *ZEROS
     C*** end del *****  02/05/21  *****************************************
     C                   XFOOT     LPR           XXWK54            5 4
     C     XXWK54        MULT(H)   XLMIN2        XXFMI2           15 4
     C                   Z-ADD     XXFMI2        XXFMIN           15 4
     C                   XFOOT     LM2           XXWK92           15 4
     C*
     C* IF DIVISION IS CANCELLED AND ENTIRE POLICY IS NOT CANCELLED
     C* ON THE SAME DAY, THEN ONLY USE THE DIVISION'S PREMIUM FOR
     C* DETERMINING INCR LIMITS MINIMUM.  OTHER DIVISIONS WILL EXCLUDE
     C* THIS DIVISION IN DETERMINING MINIMUM.
     C*
     C                   MOVE      *BLANK        XXL2CX            1
     C*
     C     DSKY1         CHAIN     PIL01                              60
     C*
     C                   Z-ADD     XXCO#         ILCO#
     C                   Z-ADD     XXFND         ILFND
     C                   Z-ADD     XXEMP#        ILEMP#
     C                   Z-ADD     XXDIV         ILDIV
     C                   Z-ADD     XXFYR         ILFYR
     C*** begin add ***  04/22/19  *************************************
     C                   Move      xxSt          ILSt
     C*** end add *****  04/22/19  *************************************
     C*
     C     BILL          IFEQ      'E'
     C                   Z-ADD     XXWK92        ILEIN2
     C                   MOVE      'E'           ILSTAT
     C                   ELSE
     C                   Z-ADD     XXWK92        ILAIN2
     C                   MOVE      'A'           ILSTAT
     C                   ENDIF                                                  BILL IFEQ
     C     XXL2CX        IFEQ      'Y'
     C                   Z-ADD     0             ILEIN2
     C                   Z-ADD     0             ILAIN2
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ILDT
     C                   Z-ADD     DSTIME        ILTIME
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PIL01
     C                   ELSE
     C                   WRITE     PIL01
     C                   ENDIF
     C*
     C     XXL2CX        IFNE      'Y'
     C     DSKY          CHAIN     PIL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ILDIV         IFNE      XXDIV
     C*
     C     ILSTAT        IFEQ      'E'
     C                   ADD       ILEIN2        XXWK92
     C                   ELSE
     C                   ADD       ILAIN2        XXWK92
     C                   ENDIF                                                  BILL IFEQ
     C*
     C                   ENDIF
     C*
     C     DSKY          READE     PIL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   ENDIF                                                  XXL2CX IFNE
     C     XXWK92        IFLT      XXFMIN
      //*** Beg Chg ***** 08/14/24 ****************************************
       XLBal2 = XLMin2 - xxWK92;
     C***                Z-ADD     0             LM2
     C*** begin add ***  08/17/16  *****************************************
     C***                Z-ADD     0             LM2T
     C***                Z-ADD     0             LM21
     C***                Z-ADD     0             LM22
     C***                Z-ADD     0             LM23
     C***                Z-ADD     0             LM24
     C***                Z-ADD     0             LM25
     C***                Z-ADD     0             LM26
     C***                Z-ADD     0             LM27
     C*** end add *****  08/17/16  *****************************************
      //*** End Chg ***** 08/14/24 *******************************************
     C     XXDIV         IFEQ      XLDIV2
     C     ELCOFL        IFNE      'Y'
     C                   MOVE      'Y'           XLIM2F
     C                   ENDIF
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
      *** Beg Chg ***** 08/14/24 *******************************************
     C***  XLMIN2        MULT(H)   LPR(X)        XXFMI2           15 4
     C***                Z-ADD(H)  XXFMI2        LM2(X)
     C***                Z-ADD(H)  XXFMI2        LM2T(X)
     C***                Z-ADD(H)  XXFMI2        LM21(X)
     C***                Z-ADD(H)  XXFMI2        LM22(X)
     C***                Z-ADD(H)  XXFMI2        LM23(X)
     C***                Z-ADD(H)  XXFMI2        LM24(X)
     C***                Z-ADD(H)  XXFMI2        LM25(X)
     C***                Z-ADD(H)  XXFMI2        LM26(X)
     C***                Z-ADD(H)  XXFMI2        LM27(X)
       //Eval(h) xxFMI2 = XLBal2 * LPR(X);
       Eval(h) xxFMI2 = XXFMIN - XXwk92;
       LMB2(X) = LMB2(X) + XXFMI2;
       LM2T(X) = LM2T(X) + XXFMI2;
       LM21(X) = LM21(X) + XXFMI2;
       LM22(X) = LM22(X) + XXFMI2;
       LM23(X) = LM23(X) + XXFMI2;
       LM24(X) = LM24(X) + XXFMI2;
       LM25(X) = LM25(X) + XXFMI2;
       LM26(X) = LM26(X) + XXFMI2;
       LM27(X) = LM27(X) + XXFMI2;
      *** End Chg ***** 08/14/24 *******************************************
     C                   ADD       1             X
     C                   END                                                    RDAT  <> 0
     C                   ENDIF                                                  EMSTRP IFEQ
     C                   END                                                    WK92 < FMIN
     C                   Z-ADD(H)  XXFMIN        XXMIN2           11 0
     C                   ADD       XXMIN2        XXHIGH
     C*** begin del ***  02/05/21  *****************************************
     C*****              END                                                    LMPCT <> 0
     C*** end del *****  02/05/21  *****************************************
     C*
     C*  ADD INCREASED LIMIT TO STANDARD
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C                   ADD       LM2(X)        STN(X)
     C                   ADD       LM2T(X)       STNT(X)
     C                   ADD       LM21(X)       STN1(X)
     C                   ADD       LM22(X)       STN2(X)
     C                   ADD       LM23(X)       STN3(X)
     C                   ADD       LM24(X)       STN4(X)
     C                   ADD       LM25(X)       STN5(X)
     C                   ADD       LM26(X)       STN6(X)
     C                   ADD       LM27(X)       STN7(X)
      //*** Beg Add ***** 08/14/24 ****************************************
       STN(X) = STN(X) + LMB2(X);
      //*** End Add ***** 08/14/24 ****************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C*** begin add ***  02/05/21  *****************************************
     C                   Endif
     C*** end add *****  02/05/21  *****************************************
     C                   ENDIF                                                  ELILCD NE *BLANK
     C*
     C*** BEGIN ADD ****** 05/26/11 ************************************
     C                   XFOOT(H)  STN           XXWK10
     C                   Z-ADD     XXWK10        XXLPRM
     C*** END   ADD ****** 05/26/11 ************************************
     C*** begin add ***  11/12/24  *****************************************
     C                   XFOOT(H)  STN1          XXWK10
     C                   Z-ADD     XXWK10        XXLPRM1
     C                   XFOOT(H)  STN2          XXWK10
     C                   Z-ADD     XXWK10        XXLPRM2
     C                   XFOOT(H)  STN3          XXWK10
     C                   Z-ADD     XXWK10        XXLPRM3
     C                   XFOOT(H)  STN4          XXWK10
     C                   Z-ADD     XXWK10        XXLPRM4
     C                   XFOOT(H)  STN5          XXWK10
     C                   Z-ADD     XXWK10        XXLPRM5
     C                   XFOOT(H)  STN6          XXWK10
     C                   Z-ADD     XXWK10        XXLPRM6
     C                   XFOOT(H)  STN7          XXWK10
     C                   Z-ADD     XXWK10        XXLPRM7
     C*** end add *****  11/12/24  *****************************************
     C*
     C* EXIT PROGRAM NOW IF CALLED FROM SRMIL.
     C*
     C     *IN96         IFEQ      *ON
     C*** begin add ***  04/22/19  *************************************
     C     BILL          ANDEQ     'A'
     C*** end add *****  04/22/19  *************************************
     C                   GOTO      END
     C                   ENDIF                                                  *IN96 IFEQ
      *
      *  VOLUNTARY LABOR CHARGE
      *
     C                   CLEAR                   XXVLCA
     C     ELVLCA        IFNE      *ZERO
     C     ELVLCA        MULT(H)   1             XXVLCA           11 0
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STNT(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN1(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN2(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN3(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN4(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN5(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN6(X)
     C     PRO(X)        MULT(H)   ELVLCA        XXWK10
     C                   ADD       XXWK10        STN7(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                            ELVLCA
     C*
     C*****************************************************************
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUM
     C*
     C                   Z-ADD     *ZERO         XXDIST           11 0
     C                   Z-ADD     *ZERO         XXD2ST           11 0
     C*** BEGIN ADD ***  01/12/10  **************************************
     C                   Z-ADD     STN           SVN
     C                   Z-ADD     STNT          SVNT
     C                   Z-ADD     STN1          SVN1
     C                   Z-ADD     STN2          SVN2
     C                   Z-ADD     STN3          SVN3
     C                   Z-ADD     STN4          SVN4
     C                   Z-ADD     STN5          SVN5
     C                   Z-ADD     STN6          SVN6
     C                   Z-ADD     STN7          SVN7
     C*** END ADD *****  01/12/10  **************************************
     C     RRRDPR        IFEQ      'M'
     C     WCFL06        ANDEQ     'Y'
     C     XXDDCT        ANDNE     *ZERO
     C*** begin add ***  09/24/24  **************************************
     C     ELDCDE        ANDNE     *blanks
     C*** end add *****  09/24/24  **************************************
     C                   Z-ADD     MAN           PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN           PRM
     C                   ENDIF
     C*** BEGIN CHG ***  01/12/10  **************************************
     C*************      Z-ADD     XXDDCT        XXPCT             9 6
     C                   Z-ADD     XXDDCT        XXPCT
     C*** END CHG *****  01/12/10  **************************************
     C                   Z-ADD     ELDCEF        XXEFD             7 0
     C     ELDCEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD             7 0
     C                   ELSE
     C                   Z-ADD     ELDCEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*** BEGIN DEL ***  01/12/10  **************************************
     C**** XXDD2P        IFNE      *ZERO
     C*
     C*********          Z-ADD     MAN           PRM
     C**** XXST          IFEQ      'PA'
     C*******            Z-ADD     STN           PRM
     C********           ENDIF
     C********           Z-ADD     XXDD2P        XXPCT             9 6
     C********           Z-ADD     EL2DEF        XXEFD             7 0
     C*
     C**** EL2DEX        IFEQ      *ZERO
     C********           Z-ADD     ELPTDT        XXEXD             7 0
     C********           ELSE
     C********           Z-ADD     EL2DEX        XXEXD
     C********           ENDIF
     C*
     C********           EXSR      DISCA
     C********           Z-ADD(H)  XXDISA        XXD2ST
     C********           Z-ADD     DIS           EDD2
     C********           Z-ADD     1             X
     C*
     C**** RDAT(X)       DOWNE     *ZERO
     C****               SUB       DIS(X)        STN(X)
     C**** RR1IDD        IFEQ      'Y'
     C**** XXST          OREQ      'PA'
     C****               SUB       DIS(X)        STN1(X)
     C****               ENDIF
     C**** RR2IDD        IFEQ      'Y'
     C**** XXST          OREQ      'PA'
     C****               SUB       DIS(X)        STN2(X)
     C****               ENDIF
     C**** RR3IDD        IFEQ      'Y'
     C**** XXST          OREQ      'PA'
     C****               SUB       DIS(X)        STN3(X)
     C****               ENDIF
     C****               ADD       1             X
     C****               ENDDO                                                  RDAT,X DOWNE
     C*
     C****               ENDIF
     C*** END DEL *****  01/12/10  **************************************
     C     RR1IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN1          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN1          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR1IDD IFEQ
     C*
     C     RR2IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN2          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN2          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR2IDD IFEQ
     C*
     C     RR3IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN3          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN3          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR3IDD IFEQ
      *
     C     RR4IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN4          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN4          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR4IDD IFEQ
      *
     C     RR5IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN5          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN5          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR5IDD IFEQ
     C     RR6IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN6          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN6          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR6IDD IFEQ
      *
     C     RR7IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN7          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN7          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR7IDD IFEQ
     C     RRRDTX        IFEQ      'Y'
     C                   Z-ADD     MANT          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STNT          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRRDTX IFEQ
     C*** BEGIN ADD ***  01/12/10  **************************************
     C     XXDD2P        IFNE      *ZERO
     C*
     C                   Z-ADD     MAN           PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN           PRM
     C                   ENDIF
     C                   Z-ADD     XXDD2P        XXPCT
     C                   Z-ADD     EL2DEF        XXEFD             7 0
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD             7 0
     C                   ELSE
     C                   Z-ADD     EL2DEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD     1             X
     C*
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RR1IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN1          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN1          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR1IDD IFEQ
     C*
     C     RR2IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN2          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN2          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR2IDD IFEQ
     C*
     C     RR3IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN3          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN3          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR3IDD IFEQ
      *
     C     RR4IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN4          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN4          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR4IDD IFEQ
      *
     C     RR5IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN5          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN5          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR5IDD IFEQ
     C*
     C     RR6IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN6          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN6          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR6IDD IFEQ
      *
     C     RR7IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     MAN7          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVN7          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR7IDD IFEQ
     C*
     C     RRRDTX        IFEQ      'Y'
     C                   Z-ADD     MANT          PRM
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     SVNT          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRRDTX IFEQ
     C*
     C                   ENDIF                                                  XXDD2P IFNE
     C*** END ADD *****  01/12/10  **************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  01/12/10  **************************************
     C     *LIKE         DEFINE    LDPCT1        XXPCT
     C*
     C                   Z-ADD     MAN           S2N
     C                   Z-ADD     MANT          S2NT
     C                   Z-ADD     MAN1          S2N1
     C                   Z-ADD     MAN2          S2N2
     C                   Z-ADD     MAN3          S2N3
     C                   Z-ADD     MAN4          S2N4
     C                   Z-ADD     MAN5          S2N5
     C                   Z-ADD     MAN6          S2N6
     C                   Z-ADD     MAN7          S2N7
     C*
     C     XXST          IFEQ      'PA'
     C*** BEGIN ADD ***  10/07/15  **************************************
     C     XXST          OREQ      'DE'
     C*** END ADD *****  10/07/15  **************************************
     C                   Z-ADD     STN           S2N
     C                   Z-ADD     STNT          S2NT
     C                   Z-ADD     STN1          S2N1
     C                   Z-ADD     STN2          S2N2
     C                   Z-ADD     STN3          S2N3
     C                   Z-ADD     STN4          S2N4
     C                   Z-ADD     STN5          S2N5
     C                   Z-ADD     STN6          S2N6
     C                   Z-ADD     STN7          S2N7
     C                   ENDIF
     C*
     C* APPLY 1ST LARGE DEDUCTIBLE TO MANUAL PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV1        ANDEQ     'M'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT1        XXPCT
     C                   Z-ADD     LDEFD1        XXEFD
     C     LDEXP1        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP1        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C* APPLY 2ND LARGE DEDUCTIBLE TO MANUAL PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV2        ANDEQ     'M'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT2        XXPCT
     C                   Z-ADD     LDEFD2        XXEFD
     C     LDEXP2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP2        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*** END ADD *****  01/12/10  **************************************
     C*** BEGIN ADD ***  11/10/10  ************************************
     C************************************************************
     C*  HCN CREDIT
     C*
     C                   Z-ADD     0             XXHCNA
     C                   Z-ADD     0             XXHCNF
     C*
     C                   IF        CLTID = '139'
     C*
     C     HCKEY         SETGT     PHC01
     C     HCKEY1        READPE    PHC01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     HCEXDT        ANDGT     XSPFDT
     C     *IN60         OREQ      *OFF
     C     HCEXDT        ANDEQ     *ZERO
     C*
     C     HCFACT        IFNE      *ZERO
     C     HCFACT        ANDNE     1
     C     HCAPPL        ANDNE     *BLANK
     C                   Z-ADD     HCFACT        XXHCNF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  CLTID = '139'
     C*
     C* HCN CREDIT APPLIED AT MANUAL PREMIUM.
     C*
     C     XXHCNF        IFNE      *ZERO
     C*
     C                   EXSR      SRHCN
     C*
     C                   ENDIF                                                  XXHCNF IFNE *ZERO
     C*** END ADD *****  11/10/10  ************************************
     C*
     C                   XFOOT     STN           XXSUBJ           15 4
     C                   XFOOT     STN1          X1SUBJ           15 4
     C                   XFOOT     STN2          X2SUBJ           15 4
     C                   XFOOT     STN3          X3SUBJ           15 4
     C                   Z-ADD     STN1          A1J
     C                   Z-ADD     STN2          A2J
     C                   Z-ADD     STN3          A3J
     C                   Z-ADD     STN4          A4J
     C                   Z-ADD     STN5          A5J
     C                   XFOOT     STN4          X4SUBJ           15 4
     C                   XFOOT     STN5          X5SUBJ           15 4
     C                   Z-ADD     STN6          A6J
     C                   Z-ADD     STN7          A7J
     C                   XFOOT     STN6          X6SUBJ           15 4
     C                   XFOOT     STN7          X7SUBJ           15 4
     C                   Z-ADD     STN           SUB
     C*
     C*
     C************************************************************
     C*  SUBJECT 1A CREDIT
     C*
     C                   Z-ADD     *ZERO         XXDISM           11 0
     C*
     C     ELASPM        IFNE      *ZERO
     C                   Z-ADD     ELASPM        XXPCT
     C                   Z-ADD     ELMCEF        XXEFD
     C                   Z-ADD     STN           PRM
     C     ELMCEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELMCEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCM
     C                   Z-ADD(H)  XXDISA        XXDISM
     C                   Z-ADD     PRM           STN
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C*
     C                   ENDIF                                                  ELASPM IFNE
     C*
     C************************************************************
     C*  SUBJECT 1B CREDIT
     C*
     C                   Z-ADD     *ZERO         XXDISS           11 0
     C*
     C     ELASPS        IFNE      *ZERO
     C                   Z-ADD     ELASPS        XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELSCEF        XXEFD
     C     ELSCEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELSCEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCS
     C                   Z-ADD(H)  XXDISA        XXDISS
     C                   Z-ADD     PRM           STN
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C*
     C                   ENDIF                                                  ELASPS IFNE
     C*
     C************************************************************
     C*  COMPUTE DEVIATION  - IF TAKEN FROM SUBJECT PREMIUM
     C*
     C     RRDV1L        IFEQ      'J'
     C     ELASP2        ANDNE     *ZERO
     C                   Z-ADD     ELASP2        XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELDVEF        XXEFD
     C     ELDVEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELDVEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDC2
     C                   Z-ADD     PRM           STN
     C                   Z-ADD(H)  XXDISA        XXDIS2
     C*** begin add ***  03/29/13  **************************************
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD1S2        IFNE      0
     C                   Z-ADD     ELD1S2        XXPCT
     C                   Z-ADD     ELD1D2        XXEFD
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD1X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     STN           PRM
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     DIS           D1E2
     C                   Z-ADD     PRM           STN
     C                   Z-ADD(H)  XXDISA        XXD1D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C************************************************************
     C*  COMPUTE 2ND DEVIATION  - IF TAKEN FROM SUBJECT PREMIUM
     C*
     C*
     C*
     C*****RRDV2L        IFEQ      'J'
     C*****ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*****XXCLT#        ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C*** begin chg ****** 02/25/10 ************************************
     C*****RRDV2L        OREQ      'M'
     C*****RRDV2L        OREQ      'J'
     C*** end chg ******** 02/25/10 ************************************
     C*****XXCLT#        ANDEQ     '025'
     C*****ELSTFC        ANDNE     *ZERO
     C     RRDV2L        IFEQ      'J'
     C     ELDEV2        IFNE      *ZERO
     C     ELSTFC        ORNE      *ZERO
     C     ELDEV2F       ORNE      *ZERO
     C*** End   Chg *** 04/07/17 ***************************************
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****XXCLT#        IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C**********         Z-ADD     ELDEV2        XXPCT
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C**********         ELSE
     C*****ELSTFC        MULT(H)   -100          XXPCT
     C**********         ADD       100           XXPCT
     C**********         ENDIF                                                  XXCLT# <> '025'
     C                   Select
     C                   When      ELstFc  <> 0
     C                   Eval      xxPct = %DecH(ELstFc*-100:12:9) + 100
     C                   When      ELdev2F <> 0
     C                   Eval      xxPct = %DecH(ELdev2F*-100:12:9) + 100
     C                   When      ELdev2  <> 0
     C                   Eval      xxPct = ELdev2
     C                   EndSl
     C*** End   Chg *** 04/07/17 ***************************************
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     STN           PRM
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate Before Premium
     C                   Eval      xxDev2BP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     ELD2EF        XXEFD
     C     ELD2EX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2EX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDV2
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate After Premium
     C                   Eval      xxDev2AP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     PRM           STN
     C                   Z-ADD(H)  XXDISA        XXDEV2
     C*** begin add ***  03/29/13  **************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD2S2        IFNE      0
     C                   Z-ADD     ELD2S2        XXPCT
     C                   Z-ADD     ELD2D2        XXEFD
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     STN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           D2E2
     C                   Z-ADD     PRM           STN
     C                   Z-ADD(H)  XXDISA        XXD2D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*
     C*** Begin Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  ELDEV2/STFC/DEV2F
     C*** End   Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C*
     C* WAVIER OF SUBROGRATION - APPLIED TO SUBJECT
     C*
     C     ELWVFL        IFEQ      'J'
     C     ELWVTP        ANDNE     *BLANK
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     STNT          PRMT
     C                   Z-ADD     STN1          PRM1
     C                   Z-ADD     STN2          PRM2
     C                   Z-ADD     STN3          PRM3
     C                   Z-ADD     STN4          PRM4
     C                   Z-ADD     STN5          PRM5
     C                   Z-ADD     STN6          PRM6
     C                   Z-ADD     STN7          PRM7
     C                   Z-ADD     0             PRML
     C*
     C                   EXSR      WAIV
     C*
     C                   Z-ADD     0             WAVL
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       WAV(X)        STN(X)
     C                   ADD       WAVT(X)       STNT(X)
     C                   ADD       WAV1(X)       STN1(X)
     C                   ADD       WAV2(X)       STN2(X)
     C                   ADD       WAV3(X)       STN3(X)
     C                   ADD       WAV4(X)       STN4(X)
     C                   ADD       WAV5(X)       STN5(X)
     C                   ADD       WAV6(X)       STN6(X)
     C                   ADD       WAV7(X)       STN7(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADD ADDITIONAL LOAD TO SUBJECT PREMIUM
     C*
     C     RRASFL        IFEQ      'J'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       ADL(X)        STN(X)
     C                   ADD       ADL(X)        STNT(X)
     C     RRST          IFEQ      'MA'
      *
     C     RR1IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN1(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ENDIF
      *
     C     RR2IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN2(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN2(X)
     C                   ENDIF
      *
     C     RR3IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN3(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN3(X)
     C                   ENDIF
      *
     C     RR4IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN4(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN4(X)
     C                   ENDIF
      *
     C     RR5IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN5(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN5(X)
     C                   ENDIF
      *
     C     RR6IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN6(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN6(X)
     C                   ENDIF
      *
     C     RR7IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN7(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF
      *
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ADD       ADL(X)        STN2(X)
     C                   ADD       ADL(X)        STN3(X)
     C                   ADD       ADL(X)        STN4(X)
     C                   ADD       ADL(X)        STN5(X)
     C                   ADD       ADL(X)        STN6(X)
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF                                                  RRST <> 'MA'
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  RRASFL IFEQ
     C*
     C* ADD SHORT RATE PENALTY PREMIUM TO SUBJECT PREMIUM.
     C*
     C     RRSRAF        IFEQ      'J'
     C     XXSRFL        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C     STN(X)        MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C     XXSRPR        SUB       STN(X)        SRP(X)
     C                   Z-ADD     XXSRPR        STN(X)
     C*
     C     STNT(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STNT(X)
     C*
     C     STN1(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN1(X)
     C*
     C     STN2(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN2(X)
     C*
     C     STN3(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN3(X)
     C*
     C     STN4(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN4(X)
     C*
     C     STN5(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN5(X)
     C*
     C     STN6(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN6(X)
     C*
     C     STN7(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN7(X)
     C*
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*** BEGIN ADD ***  01/12/10  **************************************
     C                   Z-ADD     STN           S2N
     C                   Z-ADD     STNT          S2NT
     C                   Z-ADD     STN1          S2N1
     C                   Z-ADD     STN2          S2N2
     C                   Z-ADD     STN3          S2N3
     C                   Z-ADD     STN4          S2N4
     C                   Z-ADD     STN5          S2N5
     C                   Z-ADD     STN6          S2N6
     C                   Z-ADD     STN7          S2N7
     C*
     C* APPLY 1ST LARGE DEDUCTIBLE TO SUBJECT PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV1        ANDEQ     'J'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT1        XXPCT
     C                   Z-ADD     LDEFD1        XXEFD
     C     LDEXP1        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP1        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C* APPLY 2ND LARGE DEDUCTIBLE TO SUBJECT PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV2        ANDEQ     'J'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT2        XXPCT
     C                   Z-ADD     LDEFD2        XXEFD
     C     LDEXP2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP2        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*** END ADD *****  01/12/10  **************************************
     C************************************************************
     C                   XFOOT(H)  STN           XXWK10
     C                   Z-ADD     XXWK10        XXPRXP           11 2
     C                   Z-ADD     STN           EPRM
     C*
     C*  EXPERIENCE MODIFIER 1
     C*
     C                   Z-ADD     0             XXEXP1
     C     ELEXM1        IFNE      *ZERO
     C*
     C                   Z-ADD     STN           PRM
     C     ELEXM1        MULT      100           XXPCT
     C     ELEMD1        IFLT      ELPFDT
     C                   Z-ADD     ELPFDT        XXEFD
     C                   ELSE
     C                   Z-ADD     ELEMD1        XXEFD
     C                   ENDIF
     C*
     C     ELEMD2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELEMD2        XXEXD
     C                   ENDIF
     C*
     C     BILL          IFEQ      'A'
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEXD
     C     SVCNDT        ANDGT     XXEFD
     C                   Z-ADD     SVCNDT        XXEXD
     C                   ENDIF
     C                   ENDIF                                                  BILL IFEQ
     C*
     C                   EXSR      DISCA
     C*                                                    & XXEXEF < PDAT,Z
     C                   Z-ADD(H)  XXDISA        XXEXP1
     C                   Z-ADD     DIS           EXP
     C                   Z-ADD     DIS           EXA1
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXPT
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXP1
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXP2
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXP3
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXP4
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXP5
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXP6
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXP7
     C                   Z-ADD     STOC          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EXPS
     C*
     C                   ENDIF                                                  ELEXM1 IFNE
     C*
     C************************************************************
     C*  EXPERIENCE MODIFIER 2
     C*
     C     ELEMD2        IFNE      *ZERO
     C*
     C                   Z-ADD     STN           PRM
     C     ELEXM2        MULT      100           XXPCT
     C                   Z-ADD     ELEMD2        XXEFD
     C     ELEMD3        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELEMD3        XXEXD
     C                   ENDIF
     C*
     C     BILL          IFEQ      'A'
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEXD
     C     SVCNDT        ANDGT     XXEFD
     C                   Z-ADD     SVCNDT        XXEXD
     C                   ENDIF
     C*
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEFD
     C                   Z-ADD     XXEFD         XXEXD
     C                   ENDIF
     C                   ENDIF                                                  BILL IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD(H)  XXDISA        XXEXP2
     C                   Z-ADD     DIS           EXA2
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXPT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STOC          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXPS(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  ELEXM2 IFNE
     C*
     C************************************************************
     C*  EXPERIENCE MODIFIER 3
     C*
     C     ELEMD3        IFNE      *ZERO
     C*
     C                   Z-ADD     ELEMD3        XXEFD
     C                   Z-ADD     ELPTDT        XXEXD
     C     ELEXM3        MULT      100           XXPCT
     C                   Z-ADD     STN           PRM
     C*
     C     BILL          IFEQ      'A'
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEXD
     C     SVCNDT        ANDGT     XXEFD
     C                   Z-ADD     SVCNDT        XXEXD
     C                   ENDIF
     C*
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEFD
     C                   Z-ADD     XXEFD         XXEXD
     C                   ENDIF
     C                   ENDIF                                                  BILL IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD(H)  XXDISA        XXEXP3
     C                   Z-ADD     DIS           EXA3
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXPT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXP7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STOC          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EXPS(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  ELEXM3 IFNE
     C*
     C*  ACCUMULATE MODIFIED PREMIUM
     C*
     C     ELEXM1        IFNE      *ZERO
     C                   Z-ADD     EXP           STN
     C                   Z-ADD     EXPT          STNT
     C                   Z-ADD     EXP1          STN1
     C                   Z-ADD     EXP2          STN2
     C                   Z-ADD     EXP3          STN3
     C                   Z-ADD     EXP4          STN4
     C                   Z-ADD     EXP5          STN5
     C                   Z-ADD     EXP6          STN6
     C                   Z-ADD     EXP7          STN7
     C                   Z-ADD     EXPS          STOC
     C*
     C     XXEXP1        ADD       XXEXP2        XXMODP           15 4
     C                   ADD       XXEXP3        XXMODP
     C                   ELSE
     C                   XFOOT     STN           XXMODP
     C                   Z-ADD(H)  XXMODP        XXWK10
     C                   Z-ADD     XXWK10        XXMODP
     C                   ENDIF                                                  EXEXM1 IFNE
     C                   Z-ADD     STN           MOD
     C*
      *** Beg Del ***** 04/18/24 ***************************************************
      * This line to clear xxDisd is moved above Schedule DB/CR is calculated at Manual
     C***                Z-ADD     0             XXDISD           11 0
      *** End Del ***** 04/18/24 ***************************************************
     C                   XFOOT(H)  STN           XXEXPP           11 0
     C                   XFOOT(H)  STN1          X1EXPP           11 0
     C                   XFOOT(H)  STN2          X2EXPP           11 0
     C                   XFOOT(H)  STN3          X3EXPP           11 0
     C                   XFOOT(H)  STN4          X4EXPP           11 0
     C                   XFOOT(H)  STN5          X5EXPP           11 0
     C                   XFOOT(H)  STN6          X6EXPP           11 0
     C                   XFOOT(H)  STN7          X7EXPP           11 0
     C                   Z-ADD     STN6          A6E
     C                   Z-ADD     STN7          A7E
     C                   XFOOT(H)  STOC          XXMMPR           11 0
     C                   Z-ADD     STN1          A1E
     C                   Z-ADD     STN2          A2E
     C                   Z-ADD     STN3          A3E
     C                   Z-ADD     STN4          A4E
     C                   Z-ADD     STN5          A5E
     C*** begin add ***  04/22/19  *************************************
     C     BILL          IFEQ      'E'
     C*
     C     DSKY1         CHAIN     PIL01                              60
     C*
     C                   Z-ADD     XXCO#         ILCO#
     C                   Z-ADD     XXFND         ILFND
     C                   Z-ADD     XXEMP#        ILEMP#
     C                   Z-ADD     XXDIV         ILDIV
     C                   Z-ADD     XXFYR         ILFYR
     C                   Move      xxSt          ILSt
     C*
     C                   Z-ADD     xxModp        ILEMPM
     C                   MOVE      'E'           ILSTAT
     C*
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ILDT
     C                   Z-ADD     DSTIME        ILTIME
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PIL01
     C                   ELSE
     C                   WRITE     PIL01
     C                   ENDIF
     C*
     C                   ENDIF                                                  BILL IFEQ
     C*
     C* EXIT PROGRAM NOW IF CALLED FROM SRMIL.
     C*
     C     *IN96         IFEQ      *ON
     C                   GOTO      END
     C                   ENDIF                                                  *IN96 IFEQ
     C*** end add *****  04/22/19  *************************************
     C************************************************************
     C*  CONTRACTOR ADJUSTMENT CREDIT
     C*
     C                   Z-ADD     0             XXDIS1           11 0
     C     ELASP1        IFNE      *ZERO
     C                   Z-ADD     ELASP1        XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELCREF        XXEFD
     C     ELCREX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELCREX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDC1
     C                   Z-ADD(H)  XXDISA        XXDIS1
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
      *
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
      *
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
      *
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
      *
     C                   ENDIF                                                  ELASP1 IFNE
     C*
     C************************************************************
     C*  SCHEDULE DEBIT/CREDIT
     C*   This code is duplicated below
     C*
      *** Beg Chg ***** 04/18/24 *************************************
     C***  RRDCAP        IFNE      'Y'
     C     RRDCAP        IFEQ      *Blank
      *** End Chg ***** 04/18/24 *************************************
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '136'
     C                   Z-ADD     ELPFDT        XXEFD
     C                   Z-ADD     ELPTDT        XXEXD
      *
     C                   Z-ADD     DBRRN         ##RRN             9 0
     C                   CALL      'WA060R'
     C                   PARM                    ##RRN
     C     ELCMUL        PARM                    ##FCTR            5 4
      *
     C                   UPDATE    PEL01
     C     ELKEY         CHAIN     PEL01                              60
      *
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        XXDISD
     C                   Z-ADD     DIS           EDC
     C                   ELSE
     C                   Z-ADD     STN           PRM
     C                   ENDIF
      *
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        X1DISD           11 0
     C     XXDISD        ADD       X1DISD        XXDISD
     C                   ADD       DIS           EDC
     C                   ENDIF
      *
     C                   Z-ADD     PRM           STN
     C*
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C     RRRDTX        OREQ      'Y'
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STNT          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STNT
     C                   ENDIF                                                  RRST IFNE
     C*
     C*** begin add ***  08/07/20  ***************************************
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C*** end add *****  08/07/20  ***************************************
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN1          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN1
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN2          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN2
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN3          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN3
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN4          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN4
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN5          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN5
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN6          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN6
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN7          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN7
     C*** begin add ***  08/07/20  ***************************************
     C                   Endif                                                  If RRST <> KY
     C*** end add *****  08/07/20  ***************************************
     C*
     C                   OTHER
      *
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELSDEF        XXEFD
     C     ELSDEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELSDEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        XXDISD
     C                   Z-ADD     DIS           EDC
     C                   Z-ADD     PRM           STN
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C     RRRDTX        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF                                                  RRST IFNE
     C*
     C     RRST          IFNE      'MA'
     C*** begin add ***  08/07/20  ***************************************
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C*** end add *****  08/07/20  ***************************************
     C*
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C*
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C*
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C*** begin add ***  08/07/20  ***************************************
     C                   ENDIF                                                  RRST <> 'KY'
     C*** end add *****  08/07/20  ***************************************
     C                   ENDIF                                                  RRST <> 'MA'
     C                   ENDIF                                                  ELDBCR IFNE
     C                   ENDSL
     C*** BEGIN ADD ***  01/12/10  ****************************************
     C                   ENDIF                                                  RRDCAP IFNE
     C*** END ADD *****  01/12/10  ****************************************
     C*
     C                   Z-ADD     STN           SVN
     C                   Z-ADD     STNT          SVNT
     C                   Z-ADD     STN1          SVN1
     C                   Z-ADD     STN2          SVN2
     C                   Z-ADD     STN3          SVN3
     C                   Z-ADD     STN4          SVN4
     C                   Z-ADD     STN5          SVN5
     C                   Z-ADD     STN6          SVN6
     C                   Z-ADD     STN7          SVN7
     C*
     C*** BEGIN DEL ***  01/12/10  ****************************************
     C************       ENDIF
     C*** END DEL *****  01/12/10  ****************************************
     C*
     C************************************************************
     C*  COMPUTE DEVIATION  - IF TAKEN FROM EXPERIENCE PREMIUM
     C*
     C*
     C     RRDV1L        IFEQ      'E'
     C     ELASP2        ANDNE     *ZERO
     C                   Z-ADD     ELASP2        XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELDVEF        XXEFD
     C     ELDVEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELDVEX        XXEXD
     C                   ENDIF
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO EXPERIENCE PREM.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDC2
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD(H)  XXDISA        XXDIS2
     C*** begin add ***  03/29/13  **************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     XXCLT#        IFEQ      '045'
     C                   ENDIF                                                  XXCKT# = 045
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD1S2        IFNE      0
     C                   Z-ADD     ELD1S2        XXPCT
     C                   Z-ADD     ELD1D2        XXEFD
     C                   Z-ADD     STN           PRM
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD1X2        XXEXD
     C                   ENDIF
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO EXPERIENCE PREM.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     DIS           D1E2
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD(H)  XXDISA        XXD1D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     XXCLT#        IFEQ      '045'
     C                   ENDIF                                                  XXCKT# = 045
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C************************************************************
     C*
     C*  COMPUTE 2ND DEVIATION  - IF TAKEN FROM EXPERIENCE PREMIUM
     C*
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****RRDV2L        IFEQ      'E'
     C*****ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*****XXCLT#        ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C*** begin chg ****** 02/25/10 ************************************
     C*****RRDV2L        OREQ      'M'
     C*****RRDV2L        OREQ      'E'
     C*** end chg ******** 02/25/10 ************************************
     C*****XXCLT#        ANDEQ     '025'
     C*****ELSTFC        ANDNE     *ZERO
     C     RRDV2L        IFEQ      'E'
     C     ELDEV2        IFNE      *ZERO
     C     ELSTFC        ORNE      *ZERO
     C     ELDEV2F       ORNE      *ZERO
     C*** End   Chg *** 04/07/17 ***************************************
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****XXCLT#        IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C**********         Z-ADD     ELDEV2        XXPCT
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C**********         ELSE
     C*****ELSTFC        MULT(H)   -100          XXPCT
     C**********         ADD       100           XXPCT
     C**********         ENDIF                                                  XXCLT# <> '025'
     C                   Select
     C                   When      ELstFc  <> 0
     C                   Eval      xxPct = %DecH(ELstFc * -100:12:9) + 100
     C                   When      ELdev2F <> 0
     C                   Eval      xxPct = %DecH(ELdev2F * -100:12:9) + 100
     C                   When      ELdev2  <> 0
     C                   Eval      xxPct = ELdev2
     C                   EndSl
     C*** End   Chg *** 04/07/17 ***************************************
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     STN           PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN           PRM
     C                   ENDIF
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate Before Premium
     C                   Eval      xxDev2BP = %Xfoot(STN)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     ELD2EF        XXEFD
     C     ELD2EX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2EX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDV2
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate After Premium
     C                   Eval      xxDev2AP = %Xfoot(STN)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD(H)  XXDISA        XXDEV2
     C*** begin add ***  03/29/13  **************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVNT          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN1          PRM
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN2          PRM
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN3          PRM
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN4          PRM
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN5          PRM
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN6          PRM
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN7          PRM
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD2S2        IFNE      0
     C                   Z-ADD     ELD2S2        XXPCT
     C                   Z-ADD     ELD2D2        XXEFD
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     STN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           D2E2
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD(H)  XXDISA        XXD2D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVNT          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN1          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN2          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN3          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN4          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN5          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN6          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C     XXCLT#        IFEQ      '071'
     C                   Z-ADD     SVN7          PRM
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*
     C*** Begin Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  ELDEV2/STFC/DEV2F
     C*** End   Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C*   This code is duplicated above
     C*
     C     RRDCAP        IFEQ      'Y'
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '136'
     C                   Z-ADD     ELPFDT        XXEFD
     C                   Z-ADD     ELPTDT        XXEXD
      *
     C                   Z-ADD     DBRRN         ##RRN             9 0
     C                   CALL      'WA060R'
     C                   PARM                    ##RRN
     C     ELCMUL        PARM                    ##FCTR            5 4
      *
     C                   UPDATE    PEL01
     C     ELKEY         CHAIN     PEL01                              60
      *
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        XXDISD
     C                   Z-ADD     DIS           EDC
     C                   ELSE
     C                   Z-ADD     STN           PRM
     C                   ENDIF
      *
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        X1DISD           11 0
     C     XXDISD        ADD       X1DISD        XXDISD
     C                   ADD       DIS           EDC
     C                   ENDIF
      *
     C                   Z-ADD     PRM           STN
     C*
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C     RRRDTX        OREQ      'Y'
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STNT          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STNT
     C                   ENDIF                                                  RRST IFNE
     C*
     C*** begin add ***  08/07/20  ***************************************
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C*** end add *****  08/07/20  ***************************************
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN1          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN1
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN2          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN2
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN3          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN3
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN4          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN4
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN5          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN5
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN6          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN6
     C*
     C     ELCMUL        IFNE      *ZERO
     C     ELCMUL        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   ELSE
     C                   Z-ADD     STN7          PRM
     C                   ENDIF
     C*
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   EXSR      DISCA
     C                   ENDIF
     C*
     C                   Z-ADD     PRM           STN7
     C*** begin add ***  08/07/20  ***************************************
     C                   Endif                                                  RRST <> 'KY'
     C*** end add *****  08/07/20  ***************************************
     C*
     C                   OTHER
      *
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELSDEF        XXEFD
     C     ELSDEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELSDEX        XXEXD
     C                   ENDIF
     C*
     C*** begin chg ***  12/01/23  *******************************
     C******             EXSR      DISCA
     C******             Z-ADD(H)  XXDISA        XXDISD
     C******             Z-ADD     DIS           EDC
     C******             Z-ADD     PRM           STN
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN           PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD(H)  XXDISA        XXDISD
     C                   Z-ADD     DIS           EDC
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*** end chg *****  12/01/23  *******************************
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C     RRRDTX        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C*** begin chg ***  12/01/23  *******************************
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STNT
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVNT          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*** end chg *****  12/01/23  *******************************
     C                   ENDIF                                                  RRST IFNE
     C*
     C     RRST          IFNE      'MA'
     C*** begin add ***  08/07/20  ***************************************
     C     RRST          IFNE      'KY'
     C     XXDDCT        OREQ      *ZERO
     C*** end add *****  08/07/20  ***************************************
     C*** begin chg ***  12/01/23  *******************************
     C******             Z-ADD     STN1          PRM
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STN1
     C*
     C******             Z-ADD     STN2          PRM
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STN2
     C*
     C******             Z-ADD     STN3          PRM
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STN3
     C******             Z-ADD     STN4          PRM
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STN4
     C******             Z-ADD     STN5          PRM
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STN5
     C******             Z-ADD     STN6          PRM
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STN6
     C******             Z-ADD     STN7          PRM
     C******             EXSR      DISCA
     C******             Z-ADD     PRM           STN7
     C*
     C                   Z-ADD     STN1          PRM
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN1          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN2          PRM
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN2          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN3          PRM
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN3          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN4          PRM
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN4          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN5          PRM
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN5          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN6          PRM
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN6          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN7          PRM
     C                   If        RRST = 'AZ'
     C                   Z-ADD     SVN7          PRM
     C                   Endif
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C*** end chg *****  12/01/23  *******************************
     C*** begin add ***  08/07/20  ***************************************
     C                   ENDIF                                                  RRST <> 'KY'
     C*** end add *****  08/07/20  ***************************************
     C                   ENDIF                                                  RRST <> 'MA'
     C                   ENDIF                                                  ELDBCR IFNE
     C                   ENDSL
     C*** BEGIN ADD ***  01/12/10  ****************************************
     C                   ENDIF                                                  RRDCAP IFEQ
     C*** END ADD *****  01/12/10  ****************************************
     C*
     C*** BEGIN DEL ***  01/12/10  ****************************************
     C****               Z-ADD     STN           SVN
     C****               Z-ADD     STNT          SVNT
     C****               Z-ADD     STN1          SVN1
     C****               Z-ADD     STN2          SVN2
     C****               Z-ADD     STN3          SVN3
     C****               Z-ADD     STN4          SVN4
     C****               Z-ADD     STN5          SVN5
     C****               Z-ADD     STN6          SVN6
     C****               Z-ADD     STN7          SVN7
     C*
     C****               ENDIF
     C*** END DEL *****  01/12/10  ****************************************
     C*
     C* WAVIER OF SUBROGRATION - APPLIED TO EXPERIENCE
     C*
     C     ELWVFL        IFEQ      'E'
     C     ELWVTP        ANDNE     *BLANK
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     STNT          PRMT
     C                   Z-ADD     STN1          PRM1
     C                   Z-ADD     STN2          PRM2
     C                   Z-ADD     STN3          PRM3
     C                   Z-ADD     STN4          PRM4
     C                   Z-ADD     STN5          PRM5
     C                   Z-ADD     STN6          PRM6
     C                   Z-ADD     STN7          PRM7
     C                   Z-ADD     0             PRML
     C*
     C                   EXSR      WAIV
     C*
     C                   Z-ADD     0             WAVL
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       WAV(X)        STN(X)
     C                   ADD       WAVT(X)       STNT(X)
     C                   ADD       WAV1(X)       STN1(X)
     C                   ADD       WAV2(X)       STN2(X)
     C                   ADD       WAV3(X)       STN3(X)
     C                   ADD       WAV4(X)       STN4(X)
     C                   ADD       WAV5(X)       STN5(X)
     C                   ADD       WAV6(X)       STN6(X)
     C                   ADD       WAV7(X)       STN7(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* SAVE THE CURRENT STANDARD ARRAY FOR THE NEXT FEW CREDITS.
     C*
     C                   Z-ADD     STN           SVN
     C                   Z-ADD     STNT          SVNT
     C                   Z-ADD     STN1          SVN1
     C                   Z-ADD     STN2          SVN2
     C                   Z-ADD     STN3          SVN3
     C                   Z-ADD     STN4          SVN4
     C                   Z-ADD     STN5          SVN5
     C                   Z-ADD     STN6          SVN6
     C                   Z-ADD     STN7          SVN7
     C*
     C* ADD ADDITIONAL LOAD TO EXPERIENCE PREMIUM (FORMERLY STANDARD)
     C*
     C     RRASFL        IFEQ      'E'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       ADL(X)        STN(X)
     C                   ADD       ADL(X)        STNT(X)
     C     RRST          IFEQ      'MA'
      *
     C     RR1IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN1(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ENDIF
      *
     C     RR2IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN2(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN2(X)
     C                   ENDIF
      *
     C     RR3IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN3(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN3(X)
     C                   ENDIF
      *
     C     RR4IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN4(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN4(X)
     C                   ENDIF
      *
     C     RR5IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN5(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN5(X)
     C                   ENDIF
      *
     C     RR6IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN6(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN6(X)
     C                   ENDIF
      *
     C     RR7IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN7(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF
      *
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ADD       ADL(X)        STN2(X)
     C                   ADD       ADL(X)        STN3(X)
     C                   ADD       ADL(X)        STN4(X)
     C                   ADD       ADL(X)        STN5(X)
     C                   ADD       ADL(X)        STN6(X)
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRASFL IFEQ
     C*** BEGIN ADD ***  01/12/10  **************************************
     C                   Z-ADD     STN           S2N
     C                   Z-ADD     STNT          S2NT
     C                   Z-ADD     STN1          S2N1
     C                   Z-ADD     STN2          S2N2
     C                   Z-ADD     STN3          S2N3
     C                   Z-ADD     STN4          S2N4
     C                   Z-ADD     STN5          S2N5
     C                   Z-ADD     STN6          S2N6
     C                   Z-ADD     STN7          S2N7
     C*** begin add ***  06/14/23  **************************************
     C                   If        xxst = 'MI'
     C                   Xfoot     STN           xxTargBase
     C                   Endif
     C*** end add *****  06/14/23  **************************************
     C*
     C* APPLY 1ST LARGE DEDUCTIBLE TO MODIFIED PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV1        ANDEQ     'E'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT1        XXPCT
     C                   Z-ADD     LDEFD1        XXEFD
     C     LDEXP1        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP1        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C* APPLY 2ND LARGE DEDUCTIBLE TO MODIFIED PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV2        ANDEQ     'E'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT2        XXPCT
     C                   Z-ADD     LDEFD2        XXEFD
     C     LDEXP2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP2        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*** END ADD *****  01/12/10  **************************************
     C*
     C************************************************************
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUM
     C*
     C*
     C     RRRDPR        IFEQ      'S'
     C     WCFL06        ANDEQ     'Y'
     C     XXDDCT        ANDNE     *ZERO
     C*** begin add ***  09/24/24  **************************************
     C     ELDCDE        ANDNE     *blanks
     C*** end add *****  09/24/24  **************************************
     C                   Z-ADD     XXDDCT        XXPCT
     C                   Z-ADD     SVN           PRM
     C                   Z-ADD     ELDCEF        XXEFD
     C     ELDCEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELDCEX        XXEXD
     C                   ENDIF                                                  ELDCEX IFEQ
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C     RR1IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR2IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR3IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     RR4IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR5IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     RR6IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR7IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     RRRDTX        IFEQ      'Y'
     C                   Z-ADD     SVNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRRDTX IFEQ
     C     XXDD2P        IFNE      *ZERO
     C*
     C                   Z-ADD     SVN           PRM
     C*** BEGIN CHG ***  01/12/10  **************************************
     C************       Z-ADD     XXDD2P        XXPCT             9 6
     C                   Z-ADD     XXDD2P        XXPCT
     C*** END CHG *****  01/12/10  **************************************
     C                   Z-ADD     EL2DEF        XXEFD             7 0
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD             7 0
     C                   ELSE
     C                   Z-ADD     EL2DEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     1             X
     C*
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RR1IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR2IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR3IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR4IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR5IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR6IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR7IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     SVN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*** BEGIN ADD ***  01/12/10  ************************************
     C     RRRDTX        IFEQ      'Y'
     C                   Z-ADD     SVNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRRDTX IFEQ
     C*** END ADD *****  01/12/10  ************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRRDPR = S
     C*
     C* SAVE STANDARD PREMIUM W/O LOSS CONSTANT FOR REVISED BILLING.
     C*
     C                   Z-ADD     STN           STNL
     C                   Z-ADD     STN           SVNL
     C                   Z-ADD     0             WAVL
     C*
     C*  CHECK FOR LOSS CONSTANT AND APPLY IF NEEDED.
     C*
     C                   Z-ADD     0             XXLOSA           11 2
     C*
     C     XXLOSS        IFNE      *ZERO
     C     XXDIV         ANDEQ     XXDIV1
     C     *IN94         ANDEQ     *OFF
     C*
     C                   Z-ADD     0             XXPREM
     C*
     C* GATHER ALL THE PREMIUM IN THE STATE.
     C*
     C     DSKY          CHAIN     PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     MPST          IFEQ      XXST
     C     BILL          IFEQ      'A'
     C                   ADD       MPAAPL        XXPREM
     C                   ELSE
     C                   ADD       MPANPL        XXPREM
     C                   ENDIF                                                  BILL IFEQ
     C                   ENDIF                                                  MPST IFEQ
     C*
     C     DSKY          READE     PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* CHECK THE PREMIUM BASIS FOR THE STATE.
     C*
     C     RRKY          SETGT     PSC01
     C     RRKY1         READPE    PSC01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     SCPRBS        ANDGT     *ZERO
     C     XXPREM        ANDLT     SCPRBS
     C*
     C* LOSS CONSTANT CANNOT EXCEED THE PREMIUM BASIS
     C*
     C     SCPRBS        SUB       XXPREM        XXPREM
     C     XXPREM        IFGT      XXLOSS
     C                   Z-ADD     XXLOSS        XXPREM
     C                   ENDIF
     C*
     C* ADD THE LOSS CONSTANT AMOUNT TO STANDARD PREMIUM
     C*
     C                   Z-ADD     XXPREM        XXLOSA
     C                   ADD       XXPREM        STN(1)
     C                   ADD       XXPREM        STNT(1)
     C                   ADD       XXPREM        STN1(1)
     C                   ADD       XXPREM        STN2(1)
     C                   ADD       XXPREM        STN3(1)
     C                   ADD       XXPREM        STN4(1)
     C                   ADD       XXPREM        STN5(1)
     C                   ADD       XXPREM        STN6(1)
     C                   ADD       XXPREM        STN7(1)
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  XXLOSS IFNE
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       ADL2(X)       STN(X)
     C                   ADD       ADL2(X)       STNT(X)
     C                   ADD       ADL2(X)       STN1(X)
     C                   ADD       ADL2(X)       STN2(X)
     C                   ADD       ADL2(X)       STN3(X)
     C                   ADD       ADL2(X)       STN4(X)
     C                   ADD       ADL2(X)       STN5(X)
     C                   ADD       ADL2(X)       STN6(X)
     C                   ADD       ADL2(X)       STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*** Beg Add  * 10/20/15
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       ADL3(X)       STN(X)
     C                   ADD       ADL3(X)       STNT(X)
     C                   ADD       ADL3(X)       STN1(X)
     C                   ADD       ADL3(X)       STN2(X)
     C                   ADD       ADL3(X)       STN3(X)
     C                   ADD       ADL3(X)       STN4(X)
     C                   ADD       ADL3(X)       STN5(X)
     C                   ADD       ADL3(X)       STN6(X)
     C                   ADD       ADL3(X)       STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C*** End Add  * 10/20/15
     C* ADD SHORT RATE PENALTY PREMIUM TO SUBJECT PREMIUM.
     C*
     C     RRSRAF        IFEQ      'S'
     C     XXSRFL        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C     STN(X)        MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C     XXSRPR        SUB       STN(X)        SRP(X)
     C                   Z-ADD     XXSRPR        STN(X)
     C*
     C     STNT(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STNT(X)
     C*
     C     STN1(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN1(X)
     C*
     C     STN2(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN2(X)
     C*
     C     STN3(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN3(X)
     C*
     C     STN4(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN4(X)
     C*
     C     STN5(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN5(X)
     C*
     C     STN6(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN6(X)
     C*
     C     STN7(X)       MULT(H)   FACT          XXSRPR           11 0
     C     XXSRPR        MULT(H)   STPERC        XXWK92
     C     XXWK92        MULT(H)   .01           XXSRPR
     C                   Z-ADD     XXSRPR        STN7(X)
     C*
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*
     C*  ACCUMULATE STANDARD PREMIUM
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*** begin del ****** 02/25/10 ************************************
     C*****XXCLT#        IFEQ      '025'
     C*****FNPFFL        ANDEQ     'P'
     C*****BILL          ANDEQ     'A'
     C*****              XFOOT     STN           XXSTN1           15 4
     C*****              Z-ADD     1             X
     C*****RDAT(X)       DOWNE     *ZERO
     C*****XXNNET        MULT(H)   PRO(X)        STN(X)
     C*****              ADD       1             X
     C*****              ENDDO                                                  RDAT,X DOWNE
     C*
     C*****              XFOOT     STN           XXSTAN
     C*****XXSTAN        IFNE      XXNNET
     C*****XXNNET        SUB       XXSTAN        XXSTAN
     C*****              ADD       XXSTAN        STN(1)
     C*****              ENDIF                                                  XXSTAN IFNE
     C*
     C*****              Z-ADD     STN           STN1
     C*****              Z-ADD     STN           STN2
     C*****              Z-ADD     STN           STNT
     C*
     C*****              ENDIF                                                  XXCLT# = '025'
     C*** end del ******** 02/25/10 ************************************
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     0             XXSTAN           15 4
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       STN(X)        XXSTAN
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN           SVN
     C                   XFOOT     STN1          X1STAN           15 4
     C                   XFOOT     STN2          X2STAN           15 4
     C                   Z-ADD     STNT          SVNT
     C                   Z-ADD     STN1          SVN1
     C                   Z-ADD     STN2          SVN2
     C                   XFOOT     STN3          X3STAN           15 4
     C                   Z-ADD     STN3          SVN3
     C                   Z-ADD     STN1          A1S
     C                   Z-ADD     STN2          A2S
     C                   Z-ADD     STN3          A3S
     C                   Z-ADD     STN4          A4S
     C                   Z-ADD     STN5          A5S
     C                   XFOOT     STN4          X4STAN           15 4
     C                   Z-ADD     STN4          SVN4
     C                   XFOOT     STN5          X5STAN           15 4
     C                   Z-ADD     STN5          SVN5
     C                   Z-ADD     STN6          A6S
     C                   Z-ADD     STN7          A7S
     C                   XFOOT     STN6          X6STAN           15 4
     C                   Z-ADD     STN6          SVN6
     C                   XFOOT     STN7          X7STAN           15 4
     C                   Z-ADD     STN7          SVN7
     C                   Z-ADD     STN           STD
     C*
     C* IF REDUCED EXPENSE CONSTANT IS USED, PREPARE DISCOUNT FILE.
     C*
     C                   MOVE      XXCO#         Q1CO#
     C                   MOVE      XXFND         Q1FND
     C                   MOVE      XXFYR         Q1FYR
     C                   MOVE      XXEMP#        Q1EMP#
     C                   MOVE      *BLANKS       QQSPRD            1
     C*
     C*** begin del ***  09/05/25  ******************************************
     C*****              CALL      'SREXCO'
     C*****              PARM                    Q1CO#
     C*****              PARM                    Q1FND
     C*****              PARM                    Q1FYR
     C*****              PARM                    Q1EMP#
     C*****              PARM                    QQSPRD
     C*
     C*****QQSPRD        IFEQ      'Y'
     C*** end del *****  09/05/25  ******************************************
     C*
     C     DSKY1         CHAIN     PMP01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   CLEAR                   PMP01
     C                   Z-ADD     XXCO#         MPCO#
     C                   Z-ADD     XXFND         MPFND
     C                   Z-ADD     XXEMP#        MPEMP#
     C                   Z-ADD     XXDIV         MPDIV
     C                   Z-ADD     XXFYR         MPFYR
     C                   ENDIF
     C*
     C     BILL          IFEQ      'A'
     C                   Z-ADD     XXSTAN        MPASPR
     C                   ELSE
     C                   Z-ADD     XXSTAN        MPSTPR
     C                   ENDIF
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PMP01
     C                   ELSE
     C                   WRITE     PMP01
     C                   ENDIF
     C*
     C*** begin del ***  09/05/25  ******************************************
     C*****              ENDIF                                                  QQSPRD IFEQ
     C*** end del *****  09/05/25  ******************************************
     C*
     C************************************************************
     C*
     C*  COMPUTE DEVIATION  - IF TAKEN FROM STANDARD PREMIUM
     C*
     C     RRDV1L        IFEQ      'S'
     C     ELASP2        ANDNE     *ZERO
     C                   Z-ADD     ELASP2        XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELDVEF        XXEFD
     C     ELDVEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELDVEX        XXEXD
     C                   ENDIF
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   Z-ADD     *ZERO         XXADR
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDC2
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD(H)  XXDISA        XXDIS2
     C*** begin add ***  03/29/13  **************************************
     C*
     C                   Z-ADD     STNL          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD1S2        IFNE      0
     C                   Z-ADD     ELD1S2        XXPCT
     C                   Z-ADD     ELD1D2        XXEFD
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD1X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     STN           PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C*
     C                   Z-ADD     *ZERO         XXADR
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     DIS           D1E2
     C
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD(H)  XXDISA        XXD1D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C                   Z-ADD     STNL          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C*
     C* USE PREMIUM CALCULATED WITHOUT INCR LIMITS FOR CT FOR 1ST DEV
     C* APPLIED TO STANDARD.
     C*
     C     RRST          IFEQ      'CT'
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       LM(X)         PRM(X)
     C                   SUB       LM2(X)        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST IFEQ
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C************************************************************
     C*  COMPUTE 2ND DEVIATION  - IF TAKEN FROM STANDARD PREMIUM
     C*
     C*
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****RRDV2L        IFEQ      'S'
     C*****ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*****XXCLT#        ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C*** begin chg ****** 02/25/10 ************************************
     C*****RRDV2L        OREQ      'M'
     C*****RRDV2L        OREQ      'S'
     C*** end chg ******** 02/25/10 ************************************
     C*****XXCLT#        ANDEQ     '025'
     C*****ELSTFC        ANDNE     *ZERO
     C     RRDV2L        IFEQ      'S'
     C     ELDEV2        IFNE      *ZERO
     C     ELSTFC        ORNE      *ZERO
     C     ELDEV2F       ORNE      *ZERO
     C*** End   Chg *** 04/07/17 ***************************************
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****XXCLT#        IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C**********         Z-ADD     ELDEV2        XXPCT
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C**********         ELSE
     C*****ELSTFC        MULT(H)   -100          XXPCT
     C**********         ADD       100           XXPCT
     C**********         ENDIF                                                  XXCLT# <> '025'
     C                   Select
     C                   When      ELstFc  <> 0
     C                   Eval      xxPct = %DecH(ELstFc*-100:12:9) + 100
     C                   When      ELdev2F <> 0
     C                   Eval      xxPct = %DecH(ELdev2F*-100:12:9) + 100
     C                   When      ELdev2  <> 0
     C                   Eval      xxPct = ELdev2
     C                   EndSl
     C*** End   Chg *** 04/07/17 ***************************************
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     STN           PRM
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate Before Premium
     C                   Eval      xxDev2BP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     ELD2EF        XXEFD
     C     ELD2EX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2EX        XXEXD
     C                   ENDIF
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDV2
     C                   Z-ADD(H)  XXDISA        XXDEV2
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate After Premium
     C                   Eval      xxDev2AP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     PRM           STN
     C                   Z-ADD     STNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNL
     C*** begin add ***  03/29/13  **************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD2S2        IFNE      0
     C                   Z-ADD     ELD2S2        XXPCT
     C                   Z-ADD     ELD2D2        XXEFD
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     STN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           D2E2
     C                   Z-ADD     PRM           STN
     C                   Z-ADD(H)  XXDISA        XXD2D2
     C*
     C                   Z-ADD     STNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNL
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C*
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*
     C*** Begin Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  ELDEV2/STFC/DEV2F
     C*** End   Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C*
     C* WAVIER OF SUBROGRATION - APPLIED TO STANDARD
     C*
     C     ELWVFL        IFEQ      'S'
     C     ELWVTP        ANDNE     *BLANK
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     STNT          PRMT
     C                   Z-ADD     STN1          PRM1
     C                   Z-ADD     STN2          PRM2
     C                   Z-ADD     STN3          PRM3
     C                   Z-ADD     STN4          PRM4
     C                   Z-ADD     STN5          PRM5
     C                   Z-ADD     STN6          PRM6
     C                   Z-ADD     STN7          PRM7
     C                   Z-ADD     STNL          PRML
     C*
     C                   EXSR      WAIV
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       WAV(X)        STN(X)
     C                   ADD       WAVT(X)       STNT(X)
     C                   ADD       WAV1(X)       STN1(X)
     C                   ADD       WAV2(X)       STN2(X)
     C                   ADD       WAV3(X)       STN3(X)
     C                   ADD       WAV4(X)       STN4(X)
     C                   ADD       WAV5(X)       STN5(X)
     C                   ADD       WAVL(X)       STNL(X)
     C                   ADD       WAV6(X)       STN6(X)
     C                   ADD       WAV7(X)       STN7(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADD ADDITIONAL LOAD TO STANDARD PREMIUM
     C*
     C     RRASFL        IFEQ      'S'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       ADL(X)        STN(X)
     C                   ADD       ADL(X)        STNT(X)
     C     RRST          IFEQ      'MA'
      *
     C     RR1IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN1(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ENDIF
      *
     C     RR2IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN2(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN2(X)
     C                   ENDIF
      *
     C     RR3IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN3(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN3(X)
     C                   ENDIF
      *
     C     RR4IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN4(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN4(X)
     C                   ENDIF
      *
     C     RR5IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN5(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN5(X)
     C                   ENDIF
      *
     C     RR6IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN6(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN6(X)
     C                   ENDIF
      *
     C     RR7IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN7(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF
      *
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ADD       ADL(X)        STN2(X)
     C                   ADD       ADL(X)        STN3(X)
     C                   ADD       ADL(X)        STN4(X)
     C                   ADD       ADL(X)        STN5(X)
     C                   ADD       ADL(X)        STN6(X)
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF
     C                   ADD       ADL(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  RRASFL IFEQ
     C*** BEGIN ADD ***  01/12/10  **************************************
     C                   Z-ADD     STN           S2N
     C                   Z-ADD     STNL          S2NL
     C                   Z-ADD     STNT          S2NT
     C                   Z-ADD     STN1          S2N1
     C                   Z-ADD     STN2          S2N2
     C                   Z-ADD     STN3          S2N3
     C                   Z-ADD     STN4          S2N4
     C                   Z-ADD     STN5          S2N5
     C                   Z-ADD     STN6          S2N6
     C                   Z-ADD     STN7          S2N7
     C*
     C* APPLY 1ST LARGE DEDUCTIBLE TO STANDARD PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV1        ANDEQ     'S'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT1        XXPCT
     C                   Z-ADD     LDEFD1        XXEFD
     C     LDEXP1        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP1        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM W/O LOSS CONSTANT
     C                   Z-ADD     S2NL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C* APPLY 2ND LARGE DEDUCTIBLE TO STANDARD PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV2        ANDEQ     'S'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT2        XXPCT
     C                   Z-ADD     LDEFD2        XXEFD
     C     LDEXP2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP2        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM W/O LOSS CONSTANT
     C                   Z-ADD     S2NL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*** END ADD *****  01/12/10  **************************************
     C*
     C************************************************************
     C*  IF NO DISCOUNT CODE GIVEN, USE DISCOUNT PCT ON STANDARD PREM.
     C*
     C                   XFOOT     STN           XXDIS4           15 4
      //*** Beg Add ***** 11/13/24 *****************************************
        X = 1;
        Dow RDAT(X) <> 0;
            If XXDIS4 <> 0;
               Eval(h) PRO2(X) = STN(X)/XXDIS4;
            Else;
               PRO2(X) = 0;
            Endif;
            X += 1;
        Enddo;
        xxDis5 = 1 - %Xfoot(PRO2);
        PRO2(1) = PRO2(1) + xxDis5;
      //*** End Add ***** 11/13/24 *****************************************
     C                   Z-ADD     *ZERO         XXDIS3           11 2
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
      *
     C     XXCLT#        IFEQ      '046'
     C     BILL          ANDEQ     'A'
     C     BSSEFL        ANDEQ     'S'
     C     F1UPAR        ANDEQ     'Y'
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAARCD        IFEQ      F1DSCD
     C                   SUB       EAAMT         XXDIS3
     C                   ENDIF                                                  EAARCD IFEQ
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXDIS3        MULT      PRO(X)        DIS(X)
     C                   SUB       DIS(X)        STN(X)
     C                   SUB       DIS(X)        STNT(X)
     C                   SUB       DIS(X)        STN1(X)
     C                   SUB       DIS(X)        STN2(X)
     C* ADDITIONAL ASSESSMENTS
     C                   SUB       DIS(X)        STN3(X)
     C                   SUB       DIS(X)        STN4(X)
     C                   SUB       DIS(X)        STN5(X)
     C                   SUB       DIS(X)        STN6(X)
     C                   SUB       DIS(X)        STN7(X)
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         XXADR
      *
     C     F1UPAR        IFEQ      'Y'
     C     BILL          ANDEQ     'A'
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '140'
     C                   EXSR      SRADR
     C                   ENDSL
     C*
     C                   ENDIF                                                  F1UPAR IFEQ
     C*
     C     1             SUB       XXADR         XXADR
     C*
     C*** begin add ***  03/28/13  **************************************
     C* Determine Unearned premium
     c*
     C                   if        xxFlgCalcUPSC
     c*
     C* Apply full policy factor
     C                   Z-ADD     ELPFDT        XXEFD
     C                   Z-ADD     ELPTDT        XXEXD
     C*
     C     xxPrmFact     MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C*
     C                   Z-ADD     STN           PRM
     C                   EXSR      DISCA
     C*** begin chg ***  07/24/13  ************************************************
     C*********          Z-ADD     DIS           UPFP
     C                   Z-ADD     PRM           UPFP
     C*** end chg *****  07/24/13  ************************************************
     C*
     C* Determine unearned premium
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C                   eval      UPSN(x) = UPFP(x) - STN(x)
     C*
     C                   if        UPSN(x) < 0
     C                   eval      UPSN(x) = 0
     C                   endif
     C*
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   endif                                                  xxFlgCalcUPSC
     C*
     C*** end add *****  03/28/13  **************************************
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*** begin del ****** 02/25/10 ************************************
     C*****XXCLT#        IFNE      '025'
     C*** end del ******** 02/25/10 ************************************
     C*** END   ADD ****** 01/14/10 ************************************
     C     ELASP3        IFNE      *ZERO
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN           PRM
     C                   ELSE
     C                   Z-ADD     STN           PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   XFOOT     PRM           XXDSPR           15 4
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   MULT(H)   XXADR         XXDISA
     C                   DIV       100           XXDISA
     C                   Z-ADD     XXDISA        EDC3(X)
     C                   ADD(H)    XXDISA        XXDIS3
     C                   SUB       XXDISA        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVNL          PRM
     C                   ELSE
     C                   Z-ADD     STNL          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVNT          PRM
     C                   ELSE
     C                   Z-ADD     STNT          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN1          PRM
     C                   ELSE
     C                   Z-ADD     STN1          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN2          PRM
     C                   ELSE
     C                   Z-ADD     STN2          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN3          PRM
     C                   ELSE
     C                   Z-ADD     STN3          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN4          PRM
     C                   ELSE
     C                   Z-ADD     STN4          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN5          PRM
     C                   ELSE
     C                   Z-ADD     STN5          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN6          PRM
     C                   ELSE
     C                   Z-ADD     STN6          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN7          PRM
     C                   ELSE
     C                   Z-ADD     STN7          PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     ELASP3        MULT      PRM(X)        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Apply premium discount to unearned premium
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     STOC          PRM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     SVN           PRM
     C                   ELSE
     C                   Z-ADD     STN           PRM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     PRM(X)        DIV(H)    xxPrmFact     XXWK10
     C     XXWK10        SUB       PRM(X)        XXDISA
     C                   MULT      ELASP3        XXDISA
     C                   DIV       100           XXDISA
     C                   SUB       XXDISA        UPSN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C*
     C*
     C                   ELSE
     C                   Z-ADD     1             TAX
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVNT          XXPREM
     C                   ELSE
     C                   XFOOT     STNT          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVNL          XXPREM
     C                   ELSE
     C                   XFOOT     STNL          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN1          XXPREM
     C                   ELSE
     C                   XFOOT     STN1          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/12/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/12/24 ******************************************
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN2          XXPREM
     C                   ELSE
     C                   XFOOT     STN2          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN3          XXPREM
     C                   ELSE
     C                   XFOOT     STN3          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN4          XXPREM
     C                   ELSE
     C                   XFOOT     STN4          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN5          XXPREM
     C                   ELSE
     C                   XFOOT     STN5          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN6          XXPREM
     C                   ELSE
     C                   XFOOT     STN6          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN7          XXPREM
     C                   ELSE
     C                   XFOOT     STN7          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Apply stock discount table to unearned premium
     C*
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C     RRDSPR        IFEQ      'M'
     C     XXMMPR        DIV(H)    xxPrmFact     XXWK10
     C     XXWK10        SUB       XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN           XXPREM
     C     XXPREM        DIV(H)    xxPrmFact     XXWK10
     C     XXWK10        SUB       XXPREM        XXPREM
     C                   ELSE
     C                   XFOOT     UPSN          XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C*
     C                   Move      'Y'           xxUnearned        1
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Move      ' '           xxUnearned        1
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   SUB       DIS(X)        UPSN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C*
     C                   Z-ADD     *ZERO         XXDIS3
     C                   Z-ADD     *ZERO         XXDISA
     C                   Z-ADD     *ZERO         XXPREM
     C*
     C                   Z-ADD     2             TAX               1 0
     C*
     C*  IF DISCOUNT CODE GIVEN, USE TABLE.
     C*
     C     RRDSPR        IFEQ      'M'
     C                   Z-ADD     XXMMPR        XXPREM
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   XFOOT     SVN           XXPREM
     C                   ELSE
     C                   XFOOT     STN           XXPREM
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   Z-ADD     XXPREM        XXDSPR
     C*
     C                   EXSR      SRDSCT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXDIS3        MULT(H)   XXADR         XXDIS3
      *** Beg Chg ***** 11/13/24 ******************************************
     C**** XXDIS3        MULT      PRO(X)        DIS(X)
     C     XXDIS3        MULT      PRO2(X)       DIS(X)
      *** End Chg ***** 11/13/24 ******************************************
     C                   Z-ADD     DIS(X)        EDC3(X)
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  ELASP3 IFNE
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*** begin del ****** 02/25/10 ************************************
     C*******            ELSE
     C*
     C* APPLY FUND MULTIPLIER AND STABILIZATION FACTORS.
     C*
     C* TAX
     C*****              Z-ADD     *ZERO         XXDIS3           11 2
     C*****              XFOOT     STNT          XXPREM
     C*****
     C*****ELMULT        IFNE      *ZERO
     C*****XXPREM        MULT(H)   ELMULT        XXDIS3
     C*****              ENDIF                                                  ELMULT IFNE
     C*****
     C*****BILL          IFNE      'A'
     C*****
     C*****XXSTAN        IFNE      *ZERO
     C*****FNPFFL        ANDEQ     'P'
     C*****ELFYR         ANDGE     94
     C*****              Z-ADD     0             ELPROR
      *****
     C*****FDAYS         IFNE      *ZERO
     C*****PDAYS         DIV(H)    FDAYS         ELPROR
     C*****              ENDIF                                                  FDAYS IFNE 0
      *****
     C*****XXNNET        MULT(H)   ELPROR        XXWK10
     C*****XXWK10        DIV(H)    XXSTAN        XXSTFC           11 9
     C*****              Z-ADD(H)  XXSTFC        ELSTFC
     C*****              ENDIF                                                  ELSTPR IFNE
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****ELMULT        ANDNE     *ZERO
     C*****XXDIS3        MULT(H)   XXSTFC        XXDIS3
     C*****              ELSE
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****XXPREM        MULT(H)   XXSTFC        XXDIS3
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****
     C*****              ENDIF                                                  BILL IFNE
     C*****
     C*****XXDIS3        IFNE      *ZERO
     C*****XXPREM        SUB       XXDIS3        XXDIS3
     C*****              ENDIF                                                  XXDIS3 IFNE
     C*****
     C*****              Z-ADD     1             X
     C*****RDAT(X)       DOWNE     *ZERO
     C*****XXDIS3        MULT      PRO(X)        DIS(X)
     C*****              SUB       DIS(X)        STNT(X)
     C*****              ADD       1             X
     C*****              ENDDO                                                  RDAT,X DOWNE
     C*
     C* 1ST ASSESSMENT
     C*
     C*****              Z-ADD     *ZERO         XXDIS3           11 2
     C*****              XFOOT     STN1          XXPREM
     C*****
     C*****ELMULT        IFNE      *ZERO
     C*****XXPREM        MULT(H)   ELMULT        XXDIS3
     C*****              ENDIF                                                  ELMULT IFNE
     C*****BILL          IFNE      'A'
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****ELMULT        ANDNE     *ZERO
     C*****XXDIS3        MULT(H)   XXSTFC        XXDIS3
     C*****              ELSE
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****XXPREM        MULT(H)   XXSTFC        XXDIS3
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****              ENDIF                                                  BILL IFNE
     C*****
     C*****XXDIS3        IFNE      *ZERO
     C*****XXPREM        SUB       XXDIS3        XXDIS3
     C*****              ENDIF                                                  XXDIS3 IFNE
     C*****
     C*****              Z-ADD     1             X
     C*****RDAT(X)       DOWNE     *ZERO
     C*****XXDIS3        MULT      PRO(X)        DIS(X)
     C*****              SUB       DIS(X)        STN1(X)
     C*****              ADD       1             X
     C*****              ENDDO                                                  RDAT,X DOWNE
     C*
     C* 2ND ASSESSMENT
     C*
     C*****              Z-ADD     *ZERO         XXDIS3           11 2
     C*****              XFOOT     STN2          XXPREM
     C*****
     C*****ELMULT        IFNE      *ZERO
     C*****XXPREM        MULT(H)   ELMULT        XXDIS3
     C*****              ENDIF                                                  ELMULT IFNE
     C*****BILL          IFNE      'A'
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****ELMULT        ANDNE     *ZERO
     C*****XXDIS3        MULT(H)   XXSTFC        XXDIS3
     C*****              ELSE
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****XXPREM        MULT(H)   XXSTFC        XXDIS3
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****              ENDIF                                                  BILL IFNE
     C*****
     C*****XXDIS3        IFNE      *ZERO
     C*****XXPREM        SUB       XXDIS3        XXDIS3
     C*****              ENDIF                                                  XXDIS3 IFNE
     C*****
     C*****              Z-ADD     1             X
     C*****RDAT(X)       DOWNE     *ZERO
     C*****XXDIS3        MULT      PRO(X)        DIS(X)
     C*****              SUB       DIS(X)        STN2(X)
     C*****              ADD       1             X
     C*****              ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM
     C*
     C*****              Z-ADD     *ZERO         XXDIS3           11 2
     C*****              XFOOT     STN           XXPREM
     C*****              Z-ADD     XXPREM        XXDSPR           15 4
     C*****
     C*****ELMULT        IFNE      *ZERO
     C*****XXPREM        MULT(H)   ELMULT        XXDIS3
     C*****              ENDIF                                                  ELMULT IFNE
     C*****BILL          IFNE      'A'
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****ELMULT        ANDNE     *ZERO
     C*****XXDIS3        MULT(H)   XXSTFC        XXDIS3
     C*****              ELSE
     C*****
     C*****ELSTFC        IFNE      *ZERO
     C*****XXPREM        MULT(H)   XXSTFC        XXDIS3
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****
     C*****              ENDIF                                                  ELSTFC IFNE
     C*****              ENDIF                                                  BILL IFNE
     C*****
     C*****XXDIS3        IFNE      *ZERO
     C*****XXPREM        SUB       XXDIS3        XXDIS3
     C*****              ENDIF                                                  XXDIS3 IFNE
     C*****
     C*****              Z-ADD     1             X
     C*****RDAT(X)       DOWNE     *ZERO
     C*****XXDIS3        MULT      PRO(X)        DIS(X)
     C*****              SUB       DIS(X)        STN(X)
     C*****              ADD       1             X
     C*****              ENDDO                                                  RDAT,X DOWNE
     C*****              ENDIF                                                  XXCLT# <> 025
     C*** end del ******** 02/25/10 ************************************
     C*** END   ADD ****** 01/14/10 ************************************
      *
     C                   ENDIF                                                  XXCLT# = 046
     C*
     C                   XFOOT     STN           XXDISP           15 4
     C                   XFOOT     STN1          X1DISP           15 4
     C                   XFOOT     STN2          X2DISP           15 4
     C                   XFOOT     STN3          X3DISP           15 4
     C                   Z-ADD     STN1          A1D
     C                   Z-ADD     STN2          A2D
     C                   Z-ADD     STN3          A3D
     C                   Z-ADD     STN4          A4D
     C                   Z-ADD     STN5          A5D
     C                   XFOOT     STN4          X4DISP           15 4
     C                   XFOOT     STN5          X5DISP           15 4
     C                   Z-ADD     STN6          A6D
     C                   Z-ADD     STN7          A7D
     C                   XFOOT     STN6          X6DISP           15 4
     C                   XFOOT     STN7          X7DISP           15 4
     C                   Z-ADD(H)  XXDIS4        XXWK10
     C                   Z-ADD     XXWK10        XXDIS4
     C                   Z-ADD(H)  XXDISP        XXWK10
     C                   Z-ADD     XXWK10        XXDISP
     C                   Z-ADD(H)  XXDIS3        XXWK10
     C                   Z-ADD     XXWK10        XXDIS3
     C                   SUB       XXDIS3        XXDIS4
     C     XXDIS4        IFNE      XXDISP
     C                   SUB       XXDISP        XXDIS4
     C                   ADD       XXDIS4        XXDISP
     C                   ADD       XXDIS4        STN(1)
     C                   ENDIF                                                  XXDIS4 IFNE
     C*
     C************************************************************
     C*
     C*  COMPUTE DEVIATION  - IF TAKEN FROM DISCOUNTED PREMIUM.
     C*
     C*
     C     RRDV1L        IFEQ      'D'
     C     ELASP2        ANDNE     *ZERO
     C                   Z-ADD     ELASP2        XXPCT
     C                   Z-ADD     ELDVEF        XXEFD
     C                   Z-ADD     STN           PRM
     C     ELDVEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELDVEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDC2
     C                   Z-ADD(H)  XXDISA        XXDIS2
     C                   Z-ADD     PRM           STN
     C*** begin add ***  03/29/13  **************************************
      *
     C                   Z-ADD     STNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNL
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
      *
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
      *
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Apply deviation to unearned premium
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C                   Z-ADD     UPSN          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           UPSN
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C*** end add *****  03/29/13  **************************************
      *
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD1S2        IFNE      0
     C                   Z-ADD     ELD1S2        XXPCT
     C                   Z-ADD     ELD1D2        XXEFD
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD1X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     STN           PRM
     C*
     C                   EXSR      DISCA
     C*
     C                   Z-ADD     DIS           D1E2
     C                   Z-ADD     PRM           STN
     C                   Z-ADD(H)  XXDISA        XXD1D2
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
      *
     C                   Z-ADD     STNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNL
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
     C*
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
      *
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
      *
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Apply deviation to unearned premium
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C                   Z-ADD     UPSN          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           UPSN
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C************************************************************
     C*
     C*  COMPUTE 2ND DEVIATION  - IF TAKEN FROM DISCOUNTED PREMIUM.
     C*
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****RRDV2L        IFEQ      'D'
     C*****ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C*****XXCLT#        ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C*** begin chg ****** 02/25/10 ************************************
     C*****RRDV2L        OREQ      'M'
     C*****RRDV2L        OREQ      'D'
     C*** end chg ******** 02/25/10 ************************************
     C*****XXCLT#        ANDEQ     '025'
     C*****ELSTFC        ANDNE     *ZERO
     C     RRDV2L        IFEQ      'D'
     C     ELDEV2        IFNE      *ZERO
     C     ELSTFC        ORNE      *ZERO
     C     ELDEV2F       ORNE      *ZERO
     C*** End   Chg *** 04/07/17 ***************************************
     C*
     C*** Begin Chg *** 04/07/17 ***************************************
     C*****XXCLT#        IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C**********         Z-ADD     ELDEV2        XXPCT
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C**********         ELSE
     C*****ELSTFC        MULT(H)   -100          XXPCT
     C**********         ADD       100           XXPCT
     C**********         ENDIF                                                  XXCLT# <> '025'
     C                   Select
     C                   When      ELstFc  <> 0
     C                   Eval      xxPct = %DecH(ELstFc*-100:12:9) + 100
     C                   When      ELdev2F <> 0
     C                   Eval      xxPct = %DecH(ELdev2F*-100:12:9) + 100
     C                   When      ELdev2  <> 0
     C                   Eval      xxPct = ELdev2
     C                   EndSl
     C*** End   Chg *** 04/07/17 ***************************************
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     ELD2EF        XXEFD
     C                   Z-ADD     STN           PRM
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate Before Premium
     C                   Eval      xxDev2BP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C     ELD2EX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2EX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDV2
     C                   Z-ADD(H)  XXDISA        XXDEV2
     C*** Begin Add *** 04/07/17 ***************************************
     C* Populate After Premium
     C                   Eval      xxDev2AP = %Xfoot(PRM)
     C*** End   Add *** 04/07/17 ***************************************
     C                   Z-ADD     PRM           STN
     C                   Z-ADD     STNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNL
     C*** begin add ***  03/29/13  **************************************
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
      *
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
      *
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
      *
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
      *
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Apply deviation to unearned premium
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C                   Z-ADD     UPSN          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           UPSN
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C*** end add *****  03/29/13  **************************************
     C*
     C*** BEG ADD *****  06/21/10  ************************************
     C     ELD2S2        IFNE      0
     C                   Z-ADD     ELD2S2        XXPCT
     C                   Z-ADD     ELD2D2        XXEFD
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELD2X2        XXEXD
     C                   ENDIF
     C*
     C                   Z-ADD     STN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           D2E2
     C                   Z-ADD     PRM           STN
     C                   Z-ADD(H)  XXDISA        XXD2D2
     C*
     C                   Z-ADD     STNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNL
     C*** begin del ***  03/29/13  **************************************
     C*********          ENDIF
     C*** end del *****  03/29/13  **************************************
     C*** END ADD *****  06/21/10  ************************************
     C     XXDDCT        IFEQ      0
     C     RRIDTP        OREQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STNT
     C                   ENDIF
     C*
     C     RRST          IFNE      'MA'
     C     RR1IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR1IDV        OREQ      'Y'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN1
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR2IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR2IDV        OREQ      'Y'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN2
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR3IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR3IDV        OREQ      'Y'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN3
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C     RRST          IFNE      'MA'
     C     RR4IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR4IDV        OREQ      'Y'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN4
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR5IDV        OREQ      'Y'
     C     XXDDCT        IFEQ      0
     C     RR5IDV        OREQ      'Y'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN5
     C                   ENDIF
     C                   ENDIF                                                  RRST <> 'MA'
     C     RRST          IFNE      'MA'
     C     RR6IDV        OREQ      'Y'
      *
     C     XXDDCT        IFEQ      0
     C     RR6IDV        OREQ      'Y'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN6
     C                   ENDIF
      *
     C                   ENDIF                                                  RRST <> 'MA'
      *
     C     RRST          IFNE      'MA'
     C     RR7IDV        OREQ      'Y'
      *
     C     XXDDCT        IFEQ      0
     C     RR7IDV        OREQ      'Y'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           STN7
     C                   ENDIF
      *
     C                   ENDIF                                                  RRST <> 'MA'
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Apply deviation to unearned premium
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C                   Z-ADD     UPSN          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     PRM           UPSN
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C*** begin add ***  03/29/13  **************************************
     C                   ENDIF                                                  If ELD1S2 <> 0
     C*** end add *****  03/29/13  **************************************
     C*** Begin Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  ELDEV2/STFC/DEV2F
     C*** End   Add *** 04/07/17 ***************************************
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C*
     C* WAVIER OF SUBROGRATION - APPLIED TO DISCOUNTED
     C*
     C     ELWVFL        IFEQ      'D'
     C     ELWVTP        ANDNE     *BLANK
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     STNT          PRMT
     C                   Z-ADD     STN1          PRM1
     C                   Z-ADD     STN2          PRM2
     C                   Z-ADD     STN3          PRM3
     C                   Z-ADD     STN4          PRM4
     C                   Z-ADD     STN5          PRM5
     C                   Z-ADD     STN6          PRM6
     C                   Z-ADD     STN7          PRM7
     C                   Z-ADD     STNL          PRML
     C*** begin add ***  03/28/13  **************************************
     C                   Z-ADD     UPSN          PRMU
     C*** end add *****  03/28/13  **************************************
     C*
     C                   EXSR      WAIV
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       WAV(X)        STN(X)
     C                   ADD       WAVT(X)       STNT(X)
     C                   ADD       WAV1(X)       STN1(X)
     C                   ADD       WAV2(X)       STN2(X)
     C                   ADD       WAV3(X)       STN3(X)
     C                   ADD       WAV4(X)       STN4(X)
     C                   ADD       WAV5(X)       STN5(X)
     C                   ADD       WAVL(X)       STNL(X)
     C                   ADD       WAV6(X)       STN6(X)
     C                   ADD       WAV7(X)       STN7(X)
     C*** begin add ***  03/28/13  **************************************
     C                   ADD       WAVU(X)       UPSN(X)
     C*** end add *****  03/28/13  **************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADD ADDITIONAL LOAD TO DISCOUNTED PREMIUM
     C*
     C     RRASFL        IFEQ      'D'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       ADL(X)        STN(X)
     C                   ADD       ADL(X)        STNT(X)
     C     RRST          IFEQ      'MA'
      *
     C     RR1IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN1(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ENDIF
      *
     C     RR2IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN2(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN2(X)
     C                   ENDIF
      *
     C     RR3IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN3(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN3(X)
     C                   ENDIF
      *
     C     RR4IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN4(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN4(X)
     C                   ENDIF
      *
     C     RR5IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN5(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN5(X)
     C                   ENDIF
      *
     C     RR6IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN6(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN6(X)
     C                   ENDIF
      *
     C     RR7IDV        IFNE      'Y'
     C                   ADD       ADLD(X)       STN7(X)
     C                   ELSE
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF
      *
     C                   ELSE
     C                   ADD       ADL(X)        STN1(X)
     C                   ADD       ADL(X)        STN2(X)
     C                   ADD       ADL(X)        STN3(X)
     C                   ADD       ADL(X)        STN4(X)
     C                   ADD       ADL(X)        STN5(X)
     C                   ADD       ADL(X)        STN6(X)
     C                   ADD       ADL(X)        STN7(X)
     C                   ENDIF
     C                   ADD       ADL(X)        STNL(X)
     C*** begin add ***  03/28/13  **************************************
     C                   ADD       ADL(X)        UPSN(X)
     C*** end add *****  03/28/13  **************************************
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  RRASFL IFEQ
     C*** BEGIN ADD ***  01/12/10  **************************************
     C                   Z-ADD     STN           S2N
     C                   Z-ADD     STNL          S2NL
     C                   Z-ADD     STNT          S2NT
     C                   Z-ADD     STN1          S2N1
     C                   Z-ADD     STN2          S2N2
     C                   Z-ADD     STN3          S2N3
     C                   Z-ADD     STN4          S2N4
     C                   Z-ADD     STN5          S2N5
     C                   Z-ADD     STN6          S2N6
     C                   Z-ADD     STN7          S2N7
     C*** begin add ***  03/28/13  **************************************
     C                   Z-ADD     UPSN          PRMU
     C*** end add *****  03/28/13  **************************************
     C*** begin add ***  08/03/17  **************************************
     C*** begin chg ***  06/14/23  **************************************
     C******             Xfoot     STN           xxTargBase       11 0
     C                   If        xxst <> 'MI'
     C                   Xfoot     STN           xxTargBase       11 0
     C                   Endif
     C*** end chg *****  06/14/23  **************************************
     C*** end add *****  08/03/17  **************************************
     C*
     C* APPLY 1ST LARGE DEDUCTIBLE TO DISCOUNTED PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV1        ANDEQ     'D'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT1        XXPCT
     C                   Z-ADD     LDEFD1        XXEFD
     C     LDEXP1        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP1        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM W/O LOSS CONSTANT
     C                   Z-ADD     S2NL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Unearned Premium Basis
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C                   Z-ADD     PRMU          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        UPSN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C* APPLY 2ND LARGE DEDUCTIBLE TO DISCOUNTED PREMIUM.
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDPLV2        ANDEQ     'D'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C                   Z-ADD     LDPCT2        XXPCT
     C                   Z-ADD     LDEFD2        XXEFD
     C     LDEXP2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     LDEXP2        XXEXD
     C                   ENDIF
     C*
     C* PREMIUM
     C                   Z-ADD     S2N           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM W/O LOSS CONSTANT
     C                   Z-ADD     S2NL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* COMPUTE ADDITIONAL ARRAYS
     C                   EXSR      DEDA
     C*
     C*** begin add ***  03/28/13  **************************************
     C*
     C* Unearned Premium Basis
     C*
     C                   if        xxFlgCalcUPSC
     C*
     C                   Z-ADD     PRMU          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        UPSN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   ENDIF                                                  xxFlgCalcUPSC
     C*** end add *****  03/28/13  **************************************
     C                   ENDIF                                                  XXLRGD IFEQ
     C*** END ADD *****  01/12/10  **************************************
     C*
     C*****************************************************************
      *** Beg Add *****  11/11/24  *******************************************
     C*
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO DISCOUNTED PREMIUM
     C*
     C     RRRDPR        IFEQ      'D'
     C     WCFL06        ANDEQ     'Y'
     C     XXDDCT        ANDNE     *ZERO
     C     ELDCDE        ANDNE     *blanks
     C                   Z-ADD     XXDDCT        XXPCT
     C                   Z-ADD     STN           PRM
     C                   Z-ADD     ELDCEF        XXEFD
     C     ELDCEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELDCEX        XXEXD
     C                   ENDIF                                                  ELDCEX IFEQ
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDCT
     C                   Z-ADD(H)  XXDISA        XXDIST
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C     RR1IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     STN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR2IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     STN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR3IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     STN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     RR4IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     STN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR5IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     STN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     RR6IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     STN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR7IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     STN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     RRRDTX        IFEQ      'Y'
     C                   Z-ADD     STNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRRDTX IFEQ
     C     XXDD2P        IFNE      *ZERO
     C*
     C                   Z-ADD     S2N           PRM
     C                   Z-ADD     XXDD2P        XXPCT
     C                   Z-ADD     EL2DEF        XXEFD             7 0
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD             7 0
     C                   ELSE
     C                   Z-ADD     EL2DEX        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           EDD2
     C                   Z-ADD(H)  XXDISA        XXD2ST
     C                   Z-ADD     1             X
     C*
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     RR1IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR2IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR3IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR4IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR5IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR6IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C*
     C     RR7IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF
     C     RRRDTX        IFEQ      'Y'
     C                   Z-ADD     S2NT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRRDTX IFEQ
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRRDPR = D
      *** End Add *****  11/11/24  *******************************************
     C*
     C*  ARAP CALC #1 - APPLIED TO STANDARD PREMIUM (SVN)
     C*
     C                   Z-ADD     *ZERO         XXARP1           11 0
     C                   Z-ADD     *ZERO         XXARP2           11 0
     C                   Z-ADD     *ZERO         XXARP3           11 0
     C*
     C     ELARP1        IFNE      *ZERO
     C*
     C     ELARP1        SUB       1             XXPCT
     C                   MULT      100           XXPCT
     C     ELEMD1        IFLT      ELPFDT
     C                   Z-ADD     ELPFDT        XXEFD
     C                   ELSE
     C                   Z-ADD     ELEMD1        XXEFD
     C                   ENDIF
     C*
     C     ELEMD2        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELEMD2        XXEXD
     C                   ENDIF
     C*
     C     BILL          IFEQ      'A'
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEXD
     C     SVCNDT        ANDGT     XXEFD
     C                   Z-ADD     SVCNDT        XXEXD
     C                   ENDIF
     C                   ENDIF                                                  BILL IFEQ
     C*
     C* PREMIUM                                            & XXEXEF < PDAT,Z
     C*                                                    & XXEXEF < PDAT,Z
     C                   Z-ADD     SVN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           ARP1
     C                   Z-ADD(H)  XXDISA        XXARP1
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM W/O LOSS CONSTANT
     C*
     C                   Z-ADD     SVNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* TAXABLE PREMIUM
     C*
     C                   Z-ADD     SVNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 1ST ASSESSMENT PREMIUM
     C*
     C     RRST          IFNE      'MA'
     C                   Z-ADD     SVN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 2ND ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 3RD ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 4TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 5TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 6TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 7TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C                   ENDIF                                                  ELARP1 IFNE
     C*
     C************************************************************
     C*
     C*  ARAP CALC #2 - APPLIED TO STANDARD PREMIUM (SVN)
     C*
     C     ELARP2        IFNE      *ZERO
     C*
     C     ELARP2        SUB       1             XXPCT
     C                   MULT      100           XXPCT
     C                   Z-ADD     ELEMD2        XXEFD
     C     ELEMD3        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELEMD3        XXEXD
     C                   ENDIF
     C*
     C     BILL          IFEQ      'A'
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEXD
     C     SVCNDT        ANDGT     XXEFD
     C                   Z-ADD     SVCNDT        XXEXD
     C                   ENDIF
     C*
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEFD
     C                   Z-ADD     XXEFD         XXEXD
     C                   ENDIF
     C                   ENDIF                                                  BILL IFEQ
     C*
     C* PREMIUM                                                        DAT,Z
     C*                                                                DAT,Z
     C                   Z-ADD     SVN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           ARP2
     C                   Z-ADD(H)  XXDISA        XXARP2
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM W/O LOSS CONSTANT
     C*
     C                   Z-ADD     SVNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* TAXABLE PREMIUM
     C*
     C                   Z-ADD     SVNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 1ST ASSESSMENT PREMIUM
     C*
     C     RRST          IFNE      'MA'
     C                   Z-ADD     SVN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 2ND ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 3RD ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 4TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 5TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 6TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 7TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C                   ENDIF                                                  ELARP2 IFNE
     C*
     C************************************************************
     C*
     C*  ARAP CALC #3 - APPLIED TO STANDARD PREMIUM (SVN)
     C*
     C     ELARP3        IFNE      *ZERO
     C*
     C                   Z-ADD     ELEMD3        XXEFD
     C                   Z-ADD     ELPTDT        XXEXD
     C     ELARP3        SUB       1             XXPCT
     C                   MULT      100           XXPCT
     C*
     C     BILL          IFEQ      'A'
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEXD
     C     SVCNDT        ANDGT     XXEFD
     C                   Z-ADD     SVCNDT        XXEXD
     C                   ENDIF
     C*
     C     SVCNCL        IFEQ      'Y'
     C     SVCNDT        ANDLT     XXEFD
     C                   Z-ADD     XXEFD         XXEXD
     C                   ENDIF
     C                   ENDIF                                                  BILL IFEQ
     C*
     C* PREMIUM                                                        DAT,Z
     C*                                                                DAT,Z
     C                   Z-ADD     SVN           PRM
     C                   EXSR      DISCA
     C                   Z-ADD     DIS           ARP3
     C                   Z-ADD(H)  XXDISA        XXARP3
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* PREMIUM W/O LOSS CONSTANT
     C*
     C                   Z-ADD     SVNL          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STNL(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* TAXABLE PREMIUM
     C*
     C                   Z-ADD     SVNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 1ST ASSESSMENT PREMIUM
     C*
     C     RRST          IFNE      'MA'
     C                   Z-ADD     SVN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 2ND ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 3RD ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 4TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 5TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 6TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C* 7TH ASSESSMENT PREMIUM
     C*
     C                   Z-ADD     SVN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRST <> 'MA'
     C*
     C                   ENDIF                                                  ELARP3 IFNE
     C*
     C                   Z-ADD     1             X
     C                   Z-ADD     0             XXNORM           15 4
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       STN(X)        XXNORM
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     STN           NOM
     C                   Z-ADD(H)  XXNORM        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXNORM
     C                   Z-ADD     XXNORM        XXNET            15 4
     C                   XFOOT     STNT          XTNET            15 4
     C                   XFOOT     STNL          XLNET            15 4
     C                   XFOOT     STN1          X1NET            15 4
     C                   XFOOT     STN2          X2NET            15 4
     C                   XFOOT     STN3          X3NET            15 4
     C                   XFOOT     STN4          X4NET            15 4
     C                   XFOOT     STN5          X5NET            15 4
     C                   XFOOT     STN6          X6NET            15 4
     C                   XFOOT     STN7          X7NET            15 4
     C*** begin add ***  03/28/13  **************************************
     C                   XFOOT     UPSN          XUNET            15 4
     C*** end add *****  03/28/13  **************************************
     C*
     C***********************************************************
     C                   ENDSR
     C*
     C*====================================================================
     C* Calculate waiver charge
     C*====================================================================
     C     WAIV          BEGSR
     C*
     C     ELWVTP        IFEQ      'B'
     C*
     C* BLANKET CHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C*
     C* BLANKET WAIVER PERCENTAGE
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     PRM(X)        MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV(X)
     C     PRMT(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAVT(X)
     C     PRM1(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV1(X)
     C     PRM2(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV2(X)
     C     PRM3(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV3(X)
     C     PRM4(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV4(X)
     C     PRM5(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV5(X)
     C     PRM6(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV6(X)
     C     PRM7(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAV7(X)
     C     PRML(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAVL(X)
     C*** begin add ***  03/28/13  **************************************
     C     PRMU(X)       MULT      ELWVPC        XXPREM
     C     XXPREM        DIV(H)    100           WAVU(X)
     C*** end add *****  03/28/13  **************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C* CHECK MINIMUM CHARGE
     C*
     C                   XFOOT     WAV           XXWAVE           11 0
     C*
     C                   Z-ADD     ELWMIN        XXWMIN            9 2
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV           XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAVT          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAVT(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAVT          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAVT(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAVL          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAVL(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAVL          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAVL(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV1          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV1(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV1          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV1(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV2          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV2(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV2          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV2(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C                   XFOOT     WAV3          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV3(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV3          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV3(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV4          XXWAVE           11 0
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV4(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV4          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV4(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C                   XFOOT     WAV5          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV5(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV5          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV5(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV6          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV6(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV6          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV6(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV7          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV7(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV7          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV7(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*** begin add ***  03/28/13  **************************************
     C*
     C                   XFOOT     WAVU          XXWAVE           11 0
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAVU(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAVU          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAVU(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*** end add *****  03/28/13  **************************************
     C*
     C* CHECK MAXIMUM CHARGE
     C*
     C     ELWMAX        IFNE      *ZERO
      *
     C                   XFOOT     WAV           XXWAVE           11 0
     C                   Z-ADD     ELWMAX        XXWMAX            9 2
     C*
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV           XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAVT          XXWAVE           11 0
     C*
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAVT(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAVT          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAVT(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAVL          XXWAVE           11 0
     C*
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAVL(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAVL          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAVL(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV1          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV1(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV1          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV1(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV2          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV2(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV2          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV2(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV3          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV3(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV3          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV3(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV4          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV4(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV4          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV4(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV5          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV5(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV5          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV5(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV6          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV6(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV6          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV6(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C                   XFOOT     WAV7          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV7(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV7          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV7(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*** begin add ***  03/28/13  **************************************
     C*
     C                   XFOOT     WAVU          XXWAVE           11 0
     C     XXWAVE        IFGT      XXWMAX
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAVU(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAVU          XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAVU(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*** end add *****  03/28/13  **************************************
     C*
     C                   ENDIF                                                  ELWMAX <> 0
      *
     C                   ELSE                                                   ELWVPC ELSE
     C*
     C* FLAT BLANKET CHARGE
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     PRO(X)        MULT(H)   ELFCHG        WAV(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAVT(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAV1(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAV2(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAV3(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAV4(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAV5(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAV6(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAV7(X)
     C     PRO(X)        MULT(H)   ELFCHG        WAVL(X)
     C*** begin add ***  03/28/13  **************************************
     C     PRO(X)        MULT(H)   ELFCHG        WAVU(X)
     C*** end add *****  03/28/13  **************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV           XXWAVE           11 0
     C     XXWAVE        IFNE      ELFCHG
     C     XXWAVE        SUB       ELFCHG        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV(1)
     C                   SUB       X1DIFF        WAVT(1)
     C                   SUB       X1DIFF        WAV1(1)
     C                   SUB       X1DIFF        WAV2(1)
     C                   SUB       X1DIFF        WAV3(1)
     C                   SUB       X1DIFF        WAV4(1)
     C                   SUB       X1DIFF        WAV5(1)
     C                   SUB       X1DIFF        WAV6(1)
     C                   SUB       X1DIFF        WAV7(1)
     C                   SUB       X1DIFF        WAVL(1)
     C*** begin add ***  03/28/13  **************************************
     C                   SUB       X1DIFF        WAVU(1)
     C*** end add *****  03/28/13  **************************************
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELWVPC IFNE
     C*
     C                   ELSE                                                   ELWVTP ELSE
     C*
     C* SPECIFIC WAIVER CHARGE
     C*
     C                   Z-ADD     0             XXWAVE
     C*
     C     WVKY          CHAIN     PWV01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     WVSWPC        IFNE      *ZERO
     C*
     C     BILL          IFEQ      'A'
     C     WVAPAY        MULT      WVSWPC        XXPREM
     C                   ELSE
     C     WVEPAY        MULT      WVSWPC        XXPREM
     C                   ENDIF
     C*
     C                   DIV       100           XXPREM
     C*
     C     XXPREM        IFLT      WVWMIN
     C                   ADD       WVWMIN        XXWAVE
     C                   ELSE
     C                   Z-ADD(H)  XXPREM        XXWK10
     C                   ADD       XXWK10        XXWAVE
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   ADD       WVFCHG        XXWAVE
     C*
     C                   ENDIF                                                  WVSWPC IFNE
     C*
     C     WVKY          READE     PWV01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* CHECK SPECIFIC WAVIER TOTAL AGAINST MINIMUM
     C*
     C                   Z-ADD     ELWMIN        XXWMIN            9 2
     C*
     C     XXWAVE        IFLT      XXWMIN
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMIN        MULT(H)   PRO(X)        WAV(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAVT(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAV1(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAV2(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAV3(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAV4(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAV5(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAV6(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAV7(X)
     C     XXWMIN        MULT(H)   PRO(X)        WAVL(X)
     C*** begin add ***  03/28/13  **************************************
     C     XXWMIN        MULT(H)   PRO(X)        WAVU(X)
     C*** end add *****  03/28/13  **************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV           XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMIN
     C     XXWAVE        SUB       XXWMIN        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV(1)
     C                   SUB       X1DIFF        WAVT(1)
     C                   SUB       X1DIFF        WAV1(1)
     C                   SUB       X1DIFF        WAV2(1)
     C                   SUB       X1DIFF        WAV3(1)
     C                   SUB       X1DIFF        WAV4(1)
     C                   SUB       X1DIFF        WAV5(1)
     C                   SUB       X1DIFF        WAV6(1)
     C                   SUB       X1DIFF        WAV7(1)
     C                   SUB       X1DIFF        WAVL(1)
     C*** begin add ***  03/28/13  **************************************
     C                   SUB       X1DIFF        WAVU(1)
     C*** end add *****  03/28/13  **************************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWAVE        MULT(H)   PRO(X)        WAV(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAVT(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAV1(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAV2(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAV3(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAV4(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAV5(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAV6(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAV7(X)
     C     XXWAVE        MULT(H)   PRO(X)        WAVL(X)
     C*** begin add ***  03/28/13  **************************************
     C     XXWAVE        MULT(H)   PRO(X)        WAVU(X)
     C*** end add *****  03/28/13  **************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV           X1WAVE           11 0
     C     X1WAVE        IFNE      XXWAVE
     C     X1WAVE        SUB       XXWAVE        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV(1)
     C                   SUB       X1DIFF        WAVT(1)
     C                   SUB       X1DIFF        WAV1(1)
     C                   SUB       X1DIFF        WAV2(1)
     C                   SUB       X1DIFF        WAV3(1)
     C                   SUB       X1DIFF        WAV4(1)
     C                   SUB       X1DIFF        WAV5(1)
     C                   SUB       X1DIFF        WAV6(1)
     C                   SUB       X1DIFF        WAV7(1)
     C                   SUB       X1DIFF        WAVL(1)
     C*** begin add ***  03/28/13  **************************************
     C                   SUB       X1DIFF        WAVU(1)
     C*** end add *****  03/28/13  **************************************
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE IFLT
     C*
     C* CHECK SPECIFIC WAVIER TOTAL AGAINST MAXIMUM
     C*
     C                   Z-ADD     ELWMAX        XXWMAX            9 2
     C     XXWAVE        IFGT      XXWMAX
     C     XXWMAX        ANDNE     *ZERO
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXWMAX        MULT(H)   PRO(X)        WAV(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAVT(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAV1(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAV2(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAV3(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAV4(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAV5(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAV6(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAV7(X)
     C     XXWMAX        MULT(H)   PRO(X)        WAVL(X)
     C*** begin add ***  03/28/13  **************************************
     C     XXWMAX        MULT(H)   PRO(X)        WAVU(X)
     C*** end add *****  03/28/13  **************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     WAV           XXWAVE           11 0
     C     XXWAVE        IFNE      XXWMAX
     C     XXWAVE        SUB       XXWMAX        X1DIFF           15 4
     C                   SUB       X1DIFF        WAV(1)
     C                   SUB       X1DIFF        WAVT(1)
     C                   SUB       X1DIFF        WAV1(1)
     C                   SUB       X1DIFF        WAV2(1)
     C                   SUB       X1DIFF        WAV3(1)
     C                   SUB       X1DIFF        WAV4(1)
     C                   SUB       X1DIFF        WAV5(1)
     C                   SUB       X1DIFF        WAV6(1)
     C                   SUB       X1DIFF        WAV7(1)
     C                   SUB       X1DIFF        WAVL(1)
     C*** begin add ***  03/28/13  **************************************
     C                   SUB       X1DIFF        WAVU(1)
     C*** end add *****  03/28/13  **************************************
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAVE GT MAX
     C*
     C                   ENDIF                                                  ELWVTP IFEQ
     C*
     C                   ENDSR                                                  WAIV SUBR
     C*** BEGIN ADD ***  01/12/10  **************************************
     C*====================================================================
     C* CALCULATE LARGE DEDUCTIBLE FOR ADDITIONAL PREMIUM ARRAYS.
     C*====================================================================
     C     DEDA          BEGSR
     C*
     C* 1ST ASSESSMENT
     C     RR1IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR1IDD IFEQ
     C*
     C* 2ND ASSESSMENT
     C     RR2IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR2IDD IFEQ
     C*
     C* 3RD ASSESSMENT
     C     RR3IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR3IDD IFEQ
     C*
     C* 4TH ASSESSMENT
     C     RR4IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR4IDD IFEQ
     C*
     C* 5TH ASSESSMENT
     C     RR5IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR5IDD IFEQ
     C*
     C* 6TH ASSESSMENT
     C     RR6IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR6IDD IFEQ
     C*
     C* 7TH ASSESSMENT
     C     RR7IDD        IFEQ      'Y'
     C     XXST          OREQ      'PA'
     C                   Z-ADD     S2N7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RR7IDD IFEQ
     C*
     C* TAXABLE PREMIUM
     C     RRRDTX        IFEQ      'Y'
     C                   Z-ADD     S2NT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C                   ENDIF                                                  RRRDTX IFEQ
     C*
     C                   ENDSR
     C*** END ADD *****  01/12/10  **************************************
     C*** BEGIN ADD ***  11/10/10  ************************************
     C*****************************************************************
     C*
     C* SUBROUTINE TO APPLY HCN CREDIT AGAINST PREMIUM.
     C*
     C*****************************************************************
     C     SRHCN         BEGSR
     C*
     C                   Z-ADD     STN           SVN
     C                   Z-ADD     STNT          SVNT
     C                   Z-ADD     STN1          SVN1
     C                   Z-ADD     STN2          SVN2
     C                   Z-ADD     STN3          SVN3
     C                   Z-ADD     STN4          SVN4
     C                   Z-ADD     STN5          SVN5
     C                   Z-ADD     STN6          SVN6
     C                   Z-ADD     STN7          SVN7
     C*
     C     XXHCNF        MULT      -100          XXPCT
     C                   ADD       100           XXPCT
     C*
     C     HCKY          CHAIN     PHC02                              60
     c*** begin add ***  01/06/11  ***************************************
     c   60              z-add     0             xxhcnf
     c*** end add *****  01/06/11  ***************************************
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     SVN           PRM
     C                   Z-ADD     HCIN          XXEFD
     C*
     C     HCOUT         IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     HCOUT         XXEXD
     C                   ENDIF
     C*
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD(H)  XXDISA        XXWK11           11 0
     C                   ADD       XXWK11        XXHCNA
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   ADD       DIS(X)        EHCN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVNT          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STNT(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVN1          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN1(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVN2          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN2(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVN3          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN3(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVN4          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN4(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVN5          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN5(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVN6          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN6(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   Z-ADD     SVN7          PRM
     C                   EXSR      DISCA
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   SUB       DIS(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C     HCKY          READE     PHC02                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     C*** END ADD *****  11/10/10  ************************************
