     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRINPR - CALCULATE INFORCED PREMIUM
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM CALCULATES THE INFORCED PREMIUM, AND THE       *
     H*   CHANGES IN WRITTEN AND INFORCED PREMIUMS.  THE INFORCED     *
     H*   PREMIUM IS CALCULATED BY WRITING ANNUALIZED RECORDS INTO    *
     H*   THE OVERRIDE FILES AND CALLING THE BILLING PROGRAM W8010R.  *
     H*   THE EMPLOYER DETAIL IS ANNUALIZED BY MAKING ALL CURRENT     *
     H*   FACTORS EFFECTIVE THROUGHOUT THE POLICY PERIOD.  MID YEAR   *
     H*   PAYROLLS ARE ANNUALIZED AND GIVEN AN EFFECTIVE DATE EQUAL   *
     H*   TO THE POLICY START DATE.                                   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   SRINPRC                                                     *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   W8010R                                                      *
     H*                                                               *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 08/14/95               PROJECT: WC311                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/14/24 C:291 Project: WC2974                                *
     H*                Populate the Inforce Actual Premium fields for *
     H*                Increased Limits.                              *
     H*                                                               *
     H* 08/01/24 C:053 Project: WC2975                                *
     H*                Inforce Premium Calc Config                    *
     H*                                                               *
     H* 02/27/24 C:333 PROJECT: WC2953                                *
     H*                Premium History Balance to Minimum             *
     H*                                                               *
     H* 11/09/23 C:65  PROJECT: WC2810                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDMPP.                                *
     H*                                                               *
     H* 07/30/23 C:62  PROJECT: WC2918                                *
     H*                Populated non rating for exp mod 2 and 3       *
     H*                                                               *
     H* 07/30/20 C:65  PROJECT: Phw#90082 (Sunz)                      *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDMPP.                                *
     H*                                                               *
     H* 12/09/19 C:65  Project: Phw#83902/150WCS  (LUBA)              *
     H*                Clear WXEVP file before doing calculation      *
     H*                run for inforce premium.                       *
     H*                                                               *
     H* 02/12/19 C:291 Project: WC2206                                *
     H*                Populate the new flag EHNOEXM1 in WDEHP file.  *
     H*                                                               *
     H* 08/21/18 C:65  Project: 150WAR/150WAD/Phw#72966               *
     H*                Clear large deductible physical file.          *
     H*                (Data change in array)                         *
     H*                                                               *
     H* 05/07/18 C:65  Project: 150WAR/150WAD                         *
     H*                Copy large deductible setup information since  *
     H*                the file is now updated from billing.          *
     H*                                                               *
     H* 02/01/18 C:136 Project: 148WAH                                *
     H*                Populate Policy Fee in WDEHP                   *
     H*                                                               *
     H* 07/17/17 C:65  PROJECT: 150WAP                                *
     H*                ADD CODING TO RECOGNIZE THE 'C' VALUE FOR PEO. *
     H*                                                               *
     H* 04/07/17 C:062 Project 136WHF                                 *
     H*                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 09/14/16 C:136 Project: WC2070                                *
     H*                Populate Audit Noncompliance Charge            *
     H*                fields in WDEHP                                *
     H*                                                               *
     H* 04/27/16 C:306 Project: WC2424                                *
     H*                Populating the new fields in WDEHP file.       *
     H*                Exeperience Modifier - not final status.       *
     H*                                                               *
      * 04/19/16 C:242 Project: WC2069                                *
      *                Add Billing Fees to Premium History            *
      *                                                               *
     H* 12/15/15 C:65  Project: Phw#38901 (Coverisk/WC744)            *
     H*                Use latest ARAP factor to match modifier calc. *
     H*                                                               *
     H* 07/16/14 C:136 Project: WC2295                                *
     H*                Do not annualize OK Rate Split payrolls        *
     H*                                                               *
     H* 06/16/14 C:136 Project: WC2283                                *
     H*                Do not annualize TN Rate Split payrolls        *
     H*                                                               *
     H* 07/15/13 C:282 Project: WC2211                                *
     H*                Apply LCM from each payroll class when         *
     H*                calculating terrorism rate.                    *
     H*                                                               *
     H* 08/18/11 C:180 SUPPORT 77706                                  *
     H*                When reading all records, read by key using
     H*                company and fund to prevent record locks when
     H*                2 jobs for diff groups run simultaneously.
     H*
     H* 07/13/11 C:191 Project: WC2037                                *
     H*                Premium Determination on cancellations in GA   *
     H*                                                               *
     H* 04/06/11 C:235 PROJECT:114WYM                                *
     H*                COMPILED ONLY                                 *
     H*                                                               *
     H* 12/06/10 C:191 SUPPORT: WA114 75037 (114WVY)                  *
     H*                Inforce calculations for deviations should     *
     H*                use lates discount on the policy               *
     H*                                                               *
     H* 11/10/10 C:65  Project: 139WPL                                *
     H*                Add overrides for HCN Program and set the      *
     H*                credit for the inforce premium calculation.    *
     H*                                                               *
     H* 10/18/10 C:191 Project: WC1939                                *
     H*                When determining the annualized payroll on     *
     H*                an OK policy  take the rate split date into    *
     H*                consideration.                                 *
     H*                                                               *
     H* 06/21/10 C:255 Project: 114WVY                                *
     H*                Add changes in program to populate information *
     H*                of 2nd period deviation in Employer History fi-*
     H*                le (WDEHP) for split deviations.               *
     H*                                                               *
     H* 04/20/10 C:136 SUPPORT: WA139 72292                           *
     H*                CORRECT LOGIC TO MATCH HOW W8010R POPULATES    *
     H*                Q1SPRD (SPREAD DISCOUNT FLAG)                  *
     H*                                                               *
     H* 04/06/10 C:65  PROJECT: 139WNR                                *
     H*                FIX THE ANNUALIZING CHECK ON PAYROLL.          *
     H*                                                               *
     H* 02/01/10 C:65  PROJECT: WC1473/SUPP#71541                     *
     H*                OVERRIDE TO NEW PAYROLL LOGICAL WMEPL1E.       *
     H*                                                               *
     H* 12/29/09 C:136 PROJECT: WC1861                                *
     H*                ADD AZ RATE SPLIT - 01/01/2010                 *
     H*                                                               *
     H* 11/23/09 C:136 PROJECT: 139WNR                                *
     H*                FL RATE SPLIT FOR 07/01/09 - CLIENT SPECIFIC   *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 05/06/09 C:136 SUPPORT                                        *
     H*                OVER-RIDE TO WAELL1 SINCE W8010R REFERENCES    *
     H*                WDELL1                                         *
     H*                                                               *
     H* 12/16/08 C:136 PROJECT: WC1768                                *
     H*                DE RATE SPLIT FOR 10/01/08 - DO NOT ANNUALIZE  *
     H*                                                               *
     H* 10/02/07 C:65  PROJECT: 45WDI/45WFN                           *
     H*                Change "Paymnt due" verbiage from audit bill.  *
     H*                IMPLEMENTED INTO BASE FOR CLT045.              *
     H*                                                               *
     H* 08/31/07 C:136 WA139 59936                                    *
     H*                ALLOW PROGRAM TO RUN IN SINGLE DIVISION        *
     H*                MODE                                           *
     H*                                                               *
     H* 04/06/07  C:65  PROJECT: WC1514                               *
     H*                 APPLY SHORT RATE PENALTY TO DIFFERENT PREMIUM *
     H*                 LEVELS AND STORE AMOUNT IN A NEW FIELD.       *
     H*                                                               *
     H* 12/18/06 C:65 PROJECT SUPP#53349/WC1297                       *
     H*                IF A GROUP IS A PEO GROUP AND SET TO SPREAD    *
     H*                THE DISCOUNT ACROSS THE ENTIRE GROUP, ENSURE   *
     H*                THE FILE WADSP IS LOADED WITH THE ENTIRE       *
     H*                GROUP'S INFORMATION.                           *
     H*                THIS CHANGE COPIED BY C180 TO SOLVE
     H*                ALDON CONFLICTS.
     H*
     H*
     H* 10/03/06 C:180 PROJECT 114WMF                                 *
      *               REF. 114WBX
     H*               Need to annualize payroll in calculation of     *
     H*               inforce premium if policy year is short-term    *
     H*               and the user has elected to force a prorata     *
     H*               factor of 1.00.  This will ensure that the      *
     H*               inforce premium reflects an annual premium      *
     H*               number.                                         *
     H*
     H*               DO NOT ANNUALIZE ANY POPULATION RATED PAYROLLS. *
     H*
     H* 07/06/06 C:65  PROJECT: 132WEW                                *
     H*                POPULATE AIRCRAFT REINSURANCE CHARGE FIELDS.   *
     H*                Change copied by C180 to solve ALDON CONFLICTS.
     H*
     H* 06/19/06 C:136 PROJECT: 133WCV                                *
     H*                POPULATE HARBOR EXPOSURE FIELDS.               *
     H*                THIS CHANGE COPIED BY C180 TO SOLVE
     H*                ALDON CONFLICTS.
     H*
     H* 02/09/06 C:65 PROJECT SUPP#51273                              *
     H*               POPULATE FIELDS FOR WRITTEN CHANGE SUBJECT AND  *
     H*               MODIFIED PREMIUM ALONG WITH INFORCE/INFORCE     *
     H*               CHANGE SUBJECT AND MODIFIED PREMIUM.            *
     H*         C:136 ADD OVERRIDE FOR WDELP SO THAT COMMISSION       *
     H*               MOD FACTOR COULD BE PULLED IN BY WA060R IN      *
     H*               W8010R                                          *
     H*         C:136 FOUND OUT THAT SCHED DB/CR FACTOR CHANGE        *
     H*               WASN'T BEING POPULATED                          *
     H*                                                               *
     H* 01/24/06 C:65 PROJECT 114WHO                                  *
     H*               ZERO OUT INFORCE PREMIUM FIGURES FOR RETRO      *
     H*               WDEHP RECORDS.                                  *
     H*                                                               *
     H* 05/04/05 C:136 Project: 126WDK                                *
     H*                MOVE MULTIPLE DEDUCTIBLE DISCOUINTS INTO BASE. *
     H*                                                               *
     H* 01/07/05 C:136 PROJECT: WC1250                                *
     H*                POPULATE DOMESTIC TERRORISM FIELDS             *
     H*                                                               *
     H* 11/19/04 C:65  PROJECT: WC1149                                *
     H*                DO NOT ANNUALIZE PAYROLLS SPLIT FOR FL RATES.  *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: 93WEH, 109WDX                         *
     H*                ADD VOLUNTARY LABOR CHARGE                     *
     H*                                                               *
     H* 06/29/04 C:136 SUPPORT: WA109 42898                           *
     H*                IF POLICY FROM DATE = LEAP YEAR DATE THEN      *
     H*                ENSURE THAT POLICY TO DATE = YYY0228.          *
     H*                                                               *
     H* 06/04/03 C:65  PROJECT: SUPPORT/#37442                        *
     H*                LOAD WORKFILES ALWAYS IF AN EMPLOYER IS MULTI  *
     H*                DIVISION FOR MINIMUM PREMIUM CALCULATIONS.     *
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
     H* 11/27/01 C:65 PROJECT: WC1059                                 *
     H*               RUN THE CALCULATIONS FOR ANYONE WITH A REDUCED  *
     H*               EXPENSE CONSTANT AS WELL AS STOCK DISCOUNT.     *
     H*                                                               *
     H* 01/30/01 C:65 PROJECT: SUPPORT#25641                          *
     H*               FIND FIRST EXISTING DIVISION RATHER THAN DIV 0  *
     H*               FOR SPREADING DISC.                             *
     H*                                                               *
     H*  09/28/00 C:65 PROJECT: SUPPORT#24418                         *
     H*                IF AN ACCOUNT IS CANCELLED THEN ZERO OUT       *
     H*                THE INFORCE PREMIUM FIELDS.                    *
     H*                                                               *
     H*  06/08/00 C:65 PROJECT: SUPPORT                               *
     H*                LOAD EMPLOYER HISTORY WITH SCHEDULE DB/CR.     *
     H*                CHAIN FROM BEGINNING OF EMPLOYER RATHER
     H*                DIVISION BEING PASSED INTO PROGRAM.            *
     H*                                                               *
     H*  03/20/00 C:65 PROJECT: WC983                                 *
     H*                MISCELLANEOUS SUPPORT CHANGES:  DO NOT LOAD    *
     H*                THE OVERRIDE WDELP FILE WITH THE VALUES FROM   *
     H*                EMPLOYER HISTORY SINCE THE DIVISION CHECK IS   *
     H*                ALWAYS TRUE AND EVERY DIVISION RECEIVES THE    *
     H*                DISCOUNT STRUCTURE OF THE FIRST DIVISION.      *
     H*                ALSO, ANNUALIZE THE PAYROLL IF AN ACCOUNT IS   *
     H*                SHORT TERM AND AUDITED AND THE PAYROLL IS      *
     H*                EFFECTIVE FOR THE FULL POLICY PERIOD. THIRD,   *
     H*                UPDATE THE OVERRIDE FILE TO BE EFFECTIVE FOR   *
     H*                A FULL YEAR AND REMOVE THE ANNUALIZATION OF    *
     H*                THE ANNUAL PREMIUM FOR THE INFORCE FIGURE.     *
     H*                WE CANNOT ANNUALIZE THE ANNUAL PREMIUM BECAUSE *
     H*                THE POLICY MAY INVOKE MINIMUM PREMIUM OR MIN   *
     H*                EXPENSE CONSTANT.  LASTLY, IF THERE IS A 2ND   *
     H*                INCREASE LIMITS ON THE EMPLOYER MOVE THAT INCR *
     H*                LIMITS TO THE FIRST FIELD AND REMOVE THE 2ND.  *
     H*                IN ADDITION ADD OVERRIDES TO FILES WAILP AND   *
     H*                WAMPP.                                         *
     H*                                                               *
     H*  11/15/99 C:65 PROJECT: WC936                                 *
     H*                AFTER LOADING STD PREM, RUN CALC FOR EVERY     *
     H*                DIVISION ON AN EMPLOYER.  AFTER THAT, SKIP     *
     H*                TO NEXT EMPLOYER # IN THE HISTORY FILE.        *
     H*               SEND A UNIQUE VALUE TO EACH CALC PROGRAM TO     *
     H*               REDUCE REDUNDENT CALCULATIONS.                  *
     H*                                                               *
     H*  06/01/99 C:65 PROJECT: CM045                                 *
     H*                UPDATE NEW FIELDS FOR BILLING.                 *
     H*                                                               *
     H*  05/21/99 C:65 PROJECT: SUPPORT #17543                        *
     H*                IF THERE ARE NO PREVIOUS WDEHP RECORDS THEN    *
     H*                SET ALL THE CHANGE FIELDS TO THE VALUE OF      *
     H*                THE CURRENT WRITTEN/INFORCE AMOUNTS.           *
     H*                                                               *
     H*  03/03/98 C:65 PROJECT: WC705                                 *
     H*                UPDATE NEW FIELDS FOR BILLING.                 *
     H*                                                               *
     H*  01/19/98 C:65 PROJECT: WC633                                 *
     H*                ADD 2ND INCR LIMITS TO EMP HISTORY FILE.       *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  20  PROCESS A SINGLE RECORD                                  *
     H*  50  PRIMARY READ INDICATOR                                   *
     H*  60  UTILITY INDICATOR                                        *
     H*  61  UTILITY INDICATOR                                        *
     H*****************************************************************
     FWDEHL2    UF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWAEPL     IF A E           K DISK    USROPN
     F                                     RENAME(PEP01:PEP02)
     FWDELL     UF   E           K DISK
     FWAELL     IF A E           K DISK    USROPN
     F                                     RENAME(PEL01:PEL02)
     FWAELL1    IF   E           K DISK    USROPN
     F                                     RENAME(PEL01:PELL1)
     FWDDSL     IF   E           K DISK
     FWADSL     IF A E           K DISK    USROPN
     F                                     RENAME(PDS01:PDS02)
     FWDILL     IF   E           K DISK
     FWAILL     IF A E           K DISK    USROPN
     F                                     RENAME(PIL01:PIL02)
     F*** begin add ***  05/07/18  **********************************
     FWMLDL1    IF   E           K DISK
     FWALDL1    IF A E           K DISK    USROPN
     F                                     RENAME(PLD01:PLD02)
     F*** end add *****  05/07/18  **********************************
     FWDMPL     IF   E           K DISK
     FWAMPL     IF A E           K DISK    USROPN
     F                                     RENAME(PMP01:PMP02)
     FWDHCLR    IF   E           K DISK
     F                                     RENAME(PHC01:PHC0R)
     FWDHCL3    IF A E           K DISK
     F                                     RENAME(PHC01:PHC03)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWDEHL4    IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*++ BEGIN ADD +++  03/05/03  ++++++++++TBC 10/03/06++++++++++++++
     FWMPRL1    IF   E           K DISK
     F*++ END ADD +++++  03/05/03  ++++++++++TBC 10/03/06++++++++++++++
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SSY01)
     FWMPXL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMRTL     IF   E           K DISK
     F                                     RENAME(PRT01:PRT0X)
     F*** begin add ***  12/09/19  *******************************
     Fwxevp     UF   E           K DISK    RENAME(pev01:pev0p) USROPN
     F*** end add *****  12/09/19  *******************************
     F*** begin add ***  08/01/24  *******************************
     Fwmexl     IF   E           K DISK
     F*** end add *****  08/01/24  *******************************
     F*

     D*** begin chg ***  05/07/18  **********************************
     D*QCA*****        S             55    DIM(23) CTDATA PERRCD(1)             CMD EXEC/QCAEXEC
     D QCA             S             55    DIM(26) CTDATA PERRCD(1)             CMD EXEC/QCAEXEC
     D*** end chg *****  05/07/18  **********************************
     D*
     D*
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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*++ BEGIN ADD ++++ 04/18/01  +++++++++++++++TBC 10/03/06+++++++++
     D                 DS
     D  XXTODT                 1      7  0
     D  XXTOYR                 1      3  0
     D*++ END ADD +++++  04/18/01  ++++++++++++++++++++++++TBC 10/03/06
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* GROUP MASTER FILE.
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* SPREAD DISCOUNT FILE.
     C*
     C     DSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  EMPLOYER DISCOUNT FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXFAF
     C*
     C     EHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C     EHKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  WORK COMP CONTROL FILE
     C*
     C     WCKY          KLIST
     C                   KFLD                    XCKEY
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XPST              2
     C*++ BEGIN ADD +++  03/05/03  +++++++++++++++++++++TBC 10/03/06+++
     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    ELPFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C*++ END ADD +++++  03/05/03  +++++++++++++++++++++TBC 10/03/06+++
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEFDT        XXEFDT
     C     *LIKE         DEFINE    EHFAF         XXFAF
     C     *LIKE         DEFINE    WCKEY         XCKEY
     C     *LIKE         DEFINE    ELANDT        XXANDT
     C     *LIKE         DEFINE    ELANDT        XXLPDT
     C     *LIKE         DEFINE    ELANDT        XQFROM
     C     *LIKE         DEFINE    ELANDT        XQTO
     C     *LIKE         DEFINE    EPEAPR        XXEAPR
      *
     C*++ BEGIN ADD ++++ 04/18/01  +++++++++++++++++++++TBC 10/03/06+++
     C     *LIKE         DEFINE    ELPFDT        X2PFDT
     C     *LIKE         DEFINE    ELPTDT        X2PTDT
     C     *LIKE         DEFINE    ELPROR        X2PROR
     C     *LIKE         DEFINE    ELFFL9        X2FFL9
     C*++ END ADD ++++++ 04/18/01  +++++++++++++++++++++TBC+10/03/06+++
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQFAF             1
     C                   PARM                    QQRNB             1
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
     C     Q1PRMS        IFGT      7
     C                   MOVE      QQRNB         XXRNB             1
     C                   ENDIF
     C*** begin add ***  08/01/24  *******************************
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C     FNKY          CHAIN     PEX01                              51
     C     EXbinf        Ifeq      'Y'
     C                   Move      *on           BypInfPrmC        1
     C                   Else
     C                   Move      *off          BypInfPrmC
     C                   Endif
     C*** end add *****  08/01/24  *******************************
     C*
     C*  IF EMPLOYER NUMBER IS NOT BLANK, SET UP FOR SINGLE RECORD PROCESS
     C*
     C     QQEMP#        IFNE      *BLANK
     C     QQEMP#        ANDNE     *ZEROS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*
     C     CLTID         IFEQ      '045'
     C*
     C     QQFAF         IFEQ      'A'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT
     C                   ENDIF                                                  QQFAF IFEQ
     C*
     C                   ENDIF                                                  CLTID IFEQ
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      QQFAF         XXFAF
     C                   MOVE      *ON           *IN20
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*** begin add ***  12/09/19  *******************************
      /Free
       Monitor;
          Open wxevp;
          Read pev0p;
          Dow not %eof;
             Delete pev0p;
             Read pev0p;
          Enddo;
          Close wxevp;
       On-Error;
       Endmon;
      /End-Free
     C*** end add *****  12/09/19  *******************************
     C*
     C*  OVERRIDE DATABASE FILE WDELP TO WAELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(18)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDELL TO WAELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C*
     C*  OVERRIDE DATABASE FILE WDELL1 TO WAELL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(20)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL1 TO WAEPL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL1E TO WAEPL1E
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(22)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDDSL TO WADSL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDILL TO WAILL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(12)
     C                   PARM                    LENGTH           15 5
     C*** begin add ***  05/07/18  **********************************
     C*
     C*  OVERRIDE DATABASE FILE WMLDL1 TO WALDL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(24)
     C                   PARM                    LENGTH           15 5
     C*** end add *****  05/07/18  **********************************
     C*
     C*  OVERRIDE DATABASE FILE WDMPL TO WAMPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(15)
     C                   PARM                    LENGTH           15 5
     C*
     C*  GET SYSTEM VALUE FOR SPREAD DISCOUNT FLAG
     C*
     C                   MOVEL(P)  'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        WCFL02
     C                   ENDIF
     C                   MOVE      *BLANK        SVFAF             1
     C*
     C*  BEGIN PROCESSING HISTORY RECORDS
     C*
     C     *IN20         IFEQ      *OFF
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C     FNKY          SETLL     PEH01
     C     FNKY          READE     PEH01                                  50
     C                   ELSE
     C*
     C     QQFAF         IFEQ      'R'
     C     EHKY1         CHAIN     PEH01                              50
     C                   ELSE
     C     EHKY2         CHAIN     PEH01                              50
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C*  OPEN THE OVERRIDE FILES
     C*
     C                   OPEN      WAELL
     C                   OPEN      WAELL1
     C                   OPEN      WAEPL
     C                   OPEN      WADSL
     C                   OPEN      WAILL
     C                   OPEN      WAMPL
     C*** begin add ***  05/07/18  **********************************
     C                   OPEN      WALDL1
     C*** end add *****  05/07/18  **********************************
     C*
     C*  ESTABLISH KEY FIELDS
     C*
     C                   MOVE      EHCO#         XXCO#
     C                   MOVE      EHFND         XXFND
     C                   MOVE      EHFYR         XXFYR
     C                   MOVE      EHEMP#        XXEMP#
     C                   MOVE      EHDIV         XXDIV
     C     FNKY          CHAIN     PFN01                              60
     C*
     C*  LOAD OVERRIDE FILES
     C*
     C     ELKEY         CHAIN(N)  PEL01                              60
     C*++ BEGIN ADD +++  04/18/01  +++++++++++++++++++++TBC 10/03/06+++
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO              8 0
     C*
     C                   MOVE      X1FROM        QQFROM            8 0
     C                   MOVE      X1TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     X1TO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C*
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
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1FROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C                   Z-ADD     ELPFDT        X2PFDT
     C                   Z-ADD     ELPTDT        X2PTDT
     C                   Z-ADD     ELPROR        X2PROR
     C                   MOVEL     ELFFL9        X2FFL9
     C*** beg add *****  08/01/24  *******************************
     C                   If        BypInfPrmC = *off
     C*** end add *****  08/01/24  *******************************
     C*
     C*++ END ADD +++++  04/18/01  ++++++++++++++++++++TBC 10/03/06++++
     C                   EXSR      LDOVRF
     C*
     C*  DETERMINE IF SPREADING DISCOUNTS
     C*
     C                   MOVE      ' '           Q1SPRD            1
     C                   MOVE      ' '           XXMULT            1
     C     ELKY1         CHAIN     PEL03                              60
     C  N60ELKY1         READE     PEL03                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           XXMULT
     C                   ENDIF
     C*
     C     WCFL02        IFEQ      'Y'
     C*
     C                   MOVE      XXCO#         Q1CO#             3
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      XXFYR         Q1FYR             3
     C                   MOVE      XXEMP#        Q1EMP#            9
     C                   MOVE      ' '           Q1SPRD            1
     C                   CALL      'SRDISC'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1SPRD
     C     FNPEOF        IFEQ      'M'
     C     FNPEOA        ANDEQ     'Y'
     C     FNPEOF        OREQ      'G'
     C     FNPEOA        ANDEQ     'Y'
     C     FNPEOF        OREQ      'Y'
     C*** BEGIN ADD *** 07/17/17 **************************************
     C     FNPEOF        OREQ      'C'
     C     FNPEOA        ANDEQ     'Y'
     C*** END ADD ***** 07/17/17 **************************************
     C                   MOVE      'Y'           Q1SPRD
     C                   ENDIF
     C                   ENDIF                                                  WCFL02 = Y
     C*
     C     Q1SPRD        IFNE      'Y'
     C*
     C                   MOVE      XXCO#         Q1CO#             3
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      XXFYR         Q1FYR             3
     C                   MOVE      XXEMP#        Q1EMP#            9
     C                   MOVE      *BLANKS       Q1SPRD
     C*
     C                   CALL      'SREXCO'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1SPRD
     C*
     C                   ENDIF
     C*
     C*
     C*  IF THE DISCOUNT IS SPREAD, LOAD OVERRIDE FILES WAELP,
     C*  WAEPP, AND WADSP FOR ALL DIVISIONS.
     C*
     C     XXMULT        IFEQ      'Y'
     C     Q1SPRD        OREQ      'Y'
     C*
     C                   Z-ADD     0             XXDIV
     C     ELKY1         CHAIN     PEL03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C                   EXSR      LDOVRF
     C     ELKY1         READE     PEL03                                  60
     C                   ENDDO
     C*
     C*  IF WE ARE SPREADING THE DISCOUNT, RUN THE BILLING CALC FOR
     C*  EACH DIVISION TO GET THE INFORCED STANDARD PREMIUM INTO EACH
     C*  DIVISION'S OVERRIDE RECORD.
     C*
     C                   Z-ADD     0             XXDIV
     C     ELKY1         CHAIN     PEL03                              60
      *
     C     XXRNB         IFNE      '#'
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C*
     C                   MOVE      XXCO#         ZZCO#             3
     C                   MOVE      XXFND         ZZFND             3
     C                   MOVE      XXFYR         ZZFYR             3
     C                   MOVE      XXEMP#        ZZEMP#            9
     C                   MOVE      XXDIV         ZZDIV             5
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZDATE            8
     C*
     C                   MOVE      *BLANKS       ZZESCD            2
     C                   MOVE      'Y'           ZZOVR             1
     C                   MOVE      *BLANKS       ZZSRCD            2
     C                   MOVE      *BLANKS       ZZPRT             1
     C                   MOVE      *BLANKS       ZZAJCD            2
     C                   MOVE      'W'           ZZQTE             1
     C                   MOVE      'H'           ZZRNB             1
     C                   CALL      'W8010R'
     C                   PARM                    ZZCO#
     C                   PARM                    ZZFND
     C                   PARM                    ZZFYR
     C                   PARM                    ZZEMP#
     C                   PARM                    ZZDIV
     C                   PARM                    ZZDATE
     C                   PARM                    ZZESCD
     C                   PARM                    ZZOVR
     C                   PARM                    ZZSRCD
     C                   PARM                    ZZPRT
     C                   PARM                    ZZAJCD
     C                   PARM                    ZZQTE
     C                   PARM                    ZZRNB
     C*
     C     ELKY1         READE     PEL03                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXRNB <> '#'
     C*
     C                   ENDIF                                                  XXMULT = Y
     C*** beg add *****  08/01/24  *******************************
     C                   Endif
     C*** end add *****  08/01/24  *******************************
     C*
     C* RUN THE BILL CALCULATION TO GET THE INFORCED PREMIUM
     C*
     C     *IN50         DOWEQ     *OFF
     C                   Z-ADD     EHDIV         XXDIV
     C                   Z-ADD     EHFYR         XXFYR
     C     ELKEY         CHAIN(N)  PEL01                              60
     C*
     C                   MOVE      XXCO#         ZZCO#             3
     C                   MOVE      XXFND         ZZFND             3
     C                   MOVE      XXFYR         ZZFYR             3
     C                   MOVE      XXEMP#        ZZEMP#            9
     C                   MOVE      XXDIV         ZZDIV             5
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZDATE            8
     C*
     C                   MOVE      *BLANKS       ZZESCD            2
     C                   MOVE      'Y'           ZZOVR             1
     C                   MOVE      *BLANKS       ZZSRCD            2
     C                   MOVE      *BLANKS       ZZPRT             1
     C                   MOVE      *BLANKS       ZZAJCD            2
     C                   MOVE      'W'           ZZQTE             1
     C                   MOVE      'H'           ZZRNB             1
     C*** beg add *****  08/01/24  *******************************
     C                   If        BypInfPrmC = *off
     C*** end add *****  08/01/24  *******************************
     C                   CALL      'W8010R'
     C                   PARM                    ZZCO#
     C                   PARM                    ZZFND
     C                   PARM                    ZZFYR
     C                   PARM                    ZZEMP#
     C                   PARM                    ZZDIV
     C                   PARM                    ZZDATE
     C                   PARM                    ZZESCD
     C                   PARM                    ZZOVR
     C                   PARM                    ZZSRCD
     C                   PARM                    ZZPRT
     C                   PARM                    ZZAJCD
     C                   PARM                    ZZQTE
     C                   PARM                    ZZRNB
     C*
     C*  LOAD INFORCED FIELDS FROM THE OVERRIDE DETAIL RECORD
     C*
     C*
     C*  LINE ADDED SO THAT PROGRAM WOULD COMPILE, JUST NEED THE OVERRIDE
     C*
     C   60
     CANN60XXCO#         CHAIN     PELL1                              60
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
       //*** Beg Add ***** 02/27/24 **********************************
     c     DSKY          CHAIN     PMP02                              60
       //*** End Add ***** 02/27/24 **********************************
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   MOVE      *BLANK        SVCX              1
     C                   DO        5
     C     CD(XXX)       IFLE      ELPTDT
     C     CD(XXX)       ANDNE     0
     C     ED(XXX)       ANDNE     0
     C                   MOVE      'Y'           SVCX
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*** beg add *****  08/01/24  *******************************
     C                   Endif
     C*** end add *****  08/01/24  *******************************
     C*
     C     SVCX          IFEQ      'Y'
     C     EHFAF         ANDEQ     'A'
     C     SVCX          OREQ      'Y'
     C     EHFAF         ANDEQ     'X'
     C     SVFAF         ANDEQ     'A'
     C*** beg add *****  08/01/24  *******************************
     C     BypInfPrmC    Oreq      *on
     C*** end add *****  08/01/24  *******************************
     C*
     C                   Z-ADD     0             EHIMPR
     C                   Z-ADD     0             EHISPR
     C                   Z-ADD     0             EHINMP
     C                   Z-ADD     0             EHIAPR
     C                   Z-ADD     0             EHISBJ
     C                   Z-ADD     0             EHIMPM
     C                   MOVE      *BLANK        EHIMNP
     C                   Z-ADD     0             EHIDC1
     C                   Z-ADD     0             EHIDC2
     C                   Z-ADD     0             EHIDC3
     C*** Begin Add *** 04/07/17 *******************************
     C                   Z-ADD     0             EHDEV2IBP
     C                   Z-ADD     0             EHDEV2IAP
     C*** End   Add *** 04/07/17 *******************************
     C                   Z-ADD     0             EHIHCN
     C                   Z-ADD     0             EHIXP1
     C                   Z-ADD     0             EHIXP2
     C                   Z-ADD     0             EHIXP3
     C                   Z-ADD     0             EHIIN1
     C                   Z-ADD     0             EHIIN2
     C                   Z-ADD     0             EHIDCT
     C                   Z-ADD     0             EHIDD2
     C                   Z-ADD     0             EHIDCM
     C                   Z-ADD     0             EHIDCS
     C                   Z-ADD     0             EHIDCR
     C                   Z-ADD     0             EHIFRS
     C                   Z-ADD     0             EHIDV2
     C                   Z-ADD     0             EHD1I2
     C                   Z-ADD     0             EHD2I2
     C                   Z-ADD     0             EHIDC
     C                   Z-ADD     0             EHIWAV
     C                   Z-ADD     0             EHIADL
     C                   Z-ADD     0             EHITER
     C                   Z-ADD     0             EHIDTR
     C                   Z-ADD     0             EHIVLC
     C                   Z-ADD     0             EHIRP1
     C                   Z-ADD     0             EHIRP2
     C                   Z-ADD     0             EHIRP3
     C                   Z-ADD     0             EHIARC
     C                   Z-ADD     0             EHIHXM
     C*** BEG ADD ***  07/15/13  ************************************
     C                   Z-ADD     0             EHITRR
     C                   Z-ADD     0             EHIDRR
     C*** END ADD ***  07/15/13  ************************************
      *** Beg Add ***** 04/19/16 ***********************************
     C                   Z-ADD     0             EHI1AS
     C                   Z-ADD     0             EHI2AS
     C                   Z-ADD     0             EHI3AS
     C                   Z-ADD     0             EHI4AS
     C                   Z-ADD     0             EHI5AS
     C                   Z-ADD     0             EHI6AS
     C                   Z-ADD     0             EHI7AS
     C                   Z-ADD     0             EHISF
     C                   Z-ADD     0             EHIMF
      *** End Add ***** 04/19/16 ***********************************
      *** Beg Add ***** 09/14/16 ***********************************
      /Free
       EHianc = 0;
      /End-free
      *** End Add ***** 09/14/16 ***********************************
      *** Beg Add ***** 02/01/18 ***********************************
     C                   Z-ADD     0             EHIPF
      *** End Add ***** 02/01/18 ***********************************
      *** Beg Add ***** 02/27/24 ***********************************
      /Free
       EHIBAL = 0;
       EHIAMAN = 0;
       EHIASBJ = 0;
       EHIAMOD = 0;
       EHIASTD = 0;
       EHIANORM = 0;
       EHIAANN = 0;
       EHIAIL1 = 0;
       EHIAIL2 = 0;
       EHIADC1 = 0;
       EHIADC2 = 0;
       EHIADC3 = 0;
       EHIADV2 = 0;
       EHIADCT = 0;
       EHIADD2 = 0;
       EHIADCM = 0;
       EHIADCS = 0;
       EHIADC = 0;
       EHIAWAV = 0;
       EHIATCA = 0;
       EHIAPRD = 0;
       EHIAPRF = 0;
       EHIAXPA = 0;
       EHIAVLC = 0;
       EHIAARC = 0;
       EHIAD12 = 0;
       EHIAD22 = 0;
       EHIAHCN = 0;
       EHIAUSA = 0;
       EHIAXP1 = 0;
       EHIAXP2 = 0;
       EHIAXP3 = 0;
       EHIARP1 = 0;
       EHIARP2 = 0;
       EHIARP3 = 0;
       EHIALOS = 0;
       //*** Beg Add ***** 08/14/24 *************************************
       EHIBIL  = 0;
       EHIBIL2 = 0;
       //*** End Add ***** 08/14/24 *************************************
      /End-free
      *** End Add ***** 02/27/24 ***********************************
     C*
     C                   ELSE
     C                   MOVE      ELMAPR        EHIMPR
     C                   MOVE      ELESBJ        EHISBJ
     C                   MOVE      ELEMPM        EHIMPM
     C                   MOVE      ELSTPR        EHISPR
     C                   MOVE      ELENMP        EHINMP
     C                   Z-ADD     ELANPR        EHIAPR
     C                   MOVE      ELEMNP        EHIMNP
     C                   MOVE      ELEDC1        EHIDC1
     C                   MOVE      ELEDC2        EHIDC2
     C                   MOVE      ELEDC3        EHIDC3
     C*** Begin Add *** 04/07/17 *******************************
     C                   MOVE      ELDEV2BP      EHDEV2IBP
     C                   MOVE      ELDEV2AP      EHDEV2IAP
     C*** End   Add *** 04/07/17 *******************************
     C                   MOVE      ELEHCN        EHIHCN
     C                   MOVE      ELEXP1        EHIXP1
     C                   MOVE      ELEXP2        EHIXP2
     C                   MOVE      ELEXP3        EHIXP3
     C                   MOVE      ELEIN1        EHIIN1
     C                   MOVE      ELEIN2        EHIIN2
     C                   MOVE      ELEDCT        EHIDCT
     C                   MOVE      ELEDD2        EHIDD2
     C                   MOVE      ELEDCM        EHIDCM
     C                   MOVE      ELEDCS        EHIDCS
     C                   MOVE      ELEDCR        EHIDCR
     C                   MOVE      ELEFRS        EHIFRS
     C                   MOVE      ELEDV2        EHIDV2
     C                   MOVE      ELD1E2        EHD1I2
     C                   MOVE      ELD2E2        EHD2I2
     C                   MOVE      ELEDC         EHIDC
     C                   Z-ADD     ELETER        EHITER
     C                   Z-ADD     ELEDTR        EHIDTR
     C*** BEG ADD ***  07/15/13  ************************************
     C                   Z-ADD     ELTERR        EHITRR
     C                   Z-ADD     ELDTRR        EHIDRR
     C*** END ADD ***  07/15/13  ************************************
     C                   Z-ADD     ELEARC        EHIARC
     C                   Z-ADD     ELHXMN        EHIHXM
     C                   Z-ADD     ELVLCA        EHIVLC
     C                   Z-ADD     ELRPE1        EHIRP1
     C                   Z-ADD     ELRPE2        EHIRP2
     C                   Z-ADD     ELRPE3        EHIRP3
     C                   Z-ADD     ELEWAV        EHIWAV
      *** Beg Add ***** 04/19/16 ***********************************
     C                   Z-ADD     EL1AAM        EHI1AS
     C                   Z-ADD     EL2AAM        EHI2AS
     C                   Z-ADD     EL3AAM        EHI3AS
     C                   Z-ADD     EL4AAM        EHI4AS
     C                   Z-ADD     EL5AAM        EHI5AS
     C                   Z-ADD     EL6AAM        EHI6AS
     C                   Z-ADD     EL7AAM        EHI7AS
     C                   Z-ADD     ELSFAM        EHISF
     C                   Z-ADD     ELMFAM        EHIMF
      *** End Add ***** 04/19/16 ***********************************
      *** Beg Add ***** 02/01/18 ***********************************
     C                   Z-add     ELepf         EHipf
      *** End Add ***** 02/01/18 ***********************************
      *** Beg Add ***** 04/27/16 ***********************************
      /Free
        EHexMd1Nf =  ELexMd1Nf;
        EHexMd2Nf =  ELexMd2Nf;
        EHexMd3Nf =  ELexMd3Nf;
      /End-free
      *** End Add ***** 04/27/16 ***********************************
      *** Beg Add ***** 09/14/16 ***********************************
      /Free
       EHianc = ELanca;
      /End-free
      *** End Add ***** 09/14/16 ***********************************
      *** Beg Add ***** 02/12/19 ***********************************
      /Free
        EHnoexm1 =  ELnoexm1;
        //*** Begin Add *** 07/30/23 ***************************************
        EHnoexm2 =  ELnoexm2;
        EHnoexm3 =  ELnoexm3;
        //*** End   Add *** 07/30/23 ***************************************
        //*** Beg Add *** 02/27/24 ***************************************
        EHIBAL = MPEBAL;
        EHIAMAN = MPMAPR;
        EHIASBJ = MPESBJ;
        EHIAMOD = MPEMPM;
        EHIASTD = MPSTPR;
        EHIANORM = MPENMP;
        EHIAANN = MPANPR;
        //*** Beg Chg ***** 08/14/24 *************************************
        //***EHIAIL1 = ELEIN1;
        //***EHIAIL2 = ELEIN2;
        EHIAIL1 = MPEIN1;
        EHIAIL2 = MPEIN2;
        EHIBIL  = MPEBIL;
        EHIBIL2 = MPEBIL2;
        //*** End Chg ***** 08/14/24 *************************************
        EHIADC1 = MPEDC1;
        EHIADC2 = MPEDC2;
        EHIADC3 = MPEDC3;
        EHIADV2 = MPEDV2;
        EHIADCT = MPEDCT;
        EHIADD2 = MPEDD2;
        EHIADCM = MPEDCM;
        EHIADCS = MPEDCS;
        EHIADC = MPEDC;
        EHIAWAV = MPEWAV;
        EHIATCA = MPETCA;
        EHIAPRD = MPPRTD;
        EHIAXPA = MPEXPA;
        EHIAVLC = MPEVLC;
        EHIAARC = MPEARC;
        EHIAD12 = MPD1E2;
        EHIAD22 = MPD2E2;
        EHIAHCN = ELEHCN;
        EHIAUSA = ELUPSA;
        EHIAXP1 = MPEXP1;
        EHIAXP2 = MPEXP2;
        EHIAXP3 = MPEXP3;
        EHIARP1 = MPRPE1;
        EHIARP2 = MPRPE2;
        EHIARP3 = MPRPE3;
        EHIALOS = MPALOS;
        //*** End Add *** 02/27/24 ***************************************
      /End-free
      *** End Add ***** 02/12/19 ***********************************
     C                   Z-ADD     0             XXADLD           11 2
     C     EPKY1         CHAIN     PEP02                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
     C                   ADD       EPADLD        XXADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C     EPKY1         READE     PEP02                                  60
     C                   ENDDO
     C                   Z-ADD     XXADLD        EHIADL
     C                   ENDIF                                                  SVCX IFEQ
     C*
     C*  IF A PRIOR HISTORY RECORD EXISTS, UPDATE THE PREMIUM
     C*  CHANGED FIELDS.
     C*
     C     ELKEY         SETGT     PEQ01
     C     ELKEY         READPE    PEQ01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C     EHWMPR        SUB       EQWMPR        EHXMPR
     C     EHWSPR        SUB       EQWSPR        EHXSPR
     C     EHWSRA        SUB       EQWSRA        EHXSRA
     C     EHWNMP        SUB       EQWNMP        EHXNMP
     C     EHWAPR        SUB       EQWAPR        EHXAPR
     C     EHWSBJ        SUB       EQWSBJ        EHXSBJ
     C     EHWMPM        SUB       EQWMPM        EHXMPM
     C     EHWDC         SUB       EQWDC         EHXDC
     C     EHWDC1        SUB       EQWDC1        EHXDC1
     C     EHWDC2        SUB       EQWDC2        EHXDC2
     C     EHD1W2        SUB       EQD1W2        EHD1C2
     C     EHWDC3        SUB       EQWDC3        EHXDC3
     C     EHWHCN        SUB       EQWHCN        EHXHCN
     C     EHWUSA        SUB       EQWUSA        EHXUSA
     C     EHWXP1        SUB       EQWXP1        EHXXP1
     C     EHWXP2        SUB       EQWXP2        EHXXP2
     C     EHWXP3        SUB       EQWXP3        EHXXP3
     C     EHWIN1        SUB       EQWIN1        EHXIN1
     C     EHWIN2        SUB       EQWIN2        EHXIN2
     C     EHWDCT        SUB       EQWDCT        EHXDCT
     C     EHWDD2        SUB       EQWDD2        EHXDD2
     C     EHWDCM        SUB       EQWDCM        EHXDCM
     C     EHWDCS        SUB       EQWDCS        EHXDCS
     C     EHWDCR        SUB       EQWDCR        EHXDCR
     C     EHWFRS        SUB       EQWFRS        EHXFRS
     C     EHWDV2        SUB       EQWDV2        EHXDV2
     C     EHD2W2        SUB       EQD2W2        EHD2C2
     C     EHWWAV        SUB       EQWWAV        EHXWAV
     C     EHWADL        SUB       EQWADL        EHXADL
     C     EHWTER        SUB       EQWTER        EHXTER
     C     EHWDTR        SUB       EQWDTR        EHXDTR
     C     EHWRP1        SUB       EQWRP1        EHXRP1
     C     EHWRP2        SUB       EQWRP2        EHXRP2
     C     EHWRP3        SUB       EQWRP3        EHXRP3
     C     EHWVLC        SUB       EQWVLC        EHXVLC
     C     EHWARC        SUB       EQWARC        EHXARC
     C     EHWHXM        SUB       EQWHXM        EHXHXM
      *** Beg Add ***** 04/19/16 ***********************************
     C     EHW1AS        SUB       EQW1AS        EHX1AS
     C     EHW2AS        SUB       EQW2AS        EHX2AS
     C     EHW3AS        SUB       EQW3AS        EHX3AS
     C     EHW4AS        SUB       EQW4AS        EHX4AS
     C     EHW5AS        SUB       EQW5AS        EHX5AS
     C     EHW6AS        SUB       EQW6AS        EHX6AS
     C     EHW7AS        SUB       EQW7AS        EHX7AS
     C     EHWSF         SUB       EQWSF         EHXSF
     C     EHWMF         SUB       EQWMF         EHXMF
      *** End Add ***** 04/19/16 ***********************************
      *** Beg Add ***** 02/01/18 ***********************************
     C     EHwpf         Sub       EQwpf         EHxpf
     C     EHipf         Sub       EQipf         EHjpf
      *** End Add ***** 02/01/18 ***********************************
      *** Beg Add ***** 09/14/16 ***********************************
      /Free
       EHxanc = EHwanc - EQwanc;
      /End-free
      *** End Add ***** 09/14/16 ***********************************
     C*
     C*  INFORCED PREMIUM FIELDS
     C*
     C     EHIMPR        SUB       EQIMPR        EHJMPR
     C     EHISPR        SUB       EQISPR        EHJSPR
     C     EHISBJ        SUB       EQISBJ        EHJSBJ
     C     EHIMPM        SUB       EQIMPM        EHJMPM
     C     EHINMP        SUB       EQINMP        EHJNMP
     C     EHIAPR        SUB       EQIAPR        EHJAPR
     C     EHIDC         SUB       EQIDC         EHJDC
     C     EHIDC1        SUB       EQIDC1        EHJDC1
     C     EHIDC2        SUB       EQIDC2        EHJDC2
     C     EHD1I2        SUB       EQD1I2        EHD1H2
     C     EHIDC3        SUB       EQIDC3        EHJDC3
     C     EHIHCN        SUB       EQIHCN        EHJHCN
     C     EHIXP1        SUB       EQIXP1        EHJXP1
     C     EHIXP2        SUB       EQIXP2        EHJXP2
     C     EHIXP3        SUB       EQIXP3        EHJXP3
     C     EHIIN1        SUB       EQIIN1        EHJIN1
     C     EHIIN2        SUB       EQIIN2        EHJIN2
     C     EHIDCT        SUB       EQIDCT        EHJDCT
     C     EHIDD2        SUB       EQIDD2        EHJDD2
     C     EHIDCM        SUB       EQIDCM        EHJDCM
     C     EHIDCS        SUB       EQIDCS        EHJDCS
     C     EHIDCR        SUB       EQIDCR        EHJDCR
     C     EHIFRS        SUB       EQIFRS        EHJFRS
     C     EHIDV2        SUB       EQIDV2        EHJDV2
     C     EHD2I2        SUB       EQD2I2        EHD2H2
     C     EHIWAV        SUB       EQIWAV        EHJWAV
     C     EHIADL        SUB       EQIADL        EHJADL
     C     EHITER        SUB       EQITER        EHJTER
     C     EHIDTR        SUB       EQIDTR        EHJDTR
     C     EHIVLC        SUB       EQIVLC        EHJVLC
     C     EHIRP1        SUB       EQIRP1        EHJRP1
     C     EHIRP2        SUB       EQIRP2        EHJRP2
     C     EHIRP3        SUB       EQIRP3        EHJRP3
     C     EHIARC        SUB       EQIARC        EHJARC
     C     EHIHXM        SUB       EQIHXM        EHJHXM
      *** Beg Add ***** 04/19/16 ***********************************
     C     EHI1AS        SUB       EQI1AS        EHJ1AS
     C     EHI2AS        SUB       EQI2AS        EHJ2AS
     C     EHI3AS        SUB       EQI3AS        EHJ3AS
     C     EHI4AS        SUB       EQI4AS        EHJ4AS
     C     EHI5AS        SUB       EQI5AS        EHJ5AS
     C     EHI6AS        SUB       EQI6AS        EHJ6AS
     C     EHI7AS        SUB       EQI7AS        EHJ7AS
     C     EHISF         SUB       EQISF         EHJSF
     C     EHIMF         SUB       EQIMF         EHJMF
      *** End Add ***** 04/19/16 ***********************************
      *** Beg Add ***** 09/14/16 ***********************************
      /Free
       EHjanc = EHianc - EQianc;
      /End-free
      *** End Add ***** 09/14/16 ***********************************
     C                   ELSE
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C                   Z-ADD     EHWMPR        EHXMPR
     C                   Z-ADD     EHWSPR        EHXSPR
     C                   Z-ADD     EHWSRA        EHXSRA
     C                   Z-ADD     EHWNMP        EHXNMP
     C                   Z-ADD     EHWAPR        EHXAPR
     C                   Z-ADD     EHWSBJ        EHXSBJ
     C                   Z-ADD     EHWMPM        EHXMPM
     C                   Z-ADD     EHWDC1        EHXDC1
     C                   Z-ADD     EHWDC2        EHXDC2
     C                   Z-ADD     EHD1W2        EHD1C2
     C                   Z-ADD     EHWDC3        EHXDC3
     C                   Z-ADD     EQWHCN        EHXHCN
     C                   Z-ADD     EQWUSA        EHXUSA
     C                   Z-ADD     EHWXP1        EHXXP1
     C                   Z-ADD     EHWXP2        EHXXP2
     C                   Z-ADD     EHWXP3        EHXXP3
     C                   Z-ADD     EHWIN1        EHXIN1
     C                   Z-ADD     EHWIN2        EHXIN2
     C                   Z-ADD     EHWDCT        EHXDCT
     C                   Z-ADD     EHWDD2        EHXDD2
     C                   Z-ADD     EHWDCM        EHXDCM
     C                   Z-ADD     EHWDCS        EHXDCS
     C                   Z-ADD     EHWDCR        EHXDCR
     C                   Z-ADD     EHWFRS        EHXFRS
     C                   Z-ADD     EHWDV2        EHXDV2
     C                   Z-ADD     EHD2W2        EHD2C2
     C                   Z-ADD     EHWWAV        EHXWAV
     C                   Z-ADD     EHWADL        EHXADL
     C                   Z-ADD     EHWTER        EHXTER
     C                   Z-ADD     EHWDTR        EHXDTR
     C                   Z-ADD     EHWRP1        EHXRP1
     C                   Z-ADD     EHWRP2        EHXRP2
     C                   Z-ADD     EHWRP3        EHXRP3
     C                   Z-ADD     EHWVLC        EHXVLC
     C                   Z-ADD     EHWDC         EHXDC
     C                   Z-ADD     EHWARC        EHXARC
     C                   Z-ADD     EHWHXM        EHXHXM
      *** Beg Add ***** 04/19/16 ***********************************
     C                   Z-ADD     EHW1AS        EHX1AS
     C                   Z-ADD     EHW2AS        EHX2AS
     C                   Z-ADD     EHW3AS        EHX3AS
     C                   Z-ADD     EHW4AS        EHX4AS
     C                   Z-ADD     EHW5AS        EHX5AS
     C                   Z-ADD     EHW6AS        EHX6AS
     C                   Z-ADD     EHW7AS        EHX7AS
     C                   Z-ADD     EHWSF         EHXSF
     C                   Z-ADD     EHWMF         EHXMF
      *** End Add ***** 04/19/16 ***********************************
      *** Beg Add ***** 02/01/18 ***********************************
     c                   Z-add     EHwpf         EHxpf
     c                   Z-add     EHipf         EHjpf
      *** End Add ***** 02/01/18 ***********************************
      *** Beg Add ***** 09/14/16 ***********************************
      /Free
       EHxanc = EHwanc;
      /End-free
      *** End Add ***** 09/14/16 ***********************************
     C*
     C*  INFORCED PREMIUM FIELDS
     C*
     C                   Z-ADD     EHIMPR        EHJMPR
     C                   Z-ADD     EHISPR        EHJSPR
     C                   Z-ADD     EHINMP        EHJNMP
     C                   Z-ADD     EHIAPR        EHJAPR
     C                   Z-ADD     EHISBJ        EHJSBJ
     C                   Z-ADD     EHIMPM        EHJMPM
     C                   Z-ADD     EHIDC1        EHJDC1
     C                   Z-ADD     EHIDC         EHJDC
     C                   Z-ADD     EHIDC2        EHJDC2
     C                   Z-ADD     EHD1I2        EHD1H2
     C                   Z-ADD     EHIDC3        EHJDC3
     C                   Z-ADD     EHIHCN        EHJHCN
     C                   Z-ADD     EHIXP1        EHJXP1
     C                   Z-ADD     EHIXP2        EHJXP2
     C                   Z-ADD     EHIXP3        EHJXP3
     C                   Z-ADD     EHIIN1        EHJIN1
     C                   Z-ADD     EHIIN2        EHJIN2
     C                   Z-ADD     EHIDCT        EHJDCT
     C                   Z-ADD     EHIDD2        EHJDD2
     C                   Z-ADD     EHIDCM        EHJDCM
     C                   Z-ADD     EHIDCS        EHJDCS
     C                   Z-ADD     EHIDCR        EHJDCR
     C                   Z-ADD     EHIFRS        EHJFRS
     C                   Z-ADD     EHIDV2        EHJDV2
     C                   Z-ADD     EHD2I2        EHD2H2
     C                   Z-ADD     EHIWAV        EHJWAV
     C                   Z-ADD     EHIADL        EHJADL
     C                   Z-ADD     EHITER        EHJTER
     C                   Z-ADD     EHIDTR        EHJDTR
     C                   Z-ADD     EHIVLC        EHJVLC
     C                   Z-ADD     EHIRP1        EHJRP1
     C                   Z-ADD     EHIRP2        EHJRP2
     C                   Z-ADD     EHIRP3        EHJRP3
     C                   Z-ADD     EHIARC        EHJARC
     C                   Z-ADD     EHIHXM        EHJHXM
      *** Beg Add ***** 04/19/16 ***********************************
     C                   Z-ADD     EHI1AS        EHJ1AS
     C                   Z-ADD     EHI2AS        EHJ2AS
     C                   Z-ADD     EHI3AS        EHJ3AS
     C                   Z-ADD     EHI4AS        EHJ4AS
     C                   Z-ADD     EHI5AS        EHJ5AS
     C                   Z-ADD     EHI6AS        EHJ6AS
     C                   Z-ADD     EHI7AS        EHJ7AS
     C                   Z-ADD     EHISF         EHJSF
     C                   Z-ADD     EHIMF         EHJMF
      *** End Add ***** 04/19/16 ***********************************
      *** Beg Add ***** 09/14/16 ***********************************
      /Free
       EHjanc = EHianc;
      /End-free
      *** End Add ***** 09/14/16 ***********************************
     C                   ENDIF
     C*
     C*  LOAD THE TRANSACTION DATE AND UPDATE THE HISTORY RECORD
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHTRDT
     C*
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C*
     C                   UPDATE    PEH01
     C*
     C*  LOAD THE INFORCED PREMIUM INTO EMPLOYER DETAIL
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C                   Z-ADD     EHIAPR        ELINMP
     C  N60              UPDATE    PEL01
     C                   MOVE      EHFAF         SVFAF
     C     EHKY3         READE     PEH01                                  50
     C                   ENDDO
     C*
     C*  GET THE NEXT RECORD IF PROCESSING ALL RECORDS
     C*
     C     *IN20         IFEQ      *OFF
     C     EHKY3         SETGT     PEH01
     C                   READ      PEH01                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C*
     C*  CLOSE THE OVERRIDE FILES SO THEY CAN BE CLEARED
     C*
     C                   CLOSE     WAELL
     C                   CLOSE     WAELL1
     C                   CLOSE     WAEPL
     C                   CLOSE     WADSL
     C                   CLOSE     WAILL
     C                   CLOSE     WAMPL
     C*** begin add ***  05/07/18  **********************************
     C                   CLOSE     WALDL1
     C*** end add *****  05/07/18  **********************************
     C*
     C*  CLEAR OVERRIDE FILE WAELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*
     C*  CLEAR OVERRIDE FILE WAEPP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH           15 5
     C*
     C*  CLEAR OVERRIDE FILE WADSP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(11)
     C                   PARM                    LENGTH           15 5
     C*
     C*  CLEAR OVERRIDE FILE WAILP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(14)
     C                   PARM                    LENGTH           15 5
     C*** begin add ***  05/07/18  **********************************
     C*
     C*  Clear override file WALDL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(26)
     C                   PARM                    LENGTH           15 5
     C*** end add *****  05/07/18  **********************************
     C*
     C*  CLEAR OVERRIDE FILE WAMPP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(17)
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C*
     C     EHFND         IFNE      XXFND
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C                   ENDDO
     C*
     C*  DELETE OVERRIDE FOR WDELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(19)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WDELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WDELL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(21)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WMEPL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WMEPL1E
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(23)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WMEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C*
     C*  DELETE OVERRIDE FOR WDDSL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDE FOR WDILL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(13)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C*** begin add ***  05/07/18  **********************************
     C*
     C*  Delete override for WMLDL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(25)
     C                   PARM                    LENGTH           15 5
     C*** end add *****  05/07/18  **********************************
     C*
     C*  DELETE OVERRIDE FOR WDMPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(16)
     C                   PARM                    LENGTH           15 5
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*************************************************************
     C*   SUBROUTINE LDOVRF
     C*   LOAD DATA TO OVERRIDE FILES
     C*************************************************************
     C*
     C     LDOVRF        BEGSR
     C*
     C*  CALCULATE THE ANNIVERSARY RATING PERIOD START DATE.
     C*  IF IT FALLS OUTSIDE THE POLICY PERIOD, SET IT TO 999999.
     C*
     C                   MOVE      *BLANKS       XXST              2
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   ENDIF
     C                   Z-ADD     ELANDT        XXANDT
     C                   ADD       10000         XXANDT
     C*
     C     XXANDT        IFLT      ELPFDT
     C     XXANDT        ORGT      ELPTDT
     C                   Z-ADD     9999999       XXANDT
     C                   ENDIF
     C*
     C*  LOAD THE PAYROLL OVERRIDE RECORDS
     C*
     C     EPKY1         CHAIN     PEP02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     EPKY1         CHAIN     PEP01                              61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C*  FOR AUDITED INFORCED PREMIUM, LOAD THE AUDIT PAYROLL INTO
     C*  THE ESTIMATED PAYROLL FIELD.
     C*
     C     EHFAF         IFEQ      'A'
     C                   Z-ADD     EPAAPR        EPEAPR
     C                   ENDIF
      *
     C* Check rate opt out flag
     C                   SELECT
     C     XXST          WHENEQ    'OK'
      *** beg add ***** 07/16/14 ******************************
     C     ELPTDT        ANDLT     1110901
      *** end add ***** 07/16/14 ******************************
     C* Check opt out flag for OK 1100901 rate change
     C                   Z-ADD     1100901       XXSPDT            7 0
     C                   EXSR      SRAPRT
      *** beg add ***** 06/16/14 ******************************
     C     XXST          WHENEQ    'TN'
     C* Check opt out flag for TN 1140701 rate change
     C                   Z-ADD     1140701       XXSPDT            7 0
     C                   EXSR      SRAPRT
      *** end add ***** 06/16/14 ******************************
      *** beg add ***** 07/16/14 ******************************
     C     XXST          WHENEQ    'OK'
     C     ELPTDT        ANDLT     1150101
     C* Check opt out flag for OK 1140201 rate change
     C                   Z-ADD     1140201       XXSPDT            7 0
     C                   EXSR      SRAPRT
      *** end add ***** 07/16/14 ******************************
     C                   OTHER
     C                   MOVE      'N'           XXOPTF
     C                   ENDSL
     C*
     C*  IF THE PAYROLL RECORD EFFECTIVE DATE IS NOT THE SAME AS
     C*  THE POLICY START DATE OR THE ANNIVERSARY RATING DATE,
     C*  THE PAYROLL AMOUNT MUST BE ANNUALIZED TO COVER THE POLICY
     C*  PERIOD.  IF THE POLICY IS ANNIVERSARY RATED, WE MUST ALSO
     C*  WRITE A RECORD FOR THE FIRST PERIOD IF THE ORIGINAL RECORD
     C*  BECAME EFFECTIVE AFTER THE ANNIVERSARY RATING DATE.
     C*
     C     EPEFDT        IFNE      ELPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     EPEFDT        ANDNE     1030401
     C     EPEFDT        ANDNE     1031001
     C     EPEFDT        ANDNE     1090701
     C     XXST          ANDEQ     'FL'
     C     CLTID         ANDEQ     '139'
     C     EPEFDT        ORNE      ELPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     EPEFDT        ANDNE     1030401
     C     EPEFDT        ANDNE     1031001
     C     XXST          ANDEQ     'FL'
     C     CLTID         ANDNE     '139'
     C     EPEFDT        ORNE      ELPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     EPEFDT        ANDNE     1081001
     C     XXST          ANDEQ     'DE'
     C     EPEFDT        ORGT      ELPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     EPEFDT        ANDNE     1100101
     C     XXST          ANDEQ     'AZ'
     C     EPEFDT        ORNE      ELPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     EPEFDT        ANDNE     1100901
      *** beg add ***** 07/16/14 ******************************
     C     EPEFDT        ANDNE     1140201
      *** end add ***** 07/16/14 ******************************
     C     XXST          ANDEQ     'OK'
     C     XXOPTF        ANDEQ     'N'
      *** beg add ***** 06/16/14 ******************************
     C     EPEFDT        ORNE      ELPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     EPEFDT        ANDNE     1140701
     C     XXST          ANDEQ     'TN'
     C     XXOPTF        ANDEQ     'N'
      *** end add ***** 06/16/14 ******************************
     C     EPEFDT        ORNE      ELPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXST          ANDNE     'FL'
     C     XXST          ANDNE     'DE'
     C     XXST          ANDNE     'AZ'
     C     XXST          ANDNE     'OK'
      *** beg add ***** 06/16/14 ******************************
     C     XXST          ANDNE     'TN'
      *** end add ***** 06/16/14 ******************************
     C*
     C*  DETERMINE THE PAYROLL DAYS
     C*
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM            8 0
     C*
     C     EPEFDT        IFLT      XXANDT
     C     XXANDT        ANDNE     9999999
     C*
     C                   Z-ADD     XXANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO              8 0
     C                   ELSE
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO
     C                   ENDIF
     C                   MOVE      X1FROM        QQFROM            8 0
     C                   MOVE      X1TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     X1TO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C*
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
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1FROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        PRDAYS            5 0
     C*
     C*  IF THE POLICY IS ANNIVERSARY RATED, CALCULATE THE DAYS
     C*  IN EACH RATING PERIOD TO SPLIT THE ANNUALIZED PAYROLL
     C*
     C     XXANDT        IFNE      9999999
     C*
     C*  DETERMINE THE RATING DAYS
     C*
     C                   Z-ADD     0             XXLPDT
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     XXANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   MOVE      X1FROM        QQFROM
     C                   MOVE      X1TO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     ELPFDT        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      ELPFDT
     C                   Z-ADD     XXANDT        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      ELPFDT
     C     XXLPDT        ORGT      XXANDT
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1FROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        RDAYS1            5 0
     C     365           SUB       RDAYS1        RDAYS2            5 0
     C*
     C*  IF THE PAYROLL EFFECTIVE DATE FALLS AFTER THE ANNIVERSARY
     C*  RATING DATE, WRITE THE SECOND PERIOD RECORD.
     C*
     C     EPEFDT        IFGT      XXANDT
     C*
     C     PRDAYS        IFNE      *ZERO
     C     RDAYS2        DIV       PRDAYS        XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C                   Z-ADD     EPEAPR        XXEAPR
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C                   Z-ADD     XXANDT        EPEFDT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EPTIME
     C*++ BEGIN ADD +++  04/18/01  ++++++++++++++++++++TBC 10/03/06++++
     C*
     C*  IF POLICY IS SHORT-TERM AND PRORATA FACTOR IS 1.00, ANNUALIZE
     C*  THE PAYROLL
     C*
     C                   Z-ADD     X2PFDT        XXTODT
     C                   ADD       1             XXTOYR
     C*++ BEGIN ADD +++  03/05/03  ++++++++++++++++++++TBC 10/03/06++++
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       PRPCF
     C                   ENDIF
     C*++ END ADD +++++  03/05/03  +++++++++++++++++++++TBC 10/03/06+++
      *
     C     X2PTDT        IFNE      XXTODT
     C     X2PROR        ANDEQ     1.00
     C     X2FFL9        ANDEQ     'Y'
     C*++ BEGIN ADD +++  03/05/03  +++++++++++++++++++++TBC 10/03/06+++
     C     PRPCF         ANDNE     'O'
     C*++ END ADD +++++  03/05/03  +++++++++++++++TBC 10/03/06+++++++++
     C*
     C     PDAYS         IFNE      *ZERO
     C     365           DIV       PDAYS         XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C*
     C                   ENDIF
     C*
     C*++ END ADD +++++  04/18/01  +++++++++++++++TBC 10/03/06+++++++++
     C                   WRITE     PEP02
     C                   Z-ADD     XXEAPR        EPEAPR
     C                   ENDIF
     C*
     C*  ADJUST THE PAYROLL AMOUNT FOR THE FIRST PERIOD
     C*
     C     PRDAYS        IFNE      *ZERO
     C     RDAYS1        DIV       PRDAYS        XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C                   Z-ADD     ELPFDT        EPEFDT
     C*
     C                   ELSE
     C*
     C*  IF THE POLICY IS NOT ANNIVERSARY RATED, ANNUALIZE THE PAYROLL
     C*
     C     PRDAYS        IFNE      *ZERO
     C     365           DIV       PRDAYS        XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C                   Z-ADD     ELPFDT        EPEFDT
     C*
     C                   ENDIF                                                  XXANDT <> 999999
     C*
     C                   ELSE
     C*
     C     EHFAF         IFEQ      'A'
     C     ELPROR        ANDNE     *ZERO
     C     EPEAPR        DIV(H)    ELPROR        XXWK90
     C                   Z-ADD     XXWK90        EPEAPR
     C                   ENDIF                                                  EHFAF IFEQ
     C*
     C                   ENDIF                                                  EPEFDT <> ELPFDT
     C*                                               AND  EPEFDT <> XXANDT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EPTIME
     C*++ BEGIN ADD +++  04/18/01  +++++++++++++++TBC 10/03/06+++++++++
     C*
     C*  IF POLICY IS SHORT-TERM AND PRORATA FACTOR IS 1.00, ANNUALIZE
     C*  THE PAYROLL
     C*
     C                   Z-ADD     X2PFDT        XXTODT
     C                   ADD       1             XXTOYR
     C*++ BEGIN ADD +++  03/05/03  +++++++++++TBC 10/03/06+++++++++++++
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       PRPCF
     C                   ENDIF
     C*++ END ADD +++++  03/05/03  +++++++++++++TBC 10/03/06+++++++++++
     C*
     C     X2PTDT        IFNE      XXTODT
     C     X2PROR        ANDEQ     1.00
     C*++ BEGIN ADD +++  03/05/03  +++++++++++++++TBC 10/03/06+++++++++
     C     PRPCF         ANDNE     'O'
     C*++ END ADD +++++  03/05/03  ++++++++++++++++TBC 10/03/06++++++++
     C*
     C     PDAYS         IFNE      *ZERO
     C     365           DIV       PDAYS         XXFACT            9 6
     C                   ELSE
     C                   Z-ADD     1             XXFACT
     C                   ENDIF
     C*
     C     EPEAPR        MULT(H)   XXFACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C*
     C                   ENDIF
     C*++ END ADD +++++  04/18/01  +++++++++++++TBC 10/03/06+++++++++++
     C                   WRITE     PEP02
     C     EPKY1         READE     PEP01                                  61
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  WRITE THE DETAIL RECORD IF IT IS NOT ALREADY WRITTEN
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     ELKEY         CHAIN(N)  PEL01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C*  IF WE ARE LOADING THE CHANGED DIVISION, LOAD THE PREMIUM
     C*  FACTORS AND POLICY DATES FROM THE HISTORY RECORD.
     C*
     C*
     C*  ANNUALIZE THE OVERRIDE DETAIL RECORD.  MAKE THE MOST RECENT
     C*  EXPERIENCE MODIFIER THE ONLY EXPERIENCE MODIFIER.  IF THERE
     C*  IS A SECOND INCREASED LIMIT, USE FOR THE ONLY INCREASED LIMIT.
     C*  USE THE POLICY START DATE FOR ALL PREMIUM FACTOR EFFECTIVE
     C*  DATES.  SET ALL PREMIUM EXPIRATION DATES TO ZERO.
     C*
     C     ELEXM3        IFNE      *ZERO
     C                   Z-ADD     ELEXM3        ELEXM1
     C*** begin add ***  12/15/15  ****************************************
     C                   Z-ADD     ELARP3        ELARP1
     C*** end add *****  12/15/15  ****************************************
     C                   ELSE
     C*
     C     ELEXM2        IFNE      *ZERO
     C                   Z-ADD     ELEXM2        ELEXM1
     C*** begin add ***  12/15/15  ****************************************
     C                   Z-ADD     ELARP2        ELARP1
     C*** end add *****  12/15/15  ****************************************
     C                   ENDIF                                                  ELEXM2 <> 0
     C*
     C                   ENDIF                                                  ELEXM3 <> 0
     C     ELINL2        IFNE      *BLANK
     C                   MOVE      ELINL2        ELILCD
     C                   MOVE      *BLANKS       ELINL2
     C                   Z-ADD     0             ELI2EF
     C                   Z-ADD     0             ELI2EX
     C                   ENDIF
     C*
     C     ELDCD2        IFNE      *BLANK
     C                   MOVE      ELDCD2        ELDCDE
     C                   CLEAR                   ELDCD2
     C                   CLEAR                   EL2DEF
     C                   CLEAR                   EL2DEX
     C                   ENDIF
     C     ELD1S2        IFNE      *ZERO
     C                   Z-ADD     ELD1S2        ELASP2
     C                   ENDIF
     C     ELD2S2        IFNE      *ZERO
     C                   Z-ADD     ELD2S2        ELDEV2
     C                   ENDIF
     C*
     C     ELPFDT        ADD       10000         ELPTDT
     C                   MOVE      ELPTDT        XXPTDT            4 0
     C     XXPTDT        IFEQ      0229
     C                   SUB       1             XXPTDT
     C                   MOVE      XXPTDT        ELPTDT
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         ELEXM2
     C                   Z-ADD     *ZERO         ELEXM3
     C*** begin add ***  12/15/15  ****************************************
     C                   Z-ADD     *ZERO         ELARP2
     C                   Z-ADD     *ZERO         ELARP3
     C*** end add *****  12/15/15  ****************************************
     C                   Z-ADD     *ZERO         ELD1S2
     C                   Z-ADD     *ZERO         ELD2S2
     C*
     C                   Z-ADD     ELPFDT        ELCREF
     C                   Z-ADD     *ZERO         ELCREX
     C                   Z-ADD     ELPFDT        ELDVEF
     C                   Z-ADD     *ZERO         ELDVEX
     C                   Z-ADD     *ZERO         ELD1D2
     C                   Z-ADD     *ZERO         ELD1X2
     C                   Z-ADD     *ZERO         ELD2D2
     C                   Z-ADD     *ZERO         ELD2X2
     C                   Z-ADD     ELPFDT        ELEMD1
     C                   Z-ADD     *ZERO         ELEMD2
     C                   Z-ADD     *ZERO         ELEMD3
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     *ZERO         ELILEX
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     *ZERO         ELDCEX
     C                   Z-ADD     ELPFDT        ELMCEF
     C                   Z-ADD     *ZERO         ELMCEX
     C                   Z-ADD     ELPFDT        ELSCEF
     C                   Z-ADD     *ZERO         ELSCEX
     C                   Z-ADD     ELPFDT        ELSDEF
     C                   Z-ADD     *ZERO         ELSDEX
     C*
     C                   WRITE     PEL02
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  IF WE ARE SPREADING THE DISCOUNT, LOAD THE DISCOUNT RECORD
     C*
     C     Q1SPRD        IFEQ      'Y'
     C*
     C     DSKY          CHAIN     PDS02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     DSKY          CHAIN     PDS01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     XXCO#         DSCO#
     C                   Z-ADD     XXFND         DSFND
     C                   Z-ADD     XXFYR         DSFYR
     C                   Z-ADD     XXEMP#        DSEMP#
     C                   Z-ADD     XXDIV         DSDIV
     C                   Z-ADD     ELSTPR        DSSTPR
     C                   Z-ADD     0             DSEDIS
     C                   Z-ADD     0             DSTEPR
     C                   Z-ADD     0             DSTEDS
     C                   Z-ADD     0             DSEPCT
     C                   Z-ADD     0             DSANSP
     C                   Z-ADD     0             DSACSP
     C                   Z-ADD     0             DSADIS
     C                   Z-ADD     0             DSTANP
     C                   Z-ADD     0             DSTACP
     C                   Z-ADD     0             DSAPCT
     C                   MOVE      *BLANKS       DSSTAT
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C                   WRITE     PDS02
     C                   ENDIF
     C*
     C                   ENDIF                                                  Q1SPRD = 'Y'
     C     DSKY          CHAIN     PIL02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     DSKY          CHAIN     PIL01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     XXCO#         ILCO#
     C                   Z-ADD     XXFND         ILFND
     C                   Z-ADD     XXFYR         ILFYR
     C                   Z-ADD     XXEMP#        ILEMP#
     C                   Z-ADD     XXDIV         ILDIV
     C                   Z-ADD     ELEIN1        ILEIN1
     C                   Z-ADD     0             ILAIN1
     C                   Z-ADD     ELEIN2        ILEIN2
     C                   Z-ADD     0             ILAIN2
     C                   MOVE      *BLANKS       ILSTAT
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ILDT
     C                   Z-ADD     DSTIME        ILTIME
     C                   WRITE     PIL02
     C                   ENDIF
     C*
     C     DSKY          CHAIN     PMP02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     DSKY          CHAIN     PMP01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     XXCO#         MPCO#
     C                   Z-ADD     XXFND         MPFND
     C                   Z-ADD     XXFYR         MPFYR
     C                   Z-ADD     XXEMP#        MPEMP#
     C                   Z-ADD     XXDIV         MPDIV
     C                   Z-ADD     ELANPR        MPANPR
     C                   Z-ADD     0             MPAAPR
     C                   MOVE      *BLANK        MPSTAT
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MPUSER
     C                   MOVEL     Q1PGM         MPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MPDT
     C                   Z-ADD     DSTIME        MPTIME
     C                   WRITE     PMP02
     C                   ENDIF
     C*
     C*** begin add ***  05/07/18  ***************************************
     C*
     C     ELKEY         CHAIN     PLD02                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C     ELKEY         CHAIN     PLD01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   Z-ADD     DSTIME        LDTIME
     C                   WRITE     PLD02
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** end add *****  05/07/18  ***************************************
     C*
     C* COPY THE HCN CREDIT TO THE OVERRIDE FILE.  ONLY ONE RECORD WILL BE NEEDE
     C* AS THE CREDIT WILL BE APPLIED TO ENTIRE POLICY PERIOD FOR INFORCE CALC.
     C*
     C     ELKEY         CHAIN     PHC03                              60
     C*
     C     *IN60         IFEQ      *ON
     C     ELKEY         CHAIN     PHC0R                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   Z-ADD     ELPFDT        HCIN
     C                   Z-ADD     0             HCOUT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        HCUSER
     C                   MOVEL     Q1PGM         HCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C                   Z-ADD     DSTIME        HCTIME
     C                   WRITE     PHC03
     C                   ENDIF                                                  *IN61 IFEQ *OFF
     C*
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C*
     C* IF DISCOUNT IS SPREAD ACROSS ENTIRE GROUP, LOAD ENTIRE GROUP
     C* INTO WADSP.
     C*
     C     FNPEOF        IFEQ      'M'
     C     FNPEOA        ANDEQ     'Y'
     C*** BEGIN ADD *** 07/17/17 **************************************
     C     FNPEOF        OREQ      'C'
     C     FNPEOA        ANDEQ     'Y'
     C*** END ADD ***** 07/17/17 **************************************
     C*
     C     ELKY4         CHAIN     PEL03                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     DSKY1         CHAIN     PDS02                              60
     C     *IN60         IFEQ      '1'
     C     DSKY1         CHAIN     PDS01                              61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     XXCO#         DSCO#
     C                   Z-ADD     XXFND         DSFND
     C                   Z-ADD     XXFYR         DSFYR
     C                   Z-ADD     ELEMP#        DSEMP#
     C                   Z-ADD     ELDIV         DSDIV
     C                   Z-ADD     ELSTPR        DSSTPR
     C                   Z-ADD     0             DSEDIS
     C                   Z-ADD     0             DSTEPR
     C                   Z-ADD     0             DSTEDS
     C                   Z-ADD     0             DSEPCT
     C                   Z-ADD     0             DSANSP
     C                   Z-ADD     0             DSACSP
     C                   Z-ADD     0             DSADIS
     C                   Z-ADD     0             DSTANP
     C                   Z-ADD     0             DSTACP
     C                   Z-ADD     0             DSAPCT
     C                   MOVE      *BLANK        DSSTAT
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C                   WRITE     PDS02
     C                   ENDIF
     C*
     C     ELKY4         READE     PEL03                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  FNPEOF IFEQ
     C*
     C                   ENDSR
      *=============================================================
      * ApplyRateSplit - Check option flag to apply rate split
      *=============================================================
     C     SRAPRT        BEGSR
     C*
     C     RTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXSPDT            7 0
     C*
     C                   MOVE      'N'           XXOPTF            1
     C     RTKY          CHAIN     PRT0X                              60
     C     *IN60         IFEQ      *OFF
     C     RTOPT         ANDEQ     'Y'
     C                   MOVE      'Y'           XXOPTF
     C                   ENDIF
     C*
     C                   ENDSR
**  QCAEXEC TABLE
OVRDBF FILE(WDELL) TOFILE(QTEMP/WAELL)
OVRDBF FILE(WMEPL1) TOFILE(QTEMP/WAEPL1)
OVRDBF FILE(WMEPL) TOFILE(QTEMP/WAEPL)
DLTOVR FILE(WDELL)
DLTOVR FILE(WMEPL1)
DLTOVR FILE(WMEPL)
OVRDBF FILE(WDDSL) TOFILE(QTEMP/WADSL)
DLTOVR FILE(WDDSL)
CLRPFM FILE(QTEMP/WAELP)
CLRPFM FILE(QTEMP/WAEPP)
CLRPFM FILE(QTEMP/WADSP)
OVRDBF FILE(WDILL) TOFILE(QTEMP/WAILL)
DLTOVR FILE(WDILL)
CLRPFM FILE(QTEMP/WAILP)
OVRDBF FILE(WDMPL) TOFILE(QTEMP/WAMPL)
DLTOVR FILE(WDMPL)
CLRPFM FILE(QTEMP/WAMPP)
OVRDBF FILE(WDELP) TOFILE(QTEMP/WAELP)                               02/09/06MLD
DLTOVR FILE(WDELP)                                                   02/09/06MLD
OVRDBF FILE(WDELL1) TOFILE(QTEMP/WAELL1)                             05/06/09
DLTOVR FILE(WDELL1)                                                  05/06/09
OVRDBF FILE(WMEPL1E) TOFILE(QTEMP/WAEPL1E)
DLTOVR FILE(WMEPL1E)
OVRDBF FILE(WMLDL1) TOFILE(QTEMP/WALDL1)
DLTOVR FILE(WMLDL1)
CLRPFM FILE(QTEMP/WALDP)
