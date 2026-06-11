     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H/TITLE WPIPOLERR - Policy Errors Service Program (Profound)
     H DFTNAME(WPIPOLERR)
      *===============================================================*
      *  Created   : 05/29/12                                         *
      *  Programmer: C:65                                             *
      *  Project   : WC2116 - Policy Transform                        *
      *===============================================================*
      *  Description:  Service program which contains the error editing
      *                for policy programs (detail, quote, etc).
      *
      *  The Error procedures included are:
      *
      *     ValidDate      (generic - verify if date is valid)
      *     PolEfDtLess    (policy  - pol eff date cannot be less than initial eff date)
      *     PolEfDtPayroll (policy  - pol eff date cannot be later than payroll rcds)
      *     AscendDates    (generic - from date earlier than to date)
      *     PolFundDates   (policy  - policy dates outside of group year dates)
      *     ValidAgent     (policy  - agent # is valid for group)
      *     ValidProducer  (policy  - Producer is valid for agent)
      *     ValidAgtRate   (policy  - Agent rate is valid for group)
      *     PolDtLessPolEf (policy  - Date must be less than or equal pol eff date)
      *     ValidFlagY     (generic - flag must be Y or blank)
      *     ValidFlagYN    (generic - flag must be Y or N)
      *     PolProdChg     (policy  - producer cannot change if commission exists)
      *     PolAgtDate     (policy  - agent assignment eff/cnl dates valid for eff date)
      *
      *  The procedures below include editing for entire screens/tabs/windows:
      *     ErrPolRating   (called for policy detail, entry - location master and rating)
      *     ErrAddInfo     (called for policy detail, entry - add info)
      *     ErrLargeDed    (called for policy detail, entry - large deduc)
      *     ErrRetroRate   (called for policy detail, entry - retro rating)
      *     ErrSchedRate   (called for policy detail, entry - scheduled rating)
      *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 03/18/25 C:65  Support: 117380 (Amwins)                       *
      *                Move modifier defaulting code to happen after  *
      *                third modifier is moved into screen field.     *
      *                                                               *
      * 08/19/24 C:062 Ticket: 117381(148)                            *
      *                Allow 3rd mod entry when 2nd mod is zero       *
      *                but has a date entered.                        *
      *                                                               *
      * 07/11/24 C:333 Support: 115909                                *
      *                Recompile only for WPILOCMST Display Change.   *
      *                                                               *
      * 06/24/24 C:333 Project: 146WKR                                *
      *                Collateral processing.                         *
      *                - Adding edits for Collateral section.         *
      *                                                               *
      * 03/04/24 C:079 Project: WC2951                                *
      *                Recompile only for WTPOLDTL Display Change.    *
      *                                                               *
      * 12/11/23 C:340 Project: PHW114578                             *
      *                Add Validation for Change on Default Waiver    *
      *                Description.                                   *
      *                                                               *
      * 10/16/23 C:340 Project: 153WAO                                *
      *                Recompile only for WTPOLDTL Display Change.    *
      *                                                               *
      * 07/30/23 C:062 Project: WC2918                                *
      *                Add not rated logic for exp mod 2 and 3ions    *
      *                                                               *
      * 04/10/23 C:65  Support: 107397 (Prescient)                    *
      *                Pass division # to SRONLI from passed in parm. *
      *                                                               *
      * 03/28/23 C:342 Support: 107744 (Sunz)                         *
      *                Recompiled over WPILOCMST screen changes.      *
      *                                                               *
      * 02/10/23 C:342 Support: 105624                                *
      *                Expiration date and Renewal flag cannot both   *
      *                be set.                                        *
      *                                                               *
      * 01/23/23 C:079 Support: 153/105979                            *
      *                Add NCCI Carrier# to Carrier Policy# dup Edit  *
      *                                                               *
      * 01/05/23 C:291 Support: 98636                                 *
      *                Remove edit that requires Percentage and       *
      *                Flat charge when Waiver type is Blanket 'B'.   *
      *                                                               *
      * 04/20/22 C:333 Support: 101863                                *
      *                Add edit to exp mod in Policy Year Detail      *
      *                - Prevent 2nd exp mod from being added with a  *
      *                  policy effective date if 1st exp modifier has*
      *                  a prior effective date.                      *
      *                - Adding new Message ID 'WCR1202' to MSGF      *
      *                  WMESGG                                       *
      *                                                               *
      * 04/06/21B C:136 Project: WC1986                               *
      *                Add Env parm to SRONLI to account for Quote    *
      *                                                               *
      *                                                               *
      * 04/06/21 C:300 Support: 94604 (SUNZ)                          *
      *                Policy rating screen allows old deductible     *
      *                descriptions to be selected.                   *
      *                Recompile over WTPOLDTL DSPF changes           *
      *                                                               *
      * 03/19/21 C:295 Support: 94885                                 *
      *                Correct the error message for Prorate or       *
      *                Short rate.                                    *
      *                                                               *
      * 02/02/21 C:136 Project: WC2757                                *
      *                Add edit to ensure waiver form group setup     *
      *                exists when flagged for waiver.                *
      *                                                               *
      * 09/23/20 C:062 Support: 91203(099)                            *
      *                -Expand SRONLI edits.                          *
      *                -Add popup when no eft or third party in Qte   *
      *                                                               *
      * 02/12/20 C:318 Support: 86478                                 *
      *                Fixed the addition of division issue by        *
      *                clearing the EMSTRP field.                     *
      *                                                               *
      * 06/12/19 C:300 Support: 79730 (CoveRisk)                      *
      *                Recompile due to WPILOCMST screen changes.     *
      *                                                               *
      * 01/31/19 C:291 Project: WC2206                                *
      *                Allow first modifier to be zero if second is   *
      *                populated. If the first modifier is zero and   *
      *                second not zero, populate the No Rating flag.  *
      *                                                               *
      * 01/22/19 C:298 Support: PHW 76288                             *
      *                Correct Edit on LD billing plan                *
      *                in Policy Review.                              *
      *                                                               *
      * 10/18/18 C:079 Support: 150/71923                             *
      *                Lift MCO edit on credits                       *
      *                                                               *
      * 07/31/18 C:267 Support: PHW 64666                             *
      *                Modify edits for Large Deductible effective    *
      *                dates.                                         *
      *                                                               *
      * 06/14/18 C:267 Project: 150WAQ                                *
      *                add an edit to prevent the policy from being   *
      *                changed off of Large Deductible                *
      *                if Collateral records exist.                   *
      *                                                               *
      * 06/12/18 C:267 Support: 64005                                 *
      *                Add edits to Billing Sched Code.               *
      *                                                               *
      * 05/25/18 C:136 Support: 71030                                 *
      *                Allow more than 365 days for non commercial    *
      *                groups for Sunz and AmWins only.               *
      *                                                               *
      * 05/16/18 C:291 Project: 150WCR                                *
      *                Pass inCall to EdtPEOPolType procedure for     *
      *                PEO Master/MCP Override.                       *
      *                                                               *
      * 02/07/18 C:291 Support: 63953                                 *
      *                Recompiled over WPILOCMST screen changes.      *
      //                                                               *
      // 12/13/17 C:33  Project: 150WAQ                                *
      //               -Edit billing schedule code against collateral  *
      *                                                               *
      * 08/10/17 C:180 Project: 150WAV                                *
      *                Add deductible Aggregate % to existing editing.*
      *                                                               *
      * 08/03/17 C:65  Project: 150WAR                                *
      *                Add editing for % of manual premium.           *
      *                                                               *
      * 07/31/17 C:291 Project: 150WAP                                *
      *                Recompiled over WPILOCMST screen changes.      *
      *                                                               *
      * 07/31/17 C:291 Project: 150WAP                                *
      *                Recompiled over WPILOCMST screen changes.      *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Added edits for CareWest Territory Rating      *
      *                                                               *
      * 04/27/16 C:306 Prpject :WC2424                                *
      *                Added edits for Experience Modifier Not final  *
      *                status flag.                                   *
      *                                                               *
      * 03/29/16 C:136 Phaseware: 39792                               *
      *                Adjust increased limits edit to possibly force *
      *                entry into second period for non-state         *
      *                divisions added midterm                        *
      *                                                               *
      * 01/08/16 C:306 Phaseware: 39216                               *
      *                Fixed the addition of division issue           *
      *                                                               *
      * 06/26/15 C:308 Phaseware: 35528                               *
      *                Recompile due to WPILOCMST screen changes.     *
      *                                                               *
      * 06/05/15 C:65  Support: Phw#34337                             *
      *                Change edit on stock discount code to use      *
      *                anniversary rating date if it gets used.       *
      *                                                               *
      * 05/12/15 C:306 Phaseware: 29738 LUBA                          *
      *                Recompile only                                 *
      *                                                               *
      * 04/09/15 C:242 Project: 132WQS                                *
      *                CompSuite Retrofit of Custom Code to Base      *
      *                Changed Contractor Credit to Custome Label     *
      *                                                               *
      * 02/26/15 C:290 Support: 32955- SCS.                           *
      *                Changed edits for Agent History in quote so    *
      *                that an agency could be inactivated, if there  *
      *                is at least one active agent effective on or   *
      *                before the effective date of the policy.       *
      *                                                               *
      * 11/04/14 C:65  Redmine: 4638 - T.H.E.                         *
      *                Recompiled over WPILOCMST screen file.         *
      *                                                               *
      * 10/14/14 C:280 Project: 114WPE
      *          C:33  check level(s) and type(s) set for rate
      *                add srccsc                                     *
      *                                                               *
      * 08/05/14 C:279 Project: 111WBL Priss: 21                      *
      *                Removed edit for dividend amount.              *
      *                                                               *
      * 07/11/14 C:279 Project: 111WBL                                *
      *                Modified to add edit for dividend amount.      *
      *                                                               *
      * 02/05/14 C:65  Project: SY336 Priss# 45                       *
      *                Change edit on large deductible premium level  *
      *                so it will check if a premium level has been   *
      *                entered as long as the percentage is not zero. *
      *                                                               *
      * 10/07/13 C:190 Project: 25WSS                                 *
      *                - From the Policy Year Detail/ Rating Tab      *
      *                  replace "Deviation #2" with  Stabilization   *
      *                  Factor                                       *
      *                - Add Edits for Claims Liability & Claims      *
      *                  Allowance fields in Large Ded. procedure     *
      *                - Remove TASB specific edit coding on Policy   *
      *                  Year Detail/Rating Tab to require entry of   *
      *                  Policy Type code if the Claims only flag is  *
      *                  set to "N".                                  *
      *                                                               *
      * 07/25/13 C:189 Redmine: 3177                                  *
      *                '# of employee' field should be mandatory based *
      *                 on the the system flag                        *
      *                                                               *
      * 05/27/13 C:136 Project: 147WAI                                *
      *                Add Dividend Plan Processing for CSI           *
      *                                                               *
      * 04/03/13 C:65  Redmine: SY266 Redmine:2451                    *
      *                Invoke rate set code state popup as long as    *
      *                there is a rate set code, not just when it     *
      *                changes.                                       *
      *                                                               *
      * 04/01/13 C:189 Support: 81926 WA114                           *
      *                The Large deductible edit for clearing should  *
      *                only invoked when large deductible is set up   *
      *                                                               *
      * 03/20/13 C:279 Redmine: 2450                                  *
      *                Correct Summarize A/R Edit.                    *
      *                                                               *
      * 03/13/13 C:65  Project: SY266 Redmine:2451                    *
      *                Change the rate set code state popup to appear *
      *                if rating state does not match physical        *
      *                address, or mailing if no physical.            *
      *                                                               *
      * 03/12/13 C:279 Project: SY266 Redmine:2456                    *
      *                Policy Year Detail/Rating - Deductible Bill on *
      *                Cancelled Check Box changed to Radio Button.   *
      *                                                               *
      * 01/25/13 C:283 Support: WA133 81481                           *
      *                For flat cancelled policies, the edit to       *
      *                compare the policy end date with the State     *
      *                Reporting Div's policy year dates should be    *
      *                skipped and should not be invoked.             *
      *                                                               *
      *********************************************************************
      *
      *
     H nomain

     fwpilocmst cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdBillDiv)
     fwtpoldtl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdRating)
     f                                     include(rcdRating2)
     f                                     include(rcdRateSt)
     f                                     include(rcdBSCDChg)
     f                                     include(rcdSpreadF)
     f                                     include(rcdRateChg)
     f                                     include(ElementSFL)
     f                                     include(rcdAnnivDt)
     f                                     include(rcdClaimDt)
     f                                     Sfile(ElementSFL:rrnElement)
     f                                     usropn
     FWMFNL     IF   E           K DISK    rename(PFN01:PFN00)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)
     FWMEMP     IF   E             DISK    rename(PEM01:PEM0P)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     FWMEPL     IF   E           K DISK    rename(PEP01:PEP00)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPR01)
     FWMRPL3    IF   E           K DISK    rename(PRP01:PRP03)
     FWMLDL1    IF   E           K DISK    rename(PLD01:PLD01)
     FWMAAL1    IF   E           K DISK    rename(PAA01:PAA01)
     FWMAHL     IF   E           K DISK    rename(PAH01:PAH00)
     FWMAPL     IF   E           K DISK    rename(PAP01:PAP00)
     FWMARL     IF   E           K DISK    rename(PAR01:PAR00)
     FWMRSL     IF   E           K DISK    rename(PRS01:PRS00)
     FWDRRL     IF   E           K DISK    rename(PRR01:PRR00)
     FWMACL     IF   E           K DISK    rename(PAC01:PAC00)
     FWMPFL     IF   E           K DISK    rename(PPF01:PPF00)
     FWMMGL     IF   E           K DISK    rename(PMG01:PMG00)
     FWMGRL     IF   E           K DISK    rename(PGR01:PGR00)
     FWMEALC    IF   E           K DISK    rename(PEA01:PEA0C)
     FWMEAL6    IF   E           k disk    rename(PEA01:PEA06)
     FWMEAL10   IF   E           k disk    rename(PEA01:PEA10)
     FWTABL     IF   E           K DISK
     F                                     ignore(PRT01)
     FWTGTL     IF   E           K DISK    rename(pgt01:PGT00)
     FWTGIL     IF   E           K DISK    rename(pgi01:PGI00)
     FWTLML1    IF   E           K DISK    rename(PLM01:PLM01)
     FWTDDL2    IF   E           K DISK    rename(PDD01:PDD02)
     FWDDDL1    IF   E           K DISK    rename(PDD01:PDD01)
     FWMDSL3    IF   E           K DISK    rename(PDS01:PDS03)
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX00)
     FWDFHL     IF   E           K DISK    rename(PFH01:PFH00)
     FWTTBL     IF   E           K DISK    rename(PTB01:PTB00)
     FWTAVL     IF   E           K DISK    rename(PAV01:PAV00)
     FWMGCL     IF   E           K DISK    rename(PGC01:PGC00)
     FWTRTL1    IF   E           K DISK    rename(PRT01:PRT01)
     FWTDVL11   IF   E           K DISK    rename(PDV01:PDV011)
     FWASRLQ    IF   E           K DISK
     FWMCOL     IF   E           K DISK    rename(PCO01:PCO00)
     F                                     prefix(L_)
     FWMCOL1    IF   E           K DISK
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
     f*** Beg Add *** 01/23/23 ************************************************
     Fwdf2l     if   e           k Disk
     f*** End Add *** 01/23/23 ************************************************

     D clientID        s              3    inz('000')
     D*                                                                     ******
     d outSecResult    s              1
     D PSDS          ESDS                  extname(SZQ1P)
     D*                                                                     ******

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d ComAPI          pr                  extpgm('WA001R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##Conm                      40
     d   ##Fnam                      40
     d   ##Lvl                        3  0
     d   ##Type                       1

     D RcdRateChgDsp   pr
     D  indValue                      1n   const
     D*                                                                ***********
      *-------------------------------------------------------------------------
      * Prototype Declaration
      *-------------------------------------------------------------------------
     dSecurityCheck    pr                  Extpgm('SRCCSC')
     d inUserType                     1    const
     d inUserLvl                      3    const
     d inCtlType01                    1    const
     d inCtlLvl01                     3    const
     d inRunType                      1    const
     d outSecResult                   1    const
     d inCtlType02                    1    const
     d inCtlType03                    1    const
     d inCtlType04                    1    const
     d inCtlType05                    1    const
     d inCtlType06                    1    const
     d inCtlLvl02                     3    const
     d inCtlLvl03                     3    const
     d inCtlLvl04                     3    const
     d inCtlLvl05                     3    const
     d inCtlLvl06                     3    const
     d inCtlType07                    1    const
     d inCtlType08                    1    const
     d inCtlType09                    1    const
     d inCtlType10                    1    const
     d inCtlLvl07                     3    const
     d inCtlLvl08                     3    const
     d inCtlLvl09                     3    const
     d inCtlLvl10                     3    const
     d GetStRptDivInf  pr
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inFyr                              like(ELfyr)
     d  outDiv                             like(ELdiv)
     d  outPolEfDt                         like(ELpfdt)
     d  outInLim1Efd                       like(ELpfdt)
     d  outInLim2Efd                       like(ELpfdt)
     d  rateState      s              2
     d  errorMsg       s            100

     D STA             S             75    DIM(11) CTDATA PERRCD(1)             Stat data inf msg

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

     D/copy sPrDate
     D/copy wPrUsrPrms
     D/copy wPrRateSet
     d/copy wPrEmplr
     d/copy WPRPOLERR
     d/copy wPrDivplan
     d/copy sPrGenPop

     IPMG00
     I              MGPHN#                      MG_MGPHN#

       //***************************************************************************//
       //===================================================================
       // Valid Date - Date passed in is valid and return formatted. If date passed
       //              in is not valid then value passed in is returned.
       //===================================================================
     P ValidDate       B                   export
     D ValidDate       PI             1
     D date                           8  0
     D messageID                    100
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C                   Clear                   messageID
     C*
     C*  EDIT DATE
     C*
     C                   Z-ADD     date          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *ON
     C                   movel(p)  'WCR0500'     messageID
     C                   return    *ON
     C                   ELSE
     C                   MOVEL     SRDTO         date
     C                   return    *OFF
     C                   ENDIF                                                  SRERR=*ON
     C*
     P ValidDate       E

       //***************************************************************************//
       //===================================================================
       // POLICY FROM DATE CANNOT BE LESS THAN INITIAL EFFECTIVE DATE
       //===================================================================
     P PolEfDtLess     B                   export
     D PolEfDtLess     PI             1
     D polDate                        8  0 const
     D empDate                        7  0 const
     D messageID                    100
     d*
     d xxPFDT          s              7  0
     C*
     C                   Clear                   messageID
     C*
     C*  POLICY FROM DATE CANNOT BE LESS THAN INITIAL EFFECTIVE DATE
     C*
     C                   eval      xxPFDT = dtCYMD(polDate)
     C*
     C     xxPFDT        IFLT      empDate
     C                   movel(p)  'WCR0609'     messageID
     C                   return    *ON
     C                   ELSE
     C                   return    *OFF
     C                   ENDIF                                                  XXPF<EMEFD1
     C*
     P PolEfDtLess     E

       //***************************************************************************//
       //===================================================================
       // POLICY FROM DATE MUST BE LESS THAN OR EQUAL TO ALL EXISTING
       // PAYROLL RECORDS
       //===================================================================
     P PolEfDtPayroll  B                   export
     D PolEfDtPayroll  PI             1
     D polDate                        8  0 const
     D elRRN                          9  0 const
     D messageID                    100
     d*
     d xxPFDT          s              7  0
     C*
     C* Employer Payroll File Key List
     C*
     C     keyWMEPL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR
     C*
     C                   Clear                   messageID
     C*
     C                   eval      xxPFDT = dtCYMD(polDate)
     C*
     C     elRRN         Chain     PEL0P
     C*
     C                   If        %found
     C*
     C*  POLICY FROM DATE MUST BE LESS THAN OR EQUAL TO ALL EXISTING
     C*  PAYROLL RECORDS
     C*
     C     keyWMEPL      Setll     PEP00
     C     keyWMEPL      Reade     PEP00
     C*
     C                   dow       not %EOF
     C*
     C     EPEFDT        IFLT      xxPFDT
     C     EPSTAT        ANDNE     'I'
     C                   movel(p)  'WCR0811'     messageID
     c                   Return    *ON
     C                   ENDIF                                                  EPEFDT<XXPF/EPSTAT<>
     C*
     C     keyWMEPL      Reade     PEP00
     C*
     C                   ENDDO                                                  dow not %EOF
     C*
     C                   ENDIF                                                  %Found
     C*
     C                   Return    *OFF
     C*
     P PolEfDtPayroll  E

       //***************************************************************************//
       //===================================================================
       // Dates must be in ascending order
       //===================================================================
     P AscendDates     B                   export
     D AscendDates     PI             1
     D fromDate                       8  0 const
     D toDate                         8  0 const
     D messageID                    100
     d*
     d xxDat1          s              7  0
     d xxDat2          s              7  0
     C*
     C                   Clear                   messageID
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C                   eval      xxDat1 = dtCYMD(fromDate)
     C                   eval      xxDat2 = dtCYMD(toDate)
     C*
     C     xxDat2        IFLT      xxDat1
     C                   movel(p)  'WCR0529'     messageID
     C                   return    *ON
     C                   ELSE
     C                   return    *OFF
     C                   ENDIF                                                  xxDAT2<xxDAT1
     C*
     P AscendDates     E

       //***************************************************************************//
       //===================================================================
       // Dates must be within Fund Date Range
       //===================================================================
     P PolFundDates    B                   export
     D PolFundDates    PI             1
     D fromDate                       8  0 const
     D toDate                         8  0 const
     D co#                            3  0 const
     D fnd                            3  0 const
     D fyr                            3  0 const
     D strp                           1    const
     D messageID                    100
     d*
     d xxDat1          s              7  0
     d xxDat2          s              7  0
     C*
     C* Group Master File Key List
     C*
     C     keyWMFNL      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    fyr
     C*
     C                   Clear                   messageID
     C*
     C     keyWMFNL      Chain     PFN00
     C     keyWDF1L      Chain     PF100
     C*
     C                   If        %found
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C                   eval      xxDat1 = dtCYMD(fromDate)
     C                   eval      xxDat2 = dtCYMD(toDate)
     C*
     C*  BOTH DATES MUST BE BETWEEN FUND POLICY START & END DATES
     C*
     C     FNCGFL        IFNE      'Y'
     C*
     C     xxDat1        IFLT      F1STPO
     C     xxDat1        ORGE      F1ENPO
     C     xxDat2        ORLT      F1STPO
     C     xxDat2        ORGT      F1ENPO
     C                   movel(p)  'WCR0723'     messageID
     C                   Return    *ON
     C                   ENDIF                                                  XXDAT1<F1STPO
     C*
     C                   ELSE
     C*
     C     strp          IFEQ      'Y'
     C     xxDat1        IFLT      F1STPO
     C     xxDat1        ORGE      F1ENPO
     C                   movel(p)  'WCR0723'     messageID
     C                   Return    *ON
     C                   ENDIF                                                  XXDAT1<F1STPO
     C*
     C                   ENDIF                                                  EMSTRP = Y
     C*
     C                   ENDIF                                                  FNCGFL<>Y
     C*
     C                   ENDIF                                                  If %Found
     C*
     C                   Return    *OFF
     C*
     P PolFundDates    E

       //***************************************************************************//
       //===================================================================
       // Valid Agent - Agent # is valid for group.
       //===================================================================
     P ValidAgent      B                   export
     D ValidAgent      PI             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agent                          5  0 const
     D messageID                    100
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     keyWMAAL1     KLIST
     C                   KFLD                    co#
     C                   KFLD                    fnd
     C                   KFLD                    agent
     C*
     C                   Clear                   messageID
     C*
     C     keyWMAAL1     CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   movel(p)  'WCR0530'     messageID
     C                   return    *ON
     C                   ELSE
     C                   return    *OFF
     C                   ENDIF                                                  *IN60=*ON
     C*
     P ValidAgent      E

       //***************************************************************************//
       //===================================================================
       // Valid Producer - Producer is valid for agent.
       //===================================================================
     P ValidProducer   B                   export
     D ValidProducer   PI             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agent                          5  0 const
     D producer                       5  0 const
     D messageID                    100
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     keyWMAAL1     KLIST
     C                   KFLD                    co#
     C                   KFLD                    fnd
     C                   KFLD                    agent
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     keyWMAPL      KLIST
     C                   KFLD                    co#
     C                   KFLD                    AAFein
     C                   KFLD                    AAMod
     C                   KFLD                    producer
     C*
     C                   Clear                   messageID
     C*
     C     keyWMAAL1     CHAIN     PAA01                              60
     C*
     C     keyWMAPL      CHAIN     PAP00                              60
     C*
     C     *IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   movel(p)  'WCR0531'     messageID
     C                   return    *ON
     C                   ELSE
     C                   return    *OFF
     C                   ENDIF                                                  *IN60=*ON
     C*
     P ValidProducer   E

       //***************************************************************************//
       //===================================================================
       // Valid Rate - Rate is valid for group.
       //===================================================================
     P ValidAgtRate    B                   export
     D ValidAgtRate    PI             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D rate                           3  0 const
     D effDate                        8  0 const
     D messageID                    100
     d*
     d xxDate          s              7  0
     C*
     C*  AGENT RATE FILE
     C*
     C     keyWMARL      KLIST
     C                   KFLD                    co#
     C                   KFLD                    fnd
     C                   KFLD                    rate
     C                   KFLD                    xxDate
     C*
     C     key1WMARL     KLIST
     C                   KFLD                    co#
     C                   KFLD                    fnd
     C                   KFLD                    rate
     C*
     C                   Clear                   messageID
     C*
     C                   eval      xxDate = dtCYMD(effDate)
     C*
     C     keyWMARL      SETGT     PAR00
     C     key1WMARL     READPE    PAR00                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     key1WMARL     READPE    PAR00                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *ON
     C     ARSTAT        OREQ      'I'
     C                   movel(p)  'WCR0532'     messageID
     C                   return    *ON
     C                   ENDIF                                                  *IN60=*ON
     C*
      /free

       // Ensure the user has authority to the selected rate
       SecurityCheck(GetUsrType(q1User):%EditC(GetUsrLvl(q1User):'X'):
        ARSECTYP01:%EditC(ARSECLVL01:'X'):
        '1':outSecResult:
        ARSECTYP02:ARSECTYP03:ARSECTYP04:ARSECTYP05:ARSECTYP06:
        %EditC(ARSECLVL02:'X'):%EditC(ARSECLVL03:'X'):
        %EditC(ARSECLVL04:'X'):%EditC(ARSECLVL05:'X'):
        %EditC(ARSECLVL06:'X'):
        ARSECTYP07:ARSECTYP08:ARSECTYP09:ARSECTYP10:
        %EditC(ARSECLVL07:'X'):%EditC(ARSECLVL08:'X'):
        %EditC(ARSECLVL09:'X'):%EditC(ARSECLVL10:'X')
        );

       If outSecResult = '1';
          messageid = 'WCR4807';
          Return *on;
       Endif;

       Return *off;
      /End-Free
     C*
     P ValidAgtRate    E

       //***************************************************************************//
       //===================================================================
       // Date must be less than or equal to Policy Eff Date
       //===================================================================
     P PolDtLessPolEf  B                   export
     D PolDtLessPolEf  PI             1
     D date                           8  0 const
     D polDate                        8  0 const
     D messageID                    100
     d*
     d xxDate          s              7  0
     d xxPolEf         s              7  0
     C*
     C                   Clear                   messageID
     C*
     C* Date must be less than or equal to policy effective date.
     C*
     C                   eval      xxDate  = dtCYMD(date)
     C                   eval      xxPolEf = dtCYMD(polDate)
     C*
     C     xxDate        IFGT      xxPolEf
     C                   movel(p)  'WCR0577'     messageID
     C                   Return    *ON
     C                   ENDIF                                                  xxDate > xxPolEf
     C*
     C                   Return    *OFF
     C*
     P PolDtLessPolEf  E
       //***************************************************************************//
       //===================================================================
       // Valid Flag Y - Value must be Y or Blank.
       //===================================================================
     P ValidFlagY      B                   export
     D ValidFlagY      PI             1
     D flag                           1    const
     D messageID                    100
     C*
     C                   Clear                   messageID
     C*
     C* Flag must have a value of Y or blank.
     C*
     C     flag          IFNE      'Y'
     C     flag          ANDNE     ' '
     C                   movel(p)  'WCR0571'     messageID
     C                   Return    *ON
     C                   ENDIF                                                  flag <> Y/' '
     C*
     C                   Return    *OFF
     C*
     P ValidFlagY      E
     P*
       //***************************************************************************//
       //===================================================================
       // Valid Flag YN - Value must be Y or N.
       //===================================================================
     P ValidFlagYN     B                   export
     D ValidFlagYN     PI             1
     D flag                           1    const
     D messageID                    100
     C*
     C                   Clear                   messageID
     C*
     C* Flag must have a value of Y or N.
     C*
     C     flag          IFNE      'Y'
     C     flag          ANDNE     'N'
     C                   movel(p)  'WCR0501'     messageID
     C                   Return    *ON
     C                   ENDIF                                                  flag <> Y/N
     C*
     C                   Return    *OFF
     C*
     P ValidFlagYN     E
     P*
       //***************************************************************************//
       //===================================================================
       // Producer Change - Producer cannot change if commission exists
       //===================================================================
     P PolProdChg      B                   export
     D PolProdChg      PI             1
     D Prod                           5  0 const
     D svProd                         5  0 const
     D emRRN                          9  0 const
     D fyr                            3  0 const
     D agt#                           5  0 const
     D messageID                    100
     C*
     C*  Agent Commission File
     C*
     C     keyWMACL      KLIST
     C                   KFLD                    EMCo#
     C                   KFLD                    EMFnd
     C                   KFLD                    fyr
     C                   KFLD                    EMEmp#
     C                   KFLD                    EMDiv
     C                   KFLD                    agt#
     C                   KFLD                    svProd
     C*
     C                   Clear                   messageID
     C*
     C     emRRN         Chain     PEM0P
     C*
     C* Flag must have a value of Y or N.
     C*
     C     prod          IFNE      svProd
     C*
     C     keyWMACL      Chain     PAC00                              60
     C*
     C                   If        *in60 = *off
     C                   eval      messageID = 'Producer# cannot be changed ' +
     C                              '- has current Commission records.'
     C                   Return    *ON
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Return    *OFF
     C*
     P PolProdChg      E
     P*
       //***************************************************************************//
       //===================================================================
       // Agent eff/cancel dates are valid for agent history effective date
       //===================================================================
     P PolAgtDate      B                   export
     D PolAgtDate      PI             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agt#                           5  0 const
     D efdt                           8  0 const
     D messageID                    100
     d*
     d xxDate          s              7  0
     C*
     C*  Agent Assignment File
     C*
     C     keyWMAAL1     KLIST
     C                   KFLD                    co#
     C                   KFLD                    fnd
     C                   KFLD                    agt#
     C*
     C                   Clear                   messageID
     C*
     C                   eval      xxDate  = dtCYMD(efdt)
     C*
     C     keyWMAAL1     Chain     PAA01                              60
     C*
     C                   If        *in60 = *off and (AAEfdt > xxDate or
     C                             (AACndt <> 0 and AACndt < xxDate))
     C*
     C                   movel(p)  'WC30172'     messageID
     C                   Return    *ON
     C*
     C                   ENDIF
     C*
     C                   Return    *OFF
     C*
     P PolAgtDate      E
       //***************************************************************************//
       //===================================================================
       // Producer cancel date is valid for agent history effective date
       //===================================================================
     P PolProdDate     B                   export
     D PolProdDate     PI             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D agt#                           5  0 const
     D prod                           5  0 const
     D efdt                           8  0 const
     D messageID                    100
     d*
     d xxDate          s              7  0
     C*
     C*  Agent Assignment File
     C*
     C     keyWMAAL1     KLIST
     C                   KFLD                    co#
     C                   KFLD                    fnd
     C                   KFLD                    agt#
     C*
     C*  Producer File
     C*
     C     keyWMAPL      KLIST
     C                   KFLD                    co#
     C                   KFLD                    AAFein
     C                   KFLD                    AAMod
     C                   KFLD                    prod
     C*
     C                   Clear                   messageID
     C*
     C                   eval      xxDate  = dtCYMD(efdt)
     C*
     C     keyWMAAL1     Chain     PAA01                              60
     C     keyWMAPL      Chain     PAP00                              60
     C*
     C                   If        *in60 = *off and
     C                             (APCxdt <> 0 and APCxdt <= xxDate)
     C*
     C                   movel(p)  'WCR3157'     messageID
     C                   Return    *ON
     C*
     C                   ENDIF
     C*
     C                   Return    *OFF
     C*
     P PolProdDate     E
     P*
       //***************************************************************************//
       //===================================================================
       // PolAgtCount - Ensure an agent record cannot be inactivated unless
       //               another with Effective Date <= original has
       //               replaced it.
       //===================================================================
     P PolAgtCount     B                   export
     D PolAgtCount     PI             1
     D elRRN                          9  0 const
     D stat                           1    const
     D efdt                           8  0 const
     D messageID                    100
     d xxEfdt          s              7  0
     d agtError        s              1
     C*
     C*  Agent History File
     C*
     C     keyWMAHL      KLIST
     C                   KFLD                    ELCo#
     C                   KFLD                    ELFnd
     C                   KFLD                    ELEmp#
     C                   KFLD                    ELDiv
     C                   KFLD                    ELFyr
     C*
     C* Employer Master File
     C*
     C     keyWMEML      KLIST
     C                   KFLD                    ELCo#
     C                   KFLD                    ELFnd
     C                   KFLD                    ELEmp#
     C                   KFLD                    ELDiv
     C*
     C                   Clear                   messageID
     C                   eval      agtError = *off
     C*
     C     elRRN         Chain     PEL0P
     C     keyWMEML      Chain     PEM00
     C*
     C                   Z-add     0             agtCnt            5 0
     C                   Z-add     0             qteCnt            5 0
     C                   eval      xxEfdt = dtCYMD(efdt)
     C*
     C     keyWMAHL      Setll     PAH00
     C     keyWMAHL      Reade     PAH00                                  60
     C*
     C     *in60         Doweq     *off
     C*
     C     AHEfdt        Ifle      xxEfdt
     C     AHStat        Andeq     'A'
     C                   Add       1             agtCnt
     C                   Endif
     C*
      /free
           If stat = 'Q' and AHStat = 'A' and AHEfdt <= ELPfdt;
              qteCnt = qteCnt + 1;
           Endif;
      /end-free
     C     keyWMAHL      Reade     PAH00                                  60
     C                   Enddo
      /free
           Select;
              When stat = 'Q' and qteCnt <= 1;
              agtError = *on;
              When stat <> 'Q' and agtCnt <= 1;
              agtError = *on;
           Endsl;
      /end-free
     C*
     C                   If        agtError = *on
     C                   eval      messageID = 'WC32400'
     C                   Return    *ON
     C                   ENDIF
     C*
     C                   Return    *OFF
     C*
     P PolAgtCount     E
     P*
       //***************************************************************************//
       //===================================================================
       // ErrPolRating - Edit the policy rating fields from different screens/programs.
       //                The rating screens are contained in the policy detail program,
       //                the location master popup window, and the rating popup window.
       //                Different screen fields will be edited depending on the mode
       //                the procedure is called with, which will reference the following
       //                screen formats:
       //
       //   Call 'P'     Policy Rating Tab        Screen format #1 (WTPOLDTL/rcdRating)
       //                                         Screen format #2 (WTPOLDTL/rcdRating2)
       //   Call 'L'     Location Master Window   Screen format #1 (WPILOCMST/rcdBillDiv)
       //                                         Screen format #2 (none)
       //   Call 'R'     Policy Rating Window     Screen format #1 (WTPOLDTL/rcdRating2)
       //                                         Screen format #2 (none)
       //
       //                The procedure will return an *OFF value if an edit error is found,
       //                or an *ON value if all edits are passed.
       //
       //===================================================================
     P ErrPolRating    B                   export
     D ErrPolRating    PI              n
     d inCall                         1    const
     d inMode                         1    const
     d inQuote                        1    const
     d inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     d inELrrn                        9  0 const
     d inData                       500
     d inPtrFormat2                    *   options(*nopass)
     d*
     D scCo#           s                   like(ELCo#)
     D scFnd           s                   like(ELFnd)
     D scEmp#          s                   like(ELEmp#)
     D scDiv           s                   like(ELDiv)
     D scFyr           s                   like(ELFyr)
     d*
     D qteMode         s               n   inz(*off)
     d*
     d error           s               n   inz
     D message         s            100
     d*
     D achFlg          s              1n
     D vndrFl          s              1n
     D clearWMLDP      s              1n
     DdeductInstalled  s              1n
     D svBsdt          s                   like(ELBSDT)
     D svBscd          s                   like(ELBSCD)
     D sysDt7          s              7  0
     D sysDt8          s              8  0
     D intCo#          s              3  0 inz
     D intFnd          s              3  0 inz
     D compName        s             40
     D fundName        s             40
     D secType         s              1
     D secLevel        s              3  0
     d xxKey           s             10    inz
     D xsKey           s              2
     d xxSt            s              2    inz
     D xxDate          s              7  0
     D cxDate7         s              7  0
     D xxcfdt          s              7  0
     D xxredt          s              7  0
     D xxEfDt          s                   like(ELPFDT)
     D x2EfDt          s                   like(ELPFDT)
     D x3EfDt          s                   like(ELPFDT)
     D xxExDt          s                   like(ELPFDT)
     D xxPTDT          s                   like(ELPFDT)
     D xxTBID          s                   like(GTTBID)
     D xxTBCD          s                   like(TBTBCD)
     D xxDDcd          s                   like(ELDCDE)
     D xxFCOD          s                   like(PFFCOD)
     D xxMCGP          s                   like(ELMCGP)
     D xxDiscDt        s                   like(ELPFDT)
     D x2Sefl          s                   like(BSSefl)
     D x2Freq          s                   like(BSFreq)

     D x2LPGM          s                   like(BSLPGM)

     d ptrFormatB      s               *   inz
     d ptrFormatR      s               *   inz
     d ptrFormatR2     s               *   inz
     d*
     d dsBillDiv     e ds                  extname(wpilocmst:rcdBillDiv)
     d                                     prefix(bill_)
     d                                     based(ptrFormatB)
     d dsRating      e ds                  extname(wtpoldtl:rcdRating)
     d                                     prefix(rte1_)
     d                                     based(ptrFormatR)
     d dsRating2     e ds                  extname(wtpoldtl:rcdRating2)
     d                                     prefix(rte2_)
     d                                     based(ptrFormatR2)
     d isIncLim1Exc    s               n
     d stRptDiv        s                   like(ELdiv)
     d stRptFyrEfDt    s                   like(ELpfdt)
     d stRptIn1EfDt    s                   like(ELpfdt)
     d stRptIn2EfDt    s                   like(ELpfdt)
     d*
     d chrAddPgm       s             10
     d chrPrtPgm       s             10
     d chrRateState    s              2
     d chrWvBlkPrjDs   s            500
     d chrWvrForm      s             15
     d chrWvrType      s              1
     d chrWvrTypeVbg   s             20
     d intRtnDiv       s              5  0
      /Copy wPrEndorse
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
     C*
     C* Employer Payroll Self-Reporting
     C*
     C     keyWASRLQ     Klist
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
     C*
     C*  Increased Limits File Key List
     C*
     C     keyWTLML1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxKey
     C                   Kfld                    xxDate
     C*
     C     key1WTLML1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxKey
     C*
     C     key2WTLML1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C*
     C*  Deductible Discount File Key List
     C*
     C     keyWTDDL2     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDDcd
     C                   Kfld                    xxDate
     C*
     C     key1WTDDL2    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDDcd
     C*
     C     key2WTDDL2    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C*
     C*  Stock Discount File Key List
     C*
     C     keyWMDSL3     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    scDISC
     C                   Kfld                    xxDiscDt
     C*
     C     key1WMDSL3    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    scDISC
     C*
     C* MCO Group File Key List
     C*
     C     keyWMMGL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scMcgp
     C                   Kfld                    xxDate
     C*
     C     key1WMMGL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scMcgp
     C*
     C* Generic Table File Key List
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C     keyWTGIL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbid
     C*
     C* Type of Building File Key List
     C*
     C     keyWTTBL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbcd
     C*
     C* Flexible Heading File Key List
     C*
     C     keyWDFHL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
      /Free

       Open WTPOLDTL;
       dsRateEdit = inData;

       Select;
          When inCall = 'L';
             ptrFormatB = inPtrFormat1;
          When inCall = 'R';
             ptrFormatR2 = inPtrFormat1;
          When inCall = 'P';
             ptrFormatR = inPtrFormat1;
             ptrFormatR2 = inPtrFormat2;
          Other;
             Return *off;
       Endsl;

       // Setup the files and fields necessary for editing.
       Exsr Data_Setup;

       // Move data structure fields into screen fields.
       Exsr Move_Into_Scr;

       // Process Edits
       Exsr EditRating;

       // Move screen fields back into data structure fields.
       Exsr Move_Into_Ds;

       Close WTPOLDTL;
       inData = dsRateEdit;

       Return not error;

      /End-Free

      //===================================================================
      // Data_Setup - Load all the necessary files and fields for editing.
      //===================================================================
     C     Data_Setup    Begsr
      /free

       Clear error;

       sysDt7 = DateTo7(SystemDate);
       sysDt8 = DateTo8(SystemDate);

       scCo#  = inCo#;
       scFnd  = inFnd;
       scEmp# = inEmp#;
       scDiv  = inDiv;
       scFyr  = inFyr;
       GetStRptDivInf(scCo#:scFnd:scEmp#:scFyr:stRptDiv:
        stRptFyrEfDt:stRptIn1EfDt:stRptIn2EfDt);
       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;
       Chain keyWMEXL PEX00;
       Chain keyWDELL pel00;
       Chain keyWMEML PEM00;
       // To clean the previous hold value present in the EMEFD1 field,in case
       // chain fails.

       If not %found(WMEML);
          Clear EMEFD1;
          Clear EMSTRP;
       EndIf;
       Chain keyWDFHL PFH00;

       GetClientID(clientID);

       qteMode = *off;
       If inQuote = 'Q';
          qteMode = *on;
       Endif;

       intCo# = scCo#;
       intFnd = scFnd;
       ComAPI(intCo#:intFnd:compName:fundName:secLevel:secType);

       xxSt = RatingSt(ELCO#:ELRTCD);
       xxDate =  ELPFDT;
       SetGt keyWDRRL PRR00;
       Readpe key1WDRRL PRR00;

       xsKey = 'WC';
       Chain xsKey SWC01;
       deductInstalled = WCFL06 = 'Y';
       achFlg = WCFL24 = 'Y';
       vndrFl = WCFL23 = 'Y';

      /end-free
     C                   Endsr                                                  Data_Setup Subr

      //===================================================================
      // Move_Into_Scr - Load the data structure fields into screen fields.
      //===================================================================
     C     Move_Into_Scr Begsr
      /free

       If inCall = 'L';
          scPFDT = bill_scPFDT;
          scPTDT = bill_scPTDT;
          scANDT = bill_scANDT;
          scRTCD = bill_scRTCD;
          scBSTY = bill_scBSTY;
          scBSCD = bill_scBSCD;
          scBSDT = bill_scBSDT;
          scSTAT = bill_scSTAT;
          scBLFL = bill_scBLFL;
          scASCD = bill_scASCD;
          scPFIN = bill_scPFIN;
          scFFL1 = bill_scFFL1;
          scARSM = bill_scARSM;
          scIFFL = bill_scIFFL;
          scQTYP = bill_scQTYP;
          scTYPB = bill_scTYPB;
          scEMPY = bill_scEMPY;
          scOvrMP = bill_scOvrMP;

          erPFDT = bill_erPFDT;
          erPTDT = bill_erPTDT;
          erANDT = bill_erANDT;
          erRTCD = bill_erRTCD;
          erRTCD2 = bill_erRTCD2;
          erBSCD = bill_erBSCD;
          erBSDT = bill_erBSDT;
          erSTAT = bill_erSTAT;
          erBLFL = bill_erBLFL;
          erASCD = bill_erASCD;
          erPFIN = bill_erPFIN;
          erFFL1 = bill_erFFL1;
          erARSM = bill_erARSM;
          erIFFL = bill_erIFFL;
          erQTYP = bill_erQTYP;
          erTYPB = bill_erTYPB;
          erEMPY = bill_erEMPY;

          msPFDT = bill_msPFDT;
          msPTDT = bill_msPTDT;
          msANDT = bill_msANDT;
          msRTCD = bill_msRTCD;
          mgRTCD = bill_mgRTCD;
          msBSCD = bill_msBSCD;
          msBSDT = bill_msBSDT;
          msSTAT = bill_msSTAT;
          msBLFL = bill_msBLFL;
          msASCD = bill_msASCD;
          msPFIN = bill_msPFIN;
          msFFL1 = bill_msFFL1;
          msARSM = bill_msARSM;
          msIFFL = bill_msIFFL;
          mgQTYP = bill_mgQTYP;
          mgTYPB = bill_mgTYPB;
          msEMPY = bill_msEMPY;
       Endif;

       If inCall = 'R' or inCall = 'P';

          If inCall = 'R';
             scPFDT =  dtMDYY(ELPFDT);
             scPTDT =  dtMDYY(ELPTDT);
          Else;
             scPFDT = rte1_scPFDT;
             scPTDT = rte1_scPTDT;
             scANDT = rte1_scANDT;
             scARSM = rte1_scARSM;
             scASCD = rte1_scASCD;
             scBLFL = rte1_scBLFL;
             scBSCD = rte1_scBSCD;
             scBSDT = rte1_scBSDT;
             scBSTY = rte1_scBSTY;
             scEMPY = rte1_scEMPY;
             scFFL1 = rte1_scFFL1;
             scIFFL = rte1_scIFFL;
             scPFIN = rte1_scPFIN;
             scQTYP = rte1_scQTYP;
             scRTCD = rte1_scRTCD;
             scSTAT = rte1_scSTAT;
             scTYPB = rte1_scTYPB;
             scOvrMP = rte1_scOvrMP;

             erPFDT = rte1_erPFDT;
             erPTDT = rte1_erPTDT;
             erANDT = rte1_erANDT;
             erARSM = rte1_erARSM;
             erASCD = rte1_erASCD;
             erBLFL = rte1_erBLFL;
             erBSCD = rte1_erBSCD;
             erBSDT = rte1_erBSDT;
             erEMPY = rte1_erEMPY;
             erFFL1 = rte1_erFFL1;
             erIFFL = rte1_erIFFL;
             erPFIN = rte1_erPFIN;
             erQTYP = rte1_erQTYP;
             erRTCD = rte1_erRTCD;
             erRTCD2 = rte1_erRTCD2;
             erSTAT = rte1_erSTAT;
             erTYPB = rte1_erTYPB;

             msPFDT = rte1_msPFDT;
             msPTDT = rte1_msPTDT;
             msANDT = rte1_msANDT;
             msARSM = rte1_msARSM;
             msASCD = rte1_msASCD;
             msBLFL = rte1_msBLFL;
             msBSCD = rte1_msBSCD;
             msBSDT = rte1_msBSDT;
             msEMPY = rte1_msEMPY;
             msFFL1 = rte1_msFFL1;
             msIFFL = rte1_msIFFL;
             msPFIN = rte1_msPFIN;
             msQTYP = rte1_msQTYP;
             msRTCD = rte1_msRTCD;
             mgRTCD = rte1_mgRTCD;
             msSTAT = rte1_msSTAT;
             msTYPB = rte1_msTYPB;
          Endif;

          deviation1 = rte2_deviation1;
          deviation2 = rte2_deviation2;
          subject1A  = rte2_subject1A;
          subject1B  = rte2_subject1B;
          schedule   = rte2_schedule;
          waiver     = rte2_waiver;
          faculty    = rte2_faculty;
          volLabor   = rte2_volLabor;

          If inCall = 'R';
             sc2500 = rte2_sc2500;
             scCOFL = rte2_scCOFL;
             scFFL9 = rte2_scFFL9;
             scSRPR = rte2_scSRPR;

             er2500 = rte2_er2500;
             erCOFL = rte2_erCOFL;
             erFFL9 = rte2_erFFL9;
             erSRPR = rte2_erSRPR;

             ms2500 = rte2_ms2500;
             msCOFL = rte2_msCOFL;
             msFFL9 = rte2_msFFL9;
             msTSRPR = rte2_msTSRPR;
             scMANP = rte2_scMANP;
             erMANP = rte2_erMANP;
             msMANP = rte2_msMANP;
          Else;
             sc25002 = rte1_sc25002;
             scCOFL2 = rte1_scCOFL2;
             scFFL92 = rte1_scFFL92;
             scSRPR2 = rte1_scSRPR2;

             er25002 = rte1_er25002;
             erCOFL2 = rte1_erCOFL2;
             erFFL92 = rte1_erFFL92;
             erSRPR2 = rte1_erSRPR2;

             ms25002 = rte1_ms25002;
             msCOFL2 = rte1_msCOFL2;
             msFFL92 = rte1_msFFL92;
             msSRPR2 = rte1_msSRPR2;
             scMANP2 = rte1_scMANP2;
             erMANP2 = rte1_erMANP2;
             msMANP2 = rte1_msMANP2;
          Endif;

          scARCA = rte2_scARCA;
          scARCP = rte2_scARCP;
          scAREFDT = rte2_scAREFDT;
          scAREXDT = rte2_scAREXDT;
          scARP1 = rte2_scARP1;
          scARP2 = rte2_scARP2;
          scARP3 = rte2_scARP3;
          scExMd1Nf = rte2_scExMd1Nf;
          scExMd2Nf = rte2_scExMd2Nf;
          scExMd3Nf = rte2_scExMd3Nf;
          scASP1 = rte2_scASP1;
          scASP2 = rte2_scASP2;
          scASP3 = rte2_scASP3;
          scASPM = rte2_scASPM;
          scASPS = rte2_scASPS;
          scCCEFDT = rte2_scCCEFDT;
          scCCEXDT = rte2_scCCEXDT;
          scCXBL = rte2_scCXBL;
          scD1D2EFDT = rte2_scD1D2EFDT;
          scD1D2EXDT = rte2_scD1D2EXDT;
          scD1S2 = rte2_scD1S2;
          scD2D2EFDT = rte2_scD2D2EFDT;
          scD2D2EXDT = rte2_scD2D2EXDT;
          scD2S2 = rte2_scD2S2;
          scDBCR = rte2_scDBCR;
          scDCD2 = rte2_scDCD2;
          scDCDE = rte2_scDCDE;
          scDD1EFDT = rte2_scDD1EFDT;
          scDD1EXDT = rte2_scDD1EXDT;
          scDD2EFDT = rte2_scDD2EFDT;
          scDD2EXDT = rte2_scDD2EXDT;
          scDEV1H = rte2_scDEV1H;
          scDEV2 = rte2_scDEV2;
          scDEV2H = rte2_scDEV2H;
          scDISC = rte2_scDISC;
          scDISP = rte2_scDISP;
          scDV1EFDT = rte2_scDV1EFDT;
          scDV1EXDT = rte2_scDV1EXDT;
          scDV2EFDT = rte2_scDV2EFDT;
          scDV2EXDT = rte2_scDV2EXDT;
          scEM1DATE = rte2_scEM1DATE;
          scEM2DATE = rte2_scEM2DATE;
          scEM3DATE = rte2_scEM3DATE;
          scEXM1 = rte2_scEXM1;
          scEXM2 = rte2_scEXM2;
          //*** begin add ***  03/18/25  **************************************
          scEXM3 = rte2_scEXM3;
          //*** end add *****  03/18/25  **************************************
          If scExM1 = 0 and scExM2 <> 0;
             scNoExM1 = 'Y';
             scEM1Date = dtMDYY(ELpfdt);
          Else;
             scNoExM1 = *Blank;
             If scExM1 = 0 and scExM2 = 0;
                scEM1Date = *Zero;
             Endif;
          Endif;
          //*** Begin Add *** 07/30/23 *************************************
          // Set not rated for experience modifier 2
          If scExM2 = 0 And scEm2Date <> 0 And scExM1 <> 0;
             scNoExM2 = 'Y';
          Else;
             scNoExM2 = *Blank;
             If scExM2 = 0;
                scEM2Date = *Zero;
             EndIf;
          Endif;
          // Set not rated for experience modifier 3
          If scExM3 = 0 And scEm3Date <> 0 And scExM2 <> 0;
             scNoExM3 = 'Y';
          Else;
             scNoExM3 = *Blank;
             If scExM3 = 0;
                scEM3Date = *Zero;
             EndIf;
          Endif;
          //*** End   Add *** 07/30/23 *************************************
          //*** begin del ***  03/18/25  **************************************
          //*****scEXM3 = rte2_scEXM3;
          //*** end del *****  03/18/25  **************************************
          scFCHG = rte2_scFCHG;
          scIL1EFDT = rte2_scIL1EFDT;
          scIL1EXDT = rte2_scIL1EXDT;
          scIL2EFDT = rte2_scIL2EFDT;
          scIL2EXDT = rte2_scIL2EXDT;
          scILCD = rte2_scILCD;
          scINL2 = rte2_scINL2;
          scMCCD = rte2_scMCCD;
          scMCEFDT = rte2_scMCEFDT;
          scMCEXDT = rte2_scMCEXDT;
          scMCGP = rte2_scMCGP;
          scMFEE = rte2_scMFEE;
          scPROM = rte2_scPROM;
          scSBJ1AH = rte2_scSBJ1AH;
          scSBJ1BH = rte2_scSBJ1BH;
          scContCrdH= rte2_scContCrdH;
          scSCEFDT = rte2_scSCEFDT;
          scSCEXDT = rte2_scSCEXDT;
          scSDEFDT = rte2_scSDEFDT;
          scSDEXDT = rte2_scSDEXDT;
          scSFHD = rte2_scSFHD;
          scSFMN = rte2_scSFMN;
          scSFPC = rte2_scSFPC;
          scVLCA = rte2_scVLCA;
          scVLCH = rte2_scVLCH;
          scWMAX = rte2_scWMAX;
          scWMIN = rte2_scWMIN;
          scWVFL = rte2_scWVFL;
          //*** Beg Add *** 12/11/23 *** PHW114578 ***********************
          scBlnkWd = rte2_scBlnkWd;
          scOVRWD = rte2_scOVRWD;
          //*** End Add *** 12/11/23 *** PHW114578 ***********************
          scWVPC = rte2_scWVPC;
          scWVTP = rte2_scWVTP;
          scStfc = rte2_scStfc;
          scDev2F = rte2_scDev2F;
          //*** Beg Add *** 03/04/24 *************************************
          scExpRat = rte2_scExpRat;
          //*** End Add *** 03/04/24 *************************************

          erARCA = rte2_erARCA;
          erARCP = rte2_erARCP;
          erAREFDT = rte2_erAREFDT;
          erAREXDT = rte2_erAREXDT;
          erARP1 = rte2_erARP1;
          erARP2 = rte2_erARP2;
          erARP3 = rte2_erARP3;
          erExMd1Nf = rte2_erExMd1Nf;
          erExMd2Nf = rte2_erExMd2Nf;
          erExMd3Nf = rte2_erExMd3Nf;
          erASP1 = rte2_erASP1;
          erASP2 = rte2_erASP2;
          erASP3 = rte2_erASP3;
          erASPM = rte2_erASPM;
          erASPS = rte2_erASPS;
          erCCEFDT = rte2_erCCEFDT;
          erCCEXDT = rte2_erCCEXDT;
          erCXBL = rte2_erCXBL;
          erD1D2EFDT = rte2_erD1D2EFDT;
          erD1D2EXDT = rte2_erD1D2EXDT;
          erD1S2 = rte2_erD1S2;
          erD2D2EFDT = rte2_erD2D2EFDT;
          erD2D2EXDT = rte2_erD2D2EXDT;
          erD2S2 = rte2_erD2S2;
          erDBCR = rte2_erDBCR;
          erDCD2 = rte2_erDCD2;
          erDCDE = rte2_erDCDE;
          erDD1EFDT = rte2_erDD1EFDT;
          erDD1EXDT = rte2_erDD1EXDT;
          erDD2EFDT = rte2_erDD2EFDT;
          erDD2EXDT = rte2_erDD2EXDT;
          erDEV2 = rte2_erDEV2;
          erDISC = rte2_erDISC;
          erDV1EFDT = rte2_erDV1EFDT;
          erDV1EXDT = rte2_erDV1EXDT;
          erDV2EFDT = rte2_erDV2EFDT;
          erDV2EXDT = rte2_erDV2EXDT;
          erEM1DATE = rte2_erEM1DATE;
          erEM2DATE = rte2_erEM2DATE;
          erEM3DATE = rte2_erEM3DATE;
          erEXM1 = rte2_erEXM1;
          erEXM2 = rte2_erEXM2;
          erEXM3 = rte2_erEXM3;
          erFCHG = rte2_erFCHG;
          erIL1EFDT = rte2_erIL1EFDT;
          erIL1EXDT = rte2_erIL1EXDT;
          erIL2EFDT = rte2_erIL2EFDT;
          erIL2EXDT = rte2_erIL2EXDT;
          erILCD = rte2_erILCD;
          erINL2 = rte2_erINL2;
          erMCCD = rte2_erMCCD;
          erMCEFDT = rte2_erMCEFDT;
          erMCEXDT = rte2_erMCEXDT;
          erMCGP = rte2_erMCGP;
          erMFEE = rte2_erMFEE;
          erPROM = rte2_erPROM;
          erSCEFDT = rte2_erSCEFDT;
          erSCEXDT = rte2_erSCEXDT;
          erSDEFDT = rte2_erSDEFDT;
          erSDEXDT = rte2_erSDEXDT;
          erSFMN = rte2_erSFMN;
          erSFPC = rte2_erSFPC;
          erVLCA = rte2_erVLCA;
          erWMAX = rte2_erWMAX;
          erWMIN = rte2_erWMIN;
          erWVFL = rte2_erWVFL;
          //*** Beg Add *** 12/11/23 *** PHW114578 ***********************
          erBlnkWd = rte2_erBlnkWd;
          //*** End Add *** 12/11/23 *** PHW114578 ***********************
          erWVPC = rte2_erWVPC;
          erWVTP = rte2_erWVTP;
          erStfc = rte2_erStfc;
          erDev2F = rte2_erDev2F;
          erNoWvrDsc = rte2_erNoWvrDsc;
          //*** Beg Add *** 03/04/24 *************************************
          erExpRat = rte2_erExpRat;
          //*** End Add *** 03/04/24 *************************************
          msNoWvrDsc = rte2_msNoWvrDsc;
          msARCA = rte2_msARCA;
          msARCP = rte2_msARCP;
          msAREFDT = rte2_msAREFDT;
          msAREXDT = rte2_msAREXDT;
          msARP1 = rte2_msARP1;
          msARP2 = rte2_msARP2;
          msARP3 = rte2_msARP3;
          msExMd1Nf = rte2_msExMd1Nf;
          msExMd2Nf = rte2_msExMd2Nf;
          msExMd3Nf = rte2_msExMd3Nf;
          msASP1 = rte2_msASP1;
          msASP2 = rte2_msASP2;
          msASP3 = rte2_msASP3;
          msASPM = rte2_msASPM;
          msASPS = rte2_msASPS;
          msCCEFDT = rte2_msCCEFDT;
          msCCEXDT = rte2_msCCEXDT;
          msCXBL = rte2_msCXBL;
          msD1D2EFDT = rte2_msD1D2EFDT;
          msD1D2EXDT = rte2_msD1D2EXDT;
          msD1S2 = rte2_msD1S2;
          msD2D2EFDT = rte2_msD2D2EFDT;
          msD2D2EXDT = rte2_msD2D2EXDT;
          msD2S2 = rte2_msD2S2;
          msDBCR = rte2_msDBCR;
          msDCD2 = rte2_msDCD2;
          msDCDE = rte2_msDCDE;
          msDD1EFDT = rte2_msDD1EFDT;
          msDD1EXDT = rte2_msDD1EXDT;
          msDD2EFDT = rte2_msDD2EFDT;
          msDD2EXDT = rte2_msDD2EXDT;
          msDEV2 = rte2_msDEV2;
          msDISC = rte2_msDISC;
          msDV1EFDT = rte2_msDV1EFDT;
          msDV1EXDT = rte2_msDV1EXDT;
          msDV2EFDT = rte2_msDV2EFDT;
          msDV2EXDT = rte2_msDV2EXDT;
          msEM1DATE = rte2_msEM1DATE;
          msEM2DATE = rte2_msEM2DATE;
          msEM3DATE = rte2_msEM3DATE;
          msEXM1 = rte2_msEXM1;
          msEXM2 = rte2_msEXM2;
          msEXM3 = rte2_msEXM3;
          msFCHG = rte2_msFCHG;
          msIL1EFDT = rte2_msIL1EFDT;
          msIL1EXDT = rte2_msIL1EXDT;
          msIL2EFDT = rte2_msIL2EFDT;
          msIL2EXDT = rte2_msIL2EXDT;
          msILCD = rte2_msILCD;
          msINL2 = rte2_msINL2;
          msMCCD = rte2_msMCCD;
          msMCEFDT = rte2_msMCEFDT;
          msMCEXDT = rte2_msMCEXDT;
          msMCGP = rte2_msMCGP;
          msMFEE = rte2_msMFEE;
          msPROM = rte2_msPROM;
          msSCEFDT = rte2_msSCEFDT;
          msSCEXDT = rte2_msSCEXDT;
          msSDEFDT = rte2_msSDEFDT;
          msSDEXDT = rte2_msSDEXDT;
          msSFMN = rte2_msSFMN;
          msSFPC = rte2_msSFPC;
          msVLCA = rte2_msVLCA;
          msWMAX = rte2_msWMAX;
          msWMIN = rte2_msWMIN;
          msWVFL = rte2_msWVFL;
          //*** Beg Add *** 12/11/23 *** PHW114578 ***********************
          msBlnkWd = rte2_msBlnkWd;
          //*** End Add *** 12/11/23 *** PHW114578 ***********************
          msWVPC = rte2_msWVPC;
          msWVTP = rte2_msWVTP;
          msStFc = rte2_msStFc;
          msDev2F = rte2_msDev2F;
          //*** Beg Add *** 03/04/24 *************************************
          msExpRat = rte2_msExpRat;
          //*** End Add *** 03/04/24 *************************************

       Endif;

      /end-free
     C                   Endsr                                                  Move_Into_Scr Subr

      //===================================================================
      // Move_Into_Ds - Load the screen fields back into data structure fields.
      //===================================================================
     C     Move_Into_Ds  Begsr
      /free

       If inCall = 'L';
          bill_scPFDT = scPFDT;
          bill_scPTDT = scPTDT;
          bill_scANDT = scANDT;
          bill_scRTCD = scRTCD;
          bill_scBSTY = scBSTY;
          bill_scBSCD = scBSCD;
          bill_scBSDT = scBSDT;
          bill_scSTAT = scSTAT;
          bill_scBLFL = scBLFL;
          bill_scASCD = scASCD;
          bill_scPFIN = scPFIN;
          bill_scFFL1 = scFFL1;
          bill_scARSM = scARSM;
          bill_scIFFL = scIFFL;
          bill_scTYPB = scTYPB;
          bill_scQTYP = scQTYP;
          bill_scEMPY = scEMPY;
          bill_scOvrMP= scOvrMP;

          bill_erPFDT = erPFDT;
          bill_erPTDT = erPTDT;
          bill_erANDT = erANDT;
          bill_erRTCD = erRTCD;
          bill_erRTCD2 = erRTCD2;
          bill_erBSCD = erBSCD;
          bill_erBSDT = erBSDT;
          bill_erSTAT = erSTAT;
          bill_erBLFL = erBLFL;
          bill_erASCD = erASCD;
          bill_erPFIN = erPFIN;
          bill_erFFL1 = erFFL1;
          bill_erARSM = erARSM;
          bill_erIFFL = erIFFL;
          bill_erQTYP = erQTYP;
          bill_erTYPB = erTYPB;
          bill_erEMPY = erEMPY;

          bill_msPFDT = msPFDT;
          bill_msPTDT = msPTDT;
          bill_msANDT = msANDT;
          bill_msRTCD = msRTCD;
          bill_mgRTCD = mgRTCD;
          bill_msBSCD = msBSCD;
          bill_msBSDT = msBSDT;
          bill_msSTAT = msSTAT;
          bill_msBLFL = msBLFL;
          bill_msASCD = msASCD;
          bill_msPFIN = msPFIN;
          bill_msFFL1 = msFFL1;
          bill_msARSM = msARSM;
          bill_msIFFL = msIFFL;
          bill_mgTYPB = mgTYPB;
          bill_mgQTYP = mgQTYP;
          bill_msEMPY = msEMPY;

       Endif;

       If inCall = 'R' or inCall = 'P';

          If inCall = 'P';
             rte1_scPFDT = scPFDT;
             rte1_scPTDT = scPTDT;
             rte1_scANDT = scANDT;
             rte1_scARSM = scARSM;
             rte1_scASCD = scASCD;
             rte1_scBLFL = scBLFL;
             rte1_scBSCD = scBSCD;
             rte1_scBSDT = scBSDT;
             rte1_scBSTY = scBSTY;
             rte1_scEMPY = scEMPY;
             rte1_scFFL1 = scFFL1;
             rte1_scIFFL = scIFFL;
             rte1_scPFIN = scPFIN;
             rte1_scQTYP = scQTYP;
             rte1_scRTCD = scRTCD;
             rte1_scSTAT = scSTAT;
             rte1_scTYPB = scTYPB;
             rte1_scOvrMP= scOvrMP;

             rte1_erPFDT = erPFDT;
             rte1_erPTDT = erPTDT;
             rte1_erANDT = erANDT;
             rte1_erARSM = erARSM;
             rte1_erASCD = erASCD;
             rte1_erBLFL = erBLFL;
             rte1_erBSCD = erBSCD;
             rte1_erBSDT = erBSDT;
             rte1_erEMPY = erEMPY;
             rte1_erFFL1 = erFFL1;
             rte1_erIFFL = erIFFL;
             rte1_erPFIN = erPFIN;
             rte1_erQTYP = erQTYP;
             rte1_erRTCD = erRTCD;
             rte1_erRTCD2 = erRTCD2;
             rte1_erSTAT = erSTAT;
             rte1_erTYPB = erTYPB;

             rte1_msPFDT = msPFDT;
             rte1_msPTDT = msPTDT;
             rte1_msANDT = msANDT;
             rte1_msARSM = msARSM;
             rte1_msASCD = msASCD;
             rte1_msBLFL = msBLFL;
             rte1_msBSCD = msBSCD;
             rte1_msBSDT = msBSDT;
             rte1_msEMPY = msEMPY;
             rte1_msFFL1 = msFFL1;
             rte1_msIFFL = msIFFL;
             rte1_msPFIN = msPFIN;
             rte1_msQTYP = msQTYP;
             rte1_msRTCD = msRTCD;
             rte1_mgRTCD = mgRTCD;
             rte1_msSTAT = msSTAT;
             rte1_msTYPB = msTYPB;
          Endif;

          If inCall = 'R';
             rte2_sc2500 = sc2500;
             rte2_scCOFL = scCOFL;
             rte2_scFFL9 = scFFL9;
             rte2_scSRPR = scSRPR;

             rte2_er2500 = er2500;
             rte2_erCOFL = erCOFL;
             rte2_erFFL9 = erFFL9;
             rte2_erSRPR = erSRPR;

             rte2_ms2500 = ms2500;
             rte2_msCOFL = msCOFL;
             rte2_msFFL9 = msFFL9;
             rte2_msTSRPR = msTSRPR;
             rte2_scMANP = scMANP;
             rte2_erMANP = erMANP;
             rte2_msMANP = msMANP;
          Else;
             rte1_sc25002 = sc25002;
             rte1_scCOFL2 = scCOFL2;
             rte1_scFFL92 = scFFL92;
             rte1_scSRPR2 = scSRPR2;

             rte1_er25002 = er25002;
             rte1_erCOFL2 = erCOFL2;
             rte1_erFFL92 = erFFL92;
             rte1_erSRPR2 = erSRPR2;

             rte1_ms25002 = ms25002;
             rte1_msCOFL2 = msCOFL2;
             rte1_msFFL92 = msFFL92;
             rte1_msSRPR2 = msSRPR2;
             rte1_scMANP2 = scMANP2;
             rte1_erMANP2 = erMANP2;
             rte1_msMANP2 = msMANP2;
          Endif;

          rte2_scARCA     = scARCA;
          rte2_scARCP     = scARCP;
          rte2_scAREFDT   = scAREFDT;
          rte2_scAREXDT   = scAREXDT;
          rte2_scARP1     = scARP1;
          rte2_scARP2     = scARP2;
          rte2_scARP3     = scARP3;
          rte2_scExMd1Nf  = scExMd1Nf;
          rte2_scExMd2Nf  = scExMd2Nf;
          rte2_scExMd3Nf  = scExMd3Nf;
          rte2_scASP1     = scASP1;
          rte2_scASP2     = scASP2;
          rte2_scASP3     = scASP3;
          rte2_scASPM     = scASPM;
          rte2_scASPS     = scASPS;
          rte2_scCCEFDT   = scCCEFDT;
          rte2_scCCEXDT   = scCCEXDT;
          rte2_scCXBL     = scCXBL;
          rte2_scD1D2EFDT = scD1D2EFDT;
          rte2_scD1D2EXDT = scD1D2EXDT;
          rte2_scD1S2     = scD1S2;
          rte2_scD2D2EFDT = scD2D2EFDT;
          rte2_scD2D2EXDT = scD2D2EXDT;
          rte2_scD2S2     = scD2S2;
          rte2_scDBCR     = scDBCR;
          rte2_scDCD2     = scDCD2;
          rte2_scDCDE     = scDCDE;
          rte2_scDD1EFDT  = scDD1EFDT;
          rte2_scDD1EXDT  = scDD1EXDT;
          rte2_scDD2EFDT  = scDD2EFDT;
          rte2_scDD2EXDT  = scDD2EXDT;
          rte2_scDEV1H    = scDEV1H;
          rte2_scDEV2     = scDEV2;
          rte2_scDEV2H    = scDEV2H;
          rte2_scDISC     = scDISC;
          rte2_scDISP     = scDISP;
          rte2_scDV1EFDT  = scDV1EFDT;
          rte2_scDV1EXDT  = scDV1EXDT;
          rte2_scDV2EFDT  = scDV2EFDT;
          rte2_scDV2EXDT  = scDV2EXDT;
          rte2_scEM1DATE  = scEM1DATE;
          rte2_scEM2DATE  = scEM2DATE;
          rte2_scEM3DATE  = scEM3DATE;
          rte2_scEXM1     = scEXM1;
          rte2_scEXM2     = scEXM2;
          //*** begin add ***  03/18/25  **************************************
          rte2_scEXM3     = scEXM3;
          //*** end add *****  03/18/25  **************************************
          If scExM1 = 0 and scExM2 <> 0;
             rte2_scNoExM1 = 'Y';
             rte2_scEM1Date = dtMDYY(ELpfdt);
          Else;
             rte2_scNoExM1 = *Blank;
             If scExM1 = 0 and scExM2 = 0;
                rte2_scEM1Date = *zero;
             Endif;
          Endif;
          //*** Begin Add *** 07/30/23 *************************************
          // Set not rated for experience modifier 2
          If scExM2 = 0 And scEM2Date <> 0 And scExM1 <> 0;
             rte2_scNoExM2 = 'Y';
          Else;
             rte2_scNoExM2 = *Blank;
          Endif;
          // Set not rated for experience modifier 2
          If scExM3 = 0 And scEM3Date <> 0 And scExM2 <> 0;
             rte2_scNoExM3 = 'Y';
          Else;
             rte2_scNoExM3 = *Blank;
          Endif;
          //*** End   Add *** 07/30/23 *************************************
          //*** begin del ***  03/18/25  **************************************
          //*****rte2_scEXM3     = scEXM3;
          //*** end del *****  03/18/25  **************************************
          rte2_scFCHG     = scFCHG;
          rte2_scIL1EFDT  = scIL1EFDT;
          rte2_scIL1EXDT  = scIL1EXDT;
          rte2_scIL2EFDT  = scIL2EFDT;
          rte2_scIL2EXDT  = scIL2EXDT;
          rte2_scILCD     = scILCD;
          rte2_scINL2     = scINL2;
          rte2_scMCCD     = scMCCD;
          rte2_scMCEFDT   = scMCEFDT;
          rte2_scMCEXDT   = scMCEXDT;
          rte2_scMCGP     = scMCGP;
          rte2_scMFEE     = scMFEE;
          rte2_scPROM     = scPROM;
          rte2_scSBJ1AH   = scSBJ1AH;
          rte2_scSBJ1BH   = scSBJ1BH;
          rte2_scContCrdH = scContCrdH;
          rte2_scSCEFDT   = scSCEFDT;
          rte2_scSCEXDT   = scSCEXDT;
          rte2_scSDEFDT   = scSDEFDT;
          rte2_scSDEXDT   = scSDEXDT;
          rte2_scSFHD     = scSFHD;
          rte2_scSFMN     = scSFMN;
          rte2_scSFPC     = scSFPC;
          rte2_scVLCA     = scVLCA;
          rte2_scVLCH     = scVLCH;
          rte2_scWMAX     = scWMAX;
          rte2_scWMIN     = scWMIN;
          rte2_scWVFL     = scWVFL;
          //*** Beg Add *** 12/11/23 *** PHW114578 ***********************
          rte2_scOVRWD    = scOVRWD;
          rte2_scBlnkWd   = scBlnkWd;
          //*** End Add *** 12/11/23 *** PHW114578 ***********************
          rte2_scWVPC     = scWVPC;
          rte2_scWVTP     = scWVTP;
          rte2_scStfc     = scStfc;
          rte2_scDev2F    = scDev2F;
          //*** Beg Add *** 03/04/24 ***************************************
          rte2_scExpRat   = scExpRat;
          //*** End Add *** 03/04/24 ***************************************

          rte2_erARCA     = erARCA;
          rte2_erARCP     = erARCP;
          rte2_erAREFDT   = erAREFDT;
          rte2_erAREXDT   = erAREXDT;
          rte2_erARP1     = erARP1;
          rte2_erARP2     = erARP2;
          rte2_erARP3     = erARP3;
          rte2_erExMd1Nf =  erExMd1Nf;
          rte2_erExMd2Nf =  erExMd2Nf;
          rte2_erExMd3Nf =  erExMd3Nf;

          rte2_erASP1     = erASP1;
          rte2_erASP2     = erASP2;
          rte2_erASP3     = erASP3;
          rte2_erASPM     = erASPM;
          rte2_erASPS     = erASPS;
          rte2_erCCEFDT   = erCCEFDT;
          rte2_erCCEXDT   = erCCEXDT;
          rte2_erCXBL     = erCXBL;
          rte2_erD1D2EFDT = erD1D2EFDT;
          rte2_erD1D2EXDT = erD1D2EXDT;
          rte2_erD1S2     = erD1S2;
          rte2_erD2D2EFDT = erD2D2EFDT;
          rte2_erD2D2EXDT = erD2D2EXDT;
          rte2_erD2S2     = erD2S2;
          rte2_erDBCR     = erDBCR;
          rte2_erDCD2     = erDCD2;
          rte2_erDCDE     = erDCDE;
          rte2_erDD1EFDT  = erDD1EFDT;
          rte2_erDD1EXDT  = erDD1EXDT;
          rte2_erDD2EFDT  = erDD2EFDT;
          rte2_erDD2EXDT  = erDD2EXDT;
          rte2_erDEV2     = erDEV2;
          rte2_erDISC     = erDISC;
          rte2_erDV1EFDT  = erDV1EFDT;
          rte2_erDV1EXDT  = erDV1EXDT;
          rte2_erDV2EFDT  = erDV2EFDT;
          rte2_erDV2EXDT  = erDV2EXDT;
          rte2_erEM1DATE  = erEM1DATE;
          rte2_erEM2DATE  = erEM2DATE;
          rte2_erEM3DATE  = erEM3DATE;
          rte2_erEXM1     = erEXM1;
          rte2_erEXM2     = erEXM2;
          rte2_erEXM3     = erEXM3;
          rte2_erFCHG     = erFCHG;
          rte2_erIL1EFDT  = erIL1EFDT;
          rte2_erIL1EXDT  = erIL1EXDT;
          rte2_erIL2EFDT  = erIL2EFDT;
          rte2_erIL2EXDT  = erIL2EXDT;
          rte2_erILCD     = erILCD;
          rte2_erINL2     = erINL2;
          rte2_erMCCD     = erMCCD;
          rte2_erMCEFDT   = erMCEFDT;
          rte2_erMCEXDT   = erMCEXDT;
          rte2_erMCGP     = erMCGP;
          rte2_erMFEE     = erMFEE;
          rte2_erPROM     = erPROM;
          rte2_erSCEFDT   = erSCEFDT;
          rte2_erSCEXDT   = erSCEXDT;
          rte2_erSDEFDT   = erSDEFDT;
          rte2_erSDEXDT   = erSDEXDT;
          rte2_erSFMN     = erSFMN;
          rte2_erSFPC     = erSFPC;
          rte2_erVLCA     = erVLCA;
          rte2_erWMAX     = erWMAX;
          rte2_erWMIN     = erWMIN;
          rte2_erWVFL     = erWVFL;
          //*** Beg Add *** 12/11/23 *** PHW114578 ***********************
          rte2_erBlnkWd   = erBlnkWd;
          //*** End Add *** 12/11/23 *** PHW114578 ***********************
          rte2_erWVPC     = erWVPC;
          rte2_erWVTP     = erWVTP;
          rte2_erNoWvrDsc = erNoWvrDsc;
          //*** Beg Add *** 03/04/24 ***************************************
          rte2_erExpRat   = erExpRat;
          //*** End Add *** 03/04/24 ***************************************
          rte2_msNoWvrDsc = msNoWvrDsc;
          rte2_msARCA     = msARCA;
          rte2_msARCP     = msARCP;
          rte2_msAREFDT   = msAREFDT;
          rte2_msAREXDT   = msAREXDT;
          rte2_msARP1     = msARP1;
          rte2_msARP2     = msARP2;
          rte2_msARP3     = msARP3;
          rte2_msExMd1Nf =  msExMd1Nf;
          rte2_msExMd2Nf =  msExMd2Nf;
          rte2_msExMd3Nf =  msExMd3Nf;
          rte2_msASP1     = msASP1;
          rte2_msASP2     = msASP2;
          rte2_msASP3     = msASP3;
          rte2_msASPM     = msASPM;
          rte2_msASPS     = msASPS;
          rte2_msCCEFDT   = msCCEFDT;
          rte2_msCCEXDT   = msCCEXDT;
          rte2_msCXBL     = msCXBL;
          rte2_msD1D2EFDT = msD1D2EFDT;
          rte2_msD1D2EXDT = msD1D2EXDT;
          rte2_msD1S2     = msD1S2;
          rte2_msD2D2EFDT = msD2D2EFDT;
          rte2_msD2D2EXDT = msD2D2EXDT;
          rte2_msD2S2     = msD2S2;
          rte2_msDBCR     = msDBCR;
          rte2_msDCD2     = msDCD2;
          rte2_msDCDE     = msDCDE;
          rte2_msDD1EFDT  = msDD1EFDT;
          rte2_msDD1EXDT  = msDD1EXDT;
          rte2_msDD2EFDT  = msDD2EFDT;
          rte2_msDD2EXDT  = msDD2EXDT;
          rte2_msDEV2     = msDEV2;
          rte2_msDISC     = msDISC;
          rte2_msDV1EFDT  = msDV1EFDT;
          rte2_msDV1EXDT  = msDV1EXDT;
          rte2_msDV2EFDT  = msDV2EFDT;
          rte2_msDV2EXDT  = msDV2EXDT;
          rte2_msEM1DATE  = msEM1DATE;
          rte2_msEM2DATE  = msEM2DATE;
          rte2_msEM3DATE  = msEM3DATE;
          rte2_msEXM1     = msEXM1;
          rte2_msEXM2     = msEXM2;
          rte2_msEXM3     = msEXM3;
          rte2_msFCHG     = msFCHG;
          rte2_msIL1EFDT  = msIL1EFDT;
          rte2_msIL1EXDT  = msIL1EXDT;
          rte2_msIL2EFDT  = msIL2EFDT;
          rte2_msIL2EXDT  = msIL2EXDT;
          rte2_msILCD     = msILCD;
          rte2_msINL2     = msINL2;
          rte2_msMCCD     = msMCCD;
          rte2_msMCEFDT   = msMCEFDT;
          rte2_msMCEXDT   = msMCEXDT;
          rte2_msMCGP     = msMCGP;
          rte2_msMFEE     = msMFEE;
          rte2_msPROM     = msPROM;
          rte2_msSCEFDT   = msSCEFDT;
          rte2_msSCEXDT   = msSCEXDT;
          rte2_msSDEFDT   = msSDEFDT;
          rte2_msSDEXDT   = msSDEXDT;
          rte2_msSFMN     = msSFMN;
          rte2_msSFPC     = msSFPC;
          rte2_msVLCA     = msVLCA;
          rte2_msWMAX     = msWMAX;
          rte2_msWMIN     = msWMIN;
          rte2_msWVFL     = msWVFL;
          //*** Beg Add *** 12/11/23 *** PHW114578 ***********************
          rte2_msBlnkWd   = msBlnkWd;
          //*** Beg Add *** 12/11/23 *** PHW114578 ***********************
          rte2_msWVPC     = msWVPC;
          rte2_msWVTP     = msWVTP;
          rte2_msStFc     = msStFc;
          rte2_msDev2F    = msDev2F;
          //*** Beg Add *** 03/04/24 ***************************************
          rte2_msExpRat   = msExpRat;
          //*** End Add *** 03/04/24 ***************************************

       Endif;

      /end-free
     C                   Endsr                                                  Move_Into_Ds Subr
     C**************************************************************
     C*  Rating Edits
     C**************************************************************
     C     EditRating    Begsr
     C*
      /Free
       xsKey = 'WC';
       Chain xsKey SWC01;
      /End-Free
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
     C*  EDIT POLICY FROM DATE
     C*
     C                   If        inCall = 'P' or inCall = 'L'
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scPFDT
     C*
     C     SRERR         IFEQ      *ON
     c     erPFDT        andeq     *OFF
     C                   move      *on           error
     C                   move      *on           erPFDT
     C                   movel(p)  'WCR0500'     msPFDT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  POLICY FROM DATE CANNOT BE LESS THAN INITIAL EFFECTIVE DATE
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPFDT            7 0
     C*
     C     xxPFDT        IFLT      EMEFD1
     c     erPFDT        andeq     *OFF
     C                   move      *on           error
     C                   move      *on           erPFDT
     C                   movel(p)  'WCR0609'     msPFDT
     C                   ENDIF                                                  XXPF<EMEFD1
     C*
     C*  POLICY FROM DATE MUST BE LESS THAN OR EQUAL TO ALL EXISTING
     C*  PAYROLL RECORDS
     C*
     c     erPFDT        ifeq      *OFF
     C*
     C     keyWMEPL      Setll     PEP00
     C     keyWMEPL      Reade     PEP00
     C*
     C                   dow       not %EOF
     C*
     C     EPEFDT        IFLT      xxPFDT
     C     EPSTAT        ANDNE     'I'
     C                   move      *on           error
     C                   move      *on           erPFDT
     C                   movel(p)  'WCR0811'     msPFDT
     c                   leave
     C                   ENDIF                                                  EPEFDT<XXPF/EPSTAT<>
     C*
     C     keyWMEPL      Reade     PEP00
     C*
     C                   ENDDO                                                  dow not %EOF
     C*
     C                   ENDIF
     C*
     C*  EDIT POLICY TO DATE
     C*
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scPTDT
     C*
     C     SRERR         IFEQ      *ON
     c     erPTDT        andeq     *OFF
     C                   move      *on           error
     C                   move      *on           erPTDT
     C                   movel(p)  'WCR0500'     msPTDT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDAT1            7 0
     C*
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDAT2            7 0
     C*
     C     xxDAT2        IFLT      xxDAT1
     c     erPFDT        andeq     *OFF
     c     erPTDT        andeq     *OFF
     C                   move      *on           error
     C                   move      *on           erPFDT
     C                   move      *on           erPTDT
     C                   movel(p)  'WCR0529'     msPFDT
     C                   movel(p)  'WCR0529'     msPTDT
     C                   ENDIF                                                  XXDAT2<XXDAT1
     C*
     C*  BOTH DATES MUST BE BETWEEN FUND POLICY START & END DATES
     C*
     c     erPFDT        ifeq      *OFF
     c     erPTDT        andeq     *OFF
     C*
     C     FNCGFL        IFNE      'Y'
     C*
     C     xxDAT1        IFLT      F1STPO
     C     xxDAT1        ORGE      F1ENPO
     C     xxDAT2        ORLT      F1STPO
     C     xxDAT2        ORGT      F1ENPO
     C                   move      *on           error
     C                   move      *on           erPFDT
     C                   move      *on           erPTDT
     C                   movel(p)  'WCR0723'     msPFDT
     C                   movel(p)  'WCR0723'     msPTDT
     C                   ENDIF                                                  XXDAT1<F1STPO
     C                   ELSE
     C     EMSTRP        IFEQ      'Y'
     C     xxDAT1        IFLT      F1STPO
     C     xxDAT1        ORGE      F1ENPO
     C                   move      *on           error
     C                   move      *on           erPFDT
     C                   movel(p)  'WCR0723'     msPFDT
     C                   ENDIF                                                  XXDAT1<F1STPO
     C                   ENDIF                                                  EMSTRP = Y
     C*
     C                   ENDIF                                                  FNCGFL<>Y
     C*
     C                   ENDIF                                                  erPTDT ifeq
     C*
     C* DATES CANNOT OVERLAP DATE RANGES FOR THAT EMPLOYER
     C*
     C     key1WDELL     Setgt     PEL00
     C     key1WDELL     Readpe(n) PEL00
     C*
     C                   DOW       not %EOF
     C*
     C     ELFYR         IFNE      scFYR
     C*
     C     xxDAT1        IFGE      ELPFDT
     C     xxDAT1        ANDLT     ELPTDT
     c     erPFDT        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erPFDT
     C                   movel(p)  'WC30673'     msPFDT
     C                   ENDIF                                                  ELPFDT
     C*
     C     xxDAT2        IFGT      ELPFDT
     C     xxDAT2        ANDLT     ELPTDT
     c     erPTDT        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erPTDT
     C                   movel(p)  'WC30673'     msPTDT
     C                   ENDIF                                                  ELPTDT
     C*
     C                   ENDIF                                                  ELFYR
     C*
     C     key1WDELL     Readpe(n) PEL00
     C                   ENDDO                                                  dow not %EOF
     C*
     C                   MOVE      *OFF          dtLESS
     C                   MOVE      ' '           xxSTAR
     C                   MOVE      ' '           xxOTAR
     C                   Z-ADD     *ZEROS        xxSRAD
     C*
     C*  POLICY END DATE CANNOT BE > POLICY END DATE OF THE STATE
     C*   REPORTING DIVISION  OR < = Policy FROM Date (01/17/02)
      *
      *   if not claims only client
      *
     C     WCFL08        IFNE      'C'
      *
     C     key1WMEML     CHAIN     PEM00                              60
      *
     C     *IN60         DOWEQ     *OFF
     C*
     C     key2WDELL     CHAIN(N)  PEL00                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDNE     'Y'
     C     ELARSM        ANDEQ     'Y'
     C                   MOVE      'Y'           xxOTAR
     C                   ENDIF
     C*
     C     EMSTRP        IFEQ      'Y'
     C*
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         xxSRAD
     C*
     C     EMDIV         IFNE      scDIV
     C*
     C     key2WDELL     CHAIN(N)  PEL00                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     ELARSM        ANDEQ     'Y'
     C                   MOVE      ELBSCD        svBscd
     C                   MOVE      ELBSDT        svBsdt
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     ELARSM        ANDEQ     'Y'
     C                   MOVE      'Y'           xxSTAR            1
     C                   ENDIF
     C*
     C     XXDAT1        IFNE      XXDAT2
     C     xxDAT2        IFGT      ELPTDT                                        > TO DATE
     C     xxDAT2        ORLE      ELPFDT                                        < FROM DATE
     c     erPTDT        IFEQ      *OFF
     C                   move      *on           error
     C                   move      *on           erPTDT
     C                   movel(p)  'WCR3153'     msPTDT
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF                                                  XXDAT2>ELPTDT
     C                   ENDIF
     C*
     C                   MOVE      *OFF          dtLESS
     C     xxDAT2        IFLT      ELPTDT
     C                   MOVE      *ON           dtLESS            1
     C                   ENDIF                                                  XXDAT2<ELPTDT
     C*
     C                   ENDIF                                                  EMDIV<>SCDIV
     C*
     C                   ENDIF                                                  EMSTRP=Y
     C*
     C     key1WMEML     Reade     PEM00                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  WCFL08
     C*
     C     keyWMEML      chain     pem00
     C     keyWDELL      chain(n)  pel00
     C                   MOVE      ' '           xxSTRP
     C                   If        %found
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      'Y'           xxSTRP
     C                   ELSE
     C                   MOVE      ' '           xxSTRP
     C                   ENDIF
     C                   Endif
     C*
     C*  TO DATE MUST EQUAL CANCEL DATE IN EMPLOYER MASTER FOR EACH DIV
     C*
     C     dtLESS        IFEQ      *ON
     c     erPTDT        ANDEQ     *OFF
     C*
     C     keyWMEML      CHAIN     PEM00                              60
     C*
     C     xxDAT2        IFNE      EMCAD1
     C     xxDAT2        ANDNE     EMCAD2
     C     xxDAT2        ANDNE     EMCAD3
     C     xxDAT2        ANDNE     EMCAD4
     C     xxDAT2        ANDNE     EMCAD5
     C                   move      *on           error
     C                   move      *on           erPTDT
     C                   movel(p)  'WCR1282'     msPTDT
     C                   ENDIF                                                  XXDAT2<>EMCAD1/2/3/4
     C*
     C                   ENDIF                                                  DTLES=*ON
     C*
     C*  POLICY END DATE CANNOT BE MORE THAN 365(366) DAYS PAST POLICY
     C*   START DATE
     C*
      /free
       If erPTDT = *off;
          erPTDT = PolMaxLength(scPFDT:scPTDT:scCo#:scFnd:message);
          If erPTDT = *on;
             msPTDT = message;
             error  = *on;
          Endif;
       Endif;
      /end-free
     C*
     C*  EDIT ANNIVERSARY RATING DATE
     C*
     C     scANDT        IFNE      *ZERO
     C*
     C                   Z-ADD     scANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scANDT
     C*
     C     SRERR         IFEQ      *ON
     c     erANDT        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erANDT
     C                   movel(p)  'WCR0500'     msANDT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCANDT<>*ZERO
     C*
     C*  ANNIV DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C*
     C                   Z-ADD     scANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxANDT            7 0
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPFDT
     C*
     C     xxANDT        IFGT      xxPFDT
     c     erANDT        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erANDT
     C                   movel(p)  'WCR0577'     msANDT
     C                   ENDIF                                                  XXANDT>XXPF
     C*
     C* Set effective date for table file chains.
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDate
     C*
     C*  DO NOT PERFORM THESE EDIT IF CLAIMS ONLY GROUP
     C*
     C     F1CLMS        IFNE      'Y'
     C*
     C*  VALIDATE THE A/R SUMMARY FLAG
     C*
     C     scARSM        IFNE      'Y'
     C     scARSM        ANDNE     ' '
     c     erARSM        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erARSM
     C                   movel(p)  'WCR0571'     msARSM
     C                   ENDIF                                                  SCARSM IFNE
     C*
     C*  DO NOT ALLOW SUMMARY IF STATE RPT DIV IS NOT SUMMARY
     C*
     C     scARSM        IFEQ      'Y'
     C     xxSTAR        ANDNE     'Y'
     C     xxSTRP        ANDNE     'Y'
     c     erARSM        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erARSM
     C                   movel(p)  'WC30198'     msARSM
     C                   ENDIF                                                  SCARSM IFEQ
     C*
     C*  DO NOT ALLOW STATE RPT TO BE CHANGED IF OTHERS ARE SUMMARY
     C*
     C     scARSM        IFEQ      ' '
     C     xxOTAR        ANDEQ     'Y'
     C     xxSTRP        ANDEQ     'Y'
     c     erARSM        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erARSM
     C                   movel(p)  'WC30199'     msARSM
     C                   ENDIF                                                  SCARSM IFEQ
      *
      * # of Employees cannot be less than or equal to zero.
      *
     C     clientID      IFNE      '093'
     C                   IF        scEMPY <= 0
     C                             and erEMPY <> *ON
     C                             and WCFL14 = 'Y'
     C                   move      *on           error
     C                   move      *on           erEMPY
     C                   movel(p)  'WCR0502'     msEMPY
     C                   ENDIF
     C                   ENDIF
     C*
     C*  BILLING SCHEDULE CODE AND START DATE MUST EQUAL STATE RPT DIV
     C*  IF SUMMARY AND EFFECTIVE DATE IS THE SAME.
     C*
     C     scARSM        IFEQ      'Y'
     C     xxSTRP        ANDNE     'Y'
     C     xxSTAR        ANDEQ     'Y'
     C     erBSCD        ANDNE     *ON
     C     erBSDT        ANDNE     *ON
     C*
     C                   Z-ADD     scBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scBSDT
     C*
     C                   Z-ADD     svBsdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         xxBsdt            8 0
     C                   MOVE      svBscd        xxBscd            3 0
     C*
     C     scBSCD        IFNE      xxBscd
     C     scBSDT        ORNE      xxBsdt
     C                   move      *on           error
     C                   move      *on           erBSCD
     C                   move      *on           erBSDT
     C                   movel(p)  'WC30200'     msBSCD
     C                   movel(p)  'WC30200'     msBSDT
     C                   ENDIF                                                  SCBSCD IFNE
     C*
     C                   ENDIF                                                  SCARSM IFEQ

       // Do not let the installment plan change from estimated to self-reporting
       // if the Flat Dividend Deferred checkbox is checked.
      /free
       If ValidDivClient and ELdvdf = 'Y' and chgMode = *On and
        scBSCD <> ELbscd;
          xxkey = %editc(scBSCD:'X');
          Chain keyWTABL PBS01;
          If BSSefl <> 'E';
             error = *On;
             erBSCD = *On;
             msBSCD = 'WC40148';
          Endif;
       Endif;

       // Prevent the assignment of a Weekly S/R Billing Plan if any monthly reports
       // exists on the policy/div.  Vice versa, prevent a Monthly S/R Plan from being
       // assigned if any Weekly Reports exist.
       //*****
       If chgMode = *On and scBSCD <> ELbscd;
          xxkey = %editc(scBSCD:'X');
          Chain keyWTABL PBS01;
          If %found;
             x2Sefl = BSSefl;
             x2Freq = BSFreq;
          EndIf;

          If x2Sefl = 'S' and (x2Freq = ' ' or x2Freq = 'M');
             Setll keyWASRLQ PSR01;
             ReadE keyWASRLQ PSR01;
             Dow not %eof(WASRLQ);
                if SRTODT <> *zero;
                   error  = *On;
                   erBSCD = *On;
                   msBSCD = 'WC40166';
                   Leave;
                EndIf;
                ReadE keyWASRLQ PSR01;
             EndDo;
          EndIf;

          If x2Sefl = 'S' and  x2Freq = 'W';
             Setll keyWASRLQ PSR01;
             ReadE keyWASRLQ PSR01;
             Dow not %eof(WASRLQ);
                if SRRPMY <> *zero;
                   error  = *On;
                   erBSCD = *On;
                   msBSCD = 'WC40167';
                   Leave;
                EndIf;
                ReadE keyWASRLQ PSR01;
            EndDo;
          EndIf;
       EndIf;

       // prevent the policy from being changed off of Large Deductible
       // if Collateral records exist.
       //***
       xxkey = %editc(scBSCD:'X');
       Chain keyWTABL PBS01;
       If %found;
          x2LPGM = BSLPGM;
       EndIf;

       If FNPEOF <> ' ';
          Chain (scco#:scfnd:1) pco00;
       Else;
          Chain (scco#:scfnd:scemp#) pco00;
       Endif;

       If  %found(WMCOL) and x2LPGM <> 'L';
         error  = *On;
         erBSCD = *On;
         msBSCD = 'WC40168';
       Endif;

       Chain (scCo#:%editc(scBsCd:'X')) pbs01;
       Setgt (scCo#:scFnd:scEmp#:DtCYMD(scPfDt)) pco01;
       Readpe (scCo#:scFnd:scEmp#) pco01;
       If not %eof and
        (COexDt = *zero or CvtToDate(COexDt) > CvtToDate(scPfDt));
          msBsCd = 'WCR4826';
          erBsCd = *on;
          error = *On;
       Endif;
      /end-free

     C     scIFFL        IFNE      'Y'
     C     scIFFL        ANDNE     *BLANKS
     c     erIFFL        ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erIFFL
     C                   movel(p)  'WCR0571'     msIFFL
     C                   ENDIF                                                  SCIFFL<>Y/
     C*
     C*  VALIDATE THE POLICY TYPE
     C*
     C                   if        xxRQD6 = 'Y' and scQTYP = *BLANKS
     C                             and erQTYP <> *ON
     C                   move      *on           error
     C                   move      *on           erQTYP
     C                   if        inCall = 'L'
     C                   movel(p)  'WCR0974'     mgQTYP
     C                   Else
     C                   movel(p)  'WCR0974'     msQTYP
     C                   endif
     C                   endif
     C*
     C* Validate policy type field.
     C*
     C     scQTYP        IFNE      *BLANKS
     C     FHF6TB        ANDNE     *Blanks
     c     flex          andeq     'Y'
     c     scQTYP        ORNE      *Blanks
     c     flex          andne     'Y'
     c     F1CLMS        ANDEQ     'N'
     C*
     C     FHF6TB        IFEQ      *BLANKS
     C                   MOVEL(P)  'PTYP'        xxTBID
     C                   ELSE
     C                   MOVEL(P)  FHF6TB        xxTBID
     C                   ENDIF
     C                   MOVEL(P)  SCQTYP        xxKEY
     C*
     C     keyWTGTL      CHAIN     PGT00                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C*
     c     erQTYP        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erQTYP
     C                   if        inCall = 'L'
     C                   movel(p)  'WCR0513'     mgQTYP
     C                   Else
     C                   movel(p)  'WCR0513'     msQTYP
     C                   endif
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*ON/MASTAT<>A
     C*
     C                   ENDIF                                                  SCQTYP IFNE
     C*
     C*  THE BILLING SCHEDULE CODE
     C*
     C                   MOVEL(P)  scBSCD        xxKEY
     C*
     C     keyWTABL      CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     BSSTAT        ORNE      'A'
     C*
     c     erBSCD        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erBSCD
     C                   movel(p)  'WCR0513'     msBSCD
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60=*ON/BSSTAT<>A
      *
      * Billings must not be posted after policy effective date
      *
     c     erBSCD        IFNE      *ON
     C                   EXSR      SRBILL
     C                   ENDIF
     C*
     C*  EDIT BILLING START DATE
     C*
     C                   Z-ADD     scBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scBSDT
     C*
     C     SRERR         IFEQ      *ON
     c     erBSDT        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erBSDT
     C                   movel(p)  'WCR0500'     msBSDT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C* BILLING START DATE CANNOT BE GREATER THAN POLICY EFFECTIVE DATE
     C*
     C                   Z-ADD     scBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDT1             7 0
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDT2             7 0
     C*
     C     xxDT1         IFGT      xxDT2
     c     erBSDT        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erBSDT
     C                   movel(p)  'WC30136'     msBSDT
     C                   ENDIF
     C*
     C* DO NOT ALLOW BILLING START DATE TO BE PRIOR TO CURRENT DATE IF
     C* WE ARE CHANGING THE VALUE OF THE FIELD OR ADDING A RECORD.
     C* UNLESS THE CHANGE IS THE BILLING START DATE TO = POLICY FROM DATE
     C*
     C     addMode       IFEQ      *OFF
     C     xxDT1         ANDNE     ELBSDT
     C*
     C     xxDT1         IFLT      sysDt7
     C     xxDT1         ANDNE     xxDT2
     C     SCARSM        ANDNE     'Y'
     c     erBSDT        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erBSDT
     C                   movel(p)  'WCR0592'     msBSDT
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C****  Verify ACH setup if online s/r and billing schedule code changes.

       // Change so that EFT or 3rd Party setup is always verified.
       // The setup must valid as of the policy effective date or
       //  as of the current date if the policy effective date is
       //  in the past.

       If BSonli = 'Y' And erBSCD <> *On;
       //*** begin chg ***  04/10/23  ***************************************
       //***   q3Co# = %Editc(ELco#:'X');
       //***   q3Fnd = %Editc(ELfnd:'X');
       //***   q3Emp# = %Editc(ELemp#:'X');
       //***   q3Div = %Editc(ELdiv:'X');
       //***   q3Fyr = %Editc(ELfyr:'X');
       //***   q3Mode = '1';
       //***   q3Err = *Blank;
       //***   q3Data = %Editc(scBSCD:'X');
       //***   q3Env = inQuote;
       //***   If %Date(ELpfdt:*Cymd) > %Date();
       //***      q3Efdt = %Char(%Date(ELpfdt:*Cymd):*Usa0);
       //***   Else;
       //***      q3Efdt = %Char(%Date():*Usa0);
       //***   Endif;
          q3Co# = %Editc(scCo#:'X');
          q3Fnd = %Editc(scFnd:'X');
          q3Emp# = %Editc(scEmp#:'X');
          q3Div = %Editc(scDiv:'X');
          q3Fyr = %Editc(scFyr:'X');
          q3Mode = '1';
          q3Err = *Blank;
          q3Data = %Editc(scBSCD:'X');
          q3Env = inQuote;
          If %Date(DtCymd(scPfdt):*Cymd) > %Date();
             q3Efdt = %Char(%Date(DtCymd(scPfdt):*Cymd):*Usa0);
          Else;
             q3Efdt = %Char(%Date():*Usa0);
          Endif;
       //*** end chg *****  04/10/23  ***************************************
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
     C                   PARM                    Q3PAYTYPE         1
     C                   PARM                    Q3ENV             1
     c*
       Select;
       When q3Err = '0';  // Edits Passed
       When q3Err = '1';  // No EFT or 3rd Party Pay As You Go
       // Soft edit no longer needed since EFT can be assigned in Quote.
          error  = *On;
          erBsCd = *On;
          msBsCd = 'WC30391';
       When q3Err = '2';  // Not Online Self Reporting
          error  = *On;
          erBsCd = *On;
          msBsCd = 'WC40174';
       When q3Err = '3';  // Not Self Reporting
          error  = *On;
          erBsCd = *On;
          msBsCd = 'WC40173';
       // Removed because SRONLI was changed and
       //  does not return an error code of '4'.
       // It returns a '1' if no EFT or 3rd Party
       //  setup is valid.
       When q3Err = 'E';  // Miscellaneous Error
          error  = *On;
          erBsCd = *On;
          msBsCd = 'WCR0513';
       EndSl;
     c*
     c                   endif                                                  if bsonli =
     c*
     C*
     C*  RESET RATE SET CODE CHANGE INDICATOR
     C*
     C                   MOVE      'N'           xxRCCG            1
     C*
     C* Rate Set Code must be entered.
     C*
     C     scRTCD        IFEQ      *blanks
     c     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WCR0503'     msRTCD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  VERIFY THAT THE RATE SET CODE EXISTS IN THE PAYROLL CLASS FILE
     C*
     C     key4WMPRL1    CHAIN     PPR01                              60
     C*
     C     *IN60         IFEQ      *ON
     c     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'W400048'     msRTCD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  CHAIN TO STATISTICAL RPT/RATING FILE TO FIND STATE INFO
     C*  IF THERE IS NO POLICY INFO FOR THAT STATE, GIVE ERROR
     C*  MESSAGE.
     C*
     C                   CLEAR                   xxST
     C     keyWMRSL      CHAIN     PRS00                              60
     C  N60              MOVEL     RSJRST        xxST
     C*
     C     keyWDRRL      SETGT     PRR00
     C     key1WDRRL     READPE    PRR00                                  60
     C*
     C     *IN60         IFEQ      *ON
     c     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WCR1400'     msRTCD
     C                   ENDIF
     C*
     C*  IF THE RATE SET CODE HAS BEEN CHANGED, THEN RELOAD THE STOCK
     C*  DISCOUNT DEFAULTS
     C*
     C                   If        inCall = 'P'
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ELRTCD        IFNE      scRTCD
     C     xxSVST        ANDNE     xxST
     C     ELASP3        ANDEQ     *ZEROS
     C*
     C                   MOVE      'Y'           xxRCCG
     C                   MOVE      *BLANKS       scDISC
     C                   Z-ADD     *ZEROS        scASP3
     C*
     C     RRDISC        IFNE      *BLANKS
     C                   MOVEL     RRDISC        scDISC
     C                   ELSE
     C                   Z-ADD     RRDSPC        scASP3
     C                   ENDIF                                                  RRDISC<>*BLANKS
     C*
     C                   ENDIF                                                  ELRTCD<>SCRTCD
     C*
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   Endif                                                  If inCall = 'P'
      *
     C                   If        not Error
     C                   EXSR      CKRTC1
     C                   Endif

      *  Additional Rate Set Code edits
      *
     C     chgMode       IFEQ      *on
     C                   EXSR      CKRTCD
     C                   ENDIF                                                  @@MODE=C
     C*
     C                   ENDIF                                                  F1CLMS=N
     c*
     c* Policy type must be valid if entered or client 25
     c*
     C     scQTYP        IFNE      *BLANKS
     C     FHF6TB        ANDNE     *Blanks
     c     flex          andeq     'Y'
     c     scQTYP        ORNE      *Blanks
     c     flex          andne     'Y'
     C     FHF6TB        ANDNE     *Blanks
     c*
     C     FHF6TB        IFEQ      *Blanks
     C                   MOVEL(P)  'PTYP'        xxTBID
     C                   ELSE
     C                   MOVEL(P)  FHF6TB        xxTBID
     C                   ENDIF
     c*
     C                   MOVEL(P)  scQTYP        xxKEY
     C*
     C     keyWTGTL      CHAIN     pgt00                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C*
     c     erQTYP        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erQTYP
     C                   if        inCall = 'L'
     C                   movel(p)  'WCR0513'     mgQTYP
     C                   Else
     C                   movel(p)  'WCR0513'     msQTYP
     C                   endif
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*ON/MASTAT<>A
     C*
     C                   ENDIF                                                  SCQTYP IFNE
      *
      *  CANNOT ENTER BLANKS IF CLARENDON PREPROCESSOR
      *
     C     WCFL20        IFNE      *BLANK
     c     erRTCD        ANDNE     *ON
      *
     C     keyWMRSL      CHAIN     WMRSL                              60
      *
     C     *IN60         IFEQ      *ON
     C     scRTCD        OREQ      *BLANKS
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'W400049'     msRTCD
     C                   ENDIF                                                              *ON
      *
     C                   ENDIF
      *
     C                   if        chgMode
     C                              and scBsCd <> saveScBsCD
     C                              and scBsCd <> *zeros
     C                   if        saveBSLPGM = 'L'
     C                   eval      clearWMLDP = *off
     C                   endif
     C                   clear                   BSLPGM
     C                   MOVEL(P)  scBSCD        xxKEY
     C     keyWTABL      CHAIN     PBS01
     C     keyWMLDL1     CHAIN(n)  PLD01
     C     keyWDDDl1     CHAIN     PDD01
     C                   select
     C* When change from Large Deductible, deductible detail can not exist
     C                   when      (saveBSLPGM = 'L' and BSLPGM <> 'L'
     C                              and %found(WDDDL1))
     C                   If        erBSCD <> *ON
     C                   move      *on           error
     C                   move      *on           erBSCD
     C                   movel(p)  'WC40072'     msBSCD
     C                   Endif
     C* When change from Large Deductible, remove deductible setup info
     C                   when      (saveBSLPGM = 'L' and BSLPGM <> 'L'
     C                              and not %found(WDDDL1))
     C                   eval      clearWMLDP = *on
     C                   endsl
     C                   eval      saveScBsCD = scBsCd
     C                   endif
     C*
     C                   MOVEL(P)  scBSCD        xxKEY
     C     keyWTABL      CHAIN     PBS01                              60
     C*
     C                   If        inCall = 'P'
     C*
     C     scDCDE        IFNE      *BLANKS
     C     BSLPGM        ANDEQ     'L'
     C     erDCDE        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDCDE
     C                   movel(p)  'WCR0450'     msDCDE
     C                   ENDIF
     C*
     C     scDCD2        IFNE      *BLANKS
     C     BSLPGM        ANDEQ     'L'
     C     erDCD2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDCD2
     C                   movel(p)  'WCR0450'     msDCD2
     C                   ENDIF
     C*
     C                   Endif                                                  If inCall = 'P'
     C*
     C*  VALIDATE THE BILLING ON EFF DAY FLAG
     C*
     C     scBLFL        IFNE      'Y'
     C     scBLFL        ANDNE     *BLANK
     C     erBLFL        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erBLFL
     C                   movel(p)  'WCR1241'     msBLFL
     C                   ENDIF                                                  SCBLFL <> 'Y'
     C*
     C                   MOVEL(P)  scBSCD        xxKEY
     C*
     C     keyWTABL      CHAIN     PBS01                              60
     C*
     C
     C  N60scBLFL        IFEQ      'Y'
     C     erBLFL        ANDNE     *ON
     C*
     c                   if        vndrfl = *on or
     c                             achflg = *on
     C*
     C     BSFREQ        IFNE      'M'
     C     BSSEFL        ORNE      'E'
     C                   move      *on           error
     C                   move      *on           erBLFL
     C                   movel(p)  'WC30390'     msBLFL
     C                   ENDIF                                                  BSFREQ NE 'M'
      *
     c                   else
      *
     c                   if        BSSEFL <> 'E'
     C                   move      *on           error
     C                   move      *on           erBLFL
     C                   movel(p)  'WC30390'     msBLFL
     c                   endif
      *
     c                   endif
      *
     C                   ENDIF                                                  SCBLFL = 'Y'
     C*
     C*  EDIT BILL INSTALLMENTS ON 15TH FLAG
     C*
     C*  EDIT "A" RATING FLAG
     C*
     C     scSTAT        IFNE      'Y'
     C     scSTAT        ANDNE     'N'
     C     erSTAT        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSTAT
     C                   movel(p)  'WCR0501'     msSTAT
     C                   ENDIF                                                  scSTAT<>Y/N
     C*
     C*  Validate activity status code if not blank
     C*
     C     scASCD        IFNE      *BLANKS
     C                   MOVEL(P)  scASCD        xxKEY
     C     keyWTAVL      CHAIN     PAV00                              60
     C*
     C     *IN60         IFEQ      *ON
     C     erASCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASCD
     C                   movel(p)  'WCR0513'     msASCD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCASCD<>*BLANKS
      *
      *  Entered type of building code must be valid.
      *
     C     scTYPB        IFNE      *BLANKS
     C                   eval      xxTBCD = scTYPB
     C     keyWTTBL      CHAIN     PTB00                              60
      *
     C     *IN60         IFEQ      *ON
     C     erTYPB        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erTYPB
     C                   if        inCall = 'L'
     C                   movel(p)  'WCR0505'     mgTYPB
     C                   else
     C                   movel(p)  'WCR0505'     msTYPB
     C                   endif
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*  PREMIUM FINANCE COMPANY MUST BE EFFECTIVE FOR POLICY EFF DATE
     C*
     C     scPFIN        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  scPFIN        xxFCOD
     C     keyWMPFL      SETGT     PPF00
     C*
     C     key1WMPFL     READPE    PPF00                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     PFEXDT        IFNE      *ZEROS
     C     xxDate        ANDGT     PFEXDT
     C*
     C     erPFIN        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erPFIN
     C                   movel(p)  'WCR1384'     msPFIN
     C                   ENDIF
     C*
     C                   ENDIF                                                  PFDT<EFDT
     C*
     C                   ELSE
     C*
     C     erPFIN        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erPFIN
     C                   movel(p)  'WCR1384'     msPFIN
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  SCFCOD<>BLKS
     C*
     C*  Check that maximum # of employees per shift > 0
     C*
     C*
     C*  SPREAD ADJUSTMENTS FLAG MUST BE 'Y' OR BLANK.
     C*
     C     scFFL1        IFNE      'Y'
     C     scFFL1        ANDNE     ' '
     C     erFFL1        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erFFL1
     C                   movel(p)  'WCR0571'     msFFL1
     C                   ENDIF                                                  SCFFL1 IFNE
      *
      * Spread billing adjustments can't be changed if A/R exists in prior month
      * for any division that also has A/R summarized
      *
     C     SCFFL1        IFNE      ELFFL1
     C     SCARSM        ANDEQ     'Y'
     C     addMode       ANDNE     *ON
     C     erFFL1        ANDNE     *ON
      *
     C                   MOVEL     sysDt7        xxxDTE
     C     keyWMEALC     CHAIN     PEA0C                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     key3WDELL     CHAIN(N)  PEL00                              60
     C     ELARSM        IFEQ      'Y'
      *
     C     xxTRYM        IFLT      xxCUYM
     C     xxPOYM        ANDLT     xxCUYM
      *
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C                   move      *on           error
     C                   move      *on           erFFL1
     C                   movel(p)  'WC30188'     msFFL1
     C                   LEAVE
     C                   ENDIF                                                  EAARTY = 'A'
      *
     C                   ENDIF                                                  XXTRYM LT XXCUYM
      *
     C                   ENDIF                                                  ELARSM = 'Y'
      *
     C     keyWMEALC     READE     PEA0C                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  SCFFL1 <> ELFFL1
      *
      * Spread billing adjustments flag must match state reporting division if
      * A/R is summarized
      *
     C     SCARSM        IFEQ      'Y'
      *
      * If not state reporting division, get state reporting division's
      * Sprd Adj flag and compare to make sure they are the same.
      *
     C     keyWMEML      CHAIN     PEM00                              60
      *
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDNE     'Y'
      *
     C     key1WMEML     CHAIN     PEM00                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFEQ      'Y'
     C                   LEAVE
     C                   ENDIF                                                  EMSTRP = 'Y'
      *
     C     key1WMEML     READE     PEM00                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
     C     key2WDELL     CHAIN(N)  PEL00                              60
      *
     C     *IN60         IFEQ      *OFF
     C     SCFFL1        ANDNE     ELFFL1
     C     erFFL1        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erFFL1
     C                   movel(p)  'WC30279'     msFFL1
     C                   ENDIF                                                  SCFFL1 <> ELFFL1
      *
     C                   ENDIF                                                  EMSTRP <> 'Y'
      *
     C                   ENDIF                                                  SCARSM = 'Y'
      *
     C     keyWDELL      CHAIN(N)  PEL00                              60
     C     keyWMEML      CHAIN     PEM00                              60
     C*
     C                   Endif                                                  inCall = 'P' or 'L'
     C*
     C* Next 4 policy flags are stored in different fields between policy detail and popup version.
     C*
     C                   If        inCall = 'P'
     C*
     C* Validate Short Rate/Prorate setting.
     C*
     C     scSRPR2       IFNE      'S'
     C     scSRPR2       ANDNE     'P'
     C     erSRPR2       ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSRPR2
     C                   movel(p)  'WCR0510'     msSRPR2
     C                   ENDIF                                                  SCSRPR<>S/P
     C*
     C* Validate concurrency flag.
     C*
     C     scCOFL2       IFNE      'Y'
     C     scCOFL2       ANDNE     ' '
     C     erCOFL2       ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erCOFL2
     C                   movel(p)  'WCR0571'     msCOFL2
     C                   ENDIF                                                  SCCOFL2 IFNE
     C*
     C* OVERRIDE PRO-RATA FACTOR FLAG MUST BE BLANK OR "Y"
     C*
     C     scFFL92       IFNE      'Y'
     C     scFFL92       ANDNE     ' '
     C     erFFL92       ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erFFL92
     C                   movel(p)  'WCR0571'     msFFL92
     C                   ENDIF
     C*
     C*  EDIT $2500 minimum flag for client 102
     C*
     C     sc25002       IFNE      'Y'
     C     sc25002       ANDNE     ' '
     C     er25002       ANDNE     *ON
     C     clientID      ANDEQ     '102'
     C                   move      *on           error
     C                   move      *on           er25002
     C                   movel(p)  'WCR0571'     ms25002
     C                   ENDIF                                                  sc25002<>Y/N
     C*
     C*
     C* % of manual premium edits
     C*
     C* Value cannot be less than zero.
     C                   If        scManP2 < 0
     C                   move      *on           error
     C                   move      *on           ermanp2
     C                   Eval      msManp2 = 'Percentage of manual premium' +
     C                              ' cannot be less than zero.'
     C                   Endif
      //
      // Percentage of Manual Premium can not be entered if large deductible
      //  is not set up or entered.
      //
      /Free
       If scManP2 > 0;
          xxKey = %Editc(scBsCd:'X');
          Chain keyWTABL  pbs01;
          Chain keyWMLDL1 pld01;
          Select;
          When %Found(WTABL) And BSlpgm <> 'L';
             error   = *On;
             erManP2 = *On;
             msManP2 = 'Percentage of manual premium cannot be entered when ' +
              'policy is not set as large deductible.';
          When Not(%Found(WMLDL1));
             error   = *On;
             erManP2 = *On;
             msManP2 = 'Large deductible information must exist before ' +
              'entering percentage of manual premium.';
          Endsl;
       Endif;
      /End-Free
     C*
     C                   Endif                                                  If inCall = 'P'
     C*
     C                   If        inCall = 'R'
     C*
     C* Validate Short Rate/Prorate setting.
     C*
     C     scSRPR        IFNE      'S'
     C     scSRPR        ANDNE     'P'
     C     erSRPR        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSRPR
      /free
             msTSRPR  = 'Either Prorate or ShortRate must be selected';
      /End-free
     C                   ENDIF                                                  SCSRPR<>S/P
     C*
     C* Validate concurrency flag.
     C*
     C     scCOFL        IFNE      'Y'
     C     scCOFL        ANDNE     ' '
     C     erCOFL        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erCOFL
     C                   movel(p)  'WCR0571'     msCOFL
     C                   ENDIF                                                  SCCOFL IFNE
     C*
     C* OVERRIDE PRO-RATA FACTOR FLAG MUST BE BLANK OR "Y"
     C*
     C     scFFL9        IFNE      'Y'
     C     scFFL9        ANDNE     ' '
     C     erFFL9        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erFFL9
     C                   movel(p)  'WCR0571'     msFFL9
     C                   ENDIF
     C*
     C*  EDIT $2500 minimum flag for client 102
     C*
     C     sc2500        IFNE      'Y'
     C     sc2500        ANDNE     ' '
     C     er2500        ANDNE     *ON
     C     clientID      ANDEQ     '102'
     C                   move      *on           error
     C                   move      *on           er2500
     C                   movel(p)  'WCR0571'     ms2500
     C                   ENDIF                                                  sc2500<>Y/N
     C*
     C*
     C* % of manual premium edits
     C*
     C* Value cannot be less than zero.
     C                   If        scManP < 0
     C                   move      *on           error
     C                   move      *on           ermanp
     C                   Eval      msManp = 'Percentage of manual premium' +
     C                              ' cannot be less than zero.'
     C                   Endif
      //
      // Percentage of Manual Premium can not be entered if large deductible
      //  is not set up or entered.
      //
      /Free
       If scManP2 > 0;
          xxKey = %Editc(scBsCd:'X');
          Chain keyWTABL  pbs01;
          Chain keyWMLDL1 pld01;
          Select;
          When %Found(WTABL) And BSlpgm <> 'L';
             error   = *On;
             erManP2 = *On;
             msManP2 = 'Percentage of manual premium cannot be entered when ' +
              'policy is not set as large deductible.';
          When Not(%Found(WMLDL1));
             error   = *On;
             erManP2 = *On;
             msManP2 = 'Large deductible information must exist before ' +
              'entering percentage of manual premium.';
          Endsl;
       Endif;
      /End-Free
     C                   Endif                                                  If inCall = 'R'
     C*
     C                   If        inCall = 'P' or inCall = 'R'
     C*
     C*  CHECK THE INCREASED LIMITS FACTOR 1
     C*
      /free
       If stRptDiv <> ELdiv and ELpfdt >= stRptIn2EfDt
        and stRptIn2EfDt <> 0;
          isIncLim1Exc = *on;
       Else;
          isIncLim1Exc = *off;
       Endif;

       If isIncLim1Exc = *off;
      /end-free
     C                   MOVEL(P)  scILCD        xxKEY
     C*
     C     scILCD        IFNE      *BLANK
     C*
     C     keyWTLML1     SETGT     PLM01
     C     key1WTLML1    READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     key1WTLML1    READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *ON
     C     erILCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erILCD
     C                   movel(p)  'WCR0513'     msILCD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCILCD<>*BLANKS
     C*
     C*  CHECK THE INCREASED LIMITS 1 DATES
     C*
     C                   Z-ADD     scIL1EfDt     xxEF              8 0
     C                   Z-ADD     scIL1ExDt     xxEX              8 0
     C                   MOVE      *ON           xxCHAR            1
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erILCD        ANDNE     *ON
     C     erIL1EfDt     ANDNE     *ON
     C     erIl1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erILCD
     C                   move      *on           erIL1EfDt
     C                   move      *on           erIl1ExDt
     C                   movel(p)  'WCR0544'     msILCD
     C                   movel(p)  'WCR0544'     msIL1EfDt
     C                   movel(p)  'WCR0544'     msIl1ExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erIL1EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL1EfDt
     C                   movel(p)  'WCR0500'     msIL1EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erIL1EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL1EfDt
     C                   movel(p)  'WCR0930'     msIL1EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erIl1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIl1ExDt
     C                   movel(p)  'WCR0500'     msIl1ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erIL1EfDt     ANDNE     *ON
     C     erIl1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL1EfDt
     C                   move      *on           erIl1ExDt
     C                   movel(p)  'WCR0529'     msIL1EfDt
     C                   movel(p)  'WCR0529'     msIl1ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erIl1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIl1ExDt
     C                   movel(p)  'WCR0930'     msIl1ExDt
     C                   ENDIF

      /free
       Else;
          If scIlCd <> *blank or scIL1EfDt <> 0 or scIL1ExDt <> 0;
             error  = *on;
             erIlCd = *on;
             msILCD = 'WCR4813';
          Endif;
       Endif;
      /end-free
     C*
     C*  CHECK THE INCREASED LIMITS FACTOR 2
     C*
     C                   MOVEL(P)  scINL2        xxKEY
     C*
     C     scINL2        IFNE      *BLANK
     C*
     C     keyWTLML1     SETGT     PLM01
     C     key1WTLML1    READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     key1WTLML1    READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *ON
     C     erINL2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erINL2
     C                   movel(p)  'WCR0513'     msINL2
     C                   ENDIF                                                  *IN60=*ON
      /free
       Select;
       When isIncLim1Exc = *on;
          If scIL2EfDt = *ZERO
           and error <> *ON;
             scIL2EfDt = DtMdyy(ELpfdt);
          Endif;
       Other;
          If scIl1ExDt = 0 and erINL2 <> *on
           and erIl1ExDt <> *on;
             error = *on;
             erINL2 = *on;
             erIl1ExDt = *on;
             msINL2 = 'WC30073';
             msIl1ExDt = 'WC30073';
          Endif;
          If scIL2EfDt = *ZERO
           and error <> *ON;
             scIL2EfDt = scIl1ExDt;
          Endif;
       Endsl;
      /end-free
     C*
     C                   ENDIF                                                  SCINL2<>*BLANKS
     C*
     C*  CHECK THE INCREASED LIMITS 2 DATES
     C*
     C                   Z-ADD     scIL2EfDt     xxEF
     C                   Z-ADD     scIL2ExDt     xxEX
     C*
     C                   MOVE      *ON           XXCHAR
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erINL2        ANDNE     *ON
     C     erIL2EfDt     ANDNE     *ON
     C     erIL2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erINL2
     C                   move      *on           erIL2EfDt
     C                   move      *on           erIL2ExDt
     C                   movel(p)  'WCR0544'     msINL2
     C                   movel(p)  'WCR0544'     msIL2EfDt
     C                   movel(p)  'WCR0544'     msIL2ExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erIL2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL2EfDt
     C                   movel(p)  'WCR0500'     msIL2EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erIL2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL2EfDt
     C                   movel(p)  'WCR0930'     msIL2EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erIL2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL2ExDt
     C                   movel(p)  'WCR0500'     msIL2ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erIL2EfDt     ANDNE     *ON
     C     erIL2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL2EfDt
     C                   move      *on           erIL2ExDt
     C                   movel(p)  'WCR0529'     msIL2EfDt
     C                   movel(p)  'WCR0529'     msIL2ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erIL2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIL2ExDt
     C                   movel(p)  'WCR0930'     msIL2ExDt
     C                   ENDIF
     C*
     C*  INCREASED LIMITS 2 EFF DT MUST BE >= LIMIT 1 EXP DT
     C*
     C                   Z-ADD     scIL2EfDt     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEFDT
     C*
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scIL2EfDt
     C*
     C                   Z-ADD     scIl1ExDt     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEXDT
     C*
     C     xxEFDT        IFLT      xxEXDT
     C     erIl1ExDt     ANDNE     *ON
     C     erIL2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erIl1ExDt
     C                   move      *on           erIL2EfDt
     C                   movel(p)  'WC30074'     msIl1ExDt
     C                   movel(p)  'WC30074'     msIL2EfDt
     C                   ENDIF                                                  xxEFDT<xxEXDT
     C*
     C*  VALIDATE THE DEDUCTIBLE DISCOUNT CODE, IF ENTERED
     C*
     C                   if        deductInstalled
     C*
     C                   MOVEL(P)  ELBSCD        xxKEY
     C     keyWTABL      CHAIN     PBS01                              60
     C     scDCDE        IFNE      *BLANKS
     C     BSLPGM        ANDEQ     'L'
     C     erDCDE        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDCDE
     C                   movel(p)  'WCR0450'     msDCDE
     C                   ENDIF
     C*
     C     scDCD2        IFNE      *BLANKS
     C     BSLPGM        ANDEQ     'L'
     C     erDCD2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDCD2
     C                   ENDIF
     C*
     C     scDCDE        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  scDCDE        xxDDCD
     C*
     C     keyWTDDL2     SETGT     PDD02
     C     key1WTDDL2    READPE    PDD02                                  60
     C*
     C     *IN60         IFEQ      *ON
     C     erDCDE        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDCDE
     C                   movel(p)  'WCR0513'     msDCDE
     C                   ENDIF                                                  *IN60=*ON
     C*
     C     *IN60         IFEQ      *OFF
     C     DDSTAT        ANDNE     'A'
     C*
     C                   DOW       *IN60 = *OFF
     C*
     C     DDSTAT        IFNE      'A'
     C*
     C     key1WTDDL2    READPE    PDD02                                  60
     C*
     C                   IF        *IN60 = *OFF
     C                   LEAVE
     C                   ELSE
     C*
     C     erDCDE        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erDCDE
     C                   movel(p)  'WCR0513'     msDCDE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  SCCDDE<>*BLANKS
     C*
     C*  CHECK THE DEDUCTIBLE DATES
     C*
     C                   MOVEL(P)  scDCDE        xxKEY
     C                   Z-ADD     scDD1EfDt     xxEF
     C                   Z-ADD     scDD1ExDt     xxEX
     C                   MOVE      *ON           xxCHAR            1
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erDCDE        ANDNE     *ON
     C     erDD1EfDt     ANDNE     *ON
     C     erDD1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDCDE
     C                   move      *on           erDD1EfDt
     C                   move      *on           erDD1ExDt
     C                   movel(p)  'WCR0544'     msDCDE
     C                   movel(p)  'WCR0544'     msDD1EfDt
     C                   movel(p)  'WCR0544'     msDD1ExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erDD1EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD1EfDt
     C                   movel(p)  'WCR0500'     msDD1EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erDD1EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD1EfDt
     C                   movel(p)  'WCR0930'     msDD1EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erDD1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD1ExDt
     C                   movel(p)  'WCR0500'     msDD1ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erDD1EfDt     ANDNE     *ON
     C     erDD1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD1EfDt
     C                   move      *on           erDD1ExDt
     C                   movel(p)  'WCR0529'     msDD1EfDt
     C                   movel(p)  'WCR0529'     msDD1ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erDD1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD1ExDt
     C                   movel(p)  'WCR0930'     msDD1ExDt
     C                   ENDIF
     C*
     C*  VALIDATE THE 2nd DEDUCTIBLE DISCOUNT CODE, IF ENTERED
     C*
     C     scDCD2        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  scDCD2        xxDDCD
     C*
     C     keyWTDDL2     SETGT     PDD02
     C     key1WTDDL2    READPE    PDD02                                  60
     C   60key1WTDDL2    CHAIN     PDD02                              60
     C*
     C     *IN60         IFEQ      *ON
     C     DDSTAT        ORNE      'A'
     C*
     C     erDCD2        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erDCD2
     C                   movel(p)  'WCR0513'     msDCD2
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  1ST DEDUCTIBLE DATE MUST BE ENTERED
     C*
     C     scDD1ExDt     IFEQ      *ZERO
     C     erDCD2        ANDNE     *ON
     C     erDD1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD1ExDt
     C                   move      *on           erDCD2
     C                   movel(p)  'WC30073'     msDD1ExDt
     C                   movel(p)  'WC30073'     msDCD2
     C                   ENDIF
     C*
     C*  IF #2 EFF DT NOT ENTERED DEFAULT IT TO #1 EXP DT
     C*
     C     scDD2EfDt     IFEQ      *ZERO
     C                   Z-ADD     scDD1ExDt     scDD2EfDt
     C                   ENDIF
     C*
     C                   ENDIF                                                  scDCD2<>*BLANKS
     C*
     C*  CHECK THE DEDUCTIBLE DATES
     C*
     C                   MOVEL(P)  scDCD2        xxKEY
     C                   Z-ADD     scDD2EfDt     xxEF
     C                   Z-ADD     scDD2ExDt     xxEX
     C                   MOVE      *ON           xxCHAR            1
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erDCD2        ANDNE     *ON
     C     erDD2EfDt     ANDNE     *ON
     C     erDD2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDCD2
     C                   move      *on           erDD2EfDt
     C                   move      *on           erDD2ExDt
     C                   movel(p)  'WCR0544'     msDCD2
     C                   movel(p)  'WCR0544'     msDD2EfDt
     C                   movel(p)  'WCR0544'     msDD2ExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erDD2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD2EfDt
     C                   movel(p)  'WCR0500'     msDD2EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erDD2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD2EfDt
     C                   movel(p)  'WCR0930'     msDD2EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erDD2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD2ExDt
     C                   movel(p)  'WCR0500'     msDD2ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erDD2EfDt     ANDNE     *ON
     C     erDD2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD2EfDt
     C                   move      *on           erDD2ExDt
     C                   movel(p)  'WCR0529'     msDD2EfDt
     C                   movel(p)  'WCR0529'     msDD2ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erDD2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD2ExDt
     C                   movel(p)  'WCR0930'     msDD2ExDt
     C                   ENDIF
     C*
     C*  DEDUCTIBLE #2 EFF DT MUST BE >= DEDUCTBLE #1 EXP DT
     C*
     C                   Z-ADD     scDD2EfDt     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEFDT
     C*
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scDD2EfDt
     C*
     C                   Z-ADD     scDD1ExDt     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEXDT
     C*
     C     xxEFDT        IFLT      xxEXDT
     C     erDD2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDD2EfDt
     C                   movel(p)  'WC30074'     msDD2EfDt
     C                   ENDIF                                                  xxEFDT<xxEXDT
     C*
     C*  IF THE DEDUCTIBLE DISCOUNT CODE IS NOT BLANK, THE BILL DED.
     C*  FLAG MUST BE 'Y' OR 'N'.
     C*

     C                   eval      ercxbl = *off
     C     scDCDE        IFNE      *BLANKS
     C     scCXBL        ANDEQ     *BLANKS

     C     erDCDE        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erDCDE
     C                   move      *on           erCXBL
     C                   movel(p)  'WCR0544'     msDCDE
     C                   movel(p)  'WCR0544'     msCXBL
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCCDDE=*BLANKS
     C*
     C                   ENDIF                                                  *IN69=*ON
      *
     C*  EDIT CONTRACTOR CREDIT DATES                                  RED
     C*
     C* IF CODE IS NOT SET UP, DO NOT ALLOW CREDIT
     C     RRCCCD        IFEQ      *ZERO
     C     scASP1        ANDNE     *ZERO
     C     erASP1        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP1
     C                   movel(p)  'WC30128'     msASP1
     C                   ENDIF                                                  CODE IFEQ
     C*
     C     scASP1        IFGT      100
     C     erASP1        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP1
     C                   movel(p)  'WC30076'     msASP1
     C                   ENDIF                                                  SCASP1>100
     C*
     C                   MOVEL(P)  scASP1        xxKEY
     C                   Z-ADD     scCCEfDt      xxEF
     C                   Z-ADD     scCCExDt      xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erASP1        ANDNE     *ON
     C     erCCEfDt      ANDNE     *ON
     C     erCCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP1
     C                   move      *on           erCCEfDt
     C                   move      *on           erCCExDt
     C                   movel(p)  'WCR0544'     msASP1
     C                   movel(p)  'WCR0544'     msCCEfDt
     C                   movel(p)  'WCR0544'     msCCExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erCCEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erCCEfDt
     C                   movel(p)  'WCR0500'     msCCEfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erCCEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erCCEfDt
     C                   movel(p)  'WCR0930'     msCCEfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erCCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erCCExDt
     C                   movel(p)  'WCR0500'     msCCExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erCCEfDt      ANDNE     *ON
     C     erCCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erCCEfDt
     C                   move      *on           erCCExDt
     C                   movel(p)  'WCR0529'     msCCEfDt
     C                   movel(p)  'WCR0529'     msCCExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erCCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erCCExDt
     C                   movel(p)  'WCR0930'     msCCExDt
     C                   ENDIF
     C*
     C*  DEVIATION 1 CANNOT BE ENTERED WITHOUT A HEADING
     C*
     C     deviation1    IFEQ      *OFF
     C*
     C     scDev1H       IFEQ      *BLANKS
     C     scASP2        ANDNE     *ZEROS
     C     erASP2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP2
     C                   movel(p)  'WCR0527'     msASP2
     C                   ENDIF                                                  SCDVH1=*BLANKS
     C*
     C*  EDIT DEVIATION 1 DATES
     C*
     C*
     C     scASP2        IFGT      100
     C     erASP2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP2
     C                   movel(p)  'WC30076'     msASP2
     C                   ENDIF                                                  SCASP2>100
     C*
     C                   MOVEL(P)  scASP2        xxKEY
     C                   Z-ADD     scDV1EfDt     xxEF
     C                   Z-ADD     scDV1ExDt     xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erASP2        ANDNE     *ON
     C     erDv1EfDt     ANDNE     *ON
     C     erDv1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP2
     C                   move      *on           erDv1EfDt
     C                   move      *on           erDv1ExDt
     C                   movel(p)  'WCR0544'     msASP2
     C                   movel(p)  'WCR0544'     msDv1EfDt
     C                   movel(p)  'WCR0544'     msDv1ExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erDv1EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv1EfDt
     C                   movel(p)  'WCR0500'     msDv1EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erDv1EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv1EfDt
     C                   movel(p)  'WCR0930'     msDv1EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erDv1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv1ExDt
     C                   movel(p)  'WCR0500'     msDv1ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erDv1EfDt     ANDNE     *ON
     C     erDv1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv1EfDt
     C                   move      *on           erDv1ExDt
     C                   movel(p)  'WCR0529'     msDv1EfDt
     C                   movel(p)  'WCR0529'     msDv1ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erDv1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv1ExDt
     C                   movel(p)  'WCR0930'     msDv1ExDt
     C                   ENDIF
     C*
     C*  DEVIATION 1 Split 2 CANNOT BE ENTERED WITHOUT A HEADING
     C*
     C     scDev1H       IFEQ      *BLANKS
     C     scD1S2        ANDNE     *ZEROS
     C     erD1S2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1S2
     C                   movel(p)  'WCR0527'     msD1S2
     C                   ENDIF                                                  scDev1H =*BLANKS
     C*
     C*  EDIT DEVIATION 1 SPLIT 2 DATES
     C*
     C     scD1S2        IFGT      100
     C     erD1S2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1S2
     C                   movel(p)  'WC30076'     msD1S2
     C                   ENDIF                                                  SCASP2>100
     C*
     C                   MOVEL(P)  scD1S2        xxKEY
     C                   Z-ADD     scD1D2EfDt    xxEF
     C                   Z-ADD     scD1D2ExDt    xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erD1S2        ANDNE     *ON
     C     erD1D2EfDt    ANDNE     *ON
     C     erD1D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1S2
     C                   move      *on           erD1D2EfDt
     C                   move      *on           erD1D2ExDt
     C                   movel(p)  'WCR0544'     msD1S2
     C                   movel(p)  'WCR0544'     msD1D2EfDt
     C                   movel(p)  'WCR0544'     msD1D2ExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erD1D2EfDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1D2EfDt
     C                   movel(p)  'WCR0500'     msD1D2EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erD1D2EfDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1D2EfDt
     C                   movel(p)  'WCR0930'     msD1D2EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erD1D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1D2ExDt
     C                   movel(p)  'WCR0500'     msD1D2ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erD1D2EfDt    ANDNE     *ON
     C     erD1D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1D2EfDt
     C                   move      *on           erD1D2ExDt
     C                   movel(p)  'WCR0529'     msD1D2EfDt
     C                   movel(p)  'WCR0529'     msD1D2ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erD1D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1D2ExDt
     C                   movel(p)  'WCR0930'     msD1D2ExDt
     C                   ENDIF
     C*
     C*
     C     scD1S2        IFNE      0
     C     scD1D2EfDt    ANDEQ     0
     C     erD1D2EfDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD1D2EfDt
     C                   movel(p)  'WCR3350'     msD1D2EfDt
     C                   ENDIF
     C*
     C     scD1D2EfDt    IFNE      0
     C     scDV1ExDt     ANDEQ     0
     C     erDv1ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv1ExDt
     C                   movel(p)  'WCR3344'     msDv1ExDt
     C                   ENDIF
     C*
     C*Check 2nd Prd Eff. Date < 1st Prd Exp. Date
     C*
     C                   Z-ADD     scD1D2EfDt    SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEF
     C*
     C                   Z-ADD     scDV1ExDT     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEX
     C*
     C     xxEF          IFNE      0
     C     xxEX          ANDGT     XXEF
     C     erDv1ExDt     ANDNE     *ON
     C     erD1D2Efdt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv1ExDt
     C                   move      *on           erD1D2Efdt
     C                   movel(p)  'WCR3343'     msDv1ExDt
     C                   movel(p)  'WCR3343'     msD1D2Efdt
     C                   ENDIF
     C*
     C                   ENDIF                                                  deviation1
     C*
     C*  DEVIATION 2 CANNOT BE ENTERED WITHOUT A HEADING
     C*
     C     deviation2    IFEQ      *OFF
     C*
     C                   MOVEL(P)  scDEV2        xxKEY
      /free
       Select;
       // Stabilisation Factor Edits >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       When clientID = '025';

          // Stabilisation Factor cannot be entered without a heading
          If scDev2H = *blanks and scStFc <> 0 and erStFc <> *on;
             error = *on;
             erStFc = *on;
             msStFc = 'WCR0527';
          Endif;

          // Edit Stabilisation Factor dates
          If scStFc > 100 and erStFc <> *on;
             error = *on;
             erStFc = *on;
             msStFc = 'WC30076';
          Endif;

          xxKey = %editc(scStFc:'X');

       // Territory Rating Edits >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       When clientID = '136';

          // Territory Rating cannot be entered without a heading
          If scDev2H = *blanks and scDev2F <> 0 and erDev2F <> *on;
             error   = *on;
             erDev2F = *on;
             msDev2F = 'WCR0527';
          Endif;

          // Edit Territory Rating dates
          If scDev2F > 100 and erDev2F <> *on;
             error   = *on;
             erDev2F = *on;
             msDev2F = 'WC30076';
          Endif;

          xxKey = %editc(scDev2F:'X');

       // Default Deviation #2 Edits >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       Other;

          // Deviation 2 cannot be entered without a heading
          If scDev2H = *blanks and scDev2 <> 0 and erDev2 <> *on;
             error = *on;
             erDev2 = *on;
             msDev2 = 'WCR0527';
          Endif;

          // Edit Deviation 2 dates
          If scDev2 > 100 and erDev2 <> *on;
             error = *on;
             erDev2 = *on;
             msDev2 = 'WC30076';
          Endif;

          xxKey = %editc(scDev2:'X');

       EndSl;
      /End-free
     C                   Z-ADD     scDV2EfDt     xxEF
     C                   Z-ADD     scDv2ExDt     xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erASP2        ANDNE     *ON
     C     erDv2EfDt     ANDNE     *ON
     C     erDv2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2EfDt
     C                   move      *on           erDv2ExDt
     C                   movel(p)  'WCR0544'     msDv2EfDt
     C                   movel(p)  'WCR0544'     msDv2ExDt
      /free
       Select;
       When clientID = '025';
          erStFc  = *on;
          msStFc  = 'WCR0544';
       When clientID = '136';
          erDev2F = *on;
          msDev2F = 'WCR0544';
       Other;
          erDev2  = *on;
          msDev2  = 'WCR0544';
       EndSl;
      /End-free
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erDv2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2EfDt
     C                   movel(p)  'WCR0500'     msDv2EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erDv2EfDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2EfDt
     C                   movel(p)  'WCR0930'     msDv2EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erDv2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2ExDt
     C                   movel(p)  'WCR0500'     msDv2ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erDv2EfDt     ANDNE     *ON
     C     erDv2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2EfDt
     C                   move      *on           erDv2ExDt
     C                   movel(p)  'WCR0529'     msDv2EfDt
     C                   movel(p)  'WCR0529'     msDv2ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erDv2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2ExDt
     C                   movel(p)  'WCR0930'     msDv2ExDt
     C                   ENDIF
     C*
     C*  DEVIATION 1 Split 2 CANNOT BE ENTERED WITHOUT A HEADING
     C*
     C     scDev2H       IFEQ      *BLANKS
     C     scD2S2        ANDNE     *ZEROS
     C     erD2S2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2S2
     C                   movel(p)  'WCR0527'     msD2S2
     C                   ENDIF                                                  SCD2H2  =*BLANKS
     C*
     C*  EDIT DEVIATION 2 DATES
     C*
     C     SCD2S2        IFGT      100
     C     erD2S2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2S2
     C                   movel(p)  'WC30076'     msD2S2
     C                   ENDIF                                                  SCDEV2>100
     C*
     C                   MOVEL(P)  scD2S2        xxKEY
     C                   Z-ADD     scD2D2EfDt    xxEF
     C                   Z-ADD     scD2D2ExDt    xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erD2S2        ANDNE     *ON
     C     erD2D2EfDt    ANDNE     *ON
     C     erD2D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2S2
     C                   move      *on           erD2D2EfDt
     C                   move      *on           erD2D2ExDt
     C                   movel(p)  'WCR0544'     msD2S2
     C                   movel(p)  'WCR0544'     msD2D2EfDt
     C                   movel(p)  'WCR0544'     msD2D2ExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erD2D2EfDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2D2EfDt
     C                   movel(p)  'WCR0500'     msD2D2EfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erD2D2EfDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2D2EfDt
     C                   movel(p)  'WCR0930'     msD2D2EfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erD2D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2D2ExDt
     C                   movel(p)  'WCR0500'     msD2D2ExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erD2D2EfDt    ANDNE     *ON
     C     erD2D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2D2EfDt
     C                   move      *on           erD2D2ExDt
     C                   movel(p)  'WCR0529'     msD2D2EfDt
     C                   movel(p)  'WCR0529'     msD2D2ExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erD2D2ExDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2D2ExDt
     C                   movel(p)  'WCR0930'     msD2D2ExDt
     C                   ENDIF
     C*
     C*
     C     scD2S2        IFNE      0
     C     scD2D2EfDt    ANDEQ     0
     C     erD2D2EfDt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erD2D2EfDt
     C                   movel(p)  'WCR3350'     msD2D2EfDt
     C                   ENDIF
     C*
     C     scD2D2EfDt    IFNE      0
     C     scDV2ExDt     ANDEQ     0
     C     erDv2ExDt     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2ExDt
     C                   movel(p)  'WCR3344'     msDv2ExDt
     C                   ENDIF
     C*
     C*Check 2nd Prd Eff. Date < 1st Prd Exp. Date
     C*
     C                   Z-ADD     scD2D2EfDt    SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEF
     C*
     C                   Z-ADD     scDV2ExDt     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEX
     C*
     C     xxEF          IFNE      0
     C     xxEX          ANDGT     xxEF
     C     erDv2ExDt     ANDNE     *ON
     C     erD2D2Efdt    ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDv2ExDt
     C                   move      *on           erD2D2Efdt
     C                   movel(p)  'WCR3343'     msDv2ExDt
     C                   movel(p)  'WCR3343'     msD2D2Efdt
     C                   ENDIF
     C*
     C                   ENDIF                                                  deviation2
     C*
     C*  EDIT SUBJECT PREM CREDIT 1A DATES
     C*
     C     subject1A     IFEQ      *OFF
     C*
     C     scSbj1AH      IFNE      *BLANKS
     C*
     C     scASPM        IFGT      100
     C     erASPM        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASPM
     C                   movel(p)  'WC30076'     msASPM
     C                   ENDIF                                                  SCASPM>100
     C*
     C                   MOVEL(P)  scASPM        xxKEY
     C                   Z-ADD     scMCEfDt      xxEF
     C                   Z-ADD     scMCExDt      xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erASPM        ANDNE     *ON
     C     erMCEfDt      ANDNE     *ON
     C     erMCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASPM
     C                   move      *on           erMCEfDt
     C                   move      *on           erMCExDt
     C                   movel(p)  'WCR0544'     msASPM
     C                   movel(p)  'WCR0544'     msMCEfDt
     C                   movel(p)  'WCR0544'     msMCExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erMCEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMCEfDt
     C                   movel(p)  'WCR0500'     msMCEfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erMCEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMCEfDt
     C                   movel(p)  'WCR0930'     msMCEfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erMCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMCExDt
     C                   movel(p)  'WCR0500'     msMCExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erMCEfDt      ANDNE     *ON
     C     erMCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMCEfDt
     C                   move      *on           erMCExDt
     C                   movel(p)  'WCR0529'     msMCEfDt
     C                   movel(p)  'WCR0529'     msMCExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erMCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMCExDt
     C                   movel(p)  'WCR0930'     msMCExDt
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCSH1A<>*BLANKS
     C*
     C                   ENDIF                                                  subject1A
     C*
     C*  EDIT SUBJECT PREM CREDIT 1B DATES
     C*
     C     subject1B     IFEQ      *OFF
     C*
     C     scSbj1BH      IFNE      *BLANKS
     C*
     C     scASPS        IFGT      100
     C     erASPS        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASPS
     C                   movel(p)  'WC30076'     msASPS
     C                   ENDIF                                                  SCASPS>100
     C*
     C                   MOVEL(P)  scASPS        xxKEY
     C                   Z-ADD     scSCEfDt      xxEF
     C                   Z-ADD     scSCExDt      xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erASPS        ANDNE     *ON
     C     erSCEfDt      ANDNE     *ON
     C     erSCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASPS
     C                   move      *on           erSCEfDt
     C                   move      *on           erSCExDt
     C                   movel(p)  'WCR0544'     msASPS
     C                   movel(p)  'WCR0544'     msSCEfDt
     C                   movel(p)  'WCR0544'     msSCExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erSCEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSCEfDt
     C                   movel(p)  'WCR0500'     msSCEfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erSCEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSCEfDt
     C                   movel(p)  'WCR0930'     msSCEfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erSCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSCExDt
     C                   movel(p)  'WCR0500'     msSCExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erSCEfDt      ANDNE     *ON
     C     erSCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSCEfDt
     C                   move      *on           erSCExDt
     C                   movel(p)  'WCR0529'     msSCEfDt
     C                   movel(p)  'WCR0529'     msSCExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erSCExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSCExDt
     C                   movel(p)  'WCR0930'     msSCExDt
     C                   ENDIF
     C*
     C                   ENDIF                                                  scSbj1BH<>*BLANKS
     C*
     C                   ENDIF                                                  subject1B
     C*
     C*  EDIT MCO TYPE CODE
     C*
     C     scMCCD        IFNE      *BLANKS
     C     erMCCD        ANDNE     *ON
     C*
     C                   MOVEL(P)  'MCO '        xxTBID
     C                   MOVEL(P)  scMCCD        xxKEY
     C*
     C     keyWTGTL      CHAIN     PGT00                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   move      *on           error
     C                   move      *on           erMCCD
     C                   movel(p)  'WCR0505'     msMCCD
     C                   ENDIF                                                  *IN60=*ON/MASTAT<>A
     C*
     C                   ENDIF                                                  SCMCCD<>*BLANKS
     C*
     C     xxSt          IFEQ      'FL'
     C*
     C*  IF MANAGED CARE CREDIT % IS ENTERED THEN THE CODE IS REQUIRED
     C*
     C*  IF A MANAGED CARE ORGANIZATIONAL CODE IS ENTERED, THEN
     C*  THERE MUST BE A A MANAGED CARE CREDIT EFFECTIVE DATE SPECIFIED
     C*
     C     scMCCD        IFNE      *BLANKS
     C     scMCEfDt      ANDEQ     *ZEROS
     C                   Z-ADD     scPFDT        scMCEfDt
     C                   ENDIF                                                  SCMCCD<>*BLANKS
     C*
     C     scMCCD        IFNE      *BLANKS
     C     scMCEfDt      ANDEQ     *ZEROS
     C     erMCEfDt      ANDNE     *ON
     C     erMCCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMCEfDt
     C                   move      *on           erMCCD
     C                   movel(p)  'WCR1247'     msMCEfDt
     C                   movel(p)  'WCR1247'     msMCCD
     C                   ENDIF                                                  scMCCD<>*BLANKS
     C*
     C                   ENDIF                                                  xxSt=FL
      *
      * MCO Code must be valid and effective as of ELPFDT, if entered.
      *
     C     scMCGP        IFNE      *BLANKS
      *
     C     keyWMMGL      SETLL     PMG00
     C     key1WMMGL     READE     PMG00                                  60
      *
     C     xxDate        IFGE      MGEFDT
     C     *IN60         ANDEQ     *OFF
      *
     C     MGEXDT        IFNE      *ZERO
     C     xxDate        ANDGE     MGEXDT
     C     erMCGP        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMCGP
     C                   movel(p)  'WCR0513'     msMCGP
     C                   ENDIF                                                  MGEXDT
      *
     C                   ELSE
      *
     C     erMCGP        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erMCGP
     C                   movel(p)  'WCR0513'     msMCGP
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60 MGEFDT
      *
     C                   ENDIF                                                  scMCGP
     C*
     C*  THE FIRST EXPERIENCE MOD MUST BE ENTERED
     C*
     C     scEXM1        IFNE      *ZERO
     C     scEM1Date     ANDEQ     *ZERO
     C                   Z-ADD     scPFDT        scEM1Date
     C                   ENDIF                                                  SCEXM1=*ZERO
     C*
      /Free
        //3rd Exp modifier cannot be entered without entering 2nd exp mod
        //*** Begin Chg *** 08/19/24 ***************************************
        //*** If scExM2 = 0 and scExM3 <> 0;
        If scEM2Date = 0 and scExM3 <> 0;
        //*** End   Chg *** 08/19/24 ***************************************
           error = *on;
           erExM3 = *on;
           msExM3 = 'WCR4830';
        Endif;
        //Populate No Rating flag if 1st modifier is zero and second is not
        If scExM1 = 0 and scExM2 <> 0;
           scNoExM1 = 'Y';
           scEM1Date = dtMDYY(ELpfdt);
        Endif;
        //*** Begin Add *** 07/30/23 *************************************
        //Populate No Rating flag for Experience Modifier 2
        If scExM2 = 0 And scEM2Date <> 0 And scExM1 <> 0;
           scNoExM2 = 'Y';
        Endif;
        //Populate No Rating flag for Experience Modifier 3
        If scExM3 = 0 And scEM3Date <> 0 And scExM2 <> 0;
           scNoExM3 = 'Y';
        Endif;
        //*** End   Add *** 07/30/23 *************************************
      /End-Free
     C*
     C*  EDIT EXPERIENCE MOD 1 DATES
     C*
     C                   MOVEL(P)  scEXM1        xxKEY
     C                   Z-ADD     scEM1Date     xxEF
     C                   Z-ADD     *ZEROS        xxEX
     C*
     C*  IF THE CODE IS ENTERED, DATE IS REQUIRED.
     C*
     C     SCEXM1        IFNE      *ZEROS
     C*
     C     xxEF          IFEQ      *ZERO
     C     erEXM1        ANDNE     *ON
     C     erEM1Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEXM1
     C                   move      *on           erEM1Date
     C                   movel(p)  'WCR0544'     msEXM1
     C                   movel(p)  'WCR0544'     msEM1Date
     C                   ENDIF                                                  xxEF<>*ZERO
     C*
     C                   ENDIF                                                  scEXM1 <> 0
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C     xxEF          IFNE      *ZERO
     C*
     C                   Z-ADD     xxEF          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         xxEF
     C*
     C     SRERR         IFEQ      *ON
     C*
     C     erEM1Date     IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erEM1Date
     C                   movel(p)  'WCR0500'     msEM1Date
     C                   ENDIF
     C*
     C                   ELSE
     C                   Z-ADD     xxEF          scEM1Date
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  XXEF
     C*
     C*  FIRST EFFECT DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C*
     C                   Z-ADD     scEM1Date     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEFDT
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPFDT
     C*
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPTDT
     C*
     C* FIRST EFF DATE MUST BE WITHIN TERM
     C*
     C     clientID      IFEQ      '139'
      *
     C     xxEFDT        IFGT      xxPTDT
     C     erEM1Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM1Date
     C                   movel(p)  'WCR0577'     msEM1Date
     C                   ENDIF
      *
     C                   ELSE
      *
     C     xxEFDT        IFGT      xxPFDT
     C     erEM1Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM1Date
     C                   movel(p)  'WCR0577'     msEM1Date
     C                   ENDIF                                                  XXEFDT>XXPFDT
     C*
     C                   ENDIF                                                  XXEFDT>XXPFDT
     C*
     C*  EDIT EXPERIENCE MOD 2 DATES
     C*
     C                   MOVEL(P)  scEXM2        xxKEY
     C                   Z-ADD     scEM2Date     xxEF
     C                   Z-ADD     *ZEROS        xxEX
     C*
     C     scEXM2        IFNE      *ZEROS
     C     scEM2Date     ANDEQ     *ZEROS
     C     erEXM2        ANDNE     *ON
     C     erEM2Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEXM2
     C                   move      *on           erEM2Date
     C                   movel(p)  'WCR0544'     msEXM2
     C                   movel(p)  'WCR0544'     msEM2Date
     C                   ENDIF                                                  SCEXM2<>*ZERO=SCEMD2
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN72         IFEQ      *ON
     C     erEM2Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM2Date
     C                   movel(p)  'WCR0500'     msEM2Date
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erEM2Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM2Date
     C                   movel(p)  'WCR0930'     msEM2Date
     C                   ENDIF
     C*
     C                   Z-ADD     xxEF          SCEM2Date
      /Free
          IF erEXM2 = *OFF AND erEM2Date = *OFF;
             IF scEM1Date <= scPFDT and scEM2DATE = scPFDT;
                error = *on;
                erEM2Date = *on;
                msEM2Date = 'WCR1202';
             ENDIF;
          ENDIF;
      /End-Free
     C*
     C*  EDIT EXPERIENCE MOD 3 DATES
     C*
     C                   MOVEL(P)  scEXM3        xxKEY
     C                   Z-ADD     scEM3Date     xxEF
     C                   Z-ADD     *ZEROS        xxEX
     C*
     C     scEXM3        IFNE      *ZEROS
     C     scEM3Date     ANDEQ     *ZEROS
     C     erEXM3        ANDNE     *ON
     C     erEM3Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEXM3
     C                   move      *on           erEM3Date
     C                   movel(p)  'WCR0544'     msEXM3
     C                   movel(p)  'WCR0544'     msEM3Date
     C                   ENDIF                                                  SCEXM2<>*ZERO=SCEMD2
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN72         IFEQ      *ON
     C     erEM3Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM3Date
     C                   movel(p)  'WCR0500'     msEM3Date
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erEM3Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM3Date
     C                   movel(p)  'WCR0930'     msEM3Date
     C                   ENDIF
     C*
     C                   Z-ADD     xxEF          SCEM3Date
     C*
     C*  EXPERIENCE MODIFIER 1 MUST BE ENTERED FIRST
     C*
     C     scEM1Date     IFEQ      *ZEROS
     C     scEXM1        ANDEQ     *ZEROS
     C     scEM2Date     ANDNE     *ZEROS
     C     scEXM2        ANDNE     *ZEROS
     C     erEXM1        ANDNE     *ON
     C     erEM1Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEXM1
     C                   move      *on           erEM1Date
     C                   movel(p)  'WCR0646'     msEXM1
     C                   movel(p)  'WCR0646'     msEM1Date
     C                   ENDIF                                                  scEM1Date <> 0
     C*
     C*** Begin Chg *** 07/30/23 *************************************
     C**** SCEM1Date     IFEQ      *ZEROS
     C**** SCEXM1        ANDEQ     *ZEROS
     C**** SCEM3Date     ANDNE     *ZEROS
     C**** SCEXM3        ANDNE     *ZEROS
     C**** erEXM2        ANDNE     *ON
     C**** erEM2Date     ANDNE     *ON
     C*
     C*  EXPERIENCE MODIFIER 2 MUST BE ENTERED FIRST
     C*
     C     SCEM2Date     IFEQ      *ZEROS
     C     SCEM3Date     ANDNE     *ZEROS
     C     SCEXM3        ANDNE     *ZEROS
     C     erEXM2        ANDNE     *ON
     C     erEM2Date     ANDNE     *ON
     C*** End   Chg *** 07/30/23 *************************************
     C                   move      *on           error
     C                   move      *on           erEXM2
     C                   move      *on           erEM2Date
     C                   movel(p)  'WCR0646'     msEXM2
     C                   movel(p)  'WCR0646'     msEM2Date
     C                   ENDIF                                                  scEM1Date <> 0
      *
      *  Experience Modifier Dates must be in ascending order, if entered
      *
     C                   Z-ADD     scEM1Date     SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scEM1Date
      *
     C                   Z-ADD     scEM1Date     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         X1EMD1            7 0
      *
     C                   Z-ADD     scEM2Date     SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scEM2Date
      *
     C                   Z-ADD     scEM2Date     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         X1EMD2            7 0
      *
     C                   Z-ADD     scEM3Date     SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scEM3Date
      *
     C                   Z-ADD     scEM3Date     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         X1EMD3            7 0
      *
     C     X1EMD1        IFNE      *ZERO
     C     X1EMD2        ANDNE     *ZERO
     C     X1EMD1        ANDGE     X1EMD2
     C     erEM1Date     ANDNE     *ON
     C     erEM2Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM1Date
     C                   move      *on           erEM2Date
     C                   movel(p)  'WCR0529'     msEM1Date
     C                   movel(p)  'WCR0529'     msEM2Date
     C                   ENDIF
      *
     C     X1EMD2        IFNE      *ZERO
     C     X1EMD3        ANDNE     *ZERO
     C     X1EMD2        ANDGE     X1EMD3
     C     erEM2Date     ANDNE     *ON
     C     erEM3Date     ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erEM2Date
     C                   move      *on           erEM3Date
     C                   movel(p)  'WCR0529'     msEM2Date
     C                   movel(p)  'WCR0529'     msEM3Date
     C                   ENDIF
      *
      *  IF RATING STATE = MA, PERFORM FOLLOWING CHECKS
      *  ARP FACTOR NOT VALID IF SCHEDULED DB/CR FACTOR EXISTS
      *
     C     xxSt          IFEQ      'MA'
      *
     C     scARP1        IFNE      *ZERO
     C     scEXM1        ANDEQ     *ZERO
      *
     C     erARP1        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erARP1
     C                   movel(p)  'WC30244'     msARP1
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     scARP1        IFNE      *ZERO
     C     scARP1        IFLT      1.0
     C     scARP1        ORGT      1.49
      *
     C     erARP1        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erARP1
     C                   movel(p)  'WC30243'     msARP1
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
     C     scARP2        IFNE      *ZERO
     C     scEXM2        ANDEQ     *ZERO
      *
     C     erARP2        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erARP2
     C                   movel(p)  'WC30244'     msARP2
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     scARP2        IFNE      *ZERO
     C     scARP2        IFLT      1.0
     C     scARP2        ORGT      1.49
      *
     C     erARP2        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erARP2
     C                   movel(p)  'WC30243'     msARP2
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
     C     scARP3        IFNE      *ZERO
     C     scEXM3        ANDEQ     *ZERO
      *
     C     erARP3        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erARP3
     C                   movel(p)  'WC30244'     msARP3
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     scARP3        IFNE      *ZERO
     C     scARP3        IFLT      1.0
     C     scARP3        ORGT      1.49
      *
     C     erARP3        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erARP3
     C                   movel(p)  'WC30243'     msARP3
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
     C     scARP1        IFNE      *ZERO
     C     scDBCR        ANDNE     *ZERO
     C     scARP2        ORNE      *ZERO
     C     scDBCR        ANDNE     *ZERO
     C     scARP3        ORNE      *ZERO
     C     scDBCR        ANDNE     *ZERO
      *
     C     erARP1        IFNE      *ON
     C     erARP2        ANDNE     *ON
     C     erARP3        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erARP1
     C                   move      *on           erARP2
     C                   move      *on           erARP3
     C                   movel(p)  'WC30242'     msARP1
     C                   movel(p)  'WC30242'     msARP2
     C                   movel(p)  'WC30242'     msARP3
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  ST = 'MA'
     C*
      /Free
       erExMd1Nf = *off;
       msExMd1Nf = ' ';
       If scExMd1Nf = 'Y' and scExm1 = *zeros;
          error =*on;
          erExMd1Nf = *on;
          msExMd1Nf = 'WC31537';
       EndIf;

       erExMd2Nf = *off;
       msExMd2Nf = ' ';
       If scExMd2Nf = 'Y' and scExm2 = *zeros;
          error =*on;
          erExMd2Nf = *on;
          msExMd2Nf = 'WC31537';
        EndIf;

       erExMd3Nf = *off;
       msExMd3Nf = ' ';
       If scExMd3Nf = 'Y' and scExm3 = *zeros;
          error =*on;
          erExMd3Nf = *on;
          msExMd3Nf = 'WC31537';
       EndIf;
      /End-Free
     C*  INTERNALLY PROMULGATED FLAG NOT 'Y' OR BLANK
     C*
     C     scPROM        IFNE      'Y'
     C     scPROM        ANDNE     *BLANKS
     C     scPROM        ANDNE     'M'
     C     erPROM        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erPROM
     C                   movel(p)  'WC30228'     msPROM
     C                   ENDIF                                                  SCPROM<>Y/*BLANKS
       // Experience Rating must be Entered
       //*** Beg Add *** 03/04/24 ******************************************
       If scExpRat = *Blanks and erExpRat <> *on;
          error = *On;
          erExpRat = *On;
          msExpRat = 'WCR0503';
       Endif;
       //*** End Add *** 03/04/24 ******************************************
     C*
     C*  SCHEDULE DB/CR CANNOT BE < *ZERO
     C*
     C     schedule      IFEQ      *OFF
     C*
     C     SCDBCR        IFLT      *ZERO
     C     erDBCR        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDBCR
     C                   movel(p)  'WCR0547'     msDBCR
     C                   ENDIF                                                  SCDBCR<*ZERO
     C*
     C*  EDIT SCHEDULE DB/CR FACTOR DATES
     C*
     C                   MOVEL(P)  scDBCR        xxKEY
     C                   Z-ADD     scSDEfDt      xxEF
     C                   Z-ADD     scSDExDt      xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erDBCR        ANDNE     *ON
     C     erSDEfDt      ANDNE     *ON
     C     erSDExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDBCR
     C                   move      *on           erSDEfDt
     C                   move      *on           erSDExDt
     C                   movel(p)  'WCR0544'     msDBCR
     C                   movel(p)  'WCR0544'     msSDEfDt
     C                   movel(p)  'WCR0544'     msSDExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erSDEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSDEfDt
     C                   movel(p)  'WCR0500'     msSDEfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erSDEfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSDEfDt
     C                   movel(p)  'WCR0930'     msSDEfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erSDExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSDExDt
     C                   movel(p)  'WCR0500'     msSDExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erSDEfDt      ANDNE     *ON
     C     erSDExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSDEfDt
     C                   move      *on           erSDExDt
     C                   movel(p)  'WCR0529'     msSDEfDt
     C                   movel(p)  'WCR0529'     msSDExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erSDExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSDExDt
     C                   movel(p)  'WCR0930'     msSDExDt
     C                   ENDIF
     C*
     C*  EDIT to check schedule debit/credit factor is in min/max range(FMIC only)
     C*
     c                   if        scDBCR <> 0
     C                             and erDBCR <> *ON

     c                   if        ((scDBCR < rrcmax) and (rrcmax <> 0))
     c                              or
     c                             ((scDBCR > rrdmax) and (rrdmax <> 0))
     C                   move      *on           error
     C                   move      *on           erDBCR
     C                   movel(p)  'WC40050'     msDBCR
     C                   Endif                                                  Min/Max Debit/Credit

     C                   Endif                                                  scdbcr <> 0
     C*
     C                   ENDIF                                                  schedule
     C*
     C                   If        waiver = *off
     C*
     C*  The waiver type must be 'B', 'S' or blank.
     C*
     C     SCWVTP        IFNE      'B'
     C     SCWVTP        ANDNE     'S'
     C     SCWVTP        ANDNE     ' '
     C     erWVTP        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erWVTP
     C                   movel(p)  'WCR3142'     msWVTP
     C                   ENDIF                                                  *IN90 = *OFF

       // Check if division is eligible for waiver
       If scWvTp <> *Blank And error = *Off;
          chrRateState = xxSt;
          If GetWaiverInfo(scCo#:scFnd:chrRateState:xxPfDt:chrWvrForm:
           chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:scFyr:scEmp#:scDiv) = *Off;
             If %Subst(chrWvBlkPrjDs:495:6) <> 'IGNORE';
                msNoWvrDsc = chrWvBlkPrjDs;
                erNoWvrDsc = *On;
                error = *On;
             Endif;
          Endif;
       Endif;
       // Check if any other division of the same state has the
       //  other waiver type.
       If scWvTp <> *Blank And error = *Off;
          Select;
          When scWvTp = 'B';
             chrWvrType = 'S';
             chrWvrTypeVbg = 'specific waivers';
          When scWvTp = 'S';
             chrWvrType = 'B';
             chrWvrTypeVbg = 'blanket waivers';
          EndSl;
          chrRateState = xxSt;
          If GetDivForWaiverType(scCo#:scFnd:scFyr:scEmp#:chrWvrForm:
           chrRateState:chrWvrType:intRtnDiv) = *On;
             If intRtnDiv <> scDiv;
                msNoWvrDsc = 'Division ' + %Char(intRtnDiv) +
                 ' is already flagged for ' + %Trim(chrWvrTypeVbg) +
                 ' in ' + xxSt + '.';
                erNoWvrDsc = *On;
                error = *On;
             Endif;
          EndIf;
       Endif;
     C*
     C*  Waiver percentage cannot be less than zero.
     C*
     C     SCWVPC        IFLT      0
     C     erWVPC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erWVPC
     C                   movel(p)  'WCR0547'     msWVPC
     C                   ENDIF                                                  SCWVPC < 0
     C*
     C*  Flat waiver charge cannot be less than zero.
     C*
     C     SCFCHG        IFLT      0
     C     erFCHG        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erFCHG
     C                   movel(p)  'WCR0547'     msFCHG
     C                   ENDIF                                                  SCFCHG < 0
     C*
     C*  Minimum charge cannot be less than zero.
     C*
     C     scWMIN        IFLT      0
     C     erWMIN        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erWMIN
     C                   movel(p)  'WCR0547'     msWMIN
     C                   ENDIF                                                  scWMIN < 0
     C*
     C*  Maximum charge cannot be less than zero.
     C*
     C     scWMAX        IFLT      0
     C     erWMAX        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erWMAX
     C                   movel(p)  'WCR0547'     msWMAX
     C                   ENDIF                                                  scWMAX < 0
     C*
     C*  Waiver percentage and flat charge are mutually exclusive.
     C*
     C     SCWVPC        IFGT      0
     C     SCFCHG        ANDGT     0
     C     erFCHG        ANDNE     *ON
     C     erWVPC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erFCHG
     C                   move      *on           erWVPC
     C                   movel(p)  'WCR0590'     msFCHG
     C                   movel(p)  'WCR0590'     msWVPC
     C                   ENDIF                                                  SCWVPC > 0
     C*
      *** Beg Del ***** 01/05/23 ************************************
     C***If the waiver type is blanket 'B', percentage and flat charge
     C***cannot both be blank.
     C***
     C***  SCWVTP        IFEQ      'B'
     C***  SCWVPC        ANDEQ     0
     C***  SCFCHG        ANDEQ     0
     C***  erFCHG        ANDNE     *ON
     C***  erWVPC        ANDNE     *ON
     C***                move      *on           error
     C***                move      *on           erFCHG
     C***                move      *on           erWVPC
     C***                movel(p)  'WCR3144'     msFCHG
     C***                movel(p)  'WCR3144'     msWVPC
     C***                ENDIF                                                  SCWVTP = B
      *** End Del ***** 01/05/23 ************************************
     C*
     C*  Premium Level flag must be blank if the Waiver Type is blank.
     C*
     C     SCWVTP        IFNE      *BLANK
      *
     C     SCWVFL        IFNE      'J'
     C     SCWVFL        ANDNE     'E'
     C     SCWVFL        ANDNE     'S'
     C     SCWVFL        ANDNE     'D'
     C     SCWVFL        ANDNE     'M'
     C     erWVFL        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erWVFL
     C                   movel(p)  'WC30189'     msWVFL
     C                   ENDIF                                                  SCWVFL != JESDM
      *
     C                   ELSE
     C     SCWVFL        IFNE      *BLANK
     C     erWVFL        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erWVFL
     C                   movel(p)  'WCR0450'     msWVFL
      *
     C                   ENDIF                                                  SCWVFL != BLANK
      *
     C                   ENDIF                                                  SCWVTP != BLANK
        //*** Beg Add *** 12/11/23 *** PHW114578 ***********************

           If scWVTP = 'B' and scOVRWD = 'Y' and scBlnkWd = ' ';
              Error    = *On;
              erBlnkWd = *On;
              msBlnkWd = 'WCR0503';
           EndIf;

        //*** End Add *** 12/11/23 *** PHW114578 ***********************
     C*
     C                   ENDIF                                                  waiver
     C*
     C*  STOCK DISCOUNT % MUST EXIST > 100
     C*
     C     scASP3        IFGT      100
     C     erASP3        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP3
     C                   movel(p)  'WC30076'     msASP3
     C                   ENDIF                                                  SCASP3>100
     C*
     C*  DISCOUNT MUST EXIST IN DISCOUNT FILE
     C*
     C     scDISC        IFNE      *BLANK

      /free
       //* Check the stock discount code if ARD rated and the ARD will be used.
       xxDiscDt = xxDate;
       If inCall = 'R';
          xxAndt = ELAndt;
       Endif;
       If xxAndt <> 0;
          xxDiscDt = xxAndt + 10000;
          If xxDiscDt > xxDate;
             xxDiscDt = xxAndt;
          Else;
             xxDiscDt = xxDate;
          Endif;
       Endif;
      /end-free
     C*
     C     keyWMDSL3     SETGT     PDS03
     C     key1WMDSL3    READPE    PDS03                                  60
     C*
     C     *in60         IFEQ      *ON
     C     erDISC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erDISC
     C                   movel(p)  'WCR0513'     msDISC
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCDISC<>*BLANK
     C*
     C*  DISCOUNT % AND TYPE CANNOT BOTH BE USED
     C*
     C     scDISC        IFNE      *BLANKS
     C     scASP3        ANDNE     *ZEROS
     C     erASP3        ANDNE     *ON
     C     erDISC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erASP3
     C                   move      *on           erDISC
     C                   movel(p)  'WCR0581'     msASP3
     C                   movel(p)  'WCR0581'     msDISC
     C                   ENDIF                                                  SCDISC<>*BLANKS
     C*
     C*  MEMBERSHIP FEES.. cannot be less than zero
     C*
     C     scMFEE        IFLT      *ZERO
     C     erMFEE        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMFEE
     C                   movel(p)  'WCR0547'     msMFEE
     C                   ENDIF
     C*
     C*  DO NOT PERFORM THESE EDIT IF CLAIMS ONLY GROUP
     C*
     C     F1CLMS        IFEQ      'N'
     C*
     C*  SAFETY FEE % < ZERO OR > 100
     C*
     C     scSFPC        IFLT      *ZERO
     C     erSFPC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSFPC
     C                   movel(p)  'WCR0547'     msSFPC
     C                   ENDIF                                                  SCSFPC<*ZERO
     C*
     C     SCSFPC        IFGT      100
     C     erSFPC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSFPC
     C                   movel(p)  'WC30076'     msSFPC
     C                   ENDIF                                                  SCSFPC>100
     C*
     C*  SAFETY FEE MINIMUM AMOUNT < ZERO
     C*
     C     SCSFMN        IFLT      *ZERO
     C     erSFMN        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erSFMN
     C                   movel(p)  'WCR0547'     msSFMN
     C                   ENDIF                                                  SCSFMN<*ZERO
     C*
     C                   ENDIF                                                  F1CLMS='N'
      *
      * Facultative Reinsurance - Percentage can not be less than zero.
      *
     C                   If        faculty = *OFF
      *
     C     scARCP        IFLT      *ZEROS
     C     erARCP        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erARCP
     C                   movel(p)  'WCR0502'     msARCP
     C                   ENDIF                                                  scARCP IFLT 0
      *
      * Flat amount can not be less than *zeros
      *
     C     scARCA        IFLT      *ZEROS
     C     erARCA        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erARCA
     C                   movel(p)  'WCR0547'     msARCA
     C                   ENDIF                                                  scARCA IFLT 0
      *
      * Validates the date range entered
      *
     C     scARCP        IFNE      *ZERO
     C                   MOVEL(P)  scARCP        xxKEY
     C                   ELSE
     C                   MOVEL(P)  scARCA        xxKEY
     C                   ENDIF
     C*
     C                   Z-ADD     scAREfDt      xxEF
     C                   Z-ADD     scARExDt      xxEX
     C*
     C                   EXSR      DTERRS
     C*
     C     *IN71         IFEQ      *ON
     C     erARCP        ANDNE     *ON
     C     erARCA        ANDNE     *ON
     C     erAREfDt      ANDNE     *ON
     C     erARExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erARCP
     C                   move      *on           erARCA
     C                   move      *on           erAREfDt
     C                   move      *on           erARExDt
     C                   movel(p)  'WCR0544'     msARCP
     C                   movel(p)  'WCR0544'     msARCA
     C                   movel(p)  'WCR0544'     msAREfDt
     C                   movel(p)  'WCR0544'     msARExDt
     C                   ENDIF
     C*
     C     *IN72         IFEQ      *ON
     C     erAREfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erAREfDt
     C                   movel(p)  'WCR0500'     msAREfDt
     C                   ENDIF
     C*
     C     *IN73         IFEQ      *ON
     C     erAREfDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erAREfDt
     C                   movel(p)  'WCR0930'     msAREfDt
     C                   ENDIF
     C*
     C     *IN75         IFEQ      *ON
     C     erARExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erARExDt
     C                   movel(p)  'WCR0500'     msARExDt
     C                   ENDIF
     C*
     C     *IN74         IFEQ      *ON
     C     erAREfDt      ANDNE     *ON
     C     erARExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erAREfDt
     C                   move      *on           erARExDt
     C                   movel(p)  'WCR0529'     msAREfDt
     C                   movel(p)  'WCR0529'     msARExDt
     C                   ENDIF
     C*
     C     *IN76         IFEQ      *ON
     C     erARExDt      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erARExDt
     C                   movel(p)  'WCR0930'     msARExDt
     C                   ENDIF
     C*
     C                   Z-ADD     scAREfDt      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scAREfDt
     C*
     C                   Z-ADD     scARExDt      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scARExDt
      *
     C                   ENDIF
      *
      * Voluntary Labor - Flat amount can not be less than *zeros
      *
     C                   If        volLabor = *OFF
      *
     C     scVLCA        IFLT      *ZEROS
     C     erVLCA        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erVLCA
     C                   movel(p)  'WCR0547'     msVLCA
     C                   ENDIF                                                  scVLCA IFLT 0
      *
     C                   ENDIF
     C*
     C                   Endif                                                  inCall = 'P' or 'R'
     C*
     C                   IF        not error
     C                   MOVE      'N'           XXOFLG
     C                   ENDIF
     C*
     C                   EndSr
     C*===============================================================
     C* DTERRS - SUBROUTINE FOR DETERMINING FROM/TO DATE ERRORS
     C*===============================================================
     C*
     C     DTERRS        BEGSR
     C*
     C                   MOVE      *off          *IN71
     C                   MOVE      *off          *IN72
     C                   MOVE      *off          *IN73
     C                   MOVE      *off          *IN74
     C                   MOVE      *off          *IN75
     C                   MOVE      *off          *IN76
     C*
     C                   If        xxchar = *off
     C*
     C     '0'           CHECK     xxKEY         xxZERO            3 0
     C                   SUB       1             xxZERO
     C     ' '           CHECKR    xxKEY         xxLEN             3 0
     C*
     C     xxZERO        IFEQ      xxLEN
     C                   MOVEL     *ZEROS        xxKEY
     C                   ENDIF                                                  XXZERO=XXLEN
     C*
     C                   ENDIF                                                  xxchar = *off
     C*
     C*  IF ONE OF THE DATES IS ENTERED, CODE IS REQUIRED.
     C*
     C*
     C     xxKEY         IFEQ      *ZEROS
     C     xxCHAR        ANDEQ     *OFF
     C     xxKEY         OREQ      *BLANKS
     C*
     C     xxEF          IFNE      *ZERO
     C     xxEX          ORNE      *ZERO
     C                   SETON                                        71
     C                   ENDIF                                                  XXEF<>*ZERO<>XXEX
     C*
     C                   ELSE
     C*
     C     xxEF          IFEQ      *ZERO
     C                   SETON                                        71
     C                   ENDIF                                                  XXEF<>*ZERO<>XXEX
     C*
     C                   ENDIF                                                  *ZEROS=XXKEY=*BLANKS
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C     xxEF          IFNE      *ZERO
     C*
     C                   Z-ADD     xxEF          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         xxEF
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        72
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  EFFECTIVE DATE MUST BE BETWEEN POLICY FROM & TO DATES
     C*
     C                   Z-ADD     scPFDT        xxPF              8 0
     C                   Z-ADD     scPTDT        xxPT              8 0
     C*
     C                   Z-ADD     xxEF          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEFDT
     C*
     C                   Z-ADD     xxPF          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPFDT
     C*
     C                   Z-ADD     xxPT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPTDT
     C*
     C     xxEFDT        IFLT      xxPFDT
     C     xxEFDT        ORGT      xxPTDT
     C                   SETON                                        73
     C                   ENDIF                                                  xxPF>xxEF>xxPT
     C*
     C                   ENDIF                                                  xxEF<>*ZERO
     C*
     C*  IF ENTERED, EXPIRATION DATE MUST BE A VALID DATE
     C*
     C     xxEX          IFNE      *ZERO
     C*
     C                   Z-ADD     xxEX          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         xxEX
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        75
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  EFFECTIVE DATE MUST BE LESS THAN EXPIRATION DATE
     C*
     C                   Z-ADD     xxEX          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEXDT
     C*
     C     xxEXDT        IFLT      xxEFDT
     C                   SETON                                        74
     C                   ENDIF                                                  xxEXDT<xxEFDT
     C*
     C*  EXPIRATION DATE MUST BE BETWEEN POLICY FROM & TO DATES
     C*
     C     xxEXDT        IFLT      xxPFDT
     C     xxEXDT        ORGT      xxPTDT
     C                   SETON                                        76
     C                   ENDIF                                                  xxPFDT>xxEXDT>xxPTDT
     C*
     C                   ENDIF                                                  xxEXDT<>*ZERO
     C*
     C                   MOVE      *OFF          XXCHAR
     C                   ENDSR                                                  DTERRS
      *
      *===============================================================
      * CKRTC1 - "RATE SET CODE" EDITS
      *===============================================================
     C     CKRTC1        BEGSR
      *
      * Info edit if Rate Code corresponds to Phys/Mail state of Emp.
      *
     C     scRTCD        IFNE      *blanks
      *
     C     xxPhysSt      IFNE      xxSt
     C     xxPhysSt      ANDNE     *blanks
     C     xxMailSt      ORNE      xxSt
     C     xxPhysSt      ANDEQ     *blanks
     C*
     C                   move      *off          rcsCancel
     C                   move      *off          rcsConfirm
     C*
     C                   Dou       rcsCancel = *on or rcsConfirm = *on
     C                   EXFMT     rcdRateSt
     C                   ENDDO
     C*
     C     rcsCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ELSE
     C                   MOVEL(P)  scRTCD        svRTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  scRTCD
      *
      *  Check against any entered Group Rate Set Codes
      *
     C     keyWMGRL      CHAIN     PGR00                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     GRSTAT        ANDEQ     'I'
     C     keyWMGRL      READE     PGR00                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C     key1WMGRL     CHAIN     PGR00                              60
      *
     C     *IN60         IFEQ      *ON
     C     GRSTAT        OREQ      'I'
      *
     C     erRTCD        IFEQ      *OFF
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31413'     msRTCD
     C                   ENDIF
      *
     C                   ENDIF                                                  GRKY2
      *
     C                   ENDIF                                                  GRKY1
      /free

       // Check if PEO type is valid for rate set code

       If erRtCd = *Off And
              (qteMode = *Off Or (qteMode = *On And ELnwRw = 'R'));

          // Get the rating State
          Chain (scCo#:scRTCd) prs00;
          If %Found(wmrsl);
             rateState = RSjrSt;
          EndIf;

          // Determine if there are any PEO errors
          errorMsg = EdtPeoPolType(ELco#:ELfnd:ELemp#:ELdiv:rateState:ELpfDt:
                                   inCall);
          If errorMsg = 'Override Master/MCP';
             scOvrMP = 'Y';
             Clear ErrorMsg;
          Else;
             scOvrMP = ' ';
          Endif;

          // If error message returned then display error
          If errorMsg <> ' ';
             mgRtCd  = errorMsg;
             erRtCd2 = *On;
             error = *On;
          EndIf;

       EndIf;

      /end-free
      *
     C                   ENDSR                                                  CKRTC1
      *
      *
      *===============================================================
      * CKRTCD - ADDITIONAL "RATE SET CODE" EDITS
      *===============================================================
     C     CKRTCD        BEGSR
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
     C     *LIKE         DEFINE    RR3RDP        SV3RDP
     C     *LIKE         DEFINE    RR3RDL        SV3RDL
     C     *LIKE         DEFINE    RR3RDH        SV3RDH
     C     *LIKE         DEFINE    RR3RDF        SV3RDF
     C     *LIKE         DEFINE    RR3IDD        SV3IDD
     C     *LIKE         DEFINE    RR3IDV        SV3IDV
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
     C     *LIKE         DEFINE    RRVLCH        SVVLCH
      *
      *  If rate code / effective date change
      *
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDT2             7 0
      *
      *  Get statistical data elements associated with previous
      *  rate code and effective date
      *
     C     key1WMRSL     CHAIN     PRS00                              60
     C                   MOVEL     RSJRST        xxSt
      *
     C                   Z-ADD     ELPFDT        xxDate
      *
     C     ELPFDT        IFNE      xxDT2
     C     ELRTCD        ORNE      scRTCD
      *
     C     keyWDRRL      SETGT     PRR00
     C     key1WDRRL     READPE    PRR00                                  60
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
     C                   Z-ADD     RR3RDP        SV3RDP
     C                   MOVE      RR3RDL        SV3RDL
     C                   MOVE      RR3RDH        SV3RDH
     C                   Z-ADD     RR3RDF        SV3RDF
     C                   MOVE      RR3IDD        SV3IDD
     C                   MOVE      RR3IDV        SV3IDV
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
     C                   MOVE(P)   RRVLCH        SVVLCH
     C                   ENDIF
      *
      *  Get statistical data elements associated with current
      *  rate code and effective date
      *
     C     keyWMRSL      CHAIN     PRS00                              60
     C                   MOVEL     RSJRST        xxST
      *
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDate
      *
     C     keyWDRRL      SETGT     PRR00
     C     key1WDRRL     READPE    PRR00                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
      *  Error check for missing headings
      *
     C     SVDV1H        IFNE      *BLANKS
     C     RRDV1H        ANDEQ     *BLANKS
     C     scASP2        ANDNE     *ZERO
     C     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31339'     msRTCD
     C                   ENDIF                                                  DV1H
      *
     C     SVDV2H        IFNE      *BLANKS
     C     RRDV2H        ANDEQ     *BLANKS
     C     scDEV2        ANDNE     *ZERO
      *
     C     erRTCD        IFNE      *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31340'     msRTCD
     C                   ENDIF
      *
     C                   ENDIF                                                  DV2H
      *
     C     SV1AHD        IFNE      *BLANKS
     C     RR1AHD        ANDEQ     *BLANKS
     C     scASPM        ANDNE     *ZERO
     C     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31341'     msRTCD
     C                   ENDIF                                                  1AHD
      *
     C     SV1BHD        IFNE      *BLANKS
     C     RR1BHD        ANDEQ     *BLANKS
     C     scASPS        ANDNE     *ZERO
     C     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31342'     msRTCD
     C                   ENDIF                                                  1BHD
      *
     C     SVCRHD        IFNE      *BLANKS
     C     RRCRHD        ANDEQ     *BLANKS
     C     scDBCR        ANDNE     *ZERO
     C     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31343'     msRTCD
     C                   ENDIF                                                  CRHD
      *
     C     SVDBHD        IFNE      *BLANKS
     C     RRDBHD        ANDEQ     *BLANKS
     C     scDBCR        ANDNE     *ZERO
     C     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31344'     msRTCD
     C                   ENDIF                                                  DBHD
      *
     C     SVWVHD        IFNE      *BLANKS
     C     RRWVHD        ANDEQ     *BLANKS
     C     erRTCD        ANDNE     *ON
      *
     C     scWVTP        IFNE      *BLANK
     C     scWVPC        ORNE      *ZERO
     C     scFCHG        ORNE      *ZERO
     C     scWMIN        ORNE      *ZERO
     C     scWVFL        ORNE      *BLANK
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31345'     msRTCD
     C                   ENDIF                                                  WVHD
      *
     C                   ENDIF                                                  WVHD0
      *
     C     SVVLCH        IFNE      *BLANKS
     C     RRVLCH        ANDEQ     *BLANKS
      *
     C     scVLCA        IFNE      *ZERO
     C     erRTCD        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRTCD
     C                   movel(p)  'WC31500'     msRTCD
     C                   ENDIF                                                  VLCA
      *
     C                   ENDIF                                                  VLCH
      *
      *  Popup window for Info message if statistical data elements changed
      *
     C     error         IFEQ      *OFF
     C     xxONCE        ANDEQ     'N'
     C                   MOVE      'Y'           xxONCE
      *
      * Clear the subfile.
      *
      /Free
       RcdRateChgDsp(*off);
      /End-Free
     C                   Eval      clrElement = *On
     C                   Write     rcdRateChg
     C                   Eval      clrElement = *Off
      /Free
       RcdRateChgDsp(*on);
      /End-Free
     C                   Eval      rrnElement = 0
      *
     C                   MOVEL(P)  *BLANKS       scElement
      *
      *      Standard Liability Limits
      *
     C     RRACC         IFNE      SVACC
     C     RRDIS         ORNE      SVDIS
     C     RRAGG         ORNE      SVAGG
     C     RREMLB        ORNE      SVEMLB
     C                   MOVEL     STA(1)        scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      Deviation #1
      *
     C     ELASP2        IFNE      *ZERO
      *
     C     RRDV1L        IFNE      SVDV1L
     C     RRDV1H        ORNE      SVDV1H
     C                   MOVEL     STA(2)        scElement
      *
     C     SVDV1H        IFNE      *BLANKS
     C                   CAT(P)    SVDV1H:0      scElement
      *
     C                   ELSE
      *
     C                   CAT(P)    RRDV1H:0      scElement
     C                   ENDIF                                                  RRDV1H
      *
     C                   CAT(P)    '"':0         scElement
     C                   CAT(P)    'is app':1    scElement
     C                   CAT(P)    'lied':0      scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF                                                  RRDV1L
      *
     C                   ENDIF                                                  ELASP2
      *
      *      Deviation #2
      *
     C     ELDEV2        IFNE      *ZERO
     C     clientID      ANDNE     '025'
     C     ELSTFC        ORNE      *ZERO
     C     clientID      ANDEQ     '025'
      *
     C     RRDV2L        IFNE      SVDV2L
     C     RRDV2H        ORNE      SVDV2H
     C                   MOVEL     STA(3)        scElement
      *
     C     SVDV2H        IFNE      *BLANKS
     C                   CAT(P)    SVDV2H:0      scElement
      *
     C                   ELSE
      *
     C                   CAT(P)    RRDV2H:0      scElement
     C                   ENDIF                                                  RRDV2H
      *
     C                   CAT(P)    '"':0         scElement
     C                   CAT(P)    'is app':1    scElement
     C                   CAT(P)    'lied':0      scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
     C                   ENDIF                                                  ELDEV2
      *
      *      Aircraft surcharge
      *
     C     RRASFL        IFNE      SVASFL
     C                   MOVEL     STA(4)        scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      Safety fees
      *
     C     RRSFAP        IFNE      SVSFAP
     C     RRSFPC        ORNE      SVSFPC
     C     RRSFAM        ORNE      SVSFAM
     C     RRSPFL        ORNE      SVSPFL
     C     RRYRAD        ORNE      SVYRAD
     C     RRCXAD        ORNE      SVCXAD
     C                   MOVEL     STA(5)        scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      State tax
      *
     C     RRTXPC        IFNE      SVTXPC
     C     RRTDFL        ORNE      SVTDFL
     C     RRIDTP        ORNE      SVIDTP
     C     RRTXHD        ORNE      SVTXHD
      *
     C     SVTXHD        IFNE      *BLANKS
     C     '"'           CAT(P)    SVTXHD        scElement
     C                   CAT(P)    '"':0         scElement
      *
     C                   ELSE
      *
     C     'STATE'       CAT(P)    'TAX':1       scElement
     C                   ENDIF                                                  RRTXHD
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      Waiver defaults
      *
     C     RRWVFL        IFNE      SVWVFL
     C     RRWDTP        ORNE      SVWDTP
     C     RRWDPC        ORNE      SVWDPC
     C     RRWDMN        ORNE      SVWDMN
     C     RRWDFC        ORNE      SVWDFC
     C                   MOVEL     STA(7)        scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      First assessment
      *
     C     RR1STP        IFNE      SV1STP
     C     RR1STL        ORNE      SV1STL
     C     RR1STH        ORNE      SV1STH
     C     RR1STF        ORNE      SV1STF
     C     RR1IDD        ORNE      SV1IDD
     C     RR1IDV        ORNE      SV1IDV
      *
     C     SV1STH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV1STH        scElement
     C                   CAT(P)    '"':0         scElement
      *
     C                   ELSE
      *
     C     'FIRST'       CAT(P)    'ASSESS':1    scElement
     C                   CAT(P)    'MENT':0      scElement
     C                   ENDIF                                                  RR1STH
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      Second assessment
      *
     C     RR2NDP        IFNE      SV2NDP
     C     RR2NDL        ORNE      SV2NDL
     C     RR2NDH        ORNE      SV2NDH
     C     RR2NDF        ORNE      SV2NDF
     C     RR2IDD        ORNE      SV2IDD
     C     RR2IDV        ORNE      SV2IDV
      *
     C     SV2NDH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV2NDH        scElement
     C                   CAT(P)    '"':0         scElement
      *
     C                   ELSE
      *
     C     'SECOND'      CAT(P)    'ASSESS':1    scElement
     C                   CAT(P)    'MENT':0      scElement
     C                   ENDIF                                                  RR2NDH
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      THIRD ASSESSMENT
      *
     C     RR3RDP        IFNE      SV3RDP
     C     RR3RDL        ORNE      SV3RDL
     C     RR3RDH        ORNE      SV3RDH
     C     RR3RDF        ORNE      SV3RDF
     C     RR3IDD        ORNE      SV3IDD
     C     RR3IDV        ORNE      SV3IDV
      *
     C     SV3RDH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV3RDH        scElement
     C                   CAT(P)    '"':0         scElement
      *
     C                   ELSE
      *
     C     'THIRD'       CAT(P)    'ASSESS':1    scElement
     C                   CAT(P)    'MENT':0      scElement
     C                   ENDIF                                                  RR3RDH
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      FOURTH ASSESSMENT
      *
     C     RR4THP        IFNE      SV4THP
     C     RR4THL        ORNE      SV4THL
     C     RR4THH        ORNE      SV4THH
     C     RR4THF        ORNE      SV4THF
     C     RR4IDD        ORNE      SV4IDD
     C     RR4IDV        ORNE      SV4IDV
      *
     C     SV4THH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV4THH        scElement
     C                   CAT(P)    '"':0         scElement
      *
     C                   ELSE
      *
     C     'FOURTH'      CAT(P)    'ASSESS':1    scElement
     C                   CAT(P)    'MENT':0      scElement
     C                   ENDIF                                                  RR4THH
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      FIFTH ASSESSMENT
      *
     C     RR5THP        IFNE      SV5THP
     C     RR5THL        ORNE      SV5THL
     C     RR5THH        ORNE      SV5THH
     C     RR5THF        ORNE      SV5THF
     C     RR5IDD        ORNE      SV5IDD
     C     RR5IDV        ORNE      SV5IDV
      *
     C     SV5THH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV5THH        scElement
     C                   CAT(P)    '"':0         scElement
      *
     C                   ELSE
      *
     C     'FIFTH'       CAT(P)    'ASSESS':1    scElement
     C                   CAT(P)    'MENT':0      scElement
     C                   ENDIF                                                  RR5THH
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      Sixth Assessment
      *
     C     RR6THP        IFNE      SV6THP
     C     RR6THL        ORNE      SV6THL
     C     RR6THH        ORNE      SV6THH
     C     RR6THF        ORNE      SV6THF
     C     RR6IDD        ORNE      SV6IDD
     C     RR6IDV        ORNE      SV6IDV
      *
     C     SV6THH        IFNE      *BLANKS
     C                   eval      scElement = '"' + %trim(sv6thh) + '"'
     C                   ELSE
     C                   eval      scElement = 'SIXTH ASSESSMENT'
     C                   ENDIF                                                  RR4THH
      *
     C                   eval      scElement = %trim(scElement) + ' information'
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      SEVENTH ASSESSMENT
      *
     C     RR7THP        IFNE      SV7THP
     C     RR7THL        ORNE      SV7THL
     C     RR7THH        ORNE      SV7THH
     C     RR7THF        ORNE      SV7THF
     C     RR7IDD        ORNE      SV7IDD
     C     RR7IDV        ORNE      SV7IDV
      *
     C     SV7THH        IFNE      *BLANKS
     C                   eval      scElement = '"' + %trim(sv7thh) + '"'
     C                   ELSE
     C                   eval      scElement = 'SEVENTH ASSESSMENT'
     C                   ENDIF                                                  RR5THH
      *
     C                   eval      scElement = %trim(scElement) + ' information'
     C
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      Deductible/premium discount information
      *
     C     RRRDPR        IFNE      SVRDPR
     C                   MOVEL     STA(10)       scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *      Stock discount applied
      *
     C     RRDSPR        IFNE      SVDSPR
     C     RRDSIN        ORNE      SVDSIN
     C     RRDSHD        ORNE      SVDSHD
     C     RRDISC        ORNE      SVDISC
     C     RRDSPC        ORNE      SVDSPC
      *
     C     SVDSHD        IFNE      *BLANKS
     C     '"'           CAT(P)    SVDSHD        scElement
     C                   CAT(P)    '"':0         scElement
      *
     C                   ELSE
      *
     C     'STOCK'       CAT(P)    'DISCOU':1    scElement
     C                   CAT(P)    'NT':0        scElement
     C                   ENDIF                                                  RRDSHD
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
     C                   ENDIF
      *
      *  Voluntary labor charge
      *
     C     RRVLCH        IFNE      SVVLCH
      *
     C     SVVLCH        IFNE      *BLANKS
     C     '"'           CAT(P)    SVVLCH        scElement
     C                   CAT(P)    '"':0         scElement
     C                   ELSE
     C     'Volun'       CAT(P)    'tary':0      scElement
     C                   CAT(P)    'labor':1     scElement
     C                   ENDIF                                                  SVVLCH
      *
     C                   CAT(P)    'infor':1     scElement
     C                   CAT(P)    'mation':0    scElement
     C                   ADD       1             rrnElement
     C                   Write     ElementSFL
      *
     C                   ENDIF
      *
      * Display info popup with changed elements.
      *
     C     rrnElement    IFNE      0
      *
      * Position grid at top at all times.
      *
     C                   eval      rrnElement = 1
     C                   move      *off          chgCancel
     C                   move      *off          chgConfirm
      *
     C                   Dou       chgCancel = *on or chgConfirm = *on
     C                   EXFMT     rcdRateChg
     C                   ENDDO
     C*
     C     chgCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ENDIF
      *
     C                   ENDIF                                                  rrnElement ifne
      *
     C                   ENDIF                                                  not error
      *
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  PFDT RTCD
      *
     C                   ENDSR                                                  CKRTCD
      *===============================================================
      * SRBILL - CHECK NO BILLING AFTER POL EFF DT
      *===============================================================
      *
     C     SRBILL        BEGSR
      /free
       cxDate7 = GetCxDate1(scCO#:scFND:scEMP#:scDIV:scFYR:xxcfdt:xxredt);
      /end-free
      *
      *  Only perform edit if employer employer not cancelled as of
      *  policy TO date.
      *
     C                   Z-ADD     cxDate7       x1CXDT            7 0
      *
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDT3             7 0
      *
     C     x1CXDT        IFNE      *ZERO
     C     SRERR         ANDEQ     *OFF
     C     x1CXDT        ANDLE     xxDT3
      *                    *** DON'T PERFORM EDIT
      *
     C                   ELSE
      *
     C                   Z-ADD     12            J                 2 0
     C                   DO        12
     C     BS(J)         IFNE      *ZERO
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             J
     C                   ENDDO
     C                   Z-ADD     J             xxHIBS            2 0
      *
     C     scBSDT        IFNE      *ZERO
     C                   Z-ADD     scBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxBGDT            7 0
     C                   ELSE
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxBGDT
     C                   ENDIF
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPTDT
      *
      * Checks how many billings for the 1st will fall within the
      * effective dates
      *
     C                   Z-ADD     xxBGDT        xxDT
      *
     C     BSON15        IFEQ      'Y'
     C                   Z-ADD     15            xxDD
     C                   ELSE
     C                   Z-ADD     1             xxDD
     C                   ENDIF
     C*
      *
     C                   Z-ADD     xxDT          xxBGDT
     C                   Z-ADD     0             xxINCT            2 0
     C     xxDT          DOWLE     xxPTDT
     C     xxDT          IFGE      xxBGDT
     C     xxDT          ANDLT     xxPTDT
     C                   ADD       1             xxINCT
     C                   ENDIF
     C                   ADD       1             xxMM
     C     xxMM          IFEQ      13
     C                   Z-ADD     1             xxMM
     C                   ADD       1             xxYY
     C                   ENDIF
     C                   ENDDO
      *
     C     xxHIBS        IFGT      xxINCT
     C                   move      *on           error
     C                   move      *on           erBSCD
     C                   movel(p)  'WC31347'     msBSCD
     C                   ENDIF
      *
     C                   ENDIF                                                  CXDT
      *
     C                   ENDSR                                                  SRBILL
      *
     C**************************************************************

     P ErrPolRating    E
     P*
       //***************************************************************************//
       //===================================================================
       // ErrAddInfo -   Edit the additional info fields from different screens/programs.
       //                The additional info screens are contained in the policy detail
       //                program and the additional info popup window.
       //
       //   Call 'A'     Additional Info          Screen format #1 (WTPOLDTL/rcdAddInfo)
       //
       //                The procedure will return an *OFF value if an edit error is found,
       //                or an *ON value if all edits are passed.
       //
       //===================================================================
     P ErrAddInfo      B                   export
     D ErrAddInfo      PI              n
     d inCall                         1    const
     d inMode                         1    const
     d inQuote                        1    const
     d inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     d inELrrn                        9  0 const
     d inData                       500
     d*
     D scCo#           s                   like(ELCo#)
     D scFnd           s                   like(ELFnd)
     D scEmp#          s                   like(ELEmp#)
     D scDiv           s                   like(ELDiv)
     D scFyr           s                   like(ELFyr)
     d*
     D qteMode         s               n   inz(*off)
     d*
     d error           s               n   inz
     D message         s            100
     d*
     D xxKey           s                   like(BSKEY)
     D xxTBID          s                   like(GTTBID)
     D sysDt7          s              7  0
     D sysDt8          s              8  0

      *** Beg Add ***** 06/24/24 **********************************************
     D workDate        s              8  0
      *** End Add ***** 06/24/24 **********************************************

     d ptrFormatA      s               *   inz
       //*** Beg Add *** 01/23/23 *********************************************
       dcl-s intCar# Like(F2Car#);
       //*** End Add *** 01/23/23 *********************************************
     d*
     d dsAddInfo     e ds                  extname(wtpoldtl:rcdAddInfo)
     d                                     based(ptrFormatA)
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
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
     C* Group Carrier Information File Key List
     C*
     C     keyWMGCL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
     C* Generic Table File Key List
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C     keyWTGIL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbid
     C*
     C* Flexible Heading File Key List
     C*
     C     keyWDFHL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
     C*  CARRIER/STATE REPORTING MASTER FILE
     C*
     C     keyWMRPL3     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRPCAP#
     C*
      /Free

       Open WTPOLDTL;

       dsRateEdit = inData;

       Select;
          When inCall = 'A';
             ptrFormatA = inPtrFormat1;
          Other;
             Return *off;
       Endsl;

       // Setup the files and fields necessary for editing.
       Exsr Data_Setup;

       // Process Edits
       Exsr EditAddInfo;

       Close WTPOLDTL;
       inData = dsRateEdit;

       Return not error;

      /End-Free

      //===================================================================
      // Data_Setup - Load all the necessary files and fields for editing.
      //===================================================================
     C     Data_Setup    Begsr
      /free

       Clear error;

       sysDt7 = DateTo7(SystemDate);
       sysDt8 = DateTo8(SystemDate);

       scCo#  = inCo#;
       scFnd  = inFnd;
       scEmp# = inEmp#;
       scDiv  = inDiv;
       scFyr  = inFyr;

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;
       Chain keyWMEXL PEX00;
       Chain keyWDELL pel00;
       Chain keyWMEML PEM00;
       Chain keyWDFHL PFH00;
       //*** Beg Add *** 01/23/23 *********************************************
       intCar# = *zeros;
       Chain (scCo#:scFnd) pf201;
       If %Found(WDF2L);
          intCar# = F2car#;
       Endif;
       //*** End Add *** 01/23/23 *********************************************

       GetClientID(clientID);

       qteMode = *off;
       If inQuote = 'Q';
          qteMode = *on;
       Endif;

      /end-free
     C                   Endsr                                                  Data_Setup Subr

     C**************************************************************
     C*  Additional Info Edits
     C**************************************************************
     C     EditAddInfo   Begsr
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
     C* Set Requirements from Group Carrier Information
     C*
     c                   move      *blank        CarrReq           1
     c                   move      *blank        CarrDup           1
     C*
     C     keyWMGCL      CHAIN     PGC00                              60
     C     *IN60         IFEQ      *OFF
      *
     C     GCCRFL        IFEQ      'Y'
     C                   move      *on           CarrReq
     C                   ENDIF                                                  GCCRFL = Y
      *
     C     GCDCFL        IFEQ      'Y'
     C                   move      *on           CarrDup
     C                   ENDIF                                                  GCCRFL = Y
      *
     C                   ENDIF                                                  *IN60
     C*
     C* CHECK CARRIER CODE AGAINST SYSTEM TABLE VALUE FILE
     C*
     C                   If        qteMode <> *on
     C*
     C                   If        scRPCARR <> *blanks or CarrReq = *on
     C*
     C                   MOVEL(p)  'CRR'         xxTbid
     C                   MOVEL(p)  scRPCARR      xxKey
     C*
     C     keyWTGTL      CHAIN     pgt00
     C*
     C                   IF        not %found
     C                             and erRPCARR = *OFF
     C                   move      *on           error
     C                   move      *on           erRPCARR
     C                   movel(p)  'WCR0505'     msRPCARR
     C                   ENDIF
      *
      * Do not allow entry of inactive carrier code
      *
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDNE     'A'
     C     erRPCARR      ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erRPCARR
     C                   movel(p)  'WCR0690'     msRPCARR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* VALIDATE ISSUANCE DATE IF ENTERED
     C*
     C     scRPISDT      IFNE      *ZEROS
     C*
     C                   Z-ADD     scRPISDT      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scRPISDT
     C*
     C     SRERR         IFEQ      *ON
     C     erRPISDT      ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erRPISDT
     C                   movel(p)  'WCR0500'     msRPISDT
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  SCRPISDT NE 0
     C*
     C* VALIDATE START DATE IF ENTERED
     C*
     C     scRPEFDT      IFNE      *ZEROS
     C*
     C                   Z-ADD     scRPEFDT      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scRPEFDT
     C*
     C     SRERR         IFEQ      *ON
     C     erRPEFDT      ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erRPEFDT
     C                   movel(p)  'WCR0500'     msRPEFDT
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  SCRPEFDT NE 0
     C*
     C* VALIDATE END DATE IF ENTERED
     C*
     C     scRPEXDT      IFNE      *ZEROS
     C*
     C                   Z-ADD     scRPEXDT      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scRPEXDT
     C*
     C     SRERR         IFEQ      *ON
     C     erRPEXDT      ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erRPEXDT
     C                   movel(p)  'WCR0500'     msRPEXDT
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  SCRPEXDT NE 0
     C*
     C* START DATE CANNOT BE GREATER THAN END DATE
     C*
     C                   Z-ADD     scRPEFDT      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEFDT            7 0
     C*
     C                   Z-ADD     scRPEXDT      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEXDT            7 0
     C*
     C     xxEFDT        IFGT      xxEXDT
     C     erRPEFDT      ANDEQ     *OFF
     C     erRPEXDT      ANDEQ     *OFF
     C                   move      *on           error
     C                   move      *on           erRPEFDT
     C                   move      *on           erRPEXDT
     C                   movel(p)  'WCR0529'     msRPEFDT
     C                   movel(p)  'WCR0529'     msRPEXDT
     C                   ENDIF                                                  EFDT > EXDT
     C*
      //*** Beg Add ***** 06/24/24 *******************************
      *** Collateral Info Validation
       // Collateral Percentage/Flat Amount - Edits
       If scColwp < 0;
          errColwp = *On;
          msColwp = 'WCR0547';
          Error = *On;
       Endif;

       If scColrp < 0;
          errColrp = *On;
          msColrp = 'WCR0547';
          Error = *On;
       Endif;

       If scColwa < 0;
          errColwa = *On;
          msColwa = 'WCR0547';
          Error = *On;
       Endif;

       // Validate LOC issue Date - if entered
       If scCollid <> 0;
          workDate = scCollid;
          errCollid = ValidDate(workDate:message);
          If errCollid = *On;
             msCollid = message;
             Error = *On;
          Endif;
       Endif;

       // Validate LOC expiration Date - if entered
       If scColled <> 0;
          workDate = scColled;
          errColled = ValidDate(workDate:message);
          If errColled = *On;
             msColled = message;
             Error = *On;
          Endif;
       Endif;

       // LOC Dates must be in Ascending Order - if entered
       If (errCollid <> *On) and (errColled <> *On) and (scColled <> 0);
          If AscendDates(scCollid:scColled:message) = *On;
             errCollid = *On;
             errColled = *On;
             msCollid = message;
             msColled = message;
             Error = *On;
          Endif;
       Endif;

       // Validate Surety Bond issue Date - if entered
       If scColsid <> 0;
          workDate = scColsid;
          errColsid = ValidDate(workDate:message);
          If errColsid = *On;
             msColsid = message;
             Error = *On;
          Endif;
       Endif;

       // Validate Surety Bond expiration Date - if entered
       If scColsed <> 0;
          workDate = scColsed;
          errColsed = ValidDate(workDate:message);
          If errColsed = *On;
             msColsed = message;
             Error = *On;
          Endif;
       Endif;

       // Surety Bond Dates must be in Ascending Order - if entered
       If (errColsid <> *On) and (errColsed <> *On) and (scColsed <> 0);
          If AscendDates(scColsid:scColsed:message) = *On;
             errColsid = *On;
             errColsed = *On;
             msColsid = message;
             msColsed = message;
             Error = *On;
          Endif;
       Endif;
      //*** End Add ***** 06/24/24 *******************************
     C* CARRIER POLICY REQUIRED, IF SELECTED IN GROUP CARRIER INFO
     C*
     C     CarrReq       IFEQ      *ON
     C     erRPCAP#      ANDEQ     *OFF
     C*
     C     scRPCAP#      IFEQ      *BLANKS
     C                   move      *on           error
     C                   move      *on           erRPCAP#
     C                   movel(p)  'WCR0503'     msRPCAP#
     C                   ENDIF                                                  SCCAP#
     C*
     C                   ENDIF                                                  If CarrReq
     C*
     C* CARRIER POLICY CANNOT BE DUPLICATE IF FLAG IS SET
     C*
     C     CarrDup       IFEQ      *ON
     C     erRPCAP#      ANDEQ     *OFF
     C*
     C     keyWMRPL3     CHAIN     PRP03                              60
     C     *IN60         DOWEQ     *OFF
     c*
       //*** Beg Add *** 01/23/23 *********************************************
       Chain (RPCo#:RPfnd) pf201;
       If Not %Found(WDF2L);
          F2car# = *zeros;
       Endif;
       //*** End Add *** 01/23/23 *********************************************
       //*** Beg Chg *** 01/23/23 *********************************************
     c*********          If        rpfnd <> scfnd or rpemp# <> scemp#
                         If (F2car# = intCar#) and (rpfnd <> scfnd or
                          RPemp# <> scEmp#);
       //*** End Chg *** 01/23/23 *********************************************
     C                   move      *on           error
     C                   move      *on           erRPCAP#
     C                   movel(p)  'WC30005'     msRPCAP#
     c                   leave
     C                   ENDIF                                                  *IN60
     c*
     C     keyWMRPL3     Reade     PRP03                                  60
     c                   ENDDO
     C*
     C                   ENDIF                                                  If CarrDup
     C*
     C                   ENDIF                                                  If qteMode <> *on
     C*
     C* Cancellation notice days cannot be less than zero.
     C*
     C                   if        scCXDY < 0
     C                             and erCXDY <> *on
     C                   move      *on           error
     C                   move      *on           erCXDY
     C                   movel(p)  'WCR0547'     msCXDY
     C                   endif
     C*
     C* Additional mailing notice days cannot be less than zero.
     C*
     C                   if        scADDM < 0
     C                             and erADDM <> *on
     C                   move      *on           error
     C                   move      *on           erADDM
     C                   movel(p)  'WCR0547'     msADDM
     C                   endif
     C*
     C* Adverse account flag must be valid.
     C*
     C                   if        scADVR <> 'Y' and scADVR <> *blank
     C                             and erADVR <> *on
     C                   move      *on           error
     C                   move      *on           erADVR
     C                   movel(p)  'WCR1241'     msADVR
     C                   endif
     C*
     C* Do not renew flag must be valid.
     C*
     C                   if        scRLFL <> 'Y' and scRLFL <> *blank
     C                             and erRLFL <> *on
     C                   move      *on           error
     C                   move      *on           erRLFL
     C                   movel(p)  'WCR1241'     msRLFL
     C                   endif
     C*
     C* Exclude Medical flag must be valid.
     C*
     C                   if        scEXMF <> 'Y' and scEXMF <> *blank
     C                             and erEXMF <> *on
     C                   move      *on           error
     C                   move      *on           erEXMF
     C                   movel(p)  'WCR0571'     msEXMF
     C                   endif
     C*
     C* Contested Audit flag must be valid.
     C*
     C                   if        scCTST <> 'Y' and scCTST <> *blank
     C                             and erCTST <> *on
     C                   move      *on           error
     C                   move      *on           erCTST
     C                   movel(p)  'WCR0571'     msCTST
     C                   endif
     C*
     C* Accumulate Premium flag must be valid.
     C*
     C                   if        scAPFL <> 'Y' and scAPFL <> *blank
     C                             and erAPFL <> *on
     C                   move      *on           error
     C                   move      *on           erAPFL
     C                   movel(p)  'WCR0571'     msAPFL
     C                   endif
     C*
     C* Assign Audit Company flag must be valid.
     C*
     C                   if        scAAFL <> 'Y' and scAAFL <> *blank
     C                             and erAAFL <> *on
     C                   move      *on           error
     C                   move      *on           erAAFL
     C                   movel(p)  'WCR0571'     msAAFL
     C                   endif
      *
     c*
     c* Validate Flex field #1
     c*
     C                   if        flex1 = *on
     c*
     c* Verify freeform entry field.
     c*
     C                   if        flexBox1 = *on
     c*
     C                   if        XXRQD1 = 'Y' and SCFFD1 = ' '
     C                             and erFFD1 <> *on
     C                   move      *on           error
     C                   move      *on           erFFD1
     C                   movel(p)  'WCR0974'     msFFD1
     C                   endif
     C*
     C                   else                                                   flexBox1 (else)
     c*
     c* Verify dropdown entry field.
     c*
     C                   if        XXRQD1 = 'Y' and scFFDD1 = ' '
     C                             and erFFDD1 <> *on
     C                   move      *on           error
     C                   move      *on           erFFDD1
     C                   movel(p)  'WCR0974'     msFFDD1
     C                   endif
     c*
     C                   IF        scFFDD1 <> *BLANKS
     C                             and erFFDD1 <> *on
     c*
     C                   if        flex = 'N'
     C                   MOVEL(p)  EXTBD1        xxTBID
     C                   else
     C                   MOVEL(p)  FHF1TB        xxTBID
     C                   ENDIF                                                  flex = 'N'
     c*
     C     keyWTGIL      CHAIN     pgi00                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(p)  scFFDD1       xxKEY
     C*
     C     keyWTGTL      CHAIN     pgt00                              99
     C     *IN99         IFEQ      *ON
     C     erFFDD1       ANDNE     *on
     C                   move      *on           error
     C                   move      *on           erFFDD1
     C                   movel(p)  'WCR0513'     msFFDD1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *in99 = *off
     C*
     C                   ENDIF                                                  scFFDD1 <> *blanks
     C*
     C                   ENDIF                                                  flexBox1 = *on
     C*
     C                   ENDIF                                                  flex1 = *on
     C*
     c* Validate Flex field #2
     c*
     C                   if        flex2 = *on
     c*
     c* Verify freeform entry field.
     c*
     C                   if        flexBox2 = *on
     c*
     C                   if        XXRQD2 = 'Y' and scFFD2 = ' '
     C                             and erFFD2 <> *on
     C                   move      *on           error
     C                   move      *on           erFFD2
     C                   movel(p)  'WCR0974'     msFFD2
     C                   endif
     C*
     C                   else                                                   flexBox2 (else)
     c*
     c* Verify dropdown entry field.
     c*
     C                   if        XXRQD2 = 'Y' and scFFDD2 = ' '
     C                             and erFFDD2 <> *on
     C                   move      *on           error
     C                   move      *on           erFFDD2
     C                   movel(p)  'WCR0974'     msFFDD2
     C                   endif
     c*
     C                   IF        scFFDD2 <> *BLANKS
     C                             and erFFDD2 <> *on
     c*
     C                   if        flex = 'N'
     C                   MOVEL(p)  EXTBD2        xxTBID
     C                   else
     C                   MOVEL(p)  FHF2TB        xxTBID
     C                   ENDIF                                                  flex = 'N'
     c*
     C     keyWTGIL      CHAIN     pgi00                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(p)  scFFDD2       xxKEY
     C*
     C     keyWTGTL      CHAIN     pgt00                              99
     C     *IN99         IFEQ      *ON
     C     erFFDD2       ANDNE     *on
     C                   move      *on           error
     C                   move      *on           erFFDD2
     C                   movel(p)  'WCR0513'     msFFDD2
     C                   ENDIF
     C*
     C                   ENDIF                                                  *in99 = *off
     C*
     C                   ENDIF                                                  scFFDD2 <> *blanks
     C*
     C                   ENDIF                                                  flexBox2 = *on
     C*
     C                   ENDIF                                                  flex2 = *on
     C*
     c* Validate Flex field #3
     c*
     C                   if        flex3 = *on
     c*
     c* Verify freeform entry field.
     c*
     C                   if        flexBox3 = *on
     c*
     C                   if        XXRQD3 = 'Y' and scFFD3 = ' '
     C                             and erFFD3 <> *on
     C                   move      *on           error
     C                   move      *on           erFFD3
     C                   movel(p)  'WCR0974'     msFFD3
     C                   endif
     C*
     C                   else                                                   flexBox3 (else)
     c*
     c* Verify dropdown entry field.
     c*
     C                   if        XXRQD3 = 'Y' and scFFDD3 = ' '
     C                             and erFFDD3 <> *on
     C                   move      *on           error
     C                   move      *on           erFFDD3
     C                   movel(p)  'WCR0974'     msFFDD3
     C                   endif
     c*
     C                   IF        scFFDD3 <> *BLANKS
     C                             and erFFDD3 <> *on
     c*
     C                   MOVEL(p)  FHF3TB        xxTBID
     c*
     C     keyWTGIL      CHAIN     pgi00                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(p)  scFFDD3       xxKEY
     C*
     C     keyWTGTL      CHAIN     pgt00                              99
     C     *IN99         IFEQ      *ON
     C     erFFDD3       ANDNE     *on
     C                   move      *on           error
     C                   move      *on           erFFDD3
     C                   movel(p)  'WCR0513'     msFFDD3
     C                   ENDIF
     C*
     C                   ENDIF                                                  *in99 = *off
     C*
     C                   ENDIF                                                  scFFDD3 <> *blanks
     C*
     C                   ENDIF                                                  flexBox3 = *on
     C*
     C                   ENDIF                                                  flex3 = *on
     C*
     C* Validate flexible field 4
     C*
     C                   if        flex4 = *on
     C*
     C                   if        XXRQD4 = 'Y' and SCFFD4 = ' '
     C                             and erFFD4 <> *on
     C                   move      *on           error
     C                   move      *on           erFFD4
     C                   movel(p)  'WCR0974'     msFFD4
     C                   endif
     C*
     C                   endif                                                  flex4 = *on
     C*
     C* Validate flexible field 5
     C*
     C                   if        flex5 = *on
     C*
     C                   if        XXRQD5 = 'Y' and SCFFD5 = 0
     C                             and erFFD5 <> *on
     C                   move      *on           error
     C                   move      *on           erFFD5
     C                   movel(p)  'WCR0974'     msFFD5
     C                   endif
     C*
     C                   endif                                                  flex5 = *on

      /free
       Clear erDivdPlan;
       Clear msDivdPlan;
       Clear erFlatPrct;
       Clear msFlatPrct;
       Clear erFlatDef;
       Clear msFlatDef;
       // Run edit if bill has been run. The edit in WTPOLDTL will
       //  prevent assigning a Dividend Plan to a non-billed policy.
       //  This will allow changes to be made to the policy if
       //  a Dividend Plan is rolled to a renewal year.
       If scDivdPlan <> *blanks and ELfaf <> *blanks;
          erDivdPlan = not(ValidDivPlan (ELco# : ELfnd : ELemp# :
           ELdiv : ELfyr : scDivdPlan));
          If erDivdPlan = *on;
             error = *on;
             msDivdPlan = 'WC40149';
          Else;
             Chain (ELco#:scDivdPlan) PDV011;
             If (DVptyp = 'F' or DVptyp = 'C') and scFlatPrct = 0;
                erFlatPrct = *on;
                error = *on;
                msFlatPrct = 'WC40150';
             Endif;
             If DVptyp = 'S' and scFlatPrct <> 0;
                erFlatPrct = *on;
                error = *on;
                msFlatPrct = 'WC40151';
             Endif;
             If scFlatDef = 'Y';
                xxKey = %editc(scBSCD:'X');
                Chain (ELco# : xxKey) PBS01;
                If BSSefl <> 'E' or DVptyp = 'S';
                   erFlatDef = *on;
                   error = *on;
                   msFlatDef = 'WC40154';
                Endif;
             Endif;
          Endif;
       Endif;
       If scDivdplan = *blanks;
          If scFlatPrct <> 0;
             erFlatPrct = *on;
             error = *on;
             msFlatPrct = 'WC40152';
          Endif;
          If scFlatDef = 'Y';
             erFlatDef = *on;
             error = *on;
             msFlatDef = 'WC40153';
          Endif;
       Endif;

       /end-free
     C                   EndSr
     C**************************************************************

     P ErrAddInfo      E
     P*
       //***************************************************************************//
       //===================================================================
       // ErrLargeDed -  Edit the large deductible info fields from different screens/programs.
       //                The large deductible info screens are contained in the policy detail
       //                program and the large deductible info popup window.
       //
       //   Call 'D'     Large Deductible Info    Screen format #1 (WTPOLDTL/rcdLrgDed)
       //
       //                The procedure will return an *OFF value if an edit error is found,
       //                or an *ON value if all edits are passed.
       //
       //===================================================================
     P ErrLargeDed     B                   export
     D ErrLargeDed     PI              n
     d inCall                         1    const
     d inMode                         1    const
     d inQuote                        1    const
     d inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     d inELrrn                        9  0 const
     d inData                       500
     d*
     D scCo#           s                   like(ELCo#)
     D scFnd           s                   like(ELFnd)
     D scEmp#          s                   like(ELEmp#)
     D scDiv           s                   like(ELDiv)
     D scFyr           s                   like(ELFyr)
     d*
     D qteMode         s               n   inz(*off)
     d*
     d error           s               n   inz
     D message         s            100
     d*
     D xxKey           s                   like(BSKEY)
     D xsKey           s              2
     D xxArty          s                   like(EAARTY)
     D xtotalDedB      s             14  2
     D xxTBID          s                   like(GTTBID)
     d xxDat1          s              7  0
     d xxDat2          s              7  0
     d xxPFDT          s              7  0
     d xxPTDT          s              7  0
     D sysDt7          s              7  0
     D sysDt8          s              8  0

     d ptrFormatD      s               *   inz
     d*
     d dsLargeDed    e ds                  extname(wtpoldtl:rcdLrgDed)
     d                                     based(ptrFormatD)
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
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
     C*
     C* Generic Table File Key List
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C* Reserve Code File Key List
     C*
     C     keyWTRTL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
     C*
     C* Deductible Detail File Key List
     C*
     C     keyWDDDL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
     C*
     C* Table File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
     C*
     C* A/R Detail File Key List
     C*
     C     keyWMEAL10    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    xxArty
     C*
     C     keyWMLDL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      /Free

       Open WTPOLDTL;

       dsRateEdit = inData;

       Select;
          When inCall = 'D';
             ptrFormatD = inPtrFormat1;
          Other;
             Return *off;
       Endsl;

       // Setup the files and fields necessary for editing.
       Exsr Data_Setup;

       // Process Edits
       Exsr EditLargeDed;

       Close WTPOLDTL;
       inData = dsRateEdit;

       Return not error;

      /End-Free

      //===================================================================
      // Data_Setup - Load all the necessary files and fields for editing.
      //===================================================================
     C     Data_Setup    Begsr
      /free

       Clear error;

       sysDt7 = DateTo7(SystemDate);
       sysDt8 = DateTo8(SystemDate);

       scCo#  = inCo#;
       scFnd  = inFnd;
       scEmp# = inEmp#;
       scDiv  = inDiv;
       scFyr  = inFyr;

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;
       Chain keyWMEXL PEX00;
       Chain keyWDELL pel00;
       Chain keyWMEML PEM00;

       GetClientID(clientID);

       qteMode = *off;
       If inQuote = 'Q';
          qteMode = *on;
       Endif;

       xsKey = 'WC';
       Chain xsKey SWC01;

      /end-free
     C                   Endsr                                                  Data_Setup Subr

     C**************************************************************
     C*  Large Deductible Info Edits
     C**************************************************************
     C     EditLargeDed  Begsr
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
     C                   Z-ADD     x5PFDT        xxPFDT
     C                   Z-ADD     x5PTDT        xxPTDT
     c*
     C* Check the balance in Dedcutible billing
     c*
     C                   Movel     'F'           xxArty
     C                   Z-Add     0             xtotalDedB
     C
     C     keyWMEAL10    Chain     PEA10                              60
     C                   Dow       *in60 = *off
     C                   movel(p)  eaarcd        xxKey
     C
     C     keyWTABL      chain     ptt01
     C                   If        *in60 = *off
     C
     C                   if        ttctyp = 'R'
     C
     C                   Eval      xtotalDedB = xtotalDedB + EAAMT
     C                   endif
     C
     C                   endif
     C
     C     keyWMEAL10    Reade     PEA10                                  60
     C                   enddo
     C*
     C*
     C* IF BILLING RECORDS RESIDE FOR THIS POLICY YEAR, DEDUCTIBLE DATA
     C* CANNOT BE CLEARED"
     C*
     C     scLDEFD1      IFEQ      *ZERO
     C     erLDEFD1      ANDNE     *ON
     C*
     C     scLDEFD1      IFNE      LDEFD1
     C*
     C     keyWDDDL1     CHAIN     PDD01                              60
     C     *IN60         IFEQ      *OFF
     C                   move      *on           error
     C                   move      *on           erLDEFD1
     C                   movel(p)  'WC40073'     msLDEFD1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* If Deductible A/R billings exist, Large Deductible Info can not be cleared
     C*
     C     keyWMLDL1     CHAIN(n)  PLD01                              60
     C     *IN60         IFEQ      *OFF
     C     scLDEFD1      ANDEQ     *ZERO
     C     XTOTALDEDB    ANDNE     0
     C     erLDEFD1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEFD1
     C                   movel(p)  'WCR3371'     msLDEFD1
     C                   ENDIF
     c*
     C* SCR01 data must be entered, if SCR02 data exists.
     c*
     C                   if        scLDEfD1 = *zero
     C                              and scLDEfD2 <> *zero
     C                              and erLDEFD1 <> *ON
     C                   move      *on           error
     C                   move      *on           erLDEFD1
     C                   movel(p)  'WC32432'     msLDEFD1
     C                   endif
     C*
     C* Plan #1 fields.
     C*
     C* EDIT POLICY FROM DATE
     C*
     C     scLDEFD1      IFNE      *ZEROS
     C*
     C                   Z-ADD     SCLDEFD1      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLDEFD1
     C*
     C     SRERR         IFEQ      *ON
     C     erLDEFD1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEFD1
     C                   movel(p)  'WCR0500'     msLDEFD1
     C                   ENDIF
     C                   ENDIF
     C*
     C* EDIT POLICY EXPRIRATION DATE
     C*
     C     scLDEFD1      IFNE      *ZEROS
     C     scLDEXP1      ANDNE     *ZEROS
     C                   Z-ADD     scLDEXP1      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scLDEXP1
     C     SRERR         IFEQ      *ON
     C     erLDEXP1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEXP1
     C                   movel(p)  'WCR0500'     msLDEXP1
     C                   ENDIF
     C                   ENDIF
       //*** Begin Add *** 02/10/23 *******************************************
      /free
           If erLDEXP1 <> *on and scLDEXP1 <> *zeros and scLDRNW1 <> *blanks;
              error = *on;
              erLDEXP1 = *on;
              msLDEXP1 = 'WCR1022';
              erLDRNW1 = *on;
              msLDRNW1 = 'WCR1022';
           Endif;
      /end-free
       //*** End   Add *** 02/10/23 *******************************************
     C*
     C                   Z-ADD     scLDEFD1      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDAT1
     C*
     C                   Z-ADD     scLDEXP1      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDAT2
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C     scLDEFD1      IFNE      *ZEROS
     C     scLDEXP1      ANDNE     *ZEROS
     C*
     C     xxDAT2        IFLT      xxDAT1
     C     erLDEFD1      ANDNE     *ON
     C     erLDEXP1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEFD1
     C                   move      *on           erLDEXP1
     C                   movel(p)  'WCR0529'     msLDEFD1
     C                   movel(p)  'WCR0529'     msLDEXP1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  BOTH DATES MUST BE BETWEEN FUND POLICY START & END DATES
     C*
     C*  if policy is cancelled whereby pol eff and pol exp dates are equal
     C*    then bypass the following date checking
     C*
     C     xxPFDT        IFNE      xxPTDT

     C     scLDEFD1      IFNE      *ZEROS
     C     erLDEFD1      ANDNE     *ON
     C*

     C     xxDAT1        IFLT      xxPFDT
     C     xxDAT1        ORGE      xxPTDT
     C                   move      *on           error
     C                   move      *on           erLDEFD1
     C                   movel(p)  'WC32433'     msLDEFD1
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C     scLDEXP1      IFNE      *ZEROS
     C     erLDEXP1      ANDNE     *ON
     C*
     C     xxDAT2        IFLT      xxPFDT
     C     xxDAT2        ORGT      xxPTDT
     C                   move      *on           error
     C                   move      *on           erLDEXP1
     C                   movel(p)  'WC32433'     msLDEXP1
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C* EFFETIVE DATE CAN NOT BE ZERO
     C*
     C     scLDEFD1      IFEQ      *ZEROS
     C     erLDEFD1      ANDNE     *ON
     C*
     C     scLDDES1      IFNE      *BLANKS
     C     scLDPCT1      ORNE      *ZEROS
     C     scLDTYP1      ORNE      *BLANKS
     C     scLDAMP1      ORNE      *ZEROS
     C     scLDAMG1      ORNE      *ZEROS
     C     scLDPLN1      ORNE      *BLANKS
     C     scLDVRB1      ORNE      *BLANKS
     C     scLDOCV1      ORNE      *BLANKS
     C     scLDTOT1      ORNE      *BLANKS
     C     scLDCLS1      ORNE      *ZEROS
     C     scLDPLV1      ORNE      *BLANKS
     C     scLDALA1      ORNE      *BLANKS
     C     scLDRNW1      ORNE      *BLANKS
     C     scLDR101      ORNE      *BLANKS
     C     scLDR102      ORNE      *BLANKS
     C     scLDR103      ORNE      *BLANKS
     C     scLDR104      ORNE      *BLANKS
     C     scLDR105      ORNE      *BLANKS
     C     scLDR106      ORNE      *BLANKS
     C     scLDR107      ORNE      *BLANKS
     C     scLDR108      ORNE      *BLANKS
     C     scLDR109      ORNE      *BLANKS
     C     scLDR110      ORNE      *BLANKS
     C     scADP1        ORNE      *ZEROS
     C                   move      *on           error
     C                   move      *on           erLDEFD1
     C                   movel(p)  'WCR0503'     msLDEFD1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   DESCRIPTION IS REQUIRED (IF EFFECTIVE DATE IS ENTERED)
     C*
     C                   if        scLDefd1 <> *zeros
     C*
     C     scLDDES1      IFEQ      *BLANKS
     C     erLDDES1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDDES1
     C                   movel(p)  'WCR0503'     msLDDES1
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
      *
     C*   GROSS/NET FLAG SHOULD BE 'G' OR 'N'
      *
     C     scLDTYP1      IFNE      'G'
     C     scLDTYP1      ANDNE     'N'
     C     scLDTYP1      ANDNE     *BLANKS
     C     erLDTYP1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDTYP1
     C                   movel(p)  'WCR1087'     msLDTYP1
     C                   ENDIF                                                              O
      *
     C                   if        scLDefd1 <> *zeros
      *
     C     scLDTYP1      IFEQ      *BLANKS
     C     erLDTYP1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDTYP1
     C                   movel(p)  'WCR0503'     msLDTYP1
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                              O
      *
     C*   Deductible amount can not be less than zero
     C                   if        scLDDes1 <> ' '
     C                              and scLDAmp1 < *zero
     C                              and erLDAmp1 <>*ON
     C                   move      *on           error
     C                   move      *on           erLDAmp1
     C                   movel(p)  'WCR0547'     msLDAmp1
     C                   endif                                                              O
      *
     C*   VALIDATE DEDUCTIBLE PLAN TYPE.....
      *
     C     WCFL03        IFEQ      'Y'
      *
     C     scLDPLN1      IFNE      *BLANKS
     C                   MOVEL(P)  '  DT'        xxTBID
     C                   MOVEL(P)  scLDPLN1      xxKEY
     C                   if        scLDdes1 <> ' '
     C                             and erLDPln1 <>*ON
     C     keyWTGTL      CHAIN     pgt00                              60
     C     *in60         ifeq      *on
     C     *in60         oreq      *off
     C     GTSTAT        andne     'A'
     C                   move      *on           error
     C                   move      *on           erLDPln1
     C                   movel(p)  'WCR1087'     msLDPln1
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                  WCFL03 = 'Y'
      *
     C* PER CLAIM/PER OCCURRENCE FLAG SHOULD BE 'C' OR 'O'
      *
     C     scLDVRB1      IFNE      'C'
     C     scLDVRB1      ANDNE     'O'
     C     scLDVRB1      ANDNE     *BLANKS
     C     erLDVRB1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDVrb1
     C                   movel(p)  'WCR1087'     msLDVrb1
     C                   ENDIF                                                              O
     C                   if        scLDdes1 <> ' 'and scLDvrb1 = *blanks
     C                             and erLDPln1 <>*ON
     C                   move      *on           error
     C                   move      *on           erLDVrb1
     C                   movel(p)  'WCR0503'     msLDVrb1
     C                   ENDIF                                                              O
      *
     C*  OCCUPATIONAL DISEASE ONLY: PER CLAIM/PER OCCURRENCE FLAG
      * SHOULD BE 'C' OR 'O' OR BLANKS
      *
     C     scLDOCV1      IFNE      'C'
     C     scLDOCV1      ANDNE     'O'
     C     scLDOCV1      ANDNE     *BLANKS
     C     erLDOCV1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDOcv1
     C                   movel(p)  'WCR1087'     msLDOcv1
     C                   ENDIF                                                              O
      *
     C*  STATISTICAL CLASS CODE IS REQUIRED....
      *
     C     WCFL03        IFEQ      'Y'
     C                   if        scLDdes1 <> ' '
     C     scLDCLS1      IFEQ      *ZERO
     C     erLDCLS1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDCls1
     C                   movel(p)  'WCR0503'     msLDCls1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*USE TOTAL INCURRED FOR DEDUCTIBLE BILLING SHOULD BE 'Y' OR BLANKS
      *
     C     scLDTOT1      IFNE      'Y'
     C     scLDTOT1      ANDNE     *BLANKS
     C     erLDTOT1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDTOT1
     C                   movel(p)  'WCR1241'     msLDTOT1
     C                   ENDIF
      *
     C*   VALIDATE BILLING CALULATION LEVEL.....
      *
     C     scLDPCT1      IFNE      *ZERO
     C     erLDPLV1      ANDNE     *ON
     C                   MOVEL(P)  'BCLV'        xxTBID
     C                   MOVEL(P)  scLDPLV1      xxKEY
     C                   if        scLDdes1 <> ' '
     C     keyWTGTL      CHAIN     pgt00                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   move      *on           error
     C                   move      *on           erLDPlv1
     C                   movel(p)  'WCR1087'     msLDPlv1
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR101
     C*
     C     scLDR101      IFNE      *BLANKS
     C     erLDR101      ANDNE     *ON
     C                   MOVEL(P)  scLDR101      xxKEY
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR101
     C                   movel(p)  'WCR1087'     msLDR101
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR102
     C*
     C     scLDR102      IFNE      *BLANKS
     C     erLDR102      ANDNE     *ON
     C                   MOVEL(P)  scLDR102      xxKEY
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR102
     C                   movel(p)  'WCR1087'     msLDR102
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR103
     C*
     C     scLDR103      IFNE      *BLANKS
     C     erLDR103      ANDNE     *ON
     C                   MOVEL(P)  scLDR103      xxKEY
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR103
     C                   movel(p)  'WCR1087'     msLDR103
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR104
     C*
     C     scLDR104      IFNE      *BLANKS
     C     erLDR104      ANDNE     *ON
     C                   MOVEL(P)  scLDR104      xxKey
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR104
     C                   movel(p)  'WCR1087'     msLDR104
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR105
     C*
     C     SCLDR105      IFNE      *BLANKS
     C     erLDR105      ANDNE     *ON
     C                   MOVEL(P)  SCLDR105      xxKey
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR105
     C                   movel(p)  'WCR1087'     msLDR105
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR106
     C*
     C     SCLDR106      IFNE      *BLANKS
     C     erLDR106      ANDNE     *ON
     C                   MOVEL(P)  SCLDR106      xxKey
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR106
     C                   movel(p)  'WCR1087'     msLDR106
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR107
     C*
     C     SCLDR107      IFNE      *BLANKS
     C     erLDR107      ANDNE     *ON
     C                   MOVEL(P)  SCLDR107      xxKey
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR107
     C                   movel(p)  'WCR1087'     msLDR107
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR108
     C*
     C     SCLDR108      IFNE      *BLANKS
     C     erLDR108      ANDNE     *ON
     C                   MOVEL(P)  SCLDR108      xxKey
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR108
     C                   movel(p)  'WCR1087'     msLDR108
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR109
     C*
     C     SCLDR109      IFNE      *BLANKS
     C     erLDR109      ANDNE     *ON
     C                   MOVEL(P)  SCLDR109      xxKey
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR109
     C                   movel(p)  'WCR1087'     msLDR109
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR110
     C*
     C     SCLDR110      IFNE      *BLANKS
     C     erLDR110      ANDNE     *ON
     C                   MOVEL(P)  SCLDR110      xxKey
     C                   if        scLDdes1 <> ' '
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR110
     C                   movel(p)  'WCR1087'     msLDR110
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C                   ENDIF                                                              O
     C*
     C* RESERVE HIEARCHY .. AT LEAST ONE RESERVE CODE IS REQUIRED..
     C*
     C                   if        scLDdes1 <> ' '
     C                             and erLDR101 <> *ON
      *
     C     SCLDR101      IFEQ      *BLANKS
     C     SCLDR102      ANDEQ     *BLANKS
     C     SCLDR103      ANDEQ     *BLANKS
     C     SCLDR104      ANDEQ     *BLANKS
     C     SCLDR105      ANDEQ     *BLANKS
     C     SCLDR106      ANDEQ     *BLANKS
     C     SCLDR107      ANDEQ     *BLANKS
     C     SCLDR108      ANDEQ     *BLANKS
     C     SCLDR109      ANDEQ     *BLANKS
     C     SCLDR110      ANDEQ     *BLANKS
      *
     C                   move      *on           error
     C                   move      *on           erLDR101
     C                   movel(p)  'WC40029'     msLDR101
      *
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C* ALAE INCLUDED - SHOULD BE 'Y' OR BLANKS                        S
     C*
     C     SCLDALA1      IFNE      'Y'
     C     SCLDALA1      ANDNE     *BLANKS
     C     erLDALA1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDALA1
     C                   movel(p)  'WCR1241'     msLDALA1
     C                   ENDIF
     C*
     C*  AUTO RENEW - SHOULD BE 'Y' OR BLANKS                          S
     C*
     C     SCLDRNW1      IFNE      'Y'
     C     SCLDRNW1      ANDNE     *BLANKS
     C     erLDRNW1      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDRNW1
     C                   movel(p)  'WCR1241'     msLDRNW1
     C                   ENDIF
     C*
     C* Plan #2 fields.
     C*
     C     SCLDEFD2      IFNE      *ZEROS
     C*
     C                   Z-ADD     SCLDEFD2      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLDEFD2
     C*
     C     SRERR         IFEQ      *ON
     C     erLDEFD2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEFD2
     C                   movel(p)  'WCR0500'     msLDEFD2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Default 1st plan expiration date
     C*
     C                   if        scLDefd2<>0 and scLDexp1=0
     C                   eval      scLDexp1=scLDefd2
     C                   endif
      *
      * SECOND DATE RANGE CANNOT OVERLAP FIRST DATE RANGE
      *
     C                   SELECT
      *
     C                   WHEN      scLDEFD2 <> 0 AND scLDEXP1 = 0
     C                             and erLDEFD2 <> *ON
      *
     C                   move      *on           error
     C                   move      *on           erLDEFD2
     C                   movel(p)  'WC32435'     msLDEFD2
      *
     C                   WHEN      scLDEFD2 <> 0 AND scLDEXP1 <> 0
      *
     C                   Z-ADD     scLDEFD2      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT1
      *
     C                   Z-ADD     scLDEXP1      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT2
      *
     C     XXDAT1        IFLT      XXDAT2
     C     erLDEFD2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEFD2
     C                   movel(p)  'WC32435'     msLDEFD2
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C* EDIT POLICY EXPRIRATION DATE
     C*
     C     scLDEFD2      IFNE      *ZEROS
     C     scLDEXP2      ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCLDEXP2      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scLDEXP2
     C*
     C     SRERR         IFEQ      *ON
     C     erLDEXP2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEXP2
     C                   movel(p)  'WCR0500'     msLDEXP2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     scLDEFD2      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C                   Z-ADD     scLDEXP2      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C     scLDEFD2      IFNE      *ZEROS
     C     scLDEXP2      ANDNE     *ZEROS
     C*
     C     XXDAT2        IFLT      XXDAT1
     C     erLDEFD2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDEFD2
     C                   movel(p)  'WCR0529'     msLDEFD2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  BOTH DATES MUST BE BETWEEN FUND POLICY START & END DATES
     C*
     C*  if policy is cancelled whereby pol eff and pol exp dates are equal
     C*    then bypass the following date checking
     C*
     C     xxPFDT        IFNE      xxPTDT

     C     SCLDEFD2      IFNE      *ZEROS
     C     erLDEFD2      ANDNE     *ON
     C*
     C     XXDAT1        IFLT      xxPFDT
     C     XXDAT1        ORGE      xxPTDT
     C                   move      *on           error
     C                   move      *on           erLDEFD2
     C                   movel(p)  'WC32433'     msLDEFD2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SCLDEXP2      IFNE      *ZEROS
     C     erLDEXP2      ANDNE     *ON
     C*
     C     XXDAT2        IFLT      xxPFDT
     C     XXDAT2        ORGT      xxPTDT
     C                   move      *on           error
     C                   move      *on           erLDEXP2
     C                   movel(p)  'WC32433'     msLDEXP2
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C* EFFETIVE DATE CAN NOT BE ZERO
     C*
     C     SCLDEFD2      IFEQ      *ZEROS
     C     erLDEFD2      ANDNE     *ON
     C*
     C     SCLDDES2      IFNE      *BLANKS
     C     SCLDPCT2      ORNE      *ZEROS
     C     SCLDTYP2      ORNE      *BLANKS
     C     SCLDAMP2      ORNE      *ZEROS
     C     SCLDAMG2      ORNE      *ZEROS
     C     SCLDPLN2      ORNE      *BLANKS
     C     SCLDVRB2      ORNE      *BLANKS
     C     SCLDOCV2      ORNE      *BLANKS
     C     SCLDTOT2      ORNE      *BLANKS
     C     SCLDCLS2      ORNE      *ZEROS
     C     SCLDPLV2      ORNE      *BLANKS
     C     SCLDALA2      ORNE      *BLANKS
     C     SCLDR201      ORNE      *BLANKS
     C     SCLDR202      ORNE      *BLANKS
     C     SCLDR203      ORNE      *BLANKS
     C     SCLDR204      ORNE      *BLANKS
     C     SCLDR205      ORNE      *BLANKS
     C     SCLDR206      ORNE      *BLANKS
     C     SCLDR207      ORNE      *BLANKS
     C     SCLDR208      ORNE      *BLANKS
     C     SCLDR209      ORNE      *BLANKS
     C     SCLDR210      ORNE      *BLANKS
     C     SCadp2        ORNE      *ZEROS
     C                   move      *on           error
     C                   move      *on           erLDEFD2
     C                   movel(p)  'WCR0503'     msLDEFD2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   DESCRIPTION IS REQUIRED (IF EFFECTIVE DATE IS ENTERED)
     C*
     C                   if        scLDefd2 <> *zeros
      *
     C     SCLDDES2      IFEQ      *BLANKS
     C     erLDDES2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDDES2
     C                   movel(p)  'WCR0503'     msLDDES2
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                              O
      *
     C*   GROSS/NET FLAG SHOULD BE 'G' OR 'N'
      *
     C     SCLDTYP2      IFNE      'G'
     C     SCLDTYP2      ANDNE     'N'
     C     SCLDTYP2      ANDNE     *BLANKS
     C     erLDTYP2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDTYP2
     C                   movel(p)  'WCR1087'     msLDTYP2
     C                   ENDIF                                                              O
      *
     C     SCLDTYP2      IFEQ      *BLANKS
     C     erLDTYP2      ANDNE     *ON
      *
     C                   if        scLDefd2 <> *zeros
     C                   move      *on           error
     C                   move      *on           erLDTYP2
     C                   movel(p)  'WCR0503'     msLDTYP2
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                              O
      *
     C                   if        scLDdes2 <> ' '
     C                              and scLDAmP2 < *zero
     C                              and erLDAmP2 <> *ON
     C                   move      *on           error
     C                   move      *on           erLDAMP2
     C                   movel(p)  'WCR0547'     msLDAMP2
     C                   endif                                                              O
      *
     C*   VALIDATE DEDUCTIBLE PLAN TYPE.....
      *
     C     WCFL03        IFEQ      'Y'
      *
     C     SCLDPLN2      IFNE      *BLANKS
     C     erLDPLN2      ANDNE     *ON
     C                   MOVEL(P)  '  DT'        xxTBID
     C                   MOVEL(P)  SCLDPLN2      xxKEY
      *
     C                   if        scLDdes2 <> ' '
      *
     C     keyWTGTL      CHAIN     PGT00                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   move      *on           error
     C                   move      *on           erLDPLN2
     C                   movel(p)  'WCR1087'     msLDPLN2
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                  WCFL03 = 'Y'
      *
     C* PER CLAIM/PER OCCURRENCE FLAG SHOULD BE 'C' OR 'O'
      *
     C     SCLDVRB2      IFNE      'C'
     C     SCLDVRB2      ANDNE     'O'
     C     SCLDVRB2      ANDNE     *BLANKS
     C     erLDVRB2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDVRB2
     C                   movel(p)  'WCR1087'     msLDVRB2
     C                   ENDIF                                                              O
      *
     C                   if        scLDdes2 <> ' 'and scLDvrb2 = *blanks
     C                             and erLDVrb2 <> *ON
     C                   move      *on           error
     C                   move      *on           erLDVrb2
     C                   movel(p)  'WCR0503'     msLDVrb2
     C                   ENDIF                                                              O
      *
     C*  OCCUPATIONAL DISEASE ONLY: PER CLAIM/PER OCCURRENCE FLAG
      * SHOULD BE 'C' OR 'O' OR BLANKS
      *
     C     SCLDOCV2      IFNE      'C'
     C     SCLDOCV2      ANDNE     'O'
     C     SCLDOCV2      ANDNE     *BLANKS
     C     erLDOCV2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDOCV2
     C                   movel(p)  'WCR1087'     msLDOCV2
     C                   ENDIF                                                              O
      *
     C*  STATISTICAL CLASS CODE IS REQUIRED....
     C*
     C     WCFL03        IFEQ      'Y'
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     SCLDCLS2      IFEQ      *ZEROS
     C     erLDCLS2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDCLS2
     C                   movel(p)  'WCR0503'     msLDCLS2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C*USE TOTAL INCURRED FOR DEDUCTIBLE BILLING SHOULD BE 'Y' OR BLANKS
     C*
     C     SCLDTOT2      IFNE      'Y'
     C     SCLDTOT2      ANDNE     *BLANKS
     C     erLDTOT2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDTOT2
     C                   movel(p)  'WCR1241'     msLDTOT2
     C                   ENDIF
      *
     C*   VALIDATE BILLING CALULATION LEVEL.....
      *
     C     scLDPCT2      IFNE      *ZERO
     C     erLDPLV2      ANDNE     *ON
     C                   MOVEL(P)  'BCLV'        xxTBID
     C                   MOVEL(P)  SCLDPLV2      XXKEY
      *
     C                   if        scLDdes2 <> ' '
      *
     C     keyWTGTL      CHAIN     PGT00                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   move      *on           error
     C                   move      *on           erLDPLV2
     C                   movel(p)  'WCR1087'     msLDPLV2
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                              O
      *
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR201
     C*
     C     SCLDR201      IFNE      *BLANKS
     C     erLDR201      ANDNE     *ON
     C                   MOVEL(P)  scLDR201      xxKEY
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR201
     C                   movel(p)  'WCR1087'     msLDR201
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR202
     C*
     C     SCLDR202      IFNE      *BLANKS
     C     erLDR202      ANDNE     *ON
     C                   MOVEL(P)  SCLDR202      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR202
     C                   movel(p)  'WCR1087'     msLDR202
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR203
     C*
     C     SCLDR203      IFNE      *BLANKS
     C     erLDR203      ANDNE     *ON
     C                   MOVEL(P)  SCLDR203      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR203
     C                   movel(p)  'WCR1087'     msLDR203
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR204
     C*
     C     SCLDR204      IFNE      *BLANKS
     C     erLDR204      ANDNE     *ON
     C                   MOVEL(P)  SCLDR204      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR204
     C                   movel(p)  'WCR1087'     msLDR204
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR205
     C*
     C     SCLDR205      IFNE      *BLANKS
     C     erLDR205      ANDNE     *ON
     C                   MOVEL(P)  SCLDR205      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR205
     C                   movel(p)  'WCR1087'     msLDR205
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR206
     C*
     C     SCLDR206      IFNE      *BLANKS
     C     erLDR206      ANDNE     *ON
     C                   MOVEL(P)  SCLDR206      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR206
     C                   movel(p)  'WCR1087'     msLDR206
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR207
     C*
     C     SCLDR207      IFNE      *BLANKS
     C     erLDR207      ANDNE     *ON
     C                   MOVEL(P)  SCLDR207      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR207
     C                   movel(p)  'WCR1087'     msLDR207
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR208
     C*
     C     SCLDR208      IFNE      *BLANKS
     C     erLDR208      ANDNE     *ON
     C                   MOVEL(P)  SCLDR208      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR208
     C                   movel(p)  'WCR1087'     msLDR208
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR209
     C*
     C     SCLDR209      IFNE      *BLANKS
     C     erLDR209      ANDNE     *ON
     C                   MOVEL(P)  SCLDR209      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR209
     C                   movel(p)  'WCR1087'     msLDR209
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* VALIDATE THE RESERVE CODE SCR210
     C*
     C     SCLDR210      IFNE      *BLANKS
     C     erLDR210      ANDNE     *ON
     C                   MOVEL(P)  SCLDR210      xxKey
     C*
     C                   if        scLDdes2 <> ' '
     C*
     C     keyWTRTL1     CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C                   move      *on           error
     C                   move      *on           erLDR210
     C                   movel(p)  'WCR1087'     msLDR210
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C                   ENDIF                                                              O
     C*
     C* RESERVE HIEARCHY .. AT LEAST ONE RESERVE CODE IS REQUIRED..
     C*
     C                   if        scLDdes2 <> ' '
      *
     C     SCLDR201      IFEQ      *BLANKS
     C     SCLDR202      ANDEQ     *BLANKS
     C     SCLDR203      ANDEQ     *BLANKS
     C     SCLDR204      ANDEQ     *BLANKS
     C     SCLDR205      ANDEQ     *BLANKS
     C     SCLDR206      ANDEQ     *BLANKS
     C     SCLDR207      ANDEQ     *BLANKS
     C     SCLDR208      ANDEQ     *BLANKS
     C     SCLDR209      ANDEQ     *BLANKS
     C     SCLDR210      ANDEQ     *BLANKS
     C     erLDR201      ANDNE     *ON
      *
     C                   move      *on           error
     C                   move      *on           erLDR201
     C                   movel(p)  'WC40029'     msLDR201
      *
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C* ALAE INCLUDED - SHOULD BE 'Y' OR BLANKS                        S
     C*
     C     SCLDALA2      IFNE      'Y'
     C     SCLDALA2      ANDNE     *BLANKS
     C     erLDALA2      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLDALA2
     C                   movel(p)  'WCR1241'     msLDALA2
     C                   ENDIF
       // Claims liability rate must be greater than zero for TASB 'A' and 'M' plans.
      /free
       If scClmLib < 0 and visbClmLib = *On;
          error = *on;
          erClmLib = *on;
          msClmLib = 'WCR0502';
       Endif;

       // The Claims allowance field cannot be less than zero if the policy type code
       // has a value of 'M'.
       If scClmLib < 0 and visbClmAlw = *On;
          error = *on;
          erClmAlw = *on;
          msClmAlw = 'WCR0502';
       Endif;
      /end-free
     C*
     C                   EndSr
     C**************************************************************

     P ErrLargeDed     E
     P*
       //***************************************************************************//
       //===================================================================
       // ErrRetroRate - Edit the retrospective rating info fields from different screens/programs.
       //                The retro rating info screens are contained in the policy detail
       //                program and the retro rating info popup window.
       //
       //   Call 'E'     Retrospective Rating Info    Screen format #1 (WTPOLDTL/rcdRetro)
       //
       //                The procedure will return an *OFF value if an edit error is found,
       //                or an *ON value if all edits are passed.
       //
       //===================================================================
     P ErrRetroRate    B                   export
     D ErrRetroRate    PI              n
     d inCall                         1    const
     d inMode                         1    const
     d inQuote                        1    const
     d inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     d inELrrn                        9  0 const
     d inData                       500
     d*
     D scCo#           s                   like(ELCo#)
     D scFnd           s                   like(ELFnd)
     D scEmp#          s                   like(ELEmp#)
     D scDiv           s                   like(ELDiv)
     D scFyr           s                   like(ELFyr)
     d*
     D qteMode         s               n   inz(*off)
     d*
     d error           s               n   inz
     D message         s            100
     d*
     D sysDt7          s              7  0
     D sysDt8          s              8  0

     d ptrFormatE      s               *   inz
     d*
     d dsRetroRate   e ds                  extname(wtpoldtl:rcdRetro)
     d                                     based(ptrFormatE)
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
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
      /Free

       Open WTPOLDTL;

       dsRateEdit = inData;

       Select;
          When inCall = 'E';
             ptrFormatE = inPtrFormat1;
          Other;
             Return *off;
       Endsl;

       // Setup the files and fields necessary for editing.
       Exsr Data_Setup;

       // Process Edits
       Exsr EditRetroRate;

       Close WTPOLDTL;
       inData = dsRateEdit;

       Return not error;

      /End-Free

      //===================================================================
      // Data_Setup - Load all the necessary files and fields for editing.
      //===================================================================
     C     Data_Setup    Begsr
      /free

       Clear error;

       sysDt7 = DateTo7(SystemDate);
       sysDt8 = DateTo8(SystemDate);

       scCo#  = inCo#;
       scFnd  = inFnd;
       scEmp# = inEmp#;
       scDiv  = inDiv;
       scFyr  = inFyr;

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;
       Chain keyWMEXL PEX00;
       Chain keyWDELL pel00;
       Chain keyWMEML PEM00;

       GetClientID(clientID);

       qteMode = *off;
       If inQuote = 'Q';
          qteMode = *on;
       Endif;

      /end-free
     C                   Endsr                                                  Data_Setup Subr

     C**************************************************************
     C*  Retrospective Rating Info Edits
     C**************************************************************
     C     EditRetroRate Begsr
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
     C*  MINIMUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C     SCMNFC        IFLT      0
     C     erMNFC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMNFC
     C                   movel(p)  'WCR0502'     msMNFC
     C                   ENDIF                                                  SCMNFC < 0
     C*
     C*  MAXIMUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C     SCMXFC        IFLT      0
     C     erMXFC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erMXFC
     C                   movel(p)  'WCR0502'     msMXFC
     C                   ENDIF                                                  SCMXFC < 0
     C*
     C*  BASIC PREMIUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C     SCBSFC        IFLT      0
     C     erBSFC        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erBSFC
     C                   movel(p)  'WCR0502'     msBSFC
     C                   ENDIF                                                  SCBSFC < 0
     C*
     C*  TAX MULTIPLIER CANNOT BE LESS THAN ZERO.
     C*
     C     SCTAX         IFLT      0
     C     erTAX         ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erTAX
     C                   movel(p)  'WCR0502'     msTAX
     C                   ENDIF                                                  SCTAX  < 0
     C*
     C*  TAX MULTIPLIER F-CLASSES CANNOT BE LESS THAN ZERO.
     C*
     C     SCTAXF        IFLT      0
     C     erTAXF        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erTAXF
     C                   movel(p)  'WCR0502'     msTAXF
     C                   ENDIF                                                  SCTAXF < 0
     C*
     C*  LOSS CONVERSION FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C     SCLCVF        IFLT      0
     C     erLCVF        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erLCVF
     C                   movel(p)  'WCR0502'     msLCVF
     C                   ENDIF                                                  SCLCVF < 0
     C*
     C*  LOSS LIMITATION CANNOT BE LESS THAN ZERO.
     C*
     C     SCRETN        IFLT      0
     C     erRETN        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRETN
     C                   movel(p)  'WCR0547'     msRETN
     C                   ENDIF                                                  SCRETN < 0
     C*
     C*  RETRO DEVELOPMENT FACTOR #1 CANNOT BE LESS THAN ZERO.
     C*
     C     SCRDF1        IFLT      0
     C     erRDF1        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRDF1
     C                   movel(p)  'WCR0547'     msRDF1
     C                   ENDIF                                                  SCRDF1 < 0
     C*
     C*  RETRO DEVELOPMENT FACTOR #2 CANNOT BE LESS THAN ZERO.
     C*
     C     SCRDF2        IFLT      0
     C     erRDF2        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRDF2
     C                   movel(p)  'WCR0547'     msRDF2
     C                   ENDIF                                                  SCRDF2 < 0
     C*
     C*  RETRO DEVELOPMENT FACTOR #3 CANNOT BE LESS THAN ZERO.
     C*
     C     SCRDF3        IFLT      0
     C     erRDF3        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRDF3
     C                   movel(p)  'WCR0547'     msRDF3
     C                   ENDIF                                                  SCRDF3 < 0
     C*
     C*  EXCESS LOSS PREMIUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C     SCELP         IFLT      0
     C     erELP         ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erELP
     C                   movel(p)  'WCR0547'     msELP
     C                   ENDIF                                                  SCELP  < 0
     C*
     C*  EXCESS LOSS PREMIUM FACTOR F-CLASSES CANNOT BE LESS THAN ZERO.
     C*
     C     SCELPF        IFLT      0
     C     erELPF        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erELPF
     C                   movel(p)  'WCR0547'     msELPF
     C                   ENDIF                                                  SCELPF < 0
     C*
     C*  FINAL SETTLEMENT FLAG MUST BE 'Y' OR BLANK.
     C*
     C     SCFSET        IFNE      'Y'
     C     SCFSET        ANDNE     *BLANK
     C     erFSET        ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erFSET
     C                   movel(p)  'WCR0571'     msFSET
     C                   ENDIF                                                  SCFSET IFNE
     C*
     C                   EndSr
     C**************************************************************

     P ErrRetroRate    E
     P*
       //***************************************************************************//
       //===================================================================
       // ErrSchedRate - Edit the scheduled rating info fields from different screens/programs.
       //                The scheduled rating info screens are contained in the policy detail
       //                program and the scheduled rating info popup window.
       //
       //   Call 'S'     Scheduled Rating Info    Screen format #1 (WTPOLDTL/rcdSched)
       //
       //                The procedure will return an *OFF value if an edit error is found,
       //                or an *ON value if all edits are passed.
       //
       //===================================================================
     P ErrSchedRate    B                   export
     D ErrSchedRate    PI              n
     d inCall                         1    const
     d inMode                         1    const
     d inQuote                        1    const
     d inPtrFormat1                    *
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     d inELrrn                        9  0 const
     d inData                       500
     d*
     D scCo#           s                   like(ELCo#)
     D scFnd           s                   like(ELFnd)
     D scEmp#          s                   like(ELEmp#)
     D scDiv           s                   like(ELDiv)
     D scFyr           s                   like(ELFyr)
     d*
     D qteMode         s               n   inz(*off)
     d*
     d error           s               n   inz
     D message         s            100
     d*
     D sysDt7          s              7  0
     D sysDt8          s              8  0
     d*
     D workDate        s              8  0
     d*

     d ptrFormatS      s               *   inz
     d*
     d dsSchedRate   e ds                  extname(wtpoldtl:rcdSched)
     d                                     based(ptrFormatS)
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
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
      /Free

       Open WTPOLDTL;

       dsRateEdit = inData;

       Select;
          When inCall = 'S';
             ptrFormatS = inPtrFormat1;
          Other;
             Return *off;
       Endsl;

       // Setup the files and fields necessary for editing.
       Exsr Data_Setup;

       // Process Edits
       Exsr EditSchedRate;

       Close WTPOLDTL;
       inData = dsRateEdit;

       Return not error;

      /End-Free

      //===================================================================
      // Data_Setup - Load all the necessary files and fields for editing.
      //===================================================================
     C     Data_Setup    Begsr
      /free

       Clear error;

       sysDt7 = DateTo7(SystemDate);
       sysDt8 = DateTo8(SystemDate);

       scCo#  = inCo#;
       scFnd  = inFnd;
       scEmp# = inEmp#;
       scDiv  = inDiv;
       scFyr  = inFyr;

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;
       Chain keyWMEXL PEX00;
       Chain keyWDELL pel00;
       Chain keyWMEML PEM00;

       GetClientID(clientID);

       qteMode = *off;
       If inQuote = 'Q';
          qteMode = *on;
       Endif;

      /end-free
     C                   Endsr                                                  Data_Setup Subr

     C**************************************************************
     C*  Scheduled Rating Info Edits
     C**************************************************************
     C     EditSchedRate Begsr
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
      * Premises - Both can not be entered.
      *
     C     scRFPRDA      IFNE      *ZERO
     C     scRFPRCA      ANDNE     *ZERO
     C     erRFPRCA      ANDNE     *ON
     C     erRFPRDA      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRFPRCA
     C                   move      *on           erRFPRDA
     C                   movel(p)  'WCR0590'     msRFPRCA
     C                   movel(p)  'WCR0590'     msRFPRDA
     C                   ENDIF
      *
      * Value can not exceed range.
      *
     C                   SELECT
     c*
     C                   when      scRFPRDA < 0 or scRFPRDA > 10
     c*
     C                   If        erRFPRDA <> *on
     C                   move      *on           error
     C                   move      *on           erRFPRDA
     C                   movel(p)  'WCR0807'     msRFPRDA
     C                   Endif
     c*
     C                   when      scRFPRCA < 0 or scRFPRCA > 10
     c*
     C                   If        erRFPRCA <> *on
     C                   move      *on           error
     C                   move      *on           erRFPRCA
     C                   movel(p)  'WCR0807'     msRFPRCA
     C                   Endif
     c*
     C                   ENDSL
      *
      * If either factor not = zero, must enter the Factor Reason Description
      *
     C                   IF        scRFPRDA > 0  AND scRFPRRS = *BLANKS
     C                             OR scRFPRCA > 0 AND scRFPRRS = *BLANKS
     c*
     C                   If        erRFPRRS <> *on
     C                   move      *on           error
     C                   move      *on           erRFPRRS
     C                   movel(p)  'WC32407'     msRFPRRS
     C                   Endif
     c*
     C                   ENDIF
      *
      * Classifications - Both can not be entered.
      *
     C     scRFCPDA      IFNE      *ZERO
     C     scRFCPCA      ANDNE     *ZERO
     C     erRFCPCA      ANDNE     *ON
     C     erRFCPDA      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRFCPCA
     C                   move      *on           erRFCPDA
     C                   movel(p)  'WCR0590'     msRFCPCA
     C                   movel(p)  'WCR0590'     msRFCPDA
     C                   ENDIF
      *
      * Value can not exceed range.
      *
     C                   SELECT
     c*
     C                   when      scRFCPDA < 0 or scRFCPDA > 10
     c*
     C                   If        erRFCPDA <> *on
     C                   move      *on           error
     C                   move      *on           erRFCPDA
     C                   movel(p)  'WCR0807'     msRFCPDA
     C                   Endif
     c*
     C                   when      scRFCPCA < 0 or scRFCPCA > 10
     c*
     C                   If        erRFCPCA <> *on
     C                   move      *on           error
     C                   move      *on           erRFCPCA
     C                   movel(p)  'WCR0807'     msRFCPCA
     C                   Endif
     c*
     C                   ENDSL
      *
      * If either factor not = zero, must enter the Factor Reason Description
      *
     C                   IF        scRFCPDA > 0  AND scRFCPRS = *BLANKS
     C                             OR scRFCPCA > 0 AND scRFCPRS = *BLANKS
     c*
     C                   If        erRFCPRS <> *on
     C                   move      *on           error
     C                   move      *on           erRFCPRS
     C                   movel(p)  'WC32407'     msRFCPRS
     C                   Endif
     c*
     C                   ENDIF
      *
      * Medical - Both can not be entered.
      *
     C     scRFMFDA      IFNE      *ZERO
     C     scRFMFCA      ANDNE     *ZERO
     C     erRFMFCA      ANDNE     *ON
     C     erRFMFDA      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRFMFCA
     C                   move      *on           erRFMFDA
     C                   movel(p)  'WCR0590'     msRFMFCA
     C                   movel(p)  'WCR0590'     msRFMFDA
     C                   ENDIF
      *
      * Value can not exceed range.
      *
     C                   SELECT
     c*
     C                   when      scRFMFDA < 0 or scRFMFDA > 5
     c*
     C                   If        erRFMFDA <> *on
     C                   move      *on           error
     C                   move      *on           erRFMFDA
     C                   movel(p)  'WCR0807'     msRFMFDA
     C                   Endif
     c*
     C                   when      scRFMFCA < 0 or scRFMFCA > 5
     c*
     C                   If        erRFMFCA <> *on
     C                   move      *on           error
     C                   move      *on           erRFMFCA
     C                   movel(p)  'WCR0807'     msRFMFCA
     C                   Endif
     c*
     C                   ENDSL
      *
      * If either factor not = zero, must enter the Factor Reason Description
      *
     C                   IF        scRFMFDA > 0  AND scRFMFRS = *BLANKS
     C                             OR scRFMFCA > 0 AND scRFMFRS = *BLANKS
     c*
     C                   If        erRFMFRS <> *on
     C                   move      *on           error
     C                   move      *on           erRFMFRS
     C                   movel(p)  'WC32407'     msRFMFRS
     C                   Endif
     c*
     C                   ENDIF
      *
      * Safety - Both can not be entered.
      *
     C     scRFSDDA      IFNE      *ZERO
     C     scRFSDCA      ANDNE     *ZERO
     C     erRFSDCA      ANDNE     *ON
     C     erRFSDDA      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRFSDCA
     C                   move      *on           erRFSDDA
     C                   movel(p)  'WCR0590'     msRFSDCA
     C                   movel(p)  'WCR0590'     msRFSDDA
     C                   ENDIF
      *
      * Value can not exceed range.
      *
     C                   SELECT
     c*
     C                   when      scRFSDDA < 0 or scRFSDDA > 5
     c*
     C                   If        erRFSDDA <> *on
     C                   move      *on           error
     C                   move      *on           erRFSDDA
     C                   movel(p)  'WCR0807'     msRFSDDA
     C                   Endif
     c*
     C                   when      scRFSDCA < 0 or scRFSDCA > 5
     c*
     C                   If        erRFSDCA <> *on
     C                   move      *on           error
     C                   move      *on           erRFSDCA
     C                   movel(p)  'WCR0807'     msRFSDCA
     C                   Endif
     c*
     C                   ENDSL
      *
      * If either factor not = zero, must enter the Factor Reason Description
      *
     C                   IF        scRFSDDA > 0  AND scRFSDRS = *BLANKS
     C                             OR scRFSDCA > 0 AND scRFSDRS = *BLANKS
     c*
     C                   If        erRFSDRS <> *on
     C                   move      *on           error
     C                   move      *on           erRFSDRS
     C                   movel(p)  'WC32407'     msRFSDRS
     C                   Endif
     c*
     C                   ENDIF
      *
      * Employees - Both can not be entered.
      *
     C     scRFEMDA      IFNE      *ZERO
     C     scRFEMCA      ANDNE     *ZERO
     C     erRFEMCA      ANDNE     *ON
     C     erRFEMDA      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRFEMCA
     C                   move      *on           erRFEMDA
     C                   movel(p)  'WCR0590'     msRFEMCA
     C                   movel(p)  'WCR0590'     msRFEMDA
     C                   ENDIF
      *
      * Value can not exceed range.
      *
     C                   SELECT
     c*
     C                   when      scRFEMDA < 0 or scRFEMDA > 10
     c*
     C                   If        erRFEMDA <> *on
     C                   move      *on           error
     C                   move      *on           erRFEMDA
     C                   movel(p)  'WCR0807'     msRFEMDA
     C                   Endif
     c*
     C                   when      scRFEMCA < 0 or scRFEMCA > 10
     c*
     C                   If        erRFEMCA <> *on
     C                   move      *on           error
     C                   move      *on           erRFEMCA
     C                   movel(p)  'WCR0807'     msRFEMCA
     C                   Endif
     c*
     C                   ENDSL
      *
      * If either factor not = zero, must enter the Factor Reason Description
      *
     C                   IF        scRFEMDA > 0  AND scRFEMRS = *BLANKS
     C                             OR scRFEMCA > 0 AND scRFEMRS = *BLANKS
     c*
     C                   If        erRFEMRS <> *on
     C                   move      *on           error
     C                   move      *on           erRFEMRS
     C                   movel(p)  'WC32407'     msRFEMRS
     C                   Endif
     c*
     C                   ENDIF
      *
      * Cooperation - Both can not be entered.
      *
     C     scRFMCDA      IFNE      *ZERO
     C     scRFMCCA      ANDNE     *ZERO
     C     erRFMCCA      ANDNE     *ON
     C     erRFMCDA      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRFMCCA
     C                   move      *on           erRFMCDA
     C                   movel(p)  'WCR0590'     msRFMCCA
     C                   movel(p)  'WCR0590'     msRFMCDA
     C                   ENDIF
      *
      * Value can not exceed range.
      *
     C                   SELECT
     c*
     C                   when      scRFMCDA < 0 or scRFMCDA > 5
     c*
     C                   If        erRFMCDA <> *on
     C                   move      *on           error
     C                   move      *on           erRFMCDA
     C                   movel(p)  'WCR0807'     msRFMCDA
     C                   Endif
     c*
     C                   when      scRFMCCA < 0 or scRFMCCA > 5
     c*
     C                   If        erRFMCCA <> *on
     C                   move      *on           error
     C                   move      *on           erRFMCCA
     C                   movel(p)  'WCR0807'     msRFMCCA
     C                   Endif
     c*
     C                   ENDSL
      *
      * If either factor not = zero, must enter the Factor Reason Description
      *
     C                   IF        scRFMCDA > 0  AND scRFMCRS = *BLANKS
     C                             OR scRFMCCA > 0 AND scRFMCRS = *BLANKS
     c*
     C                   If        erRFMCRS <> *on
     C                   move      *on           error
     C                   move      *on           erRFMCRS
     C                   movel(p)  'WC32407'     msRFMCRS
     C                   Endif
     c*
     C                   ENDIF
      *
      * Management - Both can not be entered.
      *
     C     scRFMSDA      IFNE      *ZERO
     C     scRFMSCA      ANDNE     *ZERO
     C     erRFMSCA      ANDNE     *ON
     C     erRFMSDA      ANDNE     *ON
     C                   move      *on           error
     C                   move      *on           erRFMSCA
     C                   move      *on           erRFMSDA
     C                   movel(p)  'WCR0590'     msRFMSCA
     C                   movel(p)  'WCR0590'     msRFMSDA
     C                   ENDIF
      *
      * Value can not exceed range.
      *
     C                   SELECT
     c*
     C                   when      scRFMSDA < 0 or scRFMSDA > 5
     c*
     C                   If        erRFMSDA <> *on
     C                   move      *on           error
     C                   move      *on           erRFMSDA
     C                   movel(p)  'WCR0807'     msRFMSDA
     C                   Endif
     c*
     C                   when      scRFMSCA < 0 or scRFMSCA > 5
     c*
     C                   If        erRFMSCA <> *on
     C                   move      *on           error
     C                   move      *on           erRFMSCA
     C                   movel(p)  'WCR0807'     msRFMSCA
     C                   Endif
     c*
     C                   ENDSL
      *
      * If either factor not = zero, must enter the Factor Reason Description
      *
     C                   IF        scRFMSDA > 0  AND scRFMSRS = *BLANKS
     C                             OR scRFMSCA > 0 AND scRFMSRS = *BLANKS
     c*
     C                   If        erRFMSRS <> *on
     C                   move      *on           error
     C                   move      *on           erRFMSRS
     C                   movel(p)  'WC32407'     msRFMSRS
     C                   Endif
     c*
     C                   ENDIF
     c*
     c* Validate the Loss Survey Date field if Survey is completed, otherwise
     c* the date must be blank.
     c*
      /free
       If erRFLSDT <> *on;
          If scRFLSCF = 'Y';
             workDate = scRFLSDT;
             erRFLSDT = ValidDate(workDate:message);
             scRFLSDT = workDate;
             If erRFLSDT = *on;
                msRFLSDT = message;
                error = *on;
             Endif;
          Else;
             If scRFLSDT <> 0;
                msRFLSDT = 'WCR0450';
                erRFLSDT = *on;
                error = *on;
             Endif;
          Endif;
       Endif;
      /end-free
     c*
     c* The Modifier must be entered if premium level exceeds, otherwise
     c* it must be zero.
     C*
     C     scRFPEFL      IFEQ      'Y'
     C*
     C     scRFPEEM      IFEQ      *ZERO
     C     erRFPEEM      ANDNE     *on
     C                   move      *on           error
     C                   move      *on           erRFPEEM
     C                   movel(p)  'WCR0556'     msRFPEEM
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     scRFPEEM      IFNE      *ZERO
     C     erRFPEEM      ANDNE     *on
     C                   move      *on           error
     C                   move      *on           erRFPEEM
     C                   movel(p)  'WCR0450'     msRFPEEM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   EndSr
     C**************************************************************

     P ErrSchedRate    E

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

       //***************************************************************************//
       //===================================================================
       // Policy year cannot be more than 365(366) days or max extended days.
       //===================================================================
     P PolMaxLength    B                   export
     D PolMaxLength    PI             1
     D fromDate                       8  0 const
     D toDate                         8  0 const
     D co#                            3  0 const
     D fnd                            3  0 const
     D messageID                    100
     d*
     d xxDat1          s              7  0
     d xxDat2          s              7  0
     d xxFromDt        s              8  0
     d xxToDt          s              8  0
     D xxMXEX          s                   like(FNMXEX)
     D xxMXCY          s                   like(X7MXCY)
     d clientID        s              3

      /Free
       GetClientID(clientID);
      /End-Free
     C*
     C* Group Master File Key List
     C*
     C     keyWMFNL      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C*
     C                   Clear                   messageID
     C*
     C     keyWMFNL      Chain     PFN00
     C*
     C                   If        %found
     C*
     C                   eval      xxDat1 = dtCYMD(fromDate)
     C                   eval      xxDat2 = dtCYMD(toDate)
     C*
     C                   eval      xxFromDt = fromDate
     C                   eval      xxToDt   = toDate
     C*
     C*  POLICY END DATE CANNOT BE MORE THAN 365(366) DAYS PAST POLICY
     C*   START DATE
     C*
     C     FNCGFL        IFNE      'Y'
     c     clientID      Andne     '150'
     c     clientID      Andne     '148'
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    xxFromDt
     C                   PARM                    xxToDt
     C                   PARM                    xxDAYS            5 0
     C                   PARM                    xxERR             1
     C*
     C                   MOVE      *OFF          leapYR            1
     C*    TO DATE
     C*
     C                   MOVE      xxDAT2        xxDT
     C     xxYY          DIV       4             xxLPYR            2 1
     C                   MVR                     xxREM             1 0
     C*
     C     xxREM         IFEQ      *ZERO
     C     xxMMDD        ANDGT     0228
     C                   MOVE      *ON           leapYR
     C                   ENDIF                                                  XXREM=*ZERO
     C*
     C*    FROM DATE
     C*
     C                   MOVE      xxDAT1        xxDT
     C     xxYY          DIV       4             xxLPYR
     C                   MVR                     xxREM
     C*
     C     xxREM         IFEQ      *ZERO
     C     xxMMDD        ANDLT     0301
     C                   MOVE      *ON           leapYR
     C                   ENDIF                                                  XXREM=*ZERO
     C*
     C     leapYR        IFEQ      *OFF
     C*
     C     xxDAYS        IFGT      365
     C                   movel(p)  'WCR1281'     msPTDT
     C                   Return    *on
     C                   ENDIF                                                  XXDAYS>365
     C*
     C                   ELSE
     C*
     C     xxDAYS        IFGT      366
     C                   movel(p)  'WCR1281'     messageID
     C                   Return    *on
     C                   ENDIF                                                  XXDAYS>366
     C*
     C                   ENDIF                                                  LEAPYR=*OFF
     C*
     C                   ELSE
     C*
     C     FNMXEX        IFEQ      *ZERO
     C                   Z-ADD     12            FNMXEX
     C                   ENDIF                                                  FNMXEX=*ZERO
     C*
     C                   Z-ADD     FNMXEX        xxMXEX
     C                   Z-ADD     *ZEROS        xxMXCY
     C*
     C     xxMXEX        DOWGE     12
     C                   ADD       1             xxMXCY
     C                   SUB       12            xxMXEX
     C                   ENDDO                                                  XXMXEX>=12
     C*
     C                   Z-ADD     xxDAT1        x7MXDT                         FROM DATE
     C                   ADD       xxMXEX        x7MXMM
     C                   ADD       xxMXCY        x7MXCY
     C*
     C     x7MXMM        DOWGT     12
     C                   ADD       1             x7MXCY
     C                   SUB       12            x7MXMM
     C                   ENDDO                                                  X7MXMM>12
     C*
     C     xxDAT2        IFGT      x7MXDT
     C                   movel(p)  'WC30138'     messageID
     C                   Return    *on
     C                   ENDIF                                                  XXDAT2>X7MXDT
     C*
     C                   ENDIF                                                  FNCGFL IFNE
     C*
     C                   ENDIF                                                  If %found
     C*
     C                   Return    *OFF

     P PolMaxLength    E
     p GetStRptDivInf  b
     d GetStRptDivInf  pi
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inFyr                              like(ELfyr)
     d  outDiv                             like(ELdiv)
     d  outPolEfDt                         like(ELpfdt)
     d  outInLim1Efd                       like(ELpfdt)
     d  outInLim2Efd                       like(ELpfdt)
     d stRptDiv        s                   like(ELdiv)
      /free
       stRptDiv = GetStRptDv(inco#:infnd:inemp#);
       Chain (inco#:infnd:inemp#:stRptDiv:infyr) pel00;
       If %found;
          outDiv       = stRptDiv;
          outPolEfDt   = ELpfdt;
          outInLim1Efd = ELilef;
          outInLim2Efd = ELi2ef;
       Endif;
      /end-free
     p GetStRptDivInf  e

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
