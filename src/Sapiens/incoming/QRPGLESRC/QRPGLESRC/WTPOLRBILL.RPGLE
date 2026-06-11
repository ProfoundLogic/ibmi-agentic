     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WTPOLRBILL - EMPLOYER BILLING MAINTENANCE
     H*****************************************************************
     H* CALLED FROM:                                                  *
     H*    Mid-Term endorsement                                       *
     H*    Policy Toolbar                                             *
     H* CALLS TO:                                                     *
     H*    W8001C                                                     *
     H*    WTPOLDTL - EMPLOYER PREMIUM                                *
     H*    W7412C - NCCI POLICY INFORMATION PAGE SUBMITTER            *
     H*                                                               *
     H* Created: 10/02/2012 C:191 Project:Transform                   *
     H*                           Copied from W0090R                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/08/25 C:65  Support: 120120 (Amwins)                       *
     H*                Change the division being used to load the     *
     H*                adjustments grid when called for single div    *
     H*                and its summarized.  The state rpt div will    *
     H*                contain the adjustments.                       *
     H*                                                               *
     H* 08/25/25 C:65  Support: 119510 (CoveRisk)                     *
     H*                Set printer in LDA for Scriptura based on      *
     H*                single mode and either revised bill or MTE.    *
     H*                                                               *
     H* 03/06/25 C:65  Project: WC2670 (Phw#118641 - Internal)        *
     H*                Correct the condition to set the mass flag for *
     H*                the call to OutputWindow to only be blank if   *
     H*                being run for a single division policy or      *
     H*                combination bill is enabled.  These are the    *
     H*                only times when one email will go out. If      *
     H*                calling for a single email situation pass the  *
     H*                RRN to the OutputWindow procedure.             *
     H*                                                               *
     H* 12/05/24 C:65  Support: 118060 (Sunz)                         *
     H*                When running MTE revised bill, pull the darts  *
     H*                remark code from first WMENP record instead    *
     H*                of always using premium adjustment remark code.*
     H*                                                               *
     H* 10/18/24 C:65  Support: 117783 (Vital)                        *
     H*                Treat a bill on eff day flag change like bscd  *
     H*                change and remove all future installments and  *
     H*                rebill on new dates.                           *
     H*                                                               *
     H* 10/15/24 C:320 Project: WC2984                                *
     H*                Allow deleted summarized divisions to be picked*
     H*                for A/R reconciliation.                        *
     H*                                                               *
     H* 08/14/24 C:65  Project: Phw#117111 (Amwins)                   *
     H*                Pull the policy effective date for the policy  *
     H*                from the earliest found on policy year instead *
     H*                of from the state reporting division for WMSHP.*
     H*                                                               *
     H* 07/12/24 C:300 Support: 115772 (CoveRisk)                     *
     H*                Use seperate array of posting dates for each   *
     H*                adjustment type (premium, tax, A1 to A7)       *
     H*                                                               *
     H* 03/12/24 C:291 Project: WC2670                                *
     H*                Ensure active payroll exist for at least one   *
     H*                Division. Otherwise don't allow to submit the  *
     H*                Revised Bill.                                  *
     H*                                                               *
     H* 12/12/23 C:291 Project: WC2670                                *
     H*                Add processing to display output control window*
     H*                for Revised Bills when Scriptura is requested. *
     H*                                                               *
     H* 10/17/23 C:333 Support: 114101 (AmWins)                       *
     H*                Error Running Revised Billing Endorsement      *
     H*                                                               *
     H* 08/31/23 C:62  PROJECT: WC2918                                *
     H*                Recompile                                      *
     H*                                                               *
     H* 06/14/23 C:65  PROJECT: 150WFY (revision to 150WAR)           *
     H*                Clear LD percentage prior to running calculat- *
      *                ions in order to recalculate them properly     *
      *                in a later stage of the calculations.          *
      *                Stop calling W8010R a second time for loss     *
      *                constant within BILL subroutine.  This causes  *
      *                calculation to be called six times since the   *
      *                BILL subroutine is called three times now.     *
      *                Instead calculations will be called 3-4 times: *
      *                1) First time calculates payroll manual        *
      *                   premiums on each division and loads updated *
      *                   standard premium into WDDSP.                *
      *                2) Second call happens if loss constant or     *
      *                   % of manual is availble on the policy. This *
      *                   replicates the second call that happens     *
      *                   in SRMDIV for original bills.  The LD %     *
      *                   will be calculated on this step.            *
      *                3) Third call is able to accurately calculate  *
      *                   stock discount on each division now if      *
      *                   second call did not happen.  It also        *
      *                   calculates the final premium on each div    *
      *                   into WDMPP.                                 *
      *                4) Fourth/final call will be able to calculate *
      *                   net rates now that WDMPP has updated info.  *
      *                                                               *
      * 04/20/22 C:65  Project: WC2869                                *
      *                Remove 139WJU coding to clean up source.       *
      *                                                               *
      * 01/19/22 C:33  Project: WC2837                                *
      *                Run the calculation again regardless of net    *
      *                 rate flag.                                    *
      *                                                               *
      * 08/29/21 C:65  Project: WC2663                                *
      *                Get rid of HOLDAR calls when we are removing   *
      *                the temporary A/R - those do not need to be    *
      *                held off in backup file.                       *
      *                                                               *
      * 06/17/21 C:62  Project: WC2663                                *
      *                Apply invoice changes.                         *
      *                                                               *
      * 06/05/21 C:65  Support: 96124 (InHouse)                       *
      *                Chain to employer detail prior to error chks.  *
      *                                                               *
      * 06/04/21 C:65  Support: 96124 (InHouse)                       *
      *                Prevent the existing a/r lock check from       *
      *                occuring due to newer WC2187 project.          *
      *                                                               *
      * 01/18/21 C:65  Project: WC2447                                *
      *                Add coding to only show divisions that have    *
      *                a difference/adjustment with a checkbox to     *
      *                show all when checked.  Defaulted to show all  *
      *                when there are no adjustments on policy.       *
      *                                                               *
      * 01/05/21 C:65  Project: WC2447                                *
      *                Cosmetic changes done to new window.           *
      *                                                               *
      * 05/15/20 C:062 Project: 102WIB (Rev)                          *
      *                Change deposit cap logic.                      *
      *                                                               *
      * 05/14/20 C:65  Project: WC2447                                *
      *                New revised billing window consolidates all of *
      *                the popups into one window.  Loading and       *
      *                editing of the window is consolidated as well. *
      *                                                               *
      * 09/02/20 C:136 Support: 91154 (150WEU)                        *
      *                Don't delete future A/R on S/R policies if     *
      *                flagged to do so (WMEXP.EXExcArBl = 'Y')       *
      *                                                               *
      * 03/16/20 C:212 Project wc2447                                 *
      *                For success/submit window, change call to      *
      *                PopOneBtnAgain. No DSPF changes                *
      *
      * 03/16/20 C:180 Project wc2700                                 *
      *                Allow non-renewal cancellation override        *
     H*   !!!!!!!!!!!! Do not EBF this code without the rest !!!!!!!!!!!!!!!
     H*   !!!!!!!!!!!! of the project. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     H*                                                               *
     H* 01/23/20 C:136 Project: WC2187                                *
     H*                Replace algorithm for locking policies         *
      *||||||| Include all objects of this project with any EBF ||||||||||||||
     H*                                                               *
     H* 12/09/19 C:65  Project: Phw#83902/150WCS  (LUBA)              *
     H*                Clear WXEVP file before doing calculation      *
     H*                runs for the finalize bill action.  The stored *
     H*                values must be recalculated since policy and   *
     H*                payroll changes can occur after program first  *
     H*                starts and runs calc to load up files.         *
     H*                                                               *
     H* 11/04/19 C:298 Project: 102WIB                                *
     H*                Deposit Cap at Policy Level                    *
     H*                                                               *
     H* 01/17/19 C:136 Support: 76114                                 *
     H*                Use SYDUPFILE to copy WXEVP to QTEMP to        *
     H*                authority issue causing abend.                 *
     H*                                                               *
     H* 06/19/18 C:65  Project: 150WCS                                *
     H*                Duplicate WXEVP/L to QTEMP to allow W8010R     *
     H*                to skip information gathering on every div.    *
     H*                                                               *
     H* 06/11/18 C:65  Project: 150WAQ                                *
     H*                Update the writing/adjusting of A/R prevention *
     H*                to check the actual setup file now.            *
     H*                                                               *
     H* 10/20/17 C:65  Project: 150WAQ                                *
     H*                Ensure no writing or adjusting of A/R occurs   *
     H*                for policies with a collateral setup.          *
     H*                (temp change - disable based on 150 - setup    *
     H*                 checking to be done soon)                     *
     H*                                                               *
     H* 07/29/15 C:65  Support: Phase#36318 (CareWest)                *
     H* (reimplement   Calculate premium twice on the Finalize action *
     H*  on 06/19/17)  since the calculations are no longer called    *
     H*                when employer detail or payrolls are updated - *
     H*                which all happens within WTPOLDTL now. Each    *
     H*                divisions updated premium is not available     *
     H*                for calculations like stock discount until     *
     H*                all divisions have been run once.  Run the     *
     H*                calculations a third time for net rates.       *
     H*                                                               *
     H* 06/05/17 C:136 Support: 47019                                 *
     H*                Prevent zero post dates from being written     *
     H*                with assessment adjustments when spreading     *
     H*                to future installments                         *
     H*                                                               *
     H* 01/16/17 C:136 Project: 102WHD                                *
     H*                Implement max deposit functionality            *
     H*                                                               *
     H* 07/21/16 C:136 Support: PHW42075                              *
     H*                Expand the size of SCDSC1,2,3 on fCR04A        *
     H*                to 60A to prevent MYDSC1 from printing chars   *
     H*                past 41 on ENDORSE document                    *
     H*                                                               *
     H* 04/22/16 C:65  Support: Phw#40912 (PDS)                       *
     H*                Fix the policy page resubmission coding from   *
     H*                114WWI to only reset sub history if the policy *
     H*                page is actually going to be resubmitted.      *
     H*                                                               *
     H* 02/10/16 C:65  Support: Phw#39774 (LCTA)                      *
     H*                Populate type field for submission history key *
     H*                list.  Also, consider 'N' codes for non-rating *
     H*                submisions as resubmission transactions too.   *
     H*                                                               *
     H* 02/05/16 C:306 Support: Phw#39738                             *
     H*                Recompilation over its display file.           *
     H*                Intialize the SCDEP variable to 'Y' as         *
     H*                default.                                       *
     H*                                                               *
     H* 01/04/16 C:65  Support: Phw#29560 (CSI)                       *
     H*                Change the check on the 1st period ending date *
     H*                being less than the billing start date to be   *
     H*                less than or equal.  Since the date is replaced*
     H*                with the billing start date + 1 day the a/r    *
     H*                was not being checked properly when running a  *
     H*                bill # of interim days prior to policy.        *
     H*                                                               *
     H* 08/25/15 C:302 Project: WC2385                                *
     H*                Perform a check at the end of the revised bill *
     H*                +when a user attempts to submit the revised    *
     H*                bill to check if the live files have been      *
     H*                updated since the revised bill started.        *
     H*                                                               *
     H* 08/17/15 C:306 Support: Phase#36700                           *
     H*                (Recompilation over display file change. )     *
     H*                Corrected the Cancel and Next button on Revise *
     H*                Bill.                                          *
     H*                                                               *
     H* 09/30/14 C:283 Support: WA140 85605                           *
     H*                Calculate the State Tax on Deposit separately  *
     H*                for adjustment. Also exclude the State Tax on  *
     H*                deposit when processing State Tax on Premium.  *
     H*                                                               *
     H* 07/11/14 C:279 Project: 111WBL                                *
     H*                Modified to subtract the dividend from the     *
     H*                premium prior to determining billing amount.   *
     H*                                                               *
     H* 06/05/14 C:65  Support: Red#4286                              *
     H*                Another fix to support 83259 to correct        *
     H*                SkipDepAdj to reset to the correct division.   *
     H*                                                               *
     H* 04/24/14 C:290 Support: WA045 85451                           *
     H*                Chnaged 83259 to chain the temporary employer  *
     H*                detail file WAELP istead of WDELP while trying *
     H*                to reset files to current year being processed *
     H*                in subprocedure SkipDepAdj.                    *
     H*                                                               *
     H* 02/13/14 C:65  PROJECT: Supp# WA114/84604                     *
     H*                After completing the rounding check on         *
     H*                estimated plans verify if there are any        *
     H*                amounts remaining and move installments up     *
     H*                to compensate if there are none.               *
     H*                                                               *
     H* 12/26/13 C:136 Support: WA146 83259                           *
     H*                Do not adjust deposit in current year if       *
     H*                deposit can be adjusted in the latest year.    *
     H*                Added subprocedure SkipDepAdj.                 *
     H*                                                               *
     H* 09/23/13 C:136 Support: WA114 83577                           *
     H*                Further correction to 140WAU. Correct issue    *
     H*                with how summarized A/R is handled.            *
     H*                                                               *
     H* 08/22/13 C:65  PROJECT: Supp# WA114/83042                     *
     H*                Fix the program to use field XAAMT instead of  *
     H*                EAAMT when reading in amounts from the holding *
     H*                file.  Also, do not read in the WMXAP records  *
     H*                if the policy is not under cancellation.       *
     H*                                                               *
     H* 08/19/13 C:65  PROJECT: 140WAU/PRISS#38                       *
     H*                Modify program to disable either A/R code      *
     H*                entry when it is not needed on the policy.     *
     H*                Additionally, if the policy is S/R then        *
     H*                move the deposit a/r code into the regular     *
     H*                a/r code field to allow S/R policies to write  *
     H*                deposit records correctly.                     *
     H*                                                               *
     H* 07/30/13 C277  SUPPORT: 82958                                 *
     H*                Correct Wording on Revised Billing Message     *
     H*                in display file and recompile program.         *
     H*                                                               *
     H* 07/02/13 C:136 Project: 147WAI                                *
     H*                Implement changes for Dividend Plan            *
     H*                                                               *
     H* 06/07/13 SIS   Redmine: 1639 (Rashmi Patel)                   *
     H*                Fixed Job submiited popup in Revised Bill.     *
     H*                Do not display Job submiited polup when        *
     H*                Employee master record in use by another job.  *
     H*                                                               *
     H* 02/08/13 C:190 PROJECT: 140WAU                                *
     H*                Modify program to process deposit adjustments  *
     H*                for estimating accounts also.                  *
     H*                                                               *
     H*****************************************************************
     FWTPOLRBILLCF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*** begin add ***  05/14/20  ************************************
     F                                     Sfile(SflAdjust:rrnBillGrd)
     F*** end add *****  05/14/20  ************************************
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FWDF1L     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEPL3    IF   E           K DISK
     F                                     RENAME(PEP01:PEP03)
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
     F*** Begin Add *** 05/15/20 *******************************************
     FWMEMPHL   if   e           k Disk    Rename(pemh01:pemh00)
     F*** End   Add *** 05/15/20 *******************************************
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
     FWAEAL1    UF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     F*** begin add ***  05/14/20  ***********************************
     F                                     INFDS(DBFDS5)
     FWAEAP     UF   E             DISK
     F                                     RENAME(PEA01:PEA0P)
     F*** end add *****  05/14/20  ***********************************
     FWAEAL     O  A E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     FWMXAL1    UF   E           K DISK
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
     FWDHCL     IF A E           K DISK    USROPN
     F                                     RENAME(PHC01:PHC00)
     FWDHCLR    IF   E           K DISK
     F                                     RENAME(PHC01:PHC0R)
     FWTMYP     IF   E             DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     FWDSCL     IF   E           K DISK
     FWDELL1    IF   E           K DISK    USROPN
     F                                     RENAME(PEL01:PEL03)
     F*** begin add ***  08/14/24  ***********************************
     Fwdellb    if   e           k disk    prefix(b)
     F                                     rename(pel01:pel0b)
     F*** end add *****  08/14/24  ***********************************
     FWMIFL     IF   E           K DISK
     FWMSHL1    UF   E           K DISK
      *** beg del ***** 01/23/20 **************************
     F***WXRIL1    IF A E           K DISK
      *** end del ***** 01/23/20 **************************
     fwmdvl     if   e           k disk    rename(pdv01:pdv00)
     FWMEPL1    IF   E           K DISK    USROPN
     F                                     RENAME(PEP01:PEP04)
     FWAELL1    UF   E           K DISK    USROPN
     F                                     RENAME(PEL01:PEL44)
     FWAEPL1    UF   E           K DISK    USROPN
     F                                     RENAME(PEP01:PEP44)
     FWADSL     UF   E           K DISK    USROPN
     F                                     RENAME(PDS01:PDS44)
     FWAILL     UF   E           K DISK    USROPN
     F                                     RENAME(PIL01:PIL44)
     FWAMPL     UF   E           K DISK    USROPN
     F                                     RENAME(PMP01:PMP44)
     FWAPPL     UF   E           K DISK    USROPN
     F                                     RENAME(PPP01:PPP44)
     FWALDL2    UF   E           K DISK    USROPN
     F                                     RENAME(PLD01:PLD44)
     FWMCOL     IF   E           K DISK    RENAME(PCO01:PCO00) USROPN
     Fwxevp     UF   E           K DISK    RENAME(pev01:pev0p) USROPN
      //*** Beg Add ***** 12/12/23 ***********************************
     fwsbll     if   e           k disk    Rename(pbl01:pbl001)
      //*** End Add ***** 12/12/23 ***********************************
     F*** begin add ***  08/25/25  ********************************************
     FSTSPL     IF   E           K DISK    rename(psp01:psp00)
     FSMUPL     IF   E           K DISK    rename(pup01:sup00)
     F*** end add *****  08/25/25  ********************************************

     d xtDepDue        s                   like(xtanpr)
     d xtDepTaxDue     s                   like(xttxdu)
     d prevEstMaxDep   s             11s 0
     d currEstMaxDep   s             11s 0
     d prevSRMaxDep    s             11s 0
     d currSRMaxDep    s             11s 0
     d prevMaxDepPrem  s             11  0
     d currMaxDepPrem  s             11  0
     d currDepPrem     s             11  2
     d prevDepPrem     s             11  2
     d prevBSsefl      s              1
     D xxResubmit      s               n
     d xxColt          s              1
     D/copy wprEmplr
     D/copy sprgenPop
     D*** begin add ***  05/14/20  *********************************************
     D/copy wprpolpop
     D*** end add *****  05/14/20  *********************************************
     D*** begin add ***  06/17/21  *********************************************
     D/copy wprpmtsvc
     D*** end add *****  06/17/21  *********************************************
      *** beg add **** 01/23/20 ***************************
     d/copy sprLock
     d*** Beg Add ***** 12/12/23 *******************************************
     d/copy sPrLetter
     D/copy sPrXML
     D/copy sPrJobInfo
     D isUseScript     S              1n   inz
     d processType     s            100
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d*** End Add ***** 12/12/23 ******************************
     d conErrTtl       c                   const('*** Error ***')
     d strJobInfo      s             28
     d strProcess      s             10
     d strProcess25    s             25
     d strUser         s             50
     d xxMsg           s            256    inz
     d xxTtl           s             50    inz
     d xxRsp           s               n   inz
      *** end add **** 01/23/20 ***************************
     D riuCreated      s               n
     D xxRtn           s               n
     D AB1             S              9  8 DIM(12)
     D AB2             S              9  8 DIM(12)
     D BD              S              7  0 DIM(13)
     D J2D             S              7  0 DIM(12)
     D KEYF            S              8  0 DIM(2)
     D KEYM            S              5  0 DIM(2)
     D X0              S              1    DIM(5)
     D XB              S              1    DIM(5)
     D QCA             S             55    DIM(50) CTDATA PERRCD(1)             CMD EXEC/QCAEXEC
     D TMP             S              1    DIM(22)
     D NUM             S              1    DIM(5)
     D WKD             S              7  0 DIM(60) ASCEND
     D WKA             S              9  2 DIM(60)
     D WK1             S              9  2 DIM(53)
     D WK2             S              9  2 DIM(53)
     D WK3             S              9  2 DIM(53)
     D WK4             S              9  2 DIM(53)
     D WK5             S              9  2 DIM(53)
     D WK6             S              9  2 DIM(53)
     D WK7             S              9  2 DIM(53)
     D WKT             S              9  2 DIM(60)
     D SKA             S              9  2 DIM(53)
     D SK1             S              9  2 DIM(53)
     D SK2             S              9  2 DIM(53)
     D SK3             S              9  2 DIM(53)
     D SK4             S              9  2 DIM(53)
     D SK5             S              9  2 DIM(53)
     D SK6             S              9  2 DIM(53)
     D SK7             S              9  2 DIM(53)
     D SKT             S              9  2 DIM(53)
     D DAY             S              2    DIM(13) CTDATA PERRCD(13)              DAYS OF WEEK    %
     D CA              S             14    DIM(7) CTDATA PERRCD(1)              DEFAULT ASST HEADING
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     d xxAB01          s              9  6
     d xxAB02          s              9  6
     d QQAR2           s              2
     D*** begin add ***  05/14/20  ********************************************
     D redoCalc        s               n   inz(*off)
     D billError       s               n   inz(*off)
     D editError       s               n   inz(*off)
     D xxtaa           s               n   inz(*off)
     D xx1aa           s               n   inz(*off)
     D xx2aa           s               n   inz(*off)
     D xx3aa           s               n   inz(*off)
     D xx4aa           s               n   inz(*off)
     D xx5aa           s               n   inz(*off)
     D xx6aa           s               n   inz(*off)
     D xx7aa           s               n   inz(*off)
     D xxARType        s              1
     d xxefd2          s              7  0
     d xxesds          s             30
     d xxesty          s              1
     d xxsrds          s             30
     d xxsrty          s              1
     D*** end add *****  05/14/20  ********************************************
      *** beg add ***** 09/02/20 ************************
     d isChangedToSr   s               n
      *** end add ***** 09/02/20 ************************
     d*** Begin Add *** 06/17/21 *******************************************
     d isWMEAP         s               n
     d invoice         s             30
     d inKey           s             20
     d arrInvoice      s             30a   Dim(9999) Inz
     d index           s              5s 0
     d inJson          s          32000a   Varying Inz
     d sendToIC        s               n   Inz
     d*** End   Add *** 05/07/21 *******************************************
      //*** Beg Add ***** 03/12/24 ******************************************
     d flgPyrlFnd      s              1
      //*** End Add ***** 03/12/24 ******************************************
     d*** End   Add *** 06/17/21 *******************************************
     D*
     D OPTNM           C                   CONST('Revised Billing')
     D AGTAPI          C                   CONST('WA002R')
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
     D*** begin add ***  05/14/20  ***********************************
     D DBFDS5          DS
     D  DBRRN5               397    400B 0
     D*** end add *****  05/14/20  ***********************************
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
     D                 DS
     D  X1PFDT                 1      7  0
     D  YY1                    1      3  0
     D  MM1                    4      5  0
     D  DD1                    6      7  0
     D  YYMM1                  1      5  0
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
     D                 DS
     D  P1                     1    117  2
     D                                     DIM(13)
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
     D  SCP113               109    117  2
     D                 DS
     D  A1                     1    117  2
     D                                     DIM(13)
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
     D  SCA113               109    117  2
     D                 DS
     D  A2                     1    117  2
     D                                     DIM(13)
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
     D  SCA213               109    117  2
     D                 DS
     D  A3                     1    117  2
     D                                     DIM(13)
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
     D  SCA313               109    117  2
     D                 DS
     D  A4                     1    117  2
     D                                     DIM(13)
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
     D  SCA413               109    117  2
     D                 DS
     D  A5                     1    117  2
     D                                     DIM(13)
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
     D  SCA513               109    117  2
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
     D                 DS
     D*** Beg Chg ***** 10/17/23 ********************************
     D***  AB                     1    108  8
     D***                                     DIM(12)
     D***  BSAB01                 1      9  8
     D***  BSAB02                10     18  8
     D***  BSAB03                19     27  8
     D***  BSAB04                28     36  8
     D***  BSAB05                37     45  8
     D***  BSAB06                46     54  8
     D***  BSAB07                55     63  8
     D***  BSAB08                64     72  8
     D***  BSAB09                73     81  8
     D***  BSAB10                82     90  8
     D***  BSAB11                91     99  8
     D***  BSAB12               100    108  8
     D  AB                     1     60P 8
     D                                     DIM(12)
     D  BSAB01                 1      5P 8 Inz
     D  BSAB02                 6     10P 8 Inz
     D  BSAB03                11     15P 8 Inz
     D  BSAB04                16     20P 8 Inz
     D  BSAB05                21     25P 8 Inz
     D  BSAB06                26     30P 8 Inz
     D  BSAB07                31     35P 8 Inz
     D  BSAB08                36     40P 8 Inz
     D  BSAB09                41     45P 8 Inz
     D  BSAB10                46     50P 8 Inz
     D  BSAB11                51     55P 8 Inz
     D  BSAB12                56     60P 8 Inz
     D*** End Chg ***** 10/17/23 ********************************
     D                 DS
     D  XXDATA                 1     80
     D  XXTYPE                 1      1
     D  ZZCO#                  2      4
     D  XXUSER                 5     14
     D  XXTIME                15     20
     D  XXDATE                21     28
     D  UPOUTQ                29     38
     D*** Begin Add ***  07/12/24  ********************************
     D* DA - Array of Posting Dates for Premium Adjustment type
     D*** End   Add ***  07/12/24  ********************************
     D                 DS
     D  DA                     1    104  0
     D                                     DIM(13)
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
     D  SCDA13               097    104  0
     D*** Begin Add ***  07/12/24  ********************************
     D* D1 - Array of Posting Dates for Tax Adjustment type
     D                 DS
     D  D1                     1    104  0
     D                                     DIM(13)
     D  SCD101               001    008  0
     D  SCD102               009    016  0
     D  SCD103               017    024  0
     D  SCD104               025    032  0
     D  SCD105               033    040  0
     D  SCD106               041    048  0
     D  SCD107               049    056  0
     D  SCD108               057    064  0
     D  SCD109               065    072  0
     D  SCD110               073    080  0
     D  SCD111               081    088  0
     D  SCD112               089    096  0
     D  SCD113               097    104  0
      *
     D* DA1 - Array of Posting Dates for Assessment 1 Adjustment type
     D                 DS
     D  DA1                    1    104  0
     D                                     DIM(13)
     D  SCDA101              001    008  0
     D  SCDA102              009    016  0
     D  SCDA103              017    024  0
     D  SCDA104              025    032  0
     D  SCDA105              033    040  0
     D  SCDA106              041    048  0
     D  SCDA107              049    056  0
     D  SCDA108              057    064  0
     D  SCDA109              065    072  0
     D  SCDA110              073    080  0
     D  SCDA111              081    088  0
     D  SCDA112              089    096  0
     D  SCDA113              097    104  0
      *
     D* DA2 - Array of Posting Dates for Assessment 2 Adjustment type
     D                 DS
     D  DA2                    1    104  0
     D                                     DIM(13)
     D  SCDA201              001    008  0
     D  SCDA202              009    016  0
     D  SCDA203              017    024  0
     D  SCDA204              025    032  0
     D  SCDA205              033    040  0
     D  SCDA206              041    048  0
     D  SCDA207              049    056  0
     D  SCDA208              057    064  0
     D  SCDA209              065    072  0
     D  SCDA210              073    080  0
     D  SCDA211              081    088  0
     D  SCDA212              089    096  0
     D  SCDA213              097    104  0
      *
     D* DA3 - Array of Posting Dates for Assessment 3 Adjustment type
     D                 DS
     D  DA3                    1    104  0
     D                                     DIM(13)
     D  SCDA301              001    008  0
     D  SCDA302              009    016  0
     D  SCDA303              017    024  0
     D  SCDA304              025    032  0
     D  SCDA305              033    040  0
     D  SCDA306              041    048  0
     D  SCDA307              049    056  0
     D  SCDA308              057    064  0
     D  SCDA309              065    072  0
     D  SCDA310              073    080  0
     D  SCDA311              081    088  0
     D  SCDA312              089    096  0
     D  SCDA313              097    104  0
      *
     D* DA4 - Array of Posting Dates for Assessment 4 Adjustment type
     D                 DS
     D  DA4                    1    104  0
     D                                     DIM(13)
     D  SCDA401              001    008  0
     D  SCDA402              009    016  0
     D  SCDA403              017    024  0
     D  SCDA404              025    032  0
     D  SCDA405              033    040  0
     D  SCDA406              041    048  0
     D  SCDA407              049    056  0
     D  SCDA408              057    064  0
     D  SCDA409              065    072  0
     D  SCDA410              073    080  0
     D  SCDA411              081    088  0
     D  SCDA412              089    096  0
     D  SCDA413              097    104  0
      *
     D* DA5 - Array of Posting Dates for Assessment 5 Adjustment type
     D                 DS
     D  DA5                    1    104  0
     D                                     DIM(13)
     D  SCDA501              001    008  0
     D  SCDA502              009    016  0
     D  SCDA503              017    024  0
     D  SCDA504              025    032  0
     D  SCDA505              033    040  0
     D  SCDA506              041    048  0
     D  SCDA507              049    056  0
     D  SCDA508              057    064  0
     D  SCDA509              065    072  0
     D  SCDA510              073    080  0
     D  SCDA511              081    088  0
     D  SCDA512              089    096  0
     D  SCDA513              097    104  0
      *
     D* DA6 - Array of Posting Dates for Assessment 6 Adjustment type
     D                 DS
     D  DA6                    1    104  0
     D                                     DIM(13)
     D  SCDA601              001    008  0
     D  SCDA602              009    016  0
     D  SCDA603              017    024  0
     D  SCDA604              025    032  0
     D  SCDA605              033    040  0
     D  SCDA606              041    048  0
     D  SCDA607              049    056  0
     D  SCDA608              057    064  0
     D  SCDA609              065    072  0
     D  SCDA610              073    080  0
     D  SCDA611              081    088  0
     D  SCDA612              089    096  0
     D  SCDA613              097    104  0
      *
     D* DA7 - Array of Posting Dates for Assessment 7 Adjustment type
     D                 DS
     D  DA7                    1    104  0
     D                                     DIM(13)
     D  SCDA701              001    008  0
     D  SCDA702              009    016  0
     D  SCDA703              017    024  0
     D  SCDA704              025    032  0
     D  SCDA705              033    040  0
     D  SCDA706              041    048  0
     D  SCDA707              049    056  0
     D  SCDA708              057    064  0
     D  SCDA709              065    072  0
     D  SCDA710              073    080  0
     D  SCDA711              081    088  0
     D  SCDA712              089    096  0
     D  SCDA713              097    104  0
     D*** End   Add ***  07/12/24  ********************************
     D                 DS
     D  PA                     1    117  2
     D                                     DIM(13)
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
     D  SCPA13               109    117  2
     D*** begin add ***  05/14/20  ********************************
     D                 DS
     D  arrAm                  1    117  2
     D                                     DIM(13)
     D  SCAA01                 1      9  2
     D  SCAA02                10     18  2
     D  SCAA03                19     27  2
     D  SCAA04                28     36  2
     D  SCAA05                37     45  2
     D  SCAA06                46     54  2
     D  SCAA07                55     63  2
     D  SCAA08                64     72  2
     D  SCAA09                73     81  2
     D  SCAA10                82     90  2
     D  SCAA11                91     99  2
     D  SCAA12               100    108  2
     D  SCAA13               109    117  2
     D                 DS
     D  arrDt                  1    104  0
     D                                     DIM(13)
     D  SCAD01               001    008  0
     D  SCAD02               009    016  0
     D  SCAD03               017    024  0
     D  SCAD04               025    032  0
     D  SCAD05               033    040  0
     D  SCAD06               041    048  0
     D  SCAD07               049    056  0
     D  SCAD08               057    064  0
     D  SCAD09               065    072  0
     D  SCAD10               073    080  0
     D  SCAD11               081    088  0
     D  SCAD12               089    096  0
     D  SCAD13               097    104  0
     D*** end add *****  05/14/20  ********************************
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
      //*** Beg Chg ***** 12/12/23 *********************************************
     D*** SVLDA           DS          1024    INZ
     D***  LDEND#                 1      5  0
     D***  LDENDA                 1      5
     D***  LDMYRR                 6     14  0
     D***  LDEEDT                15     22  0
     D***  LDFYR                 23     25  0
     D***  LDPYRL                26     26
     dsvLDA            ds          1024    Inz
     d ldEnd#                         5  0 overlay(svlda:1)
     d ldEndA                         5    overlay(svlda:1)
     d ldMYRR                         9  0 overlay(svlda:6)
     d ldEEDt                         8  0 overlay(svlda:15)
     d ldFyr                          3  0 overlay(svlda:23)
     d ldPyrl                         1    overlay(svlda:26)
     d keyEmail                      40    overlay(svLDA:56)
     d keySeq#                        5    overlay(svLDA:96)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d x1Copy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      //*** End Chg ***** 12/12/23 *********************************************
     D                 DS
     D  XXWKDT                 1      7  0
     D  WYY                    1      3  0
     D  WMM                    4      5  0
     D  WDD                    6      7  0
     D  WYYMM                  1      5  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d SkipDepAdj      pr              n
     d strCommand      s            255
      *** Begin Add *** 03/16/20 ****************************************
     d outMode         s              3    Inz('BIL')
     d intCxDate       s              7  0
      *** End   Add *** 03/16/20 ****************************************

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     D saveLiveDT      S              7  0
     D saveLiveTm      S              6  0
     D saveUser        S             10
     D PopupMsg        S            256

     D* --------------------------------------------------
     D* Prototype for procedure: LiveCheck
     D* --------------------------------------------------
     D LiveCheck       PR              N

     D* --------------------------------------------------
     D* Prototype for procedure: clearTemp
     D* --------------------------------------------------
     D clearTemp       PR
     D*--------------------------------------------------
     D* Procedure name: GetLesserOf
     D*--------------------------------------------------
     D GetLesserOf     PR            11  2
     D  maxDepAmt                    11  0 const
     D  calcPremAmt                  11  2 const
     D*--------------------------------------------------
     D* Procedure name: CalcMaxDeposit
     D*--------------------------------------------------
     D CalcMaxDeposit  PR
     D  estMaxDepAmt                 11S 0
     D  srMaxDepAmt                  11S 0
     D*** Begin Add *** 05/15/20 ************************
     D  prvMaxDepReq                   n   const options(*nopass)
     D*** End   Add *** 05/15/20 ************************
     D*--------------------------------------------------
     D* Procedure name: GetDepPercent
     D*--------------------------------------------------
     D GetDepPercent   PR                   like(BSab01)
     D  billPlanType                  1A    const

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
     C     RIUKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
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
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
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
     C     EPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X@CLAS            5 0
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFD1
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
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
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
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
     C*
     C*
     C*  EMPLOYER A/R TRANSACTION
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C                   KFLD                    XXPODT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C*
     C     EAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXARTY            1
     C*
     C     EAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C*
     C     EAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*** begin add ***  05/14/20  ************************************
     C*
     C     EAKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C*** end add *****  05/14/20  ************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*  RATE SET CODE FILE
     C*
     C     RSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XX1RTC            3
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
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
     C*** begin add ***  08/14/24  *********************************************
     C     SHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C*** end add *****  08/14/24  *********************************************
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ELANPR        XXANPR
     C     *LIKE         DEFINE    ELTXDU        X1TXDU
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C     *LIKE         DEFINE    ELSTPR        XXSTPR
     C     *LIKE         DEFINE    ELMAPR        XXMAPR
     C     *LIKE         DEFINE    ELBSCD        XXBSCD
     C     *LIKE         DEFINE    ELASP2        XXASP2
     C     *LIKE         DEFINE    ELCO#         XKCO#
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    EADT          XXEFDT
     C     *LIKE         DEFINE    RSJRST        XXSTAT
     C     *LIKE         DEFINE    EADT          XX1EFD
     C     *LIKE         DEFINE    ELPTDT        XXTPFD
     C     *LIKE         DEFINE    EADT          XXEFD1
     C     *LIKE         DEFINE    EADT          XXPODT
     C     *LIKE         DEFINE    EADT          XXPFDT
     C     *LIKE         DEFINE    EADT          XXWK60
     C     *LIKE         DEFINE    EADT          XXCURD
     C     *LIKE         DEFINE    EADT          XXCUR2
     C     *LIKE         DEFINE    EADT          XXDTE
     C     *LIKE         DEFINE    EADT          XXDTE1
     C     *LIKE         DEFINE    EADT          XXDTE2
     C     *LIKE         DEFINE    EMNAME        SVNAME
     C     *LIKE         DEFINE    EMFDNM        SVFDNM
     C     *LIKE         DEFINE    EMNAM2        SVNAM2
     C     *LIKE         DEFINE    EMFDN2        SVFDN2
     C*
     C     *ENTRY        PLIST
     C                   PARM                    inRRN             9
     C                   PARM                    @@CALL            1
     C                   PARM                    @@NEXT           10
     C                   PARM                    @@TAB             5
     C                   PARM                    @@RTCD            1
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
     C*
     C*
     C* SINGLE DIVISION MODE.
     C*
     C                   Select
     C                   When      @@CALL = 'E'
     C                   Eval      mteMode = *On
     C                   When      @@CALL = 'C'
     C                   EVAL      mteMode = *Off
     C                   Endsl

     C     @@CALL        IFEQ      'S'
     C                   MOVE      *ON           *IN67
     C                   MOVE      ' '           @@CALL
     C                   ELSE
     C                   MOVE      *OFF          *IN67
     C                   ENDIF                                                  @@CALL IFEQ
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '139'
     C                   MOVE      '999'         CLTID
     C                   ENDIF
     C*
      /Free
          strCommand = 'SYDUPFILE FROM(WXEVP) TO(QTEMP/WXEVP) DATA(*NO) +
           LGL(*YES)';
          Callp QCMDEXC(strCommand:%len(strCommand));

          Monitor;
             clear strCommand;
             strCommand = 'OVRDBF FILE(WXEVP) TOFILE(QTEMP/WXEVP)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          Monitor;
             clear strCommand;
             strCommand = 'OVRDBF FILE(WXEVL) TOFILE(QTEMP/WXEVL)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

      /End-Free
        //*** Beg Add ***** 12/12/23 ***********************************
        processType = 'Revised Bill';
        isUseScript = UseScriptura(processType);
        If PgmInStack('WTPOLMEND') = *Off;
           In svLda;
           Clear svLda;
           Out svLda;
        Endif;
        //*** End Add ***** 12/12/23 ***********************************
     C*** begin add ***  05/14/20  *************************************
      /free

       dspBillGrd = *Off;
       clrBillGrd = *On;
       Write rcdBill;
       dspBillGrd = *On;
       clrBillGrd = *Off;
       rrnBillGrd = 0;

      /end-free
     C*** end add *****  05/14/20  *************************************
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      '0'           XXOPN2            1
     C                   MOVE      '0'           XXOPNH            1
     C                   MOVE      'N'           XXPYCG            1
     C*** begin add ***  06/14/23  ***************************************
     C                   Clear                   xxPercMan         1
     C*** end add *****  06/14/23  ***************************************
     C*
     C                   Z-ADD     *ZEROS        @@RRN             9 0
     C                   Z-ADD     *ZEROS        @@RRN2            9 0
     C                   MOVE      ' '           @@KC              1
     C                   MOVE      ' '           @@KL              1
     C                   EVAL      @@RRN = %INT(inRRN)
     C*  GET THE USER PARAMETER FILE
     C*
     F*** begin add ***  08/25/25  ********************************************
     C     UPKY          CHAIN     SUP00                              60
     F*** end add *****  08/25/25  ********************************************
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
     C                   Eval      riuCreated = *Off
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
     C                   EVAL      @@RRN2= GetEmpRrn(ELemp#:ELdiv:ELfnd:ELco#)
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELFYR         SCYR
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C                   MOVE      *BLANK        XXLOSS            1
     C                   OPEN      WDELL1
     C                   EXSR      CHKLC
     C                   CLOSE     WDELL1
     C     @@RRN         CHAIN     WDELP                              60
      *** beg chg ***** 01/23/20 **************************
     C***     Q1PRMS        IFGT      4
     C***                   EXSR      01KRIU
     C***                   ENDIF                                                  Q1PRMS>1
      /Free
       If Q1prms > 4;
          If GetPolicyLockStatus(ELco#:ELfnd:ELemp#:
           strJobInfo:strProcess:strUser) = *On;
             Select;
             When strProcess = 'MIDTERMEND';
                strProcess25 = 'Midterm Endorsement';
             When strProcess = 'ADDIVISION';
                strProcess25 = 'Add of Division';
             When strProcess = 'REVBILLING';
                strProcess25 = 'Revised Billing';
             Endsl;
             xxMsg = 'This operation is available when ' + %Trim(strProcess25) +
              ' Job(' + %Trim(strJobInfo) + ') has completed for ' +
              %Trim(strUser) + '.';
             xxTtl = '*** Policy Locked! ***';
             xxRsp = PopOneBtn(xxMsg:xxTtl);
             @@RtCd = *On;
             @@kc = *On;
             Exsr SrTerm;
          Else;
             SetPolicyLock(ELco#:ELfnd:ELemp#:'REVBILLING');
          Endif;
       Endif;
      /End-Free
      *** end chg ***** 01/23/20 **************************
     C                   ENDIF                                                  Q1PRMS>1
     C                   TIME                    DSDTTM
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
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
     C*
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
     C                   MOVEL     EMNAME        SVNAME
     C                   MOVEL     EMFDNM        SVFDNM
     C                   MOVEL     EMNAM2        SVNAM2
     C                   MOVEL     EMFDN2        SVFDN2
     C                   ENDIF
     C*
     C     EMKY1         READE     PEM02                                  60
     C                   ENDDO
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
     C     EX3RDB        ANDNE     'Y'
     C                   MOVE      EXREV         SCRMCD
     C                   MOVE      EXLUMP        XXLSFL            1            LUMP SUM FLAG
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       XXLSFL
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     SCRMCD        IFEQ      *BLANKS
     C                   MOVE      'N'           SCDART
     C                   ENDIF                                                  SCRMCD
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
     C*** begin del ***  06/04/21  **************************************
     C**** EAKY4         CHAIN(N)  PEA04                              60
     C**** *IN60         IFEQ      *OFF
     C****               MOVEL(P)  EAUSER        SCERRU
     C****               Z-ADD     EATIME        SCERRT
     C****               Z-ADD     EADT          SRDTI
     C****               CALL      'SRCVTDAT'    DTMDYY
     C****               MOVEL     SRDTO         SCERRD
     C****               MOVE      *BLANK        Q2RECF
     C****               MOVEL     'WDWMSG'      Q2RECF
     C****               GOTO      STRT
     C****               ENDIF                                                  *IN60 IFEQ
     C*** end del *****  06/04/21  **************************************
     C*** begin add ***  05/14/20  ********************************************
     C*  DETERMINE IF CALLED FROM EMPLOYER MID TERM ENDORSEMENTS
     C*    IF SO, WE NEED TO DISPLAY SCR04A
     C*
     C                   Clear                   visEndorse
     C*
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   Move      *on           visEndorse
     C                   ENDIF
     C*** end add *****  05/14/20  ********************************************
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
     C*
     C     Q2RECF        IFEQ      'WDWMSG'
     C                   EXFMT     WDWMSG
     C*
     C                   MOVE      *ON           *INLR
      *** beg chg ***** 01/23/20 ********************************
     C***                GOTO      TERM
      /Free
       Exsr SrTerm;
      /End-Free
      *** end chg ***** 01/23/20 ********************************
     C*
     C                   ENDIF                                                  Q2RECF IFEQ
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*** begin add ***  05/14/20  ************************************************
     C                   If        not redoCalc
     C*** end add *****  05/14/20  ************************************************
     C*
     C*  NEXT LINE IS NOT SUPPOSED TO BE EXECUTED
     C*
     C  N09
     CAN 09              EXFMT     SCR02
     C*
     C                   MOVE      *OFF          *INKE
     C*
     C                   Z-ADD     DBRRN4        ##RRN
     C                   EXSR      LDOVRF
     C*
     C                   EVAL      #crrn = %editc(##rrn:'X')
     C                   CALL      'WTPOLDTL'
     C                   PARM                    #cRRN             9
     C                   PARM      'E'           ##CALL            1
     C                   PARM      *blanks       ##NEXT           10
     C*
     C                   EXSR      DLTOVR
      /Free
         If LiveCheck();

            PopupMsg = 'User ' + %trim(saveUser)
                       + ' has updated this policy subsequent to when this '
                       + 'revised bill started. '
                       + 'Press OK to discard currect changes and '
                       + 'restart based on current data. ';

            PopOneBtn(PopupMsg:'Information');

            clearTemp();

            q2Recf = 'SCR01';

      /End-Free
     C                   goto      SCR01T
      /Free
         Endif;
      /End-Free
      //*** Beg Add ***** 03/12/24 ****************************************
      /Free
       If ##NEXt = 'FINALIZE';
          flgPyrlFnd = 'N';
          If not %Open(WAEPL1);
             Open WAEPL1;
          Endif;
          Setll (xxCo#:xxFnd:xxFyr:scEmp#) pep44;
          Reade (xxCo#:xxFnd:xxFyr:scEmp#) pep44;
          Dow Not %Eof(waepl1);
             If EPstat = 'A';
                flgPyrlFnd = 'Y';
                Leave;
             Endif;
             Reade (xxCo#:xxFnd:xxFyr:scEmp#) pep44;
          Enddo;
          If  flgPyrlFnd = 'N';
             PopupMsg = 'No active payroll records exist for this employer.';
             PopOneBtn(PopupMsg:'Error');
             q2Recf = 'SCR01';
      /End-Free
     C                   goto      SCR01T
      /Free
          Endif;
          If %Open(WAEPL1);
             Close WAEPL1;
          Endif;
       Endif;
      /End-Free
      //*** End Add ***** 03/12/24 ****************************************

     C*
     C                   Select
     C                   When      ##NEXt = 'CANCEL'
     C                   Eval      *INKC = *ON
     C                   When      ##NEXt = 'FINALIZE'
     C                   Eval      *INKG = *ON
     C                   ENDSL
     C*** begin add ***  05/14/20  ************************************************
     C                   Endif
     C                   Move      *off          redoCalc
     C*** end add *****  05/14/20  ************************************************

     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     *INKF         OREQ      *ON
     C     *INKG         OREQ      *ON
     C     *INKI         OREQ      *ON
     C     *INKS         OREQ      *ON
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  *INKC/KL/KG=*ON
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*** begin add ***  05/14/20  ************************************************
     C*
     C* Call new bill window if supposed to load up any window after bill revision
     C* has been pressed.
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03A'
     C     Q2RECF        OREQ      'SCR03B'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR04'
     C     Q2RECF        OREQ      'SCR04A'
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
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCDATE            8 0
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SCDT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   Exsr      BillWindow
     C                   Goto      Strt
     C*
     C                   Endif
     C*** end add *****  05/14/20  ************************************************
     C*
     C*** begin del ***  05/14/20  ************************************************
     C*****Q2RECF        IFEQ      'SCR03'
      *
      *   PROMPT TO LUMP SUM ADJUSTMENTS OR NOT
      *
     C*****XXLSFL        IFEQ      'Y'
     C******IN32         ANDEQ     *OFF
     C******INKL         ANDEQ     *OFF
     C*****XLMFLG        ANDNE     'Y'
     C*****              EXSR      SRCADJ
      *
     C*****SCPRAJ        IFNE      *ZERO
     C******             WRITE     BLANK
      *
     C******             MOVE      *ON           *IN90
     C******IN90         DOWEQ     *ON
     C******             EXFMT     WDLUMP
      *
     C*****SCLUMP        IFNE      'Y'
     C*****SCLUMP        ANDNE     *BLANK
     C******             MOVE      *ON           *IN88
     C******             ELSE
     C******             MOVE      *OFF          *IN90
     C******             MOVE      'Y'           XLMFLG            1
     C******             ENDIF
      *
     C******             ENDDO
      *
     C******             ENDIF                                                  SCPRAJ <> 0
      *
     C*****SCLUMP        IFEQ      'Y'
     C******             EXSR      fRLUMP
     C******             ENDIF
      *
     C******             ENDIF                                                  XXLSFL = 'Y'
     C*
     C*****              EXFMT     SCR03
     C*****              END
     C*
     C*****Q2RECF        IFEQ      'SCR03A'
     C*****              EXFMT     SCR03A
     C*****              END
     C*
     C*****Q2RECF        IFEQ      'SCR03B'
     C*****              EXFMT     SCR03B
     C*****              END
     C*
     C*****Q2RECF        IFEQ      'SCR03C'
     C*****              EXFMT     SCR03C
     C*****              END
     C*
     C*****Q2RECF        IFEQ      'SCR04'
      *
      * Default Effective Date if called as Mid-Term Endorsement
      *
     C******LOCK         IN        *DTAARA
     C*****              OUT       *DTAARA
     C*
     C*****LDENDA        IFNE      *ZEROS
     C*****LDENDA        ANDNE     *BLANKS
     C*****SCEFDT        ANDEQ     *ZERO
     C*****              Z-ADD     LDEEDT        SCEFDT
     C*****              ENDIF
      *
     C*****              CALL      'SRFUTR8'
     C*****              PARM      DSDATE        QQDT              8 0
     C*****              PARM      EXRVID        QQDAYS            5 0
     C*****              PARM                    QQERR             1
     C*****              PARM                    QQTERM            1
     C*
     C*****QQERR         IFNE      '1'
     C*****              MOVE      QQDT          SCDATE            8 0
     C*****              ENDIF
     C*****XXACDT        IFNE      *ZERO
     C*****              MOVE      XXACDT        SCDT
     C*****              ENDIF                                                  XXACDT IFNE
      /free

       //*****reqEst = *on;
       //*****If *in80 = *on or noEst = *on;
       //*****   reqEst = *off;
       //*****Endif;

       //*****reqSR = *on;
       //*****If *in80 = *on or noSR = *on;
       //*****   reqSR = *off;
       //*****Endif;

      /end-free
      /Free
       //*****    scDEP = 'Y';
      /End-Free
     C*****              EXFMT     SCR04
      /Free
       //*****  If scDEP = 'N';
       //*****     scDEP = 'N';
       //*****  else;
       //*****     scDEP = 'Y';
       //*****  EndIf;
      /End-Free


     C*****              END
     C*
     C*****Q2RECF        IFEQ      'SCR04A'
     C*****              EXFMT     SCR04A
     C*****              END
     C*
     C*****Q2RECF        IFEQ      'SCR05'
     C*****              EXFMT     SCR05
     C*****              END
     C*** end del *****  05/14/20  ************************************************
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
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     Q1PRMS        IFGT      1
     C                   MOVE      *INKC         @@KC
     C                   ENDIF                                                  Q1PRMS>1
     C                   MOVE      '1'           *INLR
      *** beg chg ***** 01/23/20 ********************************
     C***                GOTO      TERM
      /Free
       Exsr SrTerm;
      /End-Free
      *** end chg ***** 01/23/20 ********************************
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
      *** beg chg ***** 01/23/20 ********************************
     C***                GOTO      TERM
      /Free
       Exsr SrTerm;
      /End-Free
      *** end chg ***** 01/23/20 ********************************
     C                   ENDIF                                                  Q1PRMS>1
     C*
     C**
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
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
     C*
     C**
     C     Q2RECF        WHENEQ    'SCR04'
     C     *IN80         IFEQ      '1'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
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
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *INKC IFEQ
     C*** beg chg ***** 01/23/20 ********************
     C***  *INKC         CABEQ     *ON           TERM
      /Free
       If *Inkc = *On;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      'N'           SCDEP
     C                   MOVE      *BLANKS       SCAR
      /free
       scARD = ' ';
      /end-free
     C                   Z-ADD     0             SCEFDT
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C     XX3C          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03C'      Q2RECF
     C                   ELSE
     C     XX3B          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03B'      Q2RECF
     C                   ELSE
     C     XX3A          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03A'      Q2RECF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  XXDIVC IFGT
     C                   END
     C     Q2RECF        WHENEQ    'SCR04A'
     C                   MOVEL(P)  'SCR04'       Q2RECF
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
     C*** beg chg ***** 01/23/20 ********************
     C***  *INKC         CABEQ     *ON           TERM
      /Free
       If *Inkc = *On;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
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
     C                   PARM      'N'           @@BILL            1
     C*
     C*** beg chg ***** 01/23/20 ********************
     C***  *INKC         CABEQ     *ON           TERM
      /Free
       If *Inkc = *On;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
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
     C                   PARM      'R'           ##MODE            1
     C                   PARM      DBRRN4        ##RRN             9 0
     C                   PARM      DBRRN2        ##RRN2            9 0
     C                   PARM      ' '           ##QTE             1
     C*
     C*** beg chg ***** 01/23/20 ********************
     C***  *INKC         CABEQ     *ON           TERM
      /Free
       If *Inkc = *On;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
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
     C*
     C*  BLANK THE LUMSUM ADJUSTMENT WINDOW FLAG FOR SECOND TIME
     C*
     C*** begin del *** 05/14/20  *********************************************
     C*********          MOVE      ' '           XLMFLG
     C*** end del ***** 05/14/20  *********************************************
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
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      LDOVRF
     C                   EXSR      BILL
     C*** begin add ***  06/14/23  ***************************************
     C* Calculate an extra time when loss constant or percentage of manual
     C                   If        xxloss = 'Y' or xxPercMan = 'Y'
     C                   Eval      calcManP = 'Y'
     C                   Exsr      BILL
     C                   Clear                   calcManP          1
     C                   Endif
     C*** end add *****  06/14/23  ***************************************
     C                   EXSR      BILL
     C***  Begin Del  ***  01/19/22  *****************
     C**********         If        F1NetR = 'Y'
     C***  End   Del  ***  01/19/22  *****************
     C                   EXSR      BILL
     C***  Begin Del  ***  01/19/22  *****************
     C**********         Endif
     C***  End   Del  ***  01/19/22  *****************
     C                   EXSR      DLTOVR
     C*
     C*  LOAD PREVIOUS BILLING SCHEDULE
     C*
     C*
     C                   MOVE      *BLANK        XXSUM             1
     C                   MOVE      *BLANK        XXWAR             1
     c                   move      *on           noEst
     c                   move      *on           noSR
     C                   Z-ADD     0             XTANPR           11 2
     C                   Z-ADD     0             XTTXDU           11 2
      /free
       xtDepDue = 0;
       xtDepTaxDue = 0;
      /end-free
     C                   MOVE      *BLANK        XX3A              1
     C                   MOVE      *BLANK        XX3B              1
     C                   MOVE      *BLANK        XX3C              1
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
     C                   Z-ADD     0             XT6THB           11 2
     C                   Z-ADD     0             XT7THB           11 2
     C                   Z-ADD     0             ST6THB           11 2
     C                   Z-ADD     0             ST7THB           11 2
     C                   Z-ADD     0             XL6THB           11 2
     C                   Z-ADD     0             XL7THB           11 2
     C                   Z-ADD     0             SL6THB           11 2
     C                   Z-ADD     0             SL7THB           11 2
     C                   Z-ADD     0             STANPR           11 2
     C                   Z-ADD     0             STTXDU           11 2
     C                   MOVE      *OFF          *IN62
     C                   MOVE      *OFF          *IN63
     C                   MOVE      *OFF          *IN64
     C*
     C  N67EMKY1         CHAIN     PEM02                              60
     C   67EMKY          CHAIN     PEM02                              60
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
     C     EMKY2         CHAIN     PEM02                              60
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
     C     CD(Y)         ANDLT     X1DATE
     C     Y             ORNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     ELPFDT
     C                   MOVE      'Y'           XXCNCL            1
     C                   END
      /Free
       //*** Begin Add *** 03/16/20 ************************************
       If y <> 0;
          intCxDate = cd(y);
       Else;
          intCxDate = 0;
       Endif;
       If intCxDate <> 0 And OvrRenewalCx(outMode:ELCo#:ELFnd:
        ELEmp#:ELdiv:ELfyr:intCxDate) = *On;
          xxCNCL = *blank;
       Endif;
       //*** End   Add *** 03/16/20 ************************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
       If ELARSM = 'Y' and xxCNCL = 'Y';
          xxCNCL = *blank;
       Endif;
      *** End Add ***  10/15/24  ***  WC2984  ************************
      /End-Free
     C                   MOVE      ' '           xxPendCX          1
     C                   If        xxCNCL <> 'Y' and Y <> 0 and CD(Y) <> 0
     C                   Move      'Y'           xxPendCX
     C                   Endif
     C*
     C*  ONLY PROCESS DIVISION IF NOT CANCELLED.
     C*
     C     XXCNCL        IFNE      'Y'
     C*
     C                   MOVEL     ELBSCD        XXBSCD
     C                   MOVEL     ELBLFL        XXBLFL            1
     C     ELANPR        SUB       ELPRTD        SVANPR           11 2
     C                   Z-ADD     ELSTPR        SVSTPR           11 2
     C                   Z-ADD     ELMAPR        SVMAPR           11 2
     C                   Z-ADD     ELENMP        SVNMPR           11 2
     C     ELANPR        SUB       ELPRTD        S2ANPR           11 2
     C                   Z-ADD     ELTXDU        SVTXDU           11 2
     C                   MOVE      ELARSM        SVARSM            1
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
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
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
     C*
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XXBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C                   MOVEA     AB            AB1
      /Free
       prevBSsefl = BSsefl;
       //*** Begin Chg *** 05/15/20 ****************************************
       //*** CalcMaxDeposit (prevEstMaxDep:prevSRMaxDep);
       CalcMaxDeposit (prevEstMaxDep:prevSRMaxDep:*On);
       //*** End   Chg *** 05/15/20 ****************************************
      /End-free
     C*
     C     ELKY1         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*
     C*
     C*
     C*  LOAD NEW BILLING SCHEDULE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C                   MOVEL     ELBSCD        SCBSCD
     C     TBKY          CHAIN     PBS01                              60
     C                   MOVEA     AB            AB2
      /Free
       CalcMaxDeposit (currEstMaxDep:currSRMaxDep);
       //*** beg add ***** 09/02/20 **************
       If prevBSsefl = 'E' and BSsefl = 'S';
          isChangedToSR = *On;
       EndIf;
       //*** end add ***** 09/02/20 **************
      /End-free
     C*
     C*
     C*  CLEAR DATE AND AMOUNT ARRAYS TO BE USED ON ADJUSTMENT SCREEN
     C*
     C                   MOVEA     *ZEROS        DA
     C*** Begin Add ***  07/12/24  ********************************
     C                   MOVEA     *ZEROS        D1
     C                   MOVEA     *ZEROS        DA1
     C                   MOVEA     *ZEROS        DA2
     C                   MOVEA     *ZEROS        DA3
     C                   MOVEA     *ZEROS        DA4
     C                   MOVEA     *ZEROS        DA5
     C                   MOVEA     *ZEROS        DA6
     C                   MOVEA     *ZEROS        DA7
     C*** End   Add ***  07/12/24  ********************************
     C                   MOVEA     *ZEROS        PA
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C     SVARSM        IFNE      'Y'
     C     *IN67         ANDEQ     *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     0             SVMAPR
     C                   Z-ADD     0             SVSTPR
     C                   Z-ADD     0             SVNMPR
     C                   Z-ADD     0             SVANPR
     C                   Z-ADD     0             SVTXDU
     C                   Z-ADD     0             SV1STB
     C                   Z-ADD     0             SV2NDB
     C                   Z-ADD     0             SV3RDB
     C                   Z-ADD     0             SV4THB
     C                   Z-ADD     0             SV5THB
     C                   Z-ADD     0             SU1STB
     C                   Z-ADD     0             SU2NDB
     C                   Z-ADD     0             SU3RDB
     C                   Z-ADD     0             SU4THB
     C                   Z-ADD     0             SU5THB
     C                   Z-ADD     0             SV6THB
     C                   Z-ADD     0             SV7THB
     C                   Z-ADD     0             SU6THB
     C                   Z-ADD     0             SU7THB
     C                   ENDIF                                                  SVARSM IFNE
     C*
     C*  COMPUTE ADJUSTMENT IF NEW BILLING SCHEDULE DESIGNATES A
     C*  SELF-REPORTING ACCOUNT
     C*
     C     BSSEFL        IFEQ      'S'
     C     XXDIV         IFEQ      SCDIV
     C                   MOVE      *ON           *IN81
     C                   ENDIF
     C*
      /free
       noSR = *off;
       Exsr DepositAdj;
      /end-free
     C*
     C* BILL THE ASSESSMENTS INTO INSTALLMENTS IF FLAGGED TO DO SO,
     C* OTHERWISE BILL IN LUMP SUM.  ASSESSMENT CODING WILL BE HANDLED
     C* BY W8011R FOR S/R POLICIES FOR CLIENTS NOT FLAGGED TO BILL HERE.
     C*** begin del ***  04/20/22  ******************************************
     C****** deleted code was here, can view in prior releases
     C*** end del *****  04/20/22  ******************************************
     C     ELARSM        IFNE      'Y'
     C     XXWAR         OREQ      'Y'
     C*
     C*  PRINT BILL & RETURN TO SCREEN 1 IF THERE IS NO BILLING CHANGE
     C*
     C     SCTOT         IFEQ      *ZERO
     C     CLTID         ANDNE     '139'
     C     SCTOT         OREQ      *ZERO
     C     X             ANDEQ     0
     C     CLTID         ANDEQ     '139'
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
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      'N'           SCDEP
     C                   MOVE      *BLANKS       SCAR
      /free
       scARD = ' ';
      /end-free
     C*
     C                   Z-ADD     0             SCEFDT
     C*
     C     XXDIVC        IFEQ      1
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             SCEFDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDT
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     SCTOT         ANDGT     *ZERO
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SCDT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     XXDT          IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   ENDIF                                                  XXDT IFLT
     C     CLTID         IFNE      '139'
     C     SCDT          IFNE      *ZERO
     C                   Z-ADD     SCDT          DA(1)
     C                   ELSE
     C                   Z-ADD     SCEFDT        DA(1)
     C                   ENDIF                                                  SCDT IFNE
     C     XXADJ         MULT(H)   XXAB01        XXWK90            9 0
     C     XXAB01        IFEQ      *ZERO
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   Z-ADD     XXWK90        XXADJ
     C                   Z-ADD     XXADJ         PA(1)
     C     XXWAR         IFEQ      'Y'
     C                   Z-ADD     XXDEPP        PA(1)
     C                   ENDIF
     C                   Z-ADD     XXTXDU        P1(1)
     C                   ENDIF                                                  CLTID IFNE
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
     C                   MOVEA     J1D           J2D
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
     C                   MOVE      'X'           EASTAT
     C                   ELSE
     C                   MOVE      'W'           EASTAT
     C                   ENDIF
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'Z'           EASTAT
     C                   ELSE
     C                   MOVE      'Y'           EASTAT
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C                   Z-ADD     EAPO01        SVDT7             7 0
     C*
     C* WRITE 1ST ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'D'           EASTAT
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 2ND ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'E'           EASTAT
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 3RD ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'F'           EASTAT
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 4TH ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'G'           EASTAT
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 5TH ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'H'           EASTAT
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C                   END
     C                   ENDIF                                                  ELARSM IFNE
     C*
     C                   END                                                    BSSEFL = 'S'
     C*
     C*  COMPUTE ADJUSTMENT IF NEW BILLING SCHEDULE DESIGNATES AN
     C*  ESTIMATING ACCOUNT
     C*
     C     BSSEFL        IFEQ      'E'
     C*
      /free
       Exsr DepositAdj;
       noEst = *off;
       If xxDEPP <> 0;
          noSR = *off;
       Endif;
      /end-free
     C     ELARSM        IFEQ      'Y'
     C     XXWAR         ANDNE     'Y'
     C                   MOVE      'Y'           XXSUM
     C     ELANPR        SUB       ELPRTD        X1ANPR           11 2
      /free
       // Reduce annual premium (X1ANPR) by the amount of dividend premium
       x1anpr = x1anpr - ELdvpr;
      /end-free
      /free
       Chain (xxCo#:xxFnd:xxFyr:scEmp#:xxDiv) pdv00;
       If %found and cltId = '111';
          x1AnPr = x1AnPr - DVamt;
       Endif;
      /end-free
     C                   ADD       X1ANPR        XTANPR
     C                   ADD       ELTXDU        XTTXDU
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
     C                   ADD       SVANPR        STANPR
     C                   ADD       SVTXDU        STTXDU
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
     C*
     C                   ELSE
     C*
     C     XXDIV         IFEQ      SCDIV
     C                   MOVE      *OFF          *IN81
     C                   ENDIF
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
     C                   Z-ADD     *ZEROS        YRMO              5 0
     C                   Z-ADD     *ZEROS        YYMM
     C                   Z-ADD     *ZEROS        XXAMT3           11 2
     C                   Z-ADD     *ZEROS        XSAMT3           11 2
     C                   Z-ADD     *ZEROS        X2AMT3           11 2
     C                   Z-ADD     *ZEROS        X1TOT            11 2
     C                   EVAL      x1TOT = x1TOT - xxTXDU
     C                   Z-ADD     *ZEROS        X1AMT1           11 2
     C                   Z-ADD     *ZEROS        X1AMT2           11 2
     C                   Z-ADD     *ZEROS        X1AMT3           11 2
     C                   MOVE      '0'           XXFUT             1
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
     C     XXWAR         IFEQ      'Y'
     C                   Z-ADD     XTANPR        X1ANPR
     C                   Z-ADD     XTTXDU        ELTXDU
     C                   Z-ADD     STANPR        SVANPR
     C                   Z-ADD     STTXDU        SVTXDU
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
     C                   Z-ADD     XT6THB        XX6THB           11 2
     C                   Z-ADD     ST6THB        SV6THB
     C                   Z-ADD     XT7THB        XX7THB           11 2
     C                   Z-ADD     ST7THB        SV7THB
     C                   Z-ADD     XL6THB        XU6THB           11 2
     C                   Z-ADD     SL6THB        SU6THB
     C                   Z-ADD     XL7THB        XU7THB           11 2
     C                   Z-ADD     SL7THB        SU7THB
     C                   ELSE
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
     C     ELANPR        SUB       ELPRTD        X1ANPR           11 2
      /free
       // Reduce annual premium (X1ANPR) by the amount of dividend premium
       x1anpr = x1anpr - ELdvpr;
      /end-free
      /free
       Chain (xxCo#:xxFnd:xxFyr:scEmp#:xxDiv) pdv00;
       If %found and cltId = '111';
          x1AnPr = x1AnPr - DVamt;
       Endif;
      /end-free
     C                   ENDIF
     C*
     C*  PROCESS NON-WEEKLY BILLING SCHEDULE CODES.
     C*
     C     BSDOWK        IFEQ      *BLANKS
     C     BSFREQ        ANDNE     'T'
     C*
     C*  FIRST LOAD BILLING SCHEDULE DATES INTO AN ARRAY FOR LATER
     C*  USE
     C*
     C                   Z-ADD     ELBSDT        XXDTE
     C                   MOVEL     XXDTE         YYMM
     C                   MOVE      XXDTE         DD                2 0
     C                   DO        12            X
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
     C                   Z-ADD     XXDTE         BD(X)
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   END
     C                   MOVEL     YYMM          XXDTE
     C     ELBLFL        IFNE      'Y'
     C                   MOVE      '01'          XXDTE
     C                   ENDIF                                                  ELBLFL IFNE
     C                   END
     C                   Z-ADD     *HIVAL        BD(13)
     C                   Z-ADD     12            X
     C     X             DOWGT     0
     C     AB(X)         IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        BD(X)
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*
     C                   Z-ADD     XXCURD        XXCUR2
     C                   If        xxCur2 < ELBsdt
     C                   Z-add     ELBsdt        xxCur2
     C                   Endif
     C*
     C                   MOVEL     XXCURD        YRMO1             5 0
     C                   MOVE      YRMO1         YRMO
     C                   MOVEL     ELBSDT        YYMM
     C                   Z-ADD     1             Y
     C                   Z-ADD     0             X
     C                   Z-ADD     *ZEROS        XXTOT
     C     Y             DOWLE     12
     C     YYMM          ANDLE     YRMO
     C     ELBLFL        ANDEQ     *BLANK
     C     Y             ORLE      12
     C     BD(Y)         ANDLE     XXCURD
     C     ELBLFL        ANDEQ     'Y'
     C     Y             ADD       1             W
     C                   Z-ADD     BD(Y)         XXDTE1
     C     BD(W)         IFGT      XXCURD
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C     ELBLFL        ANDEQ     XXBLFL
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
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   ADD       EXRVID        SRFUTR
     C     XXACDT        IFNE      *ZERO
     C                   Z-ADD     1             SRFUTR
     C                   ADD       EXAOID        SRFUTR
     C                   ENDIF                                                  XXACDT IFNE
     C                   ENDIF
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
     C     XXDTE2        IFLE      ELBSDT
     C                   Z-ADD     ELBSDT        XXDTE2
     C                   Z-add     xxDte2        xxCur2
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
     C                   Z-add     xxDte2        xxCur2
     C*
     C                   ENDIF                                                  SRRTCD IFEQ
     C                   ENDIF                                                  XXDTE2 IFLT
     C*
     C                   EXSR      SRWASB
     C     AB2(Y)        MULT(H)   X1ANPR        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
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
     C     XXAMT2        SUB       XXAMT1        XXDIFF
     C                   ADD       XXDIFF        XXTOT
     C*
     C*  CALCULATE THE STATE TAX DUE FOR THIS PERIOD
     C*
     C     AB2(Y)        MULT(H)   ELTXDU        X1AMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT2
     C                   ENDIF                                                  EXRNDI IFEQ
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
     C     X1AMT2        SUB       X1AMT1        XXDIFF
     C                   ADD       XXDIFF        X1TOT
     C                   ADD       X1AMT2        X1AMT3
     C                   ADD       X1AMT1        X2AMT3
     C     AB2(Y)        MULT(H)   XX1STB        X1AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV1STB        X1AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       SU1STB        X1AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X1AA2         SUB       X1AA1         XXDIFF
     C                   ADD       XXDIFF        X1ATOT
     C                   ADD       X1AA2         X1AA3
     C                   ADD       X1AA1         X1AS3
     C*
     C     AB2(Y)        MULT(H)   XX2NDB        X2AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV2NDB        X2AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       SU2NDB        X2AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X2AA2         SUB       X2AA1         XXDIFF
     C                   ADD       XXDIFF        X2ATOT
     C                   ADD       X2AA2         X2AA3
     C                   ADD       X2AA1         X2AS3
     C*
     C     AB2(Y)        MULT(H)   XX3RDB        X3AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV3RDB        X3AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA1
     C                   ENDIF                                                  EXRNDI IFEQ
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
     C     AB2(Y)        MULT(H)   XX4THB        X4AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV4THB        X4AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       SU4THB        X4AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X4AA2         SUB       X4AA1         XXDIFF
     C                   ADD       XXDIFF        X4ATOT
     C                   ADD       X4AA2         X4AA3
     C                   ADD       X4AA1         X4AS3
     C*
     C     AB2(Y)        MULT(H)   XX5THB        X5AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV5THB        X5AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       SU5THB        X5AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X5AA2         SUB       X5AA1         XXDIFF
     C                   ADD       XXDIFF        X5ATOT
     C                   ADD       X5AA2         X5AA3
     C                   ADD       X5AA1         X5AS3
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
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       XXAMT2        XXAMT3
     C                   ADD       XXAMT1        XSAMT3
     C                   ADD       1             Y
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   END
     C                   END                                                    DOW Y <= 12
     C*                                                     &  YYMM <= YRMO
     C                   MOVE      *BLANK        XXFLAG            1
     C     Y             DO        12            Z
     C     AB2(Z)        IFNE      *ZEROS
     C                   MOVE      'Y'           XXFLAG
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     XXFLAG        IFEQ      *BLANK
     C     *IN67         ANDEQ     *OFF
     C     XXFLAG        OREQ      *BLANK
     C     ELARSM        ANDNE     'Y'
     C*
     C                   Z-ADD     XXDTE2        XXDTE1
     C                   Z-ADD     *HIVAL        XXDTE2
     C                   EXSR      SRWASB
     C                   SUB       XXAMT1        XXTOT
     C                   SUB       X1AMT1        X1TOT
     C                   SUB       X1AA1         X1ATOT
     C                   SUB       X2AA1         X2ATOT
     C                   SUB       X3AA1         X3ATOT
     C                   SUB       X4AA1         X4ATOT
     C                   SUB       X5AA1         X5ATOT
     C                   SUB       X6AA1         X6ATOT
     C                   SUB       X7AA1         X7ATOT
     C*
     C                   ENDIF                                                  XXFLAG IFEQ
     C*
     C     XXTOT         IFNE      *ZEROS
     C     X1TOT         ORNE      *ZEROS
     C     X1ATOT        ORNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C                   ADD       1             X
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         DA(X)
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXTOT         ANDGT     *ZERO
     C     XXTOT         OREQ      *ZERO
     C     X1TOT         ANDGT     *ZEROS
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
     C     XXTOT         OREQ      *ZERO
     C     X6ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X7ATOT        ANDGT     *ZEROS
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        DA(X)
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* IF THE POSTING DATE IS PRIOR TO THE POLICY EFFECTIVE DATE,
     C* CHANGE IT.
     C*
     C                   Z-ADD     DA(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C     XXTDT         IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C                   ENDIF                                                  XXTDT IFLT
     C*
     C*** Begin Add ***  07/12/24  ********************************
     C                   MOVE      DA(X)         D1(X)
     C                   MOVE      DA(X)         DA1(X)
     C                   MOVE      DA(X)         DA2(X)
     C                   MOVE      DA(X)         DA3(X)
     C                   MOVE      DA(X)         DA4(X)
     C                   MOVE      DA(X)         DA5(X)
     C                   MOVE      DA(X)         DA6(X)
     C                   MOVE      DA(X)         DA7(X)
     C*** End   Add ***  07/12/24  ********************************
     C                   Z-ADD     XXTOT         PA(X)
     C                   Z-ADD     X1TOT         P1(X)
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
     C                   END                                                    XXTOT <> 0
     C*
     C     ELBLFL        IFNE      'Y'
     C                   MOVE      '01'          XXDTE
     C                   ENDIF                                                  ELBLFL IFNE
     C     Y             DO        12            Z
     C     AB2(Z)        IFNE      *ZEROS
     C*
     C*  IF THE BILLING SCHEDLUE CODE DID NOT CHANGE WE MUST COMPARE
     C*  WHAT WAS BILLED FOR A GIVEN MONTH TO WHAT SHOULD BE BILLED
     C*  THAT MONTH UNDER THE NEW BILLING SCHEDULE CODE
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
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
     C     XXDTE1        IFLT      ELBSDT
     C                   Z-ADD     BD(Z)         XXDTE1
     C                   ENDIF                                                  XXDTE1 IFLT
     C*
     C                   EXSR      SRWASB
     C     AB2(Z)        MULT(H)   X1ANPR        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
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
     C     XXAMT2        SUB       XXAMT1        XXTOT
     C     AB2(Z)        MULT(H)   ELTXDU        X1AMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT2
     C                   ENDIF                                                  EXRNDI IFEQ
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
     C     X1AMT2        SUB       X1AMT1        X1TOT
     C                   ADD       X1AMT2        X1AMT3
     C                   ADD       X1AMT1        X2AMT3
      *
     C* 1ST ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX1STB        X1AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV1STB        X1AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       SU1STB        X1AA1
     C                   ENDIF
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
     C     Z             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV2NDB        X2AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       SU2NDB        X2AA1
     C                   ENDIF
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
     C     Z             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV3RDB        X3AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       SU3RDB        X3AA1
     C                   ENDIF
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
     C     Z             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV4THB        X4AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       SU4THB        X4AA1
     C                   ENDIF
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
     C     Z             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV5THB        X5AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       SU5THB        X5AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X5AA2         SUB       X5AA1         X5ATOT
     C                   ADD       X5AA2         X5AA3
     C                   ADD       X5AA1         X5AS3
     C*
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
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       XXAMT2        XXAMT3
     C                   ADD       XXAMT1        XSAMT3
     C     XXTOT         IFNE      *ZEROS
     C     X1TOT         ORNE      *ZEROS
     C     X1ATOT        ORNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C                   ADD       1             X
     C*
     C                   Z-ADD     BD(Z)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C*** Begin Add ***  07/12/24  ********************************
     C                   MOVEL     SRDTO         D1(X)
     C                   MOVEL     SRDTO         DA1(X)
     C                   MOVEL     SRDTO         DA2(X)
     C                   MOVEL     SRDTO         DA3(X)
     C                   MOVEL     SRDTO         DA4(X)
     C                   MOVEL     SRDTO         DA5(X)
     C                   MOVEL     SRDTO         DA6(X)
     C                   MOVEL     SRDTO         DA7(X)
     C*** End   Add ***  07/12/24  ********************************
     C*
     C                   Z-ADD     XXTOT         PA(X)
     C                   Z-ADD     X1TOT         P1(X)
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
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
     C*** Begin Add ***  07/12/24  ********************************
     C                   MOVEL     SRDTO         D1(X)
     C                   MOVEL     SRDTO         DA1(X)
     C                   MOVEL     SRDTO         DA2(X)
     C                   MOVEL     SRDTO         DA3(X)
     C                   MOVEL     SRDTO         DA4(X)
     C                   MOVEL     SRDTO         DA5(X)
     C                   MOVEL     SRDTO         DA6(X)
     C                   MOVEL     SRDTO         DA7(X)
     C*** End   Add ***  07/12/24  ********************************
     C*
     C     AB2(Z)        MULT(H)   X1ANPR        PA(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  PA(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        PA(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     AB2(Z)        MULT(H)   ELTXDU        P1(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  P1(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        P1(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       P1(X)         X1AMT3
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
     C**TAKE*THE FIRST ADJUSTMENT, WHICH SHOULD BE OF ALL PAST AND
     C**CURRENT ADJUSTMENTS, AND SPREAD IT ACROSS ALL FUTURE THE
     C**FUTURE*INSTALLMENTS.
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
     C     A6(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A7(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
      *
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   ENDIF                                                  XXACDT IFNE
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
     C     XU6THB        IFEQ      0
     C                   SUB       A6(1)         X6AA3
     C                   ENDIF
     C     XU7THB        IFEQ      0
     C                   SUB       A7(1)         X7AA3
     C                   ENDIF
     C     1             DO        XXINST        X
     C     X             ADD       1             W
     C     PA(1)         DIV(H)    XXINST        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       XXAMT2        PA(W)
     C                   ADD       XXAMT2        XXAMT3
     C     P1(1)         DIV(H)    XXINST        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       XXAMT2        P1(W)
     C                   ADD       XXAMT2        X1AMT3
     C* 1ST ASSESSMENT
     C     XU1STB        IFEQ      0
     C     A1(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A1(W)
     C                   ADD       XXAMT2        X1AA3
     C                   ENDIF                                                  XU1STB IFEQ
     C*
     C* 2ND ASSESSMENT
     C     XU2NDB        IFEQ      0
     C     A2(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A2(W)
     C                   ADD       XXAMT2        X2AA3
     C                   ENDIF                                                  XU2NDB IFEQ
     C*
     C* 3RD ASSESSMENT
     C     XU3RDB        IFEQ      0
     C     A3(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A3(W)
     C                   ADD       XXAMT2        X3AA3
     C                   ENDIF                                                  XU3RDB IFEQ
     C*
     C* 4TH ASSESSMENT
     C     XU4THB        IFEQ      0
     C     A4(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A4(W)
     C                   ADD       XXAMT2        X4AA3
     C                   ENDIF                                                  XU4THB IFEQ
     C*
     C* 5TH ASSESSMENT
     C     XU5THB        IFEQ      0
     C     A5(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A5(W)
     C                   ADD       XXAMT2        X5AA3
     C                   ENDIF                                                  XU5THB IFEQ
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
     C                   ENDDO
     C*
     C     2             DO        12            X
     C     X             SUB       1             W
     C                   Z-ADD     PA(X)         PA(W)
     C                   Z-ADD     DA(X)         DA(W)
     C*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     D1(X)         D1(W)
     C                   Z-ADD     DA1(X)        DA1(W)
     C                   Z-ADD     DA2(X)        DA2(W)
     C                   Z-ADD     DA3(X)        DA3(W)
     C                   Z-ADD     DA4(X)        DA4(W)
     C                   Z-ADD     DA5(X)        DA5(W)
     C                   Z-ADD     DA6(X)        DA6(W)
     C                   Z-ADD     DA7(X)        DA7(W)
     C*** End   Add ***  07/12/24  ********************************
     C                   Z-ADD     0             PA(X)
     C                   Z-ADD     0             DA(X)
     C*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     0             D1(X)
     C                   Z-ADD     0             DA1(X)
     C                   Z-ADD     0             DA2(X)
     C                   Z-ADD     0             DA3(X)
     C                   Z-ADD     0             DA4(X)
     C                   Z-ADD     0             DA5(X)
     C                   Z-ADD     0             DA6(X)
     C                   Z-ADD     0             DA7(X)
     C*** End   Add ***  07/12/24  ********************************
     C                   Z-ADD     P1(X)         P1(W)
     C                   Z-ADD     0             P1(X)
     C     XU1STB        IFEQ      0
     C                   Z-ADD     A1(X)         A1(W)
     C                   Z-ADD     0             A1(X)
     C                   ENDIF

      /Free
       // Add assessment adjustment to previous installment
       //  if an installment date doesn't exist for it. This
       //  can happen under a litany of conditions.
       If xu1stb <> 0 and a1(x) <> 0 and da(x) = 0;
          a1(w) = a1(w) + a1(x);
          a1(x) = 0;
       Endif;
      /End-Free

     C     XU2NDB        IFEQ      0
     C                   Z-ADD     A2(X)         A2(W)
     C                   Z-ADD     0             A2(X)
     C                   ENDIF

      /Free
       If xu2ndb <> 0 and a2(x) <> 0 and da(x) = 0;
          a2(w) = a2(w) + a2(x);
          a2(x) = 0;
       Endif;
      /End-Free

     C     XU3RDB        IFEQ      0
     C                   Z-ADD     A3(X)         A3(W)
     C                   Z-ADD     0             A3(X)
     C                   ENDIF

      /Free
       If xu3rdb <> 0 and a3(x) <> 0 and da(x) = 0;
          a3(w) = a3(w) + a3(x);
          a3(x) = 0;
       Endif;
      /End-Free

     C     XU4THB        IFEQ      0
     C                   Z-ADD     A4(X)         A4(W)
     C                   Z-ADD     0             A4(X)
     C                   ENDIF

      /Free
       If xu4thb <> 0 and a4(x) <> 0 and da(x) = 0;
          a4(w) = a4(w) + a4(x);
          a4(x) = 0;
       Endif;
      /End-Free

     C     XU5THB        IFEQ      0
     C                   Z-ADD     A5(X)         A5(W)
     C                   Z-ADD     0             A5(X)
     C                   ENDIF

      /Free
       If xu5thb <> 0 and a5(x) <> 0 and da(x) = 0;
          a5(w) = a5(w) + a5(x);
          a5(x) = 0;
       Endif;
      /End-Free

     C     XU6THB        IFEQ      0
     C                   Z-ADD     A6(X)         A6(W)
     C                   Z-ADD     0             A6(X)
     C                   ENDIF

      /Free
       If xu6thb <> 0 and a6(x) <> 0 and da(x) = 0;
          a6(w) = a6(w) + a6(x);
          a6(x) = 0;
       Endif;
      /End-Free

     C     XU7THB        IFEQ      0
     C                   Z-ADD     A7(X)         A7(W)
     C                   Z-ADD     0             A7(X)
     C                   ENDIF

      /Free
       If xu7thb <> 0 and a7(x) <> 0 and da(x) = 0;
          a7(w) = a7(w) + a7(x);
          a7(x) = 0;
       Endif;
      /End-Free

     C                   ENDDO
     C*
     C                   ENDIF                                                  XXINST IFGE
     C*
     C                   ENDIF                                                  XXDT1 IFLE
     C*
     C                   ENDIF                                                  PA,1 IFGT
     C*
     C*
     C*  CHECK FOR ANY ROUNDING DISCREPANCIES BETWEEN CURRENT A/R AND
     C*  NEW ADJUSTMENTS THAT WILL BE POSTED TO A/R; BOTH SHOULD EQUAL
     C*  THE NEW PREMIUM AMOUNT TO THE PENNY
     C*
     C     X1ANPR        SUB       XXAMT3        XXAMT3
     C     ELTXDU        SUB       X1AMT3        X1AMT3
     C     XX1STB        SUB       X1AA3         X1AA3
     C     XX2NDB        SUB       X2AA3         X2AA3
     C     XX3RDB        SUB       X3AA3         X3AA3
     C     XX4THB        SUB       X4AA3         X4AA3
     C     XX5THB        SUB       X5AA3         X5AA3
     C     XX6THB        SUB       X6AA3         X6AA3
     C     XX7THB        SUB       X7AA3         X7AA3
     C                   ADD       XU1STB        X1AA3
     C                   ADD       XU2NDB        X2AA3
     C                   ADD       XU3RDB        X3AA3
     C                   ADD       XU4THB        X4AA3
     C                   ADD       XU5THB        X5AA3
     C                   ADD       XU6THB        X6AA3
     C                   ADD       XU7THB        X7AA3
     C*
     C* ROUNDING FOR PRIOR PREMIUM ON A/R SUMMARIZED SINGLE DIVISION.
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     SVANPR        SUB       XSAMT3        XSAMT3
     C     SVTXDU        SUB       X2AMT3        X2AMT3
     C     SV1STB        SUB       X1AS3         X1AS3
     C     SV2NDB        SUB       X2AS3         X2AS3
     C     SV3RDB        SUB       X3AS3         X3AS3
     C     SV4THB        SUB       X4AS3         X4AS3
     C     SV5THB        SUB       X5AS3         X5AS3
     C     SV6THB        SUB       X6AS3         X6AS3
     C     SV7THB        SUB       X7AS3         X7AS3
     C                   ADD       SU1STB        X1AS3
     C                   ADD       SU2NDB        X2AS3
     C                   ADD       SU3RDB        X3AS3
     C                   ADD       SU4THB        X4AS3
     C                   ADD       SU5THB        X5AS3
     C                   ADD       SU6THB        X6AS3
     C                   ADD       SU7THB        X7AS3
     C*
     C     XSAMT3        IFLT      1.00
     C     XSAMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         OREQ      *ZERO
     C                   SUB       XSAMT3        PA(1)
     C                   ELSE
     C                   SUB       XSAMT3        PA(2)
     C                   ENDIF
     C                   END
     C*
     C     X2AMT3        IFLT      1.00
     C     X2AMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     P1(1)         IFNE      *ZERO
     C     P1(2)         OREQ      *ZERO
     C                   SUB       X2AMT3        P1(1)
     C                   ELSE
     C                   SUB       X2AMT3        P1(2)
     C                   ENDIF
     C                   END
     C*
     C*
     C     X1AS3         IFLT      1.00
     C     X1AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A1(1)         IFNE      *ZERO
     C     A1(2)         OREQ      *ZERO
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
     C     A2(2)         OREQ      *ZERO
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
     C     A3(2)         OREQ      *ZERO
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
     C     A4(2)         OREQ      *ZERO
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
     C     A5(2)         OREQ      *ZERO
     C                   SUB       X5AS3         A5(1)
     C                   ELSE
     C                   SUB       X5AS3         A5(2)
     C                   ENDIF
     C                   END
     C*
     C     X6AS3         IFLT      1.00
     C     X6AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A6(1)         IFNE      *ZERO
     C     A6(2)         OREQ      *ZERO
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
     C     A7(2)         OREQ      *ZERO
     C                   SUB       X7AS3         A7(1)
     C                   ELSE
     C                   SUB       X7AS3         A7(2)
     C                   ENDIF
     C                   END
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C*  DIFFERENCE SHOULD NEVER BE GREATER THAN OR LESS THAN $1.00
     C*
     C     XXAMT3        IFLT      1.00
     C     XXAMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         OREQ      *ZERO
     C                   ADD       XXAMT3        PA(1)
     C                   ELSE
     C                   ADD       XXAMT3        PA(2)
     C                   ENDIF
     C                   END
     C     X1AMT3        IFLT      1.00
     C     X1AMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     P1(1)         IFNE      *ZERO
     C     P1(2)         OREQ      *ZERO
     C                   ADD       X1AMT3        P1(1)
     C                   ELSE
     C                   ADD       X1AMT3        P1(2)
     C                   ENDIF
     C                   END
     C*
     C*
     C     X1AA3         IFLT      1.00
     C     X1AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A1(1)         IFNE      *ZERO
     C     A1(2)         OREQ      *ZERO
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
     C     A2(2)         OREQ      *ZERO
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
     C     A3(2)         OREQ      *ZERO
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
     C     A4(2)         OREQ      *ZERO
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
     C     A5(2)         OREQ      *ZERO
     C                   ADD       X5AA3         A5(1)
     C                   ELSE
     C                   ADD       X5AA3         A5(2)
     C                   ENDIF
     C                   END
     C*
     C     X6AA3         IFLT      1.00
     C     X6AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A6(1)         IFNE      *ZERO
     C     A6(2)         OREQ      *ZERO
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
     C     A7(2)         OREQ      *ZERO
     C                   ADD       X7AA3         A7(1)
     C                   ELSE
     C                   ADD       X7AA3         A7(2)
     C                   ENDIF
     C                   END
     C*
     C* If there are no amounts left in the 1st installment slot after the rounding
     C* is completed then move up all of the later amounts to compenstate.
     C*
     C                   If        PA(1) = 0 and P1(1) = 0 and A1(1) = 0 and
     C                             A2(1) = 0 and A3(1) = 0 and A4(1) = 0 and
     C                             A5(1) = 0 and A6(1) = 0 and A7(1) = 0
     C*
     C     2             DO        12            X
     C     X             SUB       1             W
     C                   Z-ADD     PA(X)         PA(W)
     C                   Z-ADD     DA(X)         DA(W)
     C*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     D1(X)         D1(W)
     C                   Z-ADD     DA1(X)        DA1(W)
     C                   Z-ADD     DA2(X)        DA2(W)
     C                   Z-ADD     DA3(X)        DA3(W)
     C                   Z-ADD     DA4(X)        DA4(W)
     C                   Z-ADD     DA5(X)        DA5(W)
     C                   Z-ADD     DA6(X)        DA6(W)
     C                   Z-ADD     DA7(X)        DA7(W)
     C*** End   Add ***  07/12/24  ********************************
     C                   Z-ADD     0             PA(X)
     C                   Z-ADD     0             DA(X)
     C*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     0             D1(X)
     C                   Z-ADD     0             DA1(X)
     C                   Z-ADD     0             DA2(X)
     C                   Z-ADD     0             DA3(X)
     C                   Z-ADD     0             DA4(X)
     C                   Z-ADD     0             DA5(X)
     C                   Z-ADD     0             DA6(X)
     C                   Z-ADD     0             DA7(X)
     C*** End   Add ***  07/12/24  ********************************
     C                   Z-ADD     P1(X)         P1(W)
     C                   Z-ADD     0             P1(X)
     C                   Z-ADD     A1(X)         A1(W)
     C                   Z-ADD     0             A1(X)
     C                   Z-ADD     A2(X)         A2(W)
     C                   Z-ADD     0             A2(X)
     C                   Z-ADD     A3(X)         A3(W)
     C                   Z-ADD     0             A3(X)
     C                   Z-ADD     A4(X)         A4(W)
     C                   Z-ADD     0             A4(X)
     C                   Z-ADD     A5(X)         A5(W)
     C                   Z-ADD     0             A5(X)
     C                   Z-ADD     A6(X)         A6(W)
     C                   Z-ADD     0             A6(X)
     C                   Z-ADD     A7(X)         A7(W)
     C                   Z-ADD     0             A7(X)
     C                   ENDDO
     C*
     C                   Endif                                                  1st array slot empty
     C                   ELSE                                                   BSDOWK IFEQ
     C     BSFREQ        IFEQ      'T'
      *
      * PROCESS AND BILL TO A/R TWICE A MONTH BILLLING INSTALLMENTS.
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
     C                   Z-ADD     0             WK6
     C                   Z-ADD     0             WK7
     C*
     C                   Z-ADD     0             SK6
     C                   Z-ADD     0             SK7
     C                   Z-ADD     0             WKD
     C                   Z-ADD     0             WKA
     C                   Z-ADD     0             WKT
     C                   Z-ADD     0             SKA
     C                   Z-ADD     0             SKT
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
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SVANPR        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C                   Z-ADD     SVAMT2        SKA(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C     XXCOMP        IFNE      X1ANPR
     C     X1ANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C                   XFOOT     SKA           XXCOMP           15 4
     C     XXCOMP        IFNE      SVANPR
     C     SVANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKA(1)
     C                   ENDIF
     C*
     C* STATE TAX
     C*
     C     ELTXDU        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SVTXDU        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKT(U)
     C                   Z-ADD     SVAMT2        SKT(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKT           XXCOMP           15 4
     C     XXCOMP        IFNE      ELTXDU
     C     ELTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKT(1)
     C                   ENDIF
     C                   XFOOT     SKT           XXCOMP           15 4
     C     XXCOMP        IFNE      SVTXDU
     C     SVTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKT(1)
     C                   ENDIF
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
     C*
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
     C*
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
     C*
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
     C*
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* CALCULATE A/R ADJUSTMENTS.
     C*
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C                   Z-ADD     0             SVAMT2
     C                   Z-ADD     0             S1AMT2           11 2
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
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C*
     C                   Z-ADD     0             SV6AA2           11 2
     C                   Z-ADD     0             SV7AA2           11 2
     C*
     C                   ADD       XU1STB        WK1(1)
     C                   ADD       XU2NDB        WK2(1)
     C                   ADD       XU3RDB        WK3(1)
     C                   ADD       XU4THB        WK4(1)
     C                   ADD       XU5THB        WK5(1)
     C                   ADD       XU6THB        WK6(1)
     C                   ADD       XU7THB        WK7(1)
     C*
     C                   ADD       SU1STB        SK1(1)
     C                   ADD       SU2NDB        SK2(1)
     C                   ADD       SU3RDB        SK3(1)
     C                   ADD       SU4THB        SK4(1)
     C                   ADD       SU5THB        SK5(1)
     C                   ADD       SU6THB        SK6(1)
     C                   ADD       SU7THB        SK7(1)
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
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
     C*
     C                   ADD       WK1(U)        X1AA2
     C                   ADD       WK2(U)        X2AA2
     C                   ADD       WK3(U)        X3AA2
     C                   ADD       WK4(U)        X4AA2
     C                   ADD       WK5(U)        X5AA2
     C                   ADD       WK6(U)        X6AA2
     C                   ADD       WK7(U)        X7AA2
     C*
     C                   ADD       SK1(U)        SV1AA2
     C                   ADD       SK2(U)        SV2AA2
     C                   ADD       SK3(U)        SV3AA2
     C                   ADD       SK4(U)        SV4AA2
     C                   ADD       SK5(U)        SV5AA2
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
     C*
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
     C*
     C     WK1(U)        ADD       X1AA2         A1(Y)
     C     WK2(U)        ADD       X2AA2         A2(Y)
     C     WK3(U)        ADD       X3AA2         A3(Y)
     C     WK4(U)        ADD       X4AA2         A4(Y)
     C     WK5(U)        ADD       X5AA2         A5(Y)
     C     WK6(U)        ADD       X6AA2         A6(Y)
     C     WK7(U)        ADD       X7AA2         A7(Y)
     C*
     C                   ADD       SK1(U)        SV1AA2
     C                   ADD       SK2(U)        SV2AA2
     C                   ADD       SK3(U)        SV3AA2
     C                   ADD       SK4(U)        SV4AA2
     C                   ADD       SK5(U)        SV5AA2
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
     C*
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
     C     XXAMT2        IFNE      *ZERO
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C*
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C*
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
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ORNE      XXBLFL
     C     Y             ANDEQ     1
     c*** end add *****  10/18/24  *******************************************
     C                   Z-ADD     XXCURD        XXDTE2
     C                   ENDIF                                                  ELBSCD IFNE
     C*
     C                   EXSR      SRWASB
     C*
     C* BILL FULL AMOUNT IF CHANGING BILLING SCHEDULE CODE.
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     Y             OREQ      1
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     SVAMT2        XXAMT1
     C                   Z-ADD     S1AMT2        X1AMT1
     C*
     C                   ADD       SV1AA2        X1AA1
     C                   ADD       SV2AA2        X2AA1
     C                   ADD       SV3AA2        X3AA1
     C                   ADD       SV4AA2        X4AA1
     C                   ADD       SV5AA2        X5AA1
     C                   ADD       SV6AA2        X6AA1
     C                   ADD       SV7AA2        X7AA1
     C*
     C                   ENDIF
     C                   SUB       XXAMT1        PA(Y)
     C                   SUB       X1AMT1        P1(Y)
     C*
     C                   SUB       X1AA1         A1(Y)
     C                   SUB       X2AA1         A2(Y)
     C                   SUB       X3AA1         A3(Y)
     C                   SUB       X4AA1         A4(Y)
     C                   SUB       X5AA1         A5(Y)
     C                   SUB       X6AA1         A6(Y)
     C                   SUB       X7AA1         A7(Y)
     C*
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
     C     PA(1)         IFEQ      *ZERO
     C     P1(1)         ANDEQ     *ZERO
     C     A1(1)         ANDEQ     *ZERO
     C     A2(1)         ANDEQ     *ZERO
     C     A3(1)         ANDEQ     *ZERO
     C     A4(1)         ANDEQ     *ZERO
     C     A5(1)         ANDEQ     *ZERO
     C     A6(1)         ANDEQ     *ZERO
     C     A7(1)         ANDEQ     *ZERO
     C*
     C     PA(2)         IFNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(2)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C                   Z-ADD     PA(2)         PA(1)
     C                   Z-ADD     P1(2)         P1(1)
     C                   Z-ADD     A1(2)         A1(1)
     C                   Z-ADD     A2(2)         A2(1)
     C                   Z-ADD     A3(2)         A3(1)
     C                   Z-ADD     A4(2)         A4(1)
     C                   Z-ADD     A5(2)         A5(1)
     C                   Z-ADD     A6(2)         A6(1)
     C                   Z-ADD     A7(2)         A7(1)
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
     C                   Z-ADD     0             A1(2)
     C                   Z-ADD     0             A2(2)
     C                   Z-ADD     0             A3(2)
     C                   Z-ADD     0             A4(2)
     C                   Z-ADD     0             A5(2)
     C                   Z-ADD     0             A6(2)
     C                   Z-ADD     0             A7(2)
     C                   Z-ADD     0             DA(2)
     C                   ENDIF                                                  Z IFGT
     C*
     C                   ENDIF                                                  PA,2 IFNE
     C*
     C                   ENDIF                                                  PA,1 IFEQ
     C*
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         ORNE      *ZERO
     C     P1(1)         ORNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(1)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
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
     C                   Z-ADD     2             X
     C                   ENDIF
     C*
     C                   ELSE                                                   BSFREQ IFEQ
     C*
     C*  PROCESS WEEKLY BILLING SCHEDULE CODES.
     C*
     C                   Z-ADD     0             WKD
     C                   Z-ADD     0             WKA
     C                   Z-ADD     0             WKT
     C                   Z-ADD     0             SKA
     C                   Z-ADD     0             SKT
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
     C                   Z-ADD     0             WK6
     C                   Z-ADD     0             WK7
     C                   Z-ADD     0             SK6
     C                   Z-ADD     0             SK7
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
     C     TDAYS         IFGT      365
     C                   Z-ADD     365           TDAYS
     C                   ENDIF
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
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SVANPR        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C                   Z-ADD     SVAMT2        SKA(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C     XXCOMP        IFNE      X1ANPR
     C     X1ANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C                   XFOOT     SKA           XXCOMP           15 4
     C     XXCOMP        IFNE      SVANPR
     C     SVANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKA(1)
     C                   ENDIF
     C*
     C* WEEKLY STATE TAX
     C*
     C     ELTXDU        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SVTXDU        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKT(U)
     C                   Z-ADD     SVAMT2        SKT(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKT           XXCOMP           15 4
     C     XXCOMP        IFNE      ELTXDU
     C     ELTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKT(1)
     C                   ENDIF
     C                   XFOOT     SKT           XXCOMP           15 4
     C     XXCOMP        IFNE      SVTXDU
     C     SVTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKT(1)
     C                   ENDIF
     C*
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
     C*
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
     C*
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
     C*
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
     C*
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* CALCULATE A/R ADJUSTMENTS.
     C*
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C                   Z-ADD     0             SVAMT2
     C                   Z-ADD     0             S1AMT2
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
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C                   Z-ADD     0             SV6AA2           11 2
     C                   Z-ADD     0             SV7AA2           11 2
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
     C                   ADD       XU6THB        WK6(1)
     C                   ADD       XU7THB        WK7(1)
     C*
     C                   ADD       SU6THB        SK6(1)
     C                   ADD       SU7THB        SK7(1)
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
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
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
     C                   ADD       WK6(U)        X6AA2
     C                   ADD       WK7(U)        X7AA2
     C*
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
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
     C     WK6(U)        ADD       X6AA2         A6(Y)
     C     WK7(U)        ADD       X7AA2         A7(Y)
     C*
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
     C     XXAMT2        IFNE      *ZERO
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
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
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ORNE      XXBLFL
     C     Y             ANDEQ     1
     c*** end add *****  10/18/24  *******************************************
     C                   Z-ADD     XXCURD        XXDTE2
     C                   ENDIF                                                  ELBSCD IFNE
     C*
     C                   EXSR      SRWASB
     C*
     C* BILL FULL WEEKLY AMOUNT IF CHANGING BILLING SCHEDULE CODE.
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     Y             OREQ      1
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     SVAMT2        XXAMT1
     C                   Z-ADD     S1AMT2        X1AMT1
     C                   Z-ADD     SV1AA2        X1AA1
     C                   Z-ADD     SV2AA2        X2AA1
     C                   Z-ADD     SV3AA2        X3AA1
     C                   Z-ADD     SV4AA2        X4AA1
     C                   Z-ADD     SV5AA2        X5AA1
     C                   Z-ADD     SV6AA2        X6AA1
     C                   Z-ADD     SV7AA2        X7AA1
     C                   ENDIF
     C                   SUB       XXAMT1        PA(Y)
     C                   SUB       X1AMT1        P1(Y)
     C                   SUB       X1AA1         A1(Y)
     C                   SUB       X2AA1         A2(Y)
     C                   SUB       X3AA1         A3(Y)
     C                   SUB       X4AA1         A4(Y)
     C                   SUB       X5AA1         A5(Y)
     C                   SUB       X6AA1         A6(Y)
     C                   SUB       X7AA1         A7(Y)
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
     C     PA(1)         IFEQ      *ZERO
     C     P1(1)         ANDEQ     *ZERO
     C     A1(1)         ANDEQ     *ZERO
     C     A2(1)         ANDEQ     *ZERO
     C     A3(1)         ANDEQ     *ZERO
     C     A4(1)         ANDEQ     *ZERO
     C     A5(1)         ANDEQ     *ZERO
     C     A6(1)         ANDEQ     *ZERO
     C     A7(1)         ANDEQ     *ZERO
     C     PA(2)         IFNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(2)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C                   Z-ADD     PA(2)         PA(1)
     C                   Z-ADD     P1(2)         P1(1)
     C                   Z-ADD     A1(2)         A1(1)
     C                   Z-ADD     A2(2)         A2(1)
     C                   Z-ADD     A3(2)         A3(1)
     C                   Z-ADD     A4(2)         A4(1)
     C                   Z-ADD     A5(2)         A5(1)
     C                   Z-ADD     A6(2)         A6(1)
     C                   Z-ADD     A7(2)         A7(1)
     C                   Z-ADD     DA(2)         DA(1)
     C                   Z-ADD     0             A1(2)
     C                   Z-ADD     0             A2(2)
     C                   Z-ADD     0             A3(2)
     C                   Z-ADD     0             A4(2)
     C                   Z-ADD     0             A5(2)
     C                   Z-ADD     0             A6(2)
     C                   Z-ADD     0             A7(2)
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
     C*
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         ORNE      *ZERO
     C     P1(1)         ORNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(1)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
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
     C*
     C                   Z-ADD     2             X
     C                   ENDIF
     C*
     C                   ENDIF                                                  BSFREQ IFEQ
     C                   ENDIF                                                  BSDOWK IFEQ
     C*
     C*  TEST TO SEE IF ANY ADJUSTMENTS ARE BEING POSTED
     C*
     C                   XFOOT     PA            SCTOT
     C*
     C*  ADD THE STATE TAX ADJUSTMENTS TO TOTAL ADJUSTMENT
     C*
     C                   XFOOT     P1            XXTEST
     C                   ADD       XXTEST        SCTOT
     C                   XFOOT     A1            SCTOT2
     C                   XFOOT     A2            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A3            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A4            SCTOT3
     C                   XFOOT     A5            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A6            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A7            SCTOT4
     C     *IN62         IFEQ      *ON
     C*
     C                   Z-ADD     Z             SCINST
     C     PA(2)         MULT      Z             SCTOT
     C                   ADD       PA(1)         SCTOT
     C     P1(2)         MULT      Z             XXTEST
     C                   ADD       P1(1)         XXTEST
     C                   ADD       XXTEST        SCTOT
     C     A1(2)         MULT      Z             SCTOT2
     C                   ADD       A1(1)         SCTOT2
     C     A2(2)         MULT      Z             XXTEST
     C                   ADD       A2(1)         XXTEST
     C                   ADD       XXTEST        SCTOT2
     C     A3(2)         MULT      Z             XXTEST
     C                   ADD       A3(1)         XXTEST
     C                   ADD       XXTEST        SCTOT2
     C     A4(2)         MULT      Z             SCTOT3
     C                   ADD       A4(1)         SCTOT3
     C     A5(2)         MULT      Z             XXTEST
     C                   ADD       A5(1)         XXTEST
     C                   ADD       XXTEST        SCTOT3
     C     A6(2)         MULT      Z             XXTEST
     C                   ADD       A6(1)         XXTEST
     C                   ADD       XXTEST        SCTOT3
     C     A7(2)         MULT      Z             XXTEST
     C                   ADD       A7(1)         XXTEST
     C                   ADD       XXTEST        SCTOT4
     C*
     C     Z             IFEQ      0
     C                   MOVE      *ON           *IN63
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN62 IFEQ
     C                   MOVE      '0'           *IN80
     C                   Z-ADD     12            X
     C     X             DOWGT     0
     C     PA(X)         IFNE      *ZERO
     C     P1(X)         ORNE      *ZERO
     C     A1(X)         ORNE      *ZERO
     C     A2(X)         ORNE      *ZERO
     C     A3(X)         ORNE      *ZERO
     C     A4(X)         ORNE      *ZERO
     C     A5(X)         ORNE      *ZERO
     C     A6(X)         ORNE      *ZERO
     C     A7(X)         ORNE      *ZERO
     C                   LEAVE
     C                   ELSE
     C                   Z-ADD     0             DA(X)
     C*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     0             D1(X)
     C                   Z-ADD     0             DA1(X)
     C                   Z-ADD     0             DA2(X)
     C                   Z-ADD     0             DA3(X)
     C                   Z-ADD     0             DA4(X)
     C                   Z-ADD     0             DA5(X)
     C                   Z-ADD     0             DA6(X)
     C                   Z-ADD     0             DA7(X)
     C*** End   Add ***  07/12/24  ********************************
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
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
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      'N'           SCDEP
     C                   MOVE      *BLANKS       SCAR
      /free
       scARD = ' ';
      /end-free
     C*
     C                   Z-ADD     0             SCEFDT
     C*
     C     XXDIVC        IFEQ      1
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
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
     C                   MOVEA     J1D           J2D
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
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     BSFREQ        IFEQ      'T'
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
     C                   EXSR      SREAP
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
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C                   MOVEL     SRDTO         EADT
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C* Write deposit record for estimating accounts.
     C*
     C                   If        xxDEPP <> 0
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'X'           EASTAT
     C                   ELSE
     C                   MOVE      'W'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     XXDEPP        EAAM01
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
     C                   MOVEL     SRDTO         EADT
     C                   MOVE      *BLANK        EAEDIT
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Endif                                                  If xxDEPP <> 0
     C*
     C* Write tax deposit record for estimating accounts.
     C*
     C                   If        xxTXDU <> 0
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'Z'           EASTAT
     C                   ELSE
     C                   MOVE      'Y'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     XXTXDU        EAAM01
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
     C                   MOVEL     SRDTO         EADT
     C                   MOVE      *BLANK        EAEDIT
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Endif                                                  If xxTXDU <> 0
     C*
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'D'           EASTAT
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'E'           EASTAT
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'F'           EASTAT
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'G'           EASTAT
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'H'           EASTAT
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
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
     C                   EXSR      SREAP
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
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   ENDIF                                                  SCTOT IFNE
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C                   END                                                    IF X = 0 OR
     C*                                                    F1UPAR = 'N'
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C                   END                                                    BSSEFL = 'E'
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
     C  N67EMKY1         READE     PEM02                                  60
     C   67              MOVE      *ON           *IN60
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
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      'N'           SCDEP
     C                   MOVE      *BLANKS       SCAR
      /free
       scARD = ' ';
      /end-free
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
      *
      *   PROMPT TO LUMP SUM ADJUSTMENTS OR NOT
      *
     C*** begin del ***  05/14/20  ************************************************
     C*****XXLSFL        IFEQ      'Y'
     C                   EXSR      SRCADJ
      *
     C*****SCPRAJ        IFNE      *ZERO
     C******             WRITE     BLANK
      *
     C******             MOVE      *ON           *IN90
     C******IN90         DOWEQ     *ON
     C******             EXFMT     WDLUMP
      *
     C*****SCLUMP        IFNE      'Y'
     C*****SCLUMP        ANDNE     *BLANK
     C******             MOVE      *ON           *IN88
     C******             ELSE
     C******             MOVE      *OFF          *IN90
     C******             ENDIF
      *
     C******             ENDDO
      *
     C******             ENDIF
      *
     C*****SCLUMP        IFEQ      'Y'
     C******             EXSR      SRLUMP
     C******             ENDIF
      *
     C******             ENDIF
     C*** end del *****  05/14/20  ************************************************
     C*
     C*** begin add ***  05/14/20  ************************************************
     C* Redirect to new window subroutine.
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*** end add *****  05/14/20  ************************************************
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
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *INKC IFEQ
     C*
     C*** beg chg ***** 01/23/20 ********************
     C***  *INKC         CABEQ     *ON           TERM
      /Free
       If *Inkc = *On;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
     C*
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      'N'           SCDEP
     C                   MOVE      *BLANKS       SCAR
      /free
       scARD = ' ';
      /end-free
     C                   Z-ADD     0             SCEFDT
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   Z-ADD     12            S                 2 0
     C     S             DOWGT     0
     C     A1(S)         IFNE      *ZERO
     C     A2(S)         ORNE      *ZERO
     C     A3(S)         ORNE      *ZERO
     C                   MOVE      'Y'           XX3A
     C                   ENDIF
     C     A4(S)         IFNE      *ZERO
     C     A5(S)         ORNE      *ZERO
     C     A6(S)         ORNE      *ZERO
     C                   MOVE      'Y'           XX3B
     C                   ENDIF
     C     A7(S)         IFNE      *ZERO
     C                   MOVE      'Y'           XX3C
     C                   ENDIF
     C                   SUB       1             S
     C                   ENDDO
     C                   ENDIF                                                  XXDIVC IFGT
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
     C                   PARM      'R'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN4        ##RRN2            9 0
     C*
     C*** beg chg ***** 01/23/20 ********************
     C***  *INKC         CABEQ     *ON           TERM
      /Free
       If *Inkc = *On;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
     C*
     C*  WRITE TRANSACTION FILE RECORD IF NOT EXISTING
     C*
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  CALL PROGRAM TO COMPUTE NEW BILLING TOTALS
     C*
     C                   EXSR      BILL
     C                   MOVE      'Y'           XXPYCG
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
     C*** beg chg ***** 01/23/20 ********************
     C***  *INKC         CABEQ     *ON           TERM
      /Free
       If *Inkc = *On;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
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
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*
     C*  F10=TABLE FILE SEARCH - COMMENT CODE 1
     C*
     C*  F10=TABLE FILE SEARCH - COMMENT CODE 2
     C*
     C*  COMMAND 10 = TABLE FILE SEARCH - COMMENT CODE 3
     C*
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
     C                   SETON                                        52
     C                   ENDIF                                                  SCKEY
     C*
     C                   ENDSL
     C*
     C                   GOTO      ENDF10
     C                   ENDIF                                                  HFLD
     C*
     C*  COMMAND 10 = TABLE FILE SEARCH - A/R CODE
     C*
     C     HFLD          IFEQ      'SCAR'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    SCKEY
      *
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCAR
     C                   SETON                                        37
      *
     C                   ELSE
      *
     C                   SETON                                        44
     C                   ENDIF                                                  SCKEY
      *
     C                   GOTO      ENDF10
     C                   ENDIF                                                  SCAR
     C*
     C     HFLD          IFEQ      'SCARD'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    SCKEY
     C*
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCARD
     C                   SETON                                        51
      *
     C                   ELSE
      *
     C                   SETON                                        37
     C                   ENDIF                                                  SCKEY
      *
     C                   GOTO      ENDF10
     C                   ENDIF                                                  SCARD
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     SCKEY         PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
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
     C*
     C                   GOTO      ENDF10
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C     ENDF10        TAG
     C                   MOVE      *BLANK        Q2RECF
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
     C     EX3RDB        ANDNE     'Y'
     C                   MOVE      EXREV         SCRMCD
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     SCRMCD        IFEQ      *BLANKS
     C                   MOVE      'N'           SCDART
     C                   ENDIF                                                   CRMCD
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
     C*** begin del ***  05/14/20  ****************************************
     C*****Q2RECF        CABEQ     'SCR03'       SCR03T
     C*****Q2RECF        CABEQ     'SCR03A'      SCR3AT
     C*****Q2RECF        CABEQ     'SCR03B'      SCR3BT
     C*****Q2RECF        CABEQ     'SCR03C'      SCR3CT
     C*****Q2RECF        CABEQ     'SCR04'       SCR04T
     C*****Q2RECF        CABEQ     'SCR04A'      SCR4AT
     C*****Q2RECF        CABEQ     'SCR05'       SCR05T
     C*** end del *****  05/14/20  ****************************************
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
     C                   Z-ADD     ELTXDU        X1TXDU
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
     C                   Z-ADD     ELSTPR        SVSTPR           11 2
     C                   Z-ADD     ELMAPR        SVMAPR           11 2
     C                   Z-ADD     ELENMP        SVNMPR           11 2
     C     ELANPR        SUB       ELPRTD        S2ANPR           11 2
     C                   MOVE      '1'           *IN59
     C     ELKY          CHAIN     PEL02                              60
     C     *IN60         IFEQ      '0'
     C     ELANPR        IFNE      XXANPR
     C     ELENMP        ORNE      XXENMP
     C     ELMAPR        ORNE      XXMAPR
     C     ELSTPR        ORNE      XXSTPR
     C     ELBSCD        ORNE      XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ORNE      XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     ELTXDU        ORNE      X1TXDU
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
     C*
     C*  IF NO OVERRIDE EXISTS AND DISCOUNT IS SPREAD, CALL SRDIS2
     C*  TO LOAD OVERRIDE FILES WAELP, WAEPP, WADSP FOR ALL
     C*  DIVISIONS IF DISCOUNT IS SPREAD
     C*
     C*
     C*
     C*
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
     C*** begin add ***  06/14/23  ***************************************
     C*
     C* If a policy has % of manual premium then clear the large deductible
     C* percentage to allow policy to recalculate all premium normally again.
     C* It will be reset later in calculations.
     C*

       If not %open(WALDL2);
          Open WALDL2;
       Endif;

       If *in67 = *on;
          Setll emky pem02;
          Reade(n) emky pem02;
       Else;
          Setll emky1 pem02;
          Reade(n) emky1 pem02;
       Endif;

       Dow not %eof;
          xxDiv = EMDiv;
          Chain(n) elky1 pel02;
          If %found and ELManP <> 0;
             xxPercMan = 'Y';
             Chain eaky6 pld44;
             If %found;
                LDPct1 = 0;
                Update pld44;
             Endif;
          Endif;

          If *in67 = *on;
             Reade(n) emky pem02;
          Else;
             Reade(n) emky1 pem02;
          Endif;

       Enddo;

       Close WALDL2;
     C*** end add *****  06/14/23  ***************************************
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
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ORNE      XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     ELTXDU        ORNE      X1TXDU
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
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
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

     C*  OVERRIDE DATABASE FILE WDELL1 TO WAELL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(15)
     C                   PARM                    LENGTH           15 5
     C*
     C                   END
     C*
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
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(16)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     55            LENGTH
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
     C
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
     C*** begin add ***  05/14/20  ************************************************
     C                   Clear                   visDisc
     C*** end add *****  05/14/20  ************************************************
     C     XXDIVC        IFGT      1
     C     XX1ST         ANDEQ     *BLANK
     C     *IN67         ANDEQ     *OFF
     C*** begin chg ***  05/14/20  ************************************************
     C******             MOVE      *BLANK        Q2RECF
     C******             MOVEL     'SCR05'       Q2RECF
     C******             ELSE
     C                   Move      *on           visDisc
     C                   Endif
     C*** end chg *****  05/14/20  ************************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*** begin del ***  05/14/20  ************************************************
     C******             ENDIF
     C*** end del *****  05/14/20  ************************************************
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
     C*** beg chg ***** 01/23/20 ********************
     C***  Q1PRMS        CABGT     1             TERM
      /Free
       If Q1prms > 1;
          Exsr SrTerm;
       Endif;
      /End-Free
     C*** end chg ***** 01/23/20 ********************
     C*
     C                   GOTO      STRT
     C*
     C*  BILLING REVISION SCREEN
     C*
     C     SCR03T        TAG
     C*** begin del ***  05/14/20  ********************************************
     C*
     C*  EDIT ENTERED DATES
     C*  DATE AND AMOUNT FIELD ARE MUTUALLY INCLUSIVE
     C*
     C******             Z-ADD     DSDATE        SRDTI
     C******             CALL      'SRCVTDAT'    DTCYMD
     C******             MOVEL     SRDTO         XXCURD
     C******             Z-ADD     XXCURD        XXCURR            7 0
     C*
     C*****FNKY          CHAIN     PEX01                              60
     C******
     C******IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C******             CALL      'SRFUTR8'
     C******             PARM      DSDATE        QQDT              8 0
     C******             PARM      EXRVID        QQDAYS            5 0
     C******             PARM                    QQERR             1
     C******             PARM                    QQTERM            1
     C*
     C*****QQERR         IFNE      '1'
     C******             MOVE      QQDT          SRDTI
     C******             CALL      'SRCVTDAT'    DTCYMD
     C******             MOVEL     SRDTO         XXCURD
     C******             ENDIF
     C*****XXACDT        IFNE      *ZERO
     C******             MOVE      XXACDT        SRDTI
     C******             CALL      'SRCVTDAT'    DTCYMD
     C******             MOVEL     SRDTO         XXCURD
     C******             ENDIF                                                  XXACDT IFNE
     C******
     C******             ENDIF                                                  *IN60 IFEQ
     C*
     C*
     C*****1             DO        12            Y                 3 0
     C*****DA(Y)         IFNE      *ZERO
     C*
     C******             Z-ADD     DA(Y)         SRDTI
     C******             CALL      'SRCVTDAT'    DATCHK
     C******             MOVEL     SRDTO         DA(Y)
     C*
     C*****SRERR         IFEQ      '1'
     C*****Y             ADD       19            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*
     C*****DA(Y)         IFNE      0
     C*****PA(Y)         ANDEQ     0
     C*****P1(Y)         ANDEQ     0
     C*****A1(Y)         ANDEQ     0
     C*****A2(Y)         ANDEQ     0
     C*****A3(Y)         ANDEQ     0
     C*****A4(Y)         ANDEQ     0
     C*****A5(Y)         ANDEQ     0
     C*****A6(Y)         ANDEQ     0
     C*****A7(Y)         ANDEQ     0
     C*****DA(Y)         OREQ      0
     C*****PA(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****P1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A2(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A3(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A4(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A5(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A6(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A7(Y)         ANDNE     0
     C*****Y             ADD       37            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*
     C*  ADJUSTMENT POSTING DATES MUST BE GREATER THAN OR EQUAL TO THE
     C*  CURRENT SYSTEM DATE (I.E. NO "BACK-POSTING" OF ADJ ALLOWED)
     C*
     C*****DA(Y)         IFNE      *ZEROS
     C*****
     C*****              Z-ADD     DA(Y)         SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         XXWK60
     C*
     C*****XXWK60        IFLT      XXCURR
     C*****Y             ADD       67            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*****
     C*****              END
     C*
     C*  TOTAL MUST MATCH CALCULATED ADJUSTMENT
     C*
     C*****              XFOOT     PA            XXTEST           11 2
     C*****              XFOOT     P1            X1TEST           11 2
     C*****              ADD       X1TEST        XXTEST
     C*
     C*****XXTEST        IFNE      SCTOT
     C******IN62         ANDEQ     *OFF
     C*****              MOVE      '1'           *IN32
     C*****              GOTO      STRT
     C*****              END
     C*****
     C*****XX3A          IFEQ      'Y'
     C*****              MOVEL(P)  'SCR03A'      Q2RECF
     C*****              GOTO      STRT
     C*****              ELSE
     C*****XX3B          IFEQ      'Y'
     C*****              MOVEL(P)  'SCR03B'      Q2RECF
     C*****              GOTO      STRT
     C*****              ELSE
     C*****XX3C          IFEQ      'Y'
     C*****              MOVEL(P)  'SCR03C'      Q2RECF
     C*****              GOTO      STRT
     C*****              ELSE
     C*****              GOTO      PROMPT
     C*****              ENDIF
     C*****              ENDIF
     C*****              ENDIF
     C*
     C*****SCR3AT        TAG
     C*****
     C*****              XFOOT     A1            XXTEST
     C*****              XFOOT     A2            X1TEST
     C*****              ADD       X1TEST        XXTEST
     C*****              XFOOT     A3            X1TEST
     C*****              ADD       X1TEST        XXTEST
     C*
     C*****DA(13)        IFNE      *ZERO
     C*****
     C*****              Z-ADD     DA(13)        SRDTI
     C*****              CALL      'SRCVTDAT'    DATCHK
     C*****              MOVEL     SRDTO         DA(13)
     C*
     C*****SRERR         IFEQ      '1'
     C*****              MOVE      '1'           *IN(33)
     C*****              GOTO      STRT
     C*****              ENDIF
     C*****
     C*****              ENDIF
     C*
     C*****DA(13)        IFNE      0
     C*****A1(13)        ANDEQ     0
     C*****A2(13)        ANDEQ     0
     C*****A3(13)        ANDEQ     0
     C*****A4(13)        ANDEQ     0
     C*****A5(13)        ANDEQ     0
     C*****DA(13)        OREQ      0
     C*****A1(13)        ANDNE     0
     C*****DA(13)        OREQ      0
     C*****A2(13)        ANDNE     0
     C*****DA(13)        OREQ      0
     C*****A3(13)        ANDNE     0
     C*****DA(13)        OREQ      0
     C*****A4(13)        ANDNE     0
     C*****DA(13)        OREQ      0
     C*****A5(13)        ANDNE     0
     C*****DA(13)        OREQ      0
     C*****A6(13)        ANDNE     0
     C*****DA(13)        OREQ      0
     C*****A7(13)        ANDNE     0
     C*****              MOVE      '1'           *IN(53)
     C*****              GOTO      STRT
     C*****              END
     C*
     C*  ADJUSTMENT POSTING DATES MUST BE GREATER THAN OR EQUAL TO THE
     C*  CURRENT SYSTEM DATE (I.E. NO "BACK-POSTING" OF ADJ ALLOWED)
     C*
     C*****DA(13)        IFNE      *ZEROS
     C*****
     C*****              Z-ADD     DA(13)        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         XXWK60
     C*
     C*****XXWK60        IFLT      XXCURR
     C*****              MOVE      '1'           *IN(82)
     C*****              GOTO      STRT
     C*****              ENDIF
     C*****
     C*****              ENDIF
     C*
     C*****XXTEST        IFNE      SCTOT2
     C******IN62         ANDEQ     *OFF
     C*****              MOVE      *ON           *IN32
     C*****              GOTO      STRT
     C*****              ENDIF
     C*
     C*
     C*****1             DO        12            Y                 3 0
     C*****DA(Y)         IFNE      *ZERO
     C*
     C*****              Z-ADD     DA(Y)         SRDTI
     C*****              CALL      'SRCVTDAT'    DATCHK
     C*****              MOVEL     SRDTO         DA(Y)
     C*
     C*****SRERR         IFEQ      '1'
     C*****Y             ADD       19            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*
     C*****DA(Y)         IFNE      0
     C*****PA(Y)         ANDEQ     0
     C*****P1(Y)         ANDEQ     0
     C*****A1(Y)         ANDEQ     0
     C*****A2(Y)         ANDEQ     0
     C*****A3(Y)         ANDEQ     0
     C*****A4(Y)         ANDEQ     0
     C*****A5(Y)         ANDEQ     0
     C*****A6(Y)         ANDEQ     0
     C*****A7(Y)         ANDEQ     0
     C*****DA(Y)         OREQ      0
     C*****PA(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****P1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A2(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A3(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A4(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A5(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A6(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A7(Y)         ANDNE     0
     C*****Y             ADD       37            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*
     C*****XX3B          IFEQ      'Y'
     C*****              MOVEL(P)  'SCR03B'      Q2RECF
     C*****              GOTO      STRT
     C*****              ELSE
     C*****              GOTO      PROMPT
     C*****              ENDIF
     C*
     C*****SCR3BT        TAG
     C*****
     C*****              XFOOT     A4            XXTEST
     C*****              XFOOT     A5            X1TEST
     C*****              ADD       X1TEST        XXTEST
     C*****              XFOOT     A6            X1TEST
     C*****              ADD       X1TEST        XXTEST
     C*
     C*****XXTEST        IFNE      SCTOT3
     C******IN62         ANDEQ     *OFF
     C*****              MOVE      *ON           *IN32
     C*****              GOTO      STRT
     C*****              ENDIF
     C*****
     C*****1             DO        12            Y                 3 0
     C*****DA(Y)         IFNE      *ZERO
     C*
     C*****              Z-ADD     DA(Y)         SRDTI
     C*****              CALL      'SRCVTDAT'    DATCHK
     C*****              MOVEL     SRDTO         DA(Y)
     C*
     C*****SRERR         IFEQ      '1'
     C*****Y             ADD       19            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*
     C*****DA(Y)         IFNE      0
     C*****PA(Y)         ANDEQ     0
     C*****P1(Y)         ANDEQ     0
     C*****A1(Y)         ANDEQ     0
     C*****A2(Y)         ANDEQ     0
     C*****A3(Y)         ANDEQ     0
     C*****A4(Y)         ANDEQ     0
     C*****A5(Y)         ANDEQ     0
     C*****A6(Y)         ANDEQ     0
     C*****A7(Y)         ANDEQ     0
     C*****DA(Y)         OREQ      0
     C*****PA(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****P1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A2(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A3(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A4(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A5(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A6(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A7(Y)         ANDNE     0
     C*****Y             ADD       37            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*
     C*****XX3C          IFEQ      'Y'
     C*****              MOVEL(P)  'SCR03C'      Q2RECF
     C*****              GOTO      STRT
     C*****              ELSE
     C*****              GOTO      PROMPT
     C*****              ENDIF
     C*
     C*****SCR3CT        TAG
     C*****
     C*****              XFOOT     A7            XXTEST
     C*
     C*****XXTEST        IFNE      SCTOT4
     C******IN62         ANDEQ     *OFF
     C*****              MOVE      *ON           *IN32
     C*****              GOTO      STRT
     C*****              ENDIF
     C*
     C*****1             DO        12            Y                 3 0
     C*****DA(Y)         IFNE      *ZERO
     C*
     C*****              Z-ADD     DA(Y)         SRDTI
     C*****              CALL      'SRCVTDAT'    DATCHK
     C*****              MOVEL     SRDTO         DA(Y)
     C*
     C*****SRERR         IFEQ      '1'
     C*****Y             ADD       19            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*
     C*****DA(Y)         IFNE      0
     C*****PA(Y)         ANDEQ     0
     C*****P1(Y)         ANDEQ     0
     C*****A1(Y)         ANDEQ     0
     C*****A2(Y)         ANDEQ     0
     C*****A3(Y)         ANDEQ     0
     C*****A4(Y)         ANDEQ     0
     C*****A5(Y)         ANDEQ     0
     C*****A6(Y)         ANDEQ     0
     C*****A7(Y)         ANDEQ     0
     C*****DA(Y)         OREQ      0
     C*****PA(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****P1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A1(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A2(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A3(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A4(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A5(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A6(Y)         ANDNE     0
     C*****DA(Y)         OREQ      0
     C*****A7(Y)         ANDNE     0
     C*****Y             ADD       37            X
     C*****              MOVE      '1'           *IN(X)
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*****
     C*****PROMPT        TAG
     C*
     C*  WRITE BILLING PROMPT SCREEN
     C*
     C*****              MOVE      *BLANKS       SCCOM1
     C*****              MOVE      *BLANKS       SCCOM2
     C*****              MOVE      *BLANKS       SCCOM3
     C*****              MOVE      'N'           SCDEP
     C*****              MOVE      *BLANKS       SCAR
      /free
      //***** scARD = ' ';
      /end-free
     C*****
     C*****              Z-ADD     0             SCEFDT
     C*
     C*  CHECK IF OFFICE IS INSTALLED.
     C*
     C*  DEFAULT RESOLVE TO DARTS FLAG TO 'Y' INITIALLY
     C*  RESOLVE TO DARTS FLAG SET TO 'N' IF REMARK CODE FIELD = *BLANK
     C*****              MOVEL(P)  'SCR04'       Q2RECF
     C*****              GOTO      STRT
     C*
     C*  ADJUSTMENT SCREEN
     C*
     C*****SCR04T        TAG
     C*****
      /Free
      //*If LiveCheck();
      //***
      //*** PopupMsg = 'User ' + %trim(saveUser)
      //***            + ' has updated this policy subsequent to when this '
      //***            + 'revised bill started. '
      //***            + 'Press OK to discard currect changes and '
      //***            + 'restart based on current data. ';
      //***
      //*** PopOneBtn(PopupMsg:'Information');
      //***
      //*** clearTemp();
      //***
      //*** q2Recf = 'SCR01';

      /End-Free
     C*****              goto      SCR01T
      /Free
      //***   Endif;
      /End-Free
     C*****XXOPN2        IFEQ      '0'
     C*****              OPEN      WDELL
     C*****              MOVE      '1'           XXOPN2
     C*****              END
     C*****
     C*****ELKY          CHAIN     PEL01                              60
     C*
     C*****XXOPN2        IFEQ      '1'
     C*****              CLOSE     WDELL
     C*****              MOVE      '0'           XXOPN2
     C*****              END
     C*****ELKY          CHAIN     PEL02                              60
     C*
     C*
     C* EDIT EFFECTIVE DATE IF THERE ARE NO A/R ADJUSTMENTS TO ENSURE
     C* PROPER DATE IS PASSED ALONG.
     C*
     C******IN80         IFEQ      '1'
     C*
     C*   THE ENDORSEMENT EFFECTIVE DATE MUST BE VALID
     C*
     C*****SCEFDT        IFEQ      *ZERO                                                         4
     C*****              MOVE      *ON           *IN71
     C*****              GOTO      STRT
     C*****
     C*****              ELSE
     C*
     C*****              Z-ADD     SCEFDT        SRDTI
     C*****              CALL      'SRCVTDAT'    DATCHK
     C*****              MOVEL     SRDTO         SCEFDT
     C*
     C*****SRERR         IFEQ      '1'
     C*****              MOVE      *ON           *IN71
     C*****              GOTO      STRT
     C*****              ENDIF
     C*
     C*  ENDORSEMENT EFF DATE MUST BE WITHIN THE POLICY PERIOD
     C*
     C*****              Z-ADD     SCEFDT        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         XXEFD2            7 0
     C*
     C*****XXEFD2        IFLT      ELPFDT
     C*****XXEFD2        ORGE      ELPTDT
     C*****              MOVE      *ON           *IN34
     C*****              GOTO      STRT
     C*****              ENDIF
     C*****
     C*****              ENDIF                                                  SCEFDT IFEQ
     C*****
     C*****              ENDIF                                                  *IN80 IFEQ
     C*
     C*  EDIT COMMENT CODES IF ENTERED
     C*
     C*  CLEAR XXKEY FOR CHAIN OPERATION
     C*
     C*****              MOVE      *BLANKS       XXKEY
     C*****
     C*****SCCOM1        IFNE      *BLANKS
     C*****              MOVEL     SCCOM1        XXKEY
     C*****TBKY          CHAIN     PBC01                              20
     C**N20BCSTAT        COMP      'I'                                    20
     C******IN20         CABEQ     '1'           STRT
     C*****              END
     C*
     C*****SCCOM2        IFNE      *BLANKS
     C*****              MOVEL     SCCOM2        XXKEY
     C*****TBKY          CHAIN     PBC01                              21
     C**N21BCSTAT        COMP      'I'                                    21
     C******IN21         CABEQ     '1'           STRT
     C*****              END
     C*
     C*****SCCOM3        IFNE      *BLANKS
     C*****              MOVEL     SCCOM3        XXKEY
     C*****TBKY          CHAIN     PBC01                              22
     C**N22BCSTAT        COMP      'I'                                    22
     C******IN22         CABEQ     '1'           STRT
     C*****              END
     C*****SCCOM1        IFNE      *BLANKS
     C*****SCCOM2        ORNE      *BLANKS
     C*****SCCOM3        ORNE      *BLANKS
     C*
     C*****SCFCM1        IFNE      *BLANKS
     C*****SCFCM2        ORNE      *BLANKS
     C*****SCFCM3        ORNE      *BLANKS
     C*****              MOVE      *ON           *IN46
     C*****              GOTO      STRT
     C*****              ENDIF                                                  SCFCM1 IFNE
     C*****
     C*****              ENDIF                                                  SCCOM1 IFNE
     C*
     C*  VALIDATE RECALCULATE DEPOSIT FLAG
     C*
     C*****              IF        SCDEP = ' '
     C*****              EVAL      SCDEP = 'N'
     C*****              ENDIF
     C*****SCDEP         IFNE      'Y'
     C*****SCDEP         ANDNE     'N'
     C*****              MOVE      '1'           *IN23
     C*****              GOTO      STRT
     C*****              END
     C*
     C*  VALIDATE THE RESOLVE TO DARTS FLAG.
     C*
     C*****SCDART        IFNE      'Y'
     C*****SCDART        ANDNE     'N'
     C*****              MOVE      *ON           *IN45
     C*****              GOTO      STRT
     C*****              ENDIF
     C*
     C*****SCDART        IFEQ      'Y'
     C*****SCRMCD        CHAIN     PCT01                              84
     C*
     C******IN84         IFEQ      *ON
     C*****              GOTO      STRT
     C*****              ENDIF
     C*
     C*****CTSTAT        IFEQ      'I'
     C*****              MOVE      *ON           *IN85
     C*****              GOTO      STRT
     C*****              ENDIF                                                  CTSTAT IFEQ
     C*****
     C*****              ELSE
     C*****
     C*****              MOVE      *BLANKS       SCRMCD
     C*****
     C*****              ENDIF                                                  SCDART IFEQ
     C*
     C*  *IN80 WILL BE SET IF EITHER THE FUND DETAIL FLAG IS 'N' OR IF
     C*  NO ADJUSTMENTS ARE BEING POSTED.  IN BOTH OF THESE CASES THERE
     C*  IS NO NEED TO VALIDATE THE ADJUSTMENT A/R CODE ON THIS SCREEN
     C*  NOR DO WE NEED TO WRITE A RECORD TO THE A/R ADJUSTMENT FILE
     C*  (WAEAP) SINCE NO ADJUSTMENTS WILL BE WRITTEN.
     C*
     C******IN80         IFEQ      '0'
     C*
     C*  GET BILLING SCHEDULE FOR STYLE
     C*
     C*****              MOVE      *BLANKS       XXKEY
     C*****              MOVEL     ELBSCD        XXKEY
     C*****TBKY          CHAIN     PBS01                              60
     C***60              MOVE      *BLANKS       BSSEFL
     C*
     C*  VALIDATE THE PAYMENT DUE DATE FOR SELF REPORTING TYPES
     C*
     C*****BSSEFL        IFEQ      'S'
     C*****SCDEP         ANDEQ     'Y'
     C*****SCDATE        COMP      0                                      24
     C***24              GOTO      STRT
     C*
     C*****              Z-ADD     SCDATE        SRDTI
     C*****              CALL      'SRCVTDAT'    DATCHK
     C*****              MOVEL     SRDTO         SCDATE
     C*
     C*****SRERR         IFEQ      '1'
     C*****              MOVE      '1'           *IN24
     C*****              GOTO      STRT
     C*****              END
     C*****              END
     C*
     C*   THE ENDORSEMENT EFFECTIVE DATE MUST BE VALID
     C*
     C*****SCEFDT        IFEQ      *ZERO                                                         4
     C*****              MOVE      *ON           *IN71
     C*****              GOTO      STRT
     C*****
     C*****              ELSE
     C*
     C*****              Z-ADD     SCEFDT        SRDTI
     C*****              CALL      'SRCVTDAT'    DATCHK
     C*****              MOVEL     SRDTO         SCEFDT
     C*
     C*****SRERR         IFEQ      '1'
     C*****              MOVE      *ON           *IN71
     C*****              GOTO      STRT
     C*****              ENDIF
     C*
     C*
     C*  ENDORSEMENT EFF DATE MUST BE WITHIN THE POLICY PERIOD
     C*
     C*****              Z-ADD     SCEFDT        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         XXEFD2            7 0
     C*
     C*****XXEFD2        IFLT      ELPFDT
     C*****XXEFD2        ORGE      ELPTDT
     C*****              MOVE      *ON           *IN34
     C*****              GOTO      STRT
     C*****              ENDIF
     C*****              ENDIF
     C*
     C*  VALIDATE A/R TRANSACTION CODE - REQUIRED
     C*  MUST BE ADJUSTMENT TYPE FOR ESTIMATING TYPES OF EMPLOYERS
     C*
     C**** BSSEFL        IFEQ      'E'
     C****               MOVE      *BLANKS       XXKEY
     C****               MOVEL     SCAR          XXKEY
     C**** TBKY          CHAIN     PTT01                              28
     C****  N28TTSTAT        COMP      'I'                                    28
     C**** *IN28         CABEQ     '1'           STRT
     C**** TTTYPE        COMP      'A'                                3030
     C**** *IN30         CABEQ     '1'           STRT
     C****               ELSE
     C****
     C****               MOVE      *BLANKS       XXKEY
     C****               MOVEL     SCAR          XXKEY
     C**** TBKY          CHAIN     PTT01                              28
     C****  N28TTSTAT        COMP      'I'                                    28
     C**** *IN28         CABEQ     '1'           STRT
     C**** TTTYPE        COMP      'H'                                3030
     C**** *IN30         CABEQ     '1'           STRT
     C****               END
     C****               ENDIF                                                  *IN80 IFEQ

     C*****              MOVE      *BLANKS       xxESTY            1
     C*****              MOVE      *BLANKS       xxESDS           30
     c*
     C*****              MOVE      *BLANKS       xxSRTY            1
     C*****              MOVE      *BLANKS       xxSRDS           30
     c*****
     C*****              If        noEst = *off
     c*****              Move      *Blanks       xxKEY
     c*****              MoveL     scAR          xxKEY
     c*****TBKY          Chain     PTT01                              30
     c**N30TTSTAT        Comp      'I'                                    30
     c******IN30         CabEq     '1'           STRT
     c*****TTTYPE        Comp      'A'                                3030
     c******IN30         CabEq     '1'           STRT
     C*****              MOVEL     TTTYPE        xxESTY
     C*****              MOVEL     TTDESC        xxESDS
     C*****              Endif
     C*****
     C*****              If        noSR = *off
     c*
     c*****              MOVE      *Blanks       xxKEY
     c*****              MOVEL     scARD         xxKEY
     c*****TBKY          CHAIN     PTT01                              36
     c**N28TTSTAT        COMP      'I'                                    36
     c******IN36         CABEQ     '1'           STRT
     c*****TTTYPE        COMP      'H'                                3535
     c******IN35         CABEQ     '1'           STRT
     C*****              MOVEL     TTTYPE        xxSRTY
     C*****              MOVEL     TTDESC        xxSRDS
     C*****              Endif
     C*****              ENDIF                                                  *IN80 IFEQ
     C*
     C*  DETERMINE IF CALLED FROM EMPLOYER MID TERM ENDORSEMENTS
     C*    IF SO, WE NEED TO DISPLAY SCR04A
     C*
     C******LOCK         IN        *DTAARA
     C******             OUT       *DTAARA
     C*
     C******             MOVE      *BLANKS       XXCHG
     C*****EPKY3         SETLL     PEP02
     C*****EPKY3         READE     PEP02                                  60
     C******IN60         DOWEQ     *OFF
     C******             Z-ADD     EPEFDT        XXEFD1
     C******             MOVE      EPCLAS        X@CLAS            5 0
     C*****EPKY4         CHAIN     PEP03                              60
     C******IN60         IFEQ      *ON
     C******             MOVE      'Y'           XXCHG             1
     C******             ENDIF
     C*****EPKY3         READE     PEP02                                  60
     C******             ENDDO
     C*****LDENDA        IFNE      *ZEROS
     C*****LDENDA        ANDNE     *BLANKS
     C**IF*NON-ZERO ADJUSTMENT EXIST (*IN80 = *OFF)
     C**OR*NEW PAYROLL IS ADDED
     C******             MOVEL(P)  'SCR04A'      Q2RECF
     C******             GOTO      STRT
     C******             ENDIF
     C*** end del *****  05/14/20  ************************************
     C*
     C*  SUBMIT PROGRAM TO PRINT BILL
     C*
     C     PRNT          TAG
     C     *IN80         IFEQ      *OFF
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
     C                   MOVEA     J1D           J2D
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
     C                   If        BSSEFL = 'E'
     C                   MOVEL     SCAR          EAARCD
     C                   MOVEL     xxESTY        EAARTY
     C                   MOVEL     xxESDS        EADESC
     C                   Else
     C                   MOVEL     SCARD         EAARCD
     C                   MOVEL     xxSRTY        EAARTY
     C                   MOVEL     xxSRDS        EADESC
     C                   Endif
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
     C                   EXSR      SREAP
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
     C                   MOVEL     xxESTY        EAARTY
     C                   MOVEL     xxESDS        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C     EASTAT        IFEQ      'W'
     C     EASTAT        OREQ      'X'
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     SCARD         EAARCD
     C                   MOVEL     xxSRTY        EAARTY
     C                   MOVEL     xxSRDS        EADESC
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
     C                   MOVE      *BLANKS       xxESTC            2
     C                   MOVE      *BLANKS       xxESTT            1
     C                   MOVE      *BLANKS       XXESTD           30
     C                   MOVE      *BLANKS       xxSRTC            2
     C                   MOVE      *BLANKS       xxSRTT            1
     C                   MOVE      *BLANKS       XXSRTD           30
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
     C* Premium Tax
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      'T'
     C     TTTYPE        ANDEQ     'F'
     C*
     C                   MOVEL     TTKEY         xxESTC
     C                   MOVE      TTTYPE        xxESTT
     C                   MOVE      TTDESC        xxESTD
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* Deposit Tax
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      'D'
     C     TTTYPE        ANDEQ     'F'
     C*
     C                   MOVEL     TTKEY         xxSRTC
     C                   MOVE      TTTYPE        xxSRTT
     C                   MOVE      TTDESC        xxSRTD
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     D1(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     D1(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     D1(3)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     D1(4)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     D1(5)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     D1(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     D1(7)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     D1(8)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     D1(9)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     D1(10)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     D1(11)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     D1(12)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     C     xxESTC        IFEQ      *BLANKS
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     xxESTC        EAARCD
     C                   MOVE      xxESTT        EAARTY
     C                   MOVE      xxESTD        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C     EASTAT        IFEQ      'Y'
     C     EASTAT        OREQ      'Z'
     C*
     C     xxSRTC        IFEQ      *BLANKS
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     xxSRTC        EAARCD
     C                   MOVE      xxSRTT        EAARTY
     C                   MOVE      xxSRTD        EADESC
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
     C*  GET THE 1ST ASSESSMENT A/R TYPE
     C*
     C                   Z-ADD     EAPO01        SVDT7
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
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '1'
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
     C*  WRITE A RECORD FOR THE 1ST ASSESSMENT ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     DA1(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA1(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA1(3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA1(4)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA1(5)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA1(6)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA1(7)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA1(8)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA1(9)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA1(10)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA1(11)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA1(12)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     D*** Begin Chg ***  07/12/24  ********************************
     C*****              Z-ADD     DA(13)        SRDTI
     C                   Z-ADD     DA1(13)       SRDTI
     D*** End   Chg ***  07/12/24  ********************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '1'
     C     EASTAT        OREQ      'D'
     C*
     C     TTKEY         IFEQ      *BLANKS
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
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
     C*  GET A/R CODE FOR THE 2ND ASSESSMENT ADJUSTMENT
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
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '2'
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
     C*  WRITE A RECORD FOR THE 2ND ASSESSMENT ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     DA2(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA2(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA2(3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA2(4)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA2(5)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA2(6)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA2(7)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA2(8)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA2(9)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA2(10)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA2(11)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA2(12)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     D*** Begin Chg ***  07/12/24  ********************************
     C*****              Z-ADD     DA(13)        SRDTI
     C                   Z-ADD     DA2(13)       SRDTI
     D*** End   Chg ***  07/12/24  ********************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '2'
     C     EASTAT        OREQ      'E'
     C*
     C     TTKEY         IFEQ      *BLANKS
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
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
     C*  GET A/R CODE FOR THE 3RD ASSESSMENT ADJUSTMENT
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
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '3'
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
     C*  WRITE A RECORD FOR THE 3RD ASSESSMENT ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     DA3(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA3(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA3(3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA3(4)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA3(5)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA3(6)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA3(7)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA3(8)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA3(9)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA3(10)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA3(11)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA3(12)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     D*** Begin Chg ***  07/12/24  ********************************
     C*****              Z-ADD     DA(13)        SRDTI
     C                   Z-ADD     DA3(13)       SRDTI
     D*** End   Chg ***  07/12/24  ********************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '3'
     C     EASTAT        OREQ      'F'
     C*
     C     TTKEY         IFEQ      *BLANKS
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
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
     C*  GET A/R CODE FOR THE 4TH ASSESSMENT ADJUSTMENT
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
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '4'
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
     C*  WRITE A RECORD FOR THE 4TH ASSESSMENT ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     DA4(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA4(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA4(3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA4(4)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA4(5)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA4(6)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA4(7)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA4(8)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA4(9)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA4(10)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA4(11)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA4(12)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     D*** Begin Chg ***  07/12/24  ********************************
     C*****              Z-ADD     DA(13)        SRDTI
     C                   Z-ADD     DA4(13)       SRDTI
     D*** End   Chg ***  07/12/24  ********************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '4'
     C     EASTAT        OREQ      'G'
     C*
     C     TTKEY         IFEQ      *BLANKS
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
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
     C*  GET A/R CODE FOR THE 5TH ASSESSMENT ADJUSTMENT
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
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '5'
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
     C*  WRITE A RECORD FOR THE 5TH ASSESSMENT ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     DA5(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA5(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA5(3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA5(4)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA5(5)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA5(6)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA5(7)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA5(8)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA5(9)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA5(10)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA5(11)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA5(12)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     D*** Begin Chg ***  07/12/24  ********************************
     C*****              Z-ADD     DA(13)        SRDTI
     C                   Z-ADD     DA5(13)       SRDTI
     D*** End   Chg ***  07/12/24  ********************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '5'
     C     EASTAT        OREQ      'H'
     C*
     C     TTKEY         IFEQ      *BLANKS
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
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
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     DA6(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA6(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA6(3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA6(4)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA6(5)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA6(6)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA6(7)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA6(8)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA6(9)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA6(10)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA6(11)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA6(12)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     D*** Begin Chg ***  07/12/24  ********************************
     C*****              Z-ADD     DA(13)        SRDTI
     C                   Z-ADD     DA6(13)       SRDTI
     D*** End   Chg ***  07/12/24  ********************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   EXSR      SREAP
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
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
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
     D*** Begin Add ***  07/12/24  ********************************
     C                   Z-ADD     DA7(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA7(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA7(3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA7(4)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA7(5)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA7(6)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA7(7)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA7(8)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA7(9)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA7(10)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA7(11)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA7(12)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   MOVEA     J1D           J2D
     D*** End   Add ***  07/12/24  ********************************
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
     C                   EXSR      SREAP
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
     D*** Begin Chg ***  07/12/24  ********************************
     C*****              Z-ADD     DA(13)        SRDTI
     C                   Z-ADD     DA7(13)       SRDTI
     D*** End   Chg ***  07/12/24  ********************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   EXSR      SREAP
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
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
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
     C* Write deposit record for estimating accounts.
     C*
     C                   If        noSR = *off  and xxDIVC = 1 and noEST = *off
     C                             and xxDEPP <> 0 and scARD <> '  '
     C*
     C                   Z-ADD     xxDEPP        EAAM01
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
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVEL     SCARD         EAARCD
     C                   MOVEL     xxSRTY        EAARTY
     C                   MOVEL     xxSRDS        EADESC
     C*
     C                   MOVE      *BLANK        EAEDIT
     C                   ADD       1             EATIME
     C                   MOVEL     SRDTO         EADT
     C*
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Endif                                                  If noSR = *off
     C*
     C* Write tax deposit record for estimating accounts.
     C*
     C                   If        noSR = *off  and xxDIVC = 1 and noEST = *off
     C                             and xxTXDU <> 0 and xxSRTC <> '  '
     C*
     C                   Z-ADD     xxTXDU        EAAM01
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
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVEL     xxSRTC        EAARCD
     C                   MOVEL     xxSRTT        EAARTY
     C                   MOVEL     xxSRTD        EADESC
     C*
     C                   MOVE      *BLANK        EAEDIT
     C                   ADD       1             EATIME
     C                   MOVEL     SRDTO         EADT
     C*
     C                   EXSR      SREAP
     C                   WRITE     PEA02
     C*
     C                   Endif                                                  If noSR = *off
     C*
     C                   END                                                    *IN80
     C*  CALL PROGRAM TO UPDATE A/R
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCDATE        QQDUE             8
     C                   MOVE      SCAR          QQAR              2
      /free
       QQAR2 = scARD;
      /end-free
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
     C*  Delete future A/R if account was changed from Est to S/R.
     C*  150WEU excludes this logic if EXExcArBl = 'Y'.
     C*
      *** beg chg ***** 09/02/20 *************************
     C***     BSSEFL        IFEQ      'S'
      /Free
       If isChangedToSR = *On and EXExcArBl <> 'Y';
      /End-Free
      *** end chg ***** 09/02/20 *************************
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
     C     TTCTYP        ANDNE     'C'
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *On
     c*** End   Add *** 06/17/21 *******************************************
     C                   EXSR      HOLDAR
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
     C     TTCTYP        ANDNE     'C'
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
     C     TTCTYP        ANDNE     'C'
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *On
     c*** End   Add *** 06/17/21 *******************************************
     C                   EXSR      HOLDAR
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
     C     TTCTYP        ANDNE     'C'
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
     C     TTCTYP        OREQ      '6'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '7'
     C     CLTID         ANDNE     '139'
     C     EAARCD        OREQ      IFIFBL
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *On
     c*** End   Add *** 06/17/21 *******************************************
     C                   EXSR      HOLDAR
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
     C     TTCTYP        OREQ      '6'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '7'
     C     CLTID         ANDNE     '139'
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
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C*
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        WKDATE
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     DA(1)         IFNE      *ZERO
     C                   MOVE      DA(1)         QQDATE
     C                   ELSE
     C                   MOVE      WKDATE        QQDATE            8
     C                   ENDIF                                                  DA,1 IFNE
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
     C                   MOVEL     SCFCM1        QQFCM1
     C                   MOVEL     SCFCM2        QQFCM2
     C                   MOVEL     SCFCM3        QQFCM3
     C     *IN67         IFEQ      *ON
     C                   MOVE      'Z'           QQFQT
     C                   ELSE
     C                   MOVE      *BLANK        QQFQT
     C                   ENDIF
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
     C                   PARM                    QQFQT             1
     C                   PARM                    SCRMCD
     C                   PARM                    SCAR
     C                   PARM                    QQEFDT
     C                   PARM                    QQFCM1           30
     C                   PARM                    QQFCM2           30
     C                   PARM                    QQFCM3           30
     C                   PARM                    SCARD
     C*
     C     Q1PRMS        IFGT      4
     C                   MOVE      *ON           @@RTCD
     C                   ENDIF
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C     @@RRN         CHAIN     WDELP                              60
     C*
     C                   MOVE      UPOUTQ        QQQUE            10
     C                   MOVE      'N'           SCREIS            1
     C                   Move      'P'           xxType
     C*
     C*   CHECK SUBMISSION HISTORY
     C*
      /free

       xxResubmit = *off;

       Chain(n) shky psh01;

       If %found and (SHStat = '4' or SHCode = 'I' or SHCode = 'T'
        or SHCode = 'N');

          Open WDELL1;
          Setll rrky3 WDELL1;
          Reade rrky3 WDELL1;

          Dow not %eof;

             xx1rtc = ELRtcd;
             Chain rsky1 prs01;
             xxStat = rsjrst;
             xx1efd = elpfdt;
             Setgt rrky4 prr01;
             Readpe rrky5 prr01;

             If %found and RRPPfl = 'Y';
                xxResubmit = *on;
                Leave;
             Endif;

             Reade rrky3 WDELL1;
          Enddo;

          Close WDELL1;

       Endif;

       If xxResubmit;

      /end-free
     C     SHKY          CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C     SHSTAT        IFEQ      '4'
     C     SHCODE        OREQ      'I'
     C     SHCODE        OREQ      'T'
     C     SHCODE        OREQ      'N'
     C                   MOVE      'Y'           SCREIS
     C                   ENDIF                                                  SHSTAT
     C*** begin chg ***  08/14/24  *********************************************
     C**** ELPFDT        IFNE      SHPFDT
     C**** SHPFDT        ANDNE     0
     C**** SHSTAT        ANDEQ     '4'
     C****               Z-ADD     SHPFDT        SHSBDT
     C****               Z-ADD     ELPFDT        SHPFDT
     C     shky2         Setll     pel0b
     C     shky2         Reade     pel0b
     C     BELPFDT       IFNE      SHPFDT
     C     SHPFDT        ANDNE     0
     C     SHSTAT        ANDEQ     '4'
     C                   Z-ADD     SHPFDT        SHSBDT
     C                   Z-ADD     BELPFDT       SHPFDT
     C*** end chg *****  08/14/24  *********************************************
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
     C                   Z-add     0             xx1efd
     C                   Z-add     0             xxtpfd
     C                   Move      xsDiv         QQDiv
     C                   MOVE      XX1EFD        QQFPFD            8
     C                   MOVE      XXTPFD        QQTPFD            8
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
     C
     C                   Endif                                                  If xxResubmit
     C*
     C*
     C*  DETERMINE IF CALLED FROM EMPLOYER MID TERM ENDORSEMENTS
     C*    IF SO, WE NEED TO DISPLAY SCR04A
     C*
     C*
     C*
     C*  IF DISCOUNT IS BEING SPREAD, DISPLAY SCR05, WHICH SHOWS A
     C*  MESSAGE THAT ALL DIVISIONS SHOULD BE SUBMITTED IF STANDARD
     C*  PREM WAS MODIFIED
     C*
     C*
     C*
     C*  RETURN TO SCREEN 1
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     Q1PRMS        IFGT      1
      *** beg chg ***** 01/23/20 ********************************
     C***                GOTO      TERM
      /Free
       Exsr SrTerm;
      /End-Free
      *** end chg ***** 01/23/20 ********************************
     C                   ENDIF                                                  Q1PRMS>1
     C*
     C                   GOTO      STRT
     C*
     C*** begin del ***  05/14/20  *****************************************************
     C*****SCR4AT        TAG
     C*
     C*  THE ENDORSEMENT DESCRIPTION CANNOT BE BLANK
     C*
     C*****SCDSC1        IFEQ      *BLANK
     C*****              MOVE      *ON           *IN20
     C*****              GOTO      STRT
     C*****              ENDIF
     C*
     C*  WRITE AN INTERIM ADJUSTMENT OF PREMIUM MID TERM ENDORSEMENT
     C*
     C*****              EXSR      WRTPEN
     C*
     C*  IF DISCOUNT IS BEING SPREAD, DISPLAY SCR05, WHICH SHOWS A
     C*  MESSAGE THAT ALL DIVISIONS SHOULD BE SUBMITTED IF STANDARD
     C*  PREM WAS MODIFIED
     C*
     C*****              GOTO      PRNT
     C*
     C*  RETURN TO SCREEN 1
     C*
     C*****              MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*****Q1PRMS        IFGT      1
      *** beg chg ***** 01/23/20 ********************************
     C***                GOTO      TERM
      /Free
      //****** Exsr SrTerm;
      /End-Free
      *** end chg ***** 01/23/20 ********************************
     C*****              ENDIF                                                  Q1PRMS>1
     C*
     C*****              GOTO      STRT
     C*
     C*****SCR05T        TAG
     C*
     C*  RETURN TO SCREEN 1
     C*
     C*
     C*****              MOVE      'Y'           XX1ST
     C*****              MOVE      *BLANKS       Q2RECF
     C*****              MOVEL     'SCR02'       Q2RECF
     C*
     C*****              GOTO      STRT
     C*** end del *****  05/14/20  *****************************************************
     C*
     C*  END THE PROGRAM
     C*
      *** beg chg ***** 01/23/20 **************************
     C***     TERM          TAG
     C****
     C**** Delete override for WDHCLR
     C***                   CALL      'QCMDEXC'                            99
     C***                   PARM                    QCA(43)
     C***                   PARM      55            LENGTH           15 5
     C****
     C***                   Select
     C***                   When      @@KC = *ON
     C***                   Eval      @@Next = 'EXIT'
     C***                   When      @@KL = *ON
     C***                   Eval      @@Next = 'CANCEL'
     C***                   Other
     C***                   Eval      @@Next = 'NEXT'
     C***                   If        mteMode = *Off
     C***                   Eval      xxRtn = PopOneBtn('Job submitted!':
     C***                             'Information')
     C***                   Endif
     C***                   ENDSL
      ***
     C***                   If        riuCreated
     C***                   EVAL      ##RRN4 = %Editc(@@RRN2:'X')
     C***                   CALL      'W0105R'
     C***                   PARM      'B'           @@MODE            1
     C***                   PARM                    ##RRN4            9
     C***                   PARM      'WMEMP'       @@FNM            10
     C***                   Endif
     C***                   MOVE      '1'           *INLR

      /Free
       Exsr srTerm;
       //=====================================================
       Begsr srTerm;
       //=====================================================
        Select;
        When @@KC = *On;
           @@Next = 'EXIT';
        When @@KL = *On;
           @@Next = 'CANCEL';
        Other;
           @@Next = 'NEXT';
           If mteMode = *Off;
            //***** Beg Chg *** 03/16/20 ***********************************
               xxRtn = PopOneBtnAgain('Job submitted!':'Information':
                                      'RevisedSubmit');
              //** xxRtn = PopOneBtn('Job submitted!':'Information');
            //***** End Chg *** 03/16/20 ***********************************
           Endif;
        Endsl;
        //*** Begin  Add *** 06/17/21 **************************************
        // Finalize json sent to Invoice Cloud
        If sendToIC;
           FinalizeThirdPartyInvoicing(xxCo#:xxFnd);
        EndIf;
        //*** End    Add *** 06/17/21 **************************************
        *Inlr = *On;
        Return;
       Endsr;
      /End-Free
      *** end chg ***** 01/23/20 **************************

      *
      *================================================================
      *  PROCESSING BEFORE WRITE WAEAP RECORD
      *================================================================
     C     SREAP         BEGSR
      *
      * RESTORE ORIGINAL POST DATES IN CASE PREVIOUS INSTALLMENT
      * BILLING CLEARED THEM
      *
     C                   MOVEA     J2D           J1D
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
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR
     C*
     C*** Begin  Add *** 06/17/21 ******************************************
       // Let invoice cloud know, invoice was removed.
       If EAninv <> 0 And isWMEAP;
          invoice  = %EditC(EACo#:'X')  + %EditC(EAfnd:'X') +
                     %EditC(EAemp#:'X') + %EditC(EAdiv:'X') +
                     %EditC(EAnInv:'X');
          index    = %LookUp(invoice:arrInvoice);
          If index = 0;
             index    = %LookUp(' ':arrInvoice);
             arrInvoice(index) = invoice;
             inJson   = '{"type":"billing",' +
                         '"invoice":"'  + %EditC(EAnInv:'X') + '",'  +
                         '"company":"'  + %EditC(EACo#:'X')  + '",'   +
                         '"group":"'    + %EditC(EAfnd:'X')  + '",'  +
                         '"employer":"' + %EditC(EAemp#:'X') + '",'    +
                         '"division":"' + %EditC(EADiv:'X')  + '",'    +
                         '"active":"false":"delete":"true"}';
             ThirdPartyInvoicing(inJson);
             sendToIC = *On;
          EndIf;
       EndIf;
     C*** End    Add *** 06/17/21 ******************************************
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
     C                   Z-ADD     DSTIME        ELTIME
     C  N61              WRITE     PEL02
     C                   END
     C*
     C*  EXECUTE OVERRIDE STATEMENTS
     C*
     C                   EXSR      BGNOVR
     C*
     C                   EXSR      LDHCN
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
     C                   MOVE      '1'           *IN87
     C*
     C*  OVERRIDE DATABASE FILE WDELP TO WAELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDELL1 TO WAELL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(15)
     C                   PARM                    LENGTH           15 5
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
     C*
     C*  OVERRIDE DATABASE FILE WDPPL TO WAPPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(21)
     C                   PARM                    LENGTH           15 5
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
     C*
     C*  OVERRIDE DATABASE FILE WMEPL1E TO WAEPL1E
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(23)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL6E TO WAEPL6E
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(25)
     C                   PARM                    LENGTH           15 5
     C*
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
     C*
     C                   EXSR      OVRHC
     C*
     C                   ENDSR
     C*
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
     C*
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
     C*================================================================
     C*   SUBROUTINE BILL
     C*   CALLS THE BILL WITH THE OVERRIDES AND THEN DELETES THE
     C*   OVERRIDES
     C*================================================================
     C*
     C     BILL          BEGSR
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
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C*
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        WKDATE
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      WKDATE        QQDATE            8
     C                   MOVE      *BLANKS       QQESCD            2
     C                   MOVE      'Y'           QQOVR             1
     C*** begin add ***  06/14/23  ***************************************
     C                   If        calcManP = 'Y'
     C                   Eval      QQOvr = 'Z'
     C                   Endif
     C*** end add *****  06/14/23  ***************************************
     C                   MOVE      *BLANKS       QQSRCD            2
     C                   MOVE      *BLANKS       QQPRT             1
     C                   MOVE      *BLANKS       QQAJCD            2
     C                   MOVE      'W'           QQQTE             1
     C                   MOVE      ' '           QQRNB1            1
     C     *IN67         IFEQ      *ON
     C                   MOVE      '#'           QQRNB1
     C                   ENDIF
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
     C*** begin del ***  06/14/23  ***************************************
     C*****XXLOSS        IFEQ      'Y'
     C*****              CALL      'W8010R'
     C*****              PARM                    QQCO#
     C*****              PARM                    QQFND
     C*****              PARM                    QQFYR
     C*****              PARM                    QQEMP#
     C*****              PARM                    QQDIV
     C*****              PARM                    QQDATE
     C*****              PARM                    QQESCD
     C*****              PARM                    QQOVR
     C*****              PARM                    QQSRCD
     C*****              PARM                    QQPRT
     C*****              PARM                    QQAJCD
     C*****              PARM                    QQQTE
     C*****              PARM                    QQRNB1
     C*****              ENDIF                                                  XXLOSS IFEQ
     C*** end del *****  06/14/23  ***************************************
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
     C                   MOVE      '0'           *IN87
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
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(16)
     C                   PARM                    LENGTH           15 5
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
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(22)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(24)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(26)
     C                   PARM                    LENGTH           15 5
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
      *
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
     C                   Z-ADD     *ZEROS        X1AA1
     C                   Z-ADD     *ZEROS        X2AA1
     C                   Z-ADD     *ZEROS        X3AA1
     C                   Z-ADD     *ZEROS        X4AA1
     C                   Z-ADD     *ZEROS        X5AA1
     C                   Z-ADD     *ZEROS        X6AA1
     C                   Z-ADD     *ZEROS        X7AA1
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
     C     ELBSCD        ANDEQ     XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** begin add ***  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDLT     YRMO
     c*** end add *****  10/18/24  *******************************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
     C     EAarcd        Andne     F1dvbl
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C                   If        xxPendCX = 'Y'
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C*
     C     *IN50         DOWEQ     '0'
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** begin add ***  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDLT     YRMO
     c*** end add *****  10/18/24  *******************************************
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
     C                   ADD       XAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C                   END
     C                   Endif                                                  If xxPendCX = 'Y'
     C*
     C*
     C                   MOVE      'A'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** begin add ***  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDLT     YRMO
     c*** end add *****  10/18/24  *******************************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
     c     EAarcd        Andne     F1dvbl
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C                   If        xxPendCX = 'Y'
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C*
     C     *IN50         DOWEQ     '0'
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** begin add ***  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDLT     YRMO
     c*** end add *****  10/18/24  *******************************************
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
     C                   ADD       XAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C                   END
     C                   Endif                                                  If xxPendCX = 'Y'
     C*
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
     C     ELBSCD        ANDEQ     XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** begin add ***  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDLT     YRMO
     c*** end add *****  10/18/24  *******************************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'S'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'E'
     C                   ADD       EAAMT         X1AMT1
     C                   ENDIF                                                  TTCTYP IFEQ
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
     C     TTCTYP        IFEQ      '6'
     C                   ADD       EAAMT         X6AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '7'
     C                   ADD       EAAMT         X7AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C                   If        xxPendCX = 'Y'
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C*
     C     *IN50         DOWEQ     '0'
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C*** begin add ***  10/18/24  *******************************************
     C     ELBLFL        ANDEQ     XXBLFL
     c*** end add *****  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** begin add ***  10/18/24  *******************************************
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBLFL        ANDNE     XXBLFL
     C     YYMM          ANDLT     YRMO
     c*** end add *****  10/18/24  *******************************************
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'S'
     C                   ADD       XAAMT         X1AMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C     TTCTYP        IFEQ      '1'
     C                   ADD       XAAMT         X1AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '2'
     C                   ADD       XAAMT         X2AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '3'
     C                   ADD       XAAMT         X3AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '4'
     C                   ADD       XAAMT         X4AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '5'
     C                   ADD       XAAMT         X5AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '6'
     C                   ADD       XAAMT         X6AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '7'
     C                   ADD       XAAMT         X7AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C                   END
     C                   Endif                                                  If xxPendCX = 'Y'
     C*
     C*
     C                   ENDSR
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
     C*
     C                   MOVEL     SVNAME        ENNAM1
     C                   MOVEL     SVFDNM        ENFDN1
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
     C                   MOVE      *BLANKS       ENMSFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENILFL
     C                   MOVE      *BLANKS       ENERFL
     C     XXPYCG        IFEQ      'Y'
     C                   MOVE      'Y'           ENCRFL
     C                   ELSE
     C                   MOVE      *BLANKS       ENCRFL
     C                   ENDIF
     C                   MOVE      *BLANKS       ENNOBF
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENCIFL
     C                   MOVE      *BLANKS       ENCMFL
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
     C     SCDSC1        IFNE      *BLANKS
     C                   MOVEL     SCDSC1        ENDSC1
     C                   MOVEL     SCDSC2        ENDSC2
     C                   MOVEL     SCDSC3        ENDSC3
     C                   ENDIF                                                  SCDSC1 IFNE *BLANKS
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
     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   WRITE     PEN01
     C*
     C                   ENDSR
     C*================================================================
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
     C                   XFOOT     WK1           XXA1             15 2
     C                   XFOOT     WK2           XXA2             15 2
     C                   XFOOT     WK3           XXA3             15 2
     C                   XFOOT     WK4           XXA4             15 2
     C                   XFOOT     WK5           XXA5             15 2
     C                   XFOOT     WK6           XXA6             15 2
     C                   XFOOT     WK7           XXA7             15 2
     C                   Z-ADD     DA(1)         XXDA              8 0
     C                   ELSE
     C                   XFOOT     PA            XXPA
     C                   XFOOT     P1            XXP1
     C                   XFOOT     A1            XXA1
     C                   XFOOT     A2            XXA2
     C                   XFOOT     A3            XXA3
     C                   XFOOT     A4            XXA4
     C                   XFOOT     A5            XXA5
     C                   XFOOT     A6            XXA6
     C                   XFOOT     A7            XXA7
     C                   Z-ADD     DA(1)         XXDA              8 0
     C                   ENDIF                                                  BSFREQ = 'T'
      *
     C                   Z-ADD     XXPA          SCPRAJ
     C                   ADD       XXP1          SCPRAJ
     C                   ADD       XXA1          SCPRAJ
     C                   ADD       XXA2          SCPRAJ
     C                   ADD       XXA3          SCPRAJ
     C                   ADD       XXA4          SCPRAJ
     C                   ADD       XXA5          SCPRAJ
     C                   ADD       XXA6          SCPRAJ
     C                   ADD       XXA7          SCPRAJ
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
     C*** begin add ***  05/14/20  *********************************************
      *================================================================
      *  SRCADJS - DISPLAY TOTAL OF ADJUSTMENTS for Single Division
      *================================================================
     C     SRCADJS       BEGSR
      *
     C                   CLEAR                   XXPATT
     C                   Clear                   xxda
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   If        EADiv = ELDiv
      *
     C                   If        ((EAStat = 'B' or EAStat = 'C' or
     C                             EAStat = 'W' or EAStat = 'X') and
     C                             xxARType = 'P') or
     C                             ((EAStat = 'T' or EAStat = 'U' or
     C                             EAStat = 'Y' or EAStat = 'Z') and
     C                             xxARType = 'T') or
     C                             ((EAStat = '1' or EAStat = 'D') and
     C                             xxARType = '1') or
     C                             ((EAStat = '2' or EAStat = 'E') and
     C                             xxARType = '2') or
     C                             ((EAStat = '3' or EAStat = 'F') and
     C                             xxARType = '3') or
     C                             ((EAStat = '4' or EAStat = 'G') and
     C                             xxARType = '4') or
     C                             ((EAStat = '5' or EAStat = 'H') and
     C                             xxARType = '5') or
     C                             ((EAStat = '6' or EAStat = 'J') and
     C                             xxARType = '6') or
     C                             ((EAStat = '7' or EAStat = 'K') and
     C                             xxARType = '7')
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
     C                   If        xxda = 0
     C                   Z-ADD     EApo01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         xxda
     C                   Endif
     C*
     C                   Endif
      *
     C                   Endif
      *
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   Z-ADD     XXPATT        SCPRAJ
      *
     C                   ENDSR
     C*** end add *****  05/14/20  *********************************************
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
     C                   XFOOT     WK1           XXA1             15 2
     C                   XFOOT     WK2           XXA2             15 2
     C                   XFOOT     WK3           XXA3             15 2
     C                   XFOOT     WK4           XXA4             15 2
     C                   XFOOT     WK5           XXA5             15 2
     C                   XFOOT     WK6           XXA6             15 2
     C                   XFOOT     WK7           XXA7             15 2
     C                   Z-ADD     DA(1)         XXDA              8 0
     C                   MOVE      *OFF          *IN62
     C                   ELSE
     C                   XFOOT     PA            XXPA
     C                   XFOOT     P1            XXP1
     C                   XFOOT     A1            XXA1
     C                   XFOOT     A2            XXA2
     C                   XFOOT     A3            XXA3
     C                   XFOOT     A4            XXA4
     C                   XFOOT     A5            XXA5
     C                   XFOOT     A6            XXA6
     C                   XFOOT     A7            XXA7
     C                   Z-ADD     DA(1)         XXDA
     C                   ENDIF
      *
     C                   CLEAR                   PA
     C                   CLEAR                   P1
     C                   CLEAR                   A1
     C                   CLEAR                   A2
     C                   CLEAR                   A3
     C                   CLEAR                   A4
     C                   CLEAR                   A5
     C                   CLEAR                   A6
     C                   CLEAR                   A7
     C                   CLEAR                   DA
     C                   Z-ADD     XXPA          PA(1)
     C                   Z-ADD     XXP1          P1(1)
     C                   Z-ADD     XXA1          A1(1)
     C                   Z-ADD     XXA2          A2(1)
     C                   Z-ADD     XXA3          A3(1)
     C                   Z-ADD     XXA4          A4(1)
     C                   Z-ADD     XXA5          A5(1)
     C                   Z-ADD     XXA6          A6(1)
     C                   Z-ADD     XXA7          A7(1)
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
     c*** Begin Add *** 06/17/21 *******************************************
     c                   Eval      isWMEAP = *Off
     c*** End   Add *** 06/17/21 *******************************************
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA04
     C                   ENDIF
      *
      *  FOR MONTHLY PLANS
      *
     C                   ELSE
     C                   XFOOT     AR            XXPA
     C                   CLEAR                   AR
     C                   Z-ADD     XXPA          EAAM01
     C     1             DO        12            X4                3 0
     C     J1D(X4)       IFNE      *ZERO
     C                   Z-ADD     J1D(X4)       SVPO01            7 0
     C                   CLEAR                   J1D
     C                   Z-ADD     SVPO01        EAPO01
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   UPDATE    PEA04
     C                   ENDIF
      *
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ENDIF                                                  XXDIVC = 1
      *
     C                   ENDSR                                                  SRLIMP
      *** beg del ***** 01/23/20 **************************
     C*---------------------------------------------------------------
     C* CHECK RECORD IN USE
     C*---------------------------------------------------------------
     C***     CHKRIU        BEGSR
     C***                   MOVE      *OFF          @@RTCD
     C***     RIUKY         CHAIN     PRI01                              60
     C***     *IN60         IFEQ      *OFF
     C***                   MOVE      RIJOBN        JOB6A             6
     C***                   MOVEL(P)  RIJOB         SCJOB
     C***     SCJOB         CAT       '/':0         SCJOB
     C***     SCJOB         CAT       JOB6A:0       SCJOB
     C***     SCJOB         CAT       '/':0         SCJOB
     C***     SCJOB         CAT       RIUSER:0      SCJOB
     C***                   EXFMT     WDWMSG4
     C***                   MOVE      *ON           @@RTCD
     C***                   MOVE      *ON           @@KC
     C***                   GOTO      TERM
     C***                   ELSE
     C***                   TIME                    DSDTTM
     C***                   MOVEL     'WMEMP'       RINAM
     C***                   MOVE      @@RRN2        RIRRN
     C***                   MOVE      XXCO#         RICO#
     C***                   MOVE      XXFND         RIFND
     C***                   MOVE      SCEMP#        RIEMP#
     C***                   MOVE      Q1JOBN        RIJOBN
     C***                   MOVE      Q1JOB         RIJOB
     C***                   MOVE      Q1USER        RIUSER
     C***                   MOVE      DSTIME        RITIME
     C***                   MOVE      DSDATE        RIDATE
     C***                   WRITE     PRI01
     C***                   Eval      riuCreated = *On
     C***                   ENDIF                                                  *IN60
     C***                   ENDSR
      *** end del ***** 01/23/20 **************************
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
     C*---------------------------------------------------------------
     C* Process deposit adjustments.
     C*---------------------------------------------------------------
     C     DepositAdj    BegSr
     C                   Z-ADD     *ZEROS        XXADJ            11 2
     C                   Z-ADD     *ZEROS        XXADJ2           11 2
      /free
       If skipDepAdj = *on;
          noSr = *on;
          LeaveSr;
       Endif;
      /end-free
      /Free
       xxab01 = GetDepPercent(BSsefl);

       Select;
       When BSsefl = 'E';
          currMaxDepPrem = currEstMaxDep;
       When BSsefl = 'S';
          currMaxDepPrem = currSrMaxDep;
       Endsl;

       Select;
       When prevBSsefl = 'E';
          prevMaxDepPrem = prevEstMaxDep;
       When prevBSsefl = 'S';
          prevMaxDepPrem = prevSrMaxDep;
       Endsl;

       Select;
       When F1dps = 'M';
          currDepPrem = GetLesserOf(currMaxDepPrem:ELmapr);
          prevDepPrem = GetLesserOf(prevMaxDepPrem:svMaPr);
          xxAdj = currDepPrem - prevDepPrem;
       When F1dps = 'S';
          currDepPrem = GetLesserOf(currMaxDepPrem:ELstpr);
          prevDepPrem = GetLesserOf(prevMaxDepPrem:svStPr);
          xxAdj = currDepPrem - prevDepPrem;
       When F1dps = 'N';
          currDepPrem = GetLesserOf(currMaxDepPrem:ELenmp);
          prevDepPrem = GetLesserOf(prevMaxDepPrem:svNmPr);
          xxAdj = currDepPrem - prevDepPrem;
       Other;
          currDepPrem = GetLesserOf(currMaxDepPrem:(ELanpr-Elprtd));
          prevDepPrem = GetLesserOf(prevMaxDepPrem:s2AnPr);
          xxAdj = currDepPrem - prevDepPrem;
       Endsl;
      /End-Free
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDMFL) IF THE POLICY YEAR
      *  IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      *ON           XXDPYM            1
     C                   ELSE
     C                   MOVE      *OFF          XXDPYM            1
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XXARPR           11 2
     C                   MOVE      'H'           XXARTY
     C     EAKY3         CHAIN     PEA03                              50           ALL FYRS
     C     *IN50         DOWEQ     '0'
     C     EAFYR         ANDLE     XXFYR
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
     C     EAKY3         READE     PEA03                                  50
     C                   END                                                    *IN50
     C*
     C                   Z-ADD     XXARPR        XXDEPB           11 2
     C*
     C* RESET GROUP DETAIL
     C*
     C     F1KY          SETLL     PF101
     C     F1KY          READE     PF101                                  60
     C*
     C*
     C     XXAB01        IFNE      *ZEROS
     C     XXARPR        DIV       XXAB01        XXARPR
     C                   END
     C*
      /Free
       xxAdj2 = PrevDepPrem - xxArPr;
      /End-Free
     C*
     C*  SET ADJUSTMENT TO DIFFERENCE IN OLD AND NEW PREMIUM ONLY IF
     C*  RUNNING FOR SINGLE DIVISION BILL.
     C*
     C     *IN67         IFEQ      *OFF
     C     ELARSM        ORNE      'Y'
     C                   ADD       XXADJ2        XXADJ
     C                   ENDIF                                                  *IN67 IFEQ
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
     C     XXADJ         MULT(H)   XXAB01        XXWK90            9 0
     C     XXAB01        IFEQ      *ZERO
     C     *IN67         ANDEQ     *OFF
     C     XXAB01        OREQ      *ZERO
     C     ELARSM        ANDNE     'Y'
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   Z-ADD     XXWK90        SCTOT
     C                   MOVE      *BLANK        XXTHRS            1
     C     F1BVFL        IFNE      'Y'
     C     ELBSCD        ANDEQ     XXBSCD
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
     C     XXDEPB        ANDNE     *ZERO
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
     C     XXDEPB        ANDNE     *ZERO
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ENDIF                                                  XXTMP IFLT
     C*
     C                   ENDIF                                                  XXWK90 IFGE
     C*
     C                   ENDIF                                                  F1BVFL IFNE
     C*
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
     C*
     C                   Z-ADD     SCTOT         XXDEPP           11 2
     C*
     C*  CALCULATE THE STATE TAX ADJUSTMENT
     C*
     C                   Z-ADD     0             XXDEPA           11 2
     C                   MOVE      'H'           XXARTY
     C     EAKY2         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C                   ADD       EAAMT         XXDEPA
     C     EAKY2         READE     PEA01                                  50
     C                   ENDDO                                                  *IN50
     C                   Z-ADD     0             XXTXDU           11 2
     C                   MOVE      'F'           XXARTY
     C     EAKY2         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
      /Free
       If TTcTyp = 'D';
          xxTxDu += EAamt;
       Endif;
      /End-Free
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C                   END                                                    *IN50
     C*
     C     RRTDFL        IFNE      'Y'
     C     XXTHRS        OREQ      'Y'
     C     RRTDFL        IFNE      'Y'
     C                   MULT      -1            XXTXDU
     C                   ELSE
     C     XXDEPA        MULT(H)   RRTXPC        XXWK90
     C                   MULT(H)   .01           XXWK90
     C     XXWK90        SUB       XXTXDU        XXTXDU
     C                   ENDIF                                                  RRTDFL IFNE
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     0             XXTXDU
     C                   ENDIF
     C                   ELSE
     C*
     C     XXAB01        MULT(H)   XXADJ         XXWK90
     C     XXAB01        IFEQ      *ZERO
     C     *IN67         ANDEQ     *OFF
     C     XXAB01        OREQ      *ZERO
     C     ELARSM        ANDNE     'Y'
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   ADD       XXWK90        XXDEPA
     C                   MULT(H)   RRTXPC        XXWK90
     C                   MULT(H)   .01           XXWK90
     C     XXDEPA        MULT(H)   RRTXPC        XXTEMP           11 0
     C                   MULT(H)   .01           XXTEMP
     C                   EVAL      xxTXDU = XXWK90
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XXWK90        XXTXDU
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRTDFL IFNE
     C*
     C                   ADD       XXTXDU        SCTOT
     C                   MOVE      '0'           *IN80
     C     ELARSM        IFEQ      'Y'
     C*
     C     XXWAR         IFNE      'Y'
     C                   MOVE      'Y'           XXSUM
      /free
       xtDepDue = xtDepDue + xxDepp;
       xtDepTaxDue = xtDepTaxDue + xxTxDu;
      /end-free
     C                   Z-ADD     0             SCTOT
     C                   ELSE
      /free
       xxDepp = xtDepDue;
       xxTxDu = xtDepTaxDue;
      /end-free
     C     XXDEPP        ADD       XXTXDU        SCTOT
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C                   Z-ADD     0             X
     C                   MOVE      *OFF          *IN89
     C                   EndSr
     C*** begin add ***  05/14/20  *****************************************************
     C*
     C*  New subroutine to handle display adjustments, popups, and editing fields prior to
     C*  bill submission.
     C*
     C     BillWindow    Begsr
      /Free

       If LiveCheck();

          PopupMsg = 'User ' + %trim(saveUser)
                     + ' has updated this policy subsequent to when this '
                     + 'revised bill started. '
                     + 'Press OK to discard currect changes and '
                     + 'restart based on current data. ';

          PopOneBtn(PopupMsg:'Information');

          clearTemp();

          q2Recf = 'SCR01';
          Exsr scr1Tag;
       Endif;

       //*** begin del ***  01/18/21  **************************************
       //*****If not %open(WDELL);
       //*****   Open Wdell;
       //*****   xxopn2 = '1';
       //*****Endif;
       //*** end del *****  01/18/21  **************************************

       Clear visLump;
       Clear scLump;
       Exsr SRCAdj;
       If xxLSFL = 'Y' and SCPRAJ <> 0;
          visLump = *on;
       Endif;
       //*** End Add ***** 12/12/23 ***********************************
       If isUseScript;
          *In66 = *On;
       Endif;
        //*** End Add ***** 12/12/23 ***********************************
       //*** begin add ***  01/05/21  **************************************
       If visDisc = *on;
          intBWdwTop = 100;
          intBWdwHgt = 555;
       Else;
          intBWdwTop = 140;
          intBWdwHgt = 515;
       Endif;
       //*** end add *****  01/05/21  **************************************
       xxtaa = *off;
       xx1aa = *off;
       xx2aa = *off;
       xx3aa = *off;
       xx4aa = *off;
       xx5aa = *off;
       xx6aa = *off;
       xx7aa = *off;
       scTotAdj = SCPRAJ;
       scTotNew = 0;
       scTotOld = 0;
       scTotDiff = 0;
       scDep = 'Y';
       //*** begin add ***  01/18/21  **************************************
       scShowAll = ' ';
       showAllChg = *off;
       visShowAll = *on;
       //* Default to show all divisions when there is no adjustment.
       If *in80 = *on;
          scShowAll = 'Y';
       Endif;
       //*** begin add ***  12/08/25  ****************************************
       If *in67 = *on and ELARSM = 'Y';
          scShowAll = 'Y';
       Endif;
       //*** end add *****  12/08/25  ****************************************
       //* If there is only one division hide checkbox.
       If xxDivc = 1;
          visShowAll = *off;
       Endif;
       //*** end add *****  01/18/21  **************************************

       reqEst = *on;
       If *in80 = *on or noEst = *on;
          reqEst = *off;
       Endif;

       reqSR = *on;
       If *in80 = *on or noSR = *on;
          reqSR = *off;
       Endif;

       //*** begin add ***  01/18/21  **************************************
       //*  Execute loop first time and until showAllChg button is not pressed.
       Dou showAllChg = *off;
          showAllChg = *off;

          If not %open(WDELL);
             Open Wdell;
             xxopn2 = '1';
          Endif;
       //*** end add *****  01/18/21  **************************************

       //* Load the adjustments grid if adjustments are present.
       If *in80 = *off;

          //* Single division comes from data structures.
          If xxDivc = 1;
             scGrdDiv = scDiv;
             scGrdPodt = DA(1);
             Exsr SRCadj;
             scGrdAmt = xxPA;
             scGrdSum = ' ';
             scGrdType = 'Premium';
             If BSSEFL = 'S';
                scGrdType = 'Deposit';
             Endif;
             scGrdARTY = 'P';
             Chain(n) elky1 pel01;
             scGrdOPrm = ELAnpr;
             Chain(n) elky1 pel02;
             scGrdNPrm = ELAnpr;
             scGrdDiff = scGrdNPrm - scGrdOPrm;
             visGrdPrem = *on;
             visGrdEdit = *on;
             scGrdRRN = 0;
             scGrdRRNA = dbrrn4;
             rrnBillGrd = rrnBillGrd + 1;
             Write SflAdjust;

             scTotNew = scTotNew + scGrdNPrm;
             scTotOld = scTotOld + scGrdOPrm;
             scTotDiff = scTotDiff + scGrdDiff;

             scGrdOPrm = 0;
             scGrdNPrm = 0;
             scGrdDiff = 0;
             visGrdPrem = *off;

             //* Check if record needs to be written for tax and assessments.
             Exsr assmntChk;

             //* Include state tax record.
             If xxtaa;
                scGrdAmt = xxp1;
                scGrdType = 'State Tax';
                scGrdARTY = 'T';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

             //* Include 1st assessment record.
             If xx1aa;
                scGrdAmt = xxa1;
                scGrdType = '1st Assessment';
                scGrdARTY = '1';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

             //* Include 2nd assessment record.
             If xx2aa;
                scGrdAmt = xxa2;
                scGrdType = '2nd Assessment';
                scGrdARTY = '2';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

             //* Include 3rd assessment record.
             If xx3aa;
                scGrdAmt = xxa3;
                scGrdType = '3rd Assessment';
                scGrdARTY = '3';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

             //* Include 4th assessment record.
             If xx4aa;
                scGrdAmt = xxa4;
                scGrdType = '4th Assessment';
                scGrdARTY = '4';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

             //* Include 5th assessment record.
             If xx5aa;
                scGrdAmt = xxa5;
                scGrdType = '5th Assessment';
                scGrdARTY = '5';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

             //* Include 6th assessment record.
             If xx6aa;
                scGrdAmt = xxa6;
                scGrdType = '6th Assessment';
                scGrdARTY = '6';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

             //* Include 7th assessment record.
             If xx7aa;
                scGrdAmt = xxa7;
                scGrdType = '7th Assessment';
                scGrdARTY = '7';
                rrnBillGrd = rrnBillGrd + 1;
                Write SflAdjust;
             Endif;

          Else;
          //* Multi-division loads from A/R work file, loop over employer detail.
        //*** begin chg ***  12/08/25  ****************************************
        //***If *in67 = *on;
        //***   Setll emky pem02;
        //***   Reade(n) emky pem02;
        //***Else;
        //***   Setll emky1 pem02;
        //***   Reade(n) emky1 pem02;
        //***Endif;
        //***
        //***Dow not %eof;
             Setll emky1 pem02;
             Reade(n) emky1 pem02;
             Dow not %eof;
              If *in67 = *off or (*in67 = *on and
               (EMDiv = xsDiv or EMDiv = scDiv));
        //*** end chg *****  12/08/25  ****************************************
                xxDiv = EMDiv;
                Chain(n) elky1 pel02;
        //*** begin chg ***  12/08/25  ****************************************
        //***** If %found;
                If %found and (*in67 = *off or (*in67 = *on and
                 ((Emdiv = xsDiv and ELARSM = 'Y') or EMDiv = scDiv)));
        //*** end chg *****  12/08/25  ****************************************
                   scGrdDiv = xxDiv;
                   xxARType = 'P';
                   scGrdARTY = xxARType;
                   Exsr SRCadjs;
                   scGrdPodt = xxda;
                   scGrdAmt = scpraj;
                   scGrdSum = ' ';
                   If ELARSM = 'Y' and EMSTRP = 'Y';
                      scGrdSum = 'Yes';
                   Endif;
                   scGrdType = 'Premium';
                   Chain (ELco#:%editc(ELbscd:'X')) pbs01;
                   If BSSEFL = 'S';
                      scGrdType = 'Deposit';
                   Endif;
                   Chain(n) elky1 pel01;
                   scGrdOPrm = ELAnpr;
                   Chain(n) elky1 pel02;
                   scGrdRRNA = dbrrn4;
                   scGrdNPrm = ELAnpr;
                   scGrdDiff = scGrdNPrm - scGrdOPrm;
                   visGrdPrem = *on;
                   visGrdEdit = *on;

                   scTotNew = scTotNew + scGrdNPrm;
                   scTotOld = scTotOld + scGrdOPrm;
                   scTotDiff = scTotDiff + scGrdDiff;
                   Setll eaky6 pea04;
                   Reade(n) eaky6 pea04;
                   Dow not %eof;
                      If EAStat = 'B' or EAStat = 'C' or
                       EAStat = 'X' or EAStat = 'W';
                         scGrdRRN = DBrrn5;
                         rrnBillGrd = rrnBillGrd + 1;
                         Write SflAdjust;
                         scGrdOPrm = 0;
                         scGrdNPrm = 0;
                         scGrdDiff = 0;
                         visGrdPrem = *off;
                      Endif;
                      Reade(n) eaky6 pea04;
                   Enddo;

       //*** begin chg ***  01/18/21  **************************************
       //********* If visGrdPrem = *on;
                   If visGrdPrem = *on and scShowAll = 'Y';
       //*** end chg *****  01/18/21  **************************************
                      visGrdEdit = *off;
                      rrnBillGrd = rrnBillGrd + 1;
                      Write SflAdjust;
                   Endif;

                   scGrdOPrm = 0;
                   scGrdNPrm = 0;
                   scGrdDiff = 0;
                   visGrdPrem = *off;
                   visGrdEdit = *on;

                //* Check if record needs to be written for tax and assessments.
                   xxARType = 'T';
                   Exsr SRCadjs;
                //* Include state tax record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = 'State Tax';
                      scGrdARTY = xxARType;
                      If BSSEFL = 'S';
                         scGrdType = 'Tax on Deposit';
                      Endif;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = 'T' or EAStat = 'U' or
                          EAStat = 'Y' or EAStat = 'Z';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                   xxARType = '1';
                   Exsr SRCadjs;
                //* Include 1st assessment record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = '1st Assessment';
                      scGrdARTY = xxARType;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = '1' or EAStat = 'D';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                   xxARType = '2';
                   Exsr SRCadjs;
                //* Include 2nd assessment record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = '2nd Assessment';
                      scGrdARTY = xxARType;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = '2' or EAStat = 'E';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                   xxARType = '3';
                   Exsr SRCadjs;
                //* Include 3rd assessment record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = '3rd Assessment';
                      scGrdARTY = xxARType;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = '3' or EAStat = 'F';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                   xxARType = '4';
                   Exsr SRCadjs;
                //* Include 4th assessment record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = '4th Assessment';
                      scGrdARTY = xxARType;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = '4' or EAStat = 'G';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                   xxARType = '5';
                   Exsr SRCadjs;
                //* Include 5th assessment record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = '5th Assessment';
                      scGrdARTY = xxARType;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = '5' or EAStat = 'H';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                   xxARType = '6';
                   Exsr SRCadjs;
                //* Include 6th assessment record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = '6th Assessment';
                      scGrdARTY = xxARType;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = '6' or EAStat = 'J';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                   xxARType = '7';
                   Exsr SRCadjs;
                //* Include 7th assessment record.
                   If scpraj <> 0;
                      scGrdAmt = scpraj;
                      scGrdType = '7th Assessment';
                      scGrdARTY = xxARType;
                      Setll eaky6 pea04;
                      Reade(n) eaky6 pea04;
                      Dow not %eof;
                         If EAStat = '7' or EAStat = 'K';
                            scGrdRRN = DBrrn5;
                            rrnBillGrd = rrnBillGrd + 1;
                            Write SflAdjust;
                         Endif;
                         Reade(n) eaky6 pea04;
                      Enddo;
                   Endif;

                Endif;

        //*** begin chg ***  12/08/25  ****************************************
        //***** If *in67 = *on;
        //*****    Reade(n) emky pem02;
        //***** Else;
        //*****    Reade(n) emky1 pem02;
        //***** Endif;
              Endif;
              Reade(n) emky1 pem02;
        //*** end chg *****  12/08/25  ****************************************
             Enddo;
             //* Reset file info
             xxDiv = scDiv;
             Chain(n) emky pem02;
             Chain(n) elky1 pel02;
             Chain (ELco#:%editc(ELbscd:'X')) pbs01;
          Endif;

       Else;
       //* Load all divisions with just premium info as no adjustments present.
          If *in67 = *on;
             Setll emky pem02;
             Reade(n) emky pem02;
          Else;
             Setll emky1 pem02;
             Reade(n) emky1 pem02;
          Endif;

          Dow not %eof;
             xxDiv = EMDiv;
             Chain(n) elky1 pel02;
             If %found;
                scGrdDiv = xxDiv;
                scGrdARTY = 'P';
                scGrdPodt = 0;
                scGrdAmt = 0;
                scGrdSum = ' ';
                scGrdType = 'Premium';
                Chain (ELco#:%editc(ELbscd:'X')) pbs01;
                Chain(n) elky1 pel01;
                scGrdOPrm = ELAnpr;
                Chain(n) elky1 pel02;
                scGrdRRNA = dbrrn4;
                scGrdNPrm = ELAnpr;
                scGrdDiff = scGrdNPrm - scGrdOPrm;
                visGrdPrem = *on;
                visGrdEdit = *off;
       //*** begin chg ***  01/18/21  **************************************
       //*****  rrnBillGrd = rrnBillGrd + 1;
       //*****  Write SflAdjust;
                If scShowAll = 'Y';
                   rrnBillGrd = rrnBillGrd + 1;
                   Write SflAdjust;
                Endif;
       //*** end chg *****  01/18/21  **************************************

                scTotNew = scTotNew + scGrdNPrm;
                scTotOld = scTotOld + scGrdOPrm;
                scTotDiff = scTotDiff + scGrdDiff;

             Endif;

             If *in67 = *on;
                Reade(n) emky pem02;
             Else;
                Reade(n) emky1 pem02;
             Endif;

          Enddo;

          //* Reset file info
          xxDiv = scDiv;
          Chain(n) emky pem02;
          Chain(n) elky1 pel02;
          Chain (ELco#:%editc(ELbscd:'X')) pbs01;

       Endif;

       Close Wdell;
       xxopn2 = '0';

       Dou cancelWdw = *on or submitWdw = *on;

          Clear cancelWdw;
          Clear submitWdw;
          Clear lumpChg;
          Exsr ClearErrors;

          Dou billError = *off or cancelWdw = *on;

            Exfmt rcdBill;
            Exsr ClearErrors;
          //*** begin add ***  01/18/21  **************************************
            If showAllChg = *on;
               cancelWdw = *on;
               submitWdw = *off;
            Endif;
          //*** end add *****  01/18/21  **************************************

            If lumpChg = *on;
               If scLump = 'Y';
                  Exsr SRLump;
                  cancelWdw = *off;
                  submitWdw = *off;
                  billError = *on;
               Else;
                  *inKG = *on;
                  cancelWdw = *on;
                  redoCalc = *on;
               Endif;
               lumpChg = *off;
            Endif;

            If submitWdw = *on;
               Exsr BillCheck;
               If billError;
                  submitWdw = *off;
               Endif;
            Endif;

            //*  Launch Popups for adjustments grid.
            Readc SflAdjust;
            If not %EOF();
               If gridPrem = *on;
                  Exsr PremLink;
                  gridPrem = *off;
                  Update SflAdjust;
                  billError = *on;
               Endif;
               If gridEdit = *on;
                  Exsr EditLink;
                  gridEdit = *off;
                  Update SflAdjust;
                  billError = *on;
               Endif;
            Endif;
            //*** Beg Add ***** 12/12/23 ***********************************
            If billError = *Off and cancelWdw = *Off;
               xxSubmit = *on;
               If isUseScript;
                  xxProcType = processType;
                  Chain xxCo# pbl001;

                  Select;
                  //Non-Mid-term Endorsement
                  When PgmInStack('WTPOLMEND') = *Off;
                  //*** begin chg ***  03/06/25  *******************************
                  //*****   If xxDivC = 1 and BLPrmnR = ' ';
                     If xxDivC = 1 or BLCombR = 'Y';
                  //*** end chg *****  03/06/25  *******************************
                        xxMass = ' ';
                     Else;
                        xxMass = 'Y';
                     Endif;
                     In *Lock *dtaara;
                     xxDart = 'N';
                     xxRMCD = ' ';
                     Chain(n) (xxCo#:scFnd) pex01;
                     If %found(WMEXL) and EXREV <> ' ' and EX3rdb <> 'Y';
                        xxDart = 'Y';
                        xxRMCD = EXREV;
                     Endif;
                  //*** begin add ***  08/25/25  ***********************************
                     Chain(n) xxProcType psp00;
                     If %found;
                        If UPPKey = ' ' or SPOverP = 'Y';
                           xxPrinter = SPPKey;
                           xxTray = SPTray;
                        Else;
                           xxPrinter = UPPKey;
                           xxTray = ' ';
                        Endif;
                        If xxPrinter <> ' ';
                           xxPrint = 'Y';
                        Endif;
                     Endif;
                  //*** end add *****  08/25/25  ***********************************
                     Out *dtaara;
                  //Mid-term Endorsement
                  When PgmInStack('WTPOLMEND') = *On;
                     xxMass = 'Y';
                  //*** begin add ***  03/06/25  *******************************
                     If xxDivC = 1 or BLCombR = 'Y';
                        xxMass = ' ';
                     Endif;
                  //*** end chg *****  03/06/25  *******************************
                     Chain (xxCo#:xxFnd) pdx01;
                     In *Lock *dtaara;
                     If DXPIAP <> *Blank;
                        xxDart = 'Y';
                        xxRMCD = DXPIAP;
                     Else;
                        xxDart = 'N';
                        xxRMCD = ' ';
                     Endif;
           //*** begin add ***  12/05/24  ************************************
                     Setll enky pen01;
                     Reade(n) enky pen01;
                     If not %eof and ENRMCD <> ' ';
                        xxDart = 'Y';
                        xxRMCD = ENRMCD;
                     Endif;
           //*** end add *****  12/05/24  ************************************
                     keyEmail = %Trim(Q1USER) +
                      %subst(%char(%timestamp:*iso0):1:17);
                  //*** begin add ***  08/25/25  ***********************************
                     xxProcType = 'Mid-Term Endorsement';
                     Chain(n) xxProcType psp00;
                     If %found;
                        If UPPKey = ' ' or SPOverP = 'Y';
                           xxPrinter = SPPKey;
                           xxTray = SPTray;
                        Else;
                           xxPrinter = UPPKey;
                           xxTray = ' ';
                        Endif;
                        If xxPrinter <> ' ';
                           xxPrint = 'Y';
                        Endif;
                     Endif;
                     xxProcType = processType;
                  //*** end add *****  08/25/25  ***********************************
                     Out *dtaara;
                  Endsl;

                  //*** begin chg ***  03/06/25  *******************************
                  //*****xxSubmit = OutputWindow(xxProcType:xxMass);
                  If @@RRN <> 0 and xxMass = ' ';
                     xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':@@RRN);
                  Else;
                     xxSubmit = OutputWindow(xxProcType:xxMass);
                  Endif;
                  //*** end chg *****  03/06/25  *******************************
                  If xxSubmit = *off;
                     billError = *on;
                  Else;
                     In *Lock *dtaara;
                     Out *dtaara;
                     scDart = xxDart;
                     scRmCd = xxRMCD;
                  Endif;
               Endif;
            Endif;
            //*** End Add ***** 12/12/23 ***********************************

          Enddo;

       Enddo;

       //* Return to policy detail scren if cancelling.
       If cancelWdw = *on;
          dspBillGrd = *Off;
          clrBillGrd = *On;
          Write rcdBill;
          dspBillGrd = *On;
          clrBillGrd = *Off;
          rrnBillGrd = 0;
       //*** begin chg ***  01/18/21  **************************************
       //*****   q2recf = 'SCR02';
          If showAllChg = *off;
             q2recf = 'SCR02';
          Else;
             cancelWdw = *off;
          Endif;
       //*** end chg *****  01/18/21  **************************************
       Endif;

       //*** begin add ***  01/18/21  **************************************
       //* End of showAllChg loop - drop out after 1st time unless showAllChg pressed,
       //* in which case reload everything and redisplay.
       Enddo;
       //*** end add *****  01/18/21  **************************************

       //* Write endorsement data if mid-term endorsement.
       If submitWdw = *on and visEndorse = *on;
          Exsr WrtPen;
       Endif;

       //* Proceed to submitting print job if complete.
       If submitWdw = *on;
          Exsr prtTag;
       Endif;

      /End-free
     C                   Endsr                                                  BillWindow Subr
     C**************************************************************************
     C*  Clear errors for bill window.
     C*
     C     ClearErrors   Begsr
     C
     C                   Move      *off          billError
     C                   Move      *off          *in20
     C                   Move      *off          *in24
     C                   Move      *off          *in30
     C                   Move      *off          *in34
     C                   Move      *off          *in35
     C                   Move      *off          *in46
     C                   Move      *off          *in71
     C                   Move      *off          *in84
     C                   Move      *off          *in85
     C
     C                   Endsr                                                  ClearErrors Subr
     C**************************************************************************
     C*
     C     scr1Tag       Begsr
     C                   Goto      Scr01t
     C                   Endsr
     C*
     C     prtTag        Begsr
     C                   Goto      Prnt
     C                   Endsr
     C*
     C**************************************************************************
     C* Determine if each assessment or tax array has an adjustment present.
     C     assmntChk     Begsr
      /free
           s = 1;
           Dow s < 14;
              If p1(s) <> 0;
                 xxtAA = *on;
              Endif;
              If a1(s) <> 0;
                 xx1AA = *on;
              Endif;
              If a2(s) <> 0;
                 xx2AA = *on;
              Endif;
              If a3(s) <> 0;
                 xx3AA = *on;
              Endif;
              If a4(s) <> 0;
                 xx4AA = *on;
              Endif;
              If a5(s) <> 0;
                 xx5AA = *on;
              Endif;
              If a6(s) <> 0;
                 xx6AA = *on;
              Endif;
              If a7(s) <> 0;
                 xx7AA = *on;
              Endif;
              s = s + 1;
           Enddo;
      /end-free
     C                   Endsr
     C**************************************************************************
     C*  Check for errors on bill window.
     C*
     C     BillCheck     Begsr
      /Free

       If LiveCheck();

          PopupMsg = 'User ' + %trim(saveUser)
                     + ' has updated this policy subsequent to when this '
                     + 'revised bill started. '
                     + 'Press OK to discard currect changes and '
                     + 'restart based on current data. ';

          PopOneBtn(PopupMsg:'Information');

          clearTemp();

          q2Recf = 'SCR01';
          Exsr scr1Tag;
       Endif;

       //*** begin add ***  06/05/21  ********************************************
       //* Set employer detail values to division called from.
       If xxopn2 = '0';
          Open wdell;
          xxopn2 = '1';
       Endif;
       Chain(n) elky pel01;
       If xxopn2 = '1';
          Close wdell;
          xxopn2 = '0';
       Endif;
       Chain(n) elky pel02;
       //*** end add *****  06/05/21  ********************************************

       //* EDIT EFFECTIVE DATE IF THERE ARE NO A/R ADJUSTMENTS TO ENSURE
       //* PROPER DATE IS PASSED ALONG.

       If *in80 = '1';

       //*   THE ENDORSEMENT EFFECTIVE DATE MUST BE VALID
          If scEfdt = 0;
             *in71 = *on;
             billError = *on;
          Else;
             srdti = scEfdt;
             Exsr DateCheck;
             scEfdt = %int(srdto);

             If srerr = '1';
                *in71 = *on;
                billError = *on;
             Endif;

       //*  ENDORSEMENT EFF DATE MUST BE WITHIN THE POLICY PERIOD
             srdti = scEfdt;
             Exsr DateTo7;
             xxEfd2 = %int(srdto);

             If xxEfd2 < ELPfdt or xxEfd2 >= ELPtdt;
                *in34 = *on;
                billError = *on;
             Endif;

          Endif;

       Endif;

       //*  EDIT COMMENT CODES IF ENTERED
       If scCom1 <> ' ' or scCom2 <> ' ' or scCom3 <> ' ';
          If scFcm1 <> ' ' or scFcm2 <> ' ' or scFcm3 <> ' ';
             *in46 = *on;
             billError = *on;
          Endif;
       Endif;

       //*  Default deposit flag if needed.
       If scDep = ' ';
          scDep = 'N';
       Endif;

       //*  Validate Darts remark code if needed.
       If scDart = 'Y';
          Chain(n) scRmcd pct01;
          If not %found;
             *in84 = *on;
             billError = *on;
          Else;
             If CTStat = 'I';
                *in85 = *on;
                billError = *on;
             Endif;
          Endif;
       Else;
          scRmcd = ' ';
       Endif;

       //*  *IN80 WILL BE SET IF EITHER THE FUND DETAIL FLAG IS 'N' OR IF
       //*  NO ADJUSTMENTS ARE BEING POSTED.  IN BOTH OF THESE CASES THERE
       //*  IS NO NEED TO VALIDATE THE ADJUSTMENT A/R CODE ON THIS SCREEN
       //*  NOR DO WE NEED TO WRITE A RECORD TO THE A/R ADJUSTMENT FILE
       //*  (WAEAP) SINCE NO ADJUSTMENTS WILL BE WRITTEN.

       If *in80 = '0';

       //*  VALIDATE THE PAYMENT DUE DATE FOR SELF REPORTING TYPES
          If BSSefl = 'S' and scDep = 'Y';
             If scDate = 0;
                *in24 = *on;
                billError = *on;
             Else;
                srdti = scDate;
                Exsr DateCheck;
                scDate = %int(srdto);
                If srerr = '1';
                   *in24 = *on;
                   billError = *on;
                Endif;
             Endif;
          Endif;

       //* THE ENDORSEMENT EFFECTIVE DATE MUST BE VALID
          If scEfdt = 0;
             *in71 = *on;
             billError = *on;
          Else;
             srdti = scEfdt;
             Exsr DateCheck;
             scEfdt = %int(srdto);
             If srerr = '1';
                *in71 = *on;
                billError = *on;
             Endif;

       //* ENDORSEMENT EFF DATE MUST BE WITHIN THE POLICY PERIOD
             srdti = scEfdt;
             Exsr DateTo7;
             xxEfd2 = %int(srdto);

             If xxEfd2 < ELPfdt or xxEfd2 >= ELPtdt;
                *in34 = *on;
                billError = *on;
             Endif;

          Endif;

       //*  VALIDATE A/R TRANSACTION CODE - REQUIRED
       //*  MUST BE ADJUSTMENT TYPE FOR ESTIMATING TYPES OF EMPLOYERS
       //*  OR DEPOSIT for S/R.
          xxEsty = ' ';
          xxEsds = ' ';
          xxSrty = ' ';
          xxSrds = ' ';

          If noEst = *off;

             xxKey = scAR;
             Chain(n) tbky ptt01;
             If scAR = *blanks or not %found;
                *in30 = *on;
                billError = *on;
             Else;
                xxEsty = TTType;
                xxEsds = TTDesc;
             Endif;

          Endif;

          If noSR = *off;

             xxKey = scARD;
             Chain(n) tbky ptt01;
             If scARD = *blanks or not %found;
                *in35 = *on;
                billError = *on;
             Else;
                xxSrty = TTType;
                xxSrds = TTDesc;
             Endif;

          Endif;

       Endif;

       //*  THE ENDORSEMENT DESCRIPTION CANNOT BE BLANK
       If visEndorse = *on;
          If scDsc1 = ' ';
             *in20 = *on;
             billError = *on;
          Endif;
       Endif;

      /End-Free
     C                   Endsr                                                  BillCheck Subr
     C**************************************************************************
     C*  Process the Premium link from adjustments grid.
     C*
     C     PremLink      Begsr
      /Free

       DisplayPremium(scGrdRRNA:'A');

      /End-Free
     C                   Endsr                                                  PremLink Subr
     C**************************************************************************
     C*  Process the Edit link from adjustments grid.
     C*
     C     EditLink      Begsr
      /Free
       If xxdivc = 1;
          //*** Begin Del ***  07/12/24  ********************************
          //*** arrDt = DA;
          //*** End   Del ***  07/12/24  ********************************
          Select;
          When scGrdARTY = 'P';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = PA;
          When scGrdARTY = 'T';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = D1;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = P1;
          When scGrdARTY = '1';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA1;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = A1;
          When scGrdARTY = '2';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA2;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = A2;
          When scGrdARTY = '3';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA3;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = A3;
          When scGrdARTY = '4';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA4;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = A4;
          When scGrdARTY = '5';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA5;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = A5;
          When scGrdARTY = '6';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA6;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = A6;
          When scGrdARTY = '7';
             //*** Begin Add ***  07/12/24  ********************************
             arrDt = DA7;
             //*** End   Add ***  07/12/24  ********************************
             arrAm = A7;
          Endsl;
       Else;
          Chain(n) scGrdRRN pea0p;
          arrAM = AR;
          SCAA13 = 0;
          y = 1;
          Dow y <= 12;
             srdti = j1d(y);
             Exsr Dateto8;
             Monitor;
                arrDt(y) = %int(srdto);
             On-error;
                arrDt(y) = 0;
             Endmon;
             y = y + 1;
          Enddo;
          arrDt(13) = 0;
       Endif;

       scEdtTitle = 'Adjustments - ' + %trim(scGrdType);
       scTot = %xfoot(arrAm);
       Exsr ClearEditErr;

       Dou editError = *off or *inkl = *on;

          Exfmt rcdBillEdt;
          Exsr ClearEditErr;

          If *inkl = *off;
              Exsr EditAdjChk;
          Endif;

       Enddo;

       If *inkl = *off;
          If xxdivc = 1;
             //*** Begin Del ***  07/12/24  ********************************
             //*** DA = arrDt;
             //*** End   Del ***  07/12/24  ********************************
             Select;
             When scGrdARTY = 'P';
                //*** Begin Add ***  07/12/24  ********************************
                DA = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                PA = arrAm;
             When scGrdARTY = 'T';
                //*** Begin Add ***  07/12/24  ********************************
                D1 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                P1 = arrAm;
             When scGrdARTY = '1';
                //*** Begin Add ***  07/12/24  ********************************
                DA1 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                A1 = arrAm;
             When scGrdARTY = '2';
                //*** Begin Add ***  07/12/24  ********************************
                DA2 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                A2 = arrAm;
             When scGrdARTY = '3';
                //*** Begin Add ***  07/12/24  ********************************
                DA3 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                A3 = arrAm;
             When scGrdARTY = '4';
                //*** Begin Add ***  07/12/24  ********************************
                DA4 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                A4 = arrAm;
             When scGrdARTY = '5';
                //*** Begin Add ***  07/12/24  ********************************
                DA5 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                A5 = arrAm;
             When scGrdARTY = '6';
                //*** Begin Add ***  07/12/24  ********************************
                DA6 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                A6 = arrAm;
             When scGrdARTY = '7';
                //*** Begin Add ***  07/12/24  ********************************
                DA7 = arrDt;
                //*** End   Add ***  07/12/24  ********************************
                A7 = arrAm;
             Endsl;
          Else;
             Chain scGrdRRN pea0p;
             AR = arrAM;
             y = 1;
             Dow y <= 12;
                srdti = arrDt(y);
                Exsr Dateto7;
                Monitor;
                   j1d(y) = %int(srdto);
                On-error;
                   j1d(y) = 0;
                Endmon;
                y = y + 1;
             Enddo;
             Update pea0p;
          Endif;
       Endif;

      /End-Free
     C                   Endsr                                                  EditLink Subr
     C**************************************************************************
     C*  Clear errors for adjustment edit window.
     C*
     C     ClearEditErr  Begsr
     C
     C                   Move      *off          editError
     C                   Move      *off          *in20
     C                   Move      *off          *in21
     C                   Move      *off          *in22
     C                   Move      *off          *in23
     C                   Move      *off          *in24
     C                   Move      *off          *in25
     C                   Move      *off          *in26
     C                   Move      *off          *in27
     C                   Move      *off          *in28
     C                   Move      *off          *in29
     C                   Move      *off          *in30
     C                   Move      *off          *in31
     C                   Move      *off          *in32
     C                   Move      *off          *in33
     C                   Move      *off          *in38
     C                   Move      *off          *in39
     C                   Move      *off          *in40
     C                   Move      *off          *in41
     C                   Move      *off          *in42
     C                   Move      *off          *in43
     C                   Move      *off          *in44
     C                   Move      *off          *in45
     C                   Move      *off          *in46
     C                   Move      *off          *in47
     C                   Move      *off          *in48
     C                   Move      *off          *in49
     C                   Move      *off          *in53
     C                   Move      *off          *in68
     C                   Move      *off          *in69
     C                   Move      *off          *in70
     C                   Move      *off          *in71
     C                   Move      *off          *in72
     C                   Move      *off          *in73
     C                   Move      *off          *in74
     C                   Move      *off          *in75
     C                   Move      *off          *in76
     C                   Move      *off          *in77
     C                   Move      *off          *in78
     C                   Move      *off          *in79
     C                   Move      *off          *in82
     C
     C                   Endsr                                                  ClearErrors Subr
     C**************************************************************************
     C*  Edit check the adjustments window.
     C*
     C     EditAdjChk    Begsr
     C*
     C*  EDIT ENTERED DATES
     C*  DATE AND AMOUNT FIELD ARE MUTUALLY INCLUSIVE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   Z-ADD     XXCURD        XXCURR            7 0
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
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*
     C     1             DO        12            Y                 3 0
     C     arrDt(Y)      IFNE      *ZERO
     C*
     C                   Z-ADD     arrDt(Y)      SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         arrDt(Y)
     C*
     C     SRERR         IFEQ      '1'
     C     Y             ADD       19            X
     C                   MOVE      '1'           *IN(X)
     C                   Eval      editError = *on
     C                   END
     C                   END
     C*
     C     arrDt(Y)      IFNE      0
     C     arrAm(Y)      ANDEQ     0
     C     arrDt(Y)      OREQ      0
     C     arrAm(Y)      ANDNE     0
     C     Y             ADD       37            X
     C                   MOVE      '1'           *IN(X)
     C                   Eval      editError = *on
     C                   END
     C*
     C*  ADJUSTMENT POSTING DATES MUST BE GREATER THAN OR EQUAL TO THE
     C*  CURRENT SYSTEM DATE (I.E. NO "BACK-POSTING" OF ADJ ALLOWED)
     C*
     C     arrDt(Y)      IFNE      *ZEROS
     C*
     C                   Z-ADD     arrDt(Y)      SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWK60
     C*
     C     XXWK60        IFLT      XXCURR
     C     Y             ADD       67            X
     C                   MOVE      '1'           *IN(X)
     C                   Eval      editError = *on
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  TOTAL MUST MATCH CALCULATED ADJUSTMENT
     C*
     C                   XFOOT     arrAm         XXTEST           11 2
     C*
     C     XXTEST        IFNE      SCTOT
     C     *IN62         ANDEQ     *OFF
     C                   MOVE      '1'           *IN32
     C                   Eval      editError = *on
     C                   END
     C*
     C     arrDt(13)     IFNE      *ZERO
     C*
     C                   Z-ADD     arrDt(13)     SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         arrDt(13)
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN(33)
     C                   Eval      editError = *on
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     arrDt(13)     IFNE      0
     C     arrAm(13)     ANDEQ     0
     C     arrDt(13)     OREQ      0
     C     arrAm(13)     ANDNE     0
     C                   MOVE      '1'           *IN(53)
     C                   Eval      editError = *on
     C                   END
     C*
     C*  ADJUSTMENT POSTING DATES MUST BE GREATER THAN OR EQUAL TO THE
     C*  CURRENT SYSTEM DATE (I.E. NO "BACK-POSTING" OF ADJ ALLOWED)
     C*
     C     arrDt(13)     IFNE      *ZEROS
     C*
     C                   Z-ADD     arrDt(13)     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWK60
     C*
     C     XXWK60        IFLT      XXCURR
     C                   MOVE      '1'           *IN(82)
     C                   Eval      editError = *on
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Endsr                                                  EditAdjChk Subr
     C**************************************************************************
     C     DateCheck     Begsr
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   Endsr                                                  DateCheck Subr
     C     DateTo7       Begsr
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   Endsr                                                  DateTo7 Subr
     C     DateTo8       Begsr
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   Endsr                                                  DateTo8 Subr
     C**************************************************************************
     C*** end add *****  05/14/20  *****************************************************
     OPEA01     E            PEA01E
     OPXA01     E            PXA01E
      *================================================================
      * Skip Deposit Adjustment if renewal year exists and is flagged
      *  to adjust deposit
      *================================================================
     p SkipDepAdj      b
     d SkipDepAdj      pi              n
     d wasOpened       s               n
     d skipDepCalc     s               n
      /free
       If not %open(WDELL);
          Open Wdell;
          wasOpened = *on;
       Endif;
       xxColt = ' ';
       Open wmcol;
       Chain(n) (XXCO#:XXFND:scEMP#) PCO00;
       If %found;
          xxColt = 'Y';
       Endif;
       Close wmcol;
       //Look for renewal year
       Setgt  (xxCo#:xxFnd:SCemp#:xxDiv) pel01;
       Readpe (xxCo#:xxFnd:SCemp#:xxDiv) pel01;
       Chain (ELco#:%editc(ELbscd:'X')) pbs01;
       Chain (xxCo#:xxFnd:ELfyr) pf101;
       Select;
       When xxColt = 'Y';
          skipDepCalc = *on;
       When ELfyr = xxFyr;  // There is no renewal year.
          skipDepCalc = *off;
       When F1bvfl <> 'Y' and F1brfl <> 'Y'; // The renewal year doesn't adjust deposit.
          skipDepCalc = *off;
       When BSsefl = 'S'; // renewal year is self reporting
          skipDepCalc = *on;
       When BSsefl = 'E' and (BSesdp <> 0 or BSesd2 <> 0); // renewal year is est with deposit
          skipDepCalc = *on;
       Endsl;
       // Reset files to current year being processed
       Chain (xxCo#:xxFnd:SCemp#:xxDiv:xxFyr) pel02;
       Chain (ELco#:%editc(ELbscd:'X')) pbs01;
       Chain (xxCo#:xxFnd:xxFyr) pf101;
       If wasOpened = *on;
          Close Wdell;
       Endif;
       Return skipDepCalc;
      /end-free
     p SkipDepAdj      e

     P* --------------------------------------------------
     P* Procedure name: LiveCheck
     P* Purpose:        Check to see if the files have been updated
     P*                 since the beginning of the revised bill process.
     P* Returns:
     P* --------------------------------------------------
     P LiveCheck       B
     D LiveCheck       PI              N

     D epWasOpen       S               N
     D elWasOpen       S               N

      /Free

         Clear saveLiveDT;
         Clear saveLiveTM;

         If not %open(WAELL1);
            Open WAELL1;
            Chain(N) (xxCo#:xxFnd:xxFyr:SCemp#) WAELL1;
            Close WAELL1;
         Else;
            Chain(N) (xxCo#:xxFnd:xxFyr:SCemp#) WAELL1;
         Endif;

         saveLiveDT = ELrdt;
         saveLiveTM = ELrtm;

         //  Check for updates to employer detail records.

         If %Open(wdell1);
            elWasOpen = *on;
         Else;
            elWasOpen = *off;
            Open wdell1;
         Endif;

         Setll (xxCo#:xxFnd:xxFyr:SCemp#) wdell1;
         Reade(N) (xxCo#:xxFnd:xxFyr:SCemp#) wdell1;

         Dow not %EOF(wdell1);

            If (saveLiveDt < ELdt) or (saveLiveDt = ELdt
                                and saveLiveTm < ELTime);
               If not elWasOpen;
                  Close wdell1;
               Endif;
               saveUser = ELuser;
               Return *on;
            Endif;

            Reade (xxCo#:xxFnd:xxFyr:SCemp#) wdell1;

         Enddo;
         If not elWasOpen;
            Close wdell1;
         Endif;

         //  Check for updates to payroll records.
         If %Open(wmepl1);
            epWasOpen = *on;
         Else;
            epWasOpen = *off;
            Open wmepl1;
         Endif;

         Setll (xxCo#:xxFnd:xxFyr:SCemp#) wmepl1;
         Reade(N) (xxCo#:xxFnd:xxFyr:SCemp#) wmepl1;

         Dow not %EOF(wmepl1);

            If (saveLiveDt < EPdt) or (saveLiveDt = EPdt
                                   and saveLiveTm < EPTime);
               If not epWasOpen;
                  Close wmepl1;
               Endif;
               saveUser = EPuser;
               Return *on;
            Endif;

            Reade (xxCo#:xxFnd:xxFyr:SCemp#) wmepl1;

         Enddo;

         If not epWasOpen;
            Close wmepl1;
         Endif;

         return *off;

      /End-Free
     P LiveCheck       E

     P* --------------------------------------------------
     P* Procedure name: clearTemp
     P* Purpose:        Clear temporary files.
     P* Returns:
     P* --------------------------------------------------
     P clearTemp       B
     D clearTemp       PI

      /Free

         If not %open(WAELL1);
            Open WAELL1;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAELL1;
            Close WAELL1;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAELL1;
         Endif;

         If not %open(WAEPL1);
            Open WAEPL1;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAEPL1;
            Close WAEPL1;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAEPL1;
         Endif;

         If not %open(WADSL);
            Open WADSL;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WADSL;
            Close WADSL;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WADSL;
         Endif;

         If not %open(WAILL);
            Open WAILL;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAILL;
            Close WAILL;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAILL;
         Endif;

         If not %open(WAMPL);
            Open WAMPL;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAMPL;
            Close WAMPL;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAMPL;
         Endif;

         If not %open(WAPPL);
            Open WAPPL;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAPPL;
            Close WAPPL;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAPPL;
         Endif;

         If not %open(WALDL2);
            Open WALDL2;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WALDL2;
            Close WALDL2;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WALDL2;
         Endif;

         If not %open(WAEAL1);
            Open WAEAL1;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAEAL1;
            Close WAEAL1;
         Else;
            Delete (xxCo#:xxFnd:xxFyr:SCemp#) WAEAL1;
         Endif;

         return ;
      /End-Free
     P clearTemp       E
     P*--------------------------------------------------
     P* Procedure name: GetLesserOf
     P* Purpose:        Returns lesser of max Deposit Amt
     P*                  and calculated Prem Dep Amt
     P* Parameter:      maxDepAmt
     P* Parameter:      calcPremAmt
     P*--------------------------------------------------
     P GetLesserOf     B
     D GetLesserOf     PI            11  2
     D  maxDepAmt                    11  0 const
     D  calcPremAmt                  11  2 const
      /Free
       Select;
       When maxDepAmt <> 0 and maxDepAmt < calcPremAmt;
          Return maxDepAmt;
       Other;
          Return calcPremAmt;
       Endsl;
      /End-Free
     P GetLesserOf     E
     P*--------------------------------------------------
     P* Procedure name: CalcMaxDeposit
     P* Purpose:        Populate estimated and self report max
     P*                 deposit premium amounts
     P* Returns:
     P* Parameter:      prevMaxDepAmt
     P* Parameter:      currMaxDepAmt
     P* Parameter:      prvMaxDepReq
     P*--------------------------------------------------
     P CalcMaxDeposit  B
     D CalcMaxDeposit  PI
     D  estMaxDepAmt                 11S 0
     D  srMaxDepAmt                  11S 0
     D*** Begin Add *** 05/15/20 ************************
     D  prvMaxDepReq                   n   const options(*nopass)

     D prvDepCap       S                   Like(EMdepCap) Inz
     D*** End   Add *** 05/15/20 ************************
     D depPercent      S                   like(BSab01)
      /Free
       depPercent = GetDepPercent('E');
       If depPercent <> 0;
          Eval(h) estMaxDepAmt = BSmaxDep/depPercent;
       Endif;
       depPercent = GetDepPercent('S');
       If depPercent <> 0;
          Eval(h) srMaxDepAmt  = BSmaxDep/depPercent;
       Endif;
       //*** Begin Add *** 05/15/20 ***********************************
       // If the previous cap is requested then use the previous value
       If %parms > 2 And prvMaxDepReq = *On;
          SetGt  (EMco#:EMfnd:EMemp#:EMdiv:sysDt:sysTm) pemh00;
          ReadPE (EMco#:EMfnd:EMemp#:EMdiv) pemh00;
          If %Eof(wmemphl);
             prvDepCap = 0;
          Else;
             prvDepCap = EMHdepCap;
          EndIf;
       EndIf;
       //*** End   Add *** 05/15/20 ***********************************
       //*** Begin Chg *** 05/15/20 ***********************************
       //*** Select;
       //*** When EMdepcap = 0;
       //*** Other;
       //***    estMaxDepAmt = EMdepcap;
       //***    srMaxDepAmt = EMdepcap;
       //*** Endsl;
       Select;
       When EMdepcap = 0 And prvDepCap = 0;
       When prvDepCap <> 0;
          estMaxDepAmt = prvDepCap;
          srMaxDepAmt  = prvDepCap;
       Other;
          estMaxDepAmt = EMdepCap;
          srMaxDepAmt  = EMdepCap;
       Endsl;
       //*** End   Chg *** 05/15/20 ***********************************
      /End-Free
     P CalcMaxDeposit  E
     P*--------------------------------------------------
     P* Procedure name: GetDepPercent
     P* Purpose:        Returns deposit percentage
     P* Returns:
     P* Parameter:      billPlanType => S or E
     P*--------------------------------------------------
     P GetDepPercent   B
     D GetDepPercent   PI                  like(BSab01)
     D  billPlanType                  1A   const
      /Free
       Select;
       When billPlanType = 'E';
          Select;
          When BSesdp <> 0;
             Return BSesdp/100;
          When BSesd2 <> 0;
             Return BSesd2/100;
          Other;
             Return *zero;
          Endsl;
       When billPlanType = 'S';
          Select;
          When BSab01 <> 0;
             Return BSab01;
          When BSab02 <> 0;
             Return BSab02;
          Other;
             Return *zero;
          Endsl;
       EndSl;
      /End-Free
     P GetDepPercent   E
**  QCAEXEC TABLE
OVRDBF FILE(WDELL) TOFILE(WAELL)                                                 1
OVRDBF FILE(WMEPL1) TOFILE(WAEPL1)                                               2
OVRDBF FILE(WMEPL) TOFILE(WAEPL)                                                 3
DLTOVR FILE(WDELL)                                                               4
DLTOVR FILE(WMEPL1)                                                              5
DLTOVR FILE(WMEPL)                                                               6
OVRDBF FILE(WDDSL) TOFILE(WADSL)                                                 7
DLTOVR FILE(WDDSL)                                                               8
OVRDBF FILE(WDELP) TOFILE(WAELP)                                                 9
OVRDBF FILE(WMEPP) TOFILE(WAEPP)                                                10
DLTOVR FILE(WDELP)                                                              11
DLTOVR FILE(WMEPP)                                                              12
OVRDBF FILE(WMEPL6) TOFILE(WAEPL6)                                              13
DLTOVR FILE(WMEPL6)                                                             14
OVRDBF FILE(WDELL1) TOFILE(WAELL1)                                              15
DLTOVR FILE(WDELL1)                                                             16
OVRDBF FILE(WDILL) TOFILE(WAILL)                                                17
DLTOVR FILE(WDILL)                                                              18
OVRDBF FILE(WDMPL) TOFILE(WAMPL)                                                19
DLTOVR FILE(WDMPL)                                                              20
OVRDBF FILE(WDPPL) TOFILE(WAPPL)                                                21
DLTOVR FILE(WDPPL)                                                              22
OVRDBF FILE(WMEPL1E) TOFILE(WAEPL1E)                                            23
DLTOVR FILE(WMEPL1E)                                                            24
OVRDBF FILE(WMEPL6E) TOFILE(WAEPL6E)                                            25
DLTOVR FILE(WMEPL6E)                                                            26
OVRDBF FILE(WDHCP)  MBR(REVISED) OVRSCOPE(*JOB)            11/10/10             27
DLTOVR FILE(WDHCP)  LVL(*JOB)                              11/10/10             28
OVRDBF FILE(WDHCL)  MBR(REVISED) OVRSCOPE(*JOB)            11/10/10             29
DLTOVR FILE(WDHCL)  LVL(*JOB)                              11/10/10             30
OVRDBF FILE(WDHCL1) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10             31
DLTOVR FILE(WDHCL1) LVL(*JOB)                              11/10/10             32
OVRDBF FILE(WDHCL3) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10             33
DLTOVR FILE(WDHCL3) LVL(*JOB)                              11/10/10             34
ADDPFM FILE(WDHCP) MBR(REVISED)                            11/10/10             35
ADDLFMBRS PHYFILE(WDHCP)                                   11/10/10             36
OVRDBF FILE(WDHCL4) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10             37
DLTOVR FILE(WDHCL4) LVL(*JOB)                              11/10/10             38
OVRDBF FILE(WDHCL5) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10             39
DLTOVR FILE(WDHCL5) LVL(*JOB)                              11/10/10             40
OVRDBF FILE(WDHCL6) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10             41
DLTOVR FILE(WDHCL6) LVL(*JOB)                              11/10/10             42
DLTOVR FILE(WDHCLR) LVL(*JOB)                              11/10/10             43
OVRDBF FILE(WMLDL1) TOFILE(WALDL1)                         08/30/12             44
DLTOVR FILE(WMLDL1)                                        08/30/12             45
OVRDBF FILE(WMLDP) TOFILE(WALDP)                           08/30/12             46
DLTOVR FILE(WMLDP)                                         08/30/12             47
OVRDBF FILE(WMLDL2) TOFILE(WALDL2)                         08/30/12             48
DLTOVR FILE(WMLDL2)                                        08/30/12             49
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
