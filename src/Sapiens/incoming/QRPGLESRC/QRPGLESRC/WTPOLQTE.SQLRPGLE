     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLQTE - Quote Entry Process (Profound)
     H DFTNAME(WTPOLQTE)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H*BNDDIR('WTBNDDIR':'WPIUSRPRMS')
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/25/12                                         *
      *  Programmer: C:065                                            *
      *  Project   : WC2116                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 12/05/25 C:333 Support: 120121 (SCS)                          *
      *                Issue with 'Previous' Button During Endorsement*
      *                Processing.                                    *
      *                - Updates added to determine the final page    *
      *                  based on the configured policy settings,     *
      *                  so the last page does not appear             *
      *                  to customers as an intermediate screen.      *
      *                DSPF changes:                                  *
      *                - Placing Finish/Previous, and Next buttons    *
      *                  inside separate containers for screens       *
      *                  2, 3, 4, 5, 6, 7, 8, and S.                  *
      *                Handling container visibility and position     *
      *                dynamically as per the configuration.          *
      *                                                               *
      * 10/10/25 C:342 Support: 119815                                *
      *                Load previous screen correctly after pressing  *
      *                Previous button                                *
      *                                                               *
      * 09/10/25 C:300 Support: 119663                                *
      *                Carrier Policy # Not Updating on Additional    *
      *                Divisions being added during Policy Entry      *
      *                using 'Save/Add Next' button                   *
      *                                                               *
      * 03/28/25 C:333 Support: 118734 (Vital)                        *
      *                Experience Rating not being Set in GO on Quotes*
      *                from Connect                                   *
      *                                                               *
      * 02/27/25 C:300 Support: 118580 (AmWins)                       *
      *                Copy ARAP Charge (ELARP1/2/3) only to MA Divisions
      *                                                               *
      * 06/24/24 C:300 Project: 146WKR (Prescient)                    *
      *                Collateral Processing                          *
      *                                                               *
      * 06/12/24 C:062 Project: 146WKQ                                *
      *                Recompile                                      *
      *                                                               *
      * 05/31/24 C:342 Support: 115687                                *
      *                Fixed issue of update/write Wmall file         *
      *                when chain with location ID.                   *
      *                                                               *
      * 04/29/24 C:340 Project: WC2965                                *
      *                When creating a new policy copy in the default *
      *                email address for the state reporting division.*
      *                                                               *
      * 03/19/24 C:333 Support: PHW114843                             *
      *                Mid Term Endorsement shows different state     *
      *                than entered                                   *
      *                                                               *
      * 03/04/24 C:079 Project: WC2951                                *
      *                Recompile over WPILOCMST screen changes        *
      *                                                               *
      * 05/03/23 C:342 Support: 107604 (AmWINS)                       *
      *                FEIN error handling when Admin/DARTS is set to *
      *                indicate FEIN is required.                     *
      *                                                               *
      * 04/18/23 C:65  Support: 107397 (Prescient)                    *
      *                Do not check inactive agent history records    *
      *                for duplicate effective date check.            *
      *                                                               *
      * 04/10/23 C:65  Support: 101820                                *
      *                Get policy year for newly added division based *
      *                on existing policy year on employer.           *
      *                                                               *
      * 03/28/23 C:342 Support: 107744 (Sunz)                         *
      *                Recompile over WPILOCMST screen changes        *
      *                                                               *
      * 12/12/22 C:344 Support: 103411                                *
      *                Correct issue occurring when adding a division *
      *                to a multi-division policy;                    *
      *                The data from Screen2 (Rating Information)     *
      *                ex. Experience Modifiers, Increased Limits, etc*
      *                are not being retained when the next Subsequent*
      *                divisions are displayed.                       *
      *                                                               *
      * 11/16/22 C:342 Support: 101820                                *
      *                Get policy year from WDF1L file.               *
      *                                                               *
      * 09/22/22 C:342 Support: 101820                                *
      *                - Get policy year for newly added division.    *
      *                                                               *
      * 09/21/22 C:333 Support: 103693(150)                           *
      *                - Quote tab is closing after entry is complete.*
      *                                                               *
      * 07/26/22 C:062 Support: 102960(148)                           *
      *                Alow add of peo master record.                 *
      *                                                               *
      * 06/08/22 C:291 Support: 88878                                 *
      *                Delete link on Billing Information screen      *
      *                should delete division. Delete link on all     *
      *                other screens should delete the Policy year.   *
      *                                                               *
      * 03/24/22 C:340 Project: WC2858                                *
      *              - Display Screens based on Control setup on WTPQP*
      *                                                               *
      * 01/25/22 C:136 Support: 98291 (Rework WC2685)                 *
      *              - Don't submit renewal when Add Division is      *
      *                cx'd.                                          *
      *              - Put WC2685 logic in a procedure.               *
      *              - Pass division as a parm in call to renewal     *
      *                pgm when adding a division so that renewal     *
      *                processes for the new division only.           *
      *              - Correct logic for quote renewal.               *
      *                                                               *
      * 08/24/21 C:291 Project: WC2812                                *
      *                Removed logic to default Rating factors.       *
      *                New procedure WPIEMPLR.DefaultRatingFactors    *
      *                will be called from Location Master to default *
      *                Rating Factors.                                *
      *                                                               *
      * 05/04/21 C:180 Project WC1986                                 *
      *                Log changes to 3rd party file.                 *
      *                                                               *
      *                Add EFT and 3rd Party grids via                *
      *                WPILOCMST/DspBillDiv                           *
      *                No code changes in this source WTPOLQTE.       *
      *                                                               *
      * 04/20/21 C:136 Project: 150WEX (Removed - Replaced by WC2812) *
      *                Prevent factors from copying from st rpt div   *
      *                to new MCP division or policy                  *
      *                                                               *
      * 04/06/21 C:300 Support: 94604 (SUNZ)                          *
      *                Policy rating screen allows old deductible     *
      *                descriptions to be selected.                   *
      *                Recompile over WTPOLDTL DSPF changes           *
      *                                                               *
      * 03/30/21 C:318 Support:94884 (Replaced by logic in 150WEX)    *
      *                Added edits for MCP policy if PEO doesn't have *
      *                a year for it                                  *
      *                                                               *
      * 11/18/20 C:180 Project WC2757                                 *
      *                Recompiled only.                               *
      *                                                               *
      * 05/05/20 C:136 Project: WC2187 (Lock Policy)                  *
      *                Add locking functionality for Add Division     *
      *!!!!!!!!!!!! Ensure all pieces of this project go together for EBF's !!!!!!!!!
      *                                                               *
      * 04/08/20 C:318 Support: 87222                                 *
      *                Removed line to ensure default discount(eldisc)*
      *                should be correct for all divisions            *
      *                                                               *
      * 03/27/20 C:65  Project: 88242 (SCS)                           *
      *                Chain before writing vendor history to prevent *
      *                new procedure from writing duplicates.         *
      *                                                               *
      * 03/03/20 C:298 Project WC2231                                 *
      *          C:33  Modify Policy Eff Date                         *
      *                                                               *
      * 01/21/20 C:180 Project WC2702                                 *
      *                Replace subprocedure PayrollEdit with          *
      *                EstPyrEdit.                                     *
      *                                                               *
      * 10/09/19 C:298 Project: WC2685                                *
      *                Mid-Term Add of Division on Cancelled Policy.  *
      *                                                               *
      * 09/06/19 C:33  Support: 82767                                 *
      *                Ensure quote status is not blank               *
      *                                                               *
      * 08/08/19 C:300 Support: 81577 (AmWins)                        *
      *                Policy date range did not populate on Billing  *
      *                Division Mater ADD screen when creating a new MCP quote*
      *                                                               *
      * 07/26/19 C:136 Support: 80798                                 *
      *                Supplement to 79239. Add Vendor History when   *
      *                in Quote.                                      *
      *                                                               *
      *  07/23/19 C:65  Project: SY407                                *
      *                 If cancelling a quote/policy add then clean   *
      *                 up the WMP3P records that may have been added.*
      *                                                               *
      * 06/07/19 C:136 Support: 79239                                 *
      *                Add logic to default vendor history            *
      *                for Policy Add and Add Division mode.          *
      *                                                               *
      * 03/20/19 C:062 Support: SY410                                 *
      *                Initialize Quote Status Fields.                *
      *                                                               *
      * 01/11/19 C:267 Support: 50562                                 *
      *                Revise klist for WXDA1L work file.             *
      *                                                               *
      * 10/23/18 C:267 Support: 50562                                 *
      *                Added new file WXDA1P/WXDA1L to store          *
      *                newly added division# for use by STLETTER      *
      *                to print darts remarks accordingly.            *
      *                                                               *
      * 10/01/18 C:291 Project: WC2457                                *
      *                Allow user-defined employer # and division #.  *
      *                Added new popup window rcdEmpPop.              *
      *                                                               *
      * 09/21/18 C:291 Support: 73446                                 *
      *                Fix Policy expiration date to auto-populate    *
      *                on Location master screen.                     *
      *                                                               *
      * 08/21/18 C:33  Support: 72421                                 *
      *                Ensure a duplicate agent history record is not *
      *                 added.                                        *
      *                                                               *
      * 07/12/18 C:291 Project: 148WAV                                *
      *                Default deviations on add.                     *
      *                                                               *
      * 06/22/18 C:180 Support: 63353                                 *
      *                Fix duplicate fein listing link - DSPF change  *
      *                only.                                          *
      *                                                               *
      *  06/05/18 C:33  Support: 71381                                *
      *                -Default agent history record on division add. *
      *                                                               *
      *  04/10/18 C:33  Project: 150WCF                               *
      *                 Default data on add.                          *
      *                                                               *
      *  02/01/18 C:65  Project: 150WAD/Support                       *
      *                 Write excluded states records when creating   *
      *                 a new quote/policy.                           *
      *                                                               *
      * 12/06/17 C:33  Support: 51525                                 *
      *               -On add of division                             *
      *                - Do not allow addition of second division     *
      //                                                               *
      // 11/08/17 C:33  Support: 50956                                 *
      //                Default carrier policy # on add mode           *
      *                                                               *
      *  10/13/17 C:65  Project: 150WBU                               *
      *                 Default all client 150 quotes/policies with   *
      *                 A/R summary.                                  *
      *                                                               *
      *  09/08/17 C:65  Project: 150WAR                               *
      *                 Default % of manual premium from Group Year   *
      *                 for client 150 policies/divisions.            *
      *                                                               *
      *  04/07/17 C:062 Project 136WHF                                *
      *                 Add Territory Rating for CareWest.            *
      *                                                               *
      *  03/23/17 C:180 PhaseWare: 46342 Fix infinite loop on         *
      *                            Clearance Pop Up.                  *
      *                                                               *
      *  02/21/17 C:65  PhaseWare: 45524 (71WHU)                      *
      *                 If the add default location flag is set to a  *
      *                 'Y' value then add the location with a 'L'.   *
      *                                                               *
      *  04/14/16 C:136 PhaseWare: 37786                              *
      *                 Correct default of waiver logic               *
      *                                                               *
      *  12/18/15 C:33  38911                                         *
      *                 Ensure Quote Tracking informaiton matches     *
      *                  associated division.                         *
      *                                                               *
      *  08/18/15 C:306 Support:36491                                 *
      *                 (Recompilation over the WTPOLQTE dispaly file)*
      *                 Added the Agent Rate Description              *
      *                                                               *
      *  06/05/15 C:308 Support:35175                                 *
      *                 Shift Modify Effective date box upwards       *
      *                                                               *
      *  06/04/15 C:242 Support:29590,32246,33110,33246,84803         *
      *                 Expand 84803 for all rating values.           *
      *                                                               *
      *  10/27/14 C:291 Support: WA099 84803                          *
      *                 Populate rating information for additional    *
      *                 divisions based on State Reporting Division.  *
      *                                                               *
      * 10/14/14 C:136 Support: WA129 86772                           *
      *                Map the prior carrier name and prior premium   *
      *                fields to WAQTP.                               *
      *                                                               *
    *  07/14/14 C:267 Redmine: 4211                                 *
    *                 Updated UI changing the 'CHOICE VALUE FIELD'  *
    *                 for Division field to include the following:  *
    *                 DIGITS(FNFND). Issue with Division field      *
    *                 selection value reverting to BLANKS.          *
    *                 Compile only.                                 *
    *                                                               *
    *  06/16/14 C:136 Support: WA114 87853                          *
    *                 Change javascript associated with group drop  *
    *                 down and policy inception date fields in      *
    *                 the rcdQuotePop window. Compile only.         *
    *                                                               *
      *  04/19/14 C:242 SUPPORT WA099 85246                           *
      *                 Filter group based on User                    *
      *                                                               *
      *  02/19/14 C:282 Project: 136WFZ Priss #14                     *
    *                 When Add Division from the toobar is          *
      *                 selected the mode is set to 'N'.              *
      *                 Changed the mode to 'C' if mode is 'N'.       *
    *                                                               *
      *  01/24/14 C:279 Project: SY336 Priss #29                      *
      *                 Modified to not display inactive agent and    *
      *                 producer.                                     *
      *                                                               *
      *  01/08/14 C:283 Support: WA146 84442                          *
      *                 Write the Status as "Written" when an Employer*
      *                 is created from Policy Entry.                 *
      *                                                               *
    *  08/23/13 C:279 Redmine: 3378                                 *
    *                 Modified to use CreateBigQteURL for quote     *
    *                 env.                                          *
    *                                                               *
    *  08/19/13 C:267 Support: Redmine #1180 (Haifa Kealy)          *
    *                 UI - Added two new columns - Agency Name and  *
    *                 Producer Name. Seperated name and number for  *
    *                 Agency and Producer; created two new fields.  *
    *                                                               *
    *  07/15/13 C:65  Redmine: 3378                                 *
    *                 Change WTSEARCH launch to pass a 'Q' for      *
    *                 mode if called from quote environment, to     *
    *                 prevent the quote library from being in the   *
    *                 new session's library list.                   *
    *                                                               *
    *  06/13/13 C:65  Redmine: 3150                                 *
    *                 Default safety fees.                          *
    *                                                               *
      *  05/21/13 C:279 Redmine: 2967                                 *
      *                 Modified the Delete Location Logic.           *
      *                                                               *
    *  05/20/13 C:136 Project: SY307                                *
    *                 Implement application security                *
    *                                                               *
    *  05/06/13 C:279 Redmine: 2746                                 *
    *                 Modified to default Waiver fee and charges.   *
    *                                                               *
    *  05/06/13 C:279 Redmine: 2747                                 *
    *                 Modified to default Membership fee.           *
    *                                                               *
      *  04/08/13 C:279 Redmine: 2643                                 *
      *                 Added edit for agent and producer.           *
      *                                                               *
      *  03/06/13 C:136 Redmine: 2516                                 *
      *                 Populate ELNWRW (Renewal Flag) on policy year *
      *                 add                                           *
      *                                                               *
      *  01/22/13  C:277  PROJECT: 133WNZ                             *
      *                   Add Processing for new field flgNBquote     *
     *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     FWTPOLQTE  CF   E             WORKSTN extfile('WTPOLQTE')
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(div1SFL:rrnDiv1)
     f                                     Sfile(div2SFL:rrnDiv2)
     f                                     Sfile(div3SFL:rrnDiv3)
     f                                     Sfile(div4SFL:rrnDiv4)
     f                                     Sfile(div5SFL:rrnDiv5)
     f                                     Sfile(div6SFL:rrnDiv6)
     f                                     Sfile(pay6SFL:rrnPayroll)
     f                                     Sfile(agt7SFL:rrnAgt7)
     f                                     Sfile(div8SFL:rrnDiv8)
     f                                     Sfile(loc8SFL:rrnLocatio)
     f                                     Sfile(div9SFL:rrnDiv9)
     f                                     Sfile(own9SFL:rrnOwner)
     f                                     Sfile(divSSFL:rrnDivS)
     fwtpoldtl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPayFilt)
     fwpipolpop cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtMsg)

     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     UF A E           K DISK    rename(PEL01:PEL00)
     F                                     infds(dbfds)
     FWDELL1    UF   E           K DISK    rename(PEL01:PEL01)
     F                                     infds(dbfds7)
     FWDELL5    UF   E           K DISK    rename(PEL01:PEL05) prefix(DV:2)
     FWDF1L2    IF   E           K DISK    rename(PF101:PF102) prefix(L2)
     FWDELL8    UF   E           K DISK    rename(PEL01:PEL08) prefix(E2:2)
     F*** begin add ***  04/10/23  ******************************************
     FWDELL7    IF   E           K DISK    rename(PEL01:PEL07) prefix(E3:2)
     F*** end add *****  04/10/23  ******************************************
     F*** Beg Add ***  09/10/25  ******************************************
     FWDELL2    IF   E           K DISK    rename(PEL01:PEL02) prefix(E4:2)
     F                                     infds(dbfds8)
     F*** End Add ***  09/10/25  ******************************************
     FWMDSL3    IF   E           K DISK    rename(PDS01:PDS03)
     FWTDDL2    IF   E           K DISK    rename(PDD01:PDD02)
     FWTLML1    IF   E           K DISK    rename(PLM01:PLM01)
     FWMEMP     UF A E             DISK    rename(PEM01:PEM0P)
     FWMEML     UF A E           K DISK    rename(PEM01:PEM00)
     F                                     infds(dbfds2)
     FWMEML12   IF   E           K DISK    rename(PEM01:PEM12)
     FWMEPLE    IF   E           K DISK    rename(PEP01:PEP01)
     F                                     infds(dbfds3)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPR01)
     FWMPXL1    IF   E           K DISK    rename(PPX01:PPX01)
     FWMNBL     UF A E           K DISK    rename(PNB01:PNB00)
     FWMAHL     UF A E           K DISK    rename(PAH01:PAH00)
     F                                     infds(dbfds4)
     FWMAHP     IF   E             disk    rename(pah01:pah0p)
      *** Beg Chg ***** 03/19/24 *******************************
     F*WMALL     IF A E           K DISK    rename(PAL01:PAL00)
     FWMALL     UF A E           K DISK    rename(PAL01:PAL00)
      *** End Chg ***** 03/19/24 *******************************
     F                                     infds(dbfds5)
     FWMOOL     IF   E           K DISK    rename(POO01:POO00)
     F                                     infds(dbfds6)
     FWMARL     IF   E           K DISK    rename(PAR01:PAR00)
     FWMAAL1    IF   E           K DISK    rename(PAA01:PAA01)
     FWMFNL     uF   E           K DISK    rename(PFN01:PFN00)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)
     FWMRSL     IF   E           K DISK    rename(PRS01:PRS00)
     FWDRRL     IF   E           K DISK    rename(PRR01:PRR00)
     FWAQTL7    UF A E           K DISK    rename(PQT01:PQT07)
     FWTQSL1    IF   E           K DISK    rename(PQS01:PQS01)
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX00)
     FWDFHL     IF   E           K DISK    rename(PFH01:PFH00)
     FWMEAL8    IF   E           K DISK    rename(PEA01:PEA08)
     FWZEXL     IF   E           K DISK    rename(PEX01:PEX02)
     FWASRL2    IF   E           K DISK    rename(PSR01:PSR00)
     FWMACL2    IF   E           K DISK    rename(PAC01:PAC00)
     FWDXSL     IF A E           K DISK    rename(pxs01:pxs00)
     FWDVHL1    UF   E           K DISK    Rename(pvh01:pvh01) UsrOpn
     FWDVHP     O    E             DISK    rename(pvh01:pvh0p)
     FWDVHL     IF   E           K DISK    Rename(pvh01:pvh00)
     F                                     prefix(a_)
      *
      * New Division# - workfile
     FWXDA1L    IF   E           K DISK    RENAME(PDA01:PDA1L)
     FWXDA1P    UF A E             DISK    RENAME(PDA01:PDA1P)
     FWMP3L     UF   E           K DISK    rename(pp301:pp300)
      *
     FWTABL     IF   E           K DISK
     F                                     ignore(PRT01)
     FSSYLF     IF   E           K disk
     F                                     ignore(SSY01)
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
     f*** Beg Add ***** 06/08/22 *****************************
     fwdxhl2    if   e           k disk    Rename(pxh01:pxh02)
     f*** End Add ***** 06/08/22 *****************************
      *** Beg Del ***** 08/24/21 ***************************************
     F*WMEMLQ    IF   E           K DISK    rename(PEM01:PEM0Q) UsrOpn
     F*WDELLQ    IF   E           K DISK    rename(PEL01:PEL0Q) UsrOpn
      *** End Del ***** 08/24/21 ***************************************
       //*** beg chg ***** 01/25/22 ***********************
     f***wmshp     o    e             disk    rename(psh01:psh0p)
     f***wmshl1    if   e           k disk    rename(psh01:psh01)
     f***wmenl3    if   e           k disk    rename(pen01:pen03)
       Dcl-F wmshl1 Usage(*Update: *Output) Keyed;
       Dcl-F wtgtl Usage(*Input) Keyed Rename(pgt01:pgt00);
       //*** end chg ***** 01/25/22 ***********************
      *** Beg Add ***** 03/24/22 **************************
       Dcl-F WTPQL Usage(*Input) Keyed  ;
      *** End Add ***** 03/24/22 **************************

      *** Beg Chg ***** 12/12/22 **************************
     D***PSDS          ESDS                  extname(SZQ1P)
     D*** dbfds           ds
     D*** dbrrn                397    400b 0
     D*** dbfds2          ds
     D***  dbrrn2               397    400b 0
     D*** dbfds3          ds
     D*** dbrrn3               397    400b 0
     D*** dbfds4          ds
     D***  dbrrn4               397    400b 0
     D*** dbfds5          ds
     D***  dbrrn5               397    400b 0
     D*** dbfds6          ds
     D*** dbrrn6               397    400b 0
     D*** dbfds7          ds
     D*** dbrrn7               397    400b 0
     D PSDS          ESDS                  extname(SZQ1P)
     D dbfds           ds
     D  dbrrn                397    400i 0
     D dbfds2          ds
     D  dbrrn2               397    400i 0
     D dbfds3          ds
     D  dbrrn3               397    400i 0
     D dbfds4          ds
     D  dbrrn4               397    400i 0
     D dbfds5          ds
     D  dbrrn5               397    400i 0
     D dbfds6          ds
     D  dbrrn6               397    400i 0
     D dbfds7          ds
     D  dbrrn7               397    400i 0
      *** End Chg ***** 12/12/22 **************************
      *** Beg Add ***  09/10/25  ******************************************
     D dbfds8          ds
     D  dbrrn8               397    400i 0
      *** End Add ***  09/10/25  ******************************************

     D tone            c                   CONST('0000000001')

     d decDivFrom      s                   like(ELdiv)
     d decDivStRpt     s                   like(ELdiv)
     D dspMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     D agtQteMode      s              1    inz(' ')
     D foundAgt        s               n   inz(*off)
     D foundQte        s               n   inz(*off)
     d newDiv#         s              5  0 inz
     D process         s               n   inz(*off)
     d strStRptQTtype  s                   like(QTtype)
     D isModEffDt      s               n   inz(*on)
     d cxDate          s              7  0
     d qteMode         s               n   inz(*off)
     D xxEfDt          s                   like(ELPFDT)
     D xxDate          s              8  0
     D xxPxSt          s              2
     D xxKey           s                   like(bskey)
     D xxRate          s              3  0
     D xxAgent         s                   like(aaagt#)
     D xxELID          s                   like(alelid)
     d xxRrn           s              9  0
     d xxMRrn          s              9  0
     d emRrn           s              9  0
     d elRrn           s              9  0
     d ahRrn           s              9  0
     d x#Rrn           s              9  0
     D xxCall          s              3    inz('QTE')
     d screen#         s              2  0
     d sysDt7          s              7  0
     d sysDt8          s              8  0
     D compName        s             40
     D fundName        s             40
     D secType         s              1
     D secLevel        s              3  0
     D intCo#          s              3  0
     D intFnd          s              3  0
     D screenIndex     s              5  0 inz(0)
     D errorPop        s               n   inz(*off)
     D error           s               n   inz(*off)
     D edit            s               n   inz(*off)
     D first_add       s               n   inz(*on)
     D second_add      s               n   inz(*on)
     D third_add       s               n   inz(*on)
     D fourth_add      s               n   inz(*on)
     D fifth_add       s               n   inz(*on)
     D sixth_add       s               n   inz(*on)
     D seventh_add     s               n   inz(*on)
     D schedule_add    s               n   inz(*on)
     D second_default  s               n   inz(*on)
     D xxDDcd          s                   like(ELDCDE)
     D xxSt            s              2
     D message         s            100
     D inMode          s              1
     D flgNBquote      s              1    inz('0')
     d isContribution  s               n   inz(*off)
     d XXEND#          s              5P 0
     d*** Begin Add *** 07/26/22 *******************************************
     d isPeoMasterExists...
     d                 s               n   Inz(*Off)
     d*** End   Add *** 07/26/22 *******************************************

     d inCo#           s              3    inz
     d inFnd           s              3    inz
     d inFyr           s              3    inz
     d inType          s              1    inz
     d inPct           s              5    inz
     d inOpt           s              1    inz
     d inIdfl          s              1    inz
     d inFtDt          s              8    inz
     d inTtDt          s              8    inz
     d inTime          s              6    inz
     d inOutQ          s             10    inz
     d inRtoQ          s              1    inz
     d inStIn          s             21    inz
     d inStrp          s              1    inz
     d inEmp#          s              9    inz
      *** beg add ***** 01/25/22 **************************
     d inDiv           s              5    inz
      *** end add ***** 01/25/22 **************************
     d saveFyr         s              3  0 inz
     d fyr             s              3  0 inz
     d intStRptDiv     s              5  0 inz
     d charType        s              1    inz


     d outPRDS         s             40    inz
     d outCALL         s              1    inz
     d outKC           s              1    inz
     d outKL           s              1    inz
     d outCO#          s              3  0 inz
     d outFND          s              3  0 inz
     d outEMP#         s              9  0 inz
     d outDIV          s              5  0 inz
     d outFYR          s              3  0 inz
      *** Beg Add ***** 03/24/22 **************************
     D isFirst1        s               n   inz(*on)
     D isFirst2        s               n   inz(*on)
     D isFirst3        s               n   inz(*on)
     D isFirst4        s               n   inz(*on)
     D isFirst5        s               n   inz(*on)
     D isFirst6        s               n   inz(*on)
     D isFirst7        s               n   inz(*on)
     D isFirst8        s               n   inz(*on)
     D isFirst9        s               n   inz(*on)
     D isFirstS        s               n   inz(*on)
      *** End Add ***** 03/24/22 **************************

      *** Beg Add ***** 12/05/25 *************************************
     D LastWindow      s              2  0
      *** End Add ***** 12/05/25 *************************************
     dstrCommand       s            255
     dQCMDEXC          pr                  ExtPgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     d oWDRRLds        ds                  likerec(prr00)
      *** Beg Add ***** 12/12/22 *************************************
     D dsWDRRLc        ds                  likerec(PRR00:*input) inz
     D dsWDRRLs        ds                  likerec(PRR00:*input) inz
      *** End Add ***** 12/12/22 *************************************
     D                 DS
     D  EATRDT                 1      7  0
     D  xxTRYM                 1      5  0
     D                 DS
     D  EAPODT                 1      7  0
     D  xxPOYM                 1      5  0
     D                 DS
     D  xxxDTE                 1      7  0
     D  xxCUYM                 1      5  0

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
       //*** Begin Add *** 05/04/21****************************************
       Dcl-ds WMP3Lds likerec(pp300);
       //*** End   Add *** 05/04/21 ****************************************
       //*** beg add ***** 01/25/22 ***********************
       Dcl-S idx Zoned(2:0);
       Dcl-S indWrtSH Ind;
       Dcl-Ds dsSHst;
        SHst01 Char(2);
        SHst02 Char(2);
        SHst03 Char(2);
        SHst04 Char(2);
        SHst05 Char(2);
        SHst06 Char(2);
        SHst07 Char(2);
        SHst08 Char(2);
        SHst09 Char(2);
        SHst10 Char(2);
        SHst11 Char(2);
        SHst12 Char(2);
        SHst13 Char(2);
        SHst14 Char(2);
        SHst15 Char(2);
        arySHst Char(2) Dim(15) Pos(1);
       End-Ds;
       Dcl-Ds dsSHss;
        SHss01 Char(1);
        SHss02 Char(1);
        SHss03 Char(1);
        SHss04 Char(1);
        SHss05 Char(1);
        SHss06 Char(1);
        SHss07 Char(1);
        SHss08 Char(1);
        SHss09 Char(1);
        SHss10 Char(1);
        SHss11 Char(1);
        SHss12 Char(1);
        SHss13 Char(1);
        SHss14 Char(1);
        SHss15 Char(1);
        arySHss Char(1) Dim(15) Pos(1);
       End-Ds;
       //*** end add ***** 01/25/22 ***********************
     D****** Beg add ***** 10/10/25 ***********************
     D isPrev          s               n   inz(*off)
     D****** End add ***** 10/10/25 ***********************
      *
     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Program Start
     d ProgramStart    pr
     D processMe                       n

     d* Main Processing
     d Main            pr

     d* Process Clearance Window
     d QuotePopup      pr

     d* Process Screen 1 - Policy
     d Screen1         pr

     d* Process Screen 2 - Rating
     d Screen2         pr

     d* Process Screen 3 - Large Deductible
     d Screen3         pr

     d* Process Screen 4 - Retrospective Rating
     d Screen4         pr

     d* Process Screen 5 - Additional Info
     d Screen5         pr

     d* Process Screen 6 - Payroll Info
     d Screen6         pr

     d* Process Screen 7 - Agent Info
     d Screen7         pr

     d* Process Screen 8 - Additional Locations
     d Screen8         pr

     d* Process Screen 9 - Owner/Officer Info
     d Screen9         pr

     d* Process Screen S - Scheduled Rating
     d ScreenS         pr

      //*** Beg Add ***** 06/08/22 *********************************
     d SrDltChk        pr                  extpgm('W0051R04')
     d ##RRN                          9  0
     d ##QTE                          1
     d ##IND                          2  0

     d SrDltRltFiles   pr                  extpgm('W0051R01')
     d outCO#                         3  0
     d outFND                         3  0
     d outEMP#                        9  0
     d outDIV                         5  0
     d outFYR                         3  0
     d outQTE                         1

     d DeletePolYear   pr
     d inRrn                          9  0 const
      //*** End Add ***** 06/08/22 *********************************

     d* Effective Date
     d SrEffd          pr             7  0
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d inFYR                          3  0 const
     d inEMP#                         9  0 const
     d inDIV                          5  0 const
     d inDate                         7  0
     d inType                         1    options(*nopass)
     d inClas                         5  0 options(*nopass)
     d* SReffd API prototype
     d SReffdAPI       pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5
     d ComAPI          pr                  extpgm('WA001R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##Conm                      40
     d   ##Fnam                      40
     d   ##Lvl                        3  0
     d   ##Type                       1

     d* WriteResubmitEnd
     dWriteReSubEnd    pr

     * Rollover - Submitter.
     d W7810C          pr                   extpgm('W7810C')
     d inCo#                          3
     d inFnd                          3
     d inFyr                          3
     d inType                         1
     d inPct                          5
     d inOpt                          1
     d inIdfl                         1
     d inFtDt                         8
     d inTtDt                         8
     d inTime                         6
     d inOutQ                        10
     d inRtoQ                         1
     d inStIn                        21
     d inStrp                         1
     d inEmp#                         9
     d inDiv                          5
     d WtPolPf         pr                  Extpgm('WTPOLPFDT')
     d  outPRDS                      40A   const
     d  outCALL                       1A
     d  outKC                         1
     d  outKL                         1
     d  outCO#                        3  0
     d  outFND                        3  0
     d  outEMP#                       9  0
     d  outDIV                        5  0
     d  outFYR                        3  0
     d/copy sprLock
     d strJobInfo      s             28
     d strProcess      s             10
     d strProcess25    s             25
     d strUser         s             50
     d xxMsg           s            256    inz
     d xxTtl           s             50    inz
     d xxRsp           s               n   inz
     D/copy sPrDate
     D/copy sPrString
     D/copy wPrAgent
     D/copy wPrProduce
     D/copy wPrGroup
     D/copy wPrRateSet
     d/copy WprEmpLr
     d/copy wprcarrier
     d/copy wprPolPop
     d/copy wprPolPop2
     d/copy wprPolErr
     d/copy wprGenPop
     d/copy wprLocMst
     d/copy WTSearchDs
     d/copy sPrAtrium
     d/copy wPrUsrPrms
     d/copy sPrSecure
      //*** Beg Add ***** 06/08/22 *********************************
     d/copy sPrMessage
      //*** End Add ***** 06/08/22 *********************************
      *** Begin Add *** 05/04/21 ************************************
     d/copy wPrBank
      *** End   Add *** 05/04/21 ************************************
      *** beg add ***** 08/24/21 *********************
     d/copy sPrSql
      *** end add ***** 08/24/21 *********************
     d ChkAppSecurity  pr              n
     d  inAction                     20    Const

     d aCTxRef         s             25    Dim(25) Ctdata Perrcd(1)
     d oFDxRef         s             13    Dim(25) Alt(aCTxRef)
     C*
     C* Group Master File Key List
     C*
     C     keyWMFNL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scDiv
     C*
     C     key1WMEML     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C*
     C     key2WMEML     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    ALDiv
     C*
     C     keyWMEML12    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFein
     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scDiv
     C                   Kfld                    scFyr
     C*
     C     key1WDELL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    EMDiv
     C                   Kfld                    scFyr
     C*
     C     key2WDELL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    AHDiv
     C                   Kfld                    scFyr
     C*
     C     key3WDELL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    ALDiv
     C                   Kfld                    scFyr
     C*
     C     key4WDELL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    ALDiv
     C*
     C     key5WDELL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    EPDiv
     C                   Kfld                    scFyr
     C*
     C     keyWDELL1     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scFyr
     C                   Kfld                    scEmp#
     C*
     C     key1WDELL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scFyr
     C                   Kfld                    scEmp#
     C                   Kfld                    scDiv
     C*
     C* Employer Payroll File Key List
     C*
     C     keyWMEPLE     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C*
     C* Payroll Class File Key List
     C*
     C     keyWMPRL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EPCLAS
     C                   Kfld                    xxEfDt
     C     key1WMPRL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EPCLAS
     C*
     C* Payroll Description File Key List
     C*
     C     keyWMPXL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    EPCLAS
     C                   Kfld                    EPEXT
     C                   Kfld                    xxPxSt
     C                   Kfld                    EPEFDT
     C     key1WMPXL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    EPCLAS
     C                   Kfld                    EPEXT
     C                   Kfld                    xxPxSt
     C*
     C* Agent History File Key List
     C*
     C     keyWMAHL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scDiv
     C                   Kfld                    scFyr
     C*
     C     key1WMAHL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C*
     C     key2WMAHL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    ELDiv
     C                   Kfld                    scFyr
     C*
     C     key3WMAHL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    EMDiv
     C                   Kfld                    scFyr
     C*
     C* Agent Rate File Key List
     C*
     C     keyWMARL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    xxRate
     C                   Kfld                    AHEfdt
     C*
     C     key1WMARL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    xxRate
     C*
     C* Agent Assignment File Key List
     C*
     C     keyWMAAL1     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    xxAgent
     C*
     C* Quote Status File Key List
     C*
     C     keyWAQTL7     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scDiv
     C                   Kfld                    agtQteMode
     C                   Kfld                    AHAgt#
     C*
     C     key1WAQTL7    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    EMDiv
     C                   Kfld                    agtQteMode
     C                   Kfld                    AHAgt#
     C*
     C     key2WAQTL7    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    ELDiv
     C                   Kfld                    agtQteMode
     C                   Kfld                    AHAgt#
     C*
     C* Additional Location File Key List
     C*
     C     keyWMALL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C*
     C     key1WMALL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    EMDiv
     C                   Kfld                    xxELID
     C*
     C* Owner/Officer File Key List
     C*
     C     keyWMOOL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C*
     C* New Business Tracking File Key List
     C*
     C     keyWMNBL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
      *
      * WXDA1L workfile
     C     DAKY          KLIST
     C                   KFLD                    EmCO#
     C                   KFLD                    EmFND
     C                   KFLD                    EmEMP#
     C                   KFLD                    decDivStRpt
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    XXEND#
     C                   KFLD                    EmDIV
     C*
     C* Table File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    xxKey
     C*
     C     keyWMRSL      Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELrtcd
     C*
     C     keyWDRRL      Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    RSjrst
     C                   Kfld                    ELpfdt
     C*
     C     keyWDRRL1     Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    RSjrst
     C*
     C* Admin/Darts File Key List
     C*
     C     keyWMEXL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
     C* Flexible Heading File Key List
     C*
     C     keyWDFHL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C
     c* Entry of Program.
     c*
     C     *ENTRY        PLIST
     C                   PARM                    @@rrn             9            wdelp
     C                   PARM                    @@mode            1            mode
     C                   PARM                    @@Next           10            next pgm call
     C                   PARM                    @@TabInd          5            screen index
     C                   PARM                    @@Qte             1            quote
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     c
     C     *DTAARA       DEFINE    *LDA          SVLDA
      /free

       GetClientID(clientID);

       ProgramStart(process);

       If process;
          Main();
          ClearPolicyLock(ELco#:ELfnd:ELemp#);
       Endif;

       If Q1prms >= 4;
          If btnFinish = *on;
             @@Next = 'WTPOLSUM';
             //*** Beg Add ***** 06/08/22 ***************************
             //*** Beg Del ***** 09/21/22 ***************************
             //***Chain(n) xxRrn pel0p;
             //***If Not %Found(wdelp);
             //***   @@Next = ' ';
             //***Endif;
             //*** End Del ***** 09/21/22 ***************************
             //*** End Chg ***** 06/08/22 ***************************
          Else;
             @@Next = ' ';
          Endif;
          If mteMode = *On;
            Select;
            When btnFinish = *On;
              @@Next = 'NEXT';
            When btnExit = *On;
              @@Next = 'CANCEL';
            Endsl;
          Endif;
       Endif;
       If @@mode = 'N';
          @@mode = 'C';
       EndIf;

       *InLr = *On;
      /end-free
      *
       //***************************************************************************//
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D processMe                       n
     d RtvqteNewRnw    pr                  ExtPgm('WA069C')
     d xxCo#                          3
     d xxFnd                          3
     d xxFyr                          3
     d xxEmp#                         9
     d xxNWRW                         1
     d*
     d xxCo#           s              3
     d xxFnd           s              3
     d xxEmp#          s              9
     d xxFyr           s              3
     d xxNwrw          s              1    inz('0')
      /free

       // Relative Record # for employer detail.
       If q1prms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          addMode=*on;
          qteMode=*on;
          quoteMode = 'Q';
       Endif;

       scQteHdg = 'Quote Entry';
       scClrTitle = 'Quote Entry - Clearance Information';
       mteMode = *Off;
       divAdd = *off;
       // Program Mode.
       If q1prms >= 2;
          Select;
             When @@mode='D';
                dspMode=*on;
             When @@mode='A';
                addMode=*on;
             When @@mode='C';
                chgMode=*on;
             When @@mode='M';
                mteMode=*On;      //Mid-term endorsement mode
                divAdd = *On;
                scQteHdg = 'Addition of Division Endorsement';
                In *Lock *dtaara;
                Out *dtaara;
             When @@mode='N';
                divAdd = *On;
                scQteHdg = 'Addition of Division';
             When @@mode='Y';
                addMode=*on;
                inMode=@@mode;
                xxMRrn = %int(@@rrn);
                Chain(n) xxMRrn pem0p;
          Endsl;
       Endif;

       // Screen Index
       If q1prms >= 4;
          If @@TabInd <> ' ';
             screenIndex = %int(@@TabInd);
          Else;
             screenIndex = 1;
          Endif;
       Else;
          screenIndex = 1;
       Endif;

       // Quote Environment
       qteMode=*off;
       If q1prms >= 5;
          quoteMode = @@qte;
          Select;
             When @@qte='Q';
                qteMode=*on;
          Endsl;
       Endif;

       If quoteMode = 'Q';
          agtQteMode = 'Q';
       Else;
          agtQteMode = 'W';
       Endif;

       If dspMode or addMode or chgMode;
          If qteMode = *on;
             If addMode;
                scQteHdg = 'Quote Entry';
             Else;
                scQteHdg = 'Quote Review';
               isModBtn = *on;
             Endif;
          Else;
             If addMode;
                scQteHdg = 'Policy Entry';
                scClrTitle = 'Policy Entry - Clearance Information';
             Else;
                scQteHdg = 'Policy Review';
             Endif;
          Endif;
       Endif;

       // Default payroll filter to Active only.
       scPayFStat = 'A';

       sysDt7 = DateTo7(SystemDate);
       scSysDt7 = sysDt7;
       sysDt8 = DateTo8(SystemDate);

       If inMode = 'Y';
          intCo# = EMco#;
          intFnd = EMfnd;
       Else;
          intCo# = 0;
          intFnd = 0;
          ComAPI(intCo#:intFnd:compName:fundName:secLevel:secType);
       Endif;

       scCo# = intCo#;
       scFnd = intFnd;

       // Defaults for testing purposes, if user is not set up.
       If scCo# = 0;
          scCo# = 777;
          scFnd = 999;
       Endif;

      *** Beg Add ***** 03/24/22 **************************
      // Get Screen Display Controls
       Chain(n) intCo# PPQ01 ;
       If Not %Found(WTPQL);
          PQCLEARS   = 'Y';
          PQBILLW    = 'Y';
          PQBILLS    = 'Y';
          PQRATEW    = 'Y';
          PQRATES    = 'Y';
          PQLDEDW    = 'Y';
          PQLDEDS    = 'Y';
          PQSCHDW    = 'Y';
          PQSCHDS    = 'Y';
          PQRETRW    = 'Y';
          PQRETRS    = 'Y';
          PQADDW     = 'Y';
          PQADDS     = 'Y';
          PQPAYW     = 'Y';
          PQPAYS     = 'Y';
          PQAGTW     = 'Y';
          PQAGTS     = 'Y';
          PQLOCS     = 'Y';
          PQOFFS     = 'Y';
       //*** Beg Add ***** 12/05/25 **********************************
       Else;
          Clear LastWindow;

          // Determining last window as per Configuration file
          If PQRATEW = 'Y';
             LastWindow = 2;
          Endif;
          If PQSCHDW = 'Y';
             LastWindow = 10;
          Endif;
          If PQLDEDW = 'Y';
             LastWindow = 3;
          Endif;
          If PQRETRW = 'Y';
             LastWindow = 4;
          Endif;
          If PQADDW = 'Y';
             LastWindow = 5;
          Endif;
          If PQPAYW = 'Y';
             LastWindow = 6;
          Endif;
          If PQAGTW = 'Y';
             LastWindow = 7;
          Endif;
          If PQLOCS = 'Y';
             LastWindow = 8;
          Endif;
          If PQOFFS = 'Y';
             LastWindow = 9;
          Endif;
       //*** End Add ***** 12/05/25 **********************************
       EndIf;
      *** End Add ***** 03/24/22 **************************

       flgNBquote = *off;
       Chain xxRrn PEL0P;
       If qteMode = *on;
         xxCo# = %editc(ELco#:'X');
         xxFnd = %editc(ELfnd:'X');
         xxEmp# = %editc(ELemp#:'X');
         xxFyr  = %editc(ELFyr:'X');
         xxNwrw = *blanks;
         callp  RtvqteNewRnw(xxCo#:xxFnd:xxFyr:xxEmp#:xxNwrw);

         If xxNwrw = 'N';
            flgNBquote = *on;
         Else;
            flgNBquote = *off;
         Endif;

       Endif;


       If not AddMode;

          // Chain to Employer Detail for Program.
          Chain xxRrn PEL0P;

          // Process only if valid employer detail record found.
          If %found;

            // Prevent process if revised billing has locked policy
            // If no lock exists then lock the policy.
             Select;
             When qteMode = *On;
             When @@mode <> 'N';
             When GetPolicyLockStatus(ELco#:ELfnd:ELemp#:
              strJobInfo:strProcess:strUser) = *On;
                If strProcess = 'REVBILLING';
                   Select;
                   When strProcess = 'MIDTERMEND';
                      strProcess25 = 'Midterm Endorsement';
                   When strProcess = 'ADDIVISION';
                      strProcess25 = 'Add of Division';
                   When strProcess = 'REVBILLING';
                      strProcess25 = 'Revised Billing';
                   Endsl;
                   xxMsg = 'This operation is available when ' +
                    %Trim(strProcess25) + ' Job(' + %Trim(strJobInfo) +
                    ') has completed for ' + %Trim(strUser) + '.';
                   xxTtl = '*** Policy Locked! ***';
                   xxRsp = PopOneBtn(xxMsg:xxTtl);
                   processMe = *Off;
                   Return;
                Endif;
             Other;
                SetPolicyLock(ELco#:ELfnd:ELemp#:'ADDIVISION');
             EndSl;

             scCo#  = ELCO#;
             scFnd  = ELFND;
             scEmp# = ELEMP#;
             scDiv  = ELDIV;
             scFyr  = ELFYR;
             decDivFrom = ELdiv;
             decDivStRpt = GetStRptDv(ELco#:ELfnd:Elemp#);

             Chain(n) keyWMEML PEM00;

             processMe = *on;
             Return;

          Else;
             processMe = *off;
             Return;
          Endif;

       Else;
          processMe = *on;
          Return;
       Endif;

      /end-free
     P ProgramStart    E

     PPriorYearExists  b
     DPriorYearExists  pi             1n
     D inCo#                          3s 0 const
     D inFnd                          3s 0 const
     D inFyr                          3s 0 const
     D inEmp#                         9s 0 const
     D inDiv                          5s 0 const options(*nopass)

     DdecCount         s             10i 0 inz
     DstrSql           s           1000    varying inz

      /Free
       strSql = 'select count(*) from WDELP' +
        ' where ELco# = ' + %char(inCo#) + ' and ELfnd = ' + %char(inFnd) +
        '  and ELfyr < ' + %char(inFyr) + ' and ELemp# = ' + %char(inEmp#);
       If %parms >= 5;
          strSql += ' and ELdiv = ' + %char(inDiv);
       Endif;
       Exec sql close cursorPriorYr;
       Exec sql prepare sqlPriorYr From :strSql;
       Exec sql declare cursorPriorYr Cursor For sqlPriorYr;
       Exec sql open cursorPriorYr;
       Exec sql fetch next from cursorPriorYr into :decCount;
       Select;
       When decCount > *zero;
          Return *on;
       Endsl;
       Return *off;
      /End-free
     PPriorYearExists  e
       //***************************************************************************//
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
      /free

       //*** beg add ***** 01/25/22 ***********************
       // Get unit ID for submission history record
       Dcl-Ds dsUnID Qualified;
        time Zoned(6:0);
        date Zoned(6:0);
        intUnID Zoned(12:0) Pos(1);
       End-Ds;
       Dcl-S chrState char(2);

       dsUnID.time = %Dec(%Time():*hms);
       dsUnID.date = %Dec(%Date():*mdy);
       //*** end add ***** 01/25/22 ***********************

       // Skip the Quote/Policy Entry Window if adding a division.
       If addMode;
          QuotePopup();
       Endif;

       If btnExit <> *on;

          //*  Begin quote entry on screen 1.
          screen# = 1;
          If screenIndex <> 0;
             screen# = screenIndex;
          Endif;

          Dou btnFinish = *on or btnExit = *on;

             Select;
                When screen# = 1;
                   Screen1();
                When screen# = 2;
                   Screen2();
                When screen# = 3;
                   Screen3();
                When screen# = 4;
                   Screen4();
                When screen# = 5;
                   Screen5();
                When screen# = 6;
                   Screen6();
                When screen# = 7;
                   Screen7();
                When screen# = 8;
                   Screen8();
                When screen# = 9;
                   Screen9();
                When screen# = 10;
                   ScreenS();
                Other;
                   btnFinish = *on;
             Endsl;

             If btnFinish = *on;
             Endif;

          Enddo;

       //*** beg chg ***** 01/25/22 *********************
       //   If (divAdd = *On) and (qteMode = *Off);
       //      intStRptDiv = GetStRptDv(ELco#:ELfnd:Elemp#);
       //      inCo# = %editc(ELCo#:'X');
       //      inFnd = %editc(ELfnd:'X');
       //      inEmp# =%editc(ELemp#:'X');
       //       Setgt (ELco#:ELfnd:ELemp#:ELfyr) pel08;
       //       Dou %eof(wdell8);
       //          Reade (ELco#:ELfnd:ELemp#) pel08;
       //          If %eof(wdell8);
       //             Leave;
       //          Endif;
       //          If E2div <> intStRptDiv;
       //             Iter;
       //          Endif;
       //          fyr = E2fyr - 1;
       //          inFyr = %editc(fyr:'X');
       //          inType = 'Z';
       //         // Call renewal process to create subsequent years in live
       //          W7810C(inCo#:inFnd:inFyr:inType:inPct:inOpt:inIdfl:
       //           inFtDt:inTtDt:inTime:inOutq:inRtoQ:inStIn:inStrp:inEmp#);
       //          If mteMode = *On;
       //            //Write WMSHP record for an endorsement
       //             WriteReSubEnd();
       //          Endif;
       //       Enddo;
       //       saveFyr = E2fyr;
       //       If chkFutDtl(ELco#:ELfnd:ELemp#:intStRptDiv:saveFyr)= *On;
       //          inFyr = %editc(saveFyr:'X');
       //          inType = 'Z';
       //          inRtoQ = 'Y';
       //         // Call renewal process to create subsequent year in quote
       //          W7810C(inCo#:inFnd:inFyr:inType:inPct:inOpt:inIdfl:
       //           inFtDt:inTtDt:inTime:inOutq:inRtoQ:inStIn:inStrp:inEmp#);
       //       Endif;
       //   Endif;
       Exsr RenewFutureYears;
       //*** end chg ***** 01/25/22 ***********************

       Endif;

      /end-free
       //*** beg add ***** 01/25/22 ************************
       //---------------------------------------------------
       Begsr RenewFutureYears;
       //---------------------------------------------------
        // Don't process unless adding a division to live
        If divAdd = *Off Or qteMode = *On;
           LeaveSr ;
        EndIf;
        // Get future years on state reporting division in live
        intStRptDiv = GetStRptDv(ELco#:ELfnd:Elemp#);
        inCo# = %editc(ELCo#:'X');
        inFnd = %editc(ELfnd:'X');
        inEmp# =%editc(ELemp#:'X');
        inDiv = %Editc(ELdiv:'X');
        inType = 'Z';
        Setgt (ELco#:ELfnd:ELemp#:intStRptDiv:ELfyr) pel00;
        Dou %Eof(WDELL);
           Reade(n) (ELco#:ELfnd:ELemp#:intStRptDiv) pel00;
           If %Eof(WDELL);
              Leave;
           Endif;
           fyr = ELfyr - 1;
           inFyr = %Editc(fyr:'X');
          // Call renewal process to create subsequent years in live
           W7810C(inCo#:inFnd:inFyr:inType:inPct:inOpt:inIdfl:
            inFtDt:inTtDt:inTime:inOutq:inRtoQ:inStIn:inStrp:
            inEmp#:inDiv);
          //Write WMSHP record for a midterm endorsement
           If mteMode = *On;
              Exsr WriteSbmHistory;
           Endif;
        Enddo;
       // Renew to quote if the st rpt division renewal year is
       // in quote.
        If ChkFutDtl(ELco#:ELfnd:ELemp#:intStRptDiv:ELfyr) = *On;
           inFyr = %Editc(ELfyr:'X');
           inRtoQ = 'Y';
          // Call renewal process to create subsequent year in quote
           W7810C(inCo#:inFnd:inFyr:inType:inPct:inOpt:inIdfl:
            inFtDt:inTtDt:inTime:inOutq:inRtoQ:inStIn:inStrp:
            inEmp#:inDiv);
        Endif;
       Endsr;
       //---------------------------------------------------
       Begsr WriteSbmHistory;
       //---------------------------------------------------
       // Get Policy Submission History record. Leave if it
       // doesn't exist.
        charType = 'P';
        Chain (ELco#:ELfnd:ELemp#:ELfyr:charType) psh01;
        If Not %Found(wmshl1);
           LeaveSr;
        Endif;
       // Add new state to policy record if needed.
        Chain (ELco#:ELrtcd) Prs00;
        Chain (RSco#:'NCCI':RSjrst) pgt00;
        If %Found;
           chrState = 'XX';    // NCCI state
        Else;
           chrState = RSjrst;  // non-NCCI state
        Endif;
        idx = %LookUp(chrState:arySHst);
       // Add state to next available slot with a
       // status of '4' to force policy page to
       // rewrite the WCPOLS workfiles.
        If Not %Found;
            idx = %LookUp(*Blanks:arySHst);
            arySHst(idx) = chrState;
            arySHss(idx) = '4';
        Endif;
        Update psh01;
       // Carry over fields from policy record
       // and update as if a midterm endorsement
       // were run. Update state status fields to '3'
       // except for MA.
        For idx = 1 to 15;
           If arySHss(idx) = *Blank;
              Leave;
           Endif;
           If FNpeos <> 'Y' And aryShst(idx) = 'MA';
              arySHss(idx) = '4';
           Else;
              arySHss(idx) = '3';
           Endif;
        EndFor;
        SHco# = ELco#;
        SHfnd = ELfnd;
        SHemp# = ELemp#;
        SHFyr = ELfyr;
       // Ensure SHUNID is unique for all policy years.
        dsUnID.time = %Dec(%Time(dsUnID.time:*Hms) + %Seconds(1):*Hms);
        SHunid = dsUnID.intUnID;
        SHtrdt = %Dec(%Date():*Cymd);
        SHsbdt = *Zeros;
        SHtype = 'E';
        SHcode = 'O';
        SHstat = '3';
        SHtime = dsUnID.time;
        SHuser = Q1user;
        SHpgm  = Q1pgm;
        SHdt   = SHtrdt;
        SHidt  = SHdt;
        SHiusr = Q1user;
        Write psh01;
       EndSr;
       //*** end add ***** 01/25/22 ********************************
     P Main            E
       //*** beg del ***** 01/25/22 ***********************
       //===================================================================
       // WriteReSubmitEnd - Write WMSHP rec for an endorsement
       //===================================================================
     P*** WriteReSubEnd   B
     D*** WriteReSubEnd   PI
      ***/Free
       //charType = 'P';
       //Chain (ELco#:ELfnd:ELemp#:E2fyr:charType) psh01;
       //If %found(wmshl1);
       //   Chain (ELco#:ELfnd:ELemp#:intStRptDiv:E2fyr) pen03;
       //   If %found(WMENL3);
       //      SHco#  =  ELco#;
       //      SHfnd  =  ELfnd;
       //      SHemp# =  ELemp#;
       //      SHdiv  =  ELdiv;
       //      SHfyr  =  E2fyr;
       //      SHunid =  *Zeros;
       //      SHtrdt =  ENDT;
       //      SHsbdt =  *Zeros;
       //      SHtype = 'E';
       //      SHcode = 'O';
       //      SHstat = '4';
       //      SHseq# =  *Zeros;
       //      SHtime =  ENtime;
       //      SHreq# = ENreq#;
       //      SHuser = Q1user;
       //      SHpgm  = Q1pgm;
       //      SHdt   = Dateto7(SystemDate());
       //      SHidt  = SHdt;
       //      SHiusr = Q1user;
       //      SHpfdt = ENpfd1;
       //      Write psh0p;
       //   Endif;
       //Endif;

      ***/End-free
     P*** WriteReSubEnd   E
       //*** end del ***** 01/25/22 ***********************

       //===================================================================
       // QuotePopup - Process the Clearance Window
       //===================================================================
     P QuotePopup      B
     D QuotePopup      PI
     D programIn       s             10
     D tabNameIn       s             50
     D modeIn          s             50    inz('E')
     D callIn          s             50    inz(' ')
     D kcIn            s             50    inz(' ')
     D klIn            s             50    inz(' ')
     D rrnOut          s             50    inz('000000000')
     D selectIn        s           1024
     D flgAssignQte    s              1    inz('N')
     D flgAutoAssign   s              1    inz('N')
     D errEmpDiv       s              1n
     DdsWMEML          ds                  likerec(pem00:*input) inz
      *** Beg Del ***** 08/24/21 ***************************************
     D*dsWDELL          ds                  likerec(peL00:*input) inz
     D*dsWMEMLQ         ds                  likerec(pem0Q:*input) inz
     D*dsWDELLQ         ds                  likerec(pel0Q:*input) inz
      *** End Del ***** 08/24/21 ***************************************
      *** beg add ***** 08/24/21 **************************
       Dcl-S chrEnv Char(1);
       Dcl-S decCount Packed(3:0);
       Dcl-S varSql VarChar(5000);
      *** end add ***** 08/24/21 **************************
      /free

       Exsr LoadQtePop;

       Dou popNext = *on;

          Exsr SetCriteria;

          //*  Load Master information if mode is 'M'
          If inMode = 'Y';
             Exsr LoadMstInfo;
          Endif;

          //*  Process Quote Popup (with 1st screen displayed in background)
          Write rcdPolicy;
          Exfmt rcdQtePop;

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             //* Inception date changed - update expiration date
             When btnIncep = *on;
                Exsr ChgExpDate;
                btnIncep = *off;

             Other;

                //*  Edit Quote Popup for Errors.
                Exsr CheckQtePop;

                If errorPop;
                   popNext = *off;
                Endif;

                If popNext = *on;
                   //***  Beg Chg ***** 10/01/18 *****************************
                   //Exsr AssignQuote;
                   Chain(n) keyWMFNL PFN00;
                   If (FNAutE = 'Y' and FNAutD = 'Y') or inMode = 'Y';
                      flgAutoAssign = 'Y';
                      Exsr AssignQuote;
                   Else;
                      Exsr AssignEmpDiv;
                      If flgAssignQte = 'Y';
                         flgAutoAssign = 'N';
                         Exsr AssignQuote;
                      Endif;
                   Endif;
                   //***  End Chg ***** 10/01/18 *****************************
                Endif;

          Endsl;

       Enddo;

       //===================================================================
       // AssignEmpDiv - Assign Employer/Division popup
       //===================================================================
       Begsr AssignEmpDiv;

          Clear scEmpNbr;
          Clear scEmpDiv;

          Chain(n) keyWMFNL PFN00;
          isAAsnEmp = *off;
          isAAsnDiv = *off;
          If FNAutE = 'Y';
             isAAsnEmp = *on;
          Endif;
          If FNAutD = 'Y';
             isAAsnDiv = *on;
          Endif;

          If inMode <> 'Y';
             If FNAutE = 'Y';                  //Auto Assign employer
                Chain keyWMFNL PFN00;
                Dou not %found;
                   If qteMode = *On;
                      If FNnaqn = 0;
                         FNnaqn = 1;
                      Endif;
                      scEmp# = FNnaqn;
                      FNnaqn += 1;
                   Else;
                      If FNnaen = 0;
                         FNnaen = 1;
                      Endif;
                      scEmp# = FNnaen;
                      FNnaen += 1;
                   Endif;

                   Chain(n) key1WMEML PEM00;
                Enddo;

                scEmpNbr = scEmp#;

                FNpgm  = Q1PGM;
                FNuser = Q1USER;
                FNtime = Timeto6(SystemTime());
                FNdt   = Dateto7(SystemDate());

                Update PFN00;

             Endif;                            //FNAutE = 'Y'

             //Assign division 0 or 1 based on Allow Div 0 setup for the group
             If FNdiv0 = 'Y';
                scDiv = 0;
             Else;
                scDiv = 1;
             Endif;
             scEmpDiv = scDiv;
          Endif;                               //inMode <> 'Y'

          flgAssignQte = 'N';

          Dou btnEmpOk = *on;
             Exfmt rcdEmpPop;

             Select;
             When btnEmpCncl = *on;
                popNext = *off;
                Leave;
             Other;
                Exsr CheckEmpDiv;
                If errEmpDiv;
                   btnEmpOk = *off;
                Endif;
                If btnEmpOk = *on;
                   flgAssignQte = 'Y';
                Endif;
             Endsl;
          Enddo;

       Endsr;

       //===================================================================
       // CheckEmpDiv - Assign Employer/Division popup
       //===================================================================
       Begsr CheckEmpDiv;

          errEmpDiv = *Off;
          erEmpNbr  = *Off;
          msEmpNbr  = ' ';
          erEmpDiv  = *Off;
          msEmpDiv  = ' ';
          If scEmpNbr = *zero;
             erEmpNbr  = *On;
             msEmpNbr  = 'WCR0538';
             errEmpDiv  = *on;
          Else;
             Chain(n)(scCo#:scFnd:scEmpNbr) pem00;
             If %found();
                erEmpNbr  = *On;
                msEmpNbr  = 'WCR0853';
                errEmpDiv  = *on;
             Endif;
          Endif;

          //Division 0 is not allowed if Allow div 0 = 'N'
          If scEmpDiv = 0 and FNAutD = 'N' and FNdiv0 = 'N';
             erEmpDiv  = *On;
             msEmpDiv  = 'WCR4828';
             errEmpDiv  = *on;
          Endif;

       Endsr;

      /end-free

       //===================================================================
       // LoadQtePop - Load the initial quote popup fields
       //===================================================================
     C     LoadQtePop    Begsr
      /free

       scFein     = 0;
       scIncepDte = 0;
       scExpDte   = 0;
       scAnnivDte = 0;
       scCOFL     = ' ';
       scAgent1   = 0;
       scProd1    = 0;
       scRate1    = 0;
       scAgent2   = 0;
       scProd2    = 0;
       scRate2    = 0;
       scAgent3   = 0;
       scProd3    = 0;
       scRate3    = 0;
       scControl  = 0;
       scControl1 = 0;
       scRecDate  = 0;


       programIn = 'W1058C';
       tabNameIn = 'All Owner/Officer Search';
       scOwnLink = CreateBIGUrl(programIn:tabNameIn:callIn:kcIn:klIn:rrnOut);

       popNext = *off;
       btnIncep = *off;

       Exsr ClearQtePop;

      /end-free
     C                   Endsr                                                  LoadQtePop Subr

       //===================================================================
       // SetCriteria - Load the search criteria fields
       //===================================================================
     C     SetCriteria   Begsr
      /free

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       agt1Choice = 'AACO# = ' + %char(scCO#) + ' AND AAFND = ' + %char(scFND)
        + ' AND AASTAT = ''A''';
       agt2Choice = 'AACO# = ' + %char(scCO#) + ' AND AAFND = ' + %char(scFND)
        + ' AND AASTAT = ''A''';
       agt3Choice = 'AACO# = ' + %char(scCO#) + ' AND AAFND = ' + %char(scFND)
        + ' AND AASTAT = ''A''';

       rte1Choice = 'ARCO# = ' + %char(scCO#) + ' AND ARFND = ' + %char(scFND)
        + ' AND ARSTAT = ''A''';
       rte2Choice = 'ARCO# = ' + %char(scCO#) + ' AND ARFND = ' + %char(scFND)
        + ' AND ARSTAT = ''A''';
       rte3Choice = 'ARCO# = ' + %char(scCO#) + ' AND ARFND = ' + %char(scFND)
        + ' AND ARSTAT = ''A''';

       prd1Choice = ' ';

       If scAgent1 = 0;
          prd1Disabl = *on;
       Else;
          xxAgent = scAgent1;
          Chain keyWMAAL1 PAA01;
          If %found;

             prd1Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN || APMOD = '
              + %char(AAFEIN) + %char(AAMOD) + ' AND APSTAT = ''A''';
          Endif;
          prd1Disabl = *off;
       Endif;

       prd2Choice = ' ';

       If scAgent2 = 0;
          prd2Disabl = *on;
       Else;
          xxAgent = scAgent2;
          Chain keyWMAAL1 PAA01;
          If %found;

             prd2Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN || APMOD = '
              + %char(AAFEIN) + %char(AAMOD) + ' AND APSTAT = ''A''';
          Endif;
          prd2Disabl = *off;
       Endif;

       prd3Choice = ' ';

       If scAgent3 = 0;
          prd3Disabl = *on;
       Else;
          xxAgent = scAgent3;
          Chain keyWMAAL1 PAA01;
          If %found;

             prd3Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN || APMOD = '
              + %char(AAFEIN) + %char(AAMOD) + ' AND APSTAT = ''A''';
          Endif;
          prd3Disabl = *off;
       Endif;

      /end-free
     C                   Endsr                                                  SetCriteria Subr

       //===================================================================
       // LoadMstInfo - Load the Mater Information.
       //===================================================================
     C     LoadMstInfo   Begsr
      /free
       xxMRrn = %int(@@rrn);
       Chain(n) xxMRrn pem0p;
       If %found;
          scFEIN= EMfein;
          scIncepDte = dtMDYY(Emefd1);
          If scIncepDte <> 0;
             Exsr ChgExpDate;
          Endif;
       Endif;
      /end-free
     C                   Endsr                                                  LoadMstInfo
       //===================================================================

       //===================================================================
       // ClearQtePop - Clear error/button fields
       //===================================================================
     C     ClearQtePop   Begsr
      /free

       errorPop   = *off;

       erFnd      = *off;
       msFnd      = *blanks;
       erFein     = *off;
       msFein     = *blanks;
       erIncepDte = *off;
       msIncepDte = *blanks;
       erExpDte   = *off;
       msExpDte   = *blanks;
       erAnnivDte = *off;
       msAnnivDte = *blanks;
       erCOFL     = *off;
       msCOFL     = *blanks;
       erAgent1   = *off;
       msAgent1   = *blanks;
       erProd1    = *off;
       msProd1    = *blanks;
       erRate1    = *off;
       msRate1    = *blanks;
       erAgent2   = *off;
       msAgent2   = *blanks;
       erProd2    = *off;
       msProd2    = *blanks;
       erRate2    = *off;
       msRate2    = *blanks;
       erAgent3   = *off;
       msAgent3   = *blanks;
       erProd3    = *off;
       msProd3    = *blanks;
       erRate3    = *off;
       msRate3    = *blanks;
       erControl  = *off;
       erControl1 = *off;
       //msControl  = *blanks;
       erRecDate  = *off;
       msRecDate  = *blanks;

      /end-free
     C                   Endsr                                                  ClearQtePop Subr

       //===================================================================
       // CheckQtePop - Check Quote Popup for Errors
       //===================================================================
     C     CheckQtePop   Begsr
      /free

       Exsr ClearQtePop;

       //*** beg add ***** 05/03/23 ***********************
       // error will be display when FEIN is 0 and set to required
       If qteMode <> *on;
          Chain (scCo#:scFnd) pex00;
          If %Found(Wmexl);
             If EXfifg = 'Y' and scFEIN = 0;
                msFEIN = 'WCR0502';
                erFEIN = *on;
                errorPop = *on;
             Endif;
          Endif;
       Endif;
       //*** end add ***** 05/03/23 ***********************
       erIncepDte = ValidDate(scIncepDte:message);
       If erIncepDte = *on;
          msIncepDte = message;
          errorPop   = *on;

       Else;
          Setgt keyWDF1L PF100;
          Readpe keyWDF1L PF100;

          Dow not %eof(WDF1L);
            If dtCYMD(scIncepDte) >= F1stpo and dtCYMD(scIncepDte) < F1enpo;
               MsIncepDte = *blanks;
               ErIncepDte = *Off;
               Leave;
            Endif;
            Readpe keyWDF1L PF100;
          Enddo;

          If %eof(WDF1L);
             MsIncepDte = 'WCR1145';
             ErIncepDte = *On;
             errorPop = *ON;
          Endif;
       Endif;


       erExpDte = ValidDate(scExpDte:message);
       If erExpDte = *on;
          msExpDte = message;
          errorPop = *on;
       Endif;

       If erIncepDte = *off and erExpDte = *off;
          erIncepDte = AscendDates(scIncepDte:scExpDte:message);
          If erIncepDte = *on;
             erExpDte   = *on;
             msIncepDte = message;
             msExpDte   = message;
             errorPop   = *on;
          Endif;
       Endif;

       If erIncepDte = *off and erExpDte = *off;
          erIncepDte = PolFundDates(scIncepDte:scExpDte:scCo#:scFnd:scFyr:
           'Y':message);
          If erIncepDte = *on;
             erExpDte   = *on;
             msIncepDte = message;
             msExpDte   = message;
             errorPop   = *on;
          Endif;
       Endif;

       If erExpDte = *off;
          erExpDte = PolMaxLength(scIncepDte:scExpDte:scCo#:scFnd:message);
          If erExpDte = *on;
             msExpDte = message;
             errorPop = *on;
          Endif;
       Endif;

       If scAnnivDte <> 0;

          erAnnivDte = ValidDate(scAnnivDte:message);
          If erAnnivDte = *on;
             msAnnivDte = message;
             errorPop   = *on;
          Endif;

          If erAnnivDte = *off;
             erAnnivDte = PolDtLessPolEf(scAnnivDte:scIncepDte:message);
             If erAnnivDte = *on;
                msAnnivDte = message;
                errorPop   = *on;
             Endif;
          Endif;

       Endif;

       //*** Begin Add *** 07/26/22 ****************************************
       isPeoMasterExists = *Off;
       //*** End   Add *** 07/26/22 ****************************************
      //*** beg add ***** 08/24/21 ***********************
      // Ensure PEO policy year exists before adding a member
      //  through Policy/Quote Entry.
      // First check if a PEO exists yet in live or quote. Let it
      //  be added if it doesn't.  If it does, make sure it has
      //  a policy year for the new client policy or division.
       Chain(n) keyWMFNL PFN00;
       erNoPeo = *Off;
       msNoPeo = *Blanks;
       If FNpeof <> *Blank And errorPop = *Off;
          varSql = 'Select ''Q'', ' + 'Count(*)' +
           ' From ' + %Trim(RtvQuoteLib('*QTE')) + '/WMEMP' +
           ' Where EMco# = ' + %EditC(scCo#:'X') +
           ' And EMfnd = ' + %EditC(scFnd:'X') +
           ' And EMemp# = 1' +
           ' And EMdiv = 0' +
           ' Union' +
           ' Select ''L'', ' + 'Count(*)' +
           ' From ' + %Trim(RtvQuoteLib('*LIV')) + '/WMEMP' +
           ' Where EMco# = ' + %EditC(scCo#:'X') +
           ' And EMfnd = ' + %EditC(scFnd:'X') +
           ' And EMemp# = 1' +
           ' And EMdiv = 0';
          Exec Sql Prepare sqlWMEMP  From :varSql;
          Exec Sql Declare M1 Cursor For sqlWMEMP;
          Exec Sql Open M1;
          Dou Not IsValidFetch(sqlStt);
             Exec SQL Fetch Next From M1 Into :chrEnv, :decCount;
             If Not IsValidFetch(sqlStt);
                Leave;
             Endif;
        // PEO can exist in live or quote if in quote.
        // If in live, the PEO must be in live.
             Select;
             When qteMode = *On;
                If decCount > 0;
                   //*** Begin Add *** 07/26/22 ****************************
                   isPeoMasterExists = *On;
                   //*** End   Add *** 07/26/22 ****************************
                   Leave;
                Endif;
             When chrEnv = 'L' And decCount > 0;
                //*** Begin Add *** 07/26/22 *******************************
                isPeoMasterExists = *On;
                //*** End   Add *** 07/26/22 *******************************
                Leave;
             When chrEnv = 'Q';
                decCount = 0;
             EndSl;
          Enddo;
          Exec Sql Close M1;
        // Checking for the policy year if the PEO exists.
          If decCount <> 0;
             varSql = 'Select ''Q'', ' + 'Count(*)' +
              ' From ' + %Trim(RtvQuoteLib('*QTE')) + '/WDELP' +
              ' Where ELco# = ' + %EditC(scCo#:'X') +
              ' And ELfnd = ' + %EditC(scFnd:'X') +
              ' And ELemp# = 1' +
              ' And ELdiv = 0' +
              ' And ELfyr = ' + %EditC(F1fyr:'X')  +
              ' Union' +
              ' Select ''L'', ' + 'Count(*)' +
              ' From ' + %Trim(RtvQuoteLib('*LIV')) + '/WDELP' +
              ' Where ELco# = ' + %EditC(scCo#:'X') +
              ' And ELfnd = ' + %EditC(scFnd:'X') +
              ' And ELemp# = 1' +
              ' And ELdiv = 0' +
              ' And ELfyr = ' + %EditC(F1fyr:'X');
             Exec Sql Prepare sqlWDELP From :varSql;
             Exec Sql Declare D1 Cursor For sqlWDELP;
             Exec Sql Open D1;
             Dou Not IsValidFetch(sqlStt);
                Exec SQL Fetch Next From D1 Into :chrEnv, :decCount;
                If Not IsValidFetch(sqlStt);
                   Leave;
                Endif;
        // If in quote, the policy year can be in live or quote.
        // If in live, the policy year must be in live.
                Select;
                When qteMode = *On;
                   If decCount > 0;
                      Leave;
                   Endif;
                When chrEnv = 'L' And decCount > 0;
                   Leave;
                When chrEnv = 'Q';
                   decCount = 0;
                EndSl;
             Enddo;
             Exec Sql Close D1;
          Endif;
          //*** Begin Chg *** 07/26/22 *************************************
          //**********If decCount = 0;
          If decCount = 0 And isPeoMasterExists;
          //*** End   Chg *** 07/26/22 *************************************
             errorPop  = *on;
             erNoPeo = *On;
             msNoPeo = 'No policy year exists for ' +
              'the PEO for this date.';
          Endif;
       Endif;
      //*** end add ***** 08/24/21 ***********************

       erCOFL = ValidFlagY(scCOFL:message);
       If erCOFL = *on;
          msCOFL = message;
          errorPop  = *on;
       Endif;

       erAgent1 = ValidAgent(scCo#:scFnd:scAgent1:message);
       If erAgent1 = *on;
          msAgent1 = message;
          errorPop = *on;
       Endif;

       erProd1 = ValidProducer(scCo#:scFnd:scAgent1:scProd1:message);
       If erProd1 = *on;
          msProd1 = message;
          errorPop = *on;
       Endif;

       erRate1 = ValidAgtRate(scCo#:scFnd:scRate1:scIncepDte:message);
       If erRate1 = *on;
          msRate1 = message;
          errorPop = *on;
       Endif;

       If erAgent1 = *off;
          erAgent1 = PolAgtDate(scCo#:scFnd:scAgent1:scIncepDte:message);
          If erAgent1 = *on;
             msAgent1 = message;
             errorPop = *on;
          Endif;
       Endif;

       If erProd1 = *off;
          erProd1 = PolProdDate(scCo#:scFnd:scAgent1:scProd1:scIncepDte:
           message);
          If erProd1 = *on;
             msProd1 = message;
             errorPop = *on;
          Endif;
       Endif;

       If scAgent2 <> 0;
          erAgent2 = ValidAgent(scCo#:scFnd:scAgent2:message);
          If erAgent2 = *on;
             msAgent2 = message;
             errorPop = *on;
          Endif;

          erProd2 = ValidProducer(scCo#:scFnd:scAgent2:scProd2:message);
          If erProd2 = *on;
             msProd2 = message;
             errorPop = *on;
          Endif;

          erRate2 = ValidAgtRate(scCo#:scFnd:scRate2:scIncepDte:message);
          If erRate2 = *on;
             msRate2 = message;
             errorPop = *on;
          Endif;

          If erAgent2 = *off;
             erAgent2 = PolAgtDate(scCo#:scFnd:scAgent2:scIncepDte:message);
             If erAgent2 = *on;
                msAgent2 = message;
                errorPop = *on;
             Endif;
          Endif;

          If erProd2 = *off;
             erProd2 = PolProdDate(scCo#:scFnd:scAgent2:scProd2:scIncepDte:
              message);
             If erProd2 = *on;
                msProd2 = message;
                errorPop = *on;
             Endif;
          Endif;

       Endif;

       If scAgent3 <> 0;
          erAgent3 = ValidAgent(scCo#:scFnd:scAgent3:message);
          If erAgent3 = *on;
             msAgent3 = message;
             errorPop = *on;
          Endif;

          erProd3 = ValidProducer(scCo#:scFnd:scAgent3:scProd3:message);
          If erProd3 = *on;
             msProd3 = message;
             errorPop = *on;
          Endif;

          erRate3 = ValidAgtRate(scCo#:scFnd:scRate3:scIncepDte:message);
          If erRate3 = *on;
             msRate3 = message;
             errorPop = *on;
          Endif;

          If erAgent3 = *off;
             erAgent3 = PolAgtDate(scCo#:scFnd:scAgent3:scIncepDte:message);
             If erAgent3 = *on;
                msAgent3 = message;
                errorPop = *on;
             Endif;
          Endif;

          If erProd3 = *off;
             erProd3 = PolProdDate(scCo#:scFnd:scAgent3:scProd3:scIncepDte:
              message);
             If erProd3 = *on;
                msProd3 = message;
                errorPop = *on;
             Endif;
          Endif;

       Endif;

       erRecDate = ValidDate(scRecDate:message);
       If erRecDate = *on;
          msRecDate = message;
          errorPop = *on;
       Endif;

       // Check controlling agency
       If scControl > 0 and scControl1 > 0;
          erControl = *on;
          erControl1 = *on;
          errorPop = *on;
       Endif;

       // Check for duplicate fein if one has been entered.
       If scFein <> 0 and not errorPop and inMode <> 'Y';
          feinCancel = *off;
          feinContin = *off;

          programIn = 'WTSEARCH';
          tabNameIn = 'Duplicate FEIN Listing';
          Clear dsSrchCrit;
          Clear selectIn;
          dsEmpFein = scFein;
          selectIn  = dsSrchCrit;
          If qteMode = *on;
             scDupLink = CreateBIGQteUrl(programIn:tabNameIn:modeIn:callIn:
              rrnOut:selectIn);
          Else;
          scDupLink = CreateBIGUrl(programIn:tabNameIn:modeIn:callIn:
           rrnOut:selectIn);
          Endif;

          #@err = ' ';

          Chain keyWMEML12 PEM12;

          If not %found;
             Exsr FeinAPI;
          Endif;

          If %found or #@err = '1';

             Dou feinCancel = *on or feinContin = *on;
                Exfmt rcdFeinPop;
             Enddo;

             If feinCancel = *on;
                errorPop = *on;
             Endif;

          Endif;

       Endif;

      /end-free
     C                   Endsr                                                  CheckQtePop Subr
       //===================================================================
       // Fein API - Call the duplicate Fein api to check other environment.
       //===================================================================
     C     FeinAPI       Begsr
     C*
     C*  CHECK TO SEE IF FEIN HAS BEEN USED IN OTHER ENVIRONMENT
     C*
     C                   MOVE      scCO#         #@CO#
     C                   MOVE      scFEIN        #@FEIN
     C*
     C                   CALL      'WA032C'
     C                   PARM                    #@CO#             3
     C                   PARM                    #@FEIN            9
     C                   PARM      QuoteMode     #@QTE             1
     C                   PARM                    #@ERR             1
     C*
     C                   Endsr                                                  Fein API Subr

       //===================================================================
       // ChgExpDate - Update the expiration date based on group and eff date.
       //===================================================================
     C     ChgExpDate    Begsr
     c*
     C                   Z-ADD     scIncepDte    SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C
      /Free
       Setgt keyWDF1L PF100;
       Readpe keyWDF1L PF100;

       Dow not %eof(WDF1L);
        If dtCYMD(scIncepDte) >= F1stpo and dtCYMD(scIncepDte) < F1enpo;
           MsIncepDte = *blanks;
           ErIncepDte = *Off;
           Leave;
        Endif;
        Readpe keyWDF1L PF100;
       Enddo;

       If %eof(WDF1L);
          MsIncepDte = 'WCR1145';
          ErIncepDte = *On;
          SRERR = *ON;
       Endif;

      /End-Free
     c                   If        SRERR <> *ON
     C                   MOVEL     SRDTO         scIncepDte
     c
      /free

          Chain(n) keyWMFNL PFN00;

          Setgt keyWDF1L PF100;
          Readpe keyWDF1L PF100;

          Dow  F1Stpo > dtCYMD(scIncepDte);
             Readpe keyWDF1L PF100;
          Enddo;

          scFyr = F1Fyr;

          If FNCGFL = 'Y';
             //scExpDte = dtFuture(scIncepDte:365);
             scExpDte = DateTo8(CvtToDate(scIncepDte) + %years(1));
          Else;
             scExpDte = F1Enpo;
          Endif;

      /end-free
     c                   ENDIF
     c*
     c                   If        inMode <> 'Y'
     C                   Exsr      ClearQtePop
     c                   Endif
     c*
     C                   Endsr                                                  ChgExpDate Subr

       //===================================================================
       // AssignQuote - Assign Quote # and write initial data files
       //===================================================================
     C     AssignQuote   Begsr
      /free


       //* Assign Quote # and update group master file.
       If inMode <> 'Y';
          If flgAutoAssign = 'Y';
             Chain keyWMFNL PFN00;
             Dou not %found;

                If qteMode = *On;
                   If FNNAQN = 0;
                      FNNAQN = 1;
                   Endif;
                   scEmp# = FNNAQN;
                   FNNAQN += 1;
                Else;
                   If FNNAEN = 0;
                      FNNAEN = 1;
                   Endif;
                   scEmp# = FNNAEN;
                   FNNAEN += 1;
                Endif;

                Chain(n) key1WMEML PEM00;

             Enddo;

             FNPGM  = Q1PGM;
             FNUSER = Q1USER;
             FNTIME = Timeto6(SystemTime());
             FNDT   = Dateto7(SystemDate());

             Update PFN00;
             If FNDIV0 = 'Y';
                scDiv = 0;
             Else;
                scDiv = 1;
             Endif;
          Else;                               //flgAutoAssign = 'Y'
             scEmp# = scEmpNbr;
             scDiv  = scEmpDiv;
          Endif;

       // *** Beg Del ***** 08/24/21 ***********************************
       //   Reset dsWMEML;
       //   If FNpeoF <> *blank;
       //      Chain (scCo#:scFnd:1:0) wmeml dsWMEML;
       //
       //      If not %found(wmeml);
       //         If qteMode = *On;
       //            If %Open(WMEMLQ);
       //               Close WMEMLQ;
       //            Endif;
       //            strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
       //             %Trim(RtvQuoteLib('*LIV')) + '/WMEMLQ)';
       //            Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //
       //            Open WMEMLQ;
       //            Chain (scCo#:scFnd:1:0) wmemlq dsWMEMLQ;
       //            dsWMEML = dsWMEMLQ;
       //            Close WMEMLQ;
       //
       //            strCommand = 'DLTOVR FILE(WMEMLQ)';
       //            Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //         Endif;
       //      Endif;
       //   Endif;
       // *** End Del ***** 08/24/21 **********************************************


          Clear PEM00;

          EMCo#  = scCo#;
          EMFnd  = scFnd;
          EMEmp# = scEmp#;
          EMDiv  = scDiv;

          EMFein = scFein;
          EMEfd1 = dtCYMD(scIncepDte);

          EMSTRP = 'Y';
          EMVIP  = 'N';

          EMrgn# = dsWMEML.EMrgn#;
          EMoff# = dsWMEML.EMoff#;
          EMpSrv = dsWMEML.EMpSrv;
          EMuwtr = dsWMEML.EMuwtr;
          EMpAcc = dsWMEML.EMpAcc;
          EMaRvw = dsWMEML.EMaRvw;
          EMaMgr = dsWMEML.EMaMgr;
          EMlRep = dsWMEML.EMlRep;
          EMclmC = dsWMEML.EMclmC;
          EMchap = dsWMEML.EMchap;

       //*** Beg Add ***  04/29/24  *** WC2965 *************************
          EMmail = FNEmail;
       //*** End Add ***  04/29/24  *** WC2965 *************************

          EMIPgm = Q1PGM;
          EMIUsr = Q1USER;
          EMITim = Timeto6(SystemTime());
          EMIDt  = Dateto7(SystemDate());

          Write PEM00;
       Else;
          Chain xxMRrn PEM0P;
          If %found;
             EMPgm = Q1PGM;
             EMUser= Q1USER;
             EMtime = Timeto6(SystemTime());
             EMDt  = Dateto7(SystemDate());
             EMFein = scFein;
             EMEfd1 = dtCYMD(scIncepDte);
             scEmp# = Ememp#;
             Update PEM0P;
          Endif;
       Endif;

       Setgt keyWDF1L PF100;
       Readpe keyWDF1L PF100;

       Dow  F1Stpo > EMEfd1;
          Readpe keyWDF1L PF100;
       Enddo;

       Chain keyWMEXL PEX00;
       Chain keyWDFHL PFH00;

       // *** Beg Del ***** 08/24/21 **********************************************
       //Reset dsWDELL;
       //If FNpeoF <> *blank;
       //   Chain (scCo#:scFnd:1:0:F1fyr) wdell dsWDELL;
       //
       //   If not %found(wdell);
       //      If qteMode = *On;
       //         If %Open(WDELLQ);
       //            Close WDELLQ;
       //         Endif;
       //         strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
       //          %Trim(RtvQuoteLib('*LIV')) + '/WDELLQ)';
       //         Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //
       //         Open WDELLQ;
       //         Chain (scCo#:scFnd:1:0:F1fyr) wdellq dsWDELLQ;
       //         dsWDELL = dsWDELLQ;
       //         Close WDELLQ;
       //
       //         strCommand = 'DLTOVR FILE(WDELLQ)';
       //         Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //      Endif;
       //   Endif;
       //Endif;
       // *** End Del ***** 08/24/21 **************************************

       Clear PEL00;

       ELCo#  = scCo#;
       ELFnd  = scFnd;
       ELEmp# = scEmp#;
       ELDiv  = scDiv;
       ELFyr  = F1Fyr;

       scFyr  = F1Fyr;

       ELPfdt = EMEfd1;
       ELPtdt = dtCYMD(scExpDte);
       ELAndt = dtCYMD(scAnnivDte);

       ELAafl = F1Aafl;
       ELApfl = F1Apfl;
       ELBlfl = F1Blfl;

       ELmfee = F1mfee;
       ELNwrw = 'N';

       If %found(WDFHL);
          ELFFD1 = FHF1DF;
          ELFFD2 = FHF2DF;
          ELFFD3 = FHF3DF;
       Else;
          ELFFD1 = EXFDF1;
          ELFFD2 = EXFDF2;
       Endif;

       ELCofl = scCofl;
       ELSRPR = 'P';
       //*** Beg Add ***** 03/28/25 **************************************
       ELexrt = '3';
       //*** End Add ***** 03/28/25 **************************************

       If clientID = '150';
          //*** Beg Add ***** 03/28/25 ***********************************
          ELexrt = '2';
          ELncci = 'Y';
          //*** End Add ***** 03/28/25 ***********************************
          ELARSM = 'Y';

          Chain(n) (ELCo#:ELFnd:ELFyr) pf102;
          If %found;
             ELManP = L2F1ManP;
          Endif;

       Endif;
       // *** Beg Del ***** 08/24/21 *************************************
       //If FNpeoF <> *blank;
       //   ELptDt = dsWDELL.ELptDt;
       //   ELanDt = dsWDELL.ELanDt;
       //   ELarSm = dsWDELL.ELarSm;
       //   ELbSCd = dsWDELL.ELbSCd;
       //   ELbSDt = dsWDELL.ELbSDt;
       //   ELblFl = dsWDELL.ELblFl;
       //   ELfFl9 = dsWDELL.ELfFl9;
       //   ELcoFl = dsWDELL.ELcoFl;
       //   ELsrPr = dsWDELL.ELsrPr;
       //   ELmanP = dsWDELL.ELmanP;
       //   ELfFl2 = dsWDELL.ELfFl2;
       //   ELcMul = dsWDELL.ELcMul;
       //   ELiLCd = dsWDELL.ELiLCd;
       //   ELinL2 = dsWDELL.ELinL2;
       //   ELiLEf = dsWDELL.ELiLEf;
       //   ELiLEx = dsWDELL.ELiLEx;
       //   ELi2Ef = dsWDELL.ELi2Ef;
       //   ELi2Ex = dsWDELL.ELi2Ex;
       //   ELcxBl = dsWDELL.ELcxBl;
       //   ELdCde = dsWDELL.ELdCde;
       //   ELdCd2 = dsWDELL.ELdCd2;
       //   ELdCEf = dsWDELL.ELdCEf;
       //   ELdCEx = dsWDELL.ELdCEx;
       //   EL2DEf = dsWDELL.EL2DEf;
       //   EL2DEx = dsWDELL.EL2DEx;
       //   ELasp2 = dsWDELL.ELasp2;
       //   ELdvEf = dsWDELL.ELdvEf;
       //   ELdvEx = dsWDELL.ELdvEx;
       //   ELd1S2 = dsWDELL.ELd1S2;
       //   ELd1D2 = dsWDELL.ELd1D2;
       //   ELd1X2 = dsWDELL.ELd1X2;
       //   ELstFc = dsWDELL.ELstFc;
       //   ELdev2F = dsWDELL.ELdev2F;
       //   ELdev2 = dsWDELL.ELdev2;
       //   ELd2Ef = dsWDELL.ELd2Ef;
       //   ELd2Ex = dsWDELL.ELd2Ex;
       //   ELd2S2 = dsWDELL.ELd2S2;
       //   ELd2D2 = dsWDELL.ELd2D2;
       //   ELd2X2 = dsWDELL.ELd2X2;
       //   ELmcCd = dsWDELL.ELmcCd;
       //   ELmcGp = dsWDELL.ELmcGp;
       //   ELaspM = dsWDELL.ELaspM;
       //   ELmcEf = dsWDELL.ELmcEf;
       //   ELmcEx = dsWDELL.ELmcEx;
       //   ELaspS = dsWDELL.ELaspS;
       //   ELscEf = dsWDELL.ELscEf;
       //   ELscEx = dsWDELL.ELscEx;
       //   ELasp1 = dsWDELL.ELasp1;
       //   ELcrEf = dsWDELL.ELcrEf;
       //   ELcrEx = dsWDELL.ELcrEx;
       //   ELdbCr = dsWDELL.ELdbCr;
       //   ELsdEf = dsWDELL.ELsdEf;
       //   ELsdEx = dsWDELL.ELsdEx;
       //   ELprom = dsWDELL.ELprom;
       //   ELexM1 = dsWDELL.ELexM1;
       //   ELeMD1 = dsWDELL.ELeMD1;
       //   ELeMD2 = dsWDELL.ELeMD2;
       //   ELexM2 = dsWDELL.ELexM2;
       //   ELexM3 = dsWDELL.ELexM3;
       //   ELeMD3 = dsWDELL.ELeMD3;
       //   ELaRp1 = dsWDELL.ELaRp1;
       //   ELaRp2 = dsWDELL.ELaRp2;
       //   ELaRp3 = dsWDELL.ELaRp3;
       //   ELexMd1Nf = dsWDELL.ELexMd1Nf;
       //   ELexMd2Nf = dsWDELL.ELexMd2Nf;
       //   ELexMd3Nf = dsWDELL.ELexMd3Nf;
       //   ELasp3 = dsWDELL.ELasp3;
       //   ELdisc = dsWDELL.ELdisc;
       //   ELdisP = dsWDELL.ELdisP;
       //   ELmFee = dsWDELL.ELmFee;
       //   ELsFMn = dsWDELL.ELsFMn;
       //   ELsFPc = dsWDELL.ELsFPc;
       //   ELwvTp = dsWDELL.ELwvTp;
       //   ELwvPc = dsWDELL.ELwvPc;
       //   ELfChg = dsWDELL.ELfChg;
       //   ELwMin = dsWDELL.ELwMin;
       //   ELwMax = dsWDELL.ELwMax;
       //   ELwvFl = dsWDELL.ELwvFl;
       //   ELarCp = dsWDELL.ELarCp;
       //   ELarCa = dsWDELL.ELarCa;
       //   ELarEf = dsWDELL.ELarEf;
       //   ELarEx = dsWDELL.ELarEx;
       //   ELvLCa = dsWDELL.ELvLCa;
       //Endif;
       // *** End Del ***** 08/24/21 ***********************************

       // *** Beg Add ***** 06/24/24 ***********************************
       // Assign Default values for Collateral Processing
       ELcolwp = EXcolwp;
       ELcolrp = EXcolrp;
       ELcolad = EXcolad;
       // *** End Add ***** 06/24/24 ***********************************

       ELIPgm = Q1PGM;
       ELIUsr = Q1USER;
       ELITm  = Timeto6(SystemTime());
       ELIDt  = Dateto7(SystemDate());

       Write PEL00;
       Chain (ELCO#:ELFND:ELEMP#:ELFYR) pxs00;
       If not %found;
          Setll (ELCO#:ELFND:0:0) pxs00;
          Reade (ELCO#:ELFND:0:0) pxs00;
          Dow not %eof;
             Chain (ELCO#:ELFND:ELEMP#:ELFYR:XSST) pxs00;
             If not %found;
                XSEmp# = ELEmp#;
                XSFyr  = ELFyr;
                XSIPgm = Q1PGM;
                XSIUsr = Q1USER;
                XSIDt  = Dateto7(SystemDate());
                XSPgm  = Q1PGM;
                XSUser = Q1USER;
                XSTime = Timeto6(SystemTime());
                XSDt   = XSIDt;
                Write pxs00;
             Endif;
             Reade (ELCO#:ELFND:0:0) pxs00;
          Enddo;
       Endif;

       Clear PNB00;

       NBCo#  = scCo#;
       NBFnd  = scFnd;
       NBEmp# = scEmp#;

       NBRcdt = dtCYMD(scRecDate);
       NBEndt = Dateto7(SystemDate());

       NBIPgm = Q1PGM;
       NBIUsr = Q1USER;
       NBITm  = Timeto6(SystemTime());
       NBIDt  = Dateto7(SystemDate());

       Write PNB00;

       Clear PAH00;

       AHCo#  = scCo#;
       AHFnd  = scFnd;
       AHEmp# = scEmp#;
       AHDiv  = scDiv;
       AHFyr  = scFyr;

       AHEfdt = ELPfdt;
       If AHEfdt > Dateto7(SystemDate());
          AHEfdt = Dateto7(SystemDate());
       Endif;

       AHStat = 'A';

       AHAgt# = scAgent1;
       AHProd = scProd1;
       AHRt#  = scRate1;

       AHSAgt = scAgent2;
       AHSPrd = scProd2;
       AHSRt# = scRate2;

       AH3Agt = scAgent3;
       AH3Prd = scProd3;
       AH3Rt# = scRate3;

       Select;
          When scControl = 1;
             AHCAF1 = 'Y';
          When scControl1 = 2;
             AHCAF2 = 'Y';
       Endsl;

       AHIPgm = Q1PGM;
       AHPgm  = Q1PGM;
       AHIUsr = Q1USER;
       AHUser = Q1USER;
       AHITm  = Timeto6(SystemTime());
       AHTime = Timeto6(SystemTime());
       AHIDt  = Dateto7(SystemDate());
       AHDt   = Dateto7(SystemDate());

       Write PAH00;

       Clear PQT07;

       QTCo#  = scCo#;
       QTFnd  = scFnd;
       QTEmp# = scEmp#;
       QTDiv  = scDiv;
       QTAgt# = AHAgt#;

       QTStat = agtQteMode;
       If quoteMode = 'Q';
          QTType = 'P';
       Else;
          QTType = 'W';
       EndIf;
       QTpcar = scPcName;
       QTpprm = scPPrem;

       QTWkdt = Dateto7(SystemDate());
       QTOrdt = Dateto7(SystemDate());

       QTEfdt = EMEfd1;

       QTIPgm = Q1PGM;
       QTPgm  = Q1PGM;
       QTIUsr = Q1USER;
       QTUser = Q1USER;
       QTITm  = Timeto6(SystemTime());
       QTTime = Timeto6(SystemTime());
       QTIDt  = Dateto7(SystemDate());
       QTDt   = Dateto7(SystemDate());

       Write PQT07;

       DefaultVendorHistory();

      /end-free
     C                   Endsr                                                  AssignQuote Subr

     P QuotePopup      E
       //===================================================================
       // Screen1 - Screen 1 Processing: Policy Screen
       //===================================================================
     P Screen1         B
     D Screen1         PI
     D*
     D decDivUse       s                   like(ELdiv)
     d decRrnAH        s              9  0
     D divMode         s             10
     d isProcess       s              1n
     d strName         s             40
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D xxBillRtn       s               n   inz(*off)
      *** Beg Add ***** 03/19/24 *******************************
     D xxUpdLoc        s               n
      *** End Add ***** 03/19/24 *******************************


      /free

       Exsr LoadScreen1;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
          If scQteHdg = 'Policy Review';
             isModEffDt = *on;
             Setll (scCo#:scFnd:scEmp#:scFyr) psh01;
             Reade(n) (scCo#:scFnd:scEmp#:scFyr) psh01;
             Dow not %eof;
                If %found(wmshl1);
                   isModEffDt = *off;
                   Leave;
                Endif;
                Reade(n) (scCo#:scFnd:scEmp#:scFyr) psh01;
             Enddo;

             xxxDTE = DateTo7(SystemDate);
             Chain(n) (scCo#:scFnd:scFyr:scEmp#) pea08;
             If %found(wmeal8);
                Dow not %eof(wmeal8);
                   If EAarty = 'B' or EAarty = 'A' or EAarty = 'H'
                    or EAarty = 'F' or EAarty = 'D';
                       If xxPoym < xxCuym or xxTrym < xxCuym;
                          isModEffDt = *off;
                          Leave;
                       Endif;
                    Endif;
                    Reade (scCo#:scFnd:scFyr:scEmp#) pea08;
                 Enddo;
             Endif;

             If PriorYearExists(scCo#:scFnd:scFyr:scEmp#);
                isModEffDt = *off;
             Endif;

             Chain(n) (scCo#:scFnd:scEmp#) pex02;
             If %found(wzexl);
                If PriorYearExists(EXco#:EXoFnd:scFyr:EXoEmp);
                   isModEffDt = *off;
                Endif;
             Endif;

             Chain(n) (scCo#:scFnd:scFyr:scEmp#) psr00;
             If %found(wasrl2);
                Dow not %eof(wasrl2);
                   If %found(wasrl2);
                      isModEffDt = *off;
                      Leave;
                   Endif;
                   Reade (scCo#:scFnd:scFyr:scEmp#) psr00;
                Enddo;
             Endif;

             cxDate = GetCxDate(scCO#:scFND:scEMP#:scDIV);
             If cxDate <> 0;
                isModEffDt = *off;
             Endif;

             Chain (scCo#:scFnd:scFyr:scEmp#) pac00;
             If %found(wmacl2);
                Dow not %eof(wmacl2);
                   If %found(wmacl2);
                      isModEffDt = *off;
                      Leave;
                   Endif;
                   Reade (scCo#:scFnd:scFyr:scEmp#) pac00;
                Enddo;
             Endif;

             If isModEffDt = *off;
                isModBtn = *off;
             Else;
                isModBtn = *on;
             Endif;
          Endif;
       Endif;

       Select;
       When addMode and first_add;
          first_add = *off;
          divMode = 'M';
          xxELRrn = ELRrn;
          Exsr DivisionPopup;
          If divMode = 'M' and xxBillRtn = *off;
             Exsr DeleteQuote;
             btnExit = *On;
             Return;
          Endif;
          xxELRrn = ELRrn;
          Exsr LoadGrid1;
       When divAdd=*On and first_add;
          first_add = *off;
          divMode = 'A';
          Select;
          When mteMode = *On;
            xxCall = '190';
          When qteMode = *On;
            xxCall = 'QTE';
          Other;
            xxCall = 'DIV';
          EndSl;
          xxELRrn = ELRrn;
          Exsr DivisionPopup;
          If xxBillRtn = *On;
            // Division popup in add mode returns RRN of added Employer master record
            Chain(n) xxELrrn pem0p;
            newDiv# = EMdiv; //get added div number
            scDiv = EMdiv; //get added div number
            //*** Begin Chg *** 11/16/22**************************************
            //*** Begin Add *** 09/22/22**************************************
            // scFyr = %Int(%Subst(%char(EMefd1):1:3));
            //*** begin chg ***  04/10/23  ****************************************
            //*****Chain(n) keyWMFNL PFN00;

            //*****Setgt keyWDF1L PF100;
            //*****Readpe keyWDF1L PF100;

            //*****Dow  F1Stpo > EMefd1;
            //*****   Readpe keyWDF1L PF100;
            //*****Enddo;

            //*****scFyr = F1Fyr;
            Setgt (EMCo#:EMFnd:EMEmp#:EMEfd1) pel07;
            Readpe (EMCo#:EMFnd:EMEmp#) pel07;
            scFyr = E3Fyr;
            //*** end chg *****  04/10/23  ****************************************
            //*** End   Add *** 09/22/22**************************************
            //*** End   Chg *** 11/16/22**************************************
            xxELRrn = ELRrn;
            //add workfile record for Division ADD to be used for darts in STLETTER
            Exsr addWxda1p;
            Exsr LoadGrid1;
          Else;
            btnExit = *On;
            Return;
          Endif;
       EndSl;

       Dou btnNext = *on or btnFinish = *on;

      *** Beg Add ***** 03/24/22 **************************
         //*** //*  Process Screen 1
         //*** Exfmt rcdPolicy;
      //* Do not process if
          If (addMode = *on OR divAdd = *On) and
           isFirst1 = *On  and PQBillS <> 'Y' ;
             btnNext = *On;
             isFirst1= *Off;
          Else;
             Exfmt rcdPolicy;                            // Process Screen 1
             //*** beg add ***** 10/10/25 ***********************
             isPrev = *off;
             //*** end add ***** 10/10/25 ***********************
          EndIf;
      *** End Add ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             //* Modify Effective Date click
             When modifyDate = *on;
                Exsr Modify;
                modifyDate = *off;
                Exsr LoadScreen1;

             //* Add Division button click
             When div1Add = *on and
              ChkAppSecurity('Add Division') = *on;
                divMode = 'A';
                xxELRrn = ELRrn;
                Exsr DivisionPopup;
                xxELRrn = ELRrn;
                //* Reload division grid.
                Exsr LoadGrid1;
                div1Add = *off;

             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc div1SFL;
                If not %EOF() and edit;
                   If div1Edit = *on or div1Delete = *on;
                         If div1Edit = *on;
                            divMode = 'C';
                            xxELRrn = scdiv1ELrr;
                            Exsr DivisionPopup;
                            xxELRrn = scdiv1ELrr;
                            div1Edit = *off;
                         Endif;
                         If div1Delete = *on and
                          ChkAppSecurity('Delete Division') = *on;
                            divMode = 'D';
                            xxELRrn = scdiv1ELrr;
                            Exsr DivisionPopup;
                            xxELRrn = scdiv1ELrr;
                            div1Delete = *off;
                         Endif;
                         edit = *off;
                         Exsr LoadGrid1;
                   Endif;
                Endif;

                //*  Edit Quote Popup for Errors.
                If edit;
                   Exsr CheckScreen1;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                Endif;

                If btnNext = *on;
                   screen# = 2;
                   //*** beg add ***** 10/10/25 ***********************
                   isFirst2 = *On;
                   //*** end add ***** 10/10/25 ***********************
                Endif;

          Endsl;

       Enddo;

       If btnNext = *on or btnFinish = *on;
          Exsr SaveScreen1;
          //*** Beg Chg ***** 06/08/22 ***************************
          //If policy year is deleted in Policy review, do not execute AddInfoEdit
          //AddInfoEdit(ELrrn:quoteMode:'1':'1');
          //*** Beg Chg ***** 09/10/25 ***************************
          //*** Chain(n) ELrrn pel0p;
          //*** If %Found(wdelp);
          //***    AddInfoEdit(ELrrn:quoteMode:'1':'1');
          //*** Endif;

          // Call this procedure for each division being added in Policy Entry
          Setll (ELco#:ELfnd:ELemp#) pel02;
          ReadE (ELco#:ELfnd:ELemp#) pel02;
          Dow not %Eof(wdell2);
             AddInfoEdit(dbrrn8:quoteMode:'1':'1');
             ReadE (ELco#:ELfnd:ELemp#) pel02;
          Enddo;
          //*** End Chg ***** 09/10/25 ***************************
          //*** End Chg ***** 06/08/22 ***************************
       Endif;


      /end-free

       //===================================================================
       // addWxda1p  -  Add workfile records WXDA1P
       //               For a division ADD in mid-term endorsement,
       //               add rcd in workfile for future use
       //===================================================================
     C     addWxda1p     Begsr
      /free
          XXEND# = 0;

          Chain DAKY pda1l;
          If NOT %found(WXDA1L);
             Clear pda1p;
             PDCO#  = EmCO# ;
             PDFND  = EmFND ;
             PDEMP# = EmEMP#;
             PDDIV  = decDivStRpt;         //State reporting division
             //*** Begin Chg *** 09/22/22****************************
             // PDFYR  = ELFYR ;
             PDfyr  = scFyr ;
             //*** End   Chg *** 09/22/22****************************
             PDNWDV = EmDIV ;              //new division#
             PDEND# = XXEND#;              //endorsement#
             //*** Begin Chg *** 09/22/22****************************
             // PDEFDT = ELPFDT;              //Endorsement eff date/pol eff date
             PDefdt = EMefd1;              //Endorsement eff date/pol eff date
             //*** End   Chg *** 09/22/22****************************
             PDNAME = EmNAME;              //employer name
             PDJOBN = Q1JOBN;
             PDUSER = Q1USER;
             PDPGM  = Q1PGM ;
             PDTIME = Timeto6(SystemTime());
             PDDT   = Dateto7(SystemDate());

             Write  pda1p;
             unlock wxda1p;
           EndIf;

           EndSr;

      /end-free

       //===================================================================
       // ClearScreen1 - Clear error/button fields
       //===================================================================
     C     ClearScreen1  Begsr
      /free

       error      = *off;

       erPFDT     = *off;
       msPFDT     = *blanks;
       erPTDT     = *off;
       msPTDT     = *blanks;
       erANDT     = *off;
       msANDT     = *blanks;
       erRecDate  = *off;
       msRecDate  = *blanks;
       erCOFL     = *off;
       msCOFL     = *blanks;


      /end-free
     C                   Endsr                                                  ClearScreen1 Subr

       //===================================================================
       // CheckScreen1 - Check Policy Screen for Errors
       //===================================================================
     C     CheckScreen1  Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       Exsr ClearScreen1;

       If divAdd = *Off;
          If dateRecVis = *on or scRecDate <> 0;
             erRecDate = ValidDate(scRecDate:message);
             If erRecDate = *on;
                msRecDate = message;
                error     = *on;
             Endif;
          Endif;
       Endif;

      /end-free
     C                   Endsr                                                  CheckScreen1 Subr

       //===================================================================
       // LoadScreen1 - Load the initial fields for policy screen
       //===================================================================
     C     LoadScreen1   Begsr
      /free

       Chain(n) keyWMFNL PFN00;
       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       // Now that clearance screen has been passed, update entry parms for policy summary call.
       If q1prms >= 2 and addMode;
          @@Rrn = %editc(elRRN : 'X');
          @@Mode = 'C';
       Endif;

       scFnd_D = %char(scFnd) + ' - ' + GetGrpName(scFnd);

       Select;
       //When addMode;
       //   scPFDT = scIncepDte;
       //   scPTDT = scExpDte;
       //   scANDT = scAnnivDte;
       When mteMode = *On;
          scPFDT = LDeedt;
          scPTDT = dtMDYY(ELPtdt);
          scANDT = dtMDYY(ELAndt);
       Other;
          scPFDT = dtMDYY(ELPfdt);
          scPTDT = dtMDYY(ELPtdt);
          scANDT = dtMDYY(ELAndt);
          scCOFL = ELCofl;
          scRecDate = dtMDYY(NBRcdt);
       EndSl;

       dateRecVis = *off;
       If qteMode = *on and ELRQFL <> 'Y';
          dateRecVis = *on;
       Endif;

       btnFinish  = *off;
       btnNext    = *off;
       modifyDate = *off;
       div1Add    = *off;

       Exsr LoadGrid1;

       Exsr ClearScreen1;

      /end-free
     C                   Endsr                                                  LoadScreen1 Subr

       //===================================================================
       // LoadGrid1 - Load the billing grid for policy screen
       //===================================================================
     C     LoadGrid1     Begsr
      /free

       // Clear the subfile.
       clrDiv1 = *On;
       Write rcdPolicy;
       clrDiv1 = *Off;
       rrnDiv1 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd = *Off Or (divAdd = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv1Div  =  EMDiv;
             scDiv1DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv1Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv1InDt =  dtMDYY(GetEmIncDt(scCo#:scFnd:scEmp#:EMDiv));
             scDiv1CxDt =  dtMDYY(GetCxDate(scCo#:scFnd:scEmp#:EMDiv));
             //*** Begin Add *** 09/22/22***************************************
             scPFDT =  dtMDYY(ELpfdt) ;
             scPTDT =  dtMDYY(ELptdt) ;
             //*** End   Add *** 09/22/22***************************************
             Clear scdiv1PAdr;
             If EMPAD1 <> ' ';
                scDiv1PAdr =  %trimr(EMPAD1) + ' ' + %trimr(EMPAD2)  + ' '
                 + %trimr(EMPCTY) + ', ' + EMPST + ' '
                 + %editw(EMPZIP:'     -    ');
             Endif;
             Clear scdiv1MAdr;
             scDiv1MAdr =  %trimr(EMMAD1) + ' ' + %trimr(EMMAD2)  + ' '
              + %trimr(EMMCTY) + ', ' + EMMST + ' '
              + %editw(EMMZIP:'     -    ');
             scDiv1Fein =  EMFein;
             scDiv1ELrr =  dbrrn;
             scDiv1EMrr =  dbrrn2;
             scDiv1RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv1STRP =  'Yes';
             Else;
                scDiv1STRP =  'No';
             Endif;

             rrnDiv1 =  rrnDiv1 + 1;
             Write div1SFL;

          Endif;
        Endif;
        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv1 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid1 Subr

       //===================================================================
       // DeleteQuote - Delete the shell records and let program exit
       //===================================================================
     C     DeleteQuote   Begsr
      /free

       Chain keyWMEML PEM00;
       If %found and inMODE <> 'Y';
          Delete PEM00;
       Endif;
       Chain keyWDELL PEL00;
       If %found;
          Delete PEL00;
       Endif;
       Chain keyWMNBL PNB00;
       If %found;
          Delete PNB00;
       Endif;
       Chain keyWMAHL  PAH00;
       If %found;
          Delete PAH00;
       Endif;
       Chain keyWAQTL7 PQT07;
       If %found;
          Delete PQT07;
       Endif;
       //*** begin change ***  05/04/21 *********************************
       //Setll keyWMEML pp300;
       //Reade keyWMEML pp300;
       //Dow not %eof;
       //   Delete pp300;
       //   Reade keyWMEML pp300;
       //Enddo;
       Setll keyWMEML pp300;
       Reade keyWMEML pp300 WMP3LDS;
       Dow not %eof;
          ArchiveWMP3P(WMP3LDS:'DELETE');
          Delete pp300;
          Reade keyWMEML pp300 WMP3LDS;
       Enddo;
       //*** end   change ***  05/04/21 *********************************
       If %Open(WDVHL1);
          Close WDVHL1;
       Endif;
       If qteMode = *On;
          Monitor;
             strCommand = 'DLTOVR FILE(WDVHL1)';
             Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
          On-Error;
          EndMon;
       Endif;
       Open WDVHL1;
       Setll keyWDELL pvh01;
       Reade keyWDELL pvh01;
       Dow Not %Eof;
          Delete pvh01;
          Reade keyWDELL pvh01;
       Enddo;
       Close WDVHL1;

      /end-free
     C                   Endsr                                                  DeleteQuote Subr

       //===================================================================
       // DivisionPopup - Load the location master popup for division add/edit/delete
       //===================================================================
      /Free
       Begsr DivisionPopUp;
        xxBillRtn = DspBillDiv(xxELrrn:xxCall:divMode);
      *** Beg Add ***** 03/19/24 *******************************
        If divMode = 'C';
           Chain(n) xxELrrn pem0p;
           Chain(n) (scCo#:scFnd:scEmp#:EMdiv) pal00;
           If %found();
              xxUpdLoc = *on;
              Chain(n) key2WMEML pem00;
              Exsr AddDefLoc;
              xxUpdLoc = *off;
           Endif;
        Endif;
      *** End Add ***** 03/19/24 *******************************
       Endsr;
      /End-Free
       //===================================================================
       // Modify - Call the Modify policy effective date program.
       //===================================================================
     C     Modify        Begsr
     C*
     C
      /Free
       outPRDS  = ' ';
       outCALL  = ' ';
       outCO#  = scCo#;
       outFND  = scFnd;
       outEMP#  = scEmp#;
       outDIV  = scDiv;
       outFYR  = scFyr;

       WtPolPf(outprds:outcall:outKC:outKL:outCo#:outFnd:outEmp#:outDiv:outFyr);
      /End-free
     C*
      /free

       Chain(n) ELrrn pel0p;
       scFyr = ELFyr;

      /end-free
     C*
     C                   Endsr                                                  Modify Subr

       //===================================================================
       // SaveScreen1 - Save the initial fields for policy screen
       //===================================================================
     C     SaveScreen1   Begsr
      /free

       //*** Beg Del ***** 08/24/21 ************************************
       //Chain keyWDELL PEL00;

       //If addMode;
       //   Chain keyWMRSL prs00;
       //   SetGt keyWDRRL prr00;
       //   Readpe keyWDRRL1 prr00;
       //   If not %eof(WDRRL);
       //      ELwvtp = RRwdtp;
       //      ELwvpc = RRwdpc;
       //      ELfchg = RRwdfc;
       //      ELwmin = RRwdmn;
       //      ELwvfl = RRwvfl;
       //      ELSfpc = RRSfpc;
       //      ELSfmn = RRSfam;
       //      If RRDV1DFT <> 0 and RRDV1C <> 0;
       //         ELASP2 = RRDV1DFT;
       //         ELDVEF = ELPFDT;
       //      Endif;
       //      If RRDV2DFT <> 0 and RRDV2C <> 0;
       //         ELDEV2 = RRDV2DFT;
       //         ELD2EF = ELPFDT;
       //      Endif;
       //   Endif;
       //Endif;

       //If divAdd = *On;
       //   Chain keyWMRSL prs00;
       //   SetGt keyWDRRL prr00;
       //   Readpe keyWDRRL1 prr00;
       //   If not %eof(WDRRL);
       //      If RRDV1DFT <> 0 and RRDV1C <> 0;
       //         ELASP2 = RRDV1DFT;
       //         ELDVEF = ELPFDT;
       //      Endif;
       //      If RRDV2DFT <> 0 and RRDV2C <> 0;
       //         ELDEV2 = RRDV2DFT;
       //         ELD2EF = ELPFDT;
       //      Endif;
       //   Endif;
       //Endif;

       //ELPgm  = Q1PGM;
       //ELUser = Q1USER;
       //ELTime = Timeto6(SystemTime());
       //ELDt   = Dateto7(SystemDate());

       //If %found(WDELL);
       //   Update PEL00;
       //Endif;
       //*** End Del ***** 08/24/21 ************************************

       Chain keyWMNBL PNB00;

       If not %found;
          Clear PNB00;
          NBCo#  = scCo#;
          NBFnd  = scFnd;
          NBEmp# = scEmp#;

          NBEndt = Dateto7(SystemDate());

          NBIPgm = Q1PGM;
          NBIUsr = Q1USER;
          NBITm  = Timeto6(SystemTime());
          NBIDt  = Dateto7(SystemDate());
       Endif;

       NBRcdt = dtCYMD(scRecDate);

       NBPgm  = Q1PGM;
       NBUser = Q1USER;
       NBTime = Timeto6(SystemTime());
       NBDt   = Dateto7(SystemDate());

       If not %found;
          Write PNB00;
       Else;
          Update PNB00;
       Endif;

       // Write an agent history and quote status record for each division.

       // Only one of these should be used
       decDivUse = decDivFrom;
       decDivUse = decDivStRpt;
       AHagt# = GetEmpAgt(scCo#:scFnd:scEmp#:decDivUse:scFyr:EMefD1:
        strName:decRrnAH);
       strStRptQTtype =
        GetQuoteSts(scCo#:scFnd:scEmp#:decDivUse:scFyr:agtQteMode);
       foundAgt = AHagt# <> *zero;
       foundQte = strStRptQTtype <> *blank;

       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

          Select;
          When divAdd = *on and EMdiv = scDiv;
             isProcess = *on;
          When divAdd = *off and EMdiv <> scDiv;
             isProcess = *on;
          Other;
             isProcess = *off;
          Endsl;

          If isProcess;

             Chain(n) key3WMAHL PAH00;
             If not %found and foundAgt;
                Chain decRrnAH wmahp;
                AHDiv = EMDiv;
                AHIPgm = Q1PGM;
                AHPgm  = Q1PGM;
                AHIUsr = Q1USER;
                AHUser = Q1USER;
                AHITm  = Timeto6(SystemTime());
                AHTime = Timeto6(SystemTime());
                AHIDt  = Dateto7(SystemDate());
                AHDt   = Dateto7(SystemDate());
                Write PAH00;
             Endif;

             Chain(n) key1WAQTL7 PQT07;
             If not %found and foundQte;
                Clear pqt07;
                QTCo#  = scCo#;
                QTFnd  = scFnd;
                QTEmp# = scEmp#;
                QTDiv = EMDiv;
                QTname = EMname;
                QTefDt = EMefD1;
                QTchap = EMchap;
                QTagt# = AHagt#;
                QTtype = strStRptQTtype;
                QTstat = agtQteMode;
                QTwkDt = Dateto7(SystemDate());
                QTorDt = Dateto7(SystemDate());
                QTefDt = EMEfD1;
                QTpcar = scPcName;
                QTpprm = scPPrem;
                QTIPgm = Q1PGM;
                QTPgm  = Q1PGM;
                QTIUsr = Q1USER;
                QTUser = Q1USER;
                QTITm  = Timeto6(SystemTime());
                QTTime = Timeto6(SystemTime());
                QTIDt  = Dateto7(SystemDate());
                QTDt   = Dateto7(SystemDate());
                Write PQT07;
             Endif;

             DefaultVendorHistory();

          Endif;

          // Add default location for division if flagged.
          If FNACLR <> ' ';
             Exsr AddDefLoc;
          Endif;

          Reade(n) key1WMEML PEM00;
       Enddo;

      /end-free
     C                   Endsr                                                  SaveScreen1 Subr

       //===================================================================
       // AddDefLoc - Write the default location for the division if flagged.
       //===================================================================
     C     AddDefLoc     Begsr
      /free

       xxELID = tone;

       Chain key1WMALL PAL00;

      *** Beg Chg ***** 03/19/24 *******************************
       //***If not %found;
       If not %found or xxUpdLoc = *on;
      *** End Chg ***** 03/19/24 *******************************
          Clear PAL00;

          ALCO#  = EMCO#;
          ALFND  = EMFND;
          ALEMP# = EMEMP#;
          ALDIV  = EMDIV;
          ALELID = tone;
          ALELTP = FNACLR;
          If FNACLR = 'Y';
             ALELTP = 'L';
          Endif;
          ALEFDT = EMEFD1;
          ALNAME = EMNAME;
          ALNAM2 = EMNAM2;
          ALFDNM = EMFDNM;
          ALFDN2 = EMFDN2;
          ALMAD1 = EMMAD1;
          ALMAD2 = EMMAD2;
          ALMCTY = EMMCTY;
          ALMST  = EMMST;
          ALMZIP = EMMZIP;
          ALPAD1 = EMPAD1;
          ALPAD2 = EMPAD2;
          ALPCTY = EMPCTY;
          ALPST  = EMPST;
          ALPZIP = EMPZIP;
          ALTPOB = EMTPOB;
          ALFEIN = EMFEIN;
          ALNCCI = 'N';
          ALAREA = EMAREA;
          ALPHN# = EMPHN#;
          ALFARA = EMFAXA;
          ALFAX# = EMFAX#;
          ALCNTY = EMCNTY;
          ALSEMP = EMSEMP;
          ALSTAT = 'A';
          ALSIC  = EMSIC;
          ALNAIC = EMNAIC;
          ALUE#  = EMUE#;
          ALNAOB = EMNAOB;
          ALSHFT = EMSHFT;
          ALNAMP = EMNAMP;
          ALFDNP = EMFDNP;

          ALIPgm = Q1PGM;
          ALPgm  = Q1PGM;
          ALIUsr = Q1USER;
          ALUser = Q1USER;
          ALITm  = Timeto6(SystemTime());
          ALTime = Timeto6(SystemTime());
          ALIDt  = Dateto7(SystemDate());
          ALDt   = Dateto7(SystemDate());

      *** Beg Chg ***** 03/19/24 *******************************
          //***Write PAL00;
          //*** Beg Chg ***** 05/31/24 *******************************
          //*** If xxUpdLoc = *on;
          If %found(wmall);
          //*** End Chg ***** 05/31/24 *******************************
             Update pal00;
          Else;
             Write pal00;
          Endif;
      *** End Chg ***** 03/19/24 *******************************

       Endif;

      /end-free
     C                   Endsr                                                  AddDefLoc Subr

     P Screen1         E
      **********************************************************************************************

       //===================================================================
       // Screen2 - Screen 2 Processing: Rating Screen
       //===================================================================
     P Screen2         B
     D Screen2         PI
     D*
     D divMode         s             10
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D sched           s              1    inz(' ')
     D large           s              1    inz(' ')
     D retro           s              1    inz(' ')
     D isCltFlg        s              1n   inz
      /free

       Exsr LoadScreen2;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;
       isCltFlg = *off;
       If clientID = '132' or clientID ='932';
          isCltFlg = *on;
       Endif;
      *** Beg Chg ***** 03/24/22 **************************
       //Select;
       //When addMode and second_add;
       //   second_add = *off;
       //   second_default = *on;
       //   Setll keyWDELL1 PEL01;
       //   Reade(n) keyWDELL1 PEL01;
       //   Dow not %eof;
       //      xxELRrn = dbrrn7;
       //      Exsr RatingPopup;
       //      If second_default = *on;
       //         //*** Beg Del ***** 08/24/21 *******************************
       //         //Exsr Ratingdefault;
       //         //*** End Del ***** 08/24/21 *******************************
       //         second_default = *off;
       //      Endif;
       //      Reade(n) keyWDELL1 PEL01;
       //   Enddo;
       //   Exsr LoadGrid2;
       //When divAdd =*On and second_add;
       //   second_add = *off;
       //   scDiv = newDiv#;
       //   Setll key1WDELL1 PEL01;
       //   Reade(n) key1WDELL1 PEL01;
       //   Dow not %eof;
       //      xxELRrn = dbrrn7;
       //      Exsr RatingPopup;
       //      Reade(n) key1WDELL1 PEL01;
       //   Enddo;
       //   Exsr LoadGrid2;
       //EndSl;

       If (addMode = *on OR divAdd = *On) and
        isFirst2 = *On  and PQRateW <> 'Y' ;
       Else;
          Select;
          When addMode and second_add;
             second_add = *off;
             second_default = *on;
             Setll keyWDELL1 PEL01;
             Reade(n) keyWDELL1 PEL01;
             Dow not %eof;
                xxELRrn = dbrrn7;
                Exsr RatingPopup;
                If second_default = *on;
                   //*** Beg Add ***** 12/12/22 **************************
                   If divAdd = *off;
                      Exsr RatingInfo;
                   Endif;
                   second_default = *off;
                   //*** End Add ***** 12/12/22 **************************
                Endif;
                Reade(n) keyWDELL1 PEL01;
             Enddo;
             Exsr LoadGrid2;
          When divAdd =*On and second_add;
             second_add = *off;
             scDiv = newDiv#;
             Setll key1WDELL1 PEL01;
             Reade(n) key1WDELL1 PEL01;
             Dow not %eof;
                xxELRrn = dbrrn7;
                Exsr RatingPopup;
                Reade(n) key1WDELL1 PEL01;
             Enddo;
             Exsr LoadGrid2;
          EndSl;
       EndIf;
      *** End Chg ***** 03/24/22 **************************

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;
      *** Beg Add ***** 03/24/22 **************************
          If (addMode = *on OR divAdd = *On) and
           isFirst2 = *On  and PQRateS <> 'Y' ;
             btnNext = *On;
             isFirst2  = *Off;
             //*** beg add ***** 10/10/25 ***********************
             If isPrev = *on;
                btnPrev = *on;
                btnNext = *off;
             Endif;
             //*** end add ***** 10/10/25 ***********************
          Else;
             Exfmt rcdRating;
             //*** beg add ***** 10/10/25 ***********************
             isPrev = *off;
             //*** end add ***** 10/10/25 ***********************
          EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
         //*** //*  Process Screen 2
         //***  Exfmt rcdRating;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc div2SFL;
                If not %EOF() and edit;
                   If div2Edit = *on or div2Delete = *on;
                         If div2Edit = *on;
                            xxELRrn = scdiv2ELrr;
                            Exsr RatingPopup;
                            div2Edit = *off;
                         Endif;
                         //*** Beg Chg ***** 06/08/22 *********************
                         //If div2Delete = *on and
                         // ChkAppSecurity('Delete Division') = *on;
                         //   divMode = 'D';
                         //   xxELRrn = scdiv2ELrr;
                         //   Exsr DivisionPopup;
                         //   xxELRrn = scdiv2ELrr;
                         //   div2Delete = *off;
                         //Endif;
                         If div2Delete = *on;
                            divMode = 'D';
                            xxELRrn = scdiv2ELrr;
                            DeletePolYear(xxELRrn);
                            xxELRrn = scdiv2ELrr;
                            div2Delete = *off;
                         Endif;
                         //*** End Chg ***** 06/08/22 *********************
                         edit = *off;
                         Exsr LoadGrid2;
                   Endif;
                Endif;

                //*  Edit Rating Screen for Errors.
                If edit;
                   Exsr CheckScreen2;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   screen# = 1;
                   isFirst1 = *On;
                Endif;

                If btnNext = *on;
                   If divAdd = *on;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       ' ':sched:large:retro);
                   Else;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       'Y':sched:large:retro);
                   Endif;
                   Select;
                      When sched = 'Y';
                         screen# = 10;
                      When large = 'Y';
                         screen# = 3;
                      When retro = 'Y';
                         screen# = 4;
                      Other;
                         screen# = 5;
                   Endsl;
                Endif;

          Endsl;

       Enddo;



      /end-free

       //===================================================================
       // ClearScreen2 - Clear error/button fields
       //===================================================================
     C     ClearScreen2  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen2 Subr

       //===================================================================
       // CheckScreen2 - Check Policy Screen for Errors
       //===================================================================
     C     CheckScreen2  Begsr
      /free

       Exsr ClearScreen2;


      /end-free
     C                   Endsr                                                  CheckScreen2 Subr


       //===================================================================
       // LoadScreen2 - Load the initial fields for rating screen
       //===================================================================
     C     LoadScreen2   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGrid2;

       Exsr ClearScreen2;

      /end-free
     C                   Endsr                                                  LoadScreen2 Subr

       //===================================================================
       // LoadGrid2 - Load the billing grid for policy screen
       //===================================================================
     C     LoadGrid2     Begsr
      /free

       // Clear the subfile.
       clrDiv2 = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 2;
          visNxtsc2 = *Off;
          leftCn1S2 = 995;
       Else;
          visNxtsc2 = *On;
          leftCn1S2 = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdRating;
       clrDiv2 = *Off;
       rrnDiv2 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd = *Off Or (divAdd = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv2Fyr  =  scFyr;
             scDiv2Div  =  EMDiv;
             scDiv2DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv2Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv2PolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDiv2Fein =  EMFein;
             scDiv2ELrr =  dbrrn;
             scDiv2EMrr =  dbrrn2;
             scDiv2RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv2STRP =  'Yes';
             Else;
                scDiv2STRP =  'No';
             Endif;

             rrnDiv2 =  rrnDiv2 + 1;
             Write div2SFL;

           Endif;
          Endif;

          Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv2 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid2 Subr

       //===================================================================
       // DivisionPopup - Load the location master popup for division add/edit/delete
       //===================================================================
     C     DivisionPopup Begsr
      /free

          DspBillDiv(xxELrrn:xxCall:divMode);

      /end-free
     C                   Endsr                                                  DivisionPopup Subr

       //===================================================================
       // RatingPopup - Load the rating popup for division edit
       //===================================================================
     C     RatingPopup   Begsr
      /free

          RatingEdit(xxELrrn:div2Edit);

      /end-free
     C                   Endsr                                                  RatingPopup Subr

       //*** Beg Add **** 12/12/22 ************************************=====
       //===================================================================
       // Update Rating information for all the divisions
       // Old (Ratingdefault SR)
       //===================================================================
     C     RatingInfo    Begsr
      /free

       Chain(n) dbrrn7 PEL0P;
       //When Rating State change, carry forward only valid Rating Values.
       Reset dsWDRRLc;
       CHAIN (ELco#:ELrtcd) PRS00;
       IF %found(WMRSL);
          xxDate = 0;
          xxDate = dtCYMD(scPFDT);
          Setgt (ELco#:ELfnd:RSjrst:xxDate) PRR00;
          Readpe (ELCO#:ELfnd:RSjrst) PRR00 dsWDRRLc;
       ENDIF;

           Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Dow not %eof(wdell8);
              If ELdiv <> E2div;
                 RSjrst = ' ';
                 Chain (E2co#:E2rtcd) prs00;
                 If %found(WMRSL);
                    xxSt = RSjrst;
                 Endif;

                 SetGt (E2co#:E2fnd:xxST:E2pfdt) prr00;
                 Readpe (E2co#:E2fnd:xxST) prr00;

                 //Manual Premium%
                 E2manP = ELmanP;

                 //Client Use Flag
                 E2fFl2 = ELfFl2;

                 //Comm Mod Multiplier
                 E2cMul = ELcMul;

                 //Deductible discount
                 E2cxBl = ELcxBl;
                 E2dCde = ELdCde;
                 E2dCd2 = ELdCd2;
                 E2dCEf = ELdCEf;
                 E2dCEx = ELdCEx;
                 E22DEf = EL2DEf;
                 E22DEx = EL2DEx;
                 If E2dCde <> *Blanks;
                    E2dCEf = ELpfdt;
                 Endif;
                 If E2dCd2 <> *Blanks;
                    E22DEf = ELpfdt;
                 Endif;
                 Exsr ChkDeductibleCode;

                 //Contractors Adj Credit
                 E2asp1 = ELasp1;
                 E2crEf = ELcrEf;
                 E2crEx = ELcrEx;
                 If E2asp1 <> *zeros;
                    E2cref = ELpfdt;
                 Endif;

                 //Stabilization Factor
                 E2stFc = ELstFc;

                 //Subject premium credit
                 E2aspM = ELaspM;
                 E2mcEf = ELmcEf;
                 E2mcEx = ELmcEx;
                 E2aspS = ELaspS;
                 E2scEf = ELscEf;
                 E2scEx = ELscEx;
                 If E2aspM <> *zeros;
                    E2mcef = ELpfdt;
                 Endif;
                 If E2aspS <> *zeros;
                    E2scEf = ELpfdt;
                 Endif;

                 // Experience modifiers
                 E2exm1 = ELexm1;
                 E2emd1 = ELemd1;
                 E2exm2 = ELexm2;
                 E2emd2 = ELemd2;
                 E2exm3 = ELexm3;
                 E2emd3 = ELemd3;

                 //Arap #
                 //*** Beg Add ***** 02/27/25 ******************************************
                 If xxST = 'MA';
                 //*** End Add ***** 02/27/25 ******************************************
                 E2aRp1 = ELaRp1;
                 E2aRp2 = ELaRp2;
                 E2aRp3 = ELaRp3;
                 //*** Beg Add ***** 02/27/25 ******************************************
                 Endif;
                 //*** End Add ***** 02/27/25 ******************************************

                 //Exp Mod  1 Not Final Flag
                 E2exMd1Nf = ELexMd1Nf;
                 E2exMd2Nf = ELexMd2Nf;
                 E2exMd3Nf = ELexMd3Nf;

                 //Stock Discount code
                 E2asp3 = ELasp3;
                 E2disc = ELdisc;
                 E2disP = ELdisP;
                 If E2asp3 = *Zeros;
                    Clear E2disc;
                    If dsWDRRlc.RRdisc <> *Blank;
                       E2disc = dsWDRRlc.RRdisc;
                    Else;
                       E2asp3 = dsWDRRlc.RRdspc;
                    Endif;
                 Endif;

                 // Membership Fee
                 E2mFEE = ELmFEE;

                 // Facultative Reinsurance
                 E2arCp = ELarCp;
                 E2arCa = ELarCa;       //Flat Amount
                 E2arEf = ELarEf;
                 E2arEx = ELarEx;
                 If E2arCp <> *zeros;
                    E2arEf = ELpfdt;
                 Endif;

                 //Vol Labor Charge Amt
                 E2vLCa = ELvLCa;

                 //Mco Type Code/Mco Group Code
                 If FNpeof <> *Blank;
                    E2mcCd = ELmcCd;
                    E2mcGp = ELmcGp;
                    //Internal Promulgation
                    E2prom = ELprom;
                 Else;
                    Clear E2mcCd;
                    Clear E2mcGp;
                    Clear E2prom;
                 Endif;

                 //Waiver Type/Waiver %Waiver Flat Charge
                 E2wvTp = ELwvTp;
                 E2wvPc = ELwvPc;
                 E2fChg = ELfChg;
                 E2wMin = ELwMin;    //Minimum Waiver Surcharge
                 E2wMax = ELwMax;    //Waiver Max Chrg
                 E2wvFl = ELwvFl;    //Waiver Surchrg Prem Lvl

                 //Safety Fee Percent/Min Amount
                 E2sFPc = ELsFPc;
                 E2sFMn = ELsFMn;

                 //Safety Fee Percent/Min Amount
                 If dsWDRRLc.RRsfpc <> *zeros;
                    E2Sfpc = dsWDRRLc.RRSfpc;
                    E2Sfmn = dsWDRRLc.RRSfam;
                 Endif;

                 //Default Large Deductible information from
                 //State Reporting Division
                 // If flgLargeDeduct = *on;
                 // Exsr LargeDeductible;
                 // Endif;

                 //Default Sunz Manual premium percentage from Group setup
                 If clientID = '150';
                    Chain(n) (ELCo#:ELFnd:ELFyr) pf100;
                    If %found(wdf1l);
                     E2ManP = F1ManP;
                    Endif;
                 Endif;

                 //Increased Limits code
                 E2iLCd = ELiLCd;
                 E2inL2 = ELinL2;
                 E2iLEf = ELiLEf;
                 E2iLEx = ELiLEx;
                 E2i2Ef = ELi2Ef;
                 E2i2Ex = ELi2Ex;
                 If E2iLCd <> ' ';
                    E2iLEf = ELpfdt;
                 Endif;
                 If E2inL2 <> ' ';
                    E2i2Ef = ELpfdt;
                 Endif;
                 Exsr ChkIncrLimitCode;

                 //Schedule Db/cr Factor
                 E2dbCr = ELdbCr;
                 E2sdEf = ELsdEf;
                 E2sdEx = ELsdEx;
                 If E2dbcr <> *zeros;
                    E2sdEf = ELpfdt;
                 Endif;

                 //Deviation
                 E2asp2 = ELasp2;
                 If E2asp2 <> 0;
                    E2dvEf = ELpfdt;
                    E2dvEx = 0;
                 Endif;
                 E2dev2 = ELdev2;
                 If E2dev2 <> 0;
                    E2d2ef = ELpfdt;
                    E2d2ex = 0;
                 Endif;

                 E2Pgm  = Q1PGM;
                 E2User = Q1USER;
                 E2Time = Timeto6(SystemTime());
                 E2Dt   = Dateto7(SystemDate());

                 Update pel08;

              Endif;

           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Enddo;

      /end-free
     C                   Endsr
       //*** End Add **** 12/12/22 ************************************=====
       //===================================================================
       // Update Rating information for all the divisions
       //===================================================================
     C     RatingDefault Begsr
      /free

       Chain(n) dbrrn7 PEL0P;
       //When Rating State change, carry forward only valid Rating Values.
       CLEAR oWDRRLds;
       CHAIN (ELco#:ELrtcd) PRS00;
       IF %found(WMRSL);
          xxDate = 0;
          xxDate = dtCYMD(scPFDT);
          Setgt (ELco#:ELfnd:RSjrst:xxDate) PRR00;
          Readpe (ELCO#:ELfnd:RSjrst) PRR00 oWDRRLds;
       ENDIF;

           Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Dow not %eof(wdell8);
              If ELdiv <> E2div;
                 RSjrst = ' ';
                 Chain (E2co#:E2rtcd) prs00;
                 If %found;
                    xxSt = RSjrst;
                 Endif;

                 SetGt (E2co#:E2fnd:xxST:E2pfdt) prr00;
                 Readpe (E2co#:E2fnd:xxST) prr00;

                 If ELilcd <> *blanks;
                    xxKey = ELilcd;
                    Setgt (ELco#:ELfnd:xxSt:xxKey:ELpfdt) plm01;
                    Readpe (ELco#:ELfnd:xxSt:xxKey) plm01;
                    Dow not %eof and LMSTAT = 'I';
                    Readpe (ELco#:ELfnd:xxSt:xxKey) plm01;
                    Enddo;
                    If not %eof;
                    E2ilcd = ELilcd;
                    If E2ilcd <> *Blank;
                      If ELilef < E2PFDT;
                        If ELilef <> 0;
                           E2ilef = E2PFDT;
                        Else;
                           E2ilef = *Zero;
                        Endif;
                      Else;
                         E2ilef = ELilef;
                      Endif;

                      If ELilex > E2PTDT;
                         E2ilex = E2PTDT;
                      Else;
                        If ELilex <> 0;
                           If ELilef < E2PFDT and ELilex < E2PFDT;
                              E2ilef = *Zero;
                              E2ilex = *Zero;
                              E2ilcd = *Blank;
                           Else;
                              E2ilex = ELilex;
                           Endif;
                        Else;
                           E2ilex = *Zero;
                        Endif;
                      Endif;
                    Endif;
                    Endif;
                 Endif;
                 If ELinl2 <> *Blanks;
                    xxKey = ELinl2;
                    SetGt (Elco#:ELfnd:xxSt:xxKey:ELpfdt) plm01;
                    Readpe (Elco#:ELfnd:xxSt:xxKey) plm01;
                    Dow not %eof and LMSTAT = 'I';
                    Readpe (ELco#:ELfnd:xxSt:xxKey) plm01;
                    Enddo;
                    If not %eof;
                    E2inl2 = ELinl2;
                    If E2inl2 <> *Blank;
                      If ELi2ef < E2PFDT;
                        If ELi2ef <> 0;
                           E2i2ef = E2PFDT;
                        Else;
                           E2i2ef = *Zero;
                        Endif;
                      Else;
                         E2i2ef = ELi2ef;
                      Endif;

                      If ELi2ex > E2PTDT;
                         E2i2ex = E2PTDT;
                      Else;
                        If ELi2ex <> 0;
                           If ELi2ef < E2PFDT and ELi2ex < E2PFDT;
                              E2i2ef = *Zero;
                              E2i2ex = *Zero;
                              E2inl2 = *Blank;
                           Else;
                              E2i2ex = ELi2ex;
                           Endif;
                        Else;
                           E2i2ex = *Zero;
                        Endif;
                      Endif;
                    Endif;
                    Endif;
                 Endif;
                 If ELdcde <> *Blanks;
                    xxDdCd = ELdcde;
                    SetGt (Elco#:ELfnd:xxSt:xxDdCd:ELpfdt) pdd02;
                    Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                    Dow not %eof and DDSTAT = 'I';
                    Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                    Enddo;
                    If not %eof;
                    E2dcde = ELdcde;
                    If E2dcde <> *Blank;
                      E2cxbl = ELcxbl;
                      If ELdcef < E2PFDT;
                        If ELdcef <> 0;
                           E2dcef = E2PFDT;
                        Else;
                           E2dcef = *Zero;
                        Endif;
                      Else;
                         E2dcef = ELdcef;
                      Endif;

                      If ELdcex > E2PTDT;
                         E2dcex = E2PTDT;
                      Else;
                        If ELdcex <> 0;
                           If ELdcef < E2PFDT and ELdcex < E2PFDT;
                              E2dcef = *Zero;
                              E2dcex = *Zero;
                              E2dcde = *Blank;
                           Else;
                              E2dcex = ELdcex;
                           Endif;
                        Else;
                           E2dcex = *Zero;
                        Endif;
                      Endif;
                    Endif;
                    Endif;
                 Endif;
                 If ELdcd2 <> *blanks;
                    xxDdCd = ELdcd2;
                    SetGt (Elco#:ELfnd:xxSt:xxDdCd:ELpfdt) pdd02;
                    Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                    Dow not %eof and DDSTAT = 'I';
                    Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                    Enddo;
                    If not %eof;
                    E2dcd2 = ELdcd2;
                    If E2dcd2 <> *Blank;
                      If EL2def < E2PFDT;
                        If EL2def <> 0;
                           E22def = E2PFDT;
                        Else;
                           E22def = *Zero;
                        Endif;
                      Else;
                         E22def = EL2def;
                      Endif;

                      If EL2dex > E2PTDT;
                         E22dex = E2PTDT;
                      Else;
                        If EL2dex <> 0;
                           If EL2def < E2PFDT and EL2dex < E2PFDT;
                              E22def = *Zero;
                              E22dex = *Zero;
                              E2dcd2 = *Blank;
                           Else;
                              E22dex = EL2dex;
                           Endif;
                        Else;
                           E22dex = *Zero;
                        Endif;
                      Endif;
                    Endif;
                    Endif;
                 Endif;

                 If ELdisc <> *blanks;
                    SetGt (ELco#:ELfnd:xxSt:ELdisc:ELpfdt) pds03;
                    Readpe (ELco#:ELfnd:xxSt:ELdisc) pds03;
                    If not %eof;
                    E2disc = ELdisc;
                    Endif;
                 Endif;
                    E2ASP3 = ELASP3;
                    E2DISP = ELDISP;
                 // Experience modifiers
                    E2exm1 = ELexm1;
                    E2exm2 = ELexm2;
                    E2exm3 = ELexm3;
                    E2emd1 = ELemd1;
                    E2emd2 = ELemd2;
                    E2emd3 = ELemd3;
                 // Schedule DB/CR
                 If RRDBCD <> oWDRRLds.RRDBCD or RRCRCD <> oWDRRLds.RRCRCD;
                    E2DBCR = ELDBCR;
                      //Schedule DB/CR effective date
                      If ELSDEF < E2PFDT;
                        If ELSDEF <> 0;
                           E2SDEF = E2PFDT;
                        Else;
                           E2SDEF = *Zero;
                        Endif;
                      Else;
                         E2SDEF = ELSDEF;
                      Endif;

                      //Schedule DB/CR expiration date
                      If ELSDEX > E2PTDT;
                         E2SDEX = E2PTDT;
                      Else;
                        If ELSDEX <> 0;
                           If ELSDEF < E2PFDT and ELSDEX < E2PFDT;
                              E2SDEF = *Zero;
                              E2SDEX = *Zero;
                              E2DBCR = *Zero;
                           Else;
                              E2SDEX = ELSDEX;
                           Endif;
                        Else;
                           E2SDEX = *Zero;
                        Endif;
                      Endif;
                 Endif;

                 // Membership Fee
                 E2MFEE = ELMFEE;

                 // Safety Fee
                 Select;
                 When RRsfpc <> *zeros;
                  //Default from ratings and statistics
                    E2Sfpc = RRSfpc;
                    E2Sfmn = RRSfam;
                 Endsl;

                 // Waiver
                 Select;
                 When RRwdtp <> *blank;
                    E2wvtp = RRwdtp;
                    E2wvpc = RRwdpc;
                    E2fchg = RRwdfc;
                    E2wmin = RRwdmn;
                    E2wvfl = RRwvfl;
                 When RRwvac <> 0;
                    E2wvtp = ELwvtp;
                    E2wvpc = ELwvpc;
                    E2fchg = ELfchg;
                    E2wmin = ELwmin;
                    E2wmax = ELwmax;
                    E2wvfl = ELwvfl;
                 Endsl;

                 // Deviation 1
                 If RRDV1C <> oWDRRLds.RRDV1C and (isCltFlg = *off or
                    isCltFlg = *on and xxST <> 'NM');
                    E2ASP2 = ELASP2;
                    If E2ASP2 <> *Zero;
                      //deviation 1 Effective date
                      If ELDVEF < E2PFDT;
                        If ELDVEF <> 0;
                           E2DVEF = E2PFDT;
                        Else;
                           E2DVEF = *Zero;
                        Endif;
                      Else;
                         E2DVEF = ELDVEF;
                      Endif;

                      //deviation 1 Expiration date
                      If ELDVEX > E2PTDT;
                         E2DVEX = E2PTDT;
                      Else;
                        If ELDVEX <> 0;
                           If ELDVEF < E2PFDT and ELDVEX < E2PFDT;
                              E2DVEF = *Zero;
                              E2DVEX = *Zero;
                              E2ASP2 = *Zero;
                           Else;
                              E2DVEX = ELDVEX;
                           Endif;
                        Else;
                           E2DVEX = *Zero;
                        Endif;
                      Endif;
                    Endif;

                    If RRDV1S = 'Y';
                       E2D1S2 = ELD1S2;
                    If E2D1S2 <> *Zero;
                      //deviation 1 Effective date 2
                      If ELD1D2 < E2PFDT;
                        If ELD1D2 <> 0;
                           E2D1D2 = E2PFDT;
                        Else;
                           E2D1D2 = *Zero;
                        Endif;
                      Else;
                         E2D1D2 = ELD1D2;
                      Endif;

                      //deviation 1 Expiration date 2
                      If ELD1X2 > E2PTDT;
                         E2D1X2 = E2PTDT;
                      Else;
                        If ELD1X2 <> 0;
                           If ELD1D2 < E2PFDT and ELD1X2 < E2PFDT;
                              E2D1D2 = *Zero;
                              E2D1X2 = *Zero;
                              E2D1S2 = *Zero;
                           Else;
                              E2D1X2 = ELD1X2;
                           Endif;
                        Else;
                           E2D1X2 = *Zero;
                        Endif;
                      Endif;
                    Endif;
                    Endif;
                 Endif;
                 // Deviation 2
                 If RRDV2C <> oWDRRLds.RRDV2C or clientID  = '025';
                    E2DEV2 = ELDEV2;
                    If E2DEV2 <> *Zero;
                      //deviation 2 Effective date
                      If ELD2EF < E2PFDT;
                        If ELD2EF <> 0;
                           E2D2EF = E2PFDT;
                        Else;
                           E2D2EF = *Zero;
                        Endif;
                      Else;
                         E2D2EF = ELD2EF;
                      Endif;

                      //deviation 2 Expiration date
                      If ELD2EX > E2PTDT;
                         E2D2EX = E2PTDT;
                      Else;
                        If ELD2EX <> 0;
                           If ELD2EF < E2PFDT and ELD2EX < E2PFDT;
                              E2D2EF = *Zero;
                              E2D2EX = *Zero;
                              E2DEV2 = *Zero;
                           Else;
                              E2D2EX = ELD2EX;
                           Endif;
                        Else;
                           E2D2EX = *Zero;
                        Endif;
                      Endif;
                    Endif;
                    E2dev2F = ELdev2F;
                    If E2dev2F <> *Zero;
                       //deviation 2 Effective date
                       If ELd2Ef < E2pFdt;
                          If ELd2Ef <> 0;
                             E2d2Ef = E2pFdt;
                          Else;
                             E2d2Ef = *Zero;
                          Endif;
                       Else;
                          E2D2Ef = ELd2Ef;
                       Endif;

                       //deviation 2 Expiration date
                       If ELd2Ex > E2pTdt;
                          E2d2Ex = E2pTdt;
                       Else;
                          If ELd2Ex <> 0;
                             If ELd2Ef < E2pFdt And ELd2Ex < E2pFdt;
                                E2d2Ef  = *Zero;
                                E2d2Ex  = *Zero;
                                E2dev2F = *Zero;
                             Else;
                                E2d2Ex = ELd2Ex;
                             Endif;
                          Else;
                             E2d2Ex = *Zero;
                          Endif;
                       Endif;
                    Endif;
                 Endif;
                 If RRDV2S = 'Y';
                    E2D2S2 = ELD2S2;
                    If E2D2S2 <> *Zero;
                      //deviation 2 Effective date 2
                      If ELD2D2 < E2PFDT;
                        If ELD2D2 <> 0;
                           E2D2D2 = E2PFDT;
                        Else;
                           E2D2D2 = *Zero;
                        Endif;
                      Else;
                         E2D2D2 = ELD2D2;
                      Endif;

                      //deviation 2 Expiration date 2
                      If ELD2X2 > E2PTDT;
                         E2D2X2 = E2PTDT;
                      Else;
                        If ELD2X2 <> 0;
                           If ELD2D2 < E2PFDT and ELD2X2 < E2PFDT;
                              E2D2D2 = *Zero;
                              E2D2X2 = *Zero;
                              E2D2S2 = *Zero;
                           Else;
                              E2D2X2 = ELD2X2;
                           Endif;
                        Else;
                           E2D2X2 = *Zero;
                        Endif;
                      Endif;
                    Endif;
                 Endif;
                 // Subject Premium Credit 1A
                 If RR1ACD <>  oWDRRLds.RR1ACD;
                    E2ASPM = ELASPM;
                    E2MCCD = ELMCCD;
                    E2MCGP = ELMCGP;
                    If E2ASPM <> *Zero;
                      //subject 1A credit Eff date
                      If ELMCEF < E2PFDT;
                        If ELMCEF <> 0;
                           E2MCEF = E2PFDT;
                        Else;
                           E2MCEF = *Zero;
                        Endif;
                      Else;
                         E2MCEF = ELMCEF;
                      Endif;

                      //subject 1A credit Exp date
                      If ELMCEX > E2PTDT;
                         E2MCEX = E2PTDT;
                      Else;
                        If ELMCEX <> 0;
                           If ELMCEF < E2PFDT and ELMCEX < E2PFDT;
                              E2MCEF = *Zero;
                              E2MCEX = *Zero;
                              E2ASPM = *Zero;
                           Else;
                              E2MCEX = ELMCEX;
                           Endif;
                        Else;
                           E2MCEX = *Zero;
                        Endif;
                      Endif;
                    Endif;
                 Endif;
                 If RR1BCD <>  oWDRRLds.RR1BCD;
                    E2ASPS = ELASPS;
                    If E2ASPS <> *Zero;
                      //subject 1B credit Eff date
                      If ELSCEF < E2PFDT;
                        If ELSCEF <> 0;
                           E2SCEF = E2PFDT;
                        Else;
                           E2SCEF = *Zero;
                        Endif;
                      Else;
                         E2SCEF = ELSCEF;
                      Endif;

                      //subject 1B credit Exp date
                      If ELSCEX > E2PTDT;
                         E2SCEX = E2PTDT;
                      Else;
                        If ELSCEX <> 0;
                           If ELSCEF < E2PFDT and ELSCEX < E2PFDT;
                              E2SCEF = *Zero;
                              E2SCEX = *Zero;
                              E2ASPS = *Zero;
                           Else;
                              E2SCEX = ELSCEX;
                           Endif;
                        Else;
                           E2SCEX = *Zero;
                        Endif;
                      Endif;
                    Endif;
                 Endif;
                 // Contractors Credit
                 IF RRCCCD <> oWDRRLds.RRCCCD;
                    E2ASP1 = ELASP1;
                      //Contractors Credit Eff date
                      If ELCREF < E2PFDT;
                        If ELCREF <> 0;
                           E2CREF = E2PFDT;
                        Else;
                           E2CREF = *Zero;
                        Endif;
                      Else;
                         E2CREF = ELCREF;
                      Endif;

                      //Contractors Credit Exp date
                      If ELCREX > E2PTDT;
                         E2CREX = E2PTDT;
                      Else;
                        If ELCREX <> 0;
                           If ELCREF < ELPFDT and ELCREX < ELPFDT;
                              E2CREF = *Zero;
                              E2CREX = *Zero;
                              E2ASP1 = *Zero;
                           Else;
                              E2CREX = ELCREX;
                           Endif;
                        Else;
                           E2CREX = *Zero;
                        Endif;
                      Endif;
                 Endif;
                 // Facultative Reinsurance
                 If RRARCC <> oWDRRLds.RRARCC;
                    E2ARCP = ELARCP;
                    E2ARCA = ELARCA;
                      //Facultative Reinsurance eff date
                      If ELAREF < E2PFDT;
                        If ELAREF <> 0;
                           E2AREF = E2PFDT;
                        Else;
                           E2AREF = *Zero;
                        Endif;
                      Else;
                         E2AREF = ELAREF;
                      Endif;

                      //Facultative Reinsurance exp date
                      If ELAREX > E2PTDT;
                         E2AREX = E2PTDT;
                      Else;
                        If ELAREX <> 0;
                           If ELAREF < E2PFDT and ELAREX < E2PFDT;
                              E2AREF = *Zero;
                              E2AREX = *Zero;
                              E2ARCP = *Zero;
                           Else;
                              E2AREX = ELAREX;
                           Endif;
                        Else;
                           E2AREX = *Zero;
                        Endif;
                      Endif;
                 Endif;

                 ELPgm  = Q1PGM;
                 ELUser = Q1USER;
                 ELTime = Timeto6(SystemTime());
                 ELDt   = Dateto7(SystemDate());
                 Update pel08;

              Endif;

           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Enddo;

      /end-free
     C                   Endsr

       //---------------------------------------------------------------
       Begsr ChkIncrLimitCode;
       //---------------------------------------------------------------
        //Verify Inc Limits exists for new division
        If ELiLCd <> *Blank;
           Setgt (ELco#:ELfnd:xxst:ELilcd:ELpfdt) plm01;
           Dou %Eof(WTLML1);
              Readpe (ELco#:ELfnd:xxst:ELilcd) plm01;
              If %Eof(WTLML1);
                 Leave;
              Endif;
              If LMstat = 'I';
                 Iter;
              EndIf;
              If LMstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(WTLML1);
              E2ilcd = *Blanks;
              E2ilef = *Zeros;
              E2ilex = *Zeros;
           Endif;
        Endif;
        If ELinl2 <> *Blank;
           Setgt (ELco#:ELfnd:xxST:ELinl2:ELpfdt) plm01;
           Dou %Eof(WTLML1);
              Readpe (ELco#:ELfnd:xxST:ELinl2) plm01;
              If %Eof(WTLML1);
                 Leave;
              Endif;
              If LMstat = 'I';
                 Iter;
              EndIf;
              If LMstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(WTLML1);
              E2inl2 = *Blanks;
              E2i2ef = *Zero;
              E2i2ex = *Zero;
           Endif;
        Endif;

       Endsr;

       //---------------------------------------------------------------
       Begsr ChkDeductibleCode;
       //---------------------------------------------------------------
        //Verify deduction code exists for new division
        If E2dcde <> *Blanks;
           SetGt (Elco#:ELfnd:xxST:ELdcde:ELpfdt) pdd02;
           Dou %Eof(wtddl2);
              Readpe (Elco#:ELfnd:xxST:ELdcde) pdd02;
              If %Eof(wtddl2);
                 Leave;
              Endif;
              If DDstat = 'I';
                 Iter;
              EndIf;
              If DDstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(wtddl2);
              E2dcde = *Blanks;
              E2dcef = *Zeros;
              E2dcex = *Zeros;
           Endif;
        Endif;
        If E2dcd2 <> *Blanks;
           SetGt (Elco#:ELfnd:xxST:ELdcd2:ELpfdt) pdd02;
           Dou %Eof(wtddl2);
              Readpe (Elco#:ELfnd:xxST:ELdcd2) pdd02;
              If %Eof(wtddl2);
                 Leave;
              Endif;
              If DDstat = 'I';
                 Iter;
              EndIf;
              If DDstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(wtddl2);
              E2dcd2 = *Blanks;
              E22def = *Zeros;
              E22dex = *Zeros;
           Endif;
        Endif;
       Endsr;

     P Screen2         E
      **********************************************************************************************

       //===================================================================
       // Screen3 - Screen 3 Processing: Large Deductible Screen
       //===================================================================
     P Screen3         B
     D Screen3         PI
     D*
     D divMode         s             10
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D sched           s              1    inz(' ')
     D large           s              1    inz(' ')
     D retro           s              1    inz(' ')
      /free

       Exsr LoadScreen3;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

      *** Beg Chg ***** 03/24/22 **************************
      /// Select;
      /// When addMode and third_add;
      ///    third_add = *off;
      ///    Setll keyWDELL1 PEL01;
      ///    Reade(n) keyWDELL1 PEL01;
      ///    Dow not %eof;
      ///       Clear xxKey;
      ///       xxKey = %editc(elbscd : 'X');
      ///       Chain keyWTABL PBS01;
      ///       If %found and BSLPGM = 'L';
      ///          xxELRrn = dbrrn7;
      ///          Exsr LargeDedPopup;
      ///       Endif;
      ///       Reade(n) keyWDELL1 PEL01;
      ///    Enddo;
      ///    Exsr LoadGrid3;
      /// When divAdd = *On and third_add;
      ///    third_add = *off;
      ///    scDiv = newDiv#;
      ///    Setll key1WDELL1 PEL01;
      ///    Reade(n) key1WDELL1 PEL01;
      ///    Dow not %eof;
      ///       Clear xxKey;
      ///       xxKey = %editc(elbscd : 'X');
      ///       Chain keyWTABL PBS01;
      ///       If %found and BSLPGM = 'L';
      ///          xxELRrn = dbrrn7;
      ///          Exsr LargeDedPopup;
      ///       Endif;
      ///       Reade(n) key1WDELL1 PEL01;
      ///    Enddo;
      ///    Exsr LoadGrid3;
      /// Endsl;

       If (addMode = *on OR divAdd = *On) and
        isFirst3 = *On  and PQLDedW <> 'Y' ;
       Else;
          Select;
          When addMode and third_add;
             third_add = *off;
             Setll keyWDELL1 PEL01;
             Reade(n) keyWDELL1 PEL01;
             Dow not %eof;
                Clear xxKey;
                xxKey = %editc(elbscd : 'X');
                Chain keyWTABL PBS01;
                If %found and BSLPGM = 'L';
                   xxELRrn = dbrrn7;
                   Exsr LargeDedPopup;
                Endif;
                Reade(n) keyWDELL1 PEL01;
             Enddo;
             Exsr LoadGrid3;
          When divAdd = *On and third_add;
             third_add = *off;
             scDiv = newDiv#;
             Setll key1WDELL1 PEL01;
             Reade(n) key1WDELL1 PEL01;
             Dow not %eof;
                Clear xxKey;
                xxKey = %editc(elbscd : 'X');
                Chain keyWTABL PBS01;
                If %found and BSLPGM = 'L';
                   xxELRrn = dbrrn7;
                   Exsr LargeDedPopup;
                Endif;
                Reade(n) key1WDELL1 PEL01;
             Enddo;
             Exsr LoadGrid3;
          Endsl;
       EndIf;
      *** End Chg ***** 03/24/22 **************************

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;

      *** Beg Add ***** 03/24/22 **************************
      //* Do not process if
        If (addMode = *on OR divAdd = *On) and
         isFirst3 = *On  and PQLDedS <> 'Y' ;
           btnNext = *On;
           isFirst3  = *Off;
           //*** beg add ***** 10/10/25 ***********************
           If isPrev = *on;
              btnPrev = *on;
              btnNext = *off;
           Endif;
           //*** end add ***** 10/10/25 ***********************
        Else;
           Exfmt rcdLrgDed;                           // Process Screen 3
           //*** beg add ***** 10/10/25 ***********************
           isPrev = *off;
           //*** end add ***** 10/10/25 ***********************
       EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen 3
          //*** Exfmt rcdLrgDed;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;


             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc div3SFL;
                If not %EOF() and edit;
                   If div3Edit = *on or div3Delete = *on;
                         If div3Edit = *on;
                            GetEmpRating(ELCO#:ELFND:ELEMP#:scDiv3DIV:ELFYR:
                             ' ':sched:large:retro);
                            If large = 'Y';
                               xxELRrn = scdiv3ELrr;
                               Exsr LargeDedPopup;
                            Else;
                               PopOneBtn('Payment plan for this division is no+
                                t setup for Large Deductible.':'Edit Large De+
                                ductible Information':'Enter');
                            Endif;
                            div3Edit = *off;
                         Endif;
                         //*** Beg Chg ***** 06/08/22 *********************
                         //If div3Delete = *on and
                         // ChkAppSecurity('Delete Division') = *on;
                         //   divMode = 'D';
                         //   xxELRrn = scdiv3ELrr;
                         //   Exsr DivisionPopup;
                         //   xxELRrn = scdiv3ELrr;
                         //   div3Delete = *off;
                         //Endif;
                         If div3Delete = *on;
                            divMode = 'D';
                            xxELRrn = scdiv3ELrr;
                            DeletePolYear(xxELRrn);
                            xxELRrn = scdiv3ELrr;
                            div3Delete = *off;
                         Endif;
                         //*** End Chg ***** 06/08/22 *********************
                         edit = *off;
                         Exsr LoadGrid3;
                   Endif;
                Endif;

                //*  Edit Large Deductible Screen for Errors.
                If edit;
                   Exsr CheckScreen3;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If divAdd = *on;
                   GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                    ' ':sched:large:retro);
                Else;
                   GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                    'Y':sched:large:retro);
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   If sched = 'Y';
                      screen# = 10;
                   Else;
                      screen# = 2;
                      isFirst2 = *On;
                   Endif;
                Endif;

                If btnNext = *on;
                   If retro = 'Y';
                      screen# = 4;
                      isFirst4 = *On;
                   Else;
                      screen# = 5;
                      isFirst5 = *On;
                   Endif;
                Endif;

          Endsl;

       Enddo;



      /end-free

       //===================================================================
       // ClearScreen3 - Clear error/button fields
       //===================================================================
     C     ClearScreen3  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen3 Subr

       //===================================================================
       // CheckScreen3 - Check Policy Screen for Errors
       //===================================================================
     C     CheckScreen3  Begsr
      /free

       Exsr ClearScreen3;


      /end-free
     C                   Endsr                                                  CheckScreen3 Subr


       //===================================================================
       // LoadScreen3 - Load the initial fields for large deductible screen
       //===================================================================
     C     LoadScreen3   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGrid3;

       Exsr ClearScreen3;

      /end-free
     C                   Endsr                                                  LoadScreen3 Subr

       //===================================================================
       // LoadGrid3 - Load the billing grid
       //===================================================================
     C     LoadGrid3     Begsr
      /free

       // Clear the subfile.
       clrDiv3 = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 3;
          visNxtsc3 = *Off;
          leftCn1S3 = 995;
       Else;
          visNxtsc3 = *On;
          leftCn1S3 = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdLrgDed;
       clrDiv3 = *Off;
       rrnDiv3 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd = *Off Or (divAdd  = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv3Fyr  =  scFyr;
             scDiv3Div  =  EMDiv;
             scDiv3DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv3Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv3PolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDiv3Fein =  EMFein;
             scDiv3ELrr =  dbrrn;
             scDiv3EMrr =  dbrrn2;
             scDiv3RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv3STRP =  'Yes';
             Else;
                scDiv3STRP =  'No';
             Endif;

             div3EdtVis = *off;
             GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                 ' ':sched:large:retro);
             If large = 'Y';
                div3EdtVis = *on;
             Endif;

             rrnDiv3 =  rrnDiv3 + 1;
             Write div3SFL;

          Endif;
        Endif;

        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv3 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid3 Subr

       //===================================================================
       // DivisionPopup - Load the location master popup for division add/edit/delete
       //===================================================================
     C     DivisionPopup Begsr
      /free

          DspBillDiv(xxELrrn:xxCall:divMode);

      /end-free
     C                   Endsr                                                  DivisionPopup Subr

       //===================================================================
       // LargeDedPopup - Load the Large Deductible popup for division edit
       //===================================================================
     C     LargeDedPopup Begsr
      /free

       LargeDedEdit(xxELrrn:div3Edit);

      /end-free
     C                   Endsr                                                  LargeDedPopup Subr

     P Screen3         E
      **********************************************************************************************

       //===================================================================
       // Screen4 - Screen 4 Processing: Retrospective Rating Screen
       //===================================================================
     P Screen4         B
     D Screen4         PI
     D*
     D divMode         s             10
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D sched           s              1    inz(' ')
     D large           s              1    inz(' ')
     D retro           s              1    inz(' ')
      /free

       Exsr LoadScreen4;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

      *** Beg Chg ***** 03/24/22 **************************
      /// Select;
      /// When addMode and fourth_add;
      ///    fourth_add = *off;
      ///    Setll keyWDELL1 PEL01;
      ///    Reade(n) keyWDELL1 PEL01;
      ///    Dow not %eof;
      ///       Clear xxKey;
      ///       xxKey = %editc(elbscd : 'X');
      ///       Chain keyWTABL PBS01;
      ///       If %found and (BSLPGM = 'I' or BSLPGM = 'P' or BSLPGM = 'R');
      ///          xxELRrn = dbrrn7;
      ///          Exsr RetroPopup;
      ///       Endif;
      ///       Reade(n) keyWDELL1 PEL01;
      ///    Enddo;
      ///    Exsr LoadGrid4;
      /// when divAdd  = *On and fourth_add;
      ///    fourth_add = *off;
      ///    scDiv = newDiv#;
      ///    Setll key1WDELL1 PEL01;
      ///    Reade(n) key1WDELL1 PEL01;
      ///    Dow not %eof;
      ///       Clear xxKey;
      ///       xxKey = %editc(elbscd : 'X');
      ///       Chain keyWTABL PBS01;
      ///       If %found and (BSLPGM = 'I' or BSLPGM = 'P' or BSLPGM = 'R');
      ///          xxELRrn = dbrrn7;
      ///          Exsr RetroPopup;
      ///       Endif;
      ///       Reade(n) key1WDELL1 PEL01;
      ///    Enddo;
      ///    Exsr LoadGrid4;
      /// EndSl;

       If (addMode = *on OR divAdd = *On) and
        isFirst4 = *On  and PQRetrS <> 'Y' ;
       Else;
          Select;
          When addMode and fourth_add;
             fourth_add = *off;
             Setll keyWDELL1 PEL01;
             Reade(n) keyWDELL1 PEL01;
             Dow not %eof;
                Clear xxKey;
                xxKey = %editc(elbscd : 'X');
                Chain keyWTABL PBS01;
                If %found and (BSLPGM = 'I' or BSLPGM = 'P' or BSLPGM = 'R');
                   xxELRrn = dbrrn7;
                   Exsr RetroPopup;
                Endif;
                Reade(n) keyWDELL1 PEL01;
             Enddo;
             Exsr LoadGrid4;
          when divAdd  = *On and fourth_add;
             fourth_add = *off;
             scDiv = newDiv#;
             Setll key1WDELL1 PEL01;
             Reade(n) key1WDELL1 PEL01;
             Dow not %eof;
                Clear xxKey;
                xxKey = %editc(elbscd : 'X');
                Chain keyWTABL PBS01;
                If %found and (BSLPGM = 'I' or BSLPGM = 'P' or BSLPGM = 'R');
                   xxELRrn = dbrrn7;
                   Exsr RetroPopup;
                Endif;
                Reade(n) key1WDELL1 PEL01;
             Enddo;
             Exsr LoadGrid4;
          EndSl;
       EndIf;
      *** End Chg ***** 03/24/22 **************************

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;

      *** Beg Add ***** 03/24/22 **************************
        If (addMode = *on OR divAdd = *On) and
         isFirst4 = *On  and PQRetrS <> 'Y' ;
           btnNext = *On;
           isFirst4  = *Off;
           //*** beg add ***** 10/10/25 ***********************
           If isPrev = *on;
              btnPrev = *on;
              btnNext = *off;
           Endif;
           //*** end add ***** 10/10/25 ***********************
        Else;
           Exfmt rcdRetro;
           //*** beg add ***** 10/10/25 ***********************
           isPrev = *off;
           //*** end add ***** 10/10/25 ***********************
       EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen 4
          //*** Exfmt rcdRetro;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;


             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc div4SFL;
                If not %EOF() and edit;
                   If div4Edit = *on or div4Delete = *on;
                         If div4Edit = *on;
                            GetEmpRating(ELCO#:ELFND:ELEMP#:scDiv4DIV:ELFYR:
                             ' ':sched:large:retro);
                            If retro = 'Y';
                               xxELRrn = scdiv4ELrr;
                               Exsr RetroPopup;
                            Else;
                               PopOneBtn('Payment plan for this division is no+
                                t setup for Retrospective Rating.':'Edit Retro+
                                spective Rating Information':'Enter');
                            Endif;
                            div4Edit = *off;
                         Endif;
                         //*** Beg Chg ***** 06/08/22 *********************
                         //If div4Delete = *on and
                         // ChkAppSecurity('Delete Division') = *on;
                         //   divMode = 'D';
                         //   xxELRrn = scdiv4ELrr;
                         //   Exsr DivisionPopup;
                         //   xxELRrn = scdiv4ELrr;
                         //   div4Delete = *off;
                         //Endif;
                         If div4Delete = *on;
                            divMode = 'D';
                            xxELRrn = scdiv4ELrr;
                            DeletePolYear(xxELRrn);
                            xxELRrn = scdiv4ELrr;
                            div4Delete = *off;
                         Endif;
                         //*** End Chg ***** 06/08/22 *********************
                         edit = *off;
                         Exsr LoadGrid4;
                   Endif;
                Endif;

                //*  Edit Retro Screen for Errors.
                If edit;
                   Exsr CheckScreen4;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   If divAdd = *on;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       ' ':sched:large:retro);
                   Else;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       'Y':sched:large:retro);
                   Endif;

                   Select;
                      When large = 'Y';
                         screen# = 3;
                         isFirst3 = *On;
                      When sched = 'Y';
                         screen# = 10;
                      Other;
                         screen# = 2;
                         isFirst2 = *On;
                   Endsl;
                Endif;

                If btnNext = *on;
                   screen# = 5;
                   isFirst5 = *On;
                Endif;

          Endsl;

       Enddo;



      /end-free

       //===================================================================
       // ClearScreen4 - Clear error/button fields
       //===================================================================
     C     ClearScreen4  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen4 Subr

       //===================================================================
       // CheckScreen4 - Check Retro Screen for Errors
       //===================================================================
     C     CheckScreen4  Begsr
      /free

       Exsr ClearScreen4;


      /end-free
     C                   Endsr                                                  CheckScreen4 Subr


       //===================================================================
       // LoadScreen4 - Load the initial fields for retro screen
       //===================================================================
     C     LoadScreen4   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGrid4;

       Exsr ClearScreen4;

      /end-free
     C                   Endsr                                                  LoadScreen4 Subr

       //===================================================================
       // LoadGrid4 - Load the billing grid
       //===================================================================
     C     LoadGrid4     Begsr
      /free

       // Clear the subfile.
       clrDiv4 = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 4;
          visNxtsc4 = *Off;
          leftCn1S4 = 995;
       Else;
          visNxtsc4 = *On;
          leftCn1S4 = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdRetro;
       clrDiv4 = *Off;
       rrnDiv4 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd  = *Off Or (divAdd  = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv4Fyr  =  scFyr;
             scDiv4Div  =  EMDiv;
             scDiv4DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv4Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv4PolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDiv4Fein =  EMFein;
             scDiv4ELrr =  dbrrn;
             scDiv4EMrr =  dbrrn2;
             scDiv4RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv4STRP =  'Yes';
             Else;
                scDiv4STRP =  'No';
             Endif;

             div4EdtVis = *off;
             GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                 ' ':sched:large:retro);
             If retro = 'Y';
                div4EdtVis = *on;
             Endif;

             rrnDiv4 =  rrnDiv4 + 1;
             Write div4SFL;

          Endif;
        Endif;

        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv4 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid4 Subr

       //===================================================================
       // DivisionPopup - Load the location master popup for division add/edit/delete
       //===================================================================
     C     DivisionPopup Begsr
      /free

          DspBillDiv(xxELrrn:xxCall:divMode);

      /end-free
     C                   Endsr                                                  DivisionPopup Subr

       //===================================================================
       // RetroPopup - Load the Retro popup for division edit
       //===================================================================
     C     RetroPopup    Begsr
      /free

       RetroEdit(xxELrrn);

      /end-free
     C                   Endsr                                                  RetroPopup Subr


     P Screen4         E
      **********************************************************************************************

       //===================================================================
       // Screen5 - Screen 5 Processing: Additional Info Screen
       //===================================================================
     P Screen5         B
     D Screen5         PI
     D*
     D divMode         s             10
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D sched           s              1    inz(' ')
     D large           s              1    inz(' ')
     D retro           s              1    inz(' ')
      /free

       Exsr LoadScreen5;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

      *** Beg Chg ***** 03/24/22 **************************
      ///  Select;
      ///  When addMode and fifth_add;
      ///     fifth_add = *off;
      ///     Setll keyWDELL1 PEL01;
      ///     Reade(n) keyWDELL1 PEL01;
      ///     Dow not %eof;
      ///        xxELRrn = dbrrn7;
      ///        Exsr AddInfoPopup;
      ///        Reade(n) keyWDELL1 PEL01;
      ///     Enddo;
      ///     Exsr LoadGrid5;
      ///  When divAdd  = *On and fifth_add;
      ///     fifth_add = *off;
      ///     scDiv = newDiv#;
      ///     Setll key1WDELL1 PEL01;
      ///     Reade(n) key1WDELL1 PEL01;
      ///     Dow not %eof;
      ///        xxELRrn = dbrrn7;
      ///        Exsr AddInfoPopup;
      ///        Reade(n) key1WDELL1 PEL01;
      ///     Enddo;
      ///     Exsr LoadGrid5;
      ///  EndSl;

       If (addMode = *on OR divAdd = *On) and
        isFirst5 = *On  and PQAddS <> 'Y' ;
       Else;
           Select;
           When addMode and fifth_add;
              fifth_add = *off;
              Setll keyWDELL1 PEL01;
              Reade(n) keyWDELL1 PEL01;
              Dow not %eof;
                 xxELRrn = dbrrn7;
                 Exsr AddInfoPopup;
                 Reade(n) keyWDELL1 PEL01;
              Enddo;
              Exsr LoadGrid5;
           When divAdd  = *On and fifth_add;
              fifth_add = *off;
              scDiv = newDiv#;
              Setll key1WDELL1 PEL01;
              Reade(n) key1WDELL1 PEL01;
              Dow not %eof;
                 xxELRrn = dbrrn7;
                 Exsr AddInfoPopup;
                 Reade(n) key1WDELL1 PEL01;
              Enddo;
              Exsr LoadGrid5;
           EndSl;
       EndIf;
      *** End Chg ***** 03/24/22 **************************

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;

      *** Beg Chg ***** 03/24/22 **************************
        If (addMode = *on OR divAdd = *On) and
         isFirst5 = *On  and PQAddS <> 'Y' ;
           btnNext = *On;
           isFirst5  = *Off;
           //*** beg add ***** 10/10/25 ***********************
           If isPrev = *on;
              btnPrev = *on;
              btnNext = *off;
           Endif;
           //*** end add ***** 10/10/25 ***********************
        Else;
           Exfmt rcdAddInfo;                          // Process Screen 5
           //*** beg add ***** 10/10/25 ***********************
           isPrev = *off;
           //*** end add ***** 10/10/25 ***********************
        EndIf;
      *** End Chg ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen 5
          //*** Exfmt rcdAddInfo;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;


             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc div5SFL;
                If not %EOF() and edit;
                   If div5Edit = *on or div5Delete = *on;
                         If div5Edit = *on;
                            xxELRrn = scdiv5ELrr;
                            Exsr AddInfoPopup;
                            div5Edit = *off;
                         Endif;
                         //*** Beg Chg ***** 06/08/22 *********************
                         //If div5Delete = *on and
                         // ChkAppSecurity('Delete Division') = *on;
                         //   divMode = 'D';
                         //   xxELRrn = scdiv5ELrr;
                         //   Exsr DivisionPopup;
                         //   xxELRrn = scdiv5ELrr;
                         //   div5Delete = *off;
                         //Endif;
                         If div5Delete = *on;
                            divMode = 'D';
                            xxELRrn = scdiv5ELrr;
                            DeletePolYear(xxELRrn);
                            xxELRrn = scdiv5ELrr;
                            div5Delete = *off;
                         Endif;
                         //*** End Chg ***** 06/08/22 *********************
                         edit = *off;
                         Exsr LoadGrid5;
                   Endif;
                Endif;

                //*  Edit Additional Info Screen for Errors.
                If edit;
                   Exsr CheckScreen5;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   If divAdd = *on;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       ' ':sched:large:retro);
                   Else;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       'Y':sched:large:retro);
                   Endif;

                   Select;
                      When retro = 'Y';
                         screen# = 4;
                         isFirst4 = *On;
                      When large = 'Y';
                         screen# = 3;
                         isFirst3 = *On;
                      When sched = 'Y';
                         screen# = 10;
                      Other;
                         screen# = 2;
                         isFirst2 = *On;
                   Endsl;
                Endif;

                If btnNext = *on;
                   screen# = 6;
                   isFirst6 = *On;
                Endif;

          Endsl;

       Enddo;



      /end-free

       //===================================================================
       // ClearScreen5 - Clear error/button fields
       //===================================================================
     C     ClearScreen5  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen5 Subr

       //===================================================================
       // CheckScreen5 - Check Additional Info Screen for Errors
       //===================================================================
     C     CheckScreen5  Begsr
      /free

       Exsr ClearScreen5;


      /end-free
     C                   Endsr                                                  CheckScreen5 Subr


       //===================================================================
       // LoadScreen5 - Load the initial fields for Additional Info screen
       //===================================================================
     C     LoadScreen5   Begsr
      /free

       Chain(N) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGrid5;

       Exsr ClearScreen5;

      /end-free
     C                   Endsr                                                  LoadScreen5 Subr

       //===================================================================
       // LoadGrid5 - Load the billing grid
       //===================================================================
     C     LoadGrid5     Begsr
      /free

       // Clear the subfile.
       clrDiv5 = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 5;
          visNxtsc5 = *Off;
          leftCn1S5 = 995;
       Else;
          visNxtsc5 = *On;
          leftCn1S5 = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdAddInfo;
       clrDiv5 = *Off;
       rrnDiv5 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd  = *Off Or (divAdd  = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv5Fyr  =  scFyr;
             scDiv5Div  =  EMDiv;
             scDiv5DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv5Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv5PolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDiv5Fein =  EMFein;
             scDiv5ELrr =  dbrrn;
             scDiv5EMrr =  dbrrn2;
             scDiv5RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv5STRP =  'Yes';
             Else;
                scDiv5STRP =  'No';
             Endif;

             rrnDiv5 =  rrnDiv5 + 1;
             Write div5SFL;

          Endif;
        Endif;

        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv5 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid5 Subr

       //===================================================================
       // DivisionPopup - Load the location master popup for division add/edit/delete
       //===================================================================
     C     DivisionPopup Begsr
      /free

          DspBillDiv(xxELrrn:xxCall:divMode);

      /end-free
     C                   Endsr                                                  DivisionPopup Subr

       //===================================================================
       // AddInfoPopup - Load the Additional Info popup for division edit
       //===================================================================
     C     AddInfoPopup  Begsr
      /free

       AddInfoEdit(xxELrrn:quoteMode:div5Edit);

      /end-free
     C                   Endsr                                                  AddInfoPopup Subr


     P Screen5         E
      **********************************************************************************************

       //===================================================================
       // Screen6 - Screen 6 Processing: Payroll Info Screen
       //===================================================================
     P Screen6         B
     D Screen6         PI
     D*
     D payMode         s             10
     D xxSt            s              2
     D xxType          s              1
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D xxEPRrn         s              9  0
      /free

       Exsr LoadScreen6;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

      *** Beg Chg ***** 03/24/22 **************************
      /// Select;
      /// When addMode and sixth_add;
      ///    sixth_add = *off;
      ///    payMode = 'A';
      ///    xxEPRrn = 0;
      ///    Exsr PayrollPopup;
      ///    Exsr LoadPay6;
      ///    payMode = ' ';
      /// When divAdd  = *On And sixth_add;
      ///    sixth_add = *off;
      ///    scDiv = newDiv#;
      ///    payMode = ' ';
      ///    xxEPRrn = 0;
      ///    Exsr PayrollPopup;
      ///    Exsr LoadPay6;
      ///    payMode = ' ';
      /// EndSl;

       If (addMode = *on OR divAdd = *On) and
        isFirst6 = *On  and PQPayW <> 'Y' ;
       Else;
          Select;
          When addMode and sixth_add;
             sixth_add = *off;
             payMode = 'A';
             xxEPRrn = 0;
             Exsr PayrollPopup;
             Exsr LoadPay6;
             payMode = ' ';
          When divAdd  = *On And sixth_add;
             sixth_add = *off;
             scDiv = newDiv#;
             payMode = ' ';
             xxEPRrn = 0;
             Exsr PayrollPopup;
             Exsr LoadPay6;
             payMode = ' ';
          EndSl;
       EndIf;
      *** End Chg ***** 03/24/22 **************************

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;

      *** Beg Add ***** 03/24/22 **************************
        If (addMode = *on OR divAdd = *On) and
         isFirst6 = *On  and PQPayS <> 'Y' ;
           btnNext = *On;
           isFirst6  = *Off;
           //*** beg add ***** 10/10/25 ***********************
           If isPrev = *on;
              btnPrev = *on;
              btnNext = *off;
           Endif;
           //*** end add ***** 10/10/25 ***********************
        Else;
           Exfmt rcdPayroll;                           // Process Screen 6
           //*** beg add ***** 10/10/25 ***********************
           isPrev = *off;
           //*** end add ***** 10/10/25 ***********************
        EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen 6
          //*** Exfmt rcdPayroll;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             //*  Filter Payroll button.
             When filterPay = *on;
                Exsr PayrollFilter;
                filterPay = *off;
                If  scPayFStat <> 'A';
                   payFiltOn  = *on;
                Endif;
                Exsr LoadPay6;

             //*  Filter Payroll button.
             When clearPayFt = *on;
                clearPayFt = *off;
                payFiltOn  = *off;
                scPayFStat = 'A';
                Exsr LoadPay6;

             //*  Add new payroll record.
             When addPayroll = *on;
                If divAdd = *on;
                   payMode = ' ';
                Else;
                   payMode = 'A';
                Endif;
                xxEPRrn = 0;
                Exsr EstPayPopup;
                payMode = ' ';
                addPayroll = *off;
                Exsr LoadPay6;

             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc pay6SFL;
                If not %EOF() and edit;
                   If pay6Edit = *on or pay6Delete = *on;
                         If pay6Edit = *on;
                            payMode = ' ';
                            xxEPRrn = scpay6EPrr;
                            Exsr PayrollPopup;
                            pay6Edit = *off;
                         Endif;
                         If pay6Delete = *on;
                            payMode = 'D';
                            xxEPRrn = scpay6EPrr;
                            Exsr PayrollPopup;
                            pay6Delete = *off;
                            payMode = ' ';
                         Endif;
                         edit = *off;
                         Exsr LoadPay6;
                   Endif;
                Endif;

                //*  Edit Additional Info Screen for Errors.
                If edit;
                   Exsr CheckScreen6;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   screen# = 5;
                   isFirst5 = *On;
                Endif;

                If btnNext = *on;
                   screen# = 7;
                   isFirst7 = *On;
                Endif;

          Endsl;

       Enddo;



      /end-free

       //===================================================================
       // ClearScreen6 - Clear error/button fields
       //===================================================================
     C     ClearScreen6  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen6 Subr

       //===================================================================
       // CheckScreen6 - Check Additional Info Screen for Errors
       //===================================================================
     C     CheckScreen6  Begsr
      /free

       Exsr ClearScreen6;


      /end-free
     C                   Endsr                                                  CheckScreen6 Subr


       //===================================================================
       // LoadScreen6 - Load the initial fields for Additional Info screen
       //===================================================================
     C     LoadScreen6   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGrid6;

       Exsr LoadPay6;

       Exsr ClearScreen6;

      /end-free
     C                   Endsr                                                  LoadScreen6 Subr

       //===================================================================
       // LoadGrid6 - Load the billing grid
       //===================================================================
     C     LoadGrid6     Begsr
      /free

       // Clear the subfile.
       clrDiv6 = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 6;
          visNxtsc6 = *Off;
          leftCn1S6 = 995;
       Else;
          visNxtsc6 = *On;
          leftCn1S6 = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdPayroll;
       clrDiv6 = *Off;
       rrnDiv6 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd  = *Off Or (divAdd  = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv6Fyr  =  scFyr;
             scDiv6Div  =  EMDiv;
             scDiv6DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv6Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv6PolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDiv6Fein =  EMFein;
             scDiv6ELrr =  dbrrn;
             scDiv6EMrr =  dbrrn2;
             scDiv6RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv6STRP =  'Yes';
             Else;
                scDiv6STRP =  'No';
             Endif;

             rrnDiv6 =  rrnDiv6 + 1;
             Write div6SFL;

          Endif;
        Endif;

        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv6 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid6 Subr

       //===================================================================
       // LoadPay6 - Load the payroll grid
       //===================================================================
     C     LoadPay6      Begsr
      /free


       // Clear the subfile.
       clrPayroll = *On;
       Write rcdPayroll;
       clrPayroll = *Off;
       rrnPayroll = 0;

       // Clear payroll totals
       Clear scTotalEst;
       Clear scTotalRep;
       Clear scTotalAud;
       Clear scTotalMan;

       // Populate the subfile
       Setll keyWMEPLE PEP01;
       Reade keyWMEPLE PEP01;

       Dow not %eof;

          If (EPFyr = scFyr and (scPayFStat = ' ' or EPSTAT = scPayFStat)) And
           (divAdd  = *Off Or (divAdd  = *On And EPdiv = newDiv#));

             If EPDiv <> ELDiv;
                Chain(n) key5WDELL PEL00;
             Endif;

             scPay6Div  =  EPDiv;
             scPay6EfDt =  dtMDYY(EPEFDT);
             scPay6CxDt =  dtMDYY(EPCADT);
             scPay6Cls  =  EPCLAS;
             scPay6Spec =  EPSPEC;
             scPay6Ext  =  EPEXT;
             scPay6EAPR =  EPEAPR;
             scPay6SAPR =  EPSAPR;
             scPay6AAPR =  EPAAPR;
             scPay6EMPR =  EPEMPR;
             scPay6Rate =  EPARAT;
             scPay6EPrr =  dbrrn3;
             If EPSTAT = 'A';
                scPay6Stat = 'Active';
             Else;
                scPay6Stat = 'Inactive';
             Endif;

             Clear scPay6Desc;
             xxType = 'P';
             xxSt   = GetRatingState(scCo#:scFnd:scEmp#:EPDiv:scFyr);
             xxEfDt = SrEffd(scCO#:scFND:scFYR:scEMP#:EPDIV:EPEFDT:xxType:
              EPCLAS);
             Setgt keyWMPRL1 PPR01;
             Readpe key1WMPRL1 PPR01;
             If not %eof;
                scPay6Desc = PRDESC;
                If EPEXT <> *blanks;
                   Clear xxPxSt;
                   Setgt keyWMPXL1 PPX01;
                   Readpe key1WMPXL1 PPX01;
                   If not %eof;
                      scPay6Desc = PXDESC;
                   Else;
                      xxPxSt = xxSt;
                      Setgt keyWMPXL1 PPX01;
                      Readpe key1WMPXL1 PPX01;
                      If not %eof;
                         scPay6Desc = PXDESC;
                      Endif;
                   Endif;
                Endif;
             Endif;

             scTotalEst = scTotalEst + EPEAPR;
             scTotalRep = scTotalRep + EPSAPR;
             scTotalAud = scTotalAud + EPAAPR;
             scTotalMan = scTotalMan + EPEMPR;

             rrnPayroll =  rrnPayroll + 1;
             Write pay6SFL;

          Endif;

          Reade keyWMEPLE PEP01;
       Enddo;
       isContribution = GetPrmContflg(scCO#:scFND);
       If isContribution;
          flexhead1 = 'Division,Effective Date,Class,Description,+
           Estimated Payroll,Reported Payroll,Audited Payroll,Rate,+
           Manual Contribution,Cancel Date,Status,,';
          flxDtlHd1 = 'Estimated Manual Contribution';
          flxDtlHd2 = 'Audited Manual Contribution';
       Else;
          flexhead1 = 'Division,Effective Date,Class,Description,+
           Estimated Payroll,Reported Payroll,Audited Payroll,Rate,+
           Manual Premium,Cancel Date,Status,,';
          flxDtlHd1 = 'Estimated Manual Premium';
          flxDtlHd2 = 'Audited Manual Premium';
       Endif;

       //* Position grid at selected record.
       rrnPayroll = 1;

       Chain(n) keyWDELL PEL00;

      /end-free
     C                   Endsr                                                  LoadPay6 Subr

       //===================================================================
       // PayrollFilter - Display the payroll filter popup
       //===================================================================
     C     PayrollFilter Begsr
      /free

       //* Display popup window for the payroll filter.

       payFiltApl = *off;
       payFiltCnl = *off;

       Dou payFiltApl = *on or payFiltCnl = *on;
          Exfmt rcdPayFilt;
       Enddo;

      /end-free
     C                   Endsr                                                  PayrollFilter Subr

       //===================================================================
       // PayrollPopup - Load the Payroll Info popup for payroll edit
       //===================================================================
     C     PayrollPopup  Begsr
      /free
       PayrollEdit(xxEPrrn:ELRTCD:elRRN:payMode);

      /end-free
     C                   Endsr                                                  PayrollPopup Subr

       //===================================================================
       // EstPayPopup - Load the Estimated Payroll Grid
       //===================================================================
      /free
       Begsr EstPayPopup ;
          EstPyrEdit(xxEPrrn:ELRTCD:elRRN:PayMode);
       Endsr;

      /end-free

     P Screen6         E

       //===================================================================
       // Screen7 - Screen 7 Processing: Agent Info Screen
       //===================================================================
     P Screen7         B
     D Screen7         PI
     D*
     D agtMode         s             10
     D agentName       s             40
     D prodName        s             40
     D agtFein         s              9  0
     D AgtMod          s              3  0
     D xxAHRrn         s              9  0
     D xxELRrn         s              9  0
     D xxAGrr          s              9  0
     D xxAArr          s              9  0
     D xxAPrr          s              9  0
     D svDiv           s              5  0
     D svDate          s              7  0
     D count           s              2  0
     D found           s              3  0
     D index           s              3  0
     D agt2            s              5  0 DIM(999)
     D agt3            s              5  0 DIM(999)
      /free

       Exsr LoadScreen7;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

      *** Beg Chg ***** 03/24/22 **************************
      /// Select;
      /// When addMode and seventh_add;
      ///    seventh_add = *off;
      ///    agtMode = ' ';
      ///    Setll keyWDELL1 PEL01;
      ///    Reade(n) keyWDELL1 PEL01;
      ///    Dow not %eof;
      ///       Chain(n) key2WMAHL PAH00;
      ///       If %found;
      ///          xxELRrn = dbrrn7;
      ///          xxAHRrn = dbrrn4;
      ///          Exsr AgentPopup;
      ///       Endif;
      ///       Reade(n) keyWDELL1 PEL01;
      ///    Enddo;
      ///    Exsr LoadGrid7;
      /// When divAdd =*On and seventh_add;
      ///    seventh_add = *off;
      ///    scDiv = newDiv#;
      ///    agtMode = 'B';
      ///    xxELRrn = ELRrn;
      ///    xxAHRrn = 0;
      ///    Chain(n) key2WMAHL PAH00;
      ///    If %found;
      ///       agtMode = ' ';
      ///       xxAHRrn = dbrrn4;
      ///    Endif;
      ///    Exsr AgentPopup;
      ///    Exsr LoadGrid7;
      /// EndSl;

       If (addMode = *on OR divAdd = *On) and
        isFirst7 = *On  and PQAgtW <> 'Y' ;
       Else;
          Select;
          When addMode and seventh_add;
             seventh_add = *off;
             agtMode = ' ';
             Setll keyWDELL1 PEL01;
             Reade(n) keyWDELL1 PEL01;
             Dow not %eof;
                Chain(n) key2WMAHL PAH00;
                If %found;
                   xxELRrn = dbrrn7;
                   xxAHRrn = dbrrn4;
                   Exsr AgentPopup;
                Endif;
                Reade(n) keyWDELL1 PEL01;
             Enddo;
             Exsr LoadGrid7;
          When divAdd =*On and seventh_add;
             seventh_add = *off;
             scDiv = newDiv#;
             agtMode = 'B';
             xxELRrn = ELRrn;
             xxAHRrn = 0;
             Chain(n) key2WMAHL PAH00;
             If %found;
                agtMode = ' ';
                xxAHRrn = dbrrn4;
             Endif;
             Exsr AgentPopup;
             Exsr LoadGrid7;
          EndSl;
       EndIf;
      *** End Chg ***** 03/24/22 **************************

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;

      *** Beg Add ***** 03/24/22 **************************
        If (addMode = *on OR divAdd = *On) and
         isFirst7 = *On  and PQAgtS <> 'Y' ;
           btnNext = *On;
           isFirst7  = *Off;
           //*** beg add ***** 10/10/25 ***********************
           If isPrev = *on;
              btnPrev = *on;
              btnNext = *off;
           Endif;
           //*** end add ***** 10/10/25 ***********************
        Else;
           Exfmt rcdAgent;                             // Process Screen 7
           //*** beg add ***** 10/10/25 ***********************
           isPrev = *off;
           //*** end add ***** 10/10/25 ***********************
        EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen 7
          //*** Exfmt rcdAgent;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             //* Add Agency button click
             When addAgent = *on;
                agtMode = 'A';
                xxELRrn = ELRrn;
                xxAHRrn = 0;
                Exsr AgentPopup;
                addAgent = *off;
                Exsr LoadGrid7;

             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc agt7SFL;
                If not %EOF() and edit;
                   If agt7Edit = *on;
                      agtMode = ' ';
                      xxAHRrn = scAgt7AHrr;
                      xxELRrn = scAgt7ELrr;
                      Exsr AgentPopup;
                      agt7Edit = *off;
                      edit = *off;
                      Exsr LoadGrid7;
                   Endif;
                Endif;

                //*  Edit Additional Info Screen for Errors.
                If edit;
                   Exsr CheckScreen7;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   screen# = 6;
                   isFirst6 = *On;
                Endif;

                If btnNext = *on;
                   screen# = 8;
                   isFirst8 = *On;
                Endif;

          Endsl;

       Enddo;



      /end-free

       //===================================================================
       // ClearScreen7 - Clear error/button fields
       //===================================================================
     C     ClearScreen7  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen7 Subr

       //===================================================================
       // CheckScreen7 - Check Additional Info Screen for Errors
       //===================================================================
     C     CheckScreen7  Begsr
      /free

       Exsr ClearScreen7;

       //* Multiple records with same effective date.
       If error = *off;
          Exsr CheckEffDt;
       Endif;

       //* Primary and/or Secondary agents match between records.
       If error = *off;
          Exsr CheckSameAgt;
       Endif;

      /end-free
     C                   Endsr                                                  CheckScreen7 Subr

       //===================================================================
       // CheckEffDt  - Check agent history for matching effective dates.
       //===================================================================
     C     CheckEffDt    Begsr
      /free

       // Read agent history file.
       Setll key1WMAHL PAH00;
       Reade(n) key1WMAHL PAH00;
       Clear svDiv;
       Clear svDate;
       count  = 0;

       Dow not %eof;
       //*** begin chg ***  04/18/23  **********************************
       //*****   If AHFyr = scFyr;
          If AHFyr = scFyr and AHStat = 'A';
       //*** end chg *****  04/18/23  **********************************

             If svDiv = AHDiv;
                If AHEfdt = svDate;
                   count = 1;
                   Leave;
                Else;
                   svDate = AHEfdt;
                Endif;
             Else;
                svDiv = AHDiv;
                svDate = AHEfdt;
             Endif;

          Endif;
          Reade(n) key1WMAHL PAH00;
       Enddo;

       If count <> 0;

          Clear wdmsg1;
          Clear wdmsg2;
          Clear wdmsg3;
          Clear wdmsg4;
          Clear wdmsg5;

          wdmsg1 = 'Multiple active records exist with the same effective';
          wdmsg2 = 'date. This condition must be fixed prior to exiting';
          wdmsg3 = 'the agent history screen.';

          Clear agtMsgCnl;
          Clear agtMsgCon;

          Dou agtMsgCnl = *on or agtMsgCon = *on;
             Exfmt rcdAgtMsg;
          Enddo;

          If agtMsgCnl = *on;
             error = *on;
          Endif;

       Endif;

      /end-free
     C                   Endsr                                                  CheckEffDt Subr

       //===================================================================
       // CheckSameAgt - Check agent history for matching agents.
       //===================================================================
     C     CheckSameAgt  Begsr
      /free

       // Read employer detail to loop over agent history.
       count  = 0;

       Setll keyWDELL1 PEL01;
       Reade(n) keyWDELL1 PEL01;
       Dow not %eof;

          Clear agt2;
          Clear agt3;
          index = 1;
          Setll key2WMAHL PAH00;
          Reade(n) key2WMAHL PAH00;

          Dow not %eof;
             If AHStat = 'A';
                agt2(index) = AHSagt;
                agt3(index) = AH3agt;
                index = index + 1;
             Endif;
             Reade(n) key2WMAHL PAH00;
          Enddo;

          Setll key2WMAHL PAH00;
          Reade(n) key2WMAHL PAH00;

          Dow not %eof;
             xxRate = AHRT#;
             Setgt keyWMARL PAR00;
             Readpe key1WMARL PAR00;
             Dow not %eof and ARSTAT <> 'A';
                Readpe key1WMARL PAR00;
             Enddo;
             If not %eof and AHStat = 'A' and ARNWRT <> 0;
                found = %Lookup(AHAgt#:agt2);
                If found <> 0;
                   count = 1;
                   Leave;
                Endif;
                found = %Lookup(AHAgt#:agt3);
                If found <> 0;
                   count = 1;
                   Leave;
                Endif;
             Endif;
             Reade(n) key2WMAHL PAH00;
          Enddo;

          If count = 1;
             Leave;
          Endif;

          Reade(n) keyWDELL1 PEL01;
       Enddo;

       If count <> 0;

          Clear wdmsg1;
          Clear wdmsg2;
          Clear wdmsg3;
          Clear wdmsg4;
          Clear wdmsg5;

          wdmsg1 = 'The primary agent and producer are the secondary or';
          wdmsg2 = 'tertiary agent and producer on another record.';
          wdmsg3 = 'This condition must be corrected before leaving';
          wdmsg4 = 'the agent history screen.';

          Clear agtMsgCnl;
          Clear agtMsgCon;

          Dou agtMsgCnl = *on or agtMsgCon = *on;
             Exfmt rcdAgtMsg;
          Enddo;

          If agtMsgCnl = *on;
             error = *on;
          Endif;

       Endif;

      /end-free
     C                   Endsr                                                  CheckSameAgt Subr


       //===================================================================
       // LoadScreen7 - Load the initial fields for Additional Info screen
       //===================================================================
     C     LoadScreen7   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGrid7;

       Exsr ClearScreen7;

      /end-free
     C                   Endsr                                                  LoadScreen7 Subr

       //===================================================================
       // LoadGrid7 - Load the billing grid
       //===================================================================
     C     LoadGrid7     Begsr
      /free

       // Clear the subfile.
       clrAgt7 = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 7;
          visNxtsc7 = *Off;
          leftCn1S7 = 995;
       Else;
          visNxtsc7 = *On;
          leftCn1S7 = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdAgent;
       clrAgt7 = *Off;
       rrnAgt7 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMAHL PAH00;
       Reade(n) key1WMAHL PAH00;

       Dow not %eof;

          If AHFyr = scFyr And
           (divAdd  = *Off Or (divAdd  = *On And newDiv# = AHdiv));
             Chain(n) key2WDELL PEL00;

             If %found;

                scAgt7Fyr  =  scFyr;
                scAgt7Div  =  AHDiv;
                scAgt7Efdt =  dtMDYY(AHEfdt);
                Clear agentName;
                agentName = GetAgtName(AHAgt#:xxAGrr:xxAArr:AHFnd:AHCo#);
                scAgt7Agt  =  agentName;

                scAgt7Num# =  AHAgt#;

                Clear prodName;
                agtFein = GetAgtFein(AHCo#:AHFnd:AHAgt#:agtMod);
                ProdName = GetProdNm(agtFein:agtMod:AHProd:xxAPrr:AHCo#);
                scAgt7Prod =  prodName;

                scProd7Num =  AHProd;

                xxRate = AHRT#;
                Setgt keyWMARL PAR00;
                Readpe key1WMARL PAR00;
                Dow not %eof and ARSTAT <> 'A';
                   Readpe key1WMARL PAR00;
                Enddo;

                If not %eof;
                   scAgt7NWRT =  ARNWRT;
                   scAgt7RNRT =  ARRNRT;
                Else;
                   Clear scAgt7NWRT;
                   Clear scAgt7RNRT;
                Endif;

                If AHSTAT = 'A';
                   scAgt7Stat = 'Active';
                Else;
                   scAgt7Stat = 'Inactive';
                Endif;

                scAgt7AoR  =  AHAGRC;
                scAgt7ELrr =  dbrrn;
                scAgt7AHrr =  dbrrn4;

                Clear scAgt7QSts;
                Setgt key2WAQTL7 PQT07;
                Readpe(n) key2WAQTL7 PQT07;
                If not %eof;
                   If QTtype = 'T' and QTxFer <> *blanks;
                      QTtype = QTxFer;
                   Endif;
                   Clear QSused;
                   Clear QSdesc;
                   Chain (scCo#:QTType) pqs01;
                   If QSused <> *blank;
                      Chain (scCo#:QSused) pqs01;
                   Endif;
                   scAgt7QSts = QSdesc;
                Endif;

                rrnAgt7 =  rrnAgt7 + 1;
                Write agt7SFL;

             Endif;

          Endif;

          Reade(n) key1WMAHL PAH00;
       Enddo;

       //* Position grid at selected record.
       rrnAgt7 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMAHL PAH00;

       elRRN = dbrrn;
       emRRN = dbrrn2;
       ahRRN = dbrrn4;

      /end-free
     C                   Endsr                                                  LoadGrid7 Subr

       //===================================================================
       // AgentPopup - Load the Agent Info popup for agent history edit
       //===================================================================
     C     AgentPopup    Begsr
      /free

       AgentEdit(xxAHrrn:quoteMode:xxELrrn:agtMode);

      /end-free
     C                   Endsr                                                  AgentPopup Subr


     P Screen7         E
      **********************************************************************************************

       //===================================================================
       // Screen8 - Screen 8 Processing: Additional Locations Screen
       //===================================================================
     P Screen8         B
     D Screen8         PI
     D*
     D locMode         s             10
     D xxSt            s              2
     D xxType          s              1
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D xxALRrn         s              9  0
     D divRRN          s              9  0
     D aldivRRN        s              9  0
     d  AdL1           s             40    inz
     d  AdL2           s             40    inz
     d  AdL3           s             40    inz
      /free

       Exsr LoadScreen8;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;

      *** Beg Add ***** 03/24/22 **************************
        If (addMode = *on OR divAdd = *On) and
         isFirst8 = *On  and PQLocS <> 'Y' ;
           btnNext = *On;
           isFirst8  = *Off;
           //*** beg add ***** 10/10/25 ***********************
           If isPrev = *on;
              btnPrev = *on;
              btnNext = *off;
           Endif;
           //*** end add ***** 10/10/25 ***********************
        Else;
           Exfmt rcdAddLoc;                            // Process Screen 8
           //*** beg add ***** 10/10/25 ***********************
           isPrev = *off;
           //*** end add ***** 10/10/25 ***********************
       EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen 8
          //*** Exfmt rcdAddLoc;
      *** End Del ***** 03/24/22 **************************

          If div8RowRRN <> 0;
             Chain div8RowRRN div8SFL;
             divRRN = scDiv8EMrr;
          Endif;

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             //*  Add new location record.
             When addLocatio = *on;
                locMode = 'A';
                xxALRrn = 0;
                aldivRRN = divRRN;
                Exsr LocationPopup;
                locMode = ' ';
                addLocatio = *off;
                Exsr LoadLoc8;

             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc loc8SFL;
                If not %EOF() and edit;
                   If loc8Edit = *on or loc8Delete = *on;
                         If loc8Edit = *on;
                            locMode = 'C';
                            xxALRrn = scloc8ALrr;
                            aldivRRN = scloc8EMrr;
                            Exsr LocationPopup;
                            loc8Edit = *off;
                         Endif;
                         If loc8Delete = *on;
                            locMode = 'D';
                            xxALRrn = scloc8ALrr;
                            aldivRRN = scloc8EMrr;
                            Exsr LocationPopup;
                            loc8Delete = *off;
                            locMode = ' ';
                         Endif;
                         edit = *off;
                         Exsr LoadLoc8;
                   Endif;
                Endif;

                //*  Edit Additional Locations Screen for Errors.
                If edit;
                   Exsr CheckScreen8;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   screen# = 7;
                   isFirst7 = *On;
                Endif;

                If btnNext = *on;
                   screen# = 9;
                   isFirst9 = *On;
                Endif;

          Endsl;

       Enddo;



      /end-free

       //===================================================================
       // ClearScreen8 - Clear error/button fields
       //===================================================================
     C     ClearScreen8  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen8 Subr

       //===================================================================
       // CheckScreen8 - Check Additional Locations Screen for Errors
       //===================================================================
     C     CheckScreen8  Begsr
      /free

       Exsr ClearScreen8;


      /end-free
     C                   Endsr                                                  CheckScreen8 Subr


       //===================================================================
       // LoadScreen8 - Load the initial fields for Additional Locations screen
       //===================================================================
     C     LoadScreen8   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       divRRN = emRRN;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGrid8;

       Exsr LoadLoc8;

       Exsr ClearScreen8;

      /end-free
     C                   Endsr                                                  LoadScreen8 Subr

       //===================================================================
       // LoadGrid8 - Load the billing grid
       //===================================================================
     C     LoadGrid8     Begsr
      /free

       // Clear the subfile.
       clrDiv8 = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 8;
          visNxtsc8 = *Off;
          leftCn1S8 = 995;
       Else;
          visNxtsc8 = *On;
          leftCn1S8 = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdAddLoc;
       clrDiv8 = *Off;
       rrnDiv8 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd  = *Off Or (divAdd  = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv8Fyr  =  scFyr;
             scDiv8Div  =  EMDiv;
             scDiv8DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv8Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv8PolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDiv8Fein =  EMFein;
             scDiv8ELrr =  dbrrn;
             scDiv8EMrr =  dbrrn2;
             scDiv8RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv8STRP =  'Yes';
             Else;
                scDiv8STRP =  'No';
             Endif;

             rrnDiv8 =  rrnDiv8 + 1;
             Write div8SFL;

          Endif;
        Endif;

        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv8 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid8 Subr

       //===================================================================
       // LoadLoc8 - Load the location grid
       //===================================================================
     C     LoadLoc8      Begsr
      /free


       // Clear the subfile.
       clrLocatio = *On;
       Write rcdAddLoc;
       clrLocatio = *Off;
       rrnLocatio = 0;

       // Populate the subfile
       Setll keyWMALL PAL00;
       Reade keyWMALL PAL00;

       Dow not %eof;

       If divAdd  = *Off Or (divAdd  = *On And newDiv# = ALdiv);

          Setgt key3WDELL PEL00;
          Readpe(n) key4WDELL PEL00;

          Chain(n) key2WMEML PEM00;

          scLoc8Div  =  ALDiv;
          scLoc8EfDt =  dtMDYY(ALEfdt);
          scLoc8ExDt =  dtMDYY(ALDldt);
          scLoc8FEIN =  ALFEIN;
          scLoc8Name =  ALName;
          If ALpad1 <> *Blanks;
             FormatAddress(ALpad1:ALpad2:ALpcty:ALpst:ALpzip:AdL1:AdL2:AdL3);
              scLoc8Adr = %Trim(AdL1)+ ', ' + %Trim(AdL2)+ ', ' +
              %Trim(Adl3);
          Else;
             FormatAddress(ALmad1:ALmad2:ALmcty:ALmst:ALmzip:AdL1:AdL2:AdL3);
              scLoc8Adr = %Trim(AdL1)+ ', ' + %Trim(AdL2)+ ', ' +
              %Trim(Adl3);
          Endif;
          scLoc8Num  =  ALELID;
          scLoc8ALrr =  dbrrn5;
          scLoc8EMrr =  dbrrn2;
          If ALELTP = 'E';
             scLoc8Type = 'Entity';
          Else;
             scLoc8Type = 'Location';
          Endif;
          scLoc8RtSt =  GetRatingState(scCo#:scFnd:scEmp#:ALDiv:ELFyr);
          If EMSTRP = 'Y';
             scLoc8STRP =  'Yes';
          Else;
             scLoc8STRP =  'No';
          Endif;
         If flgNBquote = *on;
            scLoc8DVal = ' Delete';
         Else;
            scLoc8DVal = ' Expire';
         Endif;

          rrnLocatio =  rrnLocatio + 1;
          Write loc8SFL;

       Endif;

          Reade keyWMALL PAL00;
       Enddo;

       //* Position grid at selected record.
       rrnLocatio = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadLoc8 Subr

       //===================================================================
       // LocationPopup - Load the Location popup for location edit
       //===================================================================
     C     LocationPopup Begsr
      /free

       PrcLocEntWdw(aldivRRN:'QTE':locMode:xxALrrn:flgNBquote);

      /end-free
     C                   Endsr                                                  LocationPopup Subr


     P Screen8         E
      **********************************************************************************************

       //===================================================================
       // Screen9 - Screen 9 Processing: Owner/Officer Info Screen
       //===================================================================
     P Screen9         B
     D Screen9         PI
     D*
     D ownMode         s             10
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D xxOORrn         s              9  0
      /free

       Exsr LoadScreen9;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

       Dou btnFinish = *on or btnPrev = *on;

      *** Beg Add ***** 03/24/22 **************************
        If (addMode = *on OR divAdd = *On) and
         isFirst9 = *On  and PQOffS <> 'Y' ;
           btnFinish = *On;
           isFirst9  = *Off;
        Else;
           Exfmt rcdOwner;
       EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen 9
          //*** Exfmt rcdOwner;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             //*  Add new owner record.
             When addOwner = *on;
                ownMode = 'A';
                xxOORrn = 0;
                Exsr OwnerPopup;
                ownMode = ' ';
                addOwner = *off;
                Exsr LoadOwn9;

             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc own9SFL;
                If not %EOF() and edit;
                   If own9Edit = *on or own9Delete = *on;
                         If own9Edit = *on;
                            ownMode = 'C';
                            xxOORrn = scOwn9OOrr;
                            Exsr OwnerPopup;
                            own9Edit = *off;
                         Endif;
                         If own9Delete = *on;
                            ownMode = 'D';
                            xxOORrn = scOwn9OOrr;
                            Exsr OwnerPopup;
                            own9Delete = *off;
                            ownMode = ' ';
                         Endif;
                         edit = *off;
                         Exsr LoadOwn9;
                   Endif;
                Endif;

                //*  Edit Owner/Officer Screen for Errors.
                If edit;
                   Exsr CheckScreen9;
                Endif;

                If error;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   screen# = 8;
                   isFirst8 = *On;
                Endif;

          Endsl;

       Enddo;


      /end-free

       //===================================================================
       // ClearScreen9 - Clear error/button fields
       //===================================================================
     C     ClearScreen9  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreen9 Subr

       //===================================================================
       // CheckScreen9 - Check Additional Locations Screen for Errors
       //===================================================================
     C     CheckScreen9  Begsr
      /free

       Exsr ClearScreen9;

       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

          If divAdd  = *Off Or (divAdd  = *On And newDiv# = EMdiv);
      /end-free
      *
     C                   move      *blank        ##ERR
     C                   CALL      'W1057R01'
     C                   PARM      dbrrn2        ##RRN             9 0
     C                   PARM      ' '           ##ELFL            1
     C                   PARM                    ##ERR             1
      *
      /free

             If ##err = 'L';
                error = *on;
                Leave;
             Endif;

          Endif;

          Reade(n) key1WMEML PEM00;
       Enddo;

       Setll keyWMALL PAL00;
       Reade keyWMALL PAL00;

       Dow not %eof and not error;
          If ALELTP = 'E';
             If divAdd  = *Off Or (divAdd  = *On And newDiv# = ALdiv);
      /end-free
      *
     C                   move      *blank        ##ERR
     C                   CALL      'W1057R01'
     C                   PARM      dbrrn5        ##RRN             9 0
     C                   PARM      'E'           ##ELFL            1
     C                   PARM                    ##ERR             1
      *
      /free

                If ##err = 'L';
                   error = *on;
                   Leave;
                Endif;

             Endif;

          Endif;

          Reade keyWMALL PAL00;
       Enddo;

       Chain(n) keyWMEML PEM00;

      /end-free
     C                   Endsr                                                  CheckScreen9 Subr


       //===================================================================
       // LoadScreen9 - Load the initial fields for Additional Locations screen
       //===================================================================
     C     LoadScreen9   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnPrev    = *off;

       Exsr LoadGrid9;

       Exsr LoadOwn9;

       Exsr ClearScreen9;

      /end-free
     C                   Endsr                                                  LoadScreen9 Subr

       //===================================================================
       // LoadGrid9 - Load the billing grid
       //===================================================================
     C     LoadGrid9     Begsr
      /free

       // Clear the subfile.
       clrDiv9 = *On;
       Write rcdOwner;
       clrDiv9 = *Off;
       rrnDiv9 = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd  = *Off Or (divAdd  = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDiv9Fyr  =  scFyr;
             scDiv9Div  =  EMDiv;
             scDiv9DBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDiv9Fed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDiv9PolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDiv9Fein =  EMFein;
             scDiv9ELrr =  dbrrn;
             scDiv9EMrr =  dbrrn2;
             scDiv9RtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDiv9STRP =  'Yes';
             Else;
                scDiv9STRP =  'No';
             Endif;

             rrnDiv9 =  rrnDiv9 + 1;
             Write div9SFL;

          Endif;
        Endif;

        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDiv9 = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGrid9 Subr

       //===================================================================
       // LoadOwn9 - Load the owner/officer grid
       //===================================================================
     C     LoadOwn9      Begsr
      /free

       // Clear the subfile.
       clrOwner = *On;
       Write rcdOwner;
       clrOwner = *Off;
       rrnOwner = 0;

       // Populate the subfile
       Setll keyWMOOL POO00;
       Reade keyWMOOL POO00;

       Dow not %eof;

        If divAdd  = *Off Or (divAdd  = *On And newDiv# = OOdiv);

          scOwn9Last =  OOOLSN;
          scOwn9Firs =  OOOFSN;
          scOwn9Perc =  OOOFPC;
          scOwn9Titl =  OOTITL;
          If OOEXFL = 'I';
             scOwn9Covr = 'Included';
          Else;
             scOwn9Covr = 'Excluded';
          Endif;
          scOwn9EfDt =  dtMDYY(OOEfdt);
          scOwn9ExDt =  dtMDYY(OOExdt);
          scOwn9Clas =  OOCLAS;
          scOwn9Div  =  OODiv;
          scOwn9Enty =  OOELID;
          scOwn9OOrr =  dbrrn6;

          rrnOwner =  rrnOwner + 1;
          Write own9SFL;

        Endif;

          Reade keyWMOOL POO00;
       Enddo;

       //* Position grid at selected record.
       rrnOwner = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadOwn9 Subr

       //===================================================================
       // OwnerPopup - Load the Owner popup for owner edit
       //===================================================================
     C     OwnerPopup    Begsr
      /free

       PrcOwnOffWdw(emRRN:'QTE':ownMode:xxOOrrn);

      /end-free
     C                   Endsr                                                  OwnerPopup Subr


     P Screen9         E
      **********************************************************************************************

       //===================================================================
       // ScreenS - Screen S Processing: Scheduled Rating Screen
       //===================================================================
     P ScreenS         B
     D ScreenS         PI
     D*
     D divMode         s             10
     D xxEMRrn         s              9  0
     D xxELRrn         s              9  0
     D sched           s              1    inz(' ')
     D large           s              1    inz(' ')
     D retro           s              1    inz(' ')
      /free

       Exsr LoadScreenS;

       If qteMode = *on;
          xxCall = 'QTE';
       Else;
          xxCall = 'LIV';
       Endif;

      *** Beg Chg ***** 03/24/22 **************************
      /// Select;
      /// When addMode and schedule_add;
      ///    schedule_add = *off;
      ///    Setll keyWDELL1 PEL01;
      ///    Reade(n) keyWDELL1 PEL01;
      ///    Dow not %eof;
      ///       GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
      ///        ' ':sched:large:retro);
      ///       If sched = 'Y';
      ///          xxELRrn = dbrrn7;
      ///          Exsr SchedulePopup;
      ///       Endif;
      ///       Reade(n) keyWDELL1 PEL01;
      ///    Enddo;
      ///    Exsr LoadGridS;
      /// when divAdd  = *On and schedule_add;
      ///    schedule_add = *off;
      ///    scDiv = newDiv#;
      ///    Setll key1WDELL1 PEL01;
      ///    Reade(n) key1WDELL1 PEL01;
      ///    Dow not %eof;
      ///       GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
      ///        ' ':sched:large:retro);
      ///       If sched = 'Y';
      ///          xxELRrn = dbrrn7;
      ///          Exsr SchedulePopup;
      ///       Endif;
      ///       Reade(n) key1WDELL1 PEL01;
      ///    Enddo;
      ///    Exsr LoadGridS;
      /// EndSl;

       If (addMode = *on OR divAdd = *On) and
        isFirstS = *On  and PQSchdW <> 'Y' ;
       Else;
          Select;
          When addMode and schedule_add;
             schedule_add = *off;
             Setll keyWDELL1 PEL01;
             Reade(n) keyWDELL1 PEL01;
             Dow not %eof;
                GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                 ' ':sched:large:retro);
                If sched = 'Y';
                   xxELRrn = dbrrn7;
                   Exsr SchedulePopup;
                Endif;
                Reade(n) keyWDELL1 PEL01;
             Enddo;
             Exsr LoadGridS;
          when divAdd  = *On and schedule_add;
             schedule_add = *off;
             scDiv = newDiv#;
             Setll key1WDELL1 PEL01;
             Reade(n) key1WDELL1 PEL01;
             Dow not %eof;
                GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                 ' ':sched:large:retro);
                If sched = 'Y';
                   xxELRrn = dbrrn7;
                   Exsr SchedulePopup;
                Endif;
                Reade(n) key1WDELL1 PEL01;
             Enddo;
             Exsr LoadGridS;
          EndSl;
       EndIf;
      *** End Chg ***** 03/24/22 **************************

       Dou btnNext = *on or btnFinish = *on or btnPrev = *on;

      *** Beg Add ***** 03/24/22 **************************
        If (addMode = *on OR divAdd = *On) and
         isFirstS = *On  and PQSchdS <> 'Y' ;
           btnNext = *On;
           isFirstS  = *Off;
           //*** beg add ***** 10/10/25 ***********************
           If isPrev = *on;
              btnPrev = *on;
              btnNext = *off;
           Endif;
           //*** end add ***** 10/10/25 ***********************
        Else;
           Exfmt rcdSchedul;
           //*** beg add ***** 10/10/25 ***********************
           isPrev = *off;
           //*** end add ***** 10/10/25 ***********************
       EndIf;
      *** End Add ***** 03/24/22 **************************
      *** Beg Del ***** 03/24/22 **************************
          //*** //*  Process Screen S
          //*** Exfmt rcdSchedul;
      *** End Del ***** 03/24/22 **************************

          //Perform actions on screen.
          Select;

             //* Exit program
             When btnExit = *on;
                Leave;

             Other;

                edit = *on;

                //*  Launch Popup for Grid Edit
                Readc divSSFL;
                If not %EOF() and edit;
                   If divSEdit = *on or divSDelete = *on;
                         If divSEdit = *on;
                            If scDivSSchd = 'Y';
                               xxELRrn = scdivSELrr;
                               Exsr SchedulePopup;
                            Else;
                               PopOneBtn('Rating state for this division is no+
                                t setup for Scheduled Rating, or no factor ha+
                                s been entered.':'Edit Scheduled Rating Infor+
                                mation':'Enter');
                            Endif;
                            divSEdit = *off;
                         Endif;
                         //*** Beg Chg ***** 06/08/22 *********************
                         //If divSDelete = *on;
                         //   divMode = 'D';
                         //   xxELRrn = scdivSELrr;
                         //   Exsr DivisionPopup;
                         //   xxELRrn = scdivSELrr;
                         //   divSDelete = *off;
                         //Endif;
                         If divSDelete = *on;
                            divMode = 'D';
                            xxELRrn = scdivSELrr;
                            DeletePolYear(xxELRrn);
                            xxELRrn = scdivSELrr;
                            divSDelete = *off;
                         Endif;
                         //*** End Chg ***** 06/08/22 *********************
                         edit = *off;
                         Exsr LoadGridS;
                   Endif;
                Endif;

                //*  Edit Sched Screen for Errors.
                If edit;
                   Exsr CheckScreenS;
                Endif;

                If error;
                   btnNext   = *off;
                   btnFinish = *off;
                   btnPrev   = *off;
                Endif;

                If btnPrev = *on;
                   //*** beg add ***** 10/10/25 ***********************
                   isPrev = *on;
                   //*** end add ***** 10/10/25 ***********************
                   screen# = 2;
                   isFirst2 = *On;
                Endif;

                If btnNext = *on;
                   If divAdd = *on;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       ' ':sched:large:retro);
                   Else;
                      GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
                       'Y':sched:large:retro);
                   Endif;

                   Select;
                      When large = 'Y';
                         screen# = 3;
                         isFirst3 = *On;
                      When retro = 'Y';
                         screen# = 4;
                         isFirst4 = *On;
                      Other;
                         screen# = 5;
                         isFirst5 = *On;
                   Endsl;
                Endif;

          Endsl;

       Enddo;

      /end-free

       //===================================================================
       // ClearScreenS - Clear error/button fields
       //===================================================================
     C     ClearScreenS  Begsr
      /free

       error      = *off;


      /end-free
     C                   Endsr                                                  ClearScreenS Subr

       //===================================================================
       // CheckScreenS - Check Scheduled Rating Screen for Errors
       //===================================================================
     C     CheckScreenS  Begsr
      /free

       Exsr ClearScreenS;


      /end-free
     C                   Endsr                                                  CheckScreenS Subr


       //===================================================================
       // LoadScreenS - Load the initial fields for scheduled rating screen
       //===================================================================
     C     LoadScreenS   Begsr
      /free

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;
       Chain(n) keyWMNBL PNB00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

       btnFinish  = *off;
       btnNext    = *off;
       btnPrev    = *off;

       Exsr LoadGridS;

       Exsr ClearScreenS;

      /end-free
     C                   Endsr                                                  LoadScreenS Subr

       //===================================================================
       // LoadGridS - Load the billing grid
       //===================================================================
     C     LoadGridS     Begsr
      /free

       // Clear the subfile.
       clrDivS = *On;
       //*** Beg Add ***** 12/05/25 **********************************
       If LastWindow = 10;
          visNxtscS = *Off;
          leftCn1SS = 995;
       Else;
          visNxtscS = *On;
          leftCn1SS = 890;
       Endif;
       //*** End Add ***** 12/05/25 **********************************
       Write rcdSchedul;
       clrDivS = *Off;
       rrnDivS = 0;

       // Clear totals

       // Populate the subfile
       Setll key1WMEML PEM00;
       Reade(n) key1WMEML PEM00;

       Dow not %eof;

        If divAdd  = *Off Or (divAdd  = *On And newDiv# = EMdiv);
          Chain(n) key1WDELL PEL00;

          If %found;

             scDivSFyr  =  scFyr;
             scDivSDiv  =  EMDiv;
             scDivSDBA  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'DBA');
             scDivSFed  =  GetEmpFullName(scEmp#:EMDiv:x#RRN:scFnd:scCo#:'FED');
             scDivSPolR =  %EditW(dtMDYY(ELPfdt):'  /  /    ') + ' to ' +
              %EditW(dtMDYY(ELPtdt):'  /  /    ');
             scDivSFein =  EMFein;
             scDivSELrr =  dbrrn;
             scDivSEMrr =  dbrrn2;
             scDivSRtSt =  GetRatingState(scCo#:scFnd:scEmp#:EMDiv:scFyr);
             If eMSTRP = 'Y';
                scDivSSTRP =  'Yes';
             Else;
                scDivSSTRP =  'No';
             Endif;

             GetEmpRating(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:
              ' ':sched:large:retro);
             scDivSSchd = sched;

             divSEdtVis = *off;
             If sched = 'Y';
                divSEdtVis = *on;
             Endif;

             rrnDivS =  rrnDivS + 1;
             Write divSSFL;

          Endif;
        Endif;

        Reade(n) key1WMEML PEM00;
       Enddo;

       //* Position grid at selected record.
       rrnDivS = 1;

       Chain(n) keyWMEML PEM00;
       Chain(n) keyWDELL PEL00;

       elRRN = dbrrn;
       emRRN = dbrrn2;

      /end-free
     C                   Endsr                                                  LoadGridS Subr

       //===================================================================
       // DivisionPopup - Load the location master popup for division add/edit/delete
       //===================================================================
     C     DivisionPopup Begsr
      /free

          DspBillDiv(xxELrrn:xxCall:divMode);

      /end-free
     C                   Endsr                                                  DivisionPopup Subr

       //===================================================================
       // SchedulePopup - Load the Scheduled Rating popup for division edit
       //===================================================================
     C     SchedulePopup Begsr
      /free

       SchdRatingEdit(xxELrrn:quoteMode);

      /end-free
     C                   Endsr                                                  SchedulePopup Subr


     P ScreenS         E
      **********************************************************************************************

       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0 const
     d   inFND                        3  0 const
     d   inFYR                        3  0 const
     d   inEMP#                       9  0 const
     d   inDIV                        5  0 const
     d   inDate                       7  0
     d   inType                       1    options(*nopass)
     d   inClas                       5  0 options(*nopass)
     d*
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Date        s              7
     d   ##Type        s              1
     d   ##Clas        s              5
     d   outDate       s              7  0
     d*

      /free

       ##Type = *blank;
       If %parms >= 7;
          ##Type = inType;
       Endif;

       ##Clas = *blanks;
       If %parms >= 8;
          ##Clas = %editc(inClas: 'X');
       Endif;

       ##CO# = %editc(inCO#: 'X');
       ##FND = %editc(inFND: 'X');
       ##FYR = %editc(inFYR: 'X');
       ##EMP# = %editc(inEMP#: 'X');
       ##DIV = %editc(inDIV: 'X');
       ##Date = %editc(inDate: 'X');

       SReffdAPI(##CO#:##FND:##FYR:##EMP#:##DIV:##Date:##Type:##Clas);

       outDate = %int(##Date);

       Return outDate;
      /end-free
     P SrEffd          E
      *==================================================================
      * ChkAppSecurity - Compare user and option/function security settings
      *                  for selected actions in change mode only. Returns
      *                  *on if user is allowed access.
      *==================================================================
     p ChkAppSecurity  b
     d ChkAppSecurity  pi              n
     d  inAction                     20    Const

     d  outPgmID       s             10
     d  outType        s              1
     d  outNbr         s              2  0
     d  inEnv          s              1    inz('L')
     d  intIdx         s              2  0
     d  inMessage      s            100
     d  isAllowed      s               n   inz(*on)
     d  isNotAllowed   s               n   inz(*off)

      /Free
       If not(chgMode);
          Return isAllowed;
       Endif;

       Eval intIdx = %Lookup(inAction : aCTxRef);
       If intIdx <> 0;
          outPgmID = %subst(oFDxRef(intIdx) : 1 : 10);
          outType  = %subst(oFDxRef(intIdx) : 11 : 1);
          outNbr   = %int(%subst(oFDxRef(intIdx) : 12 : 2));
       Endif;

       If qteMode = *on;
         inEnv = 'Q';
       Endif;

       isAllowed = CheckOFSec(outPgmID : outType : outNbr :
        inEnv : inMessage);

       If isAllowed = *off;
          PopOneBtn(inMessage : 'Enter');
          Return isNotAllowed;
       Else;
          Return isAllowed;
       Endif;

      /End-Free

     p ChkAppSecurity  e

      *** Beg Add ***** 06/08/22 *********************************
      *=====================================================
      * Delete Policy year
      *=====================================================
     p DeletePolYear   b
     d DeletePolYear   pi
     d  inRrn                         9  0 Const

     d xxElRrn         s              9  0
     d strPrYearCx     s              1a   inz
     d strCxFnd        s              1a   inz
     d inCo#           s              3  0
     d inFnd           s              3  0
     d inFyr           s              3  0
     d inEmp#          s              9  0
     d inDiv           s              5  0
     d strMessage      s            256
     d inQTE           s              1
     d outIND          s              2  0
     d errInd          s              2  0
     d errMsgId        s              7
     d xxDelResp       s               n   inz
      /Free
        xxElRrn = inRrn;
        Chain(n) xxElRrn PEL0P;
        inQTE = 'W';
        outIND =  0;
        strMessage = '  ';
        SrDltChk(xxElRrn:inQTE:outInd);
        If outIND <> 0;
           errInd = outIND;
           Select;
           When errInd = 72;               //Payments Exist
              errMsgId = 'WCR1097';
           When errInd = 71;               //Claims Exist
              errMsgId = 'WCR0623';
           When errInd = 74;               //A/R Exists in prior month
              errMsgId = 'WCR1095';
           When errInd = 77;               //Not Authorized
              errMsgId = 'WC30020';
           When errInd = 80;               //NCCI Submission Status '4' Exist
              errMsgId = 'WC30100';
           When errInd = 81;               //Non x-fer'd A/R Batch Rcds Exist
              errMsgId = 'WC30101';
           When errInd = 83;               //State Reporting Div Billings Exist
              errMsgId = 'WC30335';
           When errInd = 84;               //Detail History Exist
              errMsgId = 'WC32390';
           When errInd = 86;               //Payroll Exist
              errMsgId = 'WCR4808';
           When errInd = 87;               //Employer Master Cx'd
              errMsgId = 'WC32410';
           When errInd = 89;               //Only one Policy year
              errMsgId = 'WCR4827';
           Endsl;
           strMessage = RtvMsgD('WMESGG':errMsgId);
           PopOneBtn(strMessage:'Error');
        Else;
           xxDelresp = PopTwoBtn('Do you want to delete this Policy Year?':
            'Confirm Delete of Employer Policy Years':'Yes':'No');
           If xxDelResp = *On;
              inCo# = ELco#;
              inFnd = ELfnd;
              inEmp#= ELemp#;
              inDiv = ELdiv;
              inFyr = ELfyr;
              inQte = 'W';
              //Delete Policy year related files
              SrDltRltFiles(inCo#:inFnd:inEmp#:inDiv:inFYR:inQte);
              Chain (inCo#:inFnd:inEmp#:inDiv:inFyr) PEL00;
              If %found(wdell);
                 Delete PEL00;
              Endif;
              Exsr UpdEmpMst;
              PopOneBtn('Policy Year deleted.': 'Delete');
           Endif;

        Endif;
        //===================================================================
        // UpdEmpMst - Update Employer Master: Clear Cancellation date
        //===================================================================
        Begsr UpdEmpMst;
         //Clear CX date on Employer master if Renewal term is cancelled and
         //Renewed year is being deleted and prior year is not cancelled.
         strPrYearCx = 'N';                           //Prior Year CX exist
         strCxFnd    = 'N';                           //Prior Year CX Rcd Found
         Chain(n) (ELco#:ELfnd:ELemp#:ELdiv) pem00;
         If %Found(wmeml);
            If EMCad1 >= ELpfdt;
               //Check if prior year cancellation exist with NO Reinstatement
               Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pxh02;
               Readpe (ELco#:ELfnd:ELemp#:ELdiv) pxh02;
               DoW not %eof and strCxFnd = 'N';
                  If XHCXDT = EMCad1;
                     strCxFnd = 'Y';
                     If XHREDT = 0;
                        strPrYearCx = 'Y';
                     Endif;
                  Endif;
                  Readpe (ELco#:ELfnd:ELemp#:ELdiv) pxh02;
               Enddo;
               If strPrYearCx = 'N';         //No CX Exist in prior year
                  //Clear CX date
                  Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;
                  If %Found(wmeml);
                     Clear EMCad1;
                     Update pem00;
                  Endif;
               Endif;
            Endif;
         Endif;
       Endsr;

      /End-Free

     p DeletePolYear   e
      *** End Add ***** 06/08/22 *********************************
      *=====================================================
      * Default Vendor History on Policy and Division Add
      *  PEOs - Default from PEO
      *  Non-PEOs - Default from WMFNP if default vendor is set up
      *=====================================================
     pDefaultVendorHistory...
     p                 b
      /Free
       If FNpeoF <> *blank;
          If %Open(WDVHL1);
             Close WDVHL1;
          Endif;
          If qteMode = *On;
             strCommand = 'OVRDBF FILE(WDVHL1) TOFILE(' +
              %Trim(RtvQuoteLib('*LIV')) + '/WDVHL1)';
             Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
          Endif;
          Open WDVHL1;
          Setll (scCo#:scFnd:1:0:scFyr) wdvhl1;
          Dou %Eof;
             Reade(N) (scCo#:scFnd:1:0:scFyr) wdvhl1;
             Select;
             When %Eof;
                Iter;
             Endsl;
           Chain(n) (scCo#:scFnd:scEmp#:EMDiv:scFyr:VHVdr#:VHEfdt) pvh00;
           If not %found;
             VHemp# = scEmp#;
             VHdiv  = EMdiv;
             VHpgm  = Q1PGM;
             VHuser = Q1USER;
             VHdt   = Dateto7(SystemDate());
             VHtime = Timeto6(SystemTime());
             VHiPgm = VHpgm;
             VHiUsr = VHuser;
             VHiTim = VHtime;
             VHiDt  = VHdt;
             Write pvh0p;
           Endif;
          Enddo;
          Close WDVHL1;
          If qteMode = *On;
             strCommand = 'DLTOVR FILE(WDVHL1)';
             Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
          Endif;
          Return;
       Endif;
       If FNdftVend# <> *Zero;
          If Not(%Open(WDVHL1));
             Open WDVHL1;
          Endif;
          Chain(N) (scCo#:scFnd:scEmp#:EMdiv:scFyr) wdvhl1;
          If Not %Found;
             VHco#  = scCo#;
             VHgrp  = scFnd;
             VHemp# = scEmp#;
             VHdiv  = EMdiv;
             VHfyr  = scFyr;
             VHvdr# = FNdftVend#;
             VHefDt = ELpfDt;
             VHpgm  = Q1PGM;
             VHuser = Q1USER;
             VHdt   = Dateto7(SystemDate());
             VHtime = Timeto6(SystemTime());
             VHiPgm = VHpgm;
             VHiUsr = VHuser;
             VHiTim = VHtime;
             VHiDt  = VHdt;
             Write pvh0p;
          Endif;
          Close WDVHL1;
       Endif;
      /End-Free
     pDefaultVendorHistory...
     p                 e
** aCTxRef ------------->oFDxRef
Delete Division          W0050R    104
Delete Employer          W0050R    104
Add Division             W0050R    106
