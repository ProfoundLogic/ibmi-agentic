     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WPILOCMAIN - Policy Location Master - Main processing module
     h nomain
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 05/25/12                                         *
    *  Programmer: C:191                                            *
    *  Project   : WC2116                                           *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      * 10/22/25 C:349 Support: 119523                                *
      *                Display billing payment plan soft error message*
      *                when S/R Online enabled and plan not elibible  *
      *                                                               *
      * 08/04/25 C:079 Support: 119946 (Sapiens)                      *
      *                Set strDftSrD3 instead of strSrdMsg            *
      *                                                               *
      * 04/24/25 C:079 Support: 118909 (Sapiens)                      *
      *                Override to WMP3P member in case override was  *
      *                set to the ARCHIVE member                      *
      *                                                               *
      * 07/11/24 C:333 Support: 115909 (UBIC)                         *
      *                Error Processing Phy Address Endorsement.      *
      *                                                               *
      * 04/29/24 C:340 Project: WC2965                                *
      *                  Recompile the program for the screen changes *
      *                                                               *
      * 03/04/24 C:79  Project: WC2951                                *
      *                Recompile due to changes to WTPOLDTL           *
      *                                                               *
      * 09/11/23 C:291 Project: WC2920                                *
      *                Changed Action History grid (sflOptIn) to load *
      *                all the records.                               *
      *                Turn on filtering on Action History grid.      *
      *                                                               *
      * 03/07/23 C:291 Project: WC2896                                *
      *                Added fillDetailBox on SflAddNam onrowclick    *
      *                event and on rcdAddNam onLoad.                 *
      *                Added detail box below the grid.               *
      *                Added hidden fields scANNameI, scANNameP and   *
      *                scANCxRsn on SflAddNam grid.                   *
      *                                                               *
      * 11/03/22 C:291 Project: SY436                                 *
      *                Fix Subfile sflLocYr alignment on rcdLocEnt.   *
      *                Also changed tab panel "left" property to 250px*
      *                Changed ScOOSflTop and scOOLblTop to move      *
      *                Owner/Office grid 5px down.                    *
      *                                                               *
      * 03/25/22 C:333 Project: SY434                                 *
      *                - Update Grids for Sorting Filtering Upgrade   *
      *                                                               *
      * 03/17/22 C:079 Support: 101483 (In-House)                     *
      *                EFT grid disappears after Toolbar popup        *
      *                                                               *
      * 02/16/22 C:333 Support: 100232 (In-House)                     *
      *                Policy Location Master Showing EFT Grid When it*
      *                Should Not                                     *
      *                                                               *
      * 09/30/21 C:300 Project: WC2663, Support: 98388 (In-House)     *
      *                Prevent the 3rd Party subfile from being loaded*
      *                if the 3rd Party module is not enabled.        *
      *                                                               *
      * 06/23/21 C:318 Project WC2302 - Auto tab closure for          *
      *                deletion.                                      *
      *                                                               *
      * 08/17/21 C:079 Project: WC2663                                *
      *                Fix custom SQL on Opt In Grid                  *
      *                Additional changes to 3rd Pty and Opt In grids *
      *                                                               *
      * 08/02/21 C:298 Project: WC2820                                *
      *                Update the Loss History grid to only show      *
      *                zero, when zero was reported.                  *
      *                                                               *
      * 07/29/21 C:062 Ticket: 97036(99) WC2663                       *
      *               -Fix account#                                   *
      *          C:33 -Load a value to 3rd Party grid RRN             *

      * 05/17/21 C:180 Project: WC1986                                *
      *                Check for SRD default info flag                *
      *                when editing EFT records                       *
      *                                                               *
      *                Add 3rd Party Info Log subfile                 *
      *                                                               *
      *                Display note for non-state reporting divisions *
      *                if the state reporting division                *
      *                uses defaulted eft or 3rd party info.          *
      *                                                               *
      *                When loading 3rd party subfile, distinguish    *
      *                between Quote and Live Records.                *
      *                                                               *
      *                - New parms for RequiresPreNote                *
      * 03/30/21       - Remove pre-note and approved columns from    *
      *                  EFT grid.                                    *
      *                - Add Starting Year and Environment columns to *
      *                  EFT and 3rd party grids.                     *
      *                - Hide "Request Pre-Note" link when not        *
      *                  applicable.                                  *
      *                - Show Quote/Live Eft/3rd Party records as     *
      *                  it may apply.                                *
      *                                                               *
      * 03/11/21 C:33  Project: WC2663                                *
      *                Add 3rd Party Payments to 'Third Party Information
      *                                                               *
      * 04/29/20 C:136 Project: SY407 (DE675)                         *
      *                3rd Party Payment flag wasn't persistent.      *
      *                Reset P3RRN and scPayRrn to correct.           *
      *                                                               *
      * 02/17/20 C:33  Project: SY406                                 *
      *               -Include ACORD number with questions.
      *               -List child questions with parent.
      *               -Add application submitter information.
      *                                                               *
      * 02/11/20 C:180 Project: 150WDU                                *
      *                Tech specs changes on PEO conditioning.
      *
      * 12/30/19 C:180 Project: 150WDU                                *
      *                Change criteria for type of bus                *
      *                                                               *
      * 10/07/19 C:65  Project: SY406/Phw#83859 (SCS)                 *
      *                Do not display connect/portal tab if there are *
      *                no records in WIEMP.                           *
      *                                                               *
      * 06/26/19 C:65  Project: SY407                                 *
      *                Add third party online payment information to  *
      *                the EFT tab, and rename tab to Payment Infor-  *
      *                mation, if online payment module is enabled.   *
      *                                                               *
      * 06/12/19 C:300 Support: 79730 (CoveRisk)                      *
      *                Recompile due to WPILOCMST screen changes.     *
      *                                                               *
      *  05/21/19  C:180 Project Disable 150WDU:
      *                  Temporarily using client id 999 for
      *                  change dated 04/08/19
      *
      *  04/08/19  C:180 Project 150WDU
      *                  Add type of business column to
      *                  Additional Named Insured grid.
      *
      *  03/04/19  C:33  Project SY390
      *                  Soft-code 'Portal'
      *                                                               *
      * 12/17/18 C:33  Project: SY406                                 *
      *                Display Portal captured information.           *
      *                                                               *
      * 06/11/18 C:65  Project: 150WAQ                                *
      *                Activate the collateral tab.                   *
      *                                                               *
      * 03/05/18 C:136 Project: 150WCT                                *
      *                Add midterm endorsement for additional named   *
      *                insured                                        *
      *                                                               *
      * 02/23/18 C:291 Support: WC2521                                *
      *                Add Non-Specific Address                       *
      *                                                               *
      * 02/06/18 C:291 Support: 63953                                 *
      *                Change Division dropdown "max choices" property*
      *                to 100,000. Screen change only.                *
      *                                                               *
      * 01/19/18 C:291 Support: 63802                                 *
      *                Add Zero Fill on Federal Id # on Additional    *
      *                Named Insured grid. Screen change only.        *
      *                                                               *
      * 10/16/17 C:33  Project: 150WAQ                                *
      *                Add collateral tab for CLT150.                 *
      *                                                               *
      * 09/18/17 C:65  Project: 150WAP                                *
      *                Condition Individual Name Checkbox for PEOs.   *
      *                                                               *
      * 08/03/17 C:291 Project: 2504                                  *
      *                Turn Leasing Id Textbox into Dropdown. Screen  *
      *                changes only.                                  *
      *                                                               *
      * 07/28/17 C:291 Project: 150WAP                                *
      *                Add Individual Name Checkbox.                  *
      *                                                               *
      * 05/24/16 C:308 Project: SY378- Revert changes done move v16r5 *
      *                Adding filter to Entity/location Information   *
      *                                                               *
      * 01/29/16 C:310 Support: 37835                                 *
      *                Adding popup window for maximum record loaded  *
      *                in SflAddloc                                   *
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      *    12/07/15 C:295 Phaseware: 29738 LUBA                       *
      *                   To Populate the Depart grid after adding    *
      *                   Dept code.                                  *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *
      *                                                               *
      * 05/12/15 C:306 Phaseware: 29738 LUBA                          *
      *                Recompile only                                 *
      *                                                               *
      * 06/26/15 C:308 Phaseware: 35528                               *
      *                Recompile due to WPILOCMST screen changes.     *
    *                                                               *
      * 09/21/15 C:180 Support 37344                                *
      *                Right justify FEIN.                          *
      *                                                             *
      * 05/29/15 C:306 Phaseware: 34976                               *
      *                Fix the tab swapping issue when save the infor *
      *                mation on billing division tab.                *
    *                                                               *
      * 04/20/15 C:250 Phaseware: 31303 LCTA                          *
      *                add fields for addition employer informatinon  *
    *                                                               *
      * 04/17/15 C:242 Project: 132WQS                               *
      *                Retrofit of Custom Code to Base for CompSuite:*
      *                Added #ofShiftEmployees, MPNprovideName/Add.  *
      *                Moved wdigets below MPN into container with   *
      *                variable top position.                        *
      *                                                               *
      * 04/16/15 C:65  Project: SY251                                 *
      *                Add API call to launch new Department          *
    *                letters.                                       *
      *                                                              *
      * 04/11/15 C:295 Support: 33960                                 *
      *                Correct Division on policies with blank effec  *
      *                -tive date (EMefd1)                            *
      *                To pass on FYR to Location Master              *
    *                                                               *
      * 03/03/15 C:302 PROJECT: 114WRG                               *
      *                Policy Concentration Details                  *
      *                                                              *
      *  01/29/15 C:180 Support: 32647    No changes                  *
      *                 Fix issue that caused system to keep sending  *
      *                 an error message when user tried to change    *
      *                 the state reporting division.                 *
      *                 System incorrectly created more than 1 state  *
      *                 reporting division.                           *
      *                                                               *
      *                 Same fix as 09/07/14                          *
      *                                                               *
    *  01/09/15 C:283 Redmine: 4742 - V14R8                         *
    *                 For a Renewal Quote, the screen fields        *
    *                 should be protected for Renewal division but  *
    *                 if new divisions are added, they should be    *
    *                 editable to allow changes.                    *
    *                                                               *
    *  11/04/14 C:65  Redmine: 4638 - T.H.E.                        *
    *                 Recompiled for screen file change.            *
      *                                                               *
      *  10/13/14 C:291 RED 3161                                      *
      *                 Change the date definition on the grid from   *
      *                 Number to date data type.                     *
      *                                                               *
      *  09/07/14 C:136 Support: WA099 86699                          *
      *                 Billing division tab wasn't loading when      *
      *                 division was changed.                         *
      *                                                               *
      *  08/07/14 C:302 Support: 86410                                 *
      *                 Program failing on endorsments with an        *
      *                 invalid pointer.                              *
      *                                                               *
      *  05/27/14 C:291 Redmine: 4138                                 *
      *                 Added logic to automatically shift to the     *
      *                 screen with errors.                           *
      *                                                               *
      *  04/29/14 C:291 Redmine: 1948                                 *
      *                 Fixed Owner/Officer % edit issue              *
      *                 Change loading of Billing division tab        *
      *                                                               *
      *  04/10/14 C:291 Redmine: 4085                                 *
      *                 EFT information tab should be enabled if      *
      *                 either field of the two SWCPF felds are set   *
      *                 set to "Y"                                    *
      *                                                               *
      *  01/27/13 C:189 Redmine: 3416                                 *
      *                 Employer Federal name not appearing correctly *
      *                 on detail screen                              *
      *                                                               *
      *  08/05/13 C:283 Support: WA133 82980                          *
      *                 Pass xxQTE mode when updating the details     *
      *                 on Location Master.                           *
      *                                                               *
      *  06/21/13 C:189 Redmine 3199                                  *
      *                 EFT Information tab should not appear for     *
      *                 the clients which have not purchased EFT      *
      *                                                               *
      *  05/08/13 C:189 Project: 133WNP                               *
      *                 Chapter should have a golden splat for LUBA   *
    *                                                               *
      *  01/22/13  C:277  PROJECT: 133WNZ                             *
      *                   Change the calling for quote and live       *
     *                                                               *
      *  04/24/13 C:279 Redmine 2477                                  *
      *                 Enable year drop down for year selection.     *
      *                                                               *
    *  04/04/13  C:65  Project SY266 Redmine 2616                   *
    *                  Change checkbox in detail area for addl      *
    *                  locations to be Yes/No text instead.         *
    *                                                               *
      *  01/22/13  C:277  PROJECT: 133WNZ                             *
      *                   Add Processing for scSExpireV               *
     *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwpilocmst cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdBillDiv)
     f                                     usropn
     fwpilocmaincf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflAddLoc:RrnAddLoc)
     f                                     sfile(SflLocYr:RrnLocYr)
     f                                     sfile(SflOwnOff:RrnOwnOff)
     f                                     sfile(SflDepart:RrnDept)
     f                                     sfile(SflEftInfo:RrnEFTInfo)
     f                                     sfile(SflAddNam:RrnAddNam)
     f***  Begin Add  ***  10/16/17  **************************************
     f                                     sfile(SflCollat:RrnCollat)
     f                                     sfile(SflColAR:RrnColAR)
     f***  End   Add  ***  10/16/17  **************************************
     f***  Begin Add  ***  12/17/18  **************************************
     f                                     sfile(sflLosses:rrnLosses)
     f                                     sfile(sflQuest:rrnQuest)
     f***  End   Add  ***  12/17/18  **************************************
     f*** begin add ***  06/26/19  ****************************************
     f                                     sfile(Sfl3rdPay:rrn3rdPay)
     f*** end add *****  06/26/19  ****************************************
     f*** begin add ***  05/17/21  ****************************************
     f*** Beg Del *** 08/17/21 ********************************************
     f***                                  sfile(Sfl3pLog:rrn3pLog)
     f*** End Del *** 08/17/21 ********************************************
     f*** End   add ***  05/17/21  ***************************************
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmall     if   e           k disk
     f                                     rename(pal01:pal00)
     f                                     infds(alfds0)
     fwmalp     if   e             disk
     f                                     rename(pal01:pal0p)
     fwmefl6    if   e           k disk    rename(pef01:pef06)
     f                                     infds(effds0)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmanl     if   e           k disk    rename(pan01:pan00)
     f                                     infds(anfds0)
     f***  Begin Add  ***  10/16/17  **************************************
     fwmcol1    if   e           k disk
     f                                     infds(cofds1)
     fwtabl     if   e           k disk
     f***  End   Add  ***  10/16/17  **************************************
     f***  Begin Add  ***  04/08/19  **************************************
     fwtgtl     if   e           k disk
     f***  End   Add  ***  04/08/19  **************************************
     fwmemp     if   e             disk
     f                                     rename(pem01:pem0p)
     fwmexl     if   e           k disk
     f                                     rename(pex01:pex00)
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(elFDS)
     fsdofl1    if   e           k disk
     fwmmpl     if   e           k disk    rename(pmp01:pmp00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fssylf     if   e           k disk
     F                                     ignore(SSY01)
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
     Fsmfpl     if   e           k disk    Rename(pfp01:pfp02)
     f*** begin add ***  06/26/19  ****************************************
     Fwtp3l     if   e           k disk    Rename(pp301:pp300t)
     F*** Beg Add *** 04/24/25 ****************************************
     Fwmp3p     if   e           k disk    UsrOpn
     F*** End Add *** 04/24/25 ****************************************
     F*** Beg Chg *** 04/24/25 ****************************************
     F***wmp3l     if a e           k disk    Rename(pp301:pp300)
     Fwmp3l     if a e           k disk    Rename(pp301:pp300) UsrOpn
     F*** End Chg *** 04/24/25 ****************************************
     f                                     infds(p3FDS)
     f*** end add *****  06/26/19  ****************************************
     f*** Beg Add *** 08/17/21 ********************************************
     Fwipsphist if   e             disk
     Fwipslhist if   e           k disk    rename(pps01:ppsh01) Prefix(PSH:2)
     f*** End Add *** 08/17/21 ********************************************

     d alfds0          ds
     d  alrrn                397    400b 0
     d effds0          ds
     d  efrrn                397    400b 0
     d anfds0          ds
     d  anrrn                397    400b 0
     d***  Begin Add  ***  10/16/17  **************************************
     d cofds1          ds
     d  corrn1               397    400b 0
     d***  End   Add  ***  10/16/17  **************************************
     d elFDS           ds
     d  elRRN                397    400b 0
     f*** begin add ***  06/26/19  ****************************************
     d p3FDS           ds
     d  p3RRN                397    400b 0
     f*** end add *****  06/26/19  ****************************************

     d psds          esds                  extname(SZQ1P)

     d ##emRrn         s              9  0
     d ##eftMod        s              1
     d ##eftTyp        s              1
     d ##eftRrn        s              9  0
     d***  Begin Add  ***  10/16/17  **************************************
     d arrTabName      s             50    dim(10) inz
     d i1              s              5  0
     d i2              s              5  0
     disUseCollateral  s              1n   inz
     d***  End   Add  ***  10/16/17  **************************************
     d xxRrn           s              9  0
     d xxEMrrn         s              9  0
     d xxALrrn         s              9  0
     d xxMode          s              1    inz
     d xxNext          s             10    inz
     d xxQte           s              1    inz
     d svMode          s              1    inz
     d svNext          s             10    inz
     d svTab           s                   like(TabIndex)
     d xcRrn           s              9    inz
     d xxHdg1          s                   like(excnt1)
     d xxHdg2          s                   like(excnt1)
     d xxHdg3          s                   like(excnt1)
     d Process         s               n   inz(*off)
     d prvTabIndex     s                   like(TabIndex)
     d prvOOTop        s                   like(scOOTop)
     d xxTabInd        s              5    inz
     d isError         s               n   inz
     d***  Begin Add  ***  04/08/19  **************************************
     d xxKey           s                   like(GTkey) inz
     d xxTbId          s                   like(GTtbid) inz
     d xxANIhdgs       C                   CONST('Name,Federal Id #,Effective -
     d                                     Date,Expiration Date,Division,-
     d                                     Status')
     d xxANIhdgs150    C                   CONST('Name,Federal Id #,Effective -
     d                                     Date,Expiration Date,Division,-
     d                                     Status,Type Of Business')
     d***  End   Add  ***  04/08/19  **************************************
       // Beg del 03/03/15
     d*isDisplay       s               n   inz
       // end del 03/03/15
     d isRenewalQte    s               n   inz
     d RrnAddloc       s              9  0 inz
     d***  Begin Add  ***  10/16/17  **************************************
     d RrnColAR        s              9  0 inz
     d RrnCollat       s              9  0 inz
     d***  Begin Add  ***  10/16/17  **************************************
     d RrnLocYr        s              9  0 inz
     d RrnAddNam       s              9  0 inz
     d RrnOwnOff       s              9  0 inz
     d RrnDept         s              9  0 inz
     d***  Begin Add  ***  12/17/18  **************************************
     dstrPortalTabNm   s             50a   varying inz
     d***  End   Add  ***  12/17/18  **************************************
     d svRrnAddLoc     s              5  0 inz
     d xxEnd#          s              5  0 inz
     d xxCall          s              3    inz('LOC')
     d windowMode      s               n   inz
     d svElid          s                   inz like(ALelid)
     d svLocRrn        s              9  0 inz
     d flgE152         s               n   inz
     d qteMode         s               n   inz
     d flgNBquote      s              1    inz('0')
     D xsKey           s              2
     d flgLoadBDiv     s               n   inz(*On)
     d xxErrorTab      s              5  0 inz(-1)
     d ZeroDate        s               D   inz datfmt(*ISO)
     D BtnFilter5      s              1n   inz
     D str9999Msg      s            256
     D***  Begin Add  ***  05/17/21  ****************************************
     D sysDate7        s              7  0 inz
     D strType         s              1A   inz
     d isEftLnk        s               n   inz(*On)
     D***  End   Add  ***  05/17/21  ****************************************
     D***  Begin Add  ***  06/23/21  ****************************************
     D OutElRrn        s              9  0 export
     D***  End   Add  ***  06/23/21  ****************************************
     d*** Beg Add *** 08/17/21 **********************************************
        Dcl-s strAlias char(500) inz;
     d*** End Add *** 08/17/21 **********************************************
     d*** Beg Add *** 10/22/25 **********************************************
        Dcl-s strSrMsg  char(256) inz;
     d*** End Add *** 10/22/25 **********************************************
      *
     d dsFilter        ds                  likerec(RcdLocFlt) inz

     d* Proto-types
     p***  Begin Add  ***  10/16/17  **************************************
     DAllYrLargeDed    pr             1n
     d  inCo#                         3  0 const
     d  inFnd                         3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
     p***  End   Add  ***  10/16/17  **************************************

     DCheckSecurity    pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTYP                         1    const
     D inCLVL                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D inCTP2                         1    const
     D inCTP3                         1    const
     D inCTP4                         1    const
     D inCTP5                         1    const
     D inCTP6                         1    const
      *
     d MidTermEndorse  pr                  extpgm('WTPOLMEND')
     d   inRRN                        9
     d   inMode                       1
     d   inRtnVal                    10

     d OwnOffShare     pr                  extpgm('W1057R01')
     d   inRRN#                       9  0
     d   inELFL                       1
     d   outErr                       1

     d* Program Start
     d ProgramStart    pr
     d ProcessMe                       n

     d* Main Processing
     d Main            pr

     d* Load Selection fields
     d LoadSelection   pr

     d* Load Detail fields
     d LoadDetail      pr

     d* Load Location/Entity
     d LoadLocEnt      pr              n

     d* Load Location Year info
     d LoadLocYrInf    pr              n
     d  inLocRrn                      9  0

     d* Load Owner/Officer
     d LoadOwnOff      pr              n
     d  inLocRrn                      9  0 options(*nopass)
     d                                     const
     d LoadDepart      pr              n
     d  inDeptRrn                     9  0 options(*nopass)
     d                                     const
     d LoadAddNam      pr              n

       //***  Begin Add  ***  10/16/17  ************************************
     d LoadCollatAR    pr              n
     d LoadCollateral  pr              n
       //***  End   Add  ***  10/16/17  ************************************

     d* Load Additional Location Detail box
     d LoadAddLocDtl   pr
     d  inSFLrrn                      5  0

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d* Process EFT Information
     d LoadEftInfo     pr
       //*** Beg Add *** 04/24/25 ******************************************
       Dcl-Pr RunCommand ExtPgm('QCMDEXC');
        inCmd    char(2750) const;
        inCmdLen packed(15:5) const;
       End-Pr;
       //*** End Add *** 04/24/25 ******************************************

     d/copy SCOPYPOLPR
     d/copy sPrDate
     D***  Begin Add  ***  03/04/19  *****************************************
     D/copy sPrModule
     D***  End   Add  ***  03/04/19  *****************************************
     D***  Begin Add  ***  02/17/20  ****************************************
     D/copy sPrSql
     D***  End   Add  ***  02/17/20  ****************************************
     d/copy sPrString
     d/copy wPrAgent
     D***  Begin Add  ***  03/30/21  ****************************************
     D/copy wPrBank
     D***  End   Add  ***  03/30/21  ****************************************
     d/copy wPrCarrier
     d/copy wPrEmplr
     d/copy wPrGenPop
      //*** Begin Add *** 05/17/21 *****************************************
       Dcl-Pr GetObjLib Extpgm('RTVOBJLIB');
        inObject   char(10)   const;
        inTyp      char(7)    const;
        outLibrary char(10);
       End-Pr;
      //*** End   Add *** 05/17/21 *****************************************
     d/copy wPrGroup
     d/copy wPrLocMst
     d/copy wPrLocMain
     D***  Begin Add  ***  03/11/21  ****************************************
     D/copy wPrPmtSvc
     D***  End   Add  ***  03/11/21  ****************************************
     d/copy wPrProduce
     d/copy wPrRateSet
     d/copy wPrTable
     d/copy sPrAtrium
     d/copy wPrEft
     d/copy wPrUsrPrms
     d/copy sPrLetter
     d/copy wPrPolCpy
     d*** Beg Add *** 04/24/25 ************************************************
     d/copy sPrFiles
     d*** End Add *** 04/24/25 ************************************************
     c* EFT Account File Key List
     c     keyWMEFL6     klist
     c                   kfld                    ELco#
     c                   kfld                    ELfnd
     c                   kfld                    ELemp#
     c                   kfld                    ELdiv
     c     key1WDELL     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFyr

     c* Employer Detail File Key List
     c     keyWDELL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     p***  Begin Add  ***  10/16/17  **************************************
       //===================================================================
       // Determine if all years are using large deductible
       //===================================================================
     PAllYrLargeDed    b
     DAllYrLargeDed    pi             1n
     d  inCo#                         3  0 const
     d  inFnd                         3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
      /Free
       Setll (inCo#:inFnd:inEmp#:inDiv) pel00;
       Dou %eof;
          Reade (inCo#:inFnd:inEmp#:inDiv) pel00;
          Select;
          When %eof;
             Iter;
          Endsl;
          Clear BSlPgm;
          Chain (ELco#:%editc(ELbsCd:'X')) pbs01;
          If BSlPgm <> 'L';
             Return *off;
          Endif;
       Enddo;
       Return *on;
      /End-free
     PAllYrLargeDed    e
     p***  End   Add  ***  10/16/17  **************************************

       //***  Begin Add  ***  12/17/18  ******************************
       //===================================================================
       // Check to see if policy was entered via portal
       //===================================================================
     pChkPortal        b
     dChkPortal        pi             1n

     ddecCount         s              9  0
      /Free
       Exec SQL set option COMMIT=*NONE;
       Exec SQL
        select count(*)
         into :decCount
         from WIEMP
         where EMCO# = :scCo# and EMFND = :scFnd and EMEMP# = :scEmp#;
       //*** begin chg ***  10/07/19  *************************************
       //*****Return decCount >= 0;
       Return decCount > 0;
       //*** end chg *****  10/07/19  *************************************
      /End-free
     pChkPortal        e
       //***  End   Add  ***  12/17/18  ******************************

       //===================================================================
       // LocMaster  - Wrapper Procedure
       //===================================================================
     P LocMaster       B                   export
     D LocMaster       PI
     d  inRrn                         9
     d  inMode                        1
     d  inNext                       10
     d  inTabInd                      5    Options(*nopass)
     d  inQte                         1    Options(*nopass)
      /free
       // Relative Record # for employer detail.
       If %Parms > 0;
         xxRrn   = %int(inRrn);
       Else;
         xxRrn   = 0;
       Endif;

       // Program Mode.
       If %Parms > 1;
         xxMode = inMode;
       Else;
         xxMode = 'D';
       EndIf;

       // Next program
       If %Parms > 2;
         xxNext = inNext;
       Else;
         xxNext = *blanks;
       EndIf;

       If %Parms > 3;
         TabIndex = %int(inTabInd);
         If xxMode <> 'M' And (TabIndex < 0 Or TabIndex > 4);
           TabIndex = 0;
         Endif;
       Else;
         TabIndex = 0;
       Endif;

       prvTabIndex = TabIndex;
       If %Parms > 4;
         xxQte = inQte;
         Select;
         When xxQte = 'Q';
           qteMode = *On;
         Endsl;
       Else;
         xxQte = *Blanks;
       Endif;

       scEnv = xxQte;

       ProgramStart(Process);

       If Process;
         Main();
         inRRN = %Editc(xxRRN:'X');
         inMode = xxMode;
         inNext = xxNext;
         If %Parms > 3;
           inTabInd = *Zeros;
         Endif;
         If %Parms > 4;
           inQte = xxQte;
         Endif;
       Endif;

      /End-free
     P LocMaster       E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D ProcessMe                       n

     D flgInitBillDiv  s               n   inz
     D userLevel       s              3    inz
     D userType        s                   inz like(ofty01)
     D flgRes          s              1    inz
     D flgofEnv        s              1    inz

     d isFltrClr       s              1    inz
     d isFltrPrst      s              1    inz
     d isFltrUpd       s              1    inz
     d ssEnlcNm        s             40    inz
     d ssPhAd1         s             40    inz
     d ssEitp          s              1    inz
     d ssEnLcFein      s              9p 0 inz
     d ssEnLcStat      s              2    inz
     d RtvqteNewRenew  pr                  ExtPgm('WA069C')
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
     d xxNWRW          s              1    inz('0')
      /free

       // Chain to Employer Detail for Program.
       Chain xxRrn pel0p;
       //***** Begin Add  ***  06/23/21  ***************************************
       outElRrn  =xxRrn;
       //***** End Add  ***  06/23/21  ****************************************
       // Process only if valid employer detail record found.
       If not %found;
         processme = *Off;
         Return;
       Endif;

       scActRRn = xxRRn;
       scActMode = xxMode;
       xxEmRRN = GetEmpRRN(ELemp# :ELdiv :ELfnd :ELco#);

       If xxMode <> 'M';
         If not %Open(wpiLocMst);
           Open wpilocmst;
         Endif;
         Clear dsBillDiv;
       Endif;

       scCo# = ELco#;
       scFnd = ELfnd;
       scEmp# = ELemp#;
       scDiv = ELdiv;
       scSelDiv = ELdiv;
       scFyr = ELfyr;
       kkfyr = ELfyr;

       Chain(n) (scCo#:scFnd) pfn00;
       If FNPEOF <> ' ';
          visPEO = *on;
       Else;
          visPEO = *off;
       Endif;

       isRenewalQte = *off;
       If xxQte = 'Q' and GetRnwlQte(ELco#:ELfnd:ELemp#:ELdiv) = 'Y' and
          GetRnwlQteDiv(ELco#:ELfnd:ELemp#:ELdiv) <> 'Y';
          isRenewalQte = *on;
       Endif;

       isUse#Emp = *off;
       isUseMPN  = *off;
       GetClientId(ClientID);
       If ClientID = '132';
          isUse#Emp = *on;
          isUseMPN  = *on;
       Endif;
       //***  Begin Add  ***  10/16/17  *******************************
       arrTabName(1) = 'Billing Division';
       arrTabName(2) = 'Location/Entity Information';
       arrTabName(3) = 'Owners/Officers';
       arrTabName(4) = 'Additional Named Insured';
       //*** begin chg ***  06/26/19  *********************************
       //*****arrTabName(5) = 'EFT Information';
       arrTabName(5) = 'Payment Information';
       //*** end chg *****  06/26/19  *********************************
       i1 = 5;
       isUseCollateral = *off;
       If clientID = '150';
       //*** begin chg ***  06/11/18  *********************************
        //isUseCollateral = *on;
          isUseCollateral = *on;
       //*** end chg *****  06/11/18  *********************************
       Endif;
       If isUseCollateral;
          i1 += 1;
          arrTabName(i1) = 'Collateral';
       Endif;
       //***  Begin Add  ***  12/17/18  ******************************
       Reset strPortalTabNm;
       If ChkPortal();
        //*** Begin Chg *** 03/04/19 *********************************
        //strPortalTabNm = 'Application';
          strPortalTabNm = GetModuleProperty('Portal Module':'Name');
        //*** End   Chg *** 03/04/19 *********************************
          i1 += 1;
          arrTabName(i1) = strPortalTabNm;
       Endif;
       //***  End   Add  ***  12/17/18  ******************************
       scTabNames = arrTabName(1);
       For i2 = 2 to i1;
          Select;
          When arrTabName(i2) = *blank;
             Iter;
          Endsl;
          scTabNames = %trim(scTabNames) + ',' + arrTabName(i2);
       Endfor;
       //***  End   Add  ***  10/16/17  *******************************
       flgOverLay = *On;
       scOOTop = 95;
       prvOOTop = 00;
       wTopLocEnt = 0;
       svRrnAddLoc = 1;

       Exsr SetAllIndOff;
       flgInitBillDiv = *On;
       Select;
       When xxMode='D';
         //Display
         flgInitBillDiv = *On;
         dspMod = *On;
         SCe110 = *On;
         SCe120 = *On;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *On;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *On;
         SCe140 = *On;
         SCe180 = *On;
         hideLYopt = *On;
       When isRenewalQte;
         // Display except for year
         flgInitBillDiv = *On;
         dspMod = *On;
         SCe110 = *On;
         SCe120 = *On;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *On;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *On;
         SCe140 = *On;
         SCe180 = *On;
       //*** Begin Add  05/17/21 ***********************************************
         dspAddEft = *On;
       //*** End   Add  05/17/21 ***********************************************
       // End Add 03/03/15
       When xxMode = 'C';
         //Change
         flgInitBillDiv = *On;
         SCe150 = *On;
         SCe151 = *On;
         SCe152 = *On;
         flgE152 = *On;
         SCe160 = *On;
         SCe161 = *On;
         SCe162 = *On;
         dspAddEft = *On;
         dspEftLnk = *On;
         dspInsEdt = *On;
         dspInsAdd = *On;
       When xxMode = 'M';
         //Mid-Term endorsement processing
         scPnlTitle = 'Mid-Term Endorsement - ';
         windowMode = *On;
         xxEnd# = TabIndex;
         Select;
         When xxEnd# = 150;
           //Addition of location endorsement
           scPnlTitle = %Trim(scPnlTitle) + ' Addition of Location/Entity';
           xxCall = '150';
           flgLocWdw = *On;
           hideLocLin = *On;
           hideLYopt = *On;
           wdwLocEnt = *On;
           SCe150 = *On;
           SCe160 = *Off;
           SCe161 = *Off;
           SCe162 = *Off;
         When xxEnd# = 151;
           //Change of location endorsement
           scPnlTitle = %Trim(scPnlTitle) + ' Change of Location/Entity';
           xxCall = '151';
           flgLocWdw = *On;
           hideLYopt = *On;
           hideLocLin = *On;
           wdwLocEnt = *On;
           SCe151 = *On;
           SCe160 = *Off;
           SCe161 = *Off;
           SCe162 = *Off;
         When xxEnd# = 152;
           //Delete of location endorsement
           scPnlTitle = %Trim(scPnlTitle) + ' Deletion of Location/Entity';
           xxCall = '152';
           flgLocWdw = *On;
           hideLYopt = *On;
           hideLocLin = *On;
           wdwLocEnt = *On;
           SCe152 = *On;
           flgE152 = *On;
           SCe160 = *Off;
           SCe161 = *Off;
           SCe162 = *Off;
         //*** beg add ***** 03/05/18 ***************
         When xxEnd# = 250;
           //Additional Named Insured endorsement
           scPnlTitle = %Trim(scPnlTitle) + ' Additional Named Insured';
           xxCall = '250';
           visAnmWdw = *On;
           dspInsAdd = *On;
           dspInsEdt = *On;
         //*** end add ***** 03/05/18 ***************
         Endsl;
       Endsl;

       If flgInitBillDiv;
         ptrBillDiv = %Addr(dsBillDiv);
         ptrInitData = %Addr(dsInitData);
         flgQte = xxQte;
         If not InitBillDiv(xxEmRRN :xxMode :ptrBillDiv :ptrInitData);
           processme = *off;
           Return;
         Endif;
         dsInitData = dsB;
       Endif;

       If xxQte =  'Q';
         flgofEnv = 'Q';
       Else;
         flgofEnv = 'L';
       Endif;
       // Get user sec level/type
       userLevel = %Editc(GetUsrLvl(Q1user:userType):'X');
       // Check user authority for EFT Links
       Chain(n) ('WTPOLLMST':flgofEnv:'1':2) pof01;
       If %found(sdofl1);
         CheckSecurity(userType:userLevel:OFty01:%Editc(OFlvl:'X'):'1'
          :flgRes:OFty02:OFty03:OFty04:OFty05:OFty06);
         If flgRes <> '0';
           //Deny
           dspEftLnk = *Off;
          //*** Begin Add *** 05/17/21 ***********************************
          isEftLnk = *Off;
          //*** End   Add *** 05/17/21 ***********************************
         Endif;
       Endif;
       // Check user authority for EFT Links
       Chain(n) ('WTPOLLMST':flgofEnv:'2':6) pof01;
       If %found(sdofl1);
         CheckSecurity(userType:userLevel:OFty01:%Editc(OFlvl:'X'):'1'
          :flgRes:OFty02:OFty03:OFty04:OFty05:OFty06);
         If flgRes <> '0';
           //Deny
           dspAddEft = *Off;
         Endif;
       Endif;
       //*** begin add ***  06/26/19  *********************************
       mod3rdPay = IsModEnabled('Third Party Payment');
       mod3rdPayQ = *off;
       //*** end add *****  06/26/19  *********************************

       Chain (scCo#:scFnd) pex00;
       xxHdg1 = exCnt1;
       xxHdg2 = exCnt2;
       xxHdg3 = exCnt3;
       ProcessMe = *On;

       flgNBquote = *off;
       If qteMode = *on;
       xxCo# = %editc(ELco#:'X');
       xxFnd = %editc(ELfnd:'X');
       xxEmp# = %editc(ELemp#:'X');
       xxFyr  = %editc(ELFyr:'X');
       xxNwrw = *blanks;
       callp  RtvqteNewRenew(xxCo#:xxFnd:xxFyr:xxEmp#:xxNwrw);

         If xxNwrw = 'N';
            flgNBquote = *on;
         Else;
            flgNBquote = *off;
         Endif;

       Endif;

       Setll (q1User:'rcdlocFlt') pfp02;
       ReadE (q1User:'rcdlocFlt') pfp02;
       If not %Eof(smfpl);
          Dow not%Eof(smfpl) and FPstat = 'A';
             Select;
               When FPFIELDID = 'ssEnLcNm';
                 ssEnLcNm  = FPvalue;

               When FPFIELDID = 'ssPhAd1';
                 ssPhAd1  = FPvalue;

               When FPFIELDID = 'ssEITp';
                 ssEITp  = FPvalue;

               When FPFIELDID = 'ssEnLcFein';
                 ssEnLcFein = %Dec(FPvalue:9:0);

               When FPFIELDID = 'ssEnLcStat';
                 ssEnLcStat = FPvalue;

              EndSl;
              ReadE (q1User:'rcdlocFlt') pfp02;
          EndDo;
          isFltrPrst = *on;
       Else;
          ssEnlcNm   = *Blanks;
          ssPhAd1    = *Blanks;
          ssEitp     = *Blanks;
          ssEnLcFein = *Zeros;
          ssEnLcStat = *Blanks;
          isFltrPrst = *off;
       EndIf;
       //*** Begin Add ***  05/17/21 *************************************
       sysDate7 = DateTo7(SystemDate);
       //*** End  Add  ***  05/17/21  ************************************
       Return;
       //===================================================================
       // SetAllIndOff
       //===================================================================
       BegSr SetAllIndOff;
         dspMod = *Off;
         SCe110 = *Off;
         SCe120 = *Off;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *Off;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *Off;
         SCe140 = *Off;
         SCe150 = *Off;
         SCe151 = *Off;
         flgE152 = *Off;
         SCe152 = *Off;
         SCe160 = *Off;
         SCe161 = *Off;
         SCe162 = *Off;
         SCe180 = *Off;
         wdwLocEnt = *Off;
         flgLocWdw = *Off;
         flgShwWdw = *Off;
         windowMode = *Off;
         hideLocLin = *Off;
         hideLYopt = *Off;
         dspAddEft = *Off;
         dspEftLnk = *Off;
         dspInsEdt = *Off;
         dspInsAdd = *Off;
        //*** beg add ***** 03/05/18 **********
         visAnmWdw = *Off;
         dspInsAdd = *Off;
         dspInsEdt = *Off;
        //*** end add ***** 03/05/18 **********
       EndSr;
      /end-free
     P ProgramStart    E

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      *
     d doLoad          s               n   inz(*On)
     d outErr          s              1    inz
     d inELFL          s              1    inz
     d decMPNOffset    s              5  0 inz
     d intfyr          s              3  0 inz
        //*** Begin Add *** 05/17/21 ***************************************
        //*** Beg Del *** 08/17/21 *****************************************
        //***Dcl-ds WMP3Parc extname('WMP3P') Qualified Inz End-ds;
        //***Dcl-s strFileLib  char(10) inz;
        //***Dcl-s strAlias char(500) inz;
        //*** End Del *** 08/17/21 *****************************************
        //*** End   Add *** 05/17/21 ***************************************

         flgLoadBDiv = *On;

       //*  Process until exit button is pressed.

       DoU ExitNow = *On;
         // doLoad is initially set to *On,
         // After that HandleScreenActions routine sets it on based on User Acti
         If doLoad;
           //*  Load action bar
           LoadActionBar();
           //***  Begin Add  ***  03/17/22  *************************
           // Initialize EFT to Off so it wont show up on other tabs
           // besides the Payment tab
           Eval EFT = *OFF;
           //***  End   Add  ***  03/17/22  *************************
           //*  Load detail panel
           LoadDetail();
           doLoad = *Off;
         Endif;


         Exsr SaveAddress;

         If windowMode;
           If xxMode = 'M';
             xxCall = 'MTE';
           Endif;
           Exsr ExfmtWindows;
         Else;
           Exsr ExFmtScreens;
         Endif;
         ExSr HandleScreenActions;
       Enddo;

       If xxMode <> 'M';
         If %Open(wpiLocMst);
           Close WpilocMst;
         Endif;
       Endif;
       //===================================================================
       // Ex format screens based on tabindex
       //===================================================================
       BegSr ExfmtScreens;
         APnlHeight = 1450;
         Select;
         When TabIndex = 0;
           TabHeight =1000;
           Exsr WriteAction; //wtactpnl
           Write rcdLMst;    //wpilocmain
           Write rcdDeptGrd; //wpilocmain

       //GET ADMIN/DARTS INFORMATION & LOAD CONTACT HEADINGS

       Chain (scCo#:scFnd) pex00;

       If EXFLXH = *blanks;
          protFLXH = *off;
       Else;
          scFLXH = EXFLXH;
          protFLXH = *on;
       Endif;

       selFLXT='GTTBID=''' + EXFLXT + ''' AND GTSTAT<>''I''';

       Chain(n) (scCo#:scFnd:scEmp#:scDiv) pem00;

       scflxv = EMFLXV;

           ExFmt rcdBillDiv; //wpilocmain
           Exsr ReadAction; //wtactpnl
           Read rcdLMst;     //wpilocmain
           Read rcdDeptGrd; //wpilocmain
         When TabIndex = 1;
           TabHeight = 1300;
           scOOTop = 755;
           decMPNOffset = *zero;
           If isUseMPN = *on;
              decMPNOffset = 100;
           Endif;
           scOOTop = scOOTop + decMPNOffset;
           scYearTop = 535 + decMPNOffset;
           If scOOTop <> prvOOTop;
             prvOOTop = scOOTop;
             Exsr SetOwnOffTops;
           Endif;
           Exsr WriteAction; //wtactpnl
           Write rcdLMst;    //wpilocmain
           Write rcdLocEnt;  //wpilocmain
           Exfmt rcdOwnOff;  //wpilocmain
           Exsr ReadAction; //wtactpnl
           Read rcdLocEnt;   //wpilocmain
           Read rcdLMst;     //wpilocmain
         When TabIndex = 2;
           TabHeight = 780;
           scOOTop = 95;
           If scOOTop <> prvOOTop;
             prvOOTop = scOOTop;
             Exsr SetOwnOffTops;
           Endif;
           Exsr WriteAction; //wtactpnl
           Write rcdLMst;
           Write rcdDummy11;
           Exfmt rcdOwnOff;
           Exsr ReadAction; //wtactpnl
           Read rcdLMst;
         When TabIndex = 3;
           TabHeight = 780;
           Exsr WriteAction; //wtactpnl
           Write rcdLMst;
           Exfmt rcdAddNam;
           Exsr ReadAction; //wtactpnl
           Read rcdLMst;
         When TabIndex = 4;
           //*** Beg Add *** 08/17/21 ******************************************
           conLogTop = 585;
           //*** End Add *** 08/17/21 ******************************************
           TabHeight = 780;
           Exsr WriteAction; //wtactpnl
           Write rcdLMst;
           Exfmt rcdEftInfo;
         //***  Begin Add  ***  02/16/22  *****************************
         //***  Begin Del  ***  03/17/22  *****************************
         //***Eval EFT = *OFF;
         //***  End   Del  ***  03/17/22  *****************************
         //***  END Add  ***  02/16/22  *******************************
           Exsr ReadAction; //wtactpnl
           Read rcdLMst;
         //***  Begin Add  ***  10/16/17  *******************************
         When arrTabName(tabIndex+1) = 'Collateral';
            TabHeight = 780;
            Exsr WriteAction; //wtactpnl
            Write rcdLMst;
            Exfmt rcdCollat;
            Exsr ReadAction; //wtactpnl
            Read rcdLMst;
         //***  End   Add  ***  10/16/17  *******************************
         //***  Begin Add  ***  12/17/18  ******************************
         When arrTabName(tabIndex+1) = strPortalTabNm;
            TabHeight = 780;
            Exsr WriteAction; //wtactpnl
            Write rcdLMst;
            Exfmt rcdPortal;
            Exsr ReadAction; //wtactpnl
            Read rcdLMst;
         //***  End   Add  ***  12/17/18  ******************************
         Other;
           TabHeight = 780;
           Exsr WriteAction; //wtactpnl
           Write rcdDummy11;
           Write rcdDummy22;
           Exfmt rcdLMst;
           Exsr ReadAction; //wtactpnl
         EndSl;
       EndSr;

       //===================================================================
       // Ex format screens as windows based on mode
       //===================================================================
       BegSr ExfmtWindows;
         Select;
         When flgLocWdw = *On;
            scYearTop = 640;
           ExFmt rcdLocEnt;
         //*** beg add ***** 03/05/18 ****************
         When visANmWdw = *On;
            Exfmt rcdAddNam;
         //*** end add ***** 03/05/18 ****************
         EndSl;
       EndSr;
       //***********************************************************************
       // SaveAddress
       //***********************************************************************
       BegSr SaveAddress;
         svMCity = scMCity;
         svMSt   = scMState;
         svPCity = scPCity;
         svPSt   = scPState;
         isLuba = *off;
         GetClientId(ClientID);
         If ClientID = '133';
            isLuba = *on;
         Endif;
       Endsr;
       //***********************************************************************
        //===================================================================
        // Write action panel;
        //===================================================================
        BegSr WriteAction;

        If qteMode;
           Write rcdQtePnl;
        Else;
           Write rcdPolPnl;
        Endif;

        Endsr;
        //**********************************************************************
        //===================================================================
        // Read action panel;
        //===================================================================
        BegSr ReadAction;
         If qteMode;
            Read rcdQtePnl;
         Else;
            Read rcdPolPnl;
         Endif;

         xxErrorTab = -1;

        Endsr;

       //===================================================================
       // Handle Screen actions here
       //===================================================================
       BegSr HandleScreenActions;
         Select;

          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
             btnShowWin = *off;

       //Common actions across tabs
         When tabClick = *On;
           tabClick = *Off;
           If TabIndex <> prvTabIndex;
             prvTabIndex = TabIndex;
             doLoad = *On;
           Endif;
           TabClick = *Off;
         When scSelOpt <> *blanks;
           xxNext=scSelOpt;
           ExitNow = *On;
       //Application toolbar actions
         When btnMidEnd = *On;
           btnMidEnd = *Off;
           svMode = xxMode;
           svNext = xxNext;
           svTab = TabIndex;
           xcRrn = %Editc(xxRRN:'X');
           MidTermEndorse(xcRrn:xxMode:xxNext);
           xxMode = svMode;
           xxNext = svNext;
           TabIndex = svTab;
           ProgramStart(Process);
           If Process;
             doLoad = *On;
             ExitNow = *Off;
           Else;
             ExitNow = *On;
           Endif;
         When lossClick = *On;
           isError = Not ValidBillDiv(xxMode:ptrBillDiv :xxEmrrn);
           lossClick = *off;
           If isError = *on;
               xxErrorTab = 0;
           Endif;
         When btnSave = *On;
           btnSave = *Off;
           If ValidBillDiv(xxMode:ptrBillDiv :xxEMrrn);

             intfyr = scFyr;
             If UpdateBillDiv(ptrBillDiv :xxEMrrn :xxMode :xxQTE:intfyr);
               Exsr CheckOOShare;
               PopDataSaved();
               doLoad = *On;
               flgLoadBDiv = *On;
             EndIf;
           Else;
               xxErrorTab = 0;
           EndIf;
         When btnRefresh = *On;
           btnRefresh =*Off;
           doLoad = *On;
           flgLoadBDiv = *On;
       //TabIndex = 0 actions
         When btnDptAdd = *On;
           flgLoadBDiv = *On;
           btnDptAdd = *Off;
           PrcDeptWdw(xxEmRrn:xxCall:'A');
           doLoad = *On;
       //Tabindex = 1 actions
         When btnSflLoc = *On;
           // Row click on Loc/entity grid
           btnSflLoc = *Off;
           LoadAddLocDtl(CRrnAddLoc);
           If flgLocwdw = *Off;
             LoadLocYrInf(svLocRrn);
             If ALeltp = 'E';
               isError = LoadOwnOff(svLocRrn);
             Else;
               isError = LoadOwnOff(0);
             EndIf;
           Endif;
         When btnLocAdd = *On;
           btnLocAdd = *off;
           PrcLocEntWdw(xxEmRrn:xxCall:'A');
           doLoad = *On;
         When btnLocFltr = *On;
           btnLocFltr = *Off;
           Exsr DisplayLocFilter;
           doLoad = *On;
         When btnLocCFtr = *On;
           btnLocCftr = *Off;
           Exsr ClearLocFilter;
           doLoad = *On;
         When btnLYadd = *On;
           btnLYadd = *Off;
           PrcLocYrWdw(xxEmRrn:xxCall:'A':0:svLocRrn);
           doLoad = *On;
       //Tabindex = 2 actions
         When btnOAdd = *On;
           btnOAdd = *Off;
           Select;
           When TabIndex = 1 Or flgLocWdw = *On;
             PrcOwnOffWdw(xxEmRrn:xxCall:'A':0:svLocRrn);
           Other;
             PrcOwnOffWdw(xxEmRrn:xxCall:'A');
           EndSl;
           doLoad = *On;
         // Window Mode actions;
         When btnLocCan = *On;
           btnLocCan = *Off;
           xxNext = 'CANCEL';
           ExitNow = *On;
         When btnLocNext = *On;
           btnLocNext = *Off;
           xxNext = 'NEXT';
           ExitNow = *On;
         When btnOwnCan = *On;
           btnOwnCan = *Off;
           xxNext = 'CANCEL';
           ExitNow = *On;
         When btnOwnNext = *On;
           btnOwnNext = *Off;
           xxNext = 'NEXT';
           ExitNow = *On;

         // Tabindex = 3 actions
         When btnANadd = *On;
           btnANadd = *Off;
           PrcAddNamWdw(xxEmRrn:xxCall:'A');
           doLoad = *On;
         //*** beg add ***** 03/05/18 *********
         When btnANmCan = *On;
           btnANmCan = *Off;
           xxNext = 'CANCEL';
           ExitNow = *On;
         When btnANmNxt = *On;
           btnANmNxt = *Off;
           xxNext = 'NEXT';
           ExitNow = *On;
         //*** end add ***** 03/05/18 *********
         // Tabindex = 4 actions
         When BtnAddEft = *On;
           BtnAddEft = *Off;
           EmpDivName(ELemp#:ELdiv:##emrrn:ELfnd:ELco#);
           ##eftMod = 'A';
           ##eftTyp = 'E';
           ##eftRrn = 0;
         //** Begin Change *** 05/17/21 *************************************
         //eftAdd(##eftMod:##eftTyp:ELfnd:##eftRrn:##emrrn);
           eftAdd(##eftMod:##eftTyp:ELfnd:##eftRrn:##emrrn:xxQte:kkFyr);
         //** End   Change *** 05/17/21 *************************************
           doLoad = *On;
         //***  Begin Add  ***  10/16/17  ************************************
         // Collateral actions
         When btnColAdd = *on;
            btnColAdd = *off;
            PrcCollatWdw(xxEmRrn:xxCall:'A');
            doLoad = *on;
         //***  End   Add  ***  10/16/17  ************************************
         //***  Begin Add  ***  10/22/25  ************************************
         // Display Billing Payment Plan with S/R Online soft error message
         When BtnSREft = *on;
            BtnSREft = *off;
            strSrMsg = 'The currently assigned Billing Plan is not a -
                    valid/active Self-Reporting Online plan. You will want to -
                    proceed to the Policy Year Detail to change the billing -
                    plan accordingly.';
            PopOneBtn(strSrMsg : 'Billing Payment Plan');
            doLoad = *on;
         //***  End   Add  ***  10/22/25  ************************************
         Other;

           //* Popup Windows
           Popup();

           Select;
           When scSelDiv <> scDiv or KKfyr <> scFyr;
             // Division or Year selection changed from drop down
             setDspInd(xxQte : xxCall : xxMode : scSelDiv);
             flgLoadBDiv = *on;
             scDiv = scSelDiv;
             scFyr = kkFyr;
             Chain(n) key1WDELL PEL00;
             If not %found;
                Setgt keyWDELL PEL00;
                Readpe(n) keyWDELL PEL00;
                scFyr = ELfyr;
                kkFyr = ELfyr;
             Endif;
             xxRRN = elrrn;
             xxEmRRN = GetEmpRRN(scEmp# :scDiv :scFnd :scCo#);
             doLoad = *On;
           When TabIndex = 0 And windowMode = *Off;
             //If TabIndex =0 then check action on SflDepart
             //Process only first click
             Dou %Eof;
               Readc SflDepart;
               If %Eof;
                 Leave;
               EndIf;
               Select;
               When lnkDptEdit = *On;
                 lnkDptEdit =*Off;
                 PrcDeptWdw(xxEmRrn:xxCall:'C':scsDPrrn);
                 doLoad = *On;
                 Leave;
               EndSl;
             EndDo;
           When TabIndex = 1 Or flgLocWdw = *On;
             //If TabIndex =1 then check action on SflAddLoc
             //Process only first click
             Dou %Eof;
               Readc SflAddLoc;
               If %Eof;
                 Leave;
               EndIf;
               Select;
               When lnkSEdit = *On;
                 lnkSEdit = *Off;
                 PrcLocEntWdw(xxEmRrn:xxCall:'C':scALrrn);
                 doLoad = *On;
                 svRrnAddLoc = rrnAddLoc;
                 LeaveSr;
                 When lnkSExpire = *On;
                   lnkSExpire = *Off;
                 If scsExpDt > ZeroDate;
                   PopOneBtn('This Location/Entity is already expired!'
                    :'Expire');
                 Else;
                   If qteMode  <> *on;
                     PrcLocEntWdw(xxEmRrn:xxCall:'D':scALrrn);
                   Else;
                     PrcLocEntWdw(xxEmRrn:xxCall:'D':scALrrn:flgNBquote);
                   Endif;
                   doLoad = *On;
                 Endif;
                 svRrnAddLoc = rrnAddLoc;
                 LeaveSr;
               EndSl;
             EndDo;
             //Still here so no action on first subfile check second
             //Process only first click
             Dou %Eof;
               Readc SflLocYr;
               If %Eof;
                 Leave;
               EndIf;
               Select;
               When lnkLEdit = *On;
                 lnkLEdit = *Off;
                 PrcLocYrWdw(xxEmRrn:xxCall:'C':scLrrn:svLocRrn);
                 doLoad = *On;
                 LeaveSr;
               When lnkLDelete = *On;
                 lnkLDelete = *Off;
                 If GetUsrLvl(Q1user) < EXdltl;
                   PopOneBtn('Insufficient authority for delete action'
                    :'Delete');
                 Else;
                   PrcLocYrWdw(xxEmRrn:xxCall:'D':scLrrn:svLocRrn);
                   doLoad = *On;
                   LeaveSr;
                 EndIf;
               EndSl;
             EndDo;
             //Still here so no action on Second subfile check Third
             Exsr HandleOwnOffAction;
           When TabIndex = 2;
             Exsr HandleOwnOffAction;

           // check Tab 3 subfile
           //*** beg chg ***** 03/05/18 *******************
           //*** When TabIndex = 3;
           When TabIndex = 3 or visAnmWdw = *On;
           //*** end chg ***** 03/05/18 *******************
             Dou %Eof;
               Readc SflAddNam;
               If %Eof;
                 Leave;
               EndIf;
               Select;
               When lnkANedit = *On;
                 lnkANedit = *Off;
                 PrcAddNamWdw(xxEmRrn:xxCall:'C':scANRrn);
                 doLoad = *On;
                 LeaveSr;
               EndSl;
             EndDo;
           // check Tab 4 subfile
           When TabIndex = 4;
              // read subfile change record
              Readc SflEftInfo;
              Dow not %eof;
                 Select;
                 When scEdtEftLc = *On;
                    scEdtEftLc =*Off;
                    ##eftMod = 'E';
                    ##eftTyp = 'E';
                    ##emRrn  = 0;
                    ##eftRrn = scLocRrn;
                    //*** Begin Change *** 05/17/21 ****************************
                    //eftAdd(##eftMod:##eftTyp:ELfnd:##eftRrn:##emrrn);
                    eftAdd(##eftMod:##eftTyp:ELfnd:##eftRrn:##emrrn:xxQte:
                     kkFyr);
                    //*** End Change *** 05/17/21 *****************************
                    doLoad = *On;
                 When scEftBnkLc = *On;
                    scEftBnkLc =*Off;
                    ##eftRrn = scLocRrn;
                    EFTBANKINFO(##eftRrn);
                    doLoad = *On;
                 When scRqPNtLc = *On;
                    scRqPNtLc =*Off;
                    ##eftRrn = scLocRrn;
                    If scLocPNtDt = ZeroDate;
                       ReqPreNote(##eftRrn);
                    Else;
                       ClrPreNote(##eftRrn);
                    Endif;
                    doLoad = *On;
                 EndSl;
                 Readc SflEftInfo;
              Enddo;
        //*** Begin Add *** 05/17/21 *******************************************
        // Read 3rd Party Pay subfile changed record
        Readc Sfl3rdPay;
        If not %eof;
           //***If lnk3pLog = *On;
           //***   clr3pLog = *On;
           //***   dsp3pLog = *Off;
           //***   Write rcd3pLog;
           //***   clr3pLog = *Off;
           //***   dsp3pLog = *On;
           //***   Exsr  Prepare3pLogSql;
           //***   Exsr LoadSfl3pLog;                              //Load 3rd Pa
           //***   Exfmt rcd3pLog;                                 //Display 3rd
           //***Endif;
        Endif;
        //*** End   Add *** 05/17/21 *******************************************
           //***  Begin Add  ***  10/16/17  *******************************
           When arrTabName(6)          = 'Collateral';
              Dou %eof;
                 Readc SflCollat;
                 Select;
                 When %eof;
                    Iter;
                 When lnkColEdit = *on;
                    lnkColEdit = *off;
                    PrcCollatWdw(xxEmRrn:xxCall:'C':scColRrn);
                    doLoad = *On;
                    LeaveSr;
                 EndSl;
             EndDo;
           //***  End   Add  ***  10/16/17  *******************************
           //***  Begin Add  ***  12/17/18  ******************************
           When arrTabName(TabIndex-1) = strPortalTabNm;
              Dou %eof;
                 Readc sflLosses;
                 Select;
                 When %eof;
                    Iter;
                 Endsl;
              Enddo;
              Dou %eof;
                 Readc sflQuest;
                 Select;
                 When %eof;
                    Iter;
                 Endsl;
              Enddo;
           //***  End   Add  ***  12/17/18  ******************************

           Other;
             isError = Not ValidBillDiv(xxMode:ptrBillDiv :xxEmrrn);
             If isError = *on;
               xxErrorTab = 0;
             Endif;
           EndSl;
         EndSl;

       //* If an error has been found then set tab index to it
       If xxErrorTab >-1;
         TabIndex = xxErrorTab;
       Endif;
       EndSr;
       //===================================================================
       // Handle Owner officer subfile errors
       //===================================================================
       BegSr HandleOwnOffAction ;
         //Process only first click
         Dou %Eof;
           Readc SflOwnOff;
           If %Eof;
             Leave;
           EndIf;
           xxMode = ' ';
           Select;
           When lnkOEdit = *On;
             lnkOEdit = *Off;
             xxMode = 'C';
           When lnkOExpire = *On;
             lnkOExpire = *Off;
             xxMode = 'D';
           When lnkOIE = *On;
             lnkOIE = *Off;
             xxMode = 'I';
           EndSl;
           If xxMode <> ' ';
             If TabIndex = 1 or flgLocWdw = *On;
               PrcOwnOffWdw(xxEmRrn:xxCall:xxMode:scOOrrn:svLocRrn);
             Else;
               PrcOwnOffWdw(xxEmRrn:xxCall:xxMode:scOOrrn);
             EndIf;
             doLoad = *On;
             Leave;
           Endif;
         EndDo;
       EndSr;
       //===================================================================
       // CheckOOShare - Check Owner/officer percentage
       //===================================================================
       BegSr CheckOOShare;
         outErr = ' ';
         inELFL = ' ';
         OwnOffShare(xxEMrrn:inELFL:outErr);
           If outErr <> '0' And xxErrorTab = -1;
               xxErrorTab = 2;
           Endif;

         Setll (SCco#:SCfnd:SCemp#:SCdiv) pal00;
         Reade(N) (SCco#:SCfnd:SCemp#:SCdiv) pal00;
         Dow not %eof(wmall);
            If ALELTP = 'E';
              inELFL = 'E';
              xxALrrn= ALrrn;
              If outErr = '0';
                 outErr = *Blanks;
              Endif;
              OwnOffShare(xxALrrn:inELFL:outErr);
               If outErr <> '0' And xxErrorTab = -1;
                   xxErrorTab = 2;
               Endif;
            Endif;
         Reade(N) (SCco#:SCfnd:SCemp#:SCdiv) pal00;
         Enddo;

       Endsr;
       //===================================================================
       // Display Location/Entity filter screen
       //===================================================================
       BegSr DisplayLocFilter;
         DoU BtnEnLAppf = *On or BtnEnLCan = *On;
            ExFmt RcdLocFlt;
            If BtnEnLAppf = *On;
               dsFilter.ssEnLcNm = ssEnLcNm;
               dsFilter.ssPhAd1 = ssPhAd1;
               dsFilter.ssElTp  = ssElTp;
               dsFilter.ssEnLcFein = ssEnLcFein;
               dsFilter.ssEnLcEfDt = ssEnLcEfDt;
               dsFilter.ssEnLcStat = ssEnLcStat;
            Endif;
            If BtnEnLCan = *On;
               ssEnLcNm = dsFilter.ssEnLcNm;
               ssPhAd1 = dsFilter.ssPhAd1;
               ssElTp = dsFilter.ssElTp;
               ssEnLcFein = dsFilter.ssEnLcFein;
               ssEnLcEfDt = dsFilter.ssEnLcEfDt;
               ssEnLcStat = dsFilter.ssEnLcStat;
            Endif;

            If DtCYMD(dsFilter.ssEnLcEfDt) = *zero and
             dsFilter.ssEnLcNm = *blanks and
             dsFilter.ssPhAd1 = *blanks and
             dsFilter.ssElTp  = *blanks and
             dsFilter.ssEnLcFein = *zeros and
             dsFilter.ssEnLcStat = *blanks;
               locFilter = *off;
            Else;
               locFilter = *on;
            Endif;
          EndDo;
       EndSr;
       //===================================================================
       // Clear Location/Entity filter screen
       //===================================================================
       BegSr ClearLocFilter;
         Clear dsFilter;
         Clear RcdLocFlt;
         locFilter = *off;
       EndSr;
       //*** Begin Add *** 05/17/21 ********************************************
       //-----------------------------------------------------------------------
       // LoadSfl - Load Subfile for EFT information
       //-----------------------------------------------------------------------
        //*** Beg Del *** 08/17/21 *********************************************
        //*** BegSr LoadSfl3pLog;
        //***   If SQLCOD >= 0 and SQLCOD <> 100;
        //***      Exec SQL
        //***         Fetch Next from C1 into :WMP3Parc;
        //***      Dow SQLCOD >= 0 and SQLCOD <> 100;
        //***         scTrans  = WMP3Parc.P3pgm;
        //***         If WMP3Parc.P3dt > *zero;
        //***            scDt = %Date(WMP3Parc.P3dt:*cymd);
        //***         Else;
        //***            scDt = ZeroDate;
        //***         Endif;
        //***         scEnvFlg = WMP3Parc.P3envflg;
        //***         scPrType = WMP3Parc.P3prtype;
        //***         scEnBld  = WMP3Parc.P3enbld;
        //***         scSfyr   = WMP3Parc.P3sfyr;
        //***         If WMP3Parc.P3efdt > *zero;
        //***            scEfDt = %Date(WMP3Parc.P3efdt:*cymd);
        //***         Else;
        //***            scEfdt = ZeroDate;
        //***         Endif;
        //***         If WMP3Parc.P3exdt > *zero;
        //***            scExDt = %Date(WMP3Parc.P3exdt:*cymd);
        //***         Else;
        //***            scExdt = ZeroDate;
        //***         Endif;
        //***         scDftSrD = WMP3Parc.P3dftsrd;
        //***         rrn3pLog = rrn3pLog + 1;
        //***         write sfl3pLog;
        //***         Exec SQL
        //***            Fetch Next from C1 into :WMP3Parc;
        //***      Enddo;
        //***      Exec Sql
        //***         Close C1;
        //***      rrn3plog = 1;
        //***   Endif;

        //***Endsr;
       //-----------------------------------------------------------------------
       // Prepare3pLogsql - Prepare 3rd Party Info Log Sql
       //-----------------------------------------------------------------------
        //*** Beg Del *** 08/17/21 *********************************************
        //***BegSr Prepare3pLogSql;
        //***   GetObjLib('WMP3P':'*FILE':strFileLib);
        //***   strAlias = 'CREATE ALIAS ' + %trim(strFileLib) + '/' + 'WMP3P_AR
        //***    ' FOR ' + %trim(strFileLib) + '/' + 'WMP3P(ARCHIVE)';

        //***   Exec sql
        //***      Execute immediate :strAlias;

        //***   Exec Sql
        //***      Declare C1 cursor for
        //***       Select * from WMP3P_ARC where P3co# = :ELco# and
        //***        P3fnd = :ELfnd and P3emp# = :ELemp# and P3div = :ELdiv;

        //***   Exec Sql
        //***      Open C1;
        //***Endsr;
       //*** End   Add *** 05/17/21 ********************************************
       //===================================================================
       // Set Top corner for Owner/Officer screen
       //===================================================================
       BegSr SetOwnOffTops;
         scOOPnlTop = scOOTop - 25;
         //*** Beg Chg ***** 11/03/22 *******************
         //***scOOLblTop = scOOTop + 10;
         //***scOOSflTop = scOOTop + 40;
         scOOLblTop = scOOTop + 15;
         scOOSflTop = scOOTop + 45;
         //*** End Chg ***** 11/03/22 *******************
         scOOAddTop = scOOTop + 175;
         scOO01Top = scOOTop + 200;
         scOO02Top = scOOTop + 215;
         scOO03Top = scOOTop + 230;
         scOO04Top = scOOTop + 245;
         scOO05Top = scOOTop + 260;
         scOO06Top = scOOTop + 275;
         scOO07Top = scOOTop + 295;
         scOO08Top = scOOTop + 310;
         scOO09Top = scOOTop + 325;
         scOO10Top = scOOTop + 340;
         scOO11Top = scOOTop + 360;
         scOO12Top = scOOTop + 375;
         scOO13Top = scOOTop + 395;
         scOO14Top = scOOTop + 410;
         scOO15Top = scOOTop + 430;
        //scOO16Top = scOOTop + 445;
       EndSr;
      /end-free
     P Main            E


       //===================================================================
       // Load Location Master data
       //===================================================================
     P LoadDetail      B
     D LoadDetail      PI
      /free
       Select;
         When TabIndex = 0 And windowMode = *Off and flgLoadBDiv = *On;
         isError = LoadBillDiv(xxEMrrn :ptrBillDiv :xxCall:xxMode);
         isError = LoadDepart();
         flgLoadBDiv = *Off;
       When TabIndex = 1 Or flgLocWdw = *On;
         isError = LoadLocEnt();
       When TabIndex = 2;
         isError = LoadOwnOff();
       When TabIndex = 3 or visANmWdw = *On;
         isError = LoadAddNam();
       When TabIndex = 4;
         // EFT Information tab
         LoadEftInfo();
       //***  Begin Add  ***  10/16/17  ************************************
       When arrTabName(tabIndex+1) = 'Collateral';
         isError = LoadCollateral();
         isError = LoadCollatAR();
       //***  End   Add  ***  10/16/17  ************************************
       //***  Begin Add  ***  12/17/18  ******************************
       When arrTabName(tabIndex+1) = strPortalTabNm;
          isError = LoadPortalInfo();
       //***  End   Add  ***  12/17/18  ******************************
       EndSl;
       LoadSelection();
       If flgLoadBDiv = *On;
         isError = LoadBillDiv(xxEMrrn :ptrBillDiv :xxCall:xxMode);
         isError = LoadDepart();
         flgLoadBDiv = *Off;
       Endif;

      /end-free
     P LoadDetail      E

       //===================================================================
       // Process EFT information data
       //===================================================================
     P LoadEftInfo     B
     D LoadEftInfo     PI

       //***  Begin Add  ***  03/11/21  *********************
     Dis3rdPartyPay    s              1n   inz

     DdsWIPSP        e ds                  extname(WIPSP) inz qualified
     DdsWIPSPHIST    e ds                  extname(WIPSPHIST) inz qualified
       //***  End   Add  ***  03/11/21  *********************
       //*** Begin Add *** 03/30/21 ************************
     D strCo#          s              3A   inz
     D strFnd          s              3A   inz
       //*** End   Add *** 03/30/21 ************************
       //*** Begin Add *** 05/17/21 ************************
       Dcl-S strEftType Char(1) inz('E');
       Dcl-S strEftKey  Char(30);
       Dcl-S strEfDt Char(7);
       Dcl-S strPre Char(1);
       Dcl-S strRRN Char(9);
       Dcl-S strEobType Char(3);
       Dcl-S strCkTp Char(10);
       Dcl-S strSfyr char(3);
       Dcl-S strEnv char(1);
       Dcl-S strDftSrd char(1);
       Dcl-S intStRptDiv like(EFdiv) inz;
       Dcl-S strSrFlg char(1);
       Dcl-S intEMrrn packed(9:0) inz;
       Dcl-S isEft ind inz(*Off);
       Dcl-S isDftSrD3p ind inz(*Off);
       Dcl-S strDftSrD3p char(1) inz('X');
       //*** End   Add *** 05/17/21 ************************
       //*** beg add ***** 09/21/21 ***********************
       Dcl-S hasQteSrOnRcd Ind;
       Dcl-S hasLivSrOnRcd Ind;
       //*** end add ***** 09/21/21 ***********************
       //*** Beg Add *** 04/24/25 ***************************************
       Dcl-S intCmdLen packed(15:5) inz(36);
       Dcl-S CmdOvrDbf char(36);
       Dcl-S CmdDltOvr char(36);
       //*** End Add *** 04/24/25 ***************************************
      /free
       //***  Begin Add  ***  03/11/21  *********************
       Select;
       When IsModEnabled('Invoice Cloud') and ICGroupOpt(ELCo#:ELFnd);
          is3rdPartyPay = *on;
       Endsl;
       //***  End   Add  ***  03/11/21  *********************
       xsKey = 'WC';
       Chain xsKey SWC01;
       If WCfl24 <> 'Y' And WCfl38 <> 'Y';
          NOEFT = *On;
          DspAddEft = *Off;
          EFT = *Off;
       Endif;

       If WCfl24 = 'Y' Or WCfl38 = 'Y';
          NOEFT = *off;
          EFT =  *On;
       Endif;
       //*** begin add ***  06/26/19  *****************************************
       If mod3rdPay = *on and noEFT = *on;
          noEFT = *off;
       Endif;
       //*** end add *****  06/26/19  *****************************************

        Exsr ClearEftSub;
        Exsr LoadEftInfo;
       //***********************************************************************
       // ClearEftSub - Clear Subfile for EFT information
       //***********************************************************************
       BegSr ClearEftSub;

          rrnEFTInfo = 0;
          clrEFTInfo = *On;
          dspEFTInfo = *Off;
          //*** begin add ***  06/26/19  ***************************************
          rrn3rdPay = 0;
          clr3rdPay = *On;
          dsp3rdPay = *Off;
          //*** end add *****  06/26/19  ***************************************
          Write rcdEftInfo;
          clrEFTInfo = *Off;
          dspEFTInfo = *On;
          //*** begin add ***  06/26/19  ***************************************
          clr3rdPay = *Off;
          dsp3rdPay = *On;
          //*** end add *****  06/26/19  ***************************************

       EndSr;
       //***********************************************************************
       // LoadEftInfo - Load Subfile for EFT information
       //***********************************************************************
       BegSr LoadEftInfo;

          Setll keyWMEFL6 pef06;
          Reade keyWMEFL6 pef06;
          Dow not %eof(WMEFL6);
             //*** Begin Add *** 03/30/21 ************************************
             If (qteMode = *On) or
                (qteMode <> *On and EFenvflg = *blank);
             //*** End   Add *** 03/30/21 ************************************
                scLocDiv#  = EFdiv;
                scLocEffDt = CvtToDate(EFefdt);
                scLocExpDt = CvtToDate(EFexdt);
                scLocPNtDt = CvtToDate(EFpdt);
                scLocAppDt = CvtToDate(EFadt);
                scLocFTrDt = CvtToDate(EFftdt);
                scLocLTrDt = CvtToDate(EFltdt);
                //*** Begin Add *** 03/30/21 ****** 05/17/21 *******************
                scSfyr   = EFsfyr;
                scEnvFlg = EFenvflg;
                scLocDfInf = EFdftSrd;
                strType = 'E';
                visNoteLnk = RequiresPreNote(ELco#:ELfnd:strType:sysDate7);
                If qteMode = *On;
                   Select;
                   When EFenvFlg = *blank;   //Don't allow edit of Live recds if
                      dspEftLnk = *Off;
                      visNoteLnk = *Off;
                   When EFenvFlg = 'Q' and isEftLnk = *On;
                      dspEftLnk = *On;
                   Endsl;
                Endif;
                //*** End   Add ******* 05/17/21 *********************
                If EFpdt = 0;
                   scNoteLnk = 'Req Pre-note';
                Else;
                   scNoteLnk = 'Clr Pre-note';
                Endif;

                scLocPName = EFpnam;
                rrnEFTInfo = rrnEFTInfo + 1;
                If rrnEFTInfo = 1;
                   scSelRow = 'A';
                Else;
                   Clear scSelRow;
                Endif;
                scLocRrn = efrrn;

                Write SflEftInfo;
             //*** Begin Add *** 03/30/21 ************************************
             Endif;
             //*** End   Add *** 03/30/21 ************************************
             Reade keyWMEFL6 pef06;
          Enddo;
          rrnEFTInfo = 1;

          //*** Begin Add *** 05/17/21 ***************************************
          intEMrrn = *Zero;
          strSrFlg = *blank;
          intStRptDiv = GetStRptDv(ELco#:ELfnd:ELemp#:intEMrrn:strSrFlg);

          If ELdiv <> intStRptDiv or                      //NSRD
            (ELdiv = intStRptDiv and strSrFlg = *Blank);
             strco#    = %editc(ELco#:'X');
             strEftType = 'E';
             strFnd     = %editc(ELfnd:'X');
             strEftkey  = %Editc(ELemp#:'X') + %Editc(ELdiv:'X');
             sysDate7 = DateTo7(SystemDate);
             strEfDt    = %editc(sysDate7:'X');
             strPre     = *blanks;
             strRrn     = *zeros;
             strEobType = *blanks;
             strCkTp  = *blanks;
             strSfyr    = %Editc(ELfyr:'X');
             strEnv     = xxQte;
             strDftSrD  = *blanks;

             isEft = GetEftStatus(strCo#:strEftType:strFnd:strEftKey:strEfDt:
              strPre:strRrn:strEobType:strCkTp:strSFyr:strEnv:strDftSrD);

             If isEft and strDftSrD = 'Y';
                visDftMsg = *On;
             Else;
                visDftMsg = *Off;
             Endif;
          Else;                                                  //SRD
             visDftMsg = *Off;
          Endif;

          isDftSrD3p = *Off;
          strDftSrD3p = 'X';
          sysDate7 = DateTo7(systemDate);
          //*** End  Add *** 05/17/21 ***************************************

       //*** begin add ***  06/26/19  *****************************************
       //*** beg chg *****  09/22/21  *****************************************
       //
       //   Clear sfl3rdPay;
       //   Setll ELCo# pp300t;
       //   Reade ELCo# pp300t;
       //   Dow not %eof(wtp3l);
       //         //***  Begin Add  ***  03/11/21  *********************
       //         Reset dsWIPSP;
       //         Reset dsWIPSPHIST;
       //         dis3rd = dspMod;
       //         vis3rd = *on;
       //         Select;
       //         When P3PrType = 'Third Party Payments';
       //            Select;
       //            When not is3rdPartyPay;
       //               P3stat = *blank;
       //            Endsl;
       //         Endsl;
       //         //***  End   Add  ***  03/11/21  *********************
       //         If P3Stat = 'A';
       //            scPayCo# = %editc(ELCO#:'X');
       //            scPayEMRRN = xxEMRRN;
       //          //***  Begin Chg  ***  07/29/21  ******************
       //          //scPayRRN = '000000000';
       //            scPayRRN = 'No RRN';
       //          //***  End   Chg  ***  07/29/21  ******************
       //            scP3PrType = P3PrType;
       //            scP3PrTypT = P3PrType;
       //          //***  Begin Del  ***  03/11/21  *********************
       //          //If P3PrType = 'S/R Online';
       //          //   scP3PrType = %trim(P3PrType) + ' ' +
       //          //    %trim(GetModuleProperty('Portal Module':'Name'));
       //          //Endif;
       //          //***  End   Del  ***  03/11/21  *********************
       //
       //          //*** Begin Change *** 05/17/21 *****************************
       //            //Chain(n) (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
       //            //If not %found;
       //            //   Clear pp300;
       //            //** beg add ***** 04/29/20 ******************
       //            //   Clear P3rrn;
       //            //   Clear scPayRRN;
       //            //** end add ***** 04/29/20 ******************
       //            //Endif;
       //            //scP3Enbld = P3Enbld;
       //            //scP3Efdt  = dtMdyy(P3Efdt);
       //            //scP3Exdt  = dtMdyy(P3Exdt);
       //            //scPayRRN = %editc(p3RRN:'X');
       //            setll (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
       //            If %equal(wmp3l);
       //               reade(n) (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
       //               Dow not %eof(wmp3l);
       //                  If (xxQte  = 'Q')    or
       //                     (xxQte <> 'Q' and P3envflg = *blank);
       //                     dis3rd = dspMod;
       //                     scP3Enbld = P3Enbld;
       //                     scP3Efdt  = dtMdyy(P3Efdt);
       //                     scP3Exdt  = dtMdyy(P3Exdt);
       //                     scPayRRN = %editc(p3RRN:'X');
       //                     scEnvFlgTp = P3envflg;   //Env
       //                     scDftInf   = P3dftSrD;   //Default State Rpt Div
       //                     scSfyrTp   = P3sfyr;     //Starting Year
       //                     If xxQte = 'Q';
       //                        scDftSfyr  = ELFyr;      //Default starting yea
       //                     Else;
       //                        scDftSfyr  = *Zeros;
       //                     Endif;
       //                     Exsr Write3rdPsfl;
       //                     If ELdiv <> stateRepDiv and
       //                        (isDftSrD3p = *Off or strDftSrD3p = 'X');
       //                        isDftSrD3p = Has3rdPartyAcct(ELco#:ELfnd:
       //                         ELemp#:ELdiv:P3prtype:sysDate7:
       //                          ELfyr:flgQte:strDftSrd3p);
       //                     Endif;
       //                  Endif;
       //                  reade(n) (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
       //               Enddo;
       //            Else;
       //               Clear pp300;
       //               Clear P3rrn;
       //             //***  Begin Chg  ***  07/29/21  ******************
       //             //Clear scPayRRN;
       //               scPayRRN = 'No RRN';
       //             //***  End   Chg  ***  07/29/21  ******************
       //               mod3rdPay = IsModEnabled('Third Party Payment');
       //               If scSfyrTp = *Zeros and xxQte = 'Q';
       //                  scEnvFlgTp = 'Q';
       //                  scSfyrTp = ELfyr;
       //                  scDftSfyr  = ELfyr;      //Default starting year o
       //               Endif;
       //               Exsr Write3rdPsfl;
       //               If ELdiv <> stateRepDiv;
       //                  isDftSrD3p = Has3rdPartyAcct(ELco#:ELfnd:ELemp#:ELdiv
       //                   P3prtype:sysDate7:ELfyr:flgQte:strDftSrd3p);
       //               Endif;
       //            Endif;
       //
       //
       //            //***  Begin Add  ***  03/11/21  *********************
       //            //Clear scAccount#;
       //            //Clear scPaper;
       //            //Select;
       //            //When scP3PrType = 'S/R Online';
       //            //   scP3PrType = %trim(GetModuleProperty('S/R Online':'Nam
       //            //    + ' ' + %trim(GetModuleProperty('Portal Module':'Name
       //            //When scP3PrType = 'Third Party Payments';
       //            //   Exec sql
       //            //    select * into :dsWIPSP
       //            //     from WIPSP
       //            //     where PSco# = :ELco# and PSfnd = :ELfnd
       //            //      and PSemp# = :ELemp#;
       //            //   Exec sql
       //            //    select * into :dsWIPSPHIST
       //            //     from WIPSPHIST
       //            //     where PSco# = :ELco# and PSfnd = :ELfnd
       //            //     and PSemp# = :ELemp#
       //            //      and PSfield = 'PSOPTFLAG'
       //            //     order by PStrDt desc, PStrTm desc
       //            //     limit 1;
       //            //   dis3rd = *on;
       //            //   vis3rd = *off;
       //            //   Clear scP3enbld;
       //            //   Clear scP3efDt;
       //            //   Select;
       //            //   When dsWIPSP.PSoptFlag = 'I';
       //            //      scP3enbld = 'Y';
       //            //      scP3efDt  = dtMDYY(dsWIPSPHIST.PStrDt);
       //            //      scOutput = dtMDYY(dsWIPSPHIST.PStrDt);
       //            //   Endsl;
       //            //    scAccount# = %char(ELfnd)+%char(ELemp#)+%char(ELdiv);
       //            //   scPaper = dtMDYY(dsWIPSP.PSpaper);
       //            //   sqlAction = 'select PSaction, PStrDtDsp ' +
       //            //    'from WIPSPHIST '+
       //            //    'where PSco#='+%char(ELco#) +
       //            //    ' and PSfnd='+%char(ELfnd) +
       //            //    ' and PSemp#='+%char(ELemp#) +
       //            //    ' and PSdiv='+%char(ELdiv) +
       //            //    ' and PSfield = ''PSOPTFLG''' +
       //            //    ' order by PStrDt desc, PRtrTm desc';
       //            //Endsl;
       //            //***  End   Add  ***  03/11/21  *********************
       //            //rrn3rdPay = rrn3rdPay + 1;
       //
       //            //Write Sfl3rdPay;
       //            //*** End  Change  *** 05/17/21 ***************************
       //         Endif;
       //      Reade ELCo# pp300t;
       //   Enddo;
       //   rrn3rdPay = 1;
       //   //*** end add *****  06/26/19  *************************************
       //   //*** Begin Add *** 05/17/21 ***************************************
       //   If ELdiv <> stateRepdiv and isDftSrD3p = *On and strDftSrD3p = 'Y';
       //      visDftMsg3 = *On;
       //   Else;
       //      visDftMsg3 = *Off;
       //   Endif;
       //   //*** End   Add *** 05/17/21 ***************************************

          Clear Sfl3rdPay;
          Setll ELCo# pp300t;
          Dou %Eof(WTP3L);
          Reade ELCo# pp300t;
             If %Eof(WTP3L);
                Leave;
             Endif;
             //*** Begin Add *** 09/30/21 **************************************
             // Prevent the 3rd Party subfile from being loaded
             // if the 3rd Party module is not enabled.
             If mod3rdPay = *off;
                Leave;
             Endif;
             //*** End   Add *** 09/30/21 **************************************
             Clear scP3Enbld;
             Clear scSFyrTp;
             Clear scEnvFlgTp;
             Clear scP3EfDt;
             Clear scP3ExDt;
             Clear scDftInf;
             Clear P3rrn;
             scPayCo# = %Editc(ELCO#:'X');
             scPayEMRRN = xxEMRRN;
             scPayRRN = 'No RRN';
             scP3PrType = P3PrType;
             scP3PrTypT = P3PrType;
             scPayRRN = 'No RRN';
             Select;
             When P3PrType = 'S/R Online';
                scP3PrType = %trim(GetModuleProperty('S/R Online':'Name'))
                 + ' ' + %trim(GetModuleProperty('Portal Module':'Name'));
                visForSrOn = *On;
                isDftSrD3p = *Off;
                strDftSrD3p = 'X';
                sysDate7 = DateTo7(systemDate);
               // Find out if this division is defaulting S/R Online
               //  from the state reporting division.
                If ELdiv <> stateRepDiv;
                   scDftSfyr = ELfyr;
                   isDftSrD3p = Has3rdPartyAcct(ELco#:ELfnd:ELemp#:
                    ELdiv:P3prtype:sysDate7:ELfyr:flgQte:strDftSrD3p);
                Endif;
                If ELdiv <> stateRepdiv And isDftSrD3p = *On
                 And strDftSrD3p = 'Y';
                   visDftMsg3 = *On;
                   //*** Beg Chg *** 08/04/25 ******************************
                   //***strSRDMsg =
                   //*** %Trim(scP3PrType) +
                   //*** ' is defaulted from the state reporting division.';
                   Chain (ELco#:%editc(ELbsCd:'X')) pbs01;

                   If %Found(WTABL) and BSSEFL = 'S' and BSONLI = 'Y';
                      strDftSrD3 =
                       %Trim(scP3PrType) +
                       ' is defaulted from the state reporting division.';
                   Else;
                      strDftSrD3 = 'This Billing Division is not currently +
                       set for On-line Billing';
                   Endif;
                   //*** End Chg *** 08/04/25 ******************************
                Else;
                   //*** Beg Chg *** 08/04/25 ******************************
                   //***Clear strSRDMsg;
                   Clear strDftSrD3;
                   //*** End Chg *** 08/04/25 ******************************
                   visDftMsg3 = *Off;
                Endif;
                //*** Beg Add *** 04/24/25 *********************************
                //Override File to WMP3P member
                If not %Open(WMP3L);
                   CmdOvrDbf = %Trim('OVRDBF FILE(WMP3L) MBR(WMP3L)');
                   RunCommand(CmdOvrDbf:intCmdLen);
                   Open WMP3L;
                Endif;
                If not %Open(WMP3P);
                   CmdOvrDbf = %Trim('OVRDBF FILE(WMP3P) MBR(WMP3P)');
                   RunCommand(CmdOvrDbf:intCmdLen);
                   Open WMP3P;
                Endif;
                //*** End Add *** 04/24/25 *********************************
                Setll (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
                Dou %Eof(WMP3L);
                   Reade (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
                   If %Eof(WMP3L);
                      Leave;
                   Endif;
                  // Don't display quote records in live.
                   If qteMode = *Off And P3EnvFlg = 'Q';
                      Iter;
                   Endif;
                  // Initialize fields and display indicators.
                   Clear scP3Enbld;
                   Clear scSFyrTp;
                   Clear scEnvFlgTp;
                   Clear scP3EfDt;
                   Clear scP3ExDt;
                   Clear scDftInf;
                   disP3Enbl = *Off;
                   dis3rdSfyr = *Off;
                   disP3Dates = *Off;
                   disP3Dflt = *Off;
                  // Might have to populate grid with blank values if
                  // record doesn't exist in environment.
                   If P3EnvFlg = 'Q';
                      hasQteSrOnRcd = *On;
                   Else;
                      hasLivSrOnRcd = *On;
                   Endif;
                  // Populate grid row
                   Select;
                   When dspMod = *On And isRenewalQte = *Off;
                      disP3Enbl = *On;
                      dis3rdSfyr = *On;
                      disP3Dates = *On;
                      disP3Dflt = *On;
                   When qteMode = *On and P3EnvFlg = *Blank;
                      disP3Enbl = *On;
                      dis3rdSfyr = *On;
                      disP3Dates = *On;
                      disP3Dflt = *On;
                   When qteMode = *On And P3EnvFlg = 'Q';
                      dis3rdSfyr = *On;
                   EndSl;
                   scP3Enbld = P3Enbld;
                   scP3Efdt  = DtMdyy(P3Efdt);
                   scP3Exdt  = DtMdyy(P3Exdt);
                   scPayRRN  = %Editc(p3RRN:'X');
                   scEnvFlgTp = P3envflg;
                   scDftInf   = P3dftSrD;
                   scSfyrTp   = P3sfyr;
                   Exsr Write3rdPsfl;
                Enddo;
                //*** Beg Add *** 04/24/25 ************************************
                If %Open(WMP3L);
                   Close WMP3L;
                   CmdDltOvr = %Trim('DLTOVR FILE(WMP3L)');
                   RunCommand(CmdDltOvr:intCmdLen);
                Endif;
                If %Open(WMP3P);
                   Close WMP3P;
                   CmdDltOvr = %Trim('DLTOVR FILE(WMP3P)');
                   RunCommand(CmdDltOvr:intCmdLen);
                Endif;
                //*** End Add *** 04/24/25 ************************************
               // Shut down entry in display mode
               // in case no WMP3P records exist.
                If dspMod = *On;
                   disP3Enbl = *On;
                   dis3rdSfyr = *On;
                   disP3Dates = *On;
                   disP3Dflt = *On;
                Else;
                   disP3Enbl = *Off;
                   dis3rdSfyr = *Off;
                   disP3Dates = *Off;
                   disP3Dflt = *Off;
                Endif;
               // Write blank record to grid row if
               // record doesn't exist in environment.
               // Enable writes record in WTPOLLMS06.
                If qteMode = *On And hasQteSrOnRcd =  *Off;
                   Clear scP3Enbld;
                   Clear scSFyrTp;
                   Clear scEnvFlgTp;
                   Clear scP3EfDt;
                   Clear scP3ExDt;
                   Clear scDftInf;
                   Clear P3rrn;
                   scPayRRN = 'No RRN';
                   dis3rdSfyr = *On;
                   scEnvFlgTp = 'Q';
                   scSfyrTp = ELFyr;
                   Exsr Write3rdPsfl;
                Endif;
                If qteMode = *Off And hasLivSrOnRcd =  *Off;
                   Clear scP3Enbld;
                   Clear scSFyrTp;
                   Clear scEnvFlgTp;
                   Clear scP3EfDt;
                   Clear scP3ExDt;
                   Clear scDftInf;
                   Clear P3rrn;
                   scPayRRN = 'No RRN';
                   scSfyrTp = ELFyr;
                   Exsr Write3rdPsfl;
                Endif;
             When P3PrType = 'Third Party Payments';
               // Just shows if policy is opted in or not.
               // Detail box shows details. Third Party
               // Info is in WIPSP*, not WMP3P.
                Clear scP3Enbld;
                Clear scSFyrTp;
                Clear scEnvFlgTp;
                Clear scP3EfDt;
                Clear scP3ExDt;
                Clear scDftInf;
                Clear P3rrn;
                If is3rdPartyPay = *Off;
                   Iter;
                Endif;
                disP3Enbl = *On;
                visForSrOn = *Off;
                Exsr Write3rdPsfl;
             EndSl;
          Enddo;
         // Set cursor to first row
          rrn3rdPay = 1;
       //*** end chg *****  09/22/21  *****************************************
       EndSr;
      //***** Begin Add *** 05/17/21 *****************************************
      //*---------------------------------------------------------------------
      //* Write3rdPsfl - Write Rec to 3rd Party Sfl
      //*---------------------------------------------------------------------
       Begsr Write3rdPsfl;
          Clear scAccount#;
          Clear scPaper;
          //*** Beg Add *** 08/17/21 *******************************************
          Clear scAutoPay;
          //*** End Add *** 08/17/21 *******************************************
          Select;
          When P3PrType = 'S/R Online';
             scP3PrType = %trim(GetModuleProperty('S/R Online':'Name'))
              + ' ' + %trim(GetModuleProperty('Portal Module':'Name'));
              If scPayRrn = *blanks;
                 scPayRrn = *Zeros;
              Endif;
             //*** Beg Add *** 08/17/21 ***************************************
             //*** Beg Del *** 04/24/25 ***************************************
             //***strAlias = 'CREATE ALIAS QTEMP/WMP3P_ARC FOR WMP3P(ARCHIVE)';

             //***Exec sql
             //***Execute immediate :strAlias;
             //*** End Del *** 04/24/25 ***************************************
             sqlTrans = 'Select P3pgm, +
              case when p3dt = 0 +
              then '' '' +
              else Substr(Char(P3dt + 19000000),5,2) || ''/'' || +
              Substr(Char(P3dt + 19000000),7,2) || ''/'' || +
              Substr(Char(P3dt + 19000000),1,4) +
              end +
               , P3envFlg, P3prType, P3enbld, +
              case when p3efdt = 0 +
              then '' '' +
              else Substr(Char(P3efDt + 19000000),5,2) || ''/'' || +
              Substr(Char(P3efDt + 19000000),7,2) || ''/'' || +
              Substr(Char(P3efDt + 19000000),1,4) +
              end, +
              case when p3exdt = 0 +
              then '' '' +
              else Substr(Char(P3exDt + 19000000),5,2) || ''/'' || +
              Substr(Char(P3exDt + 19000000),7,2) || ''/'' || +
              Substr(Char(P3exDt + 19000000),1,4) +
              end +
              from WMP3P_ARC where P3co# = ' + %Char(Elco#) +
              ' and P3fnd = ' + %Char(Elfnd) + ' and P3emp# = ' +
              %Char(ELemp#) + ' and P3div = ' + %Char(ELdiv) +
              ' order by rrn(WMP3P_ARC) desc';
             //*** End Add *** 08/17/21 ***************************************
          When P3PrType = 'Third Party Payments';
             Exec sql
              select * into :dsWIPSP
               from WIPSP
               where PSco# = :ELco# and PSfnd = :ELfnd
               and PSemp# = :ELemp#;
             Exec sql
              select * into :dsWIPSPHIST
               from WIPSPHIST
               where PSco# = :ELco# and PSfnd = :ELfnd
               and PSemp# = :ELemp#
               and PSfield = 'PSOPTFLAG'
               order by PStrDt desc, PStrTm desc
               limit 1;
       //*** beg del ***** 09/22/21 *****************
       //      dis3rd = *on;
       //      vis3rd = *off;
       //*** end del ***** 09/22/21 *****************
             Clear scP3enbld;
             Clear scP3efDt;
             Select;
             When dsWIPSP.PSoptFlag = 'I';
                scP3enbld = 'Y';
                scP3efDt  = dtMDYY(dsWIPSPHIST.PStrDt);
             //*** Beg Del *** 08/17/21 ***************************************
             //***   scOutput = dtMDYY(dsWIPSPHIST.PStrDt);
             //*** End Del *** 08/17/21 ***************************************
             Endsl;
             //*** Begin Chg *** 07/29/21 **********************************
             //*** scAccount# = %char(ELfnd)+%char(ELemp#)+%char(ELdiv);
             scAccount# = %EditC(ELco#:'X') + %EditC(ELfnd:'X') +
                          %EditC(ELemp#:'X');
             //*** End   Chg *** 07/29/21 **********************************
             scPaper = dtMDYY(dsWIPSP.PSpaper);
             scAutoPay = dtMdYY(dsWIPSP.PSautoPay);
             //*** Begin Chg *** 08/17/21 **********************************
             //***sqlAction = 'select PSaction, PStrDtDsp ' +
             //*** 'from WIPSPHIST '+
             //*** 'where PSco#='+%char(ELco#) +
             //*** ' and PSfnd='+%char(ELfnd) +
             //*** ' and PSemp#='+%char(ELemp#) +
             //*** ' and PSdiv='+%char(ELdiv) +
             //*** ' and PSfield = ''PSOPTFLG''' +
             //*** ' order by PStrDt desc, PRtrTm desc';
             //*** Beg Chg ***** 09/11/23 **********************************
             //***sqlAction = 'select PSaction, substr(Char(PStrDtDsp),6,2) || +
             //*** ''/'' || substr(Char(PStrDtDsp),9,2) || ''/'' || +
             //*** substr(Char(PStrDtDsp),1,4)' +
             //*** 'from WIPSPHIST '+
             //*** 'where PSco#='+%char(ELco#) +
             //*** ' and PSfnd='+%char(ELfnd) +
             //*** ' and PSemp#='+%char(ELemp#) +
             //*** ' and PSfield = ''PSOPTFLAG''' +
             //*** ' order by PStrDt desc, PStrTm desc';
             sqlAction = 'select PSaction, substr(Char(PStrDtDsp),6,2) || +
              ''/'' || substr(Char(PStrDtDsp),9,2) || ''/'' || +
              substr(Char(PStrDtDsp),1,4) ' +
              'from WIPSPHIST '+
              'where PSco#='+%char(ELco#) +
              ' and PSfnd='+%char(ELfnd) +
              ' and PSemp#='+%char(ELemp#) +
              ' order by PStrDt desc, PStrTm desc';
             //*** End Chg ***** 09/11/23 **********************************
             //*** End   Chg *** 08/17/21 **********************************
          Endsl;

         //*** beg del ***** 09/22/21 *****************
         //   If xxQte = 'Q' and ELnwrw = 'R';
         //      dis3rdSfyr = *on;
         //   Else;
         //      dis3rdSfyr = dis3rd;
         //   Endif;
         //
         //   If xxQte = 'Q' ; //Don't allow edit if in Quote.
         //      If scP3enbld = 'Y';
         //         dis3rd = *On;
         //      Endif;
         //      dis3rdSfyr = *On;
         //   Endif;
         //*** end del ***** 09/22/21 *****************

          rrn3rdPay = rrn3rdPay + 1;
          Write Sfl3rdPay;
       Endsr;
      //***** End   Add *** 05/17/21 *****************************************
      /end-free
     P LoadEftInfo     E

       //===================================================================
       // Load Location/Entity tab data
       //===================================================================
     p LoadLocEnt      b
     d LoadLocEnt      pi              n
     d  AdL1           s             40    inz
     d  AdL2           s             40    inz
     d  AdL3           s             40    inz
     d  xxRatSt        s              2    inz
     d  xxStDiv        s              3  0 inz
     d  meetss         s               n
     d u1pDsc          s            100
     d u2pDsc          s             40
      /free
        Exsr ClearSflAddLoc;
        Exsr LoadSflAddloc;
        Exsr LoadLocDetail;
        Return *On;
       //***********************************************************************
       // ClearSflAddLoc
       //***********************************************************************
       BegSr ClearSflAddLoc;
         clrAddLoc = *On;
         dspAddLoc = *Off;
         dspLocYr = *Off;
         Write rcdLocEnt;
         clrAddLoc = *Off;
         dspAddLoc = *On;
         dspLocYr = *On;
         RrnAddLoc = 0;
       EndSr;
       //***********************************************************************
       // LoadSflAddloc;
       //***********************************************************************
       BegSr LoadSflAddloc;
         xxRatSt = GetRatingState(scco#:scfnd:scemp#:scdiv);
         xxStDiv = GetStRptDv(scco#:scfnd:scemp#);
         Setll (scCo#:scFnd:scEmp#:scDiv) pal00;
         DoU %Eof(wmall);
           Reade (scCo#:scFnd:scEmp#:scDiv) pal00;
           If %Eof(wmall);
             Leave;
           Endif;
           meetss = *On;
           Exsr MeetFilter;
           If meetss = *Off;
             iter;
           Endif;
           scALrrn = ALrrn;
           scSLocNam = ALname;
           If ALpad1 <> *Blanks;
             FormatAddress(ALpad1:ALpad2:ALpcty:ALpst:ALpzip:AdL1:AdL2:AdL3);
             scSAddress = %Trim(AdL1)+ ', ' + %Trim(AdL2)+ ', ' + %Trim(AdL3);
           Else;
             FormatAddress(ALmad1:ALmad2:ALmcty:ALmst:ALmzip:AdL1:AdL2:AdL3);
             scSAddress = %Trim(AdL1)+ ', ' + %Trim(AdL2)+ ', ' + %Trim(AdL3);
           Endif;
           scSELID = ALelid;
           scSFEIN = %editc(ALfein:'X');
           scSType = *blanks;
           Select;
           When ALeltp = 'E';
             scSType = 'Entity';
           When ALeltp = 'L';
             scSType = 'Location';
           EndSl;
           scSEffdt = CvtToDate(ALefdt);
           scSExpdt = CvtToDate(ALdldt);
           scSDiv = ALdiv;
           scSRatSt = xxRatSt;
           If scSDiv = xxStDiv;
             scSStRpt = 'Yes';
           Else;
             scSStRpt = 'No';
           EndIf;
           If flgE152 = *On;
             If ALdldt <> 0;
               SCe152 = *Off;
             Else;
               SCe152 = *On;
             Endif;
           Endif;
           RrnAddLoc += 1;
           If RrnAddLoc = svRrnAddLoc;
             scsSelect = *On;
           Else;
             scsSelect = *Off;
           Endif;
           If flgNBquote = *off;
              scSExpireV = ' Expire';
           Else;
              scSExpireV = ' Delete';
           Endif;
           Write SflAddLoc;
            If RrnAddLoc = 9999;
             str9999msg = 'Your filter returned too many records. '    +
                'Click Results to see the first 9,999 records or click Filter '+
                'to narrow your results.';
             btnFilter5=PopTwoBtn(str9999msg:'Claims Information, Selected +
                                  Billing Division':'Filter':'Results');

             If btnFilter5=*On;
               //Clear the subfile.
               clrAddLoc = *On;
               dspAddLoc = *Off;

               dspLocYr = *Off;
               Write rcdLocEnt;
               clrAddLoc = *Off;
               dspAddLoc = *On;
               dspLocYr = *On;
               RrnAddLoc = 0;
             Else;
               Leave;
             Endif;
            Endif;
         EndDo;

       EndSr;
       //***********************************************************************
       // MeetFilter
       //***********************************************************************
       BegSr MeetFilter;
       // Check Entity/Loc name against search string
       If ssEnLcNm <> ' ';
          u1pDsc = Upper(ALname + ALnam2);
          u2pDsc = Upper(ssEnLcNm);
          If %Scan(%Trim(u2pDsc):u1pDsc) <= 0;
            meetss = *Off;
            LeaveSr;
          Endif;
       Endif;

       // Check Employers Physical street Addr 1 against search string
       If ssPhAd1 <> ' ';
          u1pDsc = Upper(ALpad1);
          u2pDsc = Upper(ssPhAd1);
          If %Scan(%Trim(u2pDsc):u1pDsc) <= 0;
            u1pDsc = Upper(ALmad1);
            If %Scan(%Trim(u2pDsc):u1pDsc)<=0;
              meetss = *Off;
              LeaveSr;
            Endif;
          Endif;
       Endif;

       // Check if Entity/Location/Both
       If dsFilter.ssElTp <> ' ';
         Select;
         When dsFilter.ssElTp = 'L' and ALeltp <> 'L';
            meetss = *off;
            LeaveSr;
         When dsFilter.ssElTp = 'E' and ALeltp <> 'E';
            meetss = *off;
            LeaveSr;
         Endsl;
       Endif;

       // Check Fedearal Id
       If dsFilter.ssEnLcFein <> *zeros and dsFilter.ssEnLcFein <> ALfein;
          meetss = *off;
          LeaveSr;
       Endif;

       // Check Effecive date
       If dsFilter.ssEnLcEfDt <> 0 And
         ALefdt <> DtCYMD(dsFilter.ssEnLcEfDt);
             meetss = *off;
             LeaveSr;
       Endif;

       // Check State
       If dsFilter.ssEnLcStat <> ' ' and dsFilter.ssEnLcStat <> ALpst;
          meetss = *off;
          LeaveSr;
       Endif;
       EndSr;
       //***********************************************************************
       // LoadLocDetail
       //***********************************************************************
       BegSr LoadLocDetail;
         If RrnAddLoc > 0;
           CRrnAddLoc = svRrnAddLoc;
         Else;
           CRrnAddLoc = 0;
         Endif;
         LoadAddLocDtl(CRrnAddLoc);
         If flgLocwdw = *Off;
           LoadLocYrInf(svLocRrn);
           Exsr FillOwnOffInf;
         Endif;
       EndSr;
       //***********************************************************************
       // Fill Owner/Officer for Entity information
       //***********************************************************************
        BegSr FillOwnOffInf;
          If ALeltp = 'E';
            isError = LoadOwnOff(svLocRrn);
          Else;
            isError = LoadOwnOff(0);
          EndIf;
        EndSr;
      /end-free
     p LoadLocEnt      e
       //===================================================================
       // Load Location Year grid
       //===================================================================
     p LoadLocYrInf    b
     d LoadLocYrInf    pi              n
     d  inLocRrn                      9  0

     d outCount        s              5  0 inz
     d xxCnt           s              5  0 inz

      /free
        Exsr ClearSflLocYr;
        ptrMdsLocYr = BldLocYrGrd(inLocRrn:outCount);
        If outCount > 0;
          Exsr LoadSflLocYr;
        Endif;
        Return *On;
       //***********************************************************************
       // Clear Location year subfile
       //***********************************************************************
        BegSr ClearSflLocYr;
         clrLocYr = *On;
         dspLocYr = *Off;
         dspAddLoc = *Off;
         Write rcdLocEnt;
         clrLocYr = *Off;
         dspLocYr = *On;
         dspAddLoc = *On;
         RrnLocYr = 0;
        EndSr;
       //***********************************************************************
       // Load  Location year subfile
       //***********************************************************************
        BegSr LoadSflLocYr;
         For xxCnt = 1 to outCount;
           %Occur(mdsLocYr) = xxCnt;
           dsLocYr = fldsLY;
           RrnLocYr += 1;
           Write SflLocYr;
         EndFor;
        EndSr;
      /end-free
     p LoadLocYrInf    e

       //===================================================================
       // Load Owner/Officer Tab
       //===================================================================
     p LoadOwnOff      b
     d LoadOwnOff      pi              n
     d  inLocRrn                      9  0 options(*nopass)
     d                                     const

     d  AdL1           s             40    inz
     d  AdL2           s             40    inz
     d  AdL3           s             40    inz
     d  outCount       s              5  0 inz
     d  xxCnt          s              5  0 inz
      /free
        Exsr ClearSflOwnOff;
        If %Parms > 0;
          ptrMdsOwnOff = BldOwnOffGrd(xxEMrrn:outCount:inLocRrn);
        Else;
          ptrMdsOwnOff = BldOwnOffGrd(xxEMrrn:outCount);
        Endif;
        If outCount >0;
          Exsr LoadSflOwnOff;
        Endif;
        Return *On;
       //***********************************************************************
       // ClearSflOwnOff
       //***********************************************************************
       BegSr ClearSflOwnOff;
         dspOwnOff = *Off;
         clrOwnOff = *On;
         Write rcdOwnOff;
         clrOwnOff = *Off;
         dspOwnOff = *On;
         RrnOwnOff = 0;
       EndSr;
       //***********************************************************************
       // LoadSflOwnOff;
       //***********************************************************************
       BegSr LoadSflOwnOff;
         For xxCnt = 1 to outCount;
           %Occur(mdsOwnOff) = xxCnt;
           dsOwnOff = fldsOO;
           RrnOwnOff += 1;
           Write SflOwnOff;
         EndFor;
       EndSr;
      /end-free
     p LoadOwnOff      e

       //===================================================================
       // Load Additional Named Insured
       //===================================================================
     p LoadAddNam      b
     d LoadAddNam      pi              n

     d xxCnt           s              5  0 inz

      /free
        Exsr ClearSflAddNam;
        Exsr LoadSflAddNam;
        Return *On;
       //***********************************************************************
       // Clear Additional Named Insured
       //***********************************************************************
        BegSr ClearSflAddNam;
         clrAddNam= *On;
         dspAddNam= *Off;
         Write rcdAddNam;
         clrAddNam= *Off;
         dspAddNam= *On;
         RrnAddNam= 0;
        EndSr;
       //***********************************************************************
       // Load Additional Named Insured
       //***********************************************************************
        BegSr LoadSflAddNam;
         Setll (SCco#:SCfnd:SCemp#:SCdiv) pan00;
         Dou %Eof(wmanl);
           Reade (SCco#:SCfnd:SCemp#:SCdiv) pan00;
           If %Eof(wmanl);
             Leave;
           Endif;
           scANname = ANname + ANnam2;
           scANfein = ANfein;
           scANefdt = CvtToDate(ANefdt);
           scANexdt = CvtToDate(ANdldt);
           scANdiv = ANdiv;
           If ANstat = 'A';
             scANstat = 'Active';
           Else;
             scANstat = 'Inactive';
           Endif;
           //*** Beg Add ***** 03/07/23 *****************************
           scANNameP = ANNamP;
           scANNameI = ANNamI;
           xxTbId = 'WICX';
           xxKey  = ANCxRsn;
           Chain (scCo#:xxTbiD:xxKey) WTGTL;
           If %found(WTGTL);
              scANCxRsn = %Trim(GTdesc);
           Else;
              scANCxRsn = *Blanks;
           Endif;
           //*** End Add ***** 03/07/23 *****************************

           //*** Begin Add 04/08/19 ******************************************
           //*** Begin Change 12/30/19 ***************************************
           //If ClientId = '999' and VisPEO = *On and scEmp# = 1 and scDiv = 0;
           //*** Begin Change ** 02/11/20 **********************************
           If ClientId = '150' and VisPEO = *On;

           //*** Begin Change ** 02/11/20 **********************************
           // Select;
           // When (FNpeof = 'M' or FNpeof = 'C') and scemp# <> 1;
           //    scIs150 = *on;
           // When FNpeof = 'M' and ELemp# <> 1 and
           //      scdiv = GetStRptDv(scCo#:scFnd:scEmp#);
           //    scIs150 = *on;
           // Other;
           //    scIs150 = *Off;
           // Endsl;

              If (FNpeof = 'M' or FNpeof = 'C') and scemp# = 1 and scDiv = 0;
                 scIs150 = *On;
              Else;
                 scIs150 = *Off;
              Endif;
           //*** End   Change ** 02/11/20 **********************************

           Else;
              scIs150 = *Off;
           Endif;

           If scIs150 = *on;
           //*** End   Change 12/30/19 ***************************************
              scANIhdgs = xxANIhdgs150;
              scIs150 = *On;
              xxTbId = 'BUSI';
              xxKey  = ANtpob;
              Chain (SCco#:xxTbiD:xxKey) WTGTL;
              If %found(WTGTL);
                 scTpOb = %Trim(GTdesc);
              Else;
                 scTpOb = *blanks;
              Endif;
              scCol0 = *blanks;
           Else;
              scANIhdgs = xxANIhdgs;
              scTpOb = *blanks;
              scIs150 = *Off;
              scCol0 = '0';
           Endif;
           //*** End   Add 04/08/19 ******************************************
           scANrrn = ANrrn;
           RrnAddNam += 1;
           Write SflAddNam;
         EndDo;
        EndSr;
      /end-free
     p LoadAddNam      e
       //***  Begin Add  ***  10/16/17  ************************************
       //===================================================================
       // Load Collateral A/R
       //===================================================================
     p LoadCollatAR    b
     d LoadCollatAR    pi              n

     D strSqlCursor    s            500
     d xxCnt           s              5  0 inz
     d xxEmp#          s                   like(scEmp#)
     d xxDiv           s                   like(scDiv)

     d dsWDF1p         ds
     d  F1colB                        2
     d  F1colP                        2
     d  F1colT                        2
     d dsWMEAP       e ds                  extname(wmeap) inz qualified

      /free
       //Exec SQL set option COMMIT=*NONE;
       Exec SQL select F1colB, F1colP, F1colT
        into :dsWDF1P
        from WDF1P where F1co# = :ELco# and F1fnd = :ELfnd
         and F1fyr = :kkFyr;

        Exsr ClearSflCollatAR;
        Exsr LoadSflCollatAR;

        Return *On;
       //***********************************************************************
       // Clear Collateral A/R
       //***********************************************************************
        Begsr ClearSflCollatAR;
         clrColAR = *on;
         dspColAR = *off;
         Write rcdCollat;
         clrColAR = *off;
         dspColAR = *on;
         rrnColAR = 0;
        Endsr;
       //***********************************************************************
       // Load Collateral A/R information
       //***********************************************************************
        Begsr LoadSflCollatAR;
         strSqlCursor = 'select * ' +
          'from WMEAP ' +
          'where EAco# = ' + %editc(ELco#:'X') +
          '  and EAfnd = ' + %editc(ELfnd:'X') +
          '  and EAemp# = ' + %editc(ELemp#:'X') +
          '  and EAfyr = ' + %editc(kkFyr:'X') +
          '  and EAarCd in ' +
          '   (''' + F1colB + ''',''' + F1colP + ''',''' + F1colT + ''')';
         Exec SQL Prepare sqlEA From :strSqlCursor;
         Exec SQL Declare cursorEA Cursor For sqlEA;
         Exec SQL Open cursorEA;
         Dou SQLSTT <> '00000';
            Exec SQL Fetch next From cursorEA into :dsWMEAP;
            Select;
            When SQLSTT <> '00000';
               Iter;
            Endsl;
            scColAmt = dsWMEAP.EAamt;
            scColEntDt = CvtToDate(dsWMEAP.EAtrDt);
            Select;
            When dsWMEAP.EAcolPerc = *zero;
               scColPercA = 'Manual Entry';
               Clear scColEffDt;
               Clear scColExpDt;
            Other;
               scColPercA = %Editc(dsWMEAP.EAcolPerc:'3');
               scColEffDt = CvtToDate(dsWMEAP.EAcolEfDt);
               scColExpDt = CvtToDate(dsWMEAP.EAcolExDt);
            Endsl;
            rrnColAR  += 1;
            Write SflColAR;
         Enddo;
         Exec SQL Close cursorEA;
        Endsr;
      /end-free
     p LoadCollatAR    e
       //===================================================================
       // Load Collateral
       //===================================================================
     p LoadCollateral  b
     d LoadCollateral  pi              n

     d xxCnt           s              5  0 inz
     d xxEmp#          s                   like(scEmp#)
     d xxDiv           s                   like(scDiv)

      /free
        Select;
        When FNpeoF <> *blank;
           xxEmp# = 1;
           xxDiv = 0;
        Other;
           xxEmp# = scEmp#;
           xxDiv = scDiv;
        Endsl;
        isColAdd = *off;
        dspColEdt = *off;
        dspColAdd = *off;
        Select;
        When FNpeoF <> *blank and scEmp# = 1 and scDiv = 0;
           dspColEdt = *on;
           dspColAdd = *on;
        When FNpeoF = *blank and scDiv = GetStRptDv(scCo#:scFnd:scEmp#);
           dspColEdt = *on;
           dspColAdd = *on;
        Endsl;
        isColAdd = dspColAdd;
        scColAddTp = *blanks;
        Select;
        When dspColAdd = *on and not AllYrLargeDed(scCo#:scFnd:xxEmp#:xxDiv);
           isColAdd = *off;
           scColAddTp = 'All years must be set to Large Deductible.';
        Endsl;

        Exsr ClearSflCollateral;
        Exsr LoadSflCollateral;

        Return *On;
       //***********************************************************************
       // Clear Collateral
       //***********************************************************************
        Begsr ClearSflCollateral;
         clrCollat = *on;
         dspCollat = *off;
         Write rcdCollat;
         clrCollat = *off;
         dspCollat = *on;
         rrnCollat = 0;
        Endsr;
       //***********************************************************************
       // Load Collateral information
       //***********************************************************************
        Begsr LoadSflCollateral;
         Setll (scCo#:scFnd:xxEmp#) pco01;
         Dou %eof;
            Reade (scCo#:scFnd:xxEmp#) pco01;
            Select;
            When %eof;
               Iter;
            Endsl;
            scColPerc  = COperc;
            scColEffDt = CvtToDate(COefDt);
            scColExpDt = CvtToDate(COexDt);
            scColRrn = COrrn1;
            rrnCollat += 1;
            Write SflCollat;
         Enddo;
        Endsr;
      /end-free
     p LoadCollateral  e
       //***  End   Add  ***  10/16/17  ************************************
       //***  Begin Add  ***  12/17/18  ******************************
       //===================================================================
       // Load portal captured information
       //===================================================================
     pLoadPortalInfo   b
     dLoadPortalInfo   pi             1n

       //  Begin Add  ***  02/17/20  ***************************
     darySbmInfo       s            100    dim(3) inz
       //  End   Add  ***  02/17/20  ***************************
     di1               s              5  0 inz
     ddecCountLoss     s              5  0 inz
     ddecCountQuest    s              5  0 inz
       //  Begin Add  ***  02/17/20  ***************************
     dstrPhn#          s             10    varying inz
       //  End   Add  ***  02/17/20  ***************************
     ddsWIEMP          ds                  qualified inz
     d app#                           9  0
     d year                           3  0
     ddsWIAPP          ds                  qualified inz
     d yib                            3  0
     d audFrq                        10
     d***  Begin Add  ***  02/17/20  ***************************
     d sbmName                      100
     d sbmEmail                     100
     d sbmPhn#                       10  0
     d***  End   Add  ***  02/17/20  ***************************
     d***  Begin Chg  ***  02/17/20  ***************************
     d*dsWIAPPANS       ds                  qualified inz dim(9999)
     d* id#                            7  0
     d***  Begin Add  ***  07/15/19  ***************************
     d* class                          5  0
     d***  End   Add  ***  07/15/19  ***************************
     d* type                          40
     d* question                     500
     d* answer                        25
     ddsWIAPPANSin     ds                  inz
     d id#                            7  0
     d class                          5  0
     d type                          40
     d question                     500
     d acord                          9  0
     d answer                        25
     ddsWIAPPANS       ds                  likeds(dsWIAPPANSin) inz dim(9999)
     ddsWIAPPANSnull   s              5i 0 dim(6)
     d***  End   Chg  ***  02/17/20  ***************************
     ddsWIAPPLOS       ds                  qualified inz dim(999)
     d year                           3  0
     d prem                          13  2
     d #Losses                        5  0
     d paid                          13  2
     d reserve                       13  2
      ***** Beg Add ***** 08/02/21 *****************************
     d flgPrem                        1A
     d flgLoss                        1A
     d flgPaid                        1A
     d flgRsv                         1A
      ***** End Add ***** 08/02/21 *****************************

      /Free
       //isPortStub = *on;
       scPortHdg = strPortalTabNm + ' Captured Information';
       Exec sql
        select EMAPP#, EMFYR
         into :dsWIEMP
         from WIEMP
         where EMCO# = :scCo# and EMFND = :scFnd and EMEMP# = :scEmp#;
      //***  Begin Chg  ***  02/17/20  *** Add Submitter Info ****
       Exec sql
        select APYIB, APAUDFRQ, APSBMUSER, APSBMEMAIL, APSBMPHN#
         into :dsWIAPP
         from WIAPP
         where APCO# = :scCo# and APAPP# = :dsWIEMP.app#;
       scApp# = dsWIEMP.app#;
       scAppYr = dsWIEMP.year;
       scYIB = dsWIAPP.yib;
       scAudFrq = dsWIAPP.audFrq;
       //  Begin Add  ***  02/17/20  ***************************
       Exsr LoadSubmitterInfo;
       //  End   Add  ***  02/17/20  ***************************
       Exsr LoadScore;
       Exsr LoadLosses;
       Exsr LoadQuestions;
       Return *on;
       //======================================================================
       // Clear lossess grid
       //======================================================================
       Begsr ClearLosses;
        clrLosses = *on;
        dspLosses = *off;
        Write rcdPortal;
        clrLosses = *off;
        dspLosses = *on;
        rrnLosses = 0;
       Endsr;
       //======================================================================
       // Clear questions grid
       //======================================================================
       Begsr ClearQuestions;
        clrQuest = *on;
        dspQuest = *off;
        Write rcdPortal;
        clrQuest = *off;
        dspQuest = *on;
        rrnQuest = 0;
       Endsr;
       //======================================================================
       // Load lossess grid
       //======================================================================
       Begsr LoadLosses;
        Exec sql
         declare cursorLoss cursor for
          select APYEAR, APANNPREM, AP#LOSSES, APPAID, APRESERVE,
          //***** Beg Add ***** 08/02/21 **************************************
           APANNPREMZ,AP#LOSSESZ,APPAIDZ,APRESERVEZ
          //***** End Add ***** 08/02/21 **************************************
           from WIAPPLOS
           where APCO# = :scCo# and APAPP# = :dsWIEMP.app#;
        Exec sql close cursorLoss;
        Exec sql open cursorLoss;
        Exec sql
         fetch cursorLoss for 999 rows into :dsWIAPPLOS;
        decCountLoss = SqlEr3;
        Exsr ClearLosses;
        For i1 = 1 to decCountLoss;
           //***** Beg Add ***** 08/02/21 *****************************
           visLossPrm = *on;
           visLoss = *on;
           visLossPd = *on;
           visLossRes = *on;
           //***** End Add ***** 08/02/21 *****************************
           scLossYr = scAppYr - dsWIAPPLOS(i1).year;
           scLossPrm = dsWIAPPLOS(i1).prem;
           scLosses = dsWIAPPLOS(i1).#Losses;
           scLossPd = dsWIAPPLOS(i1).paid;
           scLossRes = dsWIAPPLOS(i1).reserve;
           //***** Beg Add ***** 08/02/21 *****************************
           If scLossPrm = 0;
              If dsWIAPPLOS(i1).flgprem <> 'Y';
                 visLossPrm = *off;
              Endif;
           Endif;
           If scLosses = 0;
              If dsWIAPPLOS(i1).flgLoss <> 'Y';
                 visLoss = *off;
              Endif;
           Endif;
           If scLossPd = 0;
              If dsWIAPPLOS(i1).flgPaid <> 'Y';
                 visLossPd = *off;
              Endif;
           Endif;
           If scLossRes = 0;
              If dsWIAPPLOS(i1).flgRsv <> 'Y';
                 visLossRes = *off;
              Endif;
           Endif;
           //***** End Add ***** 08/02/21 *****************************
           rrnLosses += 1;
           Write sflLosses;
        Endfor;
       Endsr;
       //======================================================================
       // Load questions grid
       //======================================================================
       Begsr LoadQuestions;
        //  Begin Chg  ***  07/15/19  *** Add APCLAS ************
        //  Begin Chg  ***  02/17/20  ***************************
        Exec sql
         declare cursorQuest INSENSITIVE cursor for
          with
          parent (co#, id#, question, parent, depth, family, order) as
          (select p.QUco#, p.QUid#, p.QUquestion, p.QUparent, 1 depth,
            ROW_NUMBER() OVER (ORDER BY p.QUid#) family,
            cast(lpad(p.QUseq#,7,' ') || lpad(p.QUid#,7,' ')as char(200)) order
            from WIQUP as p
             where p.QUparent = 0
           union all
            select c.QUco#, c.QUid#, c.QUquestion, c.QUparent,
             parent.depth+1 depth_level, family,
             substr(parent.order,1,14*parent.depth)
              || lpad(c.QUseq#,7,' ') || lpad(c.QUid#,7,' ') order_lvl
             from WIQUP c, parent
             where c.QUparent = parent.id#),
          list (co#, id#, parent, depth, family, order, questions) as
          (select parent.co#, parent.id#, parent.parent, depth, family, order,
            RPAD('-',(depth)*1-1,'-')||question as questions
            from parent)
          select APID#, APCLAS, q1.QUTYPE, l.questions, q3.QUACORD#,
             case when q1.QUTYPE = 'Schedule Rating' and Upper(APRESP) = 'YES'
                   then cast(q2.QUFACTOR as char(25))
                  else APRESP end as Response_Factor
           from list l
           join WIAPPANS         on l.co# = APCO#    and l.id# = APID#
           left join WIQUP    q1 on l.co# = q1.QUCO# and l.id# = q1.QUID#
           left join WIQUPSCD q2 on l.co# = q2.QUCO# and l.id# = q2.QUID#
           left join WIQUPACD q3 on l.co# = q3.QUCO# and l.id# = q3.QUID#
           where APCO# = :scCo# and APAPP# = :dsWIEMP.app#
           order by QUTYPE, q3.QUACORD#, l.order;
        Exec sql close cursorQuest;
        Exec sql open cursorQuest;
        //  Begin Add  ***  02/17/20  ***************************
        Exec SQL  Get Diagnostics :decCountQuest = DB2_NUMBER_ROWS;
        //  End   Add  ***  02/17/20  ***************************
      //  Begin Del  ***  02/17/20  ***************************
      //Exec sql
      // fetch cursorQuest for 9999 rows into :dsWIAPPANS :dsWIAPPANSnull;
      //decCountQuest = SqlEr3;
      //  End   Del  ***  02/17/20  ***************************
        Exsr ClearQuestions;
        For i1 = 1 to decCountQuest;
           //  Begin Add  ***  07/15/19  ***************************
           Exec sql
            fetch next from cursorQuest into :dsWIAPPANSin :dsWIAPPANSnull;
           If not IsValidFetch(sqlstt);
              Iter;
           Endif;
           dsWIAPPANS(i1) = dsWIAPPANSin;
           //  End   Add  ***  07/15/19  ***************************
           scQuestId = dsWIAPPANS(i1).id#;
           scQuestTyp = dsWIAPPANS(i1).type;
           //  Begin Add  ***  07/15/19  ***************************
           If dsWIAPPANS(i1).class <> *zero;
              scQuestTyp = %trim(scQuestTyp) +
               ' (' + %editc(dsWIAPPANS(i1).class:'X') + ')';
           Endif;
           //  End   Add  ***  07/15/19  ***************************
           //  Begin Add  ***  02/17/20  ***************************
           If dsWIAPPANS(i1).acord <> *zero;
              scQuestTyp = %trim(scQuestTyp) +
               ' (' + %trim(%editc(dsWIAPPANS(i1).acord:'Z')) + ')';
           Endif;
           //  End   Add  ***  02/17/20  ***************************
           scQuestion = dsWIAPPANS(i1).question;
           scAnswer = dsWIAPPANS(i1).answer;
           rrnQuest += 1;
           Write sflQuest;
        Endfor;
        scFactor = GetScheduleRatingFactor(scCo#:dsWIEMP.app#:dsWIEMP.year
         :scFnd:scEmp#:0:scFAsOf);
       Endsr;
       //======================================================================
       // Load score
       //======================================================================
       Begsr LoadScore;
        visScore = *off;
        scScore = 'Stub';
       Endsr;
       //  Begin Add  ***  02/17/20  ***************************
       //======================================================================
       // Load submitter information
       //======================================================================
       Begsr LoadSubmitterInfo;
        vsSbmInfo = *off;
        Reset i1;
        Reset arySbmInfo;
        If dsWIAPP.sbmName <> *blank;
           i1 += 1;
           arySbmInfo(i1) = dsWIAPP.sbmName;
        Endif;
        If dsWIAPP.sbmEmail <> *blank;
           i1 += 1;
           arySbmInfo(i1) = dsWIAPP.sbmEmail;
        Endif;
        If dsWIAPP.sbmPhn# <> *zero;
           i1 += 1;
           strPhn# = %editc(dsWIAPP.sbmPhn#:'Z');
           Select;
           When %len(strPhn#) > 7;
              arySbmInfo(i1) = '(' + %subst(strPhn#:1:3) + ')' +
               %subst(strPhn#:4:3) + '-' + %subst(strPhn#:7:4);
           Other;
              arySbmInfo(i1) = %subst(strPhn#:1:3) + '-' + %subst(strPhn#:4:4);
           Endsl;
        Endif;
        If i1 > 0;
           vsSbmInfo = *on;
        Endif;
        scSbmInfo1 = arySbmInfo(1);
        scSbmInfo2 = arySbmInfo(2);
        scSbmInfo3 = arySbmInfo(3);
       Endsr;
       //  End   Add  ***  02/17/20  ***************************
      /End-free
     pLoadPortalInfo   e
       //***  End   Add  ***  12/17/18  ******************************
       //***********************************************************************
       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     P LoadSelection   B
     D LoadSelection   PI
      /free

       Clear divChoices;
       divChoices = 'EMCO# = ' + %char(ELCO#) + ' AND EMFND = '
        + %char(ELFND) + ' AND EMEMP# = ' + %char(ELEMP#);

       clear fyrChoices;
       fyrChoices = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
        + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
        + ' AND ELdiv = ' + %char(ELdiv);

      /end-free
     P LoadSelection   E

       //===================================================================
       // Load Additional location detail box Detail
       //===================================================================
     p LoadAddLocDtl   b
     d LoadAddLocDtl   pi
     d  inSFLrrn                      5  0
     d  AdL1           s             40    inz
     d  AdL2           s             40    inz
     d  AdL3           s             40    inz
     d  xxKey          s             10    inz
      /free
        svLocRrn = 0;
        If inSFLrrn > 0;
          Chain inSFLrrn SflAddLoc;
          Chain(n) scAlrrn pal0p;
          If %Found(wmalp);
            svLocRrn = scALrrn;
            Exsr FillAddLocDtl;
          Endif;
        Endif;
        svRrnAddLoc = inSFLrrn;
       //***********************************************************************
       // Fill Additional Address Location Detail box
       //***********************************************************************
        BegSr FillAddLocDtl;
          scdDiv = ALdiv;
          scdType = scsType;
          scdElid = scsElid;
          scdFein = %int(scsFein);
          scdDbaName =  ALname  + ALnam2;
          scdFedName =  ALfdnm + ALfdn2;
          ScDBAPerNm =  ALnamp;
          ScFedPerNm =  ALfdnp;
          ScDBAIndNm =  ALnami;
          ScFedIndNm =  ALfdni;
          FormatAddress(ALmad1:ALmad2:ALmcty:ALmst:ALmzip:AdL1:AdL2:AdL3);
          scdMadd1 = AdL1;
          scdMadd2 = Adl2;
          scdMadd3 = Adl3;
          FormatAddress(ALpad1:ALpad2:ALpcty:ALpst:ALpzip:AdL1:AdL2:AdL3);
          scdPadd1 = AdL1;
          scdPadd2 = AdL2;
          scdPadd3 = AdL3;
          //***  Begin Add  ***  02/23/18  *******************************
          ScNonSAdrM = ALnonm;
          ScNonSAdrP = ALnonp;
          //***  End   Add  ***  02/23/18  *******************************
          xxKey = scSRatSt + ALcnty;
          scdCounty = %Trim(CountyCdDs(ALco#:xxKey));
          scdPhone# = ALarea * 10000000 + ALphn#;
          scdFax# = ALfara * 10000000 + ALfax#;
          If ALncci = 'Y';
            scdRepNCCI = 'Yes';
          Else;
            scdRepNCCI = 'No';
          Endif;
          scdSrisk = ALsemp;
          scdSUnEmp# = ALue#;
          xxKey = ALtpob;
          scdTypBus = %Trim(GetGenCodeDs(ALco#:'BUSI':xxKey));
          xxKey = ALsic;
          scdSic = ALsic + ' - ' + %Trim(GetGenCodeDs(ALco#:'SIC ':xxKey));
          xxKey = ALnaic;
          scdNaic = ALnaic + ' - ' + %Trim(GetGenCodeDs(ALco#:'NAIC':xxKey));
          scdHdg1 = xxHdg1;
          scdHdg2 = xxHdg2;
          scdHdg3 = xxHdg3;
          scdCont1 = ALcnt1;
          scdCont2 = ALcnt2;
          scdCont3 = ALcnt3;
          scDMapCoor = ALmapCoor;
          sc#ShftEmp = ALshft;
          Clear pmp00;
          Chain(n) (ALco#:ALfnd:ALemp#:ALdiv:'Y':ALelid) pmp00;
          If %found(wmmpl);
             scMPNName = MPname;
             FormatAddress(MPadr1:MPadr2:MPcity:MPst:MPzip:AdL1:AdL2:AdL3);
             scMPNAdd1 = ADL1;
             scMPNAdd2 = ADL2;
             scMPNAdd3 = ADL3;
             scMPNTyp  = MPpTyp;
          Endif;

        EndSr;
      /end-free
     p LoadAddLocDtl   e
       //===================================================================
       // Load Department Grid
       //===================================================================
     p LoadDepart      b
     d LoadDepart      pi              n
     d  inDeptRrn                     9  0 options(*nopass)
     d                                     const

     d  outCount       s              5  0 inz
     d  xxCnt          s              5  0 inz
      /free
        Exsr ClearDepartSfl;
        ptrMdsDept = BldDeptGrd(xxEMrrn:outCount);
        If outCount >0;
          Exsr LoadDepart;
        Endif;
        Return *On;
       //***********************************************************************
       // ClearDepartSfl
       //***********************************************************************
       BegSr ClearDepartSfl;
         dspDepart = *Off;
         clrDepart= *On;
         Write rcdDeptGrd;
         clrDepart=*Off;
         dspDepart=*On;
         RrnDept = 0;
       EndSr;
       //***********************************************************************
       // LoadDepart;
       //***********************************************************************
       BegSr LoadDepart;
         For xxCnt = 1 to outCount;
           %Occur(mdsDept) = xxCnt;
           dsDept = fldsDP;
           RrnDept += 1;
           Write SflDepart;
         EndFor;
       EndSr;
      /end-free
     p LoadDepart      e
       //===================================================================
       // Set Display Indicators
       //===================================================================
     p setDspInd       b                   export
     d setDspInd       pi
     d inQte                          1A
     d inCall                         3A
     d inMode                         1A
     d inDiv                          5  0

      /Free
       Exsr clearIndicators;
       isRenewalQte = *off;
       If inQte = 'Q' and GetRnwlQte(ELco#:ELfnd:ELemp#:inDiv) = 'Y' and
           inMode <> 'A' and GetRnwlQteDiv(ELco#:ELfnd:ELemp#:inDiv) <> 'Y';
          isRenewalQte = *on;
       Endif;

       Select;
       When xxMode='D';
         //Display
         dspMod = *On;
         SCe110 = *On;
         SCe120 = *On;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *On;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *On;
         SCe140 = *On;
         SCe180 = *On;
         hideLYopt = *On;
       When isRenewalQte;
         dspMod = *On;
         SCe110 = *On;
         SCe120 = *On;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *On;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *On;
         SCe140 = *On;
         SCe180 = *On;
         //*** Begin Add *** 05/17/21 ******************************************
         dspAddEft = *On;
         //*** End   Add *** 05/17/21 ******************************************
       When xxMode = 'C';
         //Change
         SCe150 = *On;
         SCe151 = *On;
         SCe152 = *On;
         flgE152 = *On;
         SCe160 = *On;
         SCe161 = *On;
         SCe162 = *On;
         dspAddEft = *On;
         dspEftLnk = *On;
         dspInsEdt = *On;
         dspInsAdd = *On;
       Endsl;

       Begsr clearIndicators;
         dspMod = *Off;
         SCe110 = *Off;
         SCe120 = *Off;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *Off;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *Off;
         SCe140 = *Off;
         SCe180 = *Off;
         hideLYopt = *Off;
         SCe150 = *Off;
         SCe151 = *Off;
         SCe152 = *Off;
         flgE152 = *Off;
         SCe160 = *Off;
         SCe161 = *Off;
         SCe162 = *Off;
         dspAddEft = *Off;
         dspEftLnk = *Off;
         dspInsEdt = *Off;
         dspInsAdd = *Off;
       Endsr;
      /End-Free
     psetDspInd        e
      * utility procedure definitions
      /COPY SCOPYPOLPI
