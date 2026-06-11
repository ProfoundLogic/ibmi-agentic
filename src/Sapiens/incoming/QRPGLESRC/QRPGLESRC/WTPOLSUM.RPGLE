     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLSUM - Policy Summary.
     H DFTNAME(WTPOLSUM)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 03/07/12                                         *
    *  Programmer: C:190                                            *
    *  Project   : WC2126                                           *
    *===============================================================*
    *  Revisions:                                                   *
      *
      * 07/18/25 C:349 Support: 119334 (CoveRisk)
      *                Include division#,location# and tooltip in the *
      *                sflClmtInf & sflClmtTb2 grids.                 *
      *                Increased fields scClmLocNm & scTb2LocNm from  *
      *                40 to 100 bytes to hold the location number.   *
      *                Increased width property of scClmDivNm,scClmLocNm,
      *                scTb2DivNm and scTb2LocNm to 500px to prevent  *
      *                text wrapping when the division or location    *
      *                names have a long name.                        *
      *                                                               *
      * 06/17/24 C:344 Project: WC2952                                *
      *                Expand rate screen field to 8.3 zone and       *
      *                increase Original Rate in the GetOrgRate sub-  *
      *                procdure.
      *                                                               *
      * 03/04/24 C:79  Project: WC2951                                *
      *                Recompile due to changes to WTPOLDTL           *
      *
      * 02/27/24 C:333 PROJECT: WC2953
      *                Premium History Balance to Minimum
      *                - Adding Container 1 , container 2 & OutInd    *
      *                - Changes in rcdDivPrem onload and sflPrmHist  *
      *                  onrowclick.                                  *
      *                                                               *
      * 07/06/22 C:65  Support : 102825 (FMIC)                        *
      *                Do not chain to different emplr/div/fyr prior  *
    *                to deleting a policy year in order to not      *
    *                delete the wrong year.  Related to WC2303 chg. *
    *                                                               *
      * 04/11/22 C:333 Project: SY434                                 *
      *                - Update Grids for Sorting Filtering Upgrade   *
    *                                                               *
      * 03/02/22 C:300 Support : 101207 (MPE)                         *
      *                Error on Policy Summary for large claim amount *
      *                Error - 'Receiver value too small to hold result'*
      *                Changes in DSPF - Increased the lenght of scClmPdLos
      *                and scTb2PdLos from 9 to 11 on Claim info Grid on Tab 1 & 2
    *                                                               *
      * 06/22/21 C:295 Project: WC2302                                *
      *                Auto Tab Closure on Deletion                   *
      *                                                               *
      * 05/21/21 C:180 Project: WC1986                                *
      *                Use GetEftStatus                               *
      *                                                               *
      * 07/16/20 C:300 Support : 89981                                *
      *                Policy Summary will no longer completely show up
      *                when its used as a background for a dialog window
      *                overlaying it                                  *
      *                (for Mid-term Endorsement and Revised bill)    *
      *                                                               *
      * 05/13/20 C:65  Support : 89081 (Demo)                         *
      *                Prevent divide by zero from 83466 coding.      *
      *                                                               *
      * 01/29/20 C:300 Support : 86530(S1 Medical)                    *
      *                Policy screens take extremely long time to load*
      *                                                               *
      * 12/03/19 C:79  Support: 150/83466                             *
      *                1. Change key list to use Fnd and Fyr on Claims*
      *                   file                                        *
      *                2. On Premium info screen for Claims info add  *
      *                   all states not just the rating state        *
      *                                                               *
      * 03/29/19 C:33  Project: SY406                                 *
      *                Check appropriate records for Lockout          *
      *                                                               *
      * 12/18/18 C:33  Project: SY406                                 *
      *                Allow for different 'lockout' processes        *
      *                                                               *
      * 07/11/18 C:062 Support : 72064(099)                           *
      *                Delete the correct policy year                 *
      *                                                               *
      * 07/02/18 C:291 Support : 46769                                *
      *                Add an edit to prevent the policy year to be   *
      *                deleted if it is the only year on the policy.  *
      *                                                               *
      * 06/14/18 C:291 Support : 70665                                *
      *                Clear CX date if Policy is Renewed, the Renewal*
      *                term is cancelled and renewal term is deleted. *
      *                                                               *
      * 04/27/18 C:UXN Support: 69971                                 *
      *                Inrease the length of xxToti variable to hold  *
      *                value above Billion.                           *
      *                                                               *
      * 04/17/18 C:291 Project : WC2593
      *                Removed changes done under WC2435. Mode will be*
      *                passed by JavaScript in 1st position of the    *
      *                4th parameter.                                 *
      *                                                               *
      * 02/07/18 C:291 Support: 63953                                 *
      *                Change Division dropdown "max choices" property*
      *                to 100,000. Screen change only.                *
      *                                                               *
      * 09/24/17 C:062 Project : 150WAL
      *                Add territorial diofferential factor
      *                                                               *
      * 11/18/16 C:310 Project : WC2435
      *                To call Certificate Holders screen as popup from
      *                Policy Tool bar. Need to pass the mode as well.
      *                                                               *
      * 09/21/16 C:136 Support: 43058                                 *
      *                Don't calculate earned premium in quote        *
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      * 11/25/15 C:309 Support: 37835                                 *
      *                Adding popup window for maximum record loaded  *
      *                in sflEnLctn                                   *
      *                                                               *
      * 10/27/15 C:308 Support: 32424                                 *
      *                Correct Delete year link on Selected Billing   *
      *                Division Tab on policy screen.                 *
      *                                                               *
      * 10/12/15 C:136 Support: 37814                                 *
      *                Limit the number of records loaded to          *
      *                sflEnLctn (Location Entity grid) to 9,999      *
      *                which is the subfile maximum record size       *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *
      *                                                               *
      *    07/29/15 C:306 Project: 36158                              *
      *                   Fixed for cancellation information will be  *
      *                   displayed for all years on the selected     *
      *                   billing division tab. (Tab2)                *
      *                                                               *
      *    06/01/15 C:308 Project: 34975                              *
      *                   Add 'Employer not eligible for deletion     *
    *                   S/R payroll has been reported' message to   *
      *                   WCR4808 MSGID.                              *
      *                                                               *
      *    05/22/15 C:306 Support: 34780                              *
      *                   Populate the year Dropdown box correctly    *
      *                   on policy summary screen.                   *
      *                                                               *
      *    04/16/15 C:65  Project: SY251                              *
      *                   Add API call to launch new Department       *
    *                   letters.                                    *
    *                                                               *
    *    10/08/14 C:291 RED 3161                                    *
    *                   Change the date definition on the grid from *
    *                   Number to date data type.                   *
    *                                                               *
    *    08/08/14 C:290 Support WC099 86393                         *
    *                   Modified the message appearing when a quote *
    *                   is currently being worked in FastQuote and  *
    *                   CompSuite tries to open that for processing *
    *                                                               *
    *    08/06/14 C:242 Support WC045 86372                         *
    *                   Apply Secure Claim Filter to Claims listed  *
    *                   in Policy Summary pages:- All Divisions,    *
    *                   Selected Division, Premium Information popup*
    *                                                               *
    *    07/22/14 C:267 Red 4551                                    *
    *                   Execute procedure LoadActionBar() only when *
    *                   any of the main key variables to Employer   *
    *                   Detail File WDELP change.                   *
    *                                                               *
    *    05/28/14 C:242 Support WA133 85618                         *
    *                   Claimant Tab Name generated from Selected   *
    *                   Billing Division Summary is fixed.          *
    *                                                               *
      *    05/01/14 C:267 Red 2523                                    *
    *                  -Allow driver program to call NEXT program   *
    *                   when ADD policy year button is clicked      *
    *                  -Updatdd UI by removing the bound RPG field  *
    *                   SCURL for passing the URL information.      *
    *                   This pertains to the Policy Information     *
    *                   screen only.                                *
    *                                                               *
      *    04/01/14 C:295 RED 3185                                    *
      *                   To change the date type to correct format   *
      *                   as 'Export to Excel' will get correct date  *
      *                   format in the excel sheet.                  *
      *                                                               *
    *    03/26/14 C:65  Support: Redmine #4089                      *
    *                   Expand size of loss fields in premium/state *
    *                   popup to avoid data size problems.          *
    *                                                               *
      *    03/18/14 C:279 Redmine: 3208                               *
      *                   Modified to default the Claim status        *
      *                   correctly on selected division tab.         *
      *                                                               *
      *    03/12/14 C:295 RED 3185                                    *
      *                   To change the date type to correct format   *
      *                   as 'Export to Excel' will get correct date  *
      *                   format in the excel sheet.                  *
      *                                                               *
      *    01/14/14 C:295 Support: WA133/84219                        *
      *                  The number of claims in the policy are not   *
      *                  displayed with origianl value. The number of *
      *                  open Claim and closed claims and total claims*
      *                  The number Claims should be current values.  *
    *                                                               *
      *    12/12/13 C:267 Red 1509                                    *
      *               295 Added new prodcedure TitleCase.             *
      *                                                               *
    *    10/07/13 C:190 Project 25WSS                               *
    *                   - Display the total of the "Accumulated -   *
    *                     Claims Liability" for TASB.               *
      *                   - Change the program to determine if        *
      *                     Premium or Contribution should be printed *
      *                     /displayed based on Group setup.          *
    *                                                               *
    *    07/10/13 C:267 Support: Redmine #1180 (Haifa Kealy)        *
    *                   Zero suppress agent and producer numbers.   *
    *                                                               *
    *    07/08/13 C:267 Support: Redmine #1180 (Haifa Kealy)        *
    *                   UI - Added two new columns - Agency Name and*
    *                   Producer Name.                              *
    *                                                               *
    *    07/04/13 C:282 Support: Redmine #3350                      *
    *                   Added the handling for FastQuote employer.  *
    *                                                               *
    *    06/14/13 C:190 Project WC2193                              *
    *                   Add button "Unbilled Premium".              *
    *                                                               *
    *    04/29/13 C:180 Support WA114 81930                         *
    *                   Include retrospective premium.              *
    *                                                               *
    *    04/23/13 C:279 Support: Redmine #2477                      *
    *                   Enable year drop down for year selection.   *
    *                                                               *
    *    02/11/13 C:191 Support: Redmine #2381 inhouse              *
    *                   Reset WDELP file pointer to div requested   *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwtpolsum  cf   e             workstn extfile('WTPOLSUM')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflBillDiv:rrnBillDiv)
     f                                     Sfile(sflCxHist:rrnCxHist)
     f                                     Sfile(sflEnLctn:rrnEnLoc)
     f                                     Sfile(sflAggPol:rrnAggPol)
     f                                     Sfile(sflSpcPol:rrnSpcPol)
     f                                     Sfile(sflPremium:rrnPrmInfo)
     f                                     Sfile(sflPrmDiv:rrnPrmDiv)
     f                                     Sfile(sflAgencyH:rrnAgtHist)
     f                                     Sfile(sflBDCxHst:rrnCxBDHst)
     f                                     Sfile(sflClmtInf:rrnClmnt)
     f                                     Sfile(sflCurPyrl:rrnPayroll)
     f                                     Sfile(sflPrmHist:rrnPrmHist)
     f                                     Sfile(sflPreRtSt:rrnPrmRtSt)
     f                                     Sfile(sflAggCde:rrnAggCde)
     f                                     Sfile(sflSpcCde:rrnSpcCde)
     f                                     Sfile(sflClSlInf:rrnClInSta)
     f                                     Sfile(sflPrmSlSt:rrnSPyroll)
     f                                     Sfile(sflClmtTb2:rrnClmtTb2)
     f                                     Sfile(sflEntLTb2:rrnTb2ELoc)
     fwtactpnl  cf   e             workstn
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     fwdelp     uf   e             disk    rename(pel01:pel0p)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmupl1    if   e           k disk    rename(pup01:pup01)
     f*** Begin Add *** 06/14/18 *****************************
     f***wmeml     if   e           k disk    rename(pem01:pem01)
     fwmeml     uf   e           k disk    rename(pem01:pem01)
     f*** End   Add *** 06/14/18 *****************************
     fwmeal6    if   e           k disk    rename(pea01:pea06)
     fwmeal10   if   e           k disk    rename(pea01:pea10)
     fwdcxl2    if   e           k disk    rename(pcx01:pcx02)
     f                                     infds(dbfds2)
     fwmall2    if   e           k disk    rename(pal01:pal02)
     f                                     infds(dbfds5)
     fwmril3    if   e           k disk    rename(pri01:pri03)
      *** Begin Change *** 05/21/21 *************************************************
     fwmefp     if   e             disk    rename(pef01:pef06)
     f**wmefl6    if   e           k disk    rename(pef01:pef06)
      *** End   Change *** 05/21/21 *************************************************
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds1)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwdell8    if   e           k disk    rename(pel01:pel08)
     f                                     infds(dbfds4)
     fwdell5    if   e           k disk    rename(pel01:pel05) prefix(a0:2)
     f                                     infds(dbfds8)
     fwdellb    uf   e           k disk    rename(pel01:pel0b)
     fwmcml     if   e           k disk    rename(pcm01:pcm01)
     fwmcmld    if   e           k disk    rename(pcm01:pcm02)
     fwmahl     if   e           k disk    rename(pah01:pah01)
     f                                     infds(dbfds3)
     fwmagl     if   e           k disk    rename(pag01:pag01)
     fwma3l     If   e           k disk    rename(pa301:pa300)
     fwmapl     if   e           k disk    rename(pap01:pap01)
     fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwmarl     if   e           k disk    rename(par01:par01)
     fwtabl     if   e           k disk    include(pb201)
     f                                     include(pbs01)
     fwmepl     if   e           k disk    rename(pep01:pep00)
     f                                     infds(dbfds6)
     fwmepl1    if   e           k disk    rename(pep01:pep01)
     fwmprl1    if   e           k disk    rename(ppr01:ppr01)
     fwmpxl1    if   e           k disk    rename(ppx01:ppx01)
     fwdehl14   if   e           k disk    rename(peh01:peh01)
     f                                     infds(dbfds7)
     fwmrsl     if   e           k disk    rename(prs01:prs00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fststl1    if   e           k disk    rename(pst01:pst01)
     fwxell     if   e           k disk    rename(pel01:pxe00)
     f                                     infds(dbfds9)
     fwdhel     if   e           k disk    Rename(phe01:phe00)
     f*** Begin Add *** 06/14/18 *****************************
     fwdxhl2    if   e           k disk    Rename(pxh01:pxh02)
     f*** End   Add *** 06/14/18 *****************************

      /copy SCOPYPOLPR

     d PSDS          ESDS                  extname(SZQ1P)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     d dbfds5          ds
     d  dbrrn5               397    400b 0
     d dbfds6          ds
     d  dbrrn6               397    400b 0
     d dbfds7          ds
     d  dbrrn7               397    400b 0
     d dbfds8          ds
     d  dbrrn8               397    400b 0
     d dbfds9          ds
     d  dbrrn9               397    400b 0

     d xxRrn           s              9  0
     d inRrn           s              9  0
     d El_RRN          s              9  0
     d inQTE           s              1
     d inRrn1          s              9
     d inMode1         s              1
     d inNext1         s             10
     d inTabInd1       s              5
     d inQTE1          s              1
     d outIND          s              2  0
     d inCO#           s              3  0
     d inFND           s              3  0
     d inEMP#          s              9  0
     d inDIV           s              5  0
     d inFYR           s              3  0
     d INV             s              2  0
     d k1CO#           s              3  0
     d k1FND           s              3  0
     d k1EMP#          s              9  0
     d k1FYR           s              3  0
     d dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D mteMode         s               n   inz(*off)
     D revMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d clientID        s              3    inz('000')
     d process         s               n   inz(*off)
     d mode            s              1
     d @@mode          s              1
     d @@next          s             10
     d xxSt            s              2
     d rateSt          s              2
     d saveFyr         s                   like(ELfyr)
     d saveEffDt       s                   like(ELpfdt)
     d svYear          s                   like(ELfyr)
     d********* Beg Add ******* 01/29/20 ************************
     d svPrvYear       s                   like(ELfyr)
     d********* End Add ******* 01/29/20 ************************
     d kkkey           s                   like(STkey)
     d ##co#           s              3
     d ##fnd           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##odr           s              1
     d ##nodr          s              1
     d ##aodr          s              1
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d x               s              1  0
     d y               s              3  0
     d z               s              3  0
     d x1              s              1  0
     d x2              s              1  0
     d flgPaidBil      s              1
     d tempZip         s              9
     d typeAmount      s             11  2
     d meetss          s              1
     d meetss2         s              1
     d meetss3         s              1
     d meetss4         s              1
     d u1pDsc          s            100
     d u2pDsc          s            100
     d xxAcdtFrmDt     s              7  0
     d xxAcdtToDt      s              7  0
     d xxAcdtFrmDt2    s              7  0
     d xxAcdtToDt2     s              7  0
     d statRptDiv      s                   like(ELdiv)
     d bAmount         s                   like(EAamt)
     d pAmount         s                   like(EAamt)
     d xxEnLcEfDt      s                   like(ELpfdt)
     d ssFDat7         s                   Like(ELpfdt)
     d kkEffdt         s                   Like(ELpfdt)
     d currDate        s                   Like(ELpfdt)
     d effDate         s                   Like(ELpfdt)
     d flgStRpDiv      s              1
     d emrrn           s              9  0
     d kk1Co#          s              3  0
     d kk1Fnd          s              3  0
     d kk1Emp#         s              9  0
     d kk1Div          s              5  0
     d kk1Fyr          s              3  0
     d********* Beg Chg ******* 04/27/18 ************************
     d***xxTotI          s             11  2
     d xxTotI          s             15  2
     d********* End Chg ******* 04/27/18 ************************
     d xxTotC          s              6  0
     d xxResP          s             11  2
     d totPmt          s             11  2
     d totRes          s             11  2
     d prmytd          s             11  2
     d prmcur          s             11  2
     d xxiTot          s             11  2
     d decWrtPrm       s             15  2
     d decEarnd        s             15  2
     d decPaidLos      s             15  2
     d decTotInc       s             15  2
     d decFrqRat       s             15  2
     d decFrqRatB      s             15  2
     d decFrqRatC      s             15  2
     d decFrqRatD      s             15  2
     d decTotincA      s             15  2
     d decTotincB      s             15  2
     d decTotincC      s             15  2
     d decTotincD      s             15  2
     d intClaims#      s              6  0
     d ##zip           s              9  0
     d xxlRat          s              9  2
     d thPrem          s             11  2
     d xxFreq          s              4  2
     d ##amount        s             11
     d ##amount2       s             11
     d ##term          s              1
     d ##fyr           s              3
     d ##date          s              8
     d ##date8         s              8  0
     d ##last          s             56
     d ##frst          s             15
     d xxKey           s             10
     d NameType        s              3
     d x2rrn           s              9  0
     d ##AGrrn         s              9  0
     d ##AArrn         s              9  0
     d xxName          s             40
     d xxMode          s              1
     d xxMad1          s             40
     d xxMZip          s              9  0
     d xxMcty          s             25
     d xxMst           s              2
     d xxPhn#          s              7  0
     d xxArea          s              3  0
     d totPayment      s             11  2
     d totReserve      s             11  2
     d intWritten      s             15  2
     d locFound        s                   like(*in03)
     d intRRN          s              9  0
     d clmRRN          s              9
     d xxType          s              1
     d xxPxSt          s              2
     d xxEfDt          s                   like(ELpfdt)
     d xxIndex         s              3  0
     d AdL1            s             40    inz
     d AdL2            s             40    inz
     d AdL3            s             40    inz
     d intRRN3         s              9  0
     d stateName       s             40
     d stateNamex      s             40
     D str9999Msg      s            256
     d xcRrn           s              9    inz
     d xxNext          s             10    inz
     d xxTab           s              5    inz
     d xxQte           s              1    inz
     d elxRRN          s              9  0
     d isContribution  s               n   inz(*off)
     D outRestrictClm  s              1
     D Restricted      C                   Const('**Restricted**')
     D BtnFilter5      s              1n   inz
     d******** Begin Add ***** 06/14/18 ********************************
     D strPrYearCx     s              1a   inz
     D strCxFnd        s              1a   inz
     d******** End   Add ***** 06/14/18 ********************************
     d********* Beg Add ******* 12/03/19 ************************
     d xxRtSt          s              2
     d********* End Add ******* 12/03/19 ************************
     d*** BEG ADD *** 05/21/21 ****************************************
     d strCo#          S              3a
     d strEftType      S              1a
     d strFnd          S              3a
     d strEftKey       S             30a
     d strDate         S              7a
     d strPre          S              1a   inz
     d strEftRrn       S              9a   inz('000000000')
     d strEobTyp       S              3    inz
     d strCkTp         S             10    inz
     d strFyr          S              3    inz
     d strEnv          S              1    inz
     d intEftRrn       S              9  0 inz
     d EftIsActive     S               N   inz(*Off)
     d*** END ADD *** 05/21/21 ****************************************

     d dsFilter        ds                  likerec(RcdLocFlt)

     d dsFilter2       ds                  likerec(RcdLocFlt2)

     d dsFilter3       ds                  likerec(RcdClm1Flt)

     d dsFilter4       ds                  likerec(RcdClm2Flt)

     d* Proto-types
     d AR_value        pr             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)
     d* Get the LCM factor
     d GetLCM          pr                  extpgm('SRLCM')
     d  chrCompany#                   3
     d  chrGroup#                     3
     d  chrRateCode                   3
     d  chrClassCode                  5
     d  chrEffDate                    7
     d  outLCM                        5
     d  chrCompany#    s              3
     d  chrGroup#      s              3
     d  chrRateCode    s              3
     d  chrClassCode   s              5
     d  chrEffDate     s              7
     d  outLCM         s              5
     d  decLCM         s              5  4

     d* Get the Territorial Differential Factor
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d  outTDF         s              5  4

     d* Get the Original Rate
      //**** Beg Chg **** 06/17/24 *****************************************
     d***GetOrgRate      pr             7  2
     d GetOrgRate      pr             8s 3
      //**** End Chg **** 06/17/24 *****************************************

     d UpdateRecent    pr                  extpgm('STUPDRCNT')
     d   inRRN                        9
     d   inType                       1    const
     d   inQte                        1
     d   inMode                       1    options(*nopass)

     d MidTermEndorse  pr                  extpgm('WTPOLMEND')
     d   inRRN                        9
     d   inMode                       1
     d   inRtnVal                    10

     d RevisedBill     pr                  extpgm('WTPOLRBILL')
     d   inRRN                        9
     d   inMode                       1
     d   inRtnVal                    10
     d   inTab                        5
     d   inQte                        1

     d ConcatAddr      pr           125
     d inAddress1                    40    const
     d inAddress2                    40    const
     d inCity                        25    const
     d inState                        2    const
     d inZipCode                      9  0 const
      *** Begin Delete *** 05/21/21 ************************************
     d** EftIsActive     pr             1n
      *** End   Delete *** 05/21/21 ************************************

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d ClaimSecurity   pr                  ExtPgm('WA067R')
     d inCo#                          3    const
     d inGroup                        3    const
     d inYear                         3    const
     d inCase                         7    const
     d inUser                        10    const
     D outRestrictClm                 1

     d* Initialize the Filter fields(TAB 1)
     d LoadIntFilter   pr

     d LoadIntFilter3  pr

     d* Initialize the Filter fields(TAB 2)
     d LoadIntFilter2  pr

     d LoadIntFilter4  pr

     d* Load Filter Clear Button (TAB 1).
     d LoadClrFltr     pr

     d* Load Filter Clear Button (TAB 2).
     d LoadClrFltr2    pr

     d* Load Selection fields
     d LoadSelection   pr

     d* Load Selected Billing Division
     d LoadBillingDiv  pr

     d* Load State Reporting Division
     d LoadStatRptDiv  pr

     d* Load Deposit - All Billing Divisions / Policy Years
     d  LoadDeposit    pr

     d* Load Deposit - Billing Divisions / Policy Years
     d  LoadDivDepost  pr

     d* Load Premium Information, All Billing Divisions
     d  LoadPremInfo   pr

     d* Load Other Information(
     d  LoadOthInfo    pr

     d* Load Premium Information, Billing Divisions
     d  LoadBPremDiv   pr

     d* Load Billing Divisions
     d  LoadBillDiv    pr

     d* Load Cancellation History, State Reporting Division.
     d  LoadCxHistory  pr

     d* Load Cancellation History, for Billing Division.
     d  LoadCxBilDivH  pr

     d* Load Locations (Other Workplaces) Information(TAB 1)
     d  LoadEnLocat    pr

     d* Load Locations (Other Workplaces) Information(TAB 2)
     d  LoadEnLocT2    pr

     d* Load Reinsurance Policy Information
     d  LoadReinsPol   pr

     d* Load Agency History, Billing Location
     d  LoadAgtHist    pr

     d* Load Claimant information.(TAB 1)
     d  LoadClaimant   pr

     d* Load Claimant information.(TAB 2)
     d  LoadClmntBDv   pr

     d* Load Current Payroll Information
     d LoadPayrollGrd  pr

     d* Load Premium History Information
     d LoadPrmHistGrd  pr

     d* Load Selected state info.
     d  LoadSltStInfo  pr

     d* Main Processing
     d Main            pr

     d MeetFilter      pr

     d MeetFilter2     pr

     d MeetFilter3     pr

     d MeetFilter4     pr

     d* Program Start
     d ProgramStart    pr
     d processMe                       n

     D RcdAllInfoDsp   pr
     D  indValue                      1n   const

     D RcdDivPremDsp   pr
     D  indValue                      1n   const

     D RcdDivSumDsp    pr
     D  indValue                      1n   const

     D RcdPrmInfoDsp   pr
     D  indValue                      1n   const

     D RcdReinsDsp     pr
     D  indValue                      1n   const

     d SRPRT1          pr                  extpgm('SRPRT1')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##fyr                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##date                         8    const
     d ##amount                      11
     d ##amount2                     11
     d ##term                         1

     d SrName          pr                  extpgm('SRNAME')
     d ##last                        56
     d ##frst                        15
     d ##term                         1

     d SrDltChk        pr                  extpgm('W0051R04')
     d ##RRN                          9  0
     d ##QTE                          1
     d ##IND                          2  0

     d SrDltRltFiles   pr                  extpgm('W0051R01')
     d ##CO#                          3  0
     d ##FND                          3  0
     d ##EMP#                         9  0
     d ##DIV                          5  0
     d ##FYR                          3  0
     d ##QTE                          1

     d* Effective Date
     d SrEffd          pr             7  0
     d inCO#                          3  0
     d inFND                          3  0
     d inFYR                          3  0
     d inEMP#                         9  0
     d inDIV                          5  0
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

     d arrState        s              2    DIM(60)
     d arrWritten      s             15  2 DIM(60)
     d arrEarned       s             15  2 DIM(60)
     d arrPaidLos      s             15  2 DIM(60)
     d arrTotInc       s             15  2 DIM(60)
     d arrClaims#      s              6  0 DIM(60)

     d arrMadDs        ds
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d arrMad                              OVERLAY(arrMadds)
     d                                     LIKE(##emn1) DIM(10)


     d scBBillAdrds    ds
     d scBillAdr1
     d scBillAdr2
     d scBillAdr3
     d scBillAdr4
     d scBBillAdr
     d                                     OVERLAY(scBBillAdrds)
     d                                     Like(scBiMadAd1) DIM(4)

     d scRBillAdrds    ds
     d scRptAdr1
     d scRptAdr2
     d scRptAdr3
     d scRptAdr4
     d scRptAdr
     d                                     OVERLAY(scRBillAdrds)
     d                                     Like(scRptAdr1) DIM(4)

     d CompanyDs       ds
     d RIco01                         3s 0
     d RIco02                         3s 0
     d RIco03                         3s 0
     d RIco04                         3s 0
     d RIco05                         3s 0
     d RIco06                         3s 0
     d RIco07                         3s 0
     d RIco08                         3s 0
     d RIco09                         3s 0
     d RIco10                         3s 0
     d RIco11                         3s 0
     d RIco12                         3s 0
     d Company                             OVERLAY(companyDs)
     d                                     LIKE(RIco01) DIM(12)

     d PolicyDs        ds
     d RIpo01                        15
     d RIpo02                        15
     d RIpo03                        15
     d RIpo04                        15
     d RIpo05                        15
     d RIpo06                        15
     d RIpo07                        15
     d RIPO08                        15
     d RIpo09                        15
     d RIpo010                       15
     d RIpo011                       15
     d RIpo012                       15
     d Policy                              OVERLAY(PolicyDs)
     d                                     LIKE(RIpo01) DIM(12)
     d                 DS
     d  Cma                    1     90  2
     d                                     DIM(10)                              PMT AMOUNT
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2

     D                 DS
     D  Cmr                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMrr01                 1      9  2
     D  CMrr02                10     18  2
     D  CMrr03                19     27  2
     D  CMrr04                28     36  2
     D  CMrr05                37     45  2
     D  CMrr06                46     54  2
     D  CMrr07                55     63  2
     D  CMrr08                64     72  2
     D  CMrr09                73     81  2
     D  CMrr10                82     90  2

     d                 ds
     d aggCode                 1     60
     d                                     DIM(30) ASCEND                       PMT CODES
     d scAgPv1C1               1      2
     d scAgPv1C2               3      4
     d scAgPv1C3               5      6
     d scAgPv1C4               7      8
     d scAgPv1C5               9     10
     d scAgPv1C6              11     12
     d scAgPv1C7              13     14
     d scAgPv1C8              15     16
     d scAgPv1C9              17     18
     d scAgPv1C10             19     20
     d scAgPv1C11             21     22
     d scAgPv1C12             23     24
     d scAgPv1C13             25     26
     d scAgPv1C14             27     28
     d scAgPv1C15             29     30
     d scAgPv1C16             31     32
     d scAgPv1C17             33     34
     d scAgPv1C18             35     36
     d scAgPv1C19             37     38
     d scAgPv1C20             39     40
     d scAgPv1C21             41     42
     d scAgPv1C22             43     44
     d scAgPv1C23             45     46
     d scAgPv1C24             47     48
     d scAgPv1C25             49     50
     d scAgPv1C26             51     52
     d scAgPv1C27             53     54
     d scAgPv1C28             55     56
     d scAgPv1C29             57     58
     d scAgPv1C30             59     60


     d                 ds
     d spcCode                 1     60
     d                                     DIM(30) ASCEND                       PMT CODES
     d scSpPv1C1               1      2
     d scSpPv1C2               3      4
     d scSpPv1C3               5      6
     d scSpPv1C4               7      8
     d scSpPv1C5               9     10
     d scSpPv1C6              11     12
     d scSpPv1C7              13     14
     d scSpPv1C8              15     16
     d scSpPv1C9              17     18
     d scSpPv1C10             19     20
     d scSpPv1C11             21     22
     d scSpPv1C12             23     24
     d scSpPv1C13             25     26
     d scSpPv1C14             27     28
     d scSpPv1C15             29     30
     d scSpPv1C16             31     32
     d scSpPv1C17             33     34
     d scSpPv1C18             35     36
     d scSpPv1C19             37     38
     d scSpPv1C20             39     40
     d scSpPv1C21             41     42
     d scSpPv1C22             43     44
     d scSpPv1C23             45     46
     d scSpPv1C24             47     48
     d scSpPv1C25             49     50
     d scSpPv1C26             51     52
     d scSpPv1C27             53     54
     d scSpPv1C28             55     56
     d scSpPv1C29             57     58
     d scSpPv1C30             59     60

     d/copy sPrDate
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrAR
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy sPrString
     d/copy wPrLoc
     d/copy sPrAtrium
     d/copy wPrClaims
     d/copy sPrGenPop
     d/copy wprPolPop
     d/copy sPrLetter
     d/copy wPrPolCpy
      *** Begin Add *** 05/21/21 **************************************
     d/copy wPrBank
      *** End   Add *** 05/21/21 **************************************
    

     c* Employer Master File Key List
     c     keyWMEML      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWMEML2     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#

     c* Employer Master File Key List for state reporting
     c     keyWMEML1     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    statRptDiv

     c* Claims Master File Key List
     c     keyWMCML      Klist
     c                   Kfld                    kk1Co#
     c                   Kfld                    kk1Fnd
     c                   Kfld                    kk1Emp#
     c                   Kfld                    kk1div
     c                   Kfld                    kk1fyr

     c     key1WMCML     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#

     c     key2WMCML     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scdiv

     c     keyWMCMLD     Klist
     c                   Kfld                    kk1Co#
     c                   Kfld                    kk1Fnd
     c                   Kfld                    kk1fyr
     c                   Kfld                    kk1Emp#
     c*** Beg Add *** 12/03/19 *********************************************
     c     keyWMCMLE     Klist
     c                   Kfld                    kk1Co#
     c                   Kfld                    kk1Fnd
     c                   Kfld                    kk1fyr
     c                   Kfld                    kk1Emp#
     c                   Kfld                    kk1Div
     c     keyWMCMLF     Klist
     c                   Kfld                    kk1Co#
     c                   Kfld                    kk1Fnd
     c                   Kfld                    kk1fyr
     c                   Kfld                    kk1Emp#
     c*** End Add *** 12/03/19 *********************************************

     c* Employer Detail File Key List
     c     keyWDELL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWDELL1     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scfyr
     c                   Kfld                    scEmp#

     c     key1WDELL1    Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELfyr
     c                   Kfld                    ELemp#

     c     key2WDELL     Klist
     c                   Kfld                    EMco#
     c                   Kfld                    EMfnd
     c                   Kfld                    EMemp#
     c                   Kfld                    EMdiv

     c     key1WDELL     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFyr

     c     keyWDELL8     Klist
     c                   Kfld                    k1Co#
     c                   Kfld                    k1Fnd
     c                   Kfld                    k1Emp#
     c                   Kfld                    k1Fyr

     c     key1WDELL8    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#

     c     keyWDELLB     Klist
     c                   Kfld                    ElCo#
     c                   Kfld                    ElFnd
     c                   Kfld                    Elfyr
     c                   Kfld                    ELemp#

     c     keyWDELL5     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    statRptDiv
     c*
     c     keyWMEAL6     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   kfld                    scFyr

     c*
     c     key2WMEAL6    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c*
     c     key3WMEAL6    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWMEAL10    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   kfld                    scfyr
     c                   Kfld                    scEmp#

     c* Cancellation detail file
     c     keyWDCXL2     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    statRptDiv

     c     key1WDCXL2    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scdiv

     c* Address Location File
     c     keyWMALL2     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#

     c     key1WMALL2    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c* Reinsurance Policy file
     c     keyWMRIL3     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    kkEffdt

     c* EFT Account file
     c     keyWMEFL6     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     key1WMEFL6    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    statRptDiv

     c* Agent History master file
     c     keyWMAHL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c* Agent master file
     c     keyWMAGL      Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AAfein
     c                   Kfld                    AAmod

     c* Agent Assignment file
     c     keyWMAAL1     Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AHfnd
     c                   Kfld                    AHagt#

     c* Agent Producer file
     c     keyWMAPL      Klist
     c                   Kfld                    AAco#
     c                   Kfld                    AAfein
     c                   Kfld                    AAmod
     c                   Kfld                    AHprod

     c* Agent Producer file
     c     keyWMARL      Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AHfnd
     c                   Kfld                    AHrt#
     c                   Kfld                    effDate

     c     key1WMARL     Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AHfnd
     c                   Kfld                    AHrt#

     c     keyTable      klist
     c                   kfld                    ELco#
     c                   kfld                    xxKey

     c* Employer Payroll File Key List
     c*
     c     keyWMEPL      Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
     c                   Kfld                    scFYR

     c     keyWMEPL1     Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c*** Beg Chg *** 12/03/19 *********************************************
     c****************   Kfld                    ELfyr
     c                   Kfld                    kk1Fyr
     c*** End Chg *** 12/03/19 *********************************************
     c                   Kfld                    scEMP#

     c* Payroll Class File Key List
     c*
     c     keyWMPRL1     Klist
     c                   Kfld                    scCO#
     c                   Kfld                    ELrtcd
     c                   Kfld                    EPclas
     c                   Kfld                    xxEfDt

     C     key1WMPRL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    ELrtcd
     C                   Kfld                    EPclas

     C* Payroll Description File Key List
     C*
     C     keyWMPXL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    EPclas
     C                   Kfld                    EPEXT
     C                   Kfld                    xxPxSt
     C                   Kfld                    EPefdt

     C     key1WMPXL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    EPclas
     C                   Kfld                    EPext
     C                   Kfld                    xxPxSt

     c* Employer Payroll File Key List
     c*
     c     keyWDEHL14    Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFyr

     c     keyWMRSL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    ELrtcd

     c     keySTStL1     Klist
     c                   Kfld                    kkkey

     c     keyWMEXL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd

     c* Entry of Program.
     c*
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@mode                         mode
     c                   Parm                    @@next
     C                   PARM                    @@TabInd          5            tab index
     C                   PARM                    @@Qte             1            quote

      /free
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          xxRrn = 3386;
       Endif;

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
       Endif;

       // To verify the Employer is in the process of being entered through
       // FastQuote.
       If mode <> 'D';
       //***  Begin Chg  ***  03/29/19  ***************************
       //*Chain(n) xxRrn pel0p;
       //*If %Found(wdelp);
          Select;
          When mode = 'Y' or mode = 'P';
             Clear EMco#;
             Clear EMfnd;
             Clear EMemp#;
             Clear EMdiv;
             Chain(n) xxRrn pem0p;
             inCo# = EMco#;
             inFnd = EMfnd;
             inEmp# = EMemp#;
             inDiv = EMdiv;
          Other;
             Clear ELco#;
             Clear ELfnd;
             Clear ELemp#;
             Clear ELdiv;
             Chain(n) xxRrn pel0p;
             inCo# = ELco#;
             inFnd = ELfnd;
             inEmp# = ELemp#;
             inDiv = ELdiv;
          Endsl;
          If %found;
       //***  End   Chg  ***  03/29/19  ***************************
           //***  Begin Chg  ***  12/18/18  ***************************
           ////*** Begin Chg *** 06/14/18 *****************************
           ////Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
           //Chain(n) (ELco#:ELfnd:ELemp#:ELdiv) pem01;
           ////*** End   Chg *** 06/14/18 *****************************
           //
           //If %Found(wmeml);
           //   //If FastQuote Editing flag is set to 'Y' than Employer will be
           //   //in Inquiry Mode.
           //    If EMfqlk = 'Y';
           //      PopOneBtn('This quote is being processed within FastQuote +
           //                 and will open for your review in inquiry. +
           //                 ':'FastQuote Message':'OK');
           //      mode = 'D';
           //   EndIf;
           //EndIf;
             //If FastQuote Editing flag is set to 'Y' than Employer will be
             // in Inquiry Mode.
             If q1prms >= 5;
                quoteMode = @@qte;
             Endif;
           //***  Begin Chg  ***  03/29/19  ***************************
           //If GetFQLockSts(ELco#:ELfnd:ELemp#:ELdiv:quoteMode:*on) = *on;
             If GetFQLockSts(inCo#:inFnd:inEmp#:inDiv:quoteMode:*on) = *on;
           //***  End   Chg  ***  03/29/19  ***************************
              //***  Begin Chg   ***  03/29/19  ***************************
              //mode = 'D';
                Select;
                When mode = 'P';
                When mode = 'Y';
                   mode = 'P';
                Other;
                   mode = 'D';
                Endsl;
              //***  End   Chg  ***  03/29/19  ***************************
             Endif;
           //***  End   Chg  ***  12/18/18  ***************************
          EndIf;
       EndIf;
       Select;
       When mode = 'D';
          dspMode = *On;
       When mode = 'A';
          addMode = *On;
       When mode = 'C';
          chgMode= *On;
       When mode = 'E';
          mteMode = *On;
       When mode = 'R';
          revMode = *On;
       When mode = 'P';
            DoU BtnAddCan = *On;
               AddOkBtn = 'OK';
               addYrMode = *Off;
               ExFmt RcdADtlInf;
               Return;
            EndDo;
       When mode = 'Y';
            DoU BtnAddCan = *On or BtnAddOk = *On;
               AddOkBtn = 'Add Policy Information';
               addYrMode = *On;
               inRRN1 = @@RRN;
               inMode1 = @@mode;
               inNext1 = 'WTPOLQTE';
               inTabInd1 = '00000';
               inQTE1 = @@QTE;
               ExFmt RcdADtlInf;
               If BtnAddOk = *On;
               @@Next  = 'WTPOLQTE';
               Endif;
               Return;
            EndDo;
       Endsl;
       scActMode = mode;

       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 2;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       // Quote Environment
       If q1prms >= 5;
          quoteMode = @@qte;
          Select;
             When @@qte='Q';
                qteMode=*on;
          Endsl;
       Endif;
       scEnv = quoteMode;

       //* Screen Heading
       Select;
          When qteMode = *on;
             scPnlHdg = 'Quote Summary';
          Other;
             scPnlHdg = 'Policy Summary';
       Endsl;

       GetClientID(ClientID);
       scActCltId=ClientID;

       ProgramStart(process);

       If %parms >= 2;
          @@mode = mode;
       Endif;
       If process;
          UpdateRecent(@@RRN:'E':@@Qte:@@mode);
          Main();
       Endif;

       *InLr = *On;
       Return;

      /end-free



    P*====================================================================
    P* AR_value - Determine if A/R amount is positive or negative
    P*====================================================================
    P*
     P AR_value        b
    P*
     d AR_value        PI             9  2
     d ar_Amount                      9  2  const
     d ar_Type                        1     const
     d outPaidBil                     1     options(*nopass)

     d amount          s              9  2

      /free

       Select;
       When ar_Type = 'P' or ar_Type = 'Q' or
        ar_Type = 'I' or ar_Type = 'T';
          amount = ar_Amount * -1;
          If %parms >=3;
             outPaidBil = 'P';
          Endif;
       Other;
          amount = ar_Amount;
          If %parms >=3;
             outPaidBil = 'B';
          Endif;
       Endsl;

       Return amount;

      /end-free
     PAR_value         e



     P*====================================================================
     P* Concatenate addresses into a single field
     P*====================================================================
     PConcatAddr       b
     DConcatAddr       pi           125
     D inAddress1                    40    const
     D inAddress2                    40    const
     D inCity                        25    const
     D inState                        2    const
     D inZipCode                      9  0 const

     D outAddress      s            125
     D outZipCode      s             10
      /Free
       If inAddress1 = *blank;
          Return *blanks;
       Endif;
       outZipCode = %editc(inZipCode:'X');
       Select;
       When %subst(outZipCode:6:4) = '0000';
          outZipCode = %subst(outZipCode:1:5);
       Other;
          outZipCode = %subst(outZipCode:1:5) + '-' + %subst(outZipCode:6:4);
       Endsl;
       outAddress = %trim(inAddress1);
       If inAddress2 <> *blank;
          outAddress = %trim(outAddress) + ', ' + %trim(inAddress2);
       Endif;
       outAddress = %trim(outAddress) + ', ' + %trim(inCity) + ', ' +
        %trim(inState) + '  ' + %trim(outZipCode);
       Return outAddress;
      /End-Free
     PConcatAddr       e


      //*** Begin Delete *** 05/21/21 **********************************
     **P*================================================================
     **P* Check Status
     **P*================================================================
      **P EftIsActive     B
      **D EftIsActive     PI             1n
      **/free
      **
      ** currDate = DateTo7(SystemDate);
      ** If (EFefdt <= currDate  and
      **     (EFexdt > currDate or EFexdt = 0));
      **    Return *on;
      ** Else;
      **    Return *off;
      ** Endif;
      **
      **/End-Free
      **P EftIsActive     e
      //*** End  Delete *** 05/21/21 **********************************



    P*================================================================
    P* GetOrgRate - Get the Original Rate
    P*================================================================
     P GetOrgRate      B
      //**** Beg Chg **** 06/17/24 *****************************************
     D***GetOrgRate      PI             7  2
     D GetOrgRate      PI             8S 3
     d***originalRate    s              7  2
     d originalRate    s              8S 3
      //**** End Chg **** 06/17/24 *****************************************
     d ratingState     s              2
      /free

       // Load payroll rate
       originalRate = PRrt;

       // Apply LCM
       chrCompany#  = %EditC(ELco#:'X');
       chrGroup#    = %EditC(ELfnd:'X');
       chrRateCode  = ELrtCd;
       chrClassCode = %EditC(EPclas:'X');
       chrEffDate   = %EditC(ELpfDt:'X');
       outLCM       = ' ';
       GetLCM(chrCompany#:chrGroup#:chrRateCode:chrClassCode:chrEffDate:outLCM);
       decLCM =%Dec(%Subst(outLCM:1:1) + '.' + %Subst(outLCM:2:4):5:4);
       originalRate = originalRate * decLCM;

       // Apply Harbor Exposure
       ratingState = RatingSt(ELco#:ELrtCd);
       If EPspec = 'F' And PRlhCd = 'N';
          SetGT (ELco#:ELfnd:ratingState:ELpfDt) phe00;
          ReadE (ELco#:ELfnd:ratingState)        phe00;
          If Not(%Eof(wdhel));
             originalRate = originalRate * HElhnf;
          EndIf;
       EndIf;

       Return originalRate;

      /End-Free
     P GetOrgRate      e



       //===================================================================
       // Initilize filter values.(TAB1)
       //===================================================================
     P LoadIntFilter   B
     D LoadIntFilter   PI
      /free

       Clear dsFilter;
       Clear RcdLocFlt;

      /end-free
     P LoadIntFilter   E

       //===================================================================
       // Initilize filter values.(TAB2)
       //===================================================================
     P LoadIntFilter2  B
     D LoadIntFilter2  PI
      /free

       Clear dsFilter2;
       Clear RcdLocFlt2;

      /end-free
     P LoadIntFilter2  E

       //===================================================================
       // Initilize filter values for Claimant Information.(TAB1)
       //===================================================================
     P LoadIntFilter3  B
     D LoadIntFilter3  PI
      /free

       Clear dsFilter3;
       Clear RcdClm1Flt;
       If btnClrFlt3 = *on;
          ssT1ClStat = ' ';
          dsFilter3.ssT1ClStat = ' ';
       Else;
          If ClaimCount(scCo#:scFnd:scEmp#)>9999;
             ssT1ClStat = 'O';
             dsFilter3.ssT1ClStat = 'O';
          Else;
             ssT1ClStat = ' ';
             dsFilter3.ssT1ClStat = ' ';
          EndIf;
       Endif;

      /end-free
     P LoadIntFilter3  E


       //===================================================================
       // Initilize filter values for Claimant Information.(TAB2)
       //===================================================================
     P LoadIntFilter4  B
     D LoadIntFilter4  PI
      /free

       If btnClrFlt4 = *on;
          ssT2ClStat = ' ';
          dsFilter4.ssT2ClStat = ' ';
          Clear dsFilter4;
          Clear RcdClm2Flt;
       Else;
          If ClaimCount(scCo#:scFnd:scEmp#:scDiv)>9999;
             ssT2ClStat = 'O';
             dsFilter4.ssT2ClStat = 'O';
          Else;
             ssT2ClStat = ' ';
             dsFilter4.ssT2ClStat = ' ';
          Endif;
       Endif;

      /end-free
     P LoadIntFilter4  E


       //===================================================================
       // Display/Hide "Clear Filter" button.
       //===================================================================
     P LoadClrFltr     B
     D LoadClrFltr     PI

      /free

       If btnClrFltr = *on;
          LoadIntFilter();
       Endif;

       xxEnLcEfDt = DtCYMD(dsFilter.ssEnLcEfDt);

       If xxEnLcEfDt = *zero and
          dsFilter.ssEnLcNm   = *blanks   and
          dsFilter.ssPhAd1    = *blanks   and
          dsFilter.ssElTp     = *blanks   and
          dsFilter.ssEnLcFein = *zeros    and
          dsFilter.ssEnLcStat = *blanks;
          dspclrfltr = *off;
       Else;
          dspclrfltr = *on;
       Endif;

       If btnClrFlt3 = *on;
          LoadIntFilter3();
          btnClrFlt3 = *off;
       Endif;

       xxAcdtFrmDt = DtCYMD(dsFilter3.ssT1FAccDt);
       xxAcdtToDt  = DtCYMD(dsFilter3.ssT1TAccDt);
       If dsFilter3.ssT1ClStat = ' ' and xxAcdtFrmDt = *zeros
        and xxAcdtToDt  = *zeros;
          dspclrflt3 = *off;
       Else;
          dspclrflt3 = *on;
       Endif;

      /end-free
     P LoadClrFltr     E



       //===================================================================
       // Display/Hide "Clear Filter" button.(TAB 2)
       //===================================================================
     P LoadClrFltr2    B
     D LoadClrFltr2    PI

      /free

       If btnT2ClrFr = *on;
          LoadIntFilter2();
       Endif;

       xxEnLcEfDt = DtCYMD(dsFilter2.ss2EnLEfDt);

       If xxEnLcEfDt = *zero and
          dsFilter2.ss2EnLcNm  = *blanks   and
          dsFilter2.ss2PhAd1   = *blanks   and
          dsFilter2.ss2ElTp     = *blanks   and
          dsFilter2.ss2EnLcFen = *zeros    and
          dsFilter2.ss2EnLStat = *blanks;
          dspT2clrFr = *off;
       Else;
          dspT2clrFr  = *on;
       Endif;

       If btnClrFlt4 = *on or (ssT2ClStat = ' ' and dsFilter4.ssT2ClStat = ' ');
          LoadIntFilter4();
          btnClrFlt4 = *off;
       Endif;

       xxAcdtFrmDt2 = DtCYMD(dsFilter4.ssT2FAccDt);
       xxAcdtToDt2 = DtCYMD(dsFilter4.ssT2TAccDt);
       If dsFilter4.ssT2ClStat = ' ' and xxAcdtFrmDt2 = *zeros
        and xxAcdtToDt2 = *zeros;
          dspclrflt4 = *off;
       Else;
          dspclrflt4 = *on;
       Endif;

      /end-free
     P LoadClrFltr2    E



       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     P LoadSelection   B
     D LoadSelection   PI

     DfyrLine          s             31
     DdivLine          s             49
     Dfirst            s               n   inz(*on)
      /free
       xxSt = RatingSt(ELco#:ELrtcd);

       Clear fyrChoices;
       fyrChoices = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
        + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
        + ' AND ELdiv = ' + %char(ELdiv);

       Clear divChoices;
       divChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

      /end-free
     P LoadSelection   E



       //===================================================================
       // Load Selected Billing Division
       //===================================================================
     P LoadBillingDiv  B
     D LoadBillingDiv  PI

     D inName          s             50
      /free
       Chain(n) keyWMEML PEM01;
       If %found;
          Clear scBillDiv#;
          Clear scBiPhyAd1;
          Clear scBiPhyAd2;
          Clear scBiPhyAd3;
          Clear scBiMadAd1;
          Clear scBiMadAd2;
          Clear scBiMadAd3;
          Clear scBillAdr1;
          Clear scBillAdr2;
          Clear scBillAdr3;
          Clear scBillAdr4;
          Clear scBilRtSt;
          Clear scBilIncDt;
          Clear scBilPolSt;
          Clear scLsCntRp;
          Clear scLsCntLbl;
          Clear scBilCanDt;
          Clear scBilRsn;
          Clear scBilUnWrt;
          Clear scBilPolRp;
          Clear scBiStRId#;
          Clear scBilEftDt;
          Clear scBIntTrDt;

          If EMname = *blanks and %subst(EMnam2:1:10) = *blanks;
             inName = '<No name available>';
          Else;
             inName = %trim(EMname) + %subst(EMnam2:1:10);
          Endif;

          scSlEftUrl = CreateURL('STDRVPGM':inName:@@RRN:'D':'WTPOLLMST':
           '00004':scEnv);

          scBillDiv#= scDiv;

       // Load Physical Address.
          FormatAddress(EMpad1:EMpad2:EMpcty:EMpst:EMpzip:AdL1:AdL2:AdL3);
          scBiPhyAd1 = ADL1;
          scBiPhyAd2 = ADL2;
          scBiPhyAd3 = ADL3;

          If scBiPhyAd1 = *Blanks;
             mapBiPhyIn = *Off;
          Else;
             mapBiPhyIn = *On;
          EndIf;

       // Load Mailing Address.
          FormatAddress(EMmad1:EMmad2:EMmcty:EMmst:EMmzip:AdL1:AdL2:AdL3);
          scBiMadAd1 = ADL1;
          scBiMadAd2 = ADL2;
          scBiMadAd3 = ADL3;

          If scBiMadAd1 = *Blanks;
             mapBiMadIn = *Off;
          Else;
             mapBiMadIn = *On;
          EndIf;

       //* Load Billing Address.
       //* Access third address file
       Chain(n) (scCO#:scFnd:scEmp#:scDiv) pa300;
       If not %found(wma3l);
         clear pa300;
       Else;
         If A3zip1 = *Blanks;
            A3zip1 = *Zeros;
         Endif;
         If A3zip2 = *Blanks;
            A3zip2 = *Zeros;
         Endif;
         ##zip = %Int(A3zip1)*10000 + %Int(A3zip2);
         FormatAddress(A3adr1:A3adr2:A3cty:A3st:##zip:AdL1:AdL2:AdL3);
         scBillAdr1 = A3cont;
         scBillAdr2 = AdL1;
         scBillAdr3 = AdL2;
         scBillAdr4 = AdL3;
         If A3ooc <> ' ';
            scBillAdr3 = A3ooc;
         Endif;
       Endif;

          If scBBillAdrds = *Blanks;
             mapBillIn = *Off;
          Else;
             mapBillIn = *On;
          EndIf;

          scBilRtSt = GetRatingState(EMco#:EMfnd:EMemp#:EMdiv:SCfyr);

          scBilIncDt = dtMDYY(
           GetEmIncDt(scCo#:scFnd:scEmp#:scDiv));

          scBilPolSt = GetQuoteStsDesc(EMco#:EMfnd:EMemp#:EMdiv);

          scBilCanDt= dtMDYY(
           GetCxDate(scCo#:scFnd:scEmp#:scDiv));

          scBilRsn = GetCxRsn(scCo#:scFnd:scEmp#:scDiv:
                      GetCxDate(scCo#:scFnd:scEmp#:scDiv));

          Chain(n) EMuwtr PUP01;
          If %Found;
             scBilUnWrt = UPusnm;
          Endif;

          Chain(n) EMpsrv PUP01;
          If %Found;
             scBilPolRp = UPusnm;
          Endif;

          Chain(n) EMlrep PUP01;
          If %Found;
             scLsCntRp = UPusnm;
          Endif;

          Chain keyWMEXL pex00;
          If EXahlc = *blanks;
             scLsCntLbl = 'Loss Control Rep';
          Else;
             scLsCntLbl = %trim(EXahlc);
          Endif;

          scBiStRId# = EMsemp;

        //*** Begin Change  *** 05/21/21 ***************************************
          dspInTrDt = *Off;
          currDate = DateTo7(SystemDate);
          strEftRrn = '000000000';
          strCo# = %Editc(scCo#:'X');
          strEftType = 'E';
          strFnd = %Editc(scFnd:'X');
          strEftKey = %Editc(scEmp#:'X') + %Editc(scDiv:'X');
          strDate =%Editc(CurrDate:'X');
          strFyr = %Editc(scFyr:'X');
          EftIsActive = GetEftStatus(strCo#:strEftType:strfnd:strEftKey:
                              strDate:strPre:strEftRrn:strEobTyp:strCkTp:
                              strFyr);
          If EftIsActive;
             intEftRrn = %int(strEftRrn);
             chain intEftRrn pef06;
             If %found(wmefp);
                dspInTrDt = *On;
                scBIntTrDt = dtMDYY(EFidt);
                scBilEftDt = dtMDYY(EFefdt);
             Endif;
          Endif;
          //Setgt  keyWMEFL6 PEF06;
          //Readpe keyiWMEFL6 PEF06;
          //If EftIsActive;
          //   dspInTrDt = *On;
          //   scBIntTrDt = dtMDYY(EFidt);
          //Else;
          //   dspInTrDt = *Off;
          //Endif;

          //scBilEftDt = dtMDYY(EFefdt);
        //*** End Change *** 05/21/21 *******************************************

       Endif;
      /end-free
     P LoadBillingDiv  E



       //===================================================================
       // Load State Reporting Division
       //===================================================================
     P LoadStatRptDiv  B
     D LoadStatRptDiv  PI

     D inName          s             50
     D inELrrn         s              9
      /free
       StatRptDiv = GetStRptDv(ELco#:ELfnd:ELEmp#);
       Chain(n) keyWMEML1 PEM01;
       If %found;
          Clear scRptDiv#;
          Clear scRpPhyAd1;
          Clear scRpPhyAd2;
          Clear scRpPhyAd3;
          Clear scRpMadAd1;
          Clear scRpMadAd2;
          Clear scRpMadAd3;
          Clear scRptAdr1;
          Clear scRptAdr2;
          Clear scRptAdr3;
          Clear scRptAdr4;
          Clear scRptRtSt;
          Clear scRptIncDt;
          Clear scRptPolSt;
          Clear scRptCanDt;
          Clear scRptRsn;
          Clear scRptUnWrt;
          Clear scRptPolRp;
          Clear scRpStRId#;
          Clear scRptEFTDt;
          Clear scRIntTrDt;
          Clear scLsCntRps;

          If EMname = *blanks and %subst(EMnam2:1:10) = *blanks;
             inName = '<No name available>';
          Else;
             inName = %trim(EMname) + %subst(EMnam2:1:10);
          Endif;
          Clear scStEftUrl;
          Chain keyWDELL5 pel05;
          If %found(WDELL5);
             inELrrn = %EditC(dbrrn8:'X');
             scStEftUrl = CreateURL('STDRVPGM':inName:inELrrn:'D':'WTPOLLMST':
              '00004':scEnv);
          Endif;

          scRptDiv# = statRptDiv;

       // Load Physical Address.
          FormatAddress(EMpad1:EMpad2:EMpcty:EMpst:EMpzip:AdL1:AdL2:AdL3);
          scRpPhyAd1 = ADL1;
          scRpPhyAd2 = ADL2;
          scRpPhyAd3 = ADL3;

          If scRpPhyAd1 = *Blanks;
             mapRpPhyIn = *Off;
          Else;
             mapRpPhyIn = *On;
          EndIf;

       // Load Mailing Address.
          FormatAddress(EMmad1:EMmad2:EMmcty:EMmst:EMmzip:AdL1:AdL2:AdL3);
          scRpMadAd1 = ADL1;
          scRpMadAd2 = ADL2;
          scRpMadAd3 = ADL3;

          If scRpMadAd1 = *Blanks;
             mapRpMadIn = *Off;
          Else;
             mapRpMadIn = *On;
          EndIf;

       // Load Billing Address.
       //Access third address file
       Chain(n) (scCO#:scFnd:scEmp#:statRptDiv) pa300;
       If not %found(wma3l);
         clear pa300;
       Else;
         If A3zip1 = *Blanks;
            A3zip1 = *Zeros;
         Endif;
         If A3zip2 = *Blanks;
            A3zip2 = *Zeros;
         Endif;
         ##zip = %Int(A3zip1)*10000 + %Int(A3zip2);
         FormatAddress(A3adr1:A3adr2:A3cty:A3st:##zip:AdL1:AdL2:AdL3);
         scRptAdr1 = A3cont;
         scRptAdr2  = AdL1;
         scRptAdr3  = AdL2;
         scRptAdr4  = AdL3;
         If A3ooc <> ' ';
            scRptAdr3 = A3ooc;
         Endif;
       Endif;

          If scRBillAdrds = *Blanks;
             mapRptIn = *Off;
          Else;
             mapRptIn = *On;
          EndIf;

          scRptRtSt = GetRatingState(EMco#:EMfnd:EMemp#:statRptDiv:SCfyr);

          scRptIncDt = dtMDYY(
           GetEmIncDt(scCo#:scFnd:scEmp#:StatRptDiv));

          scRptPolSt = GetQuoteStsDesc(EMco#:EMfnd:EMemp#:statRptDiv);

          scRptCanDt = dtMDYY(
           GetCxDate(EMco#:EMfnd:EMemp#:StatRptDiv));

          scRptRsn   = GetCxRsn(scCo#:scFnd:scEmp#:statRptDiv:
                        GetCxDate(scCo#:scFnd:scEmp#:statRptDiv));

          Chain(n) EMuwtr PUP01;
          If %Found;
             scRptUnWrt = UPusnm;
          Endif;

          Chain(n) EMpsrv PUP01;
          If %Found;
             scRptPolRp = UPusnm;
          Endif;

          Chain(n) EMlrep PUP01;
          If %Found;
             scLsCntRpS = UPusnm;
          Endif;

          scRpStRId# = EMsemp;
        //*** Begin Change  *** 05/21/21 ***************************************
          //Setgt  key1WMEFL6 PEF06;
          //Readpe key1WMEFL6 PEF06;
          //If EftIsActive;
          //   dspInTrDt = *On;
          //   scRIntTrDt = dtMDYY(EFidt);
          //Else;
          //   dspInTrDt = *Off;
          //Endif;

          //scRptEFTDt = dtMDYY(EFefdt);

          dspInTrDt = *Off;
          currDate = DateTo7(SystemDate);
          strEftRrn = '000000000';
          strCo# = %Editc(scCo#:'X');
          strEftType = 'E';
          strFnd = %Editc(scFnd:'X');
          strEftKey = %Editc(scEmp#:'X') + %Editc(statRptDiv:'X');
          strDate =%Editc(CurrDate:'X');
          strFyr = %Editc(scFyr:'X');
          EftIsActive = GetEftStatus(strCo#:strEftType:strfnd:strEftKey:
                              strDate:strPre:strEftRrn:strEobTyp:strCkTp:
                              strFyr);
          If EftIsActive;
             intEftRrn = %int(strEftRrn);
             chain intEftRrn pef06;
             If %found(wmefp);
                dspInTrDt = *On;
                scBIntTrDt = dtMDYY(EFidt);
                scBilEftDt = dtMDYY(EFefdt);
             Endif;
          Endif;
        //*** End  Change  *** 05/21/21 ***************************************

       Endif;
      /end-free
     P LoadStatRptDiv  E



       //===================================================================
       // Deposit - All Billing Divisions
       //===================================================================
     P LoadDeposit     B
     D LoadDeposit     PI
      /free

       Clear scBilled;
       Clear scPaid;
       Clear scBalance;
       Clear typeAmount;

       Setll key2WMEAL6 PEA06;
       Reade key2WMEAL6 PEA06;

       DoW not %eof;
          If EAcTyp='D';
             typeAmount = AR_value(EAamt:EAarty:flgPaidBil);
             If flgPaidBil  = 'B';
                bamount =  typeAmount;
                clear pamount;
             Else;
                pAmount =  typeAmount;
                clear bamount;
             Endif;
          Else;
             clear bamount;
             clear pamount;
          EndIf;

          Reade key2WMEAL6 PEA06;

          Clear typeAmount;
          scPaid = pAmount + scPaid;
          scBilled= bAmount + scBilled;

       Enddo;
       scBalance = (scBilled) - ((scPaid) * -1);

      /end-free
     P LoadDeposit     E



       //===================================================================
       // Deposit - Billing Divisions
       //===================================================================
     P LoadDivDepost   B
     D LoadDivDepost   PI
      /free

       Clear scT2Billed;
       Clear scT2Paid;
       Clear scT2Balnce;
       Clear typeAmount;

       Setll key3WMEAL6 PEA06;
       Reade key3WMEAL6 PEA06;
       DoW not %eof;
          If EAcTyp='D';
             typeAmount = AR_value(EAamt:EAarty:flgPaidBil);
             If flgPaidBil  = 'B';
                bamount =  typeAmount;
                Clear pamount;
             Else;
                pAmount =  typeAmount;
                Clear bamount;
             Endif;
          Else;
             Clear bamount;
             Clear pamount;
          EndIf;

          Reade key3WMEAL6 PEA06;

          Clear typeAmount;
          scT2Billed = bAmount + scT2Billed;
          scT2Paid   = pAmount + scT2Paid;

       Enddo;
       scT2Balnce = (scT2Billed) - ((scT2Paid) * -1);

      /end-free
     P LoadDivDepost   E



       //===================================================================
       // Billing Divisions
       //===================================================================
     P LoadBillDiv     B
     D LoadBillDiv     PI
      /free

       // Clear the subfile.
       RcdAllInfoDsp(*off);
       clrBillDiv = *On;
       Write rcdAllInfo;
       clrBilLDiv = *Off;
       RcdAllInfoDsp(*on);
       rrnBillDiv = 0;

       Setll keyWMEML2 PEM01;
       //*** Begin Chg *** 06/14/18 *****************************
       //Reade keyWMEML2 PEM01;
       Reade(n) keyWMEML2 PEM01;
       //*** End   Chg *** 06/14/18 *****************************

       DoW not %eof;
          Clear sflBillDiv;
          scBilDiv   = EMdiv;
          scBilDbaNm = EMname + %subst(EMnam2:1:10);
          scBilFedNm = EMfdnm + %subst(EMfdn2:1:10);
          scBdIncDt = (CvtToDate(
           GetEmIncDt(EMco#:EMfnd:EMemp#:EMdiv)));
          scCxDate   = (CvtToDate(
           GetCxDate(EMco#:EMfnd:EMemp#:EMdiv)));
          scBilPhyAd = ConcatAddr(EMpad1:EMpad2:EMpcty:
           EMpst:EMpzip);
          scBilMadAd = ConcatAddr(EMmad1:EMmad2:EMmcty:
           EMmst:EMmzip);
          scBilFedId = EMfein;
          scBilRptSt = GetRatingState(EMco#:EMfnd:EMemp#:EMdiv:SCfyr);
          If EMstrp = 'Y';
             scBilStRpD = 'Yes';
          Else;
             scBilStRpD = ' ';
          Endif;

          rrnBillDiv =  rrnBillDiv + 1;
          Write sflBillDiv;

          //*** Begin Chg *** 06/14/18 *****************************
          //Reade keyWMEML2 PEM01;
          Reade(n) keyWMEML2 PEM01;
          //*** End   Chg *** 06/14/18 *****************************

       Enddo;

       rrnBillDiv = 1;

      /end-free
     P LoadBillDiv     E



       //===================================================================
       // Load Premium Information, All Billing Divisions
       //===================================================================
     P LoadPremInfo    B
     D LoadPremInfo    PI
      /free

       scCiCnDt = SystemDate;
       // Clear the subfile.
       RcdAllInfoDsp(*off);
       clrPrmInfo = *On;
       Write rcdAllInfo;
       clrPrmInfo = *Off;
       RcdAllInfoDsp(*on);
       rrnPrmInfo = 0;

       Clear xxTotI;
       Clear xxTotC;
       Clear totPmt;
       Clear totRes;
       Clear prmytd;
       Clear prmcur;
       Clear xxiTot;
       Clear xxlRat;
       Clear thprem;
       Clear xxFreq;
       Clear scDepWrTot;
       Clear scDpErnTot;
       Clear scDpPdTot;
       Clear scDpIncTot;
       Clear scDpClmTot;
       Clear scDepWrtn;
       Clear scDpLosTot;
       Clear scDpFrqTot;
       Clear decTotincD;
       Clear decFrqRatD;
       //*** Beg Add *** 01/29/20 ******************************************
       Clear svPrvYear;
       //*** End Add *** 01/29/20 ******************************************

       Setgt  key1WDELL8 PEL08;
       Readpe key1WDELL8 PEL08;

       svYear = Elfyr;
       Clear sflPremium;
       DoW not %eof(WDELL8);
          If svYear <> ELfyr;
             Clear sflPremium;
          Endif;

       //* Written Premium.
          Select;
             when ELstam = *zero;
                  If ELfaf = 'A';
                     scDepWrtn = ELaapr + scDepWrtn;
                  Else;
                     scDepWrtn = ELanpr + scDepWrtn;
                  Endif;
             when ELstam > *zero;
                  scDepWrtn = ELstam + scDepWrtn;
          Endsl;

       //*** Beg Add *** 01/29/20 ******************************************
          If svPrvYear <> ELfyr;
             svPrvYear = ELfyr;
       //*** End Add *** 01/29/20 ******************************************

       //* Read Claim file - Get Total Claims, Total Incurred, Loss & Freq.
          xxTotI = 0;
          xxTotC = 0;
          xxResP = 0;
          kk1Co#  = ELco#;
          kk1fnd  = ELfnd;
          kk1Emp# = Elemp#;
          kk1div  = ELdiv;
          kk1fyr  = ELfyr;

          Setll keyWMCMLD PCM02;

          Dou %Eof;
             Reade keyWMCMLD PCM02;

             If %Eof;
                Leave;
             Endif;
             xxTotc =  xxTotc + 1;

             totPmt = 0;
             totRes = 0;
             totPmt = %xfoot(Cma);
             totRes = %xfoot(Cmr);
             xxToti = totPmt + totRes + xxToti;
             xxResP = totPmt + xxResP;
          Enddo;
       //*** Beg Add *** 01/29/20 ******************************************
          EndIf;
       //*** End Add *** 01/29/20 ******************************************

       //* Get Premium, Loss ratio, & Frequency ratio for the fund year
       //* Call PGM To calculate Earned Premium year-to-date and Earned premium this period.
          ##co#  = %editc(ELco#:'X');
          ##fnd  = %editc(ELfnd:'X');
          ##fyr  = %editc(ELfyr:'X');
          ##emp# = %editc(ELemp#:'X');
          ##div  = %editc(ELdiv:'X');
          ##date8 = DateTo8(SystemDate);
          ##date  = %editc(##Date8:'X');
          ##amount = ' ';
          ##amount2 = ' ';
          ##term = '1';
          SrPrt1(##co#:##fnd:##fyr:##emp#:##div:##date:##amount:
           ##amount2:##term);
      /end-free
     c                   If        qteMode = *off
     c                   Move      ##amount      prmytd
     c                   Move      ##amount2     prmcur
     c                   Endif

      /free

          scDepYear  = ELfyr;
          scDepEffDt = CvtToDate(Elpfdt);
          scDepExpDt = CvtToDate(Elptdt);
          scDepEarnd = scDepEarnd  + prmytd;
          scDepClms#  = xxTotC;
          scDepTotIn  = xxToti;
          scDepPdLos  = xxResP;

          scPrRRN = dbrrn4;

          Readpe key1WDELL8 PEL08;

          If svYear <> ELfyr;

       //* Calculate Loss Ratio
             If scDepEarnd <> *Zero;
                decTotincA = scDepTotIn * 100;
                If decTotincA <> *Zero;
                   eval(h) scDepLosRt = decTotincA/scDepEarnd;
                Endif;
             Else;
                scDepLosRt = 0;
             Endif;

       //* Calculate Frequency Ratio.
             eval(h) decFrqRat = scDepEarnd/1000;
             If decFrqRat <> *Zero;
                eval(h) scDepFreRt = scDepClms#/decFrqRat;
             Else;
                scDepFreRt = 0;
             Endif;
             If mode <> 'D';
                pdnenv1 = *On;
             Endif;
             rrnPrmInfo =  rrnPrmInfo + 1;
             If rrnPrmInfo = 1;
                scSelPrmIn = 'A';
             Else;
                clear scSelPrmIn;
             Endif;
             Write sflPremium;
             svYear = ELfyr;
             scDepWrTot = scDepWrTot + scDepWrtn;
             scDpErnTot = scDpErnTot + scDepEarnd;
             scDpPdTot  = scDpPdTot  + scDepPdLos;
             scDpIncTot = scDpIncTot + scDepTotIn;
             scDpClmTot = scDpClmTot + scDepClms#;

             Clear sflPremium;
             Clear xxToti;
             Clear totRes;
             Clear prmytd;
             Clear xxlRat;
             Clear xxfreq;
             Clear xxTotC;
             Clear decTotincA;
             Clear decFrqRat;
          Endif;
       Enddo;

       //* For Last record.
       If %EOF and ELfyr <> 0;
       //* Calculate Loss Ratio
          If scDepEarnd <> *Zero;
             decTotincA = scDepTotIn * 100;
             If decTotincA <> *Zero;
                eval(h) scDepLosRt = decTotincA/scDepEarnd;
             Endif;
          Else;
             scDepLosRt = 0;
          Endif;

       //* Calculate Frequency Ratio.
          eval(h) decFrqRat = scDepEarnd/1000;
          If decFrqRat <> *Zero;
             eval(h) scDepFreRt = scDepClms#/decFrqRat;
          Else;
             scDepFreRt = 0;
          Endif;
          If mode <> 'D';
             pdnenv1 = *On;
          Endif;
          rrnPrmInfo =  rrnPrmInfo + 1;
          If rrnPrmInfo = 1;
             scSelPrmIn = 'A';
          Else;
             clear scSelPrmIn;
          Endif;
          Write sflPremium;
          svYear = ELfyr;
          scDepWrTot = scDepWrTot + scDepWrtn;
          scDpErnTot = scDpErnTot + scDepEarnd;
          scDpPdTot  = scDpPdTot  + scDepPdLos;
          scDpIncTot = scDpIncTot + scDepTotIn;
          scDpClmTot = scDpClmTot + scDepClms#;

          Clear sflPremium;
          Clear xxToti;
          Clear totRes;
          Clear prmytd;
          Clear xxlRat;
          Clear xxfreq;
          Clear xxTotC;
          Clear decTotincA;
          Clear decFrqRat;
         Endif;

       //* Calculate Loss Ratio
             If scDpErnTot <> *Zero;
                decTotincD = scDpIncTot * 100;
                If decTotincD <> *Zero;
                   eval(h) scDpLosTot = decTotincD/scDpErnTot;
                Endif;
             Else;
                scDpLosTot = 0;
             Endif;

       //* Calculate Frequency Ratio.
          eval(h) decFrqRatD = scDpErnTot/1000;
          If decFrqRatD <> *Zero;
             eval(h) scDpFrqTot = scDpClmTot/decFrqRatD;
          Else;
             scDpFrqTot = 0;
          Endif;

       If isContribution;
          flexhead1 = 'Contribution Information, All Billing Divisions';
          flexhead2 = 'Contribution Calculation';
          flexhead3 = 'Manual Contribution';
          flexhead4 = 'Standard Contribution';
          flexhead5 = 'Normal Contribution';
          flexhead6 = 'Annual Contribution';
       Else;
          flexhead1 = 'Premium Information, All Billing Divisions';
          flexhead2 = 'Premium Calculation';
          flexhead3 = 'Manual Premium';
          flexhead4 = 'Standard Premium';
          flexhead5 = 'Normal Premium';
          flexhead6 = 'Annual Premium';
       Endif;
         rrnPrmInfo = 1;

      /end-free
     P LoadPremInfo    E



       //===================================================================
       // Load Other Information(Earned, Paid Losses, Tot Incurred, #Claims, Loss ratio, Freq ratio.
       //===================================================================
     P LoadOthInfo     B
     D LoadOthInfo     PI
      /free

       Clear xxTotI;
       Clear xxTotC;
       Clear totPmt;
       Clear totRes;
       Clear prmytd;
       Clear prmcur;

       Clear decWrtPrm;
       Clear decEarnd;
       Clear decPaidLos;
       Clear decTotInc;
       Clear intClaims#;

       //* Written Premium.
       select;
        when ELstam = *zero;
             If ELfaf = 'A';
                decWrtPrm = ELaapr;
             Else;
                decWrtPrm = ELanpr;
             Endif;
        when ELstam > *zero;
             decWrtPrm = ELstam;
       endsl;

       //* Read Claim file - Get Total Claims, Total Incurred, Loss & Freq.
       xxTotI = 0;
       xxTotC = 0;
       xxResP = 0;
       kk1Co# = ELco#;
       kk1fnd = Elfnd;
       kk1Emp# =ELemp#;
       kk1div = ELdiv;
       kk1fyr = Elfyr;

       //*** Beg Chg *** 12/03/19 ******************************************
       //***Setll keyWMCMLD PCM02;
       Setll keyWMCMLE PCM02;
       //*** End Chg *** 12/03/19 ******************************************

       Dou %Eof;
       //*** Beg Chg *** 12/03/19 ******************************************
       //**Reade keyWMCMLD PCM02;
           Reade keyWMCMLE PCM02;
       //*** End Chg *** 12/03/19 ******************************************

           If %Eof;
              Leave;
           Endif;

       //*** Beg Del *** 12/03/19 ******************************************
       //**If RSjrst = CMjrst;
       //*** End Del *** 12/03/19 ******************************************
       //*** Beg Chg *** 12/03/19 ******************************************
       //***  xxTotc = ELt#cc;
              xxTotc = xxTotc + 1;
       //*** End Chg *** 12/03/19 ******************************************
              totPmt = 0;
              totRes = 0;
              totPmt = %xfoot(Cma);
              totRes = %xfoot(Cmr);
              xxToti = totPmt + totRes + xxToti;
              xxResP = totPmt + xxResP;
       //*** Beg Del *** 12/03/19 ******************************************
       //**Endif;
       //*** End Del *** 12/03/19 ******************************************
       Enddo;

       //* Get Premium, Loss ratio, & Frequency ratio for the fund year
       //* Call PGM To calculate Earned Premium year-to-date and Earned premium this period.
          ##co#  = %editc(ELco#:'X');
          ##fnd  = %editc(ELfnd:'X');
          ##fyr  = %editc(ELfyr:'X');
          ##emp# = %editc(ELemp#:'X');
          ##div  = %editc(ELdiv:'X');
          ##date8 = DateTo8(SystemDate);
          ##date  = %editc(##Date8:'X');
          ##amount = ' ';
          ##amount2 = ' ';
          ##term = '1';
          SrPrt1(##co#:##fnd:##fyr:##emp#:##div:##date:##amount:
           ##amount2:##term);
      /end-free
     c                   If        qteMode = *off
     c                   Move      ##amount      prmytd
     c                   Move      ##amount2     prmcur
     c                   Endif

      /free

          decEarnd = decEarnd  + prmytd;
          decPaidLos   = xxResP;
          decTotInc    = xxToti;
          intClaims#   = intClaims#  + xxTotC;

      /end-free
     P LoadOthInfo     E



       //===================================================================
       // Load Premium Information, Billing Divisions
       //===================================================================
     P LoadBPremDiv    B
     D LoadBPremDiv    PI
      /free

       // Clear the subfile.
       RcdDivSumDsp(*off);
       clrPrmDiv  = *On;
       Write rcdDivSum;
       clrPrmDiv  = *Off;
       RcdDivSumDsp(*on);
       rrnPrmDiv = 0;

       Clear xxTotI;
       Clear xxTotC;
       Clear totPmt;
       Clear totRes;
       Clear prmytd;
       Clear prmcur;
       Clear xxiTot;
       Clear xxlRat;
       Clear thprem;
       Clear xxFreq;
       Clear svYear;

       Setgt  key1WDELL PEL00;
       Readpe keyWDELL PEL00;

       DoW not %eof;
          If svYear <> ELfyr;
             Clear sflPrmDiv;
          Endif;

       //* Read Claim file - Get Total Claims, Total Incurred, Loss & Freq.
          xxTotI = 0;
          xxTotC = 0;
          xxResP = 0;
          kk1Co# = scCo#;
          kk1fnd = scFnd;
          kk1Emp# =SCEmp#;
          kk1div = scdiv;
          kk1fyr = ELfyr;

          Setll keyWMCML PCM01;
          Dou %Eof;
             Reade keyWMCML PCM01;
             If %Eof;
                Leave;
             Endif;

             xxTotc = ELt#cc;
             totPmt = 0;
             totRes = 0;
             totPmt = %xfoot(Cma);
             totRes = %xfoot(Cmr);
             xxToti = totPmt + totRes + xxToti;
             xxResP = totPmt + xxResP;
          Enddo;

       //* Get Premium, Loss ratio, & Frequency ratio for the fund year
       //* Call PGM To calculate Earned Premium year-to-date and Earned premium this period.
          ##co#  = %editc(ELco#:'X');
          ##fnd  = %editc(ELfnd:'X');
          ##fyr  = %editc(ELfyr:'X');
          ##emp# = %editc(ELemp#:'X');
          ##div  = %editc(ELdiv:'X');
          ##date8 = DateTo8(SystemDate);
          ##date  = %editc(##Date8:'X');
          ##amount = ' ';
          ##amount2 = ' ';
          ##term = '1';
          SrPrt1(##co#:##fnd:##fyr:##emp#:##div:##date:##amount:
           ##amount2:##term);
      /end-free
     c                   If        qteMode = *off
     c                   Move      ##amount      prmytd
     c                   Move      ##amount2     prmcur
     c                   Endif

      /free

          scT2Year   = ELfyr;
          scT2EffDt  = CvtToDate(Elpfdt);
          scT2ExpDt  = CvtToDate(Elptdt);

          Select;
          when ELstam = *zero;
               If ELfaf = 'A';
                  scT2Writtn = ELaapr;
               Else;
                  scT2Writtn = ELanpr;
               Endif;
          when ELstam > *zero;
               scT2Writtn = ELstam;
          endsl;

          scT2Earned = scT2Earned + prmytd;
          scT2PdLoss  = xxResP;
          scT2TotInc  = xxToti;
          scT2Clms   = scT2Clms + xxTotC;
          //scT2BilSts = GetQuoteStsDesc(ELco#:ELfnd:ELemp#:ELdiv);
          scT2State = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);

          scT2rrn = dbrrn1;
          // Billing Status
          scT2BilSts = GetBillSts(scT2rrn);

          If svYear <> ELfyr;
       //* Calculate Loss Ratio
             If scT2Earned <> *Zero;
                decTotincC = scT2TotInc * 100;
                If decTotincC <> *Zero;
                   eval(h) scT2LosRat = decTotincC/scT2Earned;
                Endif;
             Else;
                scT2LosRat = 0;
             Endif;

       //* Calculate Frequency Ratio.
             eval(h) decFrqRatC = scT2Earned/1000;
             If decFrqRatC <> *Zero;
                eval(h) scT2FrRat = scT2Clms/decFrqRatC;
             Else;
                scT2FrRat = 0;
             Endif;

             If mode <> 'D';
                pdnenv2 = *On;
             Endif;
             rrnPrmDiv =  rrnPrmDiv + 1;
             If rrnPrmDiv = 1;
                scSelPrInf = 'A';
             Else;
                clear scSelPrInf;
             Endif;
             Write sflPrmDiv;
             inRRN = sct2RRN;
             Clear sflPrmDiv;
             Clear xxToti;
             Clear totRes;
             Clear prmytd;
             Clear xxlRat;
             Clear xxfreq;
             Clear xxTotC;
             Clear decTotincC;
             Clear decFrqRatC;
          Endif;
          svYear = ELfyr;
          Readpe keyWDELL PEL00;
       Enddo;

       If isContribution;
          flexhead7 = 'Contribution Information, Billing Division';
          flexhead8 = 'Contribution Calculation';
          flexhead9 = 'Manual Contribution';
          flexhead10= 'Standard Contribution';
          flexhead11= 'Normal Contribution';
          flexhead12= 'Annual Contribution';
       Else;
          flexhead7 = 'Premium Information, Billing Division';
          flexhead8 = 'Premium Calculation';
          flexhead9 = 'Manual Premium';
          flexhead10= 'Standard Premium';
          flexhead11= 'Normal Premium';
          flexhead12= 'Annual Premium';
       Endif;

       rrnPrmDiv = 1;

      /end-free
     P LoadBPremDiv    E



       //===================================================================
       // Cancellation History, State Reporting Division
       //===================================================================
     P LoadCxHistory   B
     D LoadCxHistory   PI
      /free

       // Clear the subfile.
       RcdAllInfoDsp(*off);
       clrCxHist = *On;
       Write rcdAllInfo;
       clrCxHist = *Off;
       RcdAllInfoDsp(*on);
       rrnCxHist = 0;

       Setgt  keyWDCXL2 PCX02;
       Readpe keyWDCXL2 PCX02;

       DoW not %eof;
          Clear sflCxHist;

          scCxYear   = CXfyr;
          scCxReq#   = CXreq#;
          scCxDesc   = %subst(GetCxRsn(CXco#:CXfnd:CXemp#:statRptDiv:
           CXcxdt):1:2) + ' - ' + %subst(GetCxRsn(CXco#:CXfnd:CXemp#:
           statRptDiv:CXcxdt):4:62);
          scCxIssDt  = CvtToDate(CXcdat);
          scCxEffDt  = CvtToDate(CXcxdt);
          scCxOccCnt = CXcnc#;
          If CXerr = 'Y';
             scCxCanErr = 'Yes';
          Else;
             scCxCanErr = ' ';
          Endif;
          If CXcffl = 'Y';
             scCxConfrm = 'Yes';
          Else;
             scCxConfrm = ' ';
          Endif;
          scCxAmtDue = CXamtd;
          scCxRnIsDT = CvtToDate(CXrdat);
          scCxRnEfDt = CvtToDate(CXredt);

          scCxHRRn# = dbrrn2;
          rrnCxHist = rrnCxHist + 1;
          If rrnCxHist = 1;
             scSelCxHst = 'A';
          Else;
             clear scSelCxHst;
          Endif;
          Write sflCxHist;
          Readpe keyWDCXL2 PCX02;

       Enddo;

       rrnCxHist = 1;

      /end-free
     P LoadCxHistory   E



       //===================================================================
       // Claimant Information (TAB 1)
       //===================================================================
     P LoadClaimant    B
     D LoadClaimant    PI
      /free

       // Clear the subfile.
       RcdAllInfoDsp(*off);
       clrClmt = *On;
       Write rcdAllInfo;
       clrClmt = *Off;
       RcdAllInfoDsp(*on);
       rrnClmnt = 0;
       Clear scTotPdLos;
       Clear scTotTotIn;

       Setll key1WMCML PCM01;
       Reade key1WMCML PCM01;

       DoW not %eof;
          Clear sflClmtInf;
          meetss3 = *on;
          MeetFilter3();
          If meetss3 = *on;

             If CMmid <> *Blanks;
                scClmName  = %trim(CMlsn) + ', ' + %trim(CMmid) + ', ' +
                 %trim(CMfsn);
             Else;
                scClmName  = %trim(CMlsn) + ', ' + %trim(CMfsn);
             Endif;

             scClaim#   = %editc(CMFnd:'X') + '-' + %editc(CMfyr:'X') +
                           '-' +  %editc(CMcase:'X');
             scAccDate  =  CvtToDate(CMacdt);
             scClmState = CMjrst;

             Select;
             When CMltfl = 'I';
                  scLossTime = 'Indemnity';
             When CMltfl = 'M';
                  scLossTime = 'Medical';
             When CMltfl = 'R';
                  scLossTime = 'Record only';
             Endsl;


             Select;
             When CMstat = 'O';
                  scClmStat  = 'Open';
             When CMstat = 'C';
                  scClmStat  = 'Closed';
             Other;
                  scClmStat  = 'Undefined';
             Endsl;

             NameType= 'DBA';
             scClmDivNm = GetEmpFullName(CmEmp#:CmDiv:x2RRN:CmFnd:CmCo#:
              NameType);
            //*** Beg Add *** 07/18/25 ******************************************
             scClmDivNm = %EditC(cmDiv:'X') +'-'+ scClmDivNm;
             scClmDivTp = scClmDivNm;
            //*** End Add *** 07/18/25 ******************************************

             locFound=*off;
             locFound = GetLocInfo(CMco#:CMfnd:CMemp#:CMdiv:CMctg1:xxMode:
              xxName:xxMad1:xxMzip:xxMcty:xxMst:xxPhn#:xxArea);
             If locFound=*on;
                scClmLocNm = xxName;
               //*** Beg Add *** 07/18/25 ******************************************
                scClmLocNm = CMctg1 +'-'+ scClmLocNm;
                scClmLocTp = scClmLocNm;
               //*** End Add *** 07/18/25 ******************************************
             Endif;

             totPayment = 0;
             totReserve = 0;
             totPayment = %xfoot(Cma);
             totReserve = %xfoot(Cmr);
             scClmPdLos =  totPayment;
             scClmTotIn =  totReserve + totPayment;

             intRRN = GetClaimRRn(CMco#:CMfnd:CMfyr:CMcase);
             clmRRN= %Editc(intRRN:'X');
             ClaimSecurity(%editc(cmCo#:'X'):%editc(cmFnd:'X'):
                           %editc(cmFYR:'X'):%editc(cmCase:'X'):
                              q1User:outRestrictClm);
             if outRestrictClm <> '2';
               scClmURL = CreateURL('STDRVPGM':scClmName:clmRRN:'D':'WTCLMSUM');
               flgClaim# = *on;
             else;
               scClmName = Restricted;
               scClmURL  = *blanks;
               flgClaim# = *off;
             endif;

             scTotPdLos = scTotPdLos  + scClmPdLos;
             scTotTotIn = scTotTotIn  + scClmTotIn;

             rrnClmnt = rrnClmnt + 1;
             Write sflClmtInf;

             If rrnClmnt >= 9999;
                str9999msg = 'Your filter returned too many records. ' +
                 'Click on Results to see first 9999 records Or, click Filter '+
                 'to narrow down your filter results';
                btnFilter3=PopTwoBtn(str9999msg:'Claims Information, All Bill+
                 ing Division':'Filter':'Results');
                Leave;
             Endif;
          Endif;

          Reade key1WMCML PCM01;

       Enddo;

       rrnClmnt =  1;

      /end-free
     P LoadClaimant    E


       //===================================================================
       // Claims Information, Billing Division(TAB 2)
       //===================================================================
     P LoadClmntBDv    B
     D LoadClmntBDv    PI
      /free

       // Clear the subfile.
       RcdDivSumDsp(*off);
       clrClmtTb2 = *On;
       Write rcdDivSum;
       clrClmtTb2 = *Off;
       RcdDivSumDsp(*on);
       rrnClmtTb2  = 0;
       Clear scTotPdTb2;
       Clear scTotToTb2;

       Setll key2WMCML PCM01;
       Reade key2WMCML PCM01;

       DoW not %eof;
          Clear sflClmtTb2;
          meetss4 = *on;
          MeetFilter4();
          If meetss4 = *on;

             If CMmid <> *Blanks;
                scTb2Name  = %trim(CMlsn) + ', ' + %trim(CMmid) + ', ' +
                 %trim(CMfsn);
             Else;
                scTb2Name  = %trim(CMlsn) + ', ' + %trim(CMfsn);
             Endif;

             scTb2Clm#  = %editc(CMFnd:'X') + '-' + %editc(CMfyr:'X') +
              '-' +  %editc(CMcase:'X');
             scTb2AccDt = CvtToDate(CMacdt);
             scTb2State = CMjrst;

             Select;
             When CMltfl = 'I';
                  scTb2LosTm = 'Indemnity';
             When CMltfl = 'M';
                  scTb2LosTm = 'Medical';
             When CMltfl = 'R';
                  scTb2LosTm = 'Record only';
             Endsl;

             Select;
             When CMstat = 'O';
                  scTb2Stats = 'Open';
             When CMstat = 'C';
                  scTb2Stats = 'Closed';
             Other;
                  scTb2Stats = 'Undefined';
             Endsl;

             NameType= 'DBA';
             scTb2DivNm = GetEmpFullName(CmEmp#:CmDiv:x2RRN:CmFnd:CmCo#:
              NameType);
            //*** Beg Add *** 07/18/25 ******************************************
             scTb2DivNm = %EditC(cmDiv:'X') +'-'+ scTb2DivNm;
             scTb2DivTp = scTb2DivNm;
            //*** End Add *** 07/18/25 ******************************************

             locFound=*off;
             locFound = GetLocInfo(CMco#:CMfnd:CMemp#:CMdiv:CMctg1:xxMode:
              xxName:xxMad1:xxMzip:xxMcty:xxMst:xxPhn#:xxArea);
             If locFound=*on;
                scTb2LocNm = xxName;
               //*** Beg Add *** 07/18/25 ******************************************
                scTb2LocNm = CMctg1 +'-'+ scTb2LocNm;
                scTb2LocTp = scTb2LocNm;
               //*** End Add *** 07/18/25 ******************************************
             Endif;

             totPayment = 0;
             totReserve = 0;
             totPayment = %xfoot(Cma);
             totReserve = %xfoot(Cmr);
             scTb2PdLos =  totPayment;
             scTb2TotIn =  totReserve + totPayment;

             intRRN = GetClaimRRn(CMco#:CMfnd:CMfyr:CMcase);
             clmRRN= %Editc(intRRN:'X');
             ClaimSecurity(%editc(cmCo#:'X'):%editc(cmFnd:'X'):
                           %editc(cmFYR:'X'):%editc(cmCase:'X'):
                              q1User:outRestrictClm);
             if outRestrictClm <> '2';
               scClmURLT2 = CreateURL('STDRVPGM':scTb2Name:clmRRN:'D':
                                      'WTCLMSUM');
               flgTb2Clm# = *on;
             else;
               scTb2Name = Restricted;
               scClmURLT2= *blanks;
               flgTb2Clm#= *off;
             endif;

             scTotPdTb2 = scTotPdTb2  + scTb2PdLos;
             scTotToTb2 = scTotToTb2  + scTb2TotIn;

             rrnClmtTb2 = rrnClmtTb2 + 1;
             Write sflClmtTb2;
             If rrnClmtTb2 >= 9999;
                str9999msg = 'Your filter returned too many records. ' +
                 'Click on Results to see first 9999 records Or, click Filter '+
                 'to narrow down your filter results';
                btnFilter4=PopTwoBtn(str9999msg:'Claims Information, Selected +
                 Billing Division':'Filter':'Results');
                Leave;
             Endif;
          Endif;
          Reade key2WMCML PCM01;

       Enddo;

       rrnClmtTb2 =  1;

      /end-free
     P LoadClmntBDv    E

       //===================================================================
       // Cancellation History, Billing Division.
       //===================================================================
     P LoadCxBilDivH   B
     D LoadCxBilDivH   PI
      /free

       // Clear the subfile.
       RcdDivSumDsp(*off);
       clrCxBDHst = *On;
       Write rcdDivSum;
       clrCxBDHst = *Off;
       RcdDivSumDsp(*on);
       rrnCxBDHst = 0;

       Setgt key1WDCXL2 PCX02;
       Readpe key1WDCXL2 PCX02;

       DoW not %eof;
          Clear sflBDCxHst;

          scT2CxYear = CXfyr;
          scT2CxRsn  = %subst(GetCxRsn(scCo#:scFnd:scEmp#:scDiv:
           CXcxdt):1:2) + ' - ' + %subst(GetCxRsn(scCo#:scFnd:scEmp#:
           scDiv:CXcxdt):4:62);
          scT2CxCxIs = CvtToDate(CXcdat);
          scT2CxEffD = CvtToDate(CXcxdt);
          scT2CxOCnt = CXcnc#;
          If CXerr = 'Y';
             scT2CxErr = 'Yes';
          Else;
             scT2CxErr = ' ';
          Endif;
          If CXcffl = 'Y';
             scT2CxCnfm = 'Yes';
          Else;
             scT2CxCnfm = ' ';
          Endif;
          scT2cxAmtD = CXamtd;
          scT2RnsEff = CvtToDate(CXredt);
          scT2RnsIss = CvtToDate(CXrdat);

          scT2CxRRN = dbrrn2;

          rrnCxBDHst = rrnCxBDHst + 1;
          If rrnCxBDHst = 1;
             scSelCnHst = 'A';
          Else;
             clear scSelCnHst;
          Endif;
          Write sflBDCxHst;
          Readpe key1WDCXL2 PCX02;

       Enddo;

       rrnCxBDHst = 1;

      /end-free
     P LoadCxBilDivH   E



       //===================================================================
       // Locations (Other Workplaces) Information
       //===================================================================
     P LoadEnLocat     B
     D LoadEnLocat     PI
      /free

       // Clear the subfile.
       RcdAllInfoDsp(*off);
       clrEnLoc  = *On;
       Write rcdAllInfo;
       clrEnLoc  = *Off;
       RcdAllInfoDsp(*on);
       rrnEnLoc = 0;

       Setll keyWMALL2 PAL02;
       Reade keyWMALL2 PAL02;

       DoW not %eof;
          Clear sflEnLctn;

          meetss = *on;
          MeetFilter();
          If meetss = *on;
             scEnLoName = ALname;
             scEnLoAddr = ConcatAddr(ALmad1:ALmad2:ALmcty:ALmst:ALmzip);
             scEnLoNum  = ALelid;
             scEnLoFein = ALfein;
             If ALeltp = 'E';
                scEnLoType = 'Entity';
             Else;
                scEnLoType = 'Location';
             Endif;
             scEnLoEfDt = CvtToDate(ALefdt);
             scEnLoExDt = CvtToDate(ALdldt);
             scEnLoDiv  = ALdiv;

             scEnLoRRN#= dbrrn5;
             rrnEnLoc = rrnEnLoc + 1;
             If rrnEnLoc = 1;
                scSelLocIn = 'A';
             Else;
                clear scSelLocIn;
             Endif;
             Write sflEnLctn;
            If rrnEnloc = 9999;
             str9999msg = 'Your filter returned too many records. '    +
                'Click on Results to see first 9999 records Or, click  Filter '+
                'to narrow down your filter results';
             btnFilter5=PopTwoBtn(str9999msg:'Claims Information, Selected +
                                  Billing Division':'Filter':'Results');

             If btnFilter5=*On;
               //Clear the subfile.
               RcdAllInfoDsp(*off);
               clrEnLoc  = *On;
               Write rcdAllInfo;
               clrEnLoc  = *Off;
               RcdAllInfoDsp(*on);
               rrnEnLoc = 0;
             Else;
               Leave;
             Endif;
            Endif;
          Endif;
          Reade keyWMALL2 PAL02;
       Enddo;

       rrnEnLoc = 1;

      /end-free
     P LoadEnLocat     E



       //===================================================================
       // Locations (Other Workplaces) Information(TAB 2)
       //===================================================================
     P LoadEnLocT2     B
     D LoadEnLocT2     PI
      /free

       // Clear the subfile.
       RcdDivSumDsp(*off);
       clrTb2ELoc = *On;
       Write rcdDivSum;
       clrTb2ELoc = *Off;
       RcdDivSumDsp(*on);
       rrnTb2ELoc  = 0;

       Setll key1WMALL2 PAL02;
       Reade key1WMALL2 PAL02;

       DoW not %eof;
          Clear sflEntLTb2;

          meetss2 = *on;
          MeetFilter2();
          If meetss2 = *on;
             scT2EnLoNm  = ALname;
             scT2ELoAdr = ConcatAddr(ALmad1:ALmad2:ALmcty:ALmst:ALmzip);
             scT2ELoNm  = ALelid;
             scT2ELFein = ALfein;
             If ALeltp = 'E';
                scT2ELType = 'Entity';
             Else;
                scT2ELType = 'Location';
             Endif;
             scT2ELEfDt = CvtToDate(ALefdt);
             scT2ELExDt = CvtToDate(ALdldt);
             scT2ELDiv  = ALdiv;

             scT2ELRRN#= dbrrn5;
             rrnTb2ELoc = rrnTb2ELoc + 1;
             If rrnTb2ELoc = 1;
                scSelLcDiv = 'A';
             Else;
                clear scSelLcDiv;
             Endif;
             Write sflEntLTb2;
             If rrnTb2ELoc= 9999;
                Leave;
             Endif;
          Endif;
          Reade key1WMALL2 PAL02;
       Enddo;

       rrnTb2ELoc = 1;

      /end-free
     P LoadEnLocT2     E


       //===================================================================
       // Reinsurance Policy Information
       //===================================================================
     P LoadReinsPol    B
     D LoadReinsPol    PI
      /free

       dspReinLnk = *off;
       Chain(n) key1WDELL PEL00;
       kkEffdt = ELpfdt;
       Clear aggCode;
       Clear spcCode;
       Company = *zeros;
       Chain(n) keyWMRIL3 PRI03;
       If %found;
          dspReinLnk = *on;
          scAggEffDt = dtMDYY(RIefdt);
          scAggPoint = RIattp;
          scAggMinFn = RImnls;
          scAggPrm   = RIpamt;

          If RIaudf = 'Y';
          scAggAudit = 'Yes';
          Else;
          scAggAudit = ' ';
          Endif;

          scAggPerc  = RIipct;

          If RIexpf = 'Y';
          scAggIncld = 'Yes';
          Else;
          scAggIncld = ' ';
          Endif;

          Select;
          When RIexpr = 'P';
             scSpcExp   = 'Pro Rata';
          When RIexpr = 'F';
             scSpcExp   = 'Full Reimbursement';
          Other;
             scSpcExp   = '  ';
          Endsl;
      /end-free
     c                   If        RIASFL = 'A'
     c                   MOVEA     RIuncv        aggCode
     c                   Else
     c                   MOVEA     RIuncv        spcCode
     c                   Endif
      /free

       // Clear the subfile.
             RcdReinsDsp(*off);
             clrAggCde = *On;
             Write RcdReins;
             clrAggCde = *Off;
             RcdReinsDsp(*on);
             rrnAggCde = 0;
             Clear sflAggCde;
          y = 1;
          DoW y <= 30;
             If aggCode(y) <> *blanks;
                scAgPvCode = aggCode(y);
                xxKey = aggCode(y);
              Chain(n) keyTable pb201;
              scAgPv1Des = B2desc;
             Endif;
             y = y + 1;
             rrnAggCde =  rrnAggCde + 1;
             Write sflAggCde;
          Enddo;
          rrnAggCde = 1;

       // Clear the subfile.
             RcdReinsDsp(*off);
             clrSpcCde = *On;
             Write RcdReins;
             clrSpcCde = *Off;
             RcdReinsDsp(*on);
             rrnAggCde = 0;
             Clear sflSpcCde;
          y = 1;
          DoW y <= 30;
             If spcCode(y) <> *blanks;
                scSpPvCode = aggCode(y);
                xxKey = spcCode(y);
                Chain(n) keyTable pb201;
                scSpPv1Des = B2desc;
             Endif;
             y = y + 1;
             rrnSpcCde =  rrnSpcCde + 1;
             Write sflSpcCde;
          Enddo;
          rrnSpcCde = 1;

       // Clear the subfile.
             RcdReinsDsp(*off);
             clrAggPol = *On;
             Write RcdReins;
             clrAggPol = *Off;
             RcdReinsDsp(*on);
             rrnAggPol= 0;
             Clear sflAggPol;

          y = 1;
          DoW y <= 12;
             If Company(y) = *zeros;
                Leave;
             Endif;
             scAggComp = Company(y);
             scAggPol  = Policy(y);
             y = y + 1;
             rrnAggPol =  rrnAggPol + 1;
             Write SflAggPol;
          Enddo;
       Endif;

       rrnAggPol= 1;

      /end-free
     P LoadReinsPol    E



       //===================================================================
       // Agency History, Billing Location
       //===================================================================
     P LoadAgtHist     B
     D LoadAgtHist     PI
      /free

       // Clear the subfile.
       RcdDivSumDsp(*off);
       clrAgtHist = *On;
       Write rcdDivSum;
       clrAgtHist = *Off;
       RcdDivSumDsp(*on);
       rrnAgtHist = 0;

       Setgt  keyWMAHL PAH01;
       Readpe keyWMAHL PAH01;
       DoW not %eof;
          scAgYear  = AHfyr;
          scAgEffDt = CvtToDate(AHefdt);
          scAgtNum# = AHagt#;
          scAgRate  = AHrt#;
          scAgAOR   = AHagrc;
          scAgTStat = GetQuoteStsDesc(AHco#:AHfnd:AHemp#:AHdiv);

          If AHstat = 'A';
             scAgStatus = 'Active';
          Else;
             scAgStatus = 'Inactive';
          Endif;

          scProdNum  = AHprod;
       //* Load Agent name
          Chain(n) keyWMAAL1 PAA01;
          If %found;
             Chain(n) keyWMAGL PAG01;
             If %found;
                scAGName   = AGname;
             Endif;
             Chain(n) KeyWMAPL PAP01;
             If %found;
                If APfsn = ' ';
                   scProName = APlsn;
                Else;
                   ##last = APlsn;
                   ##frst = APfsn;
                   ##term = '1';
                   SrName(##last:##frst:##term);
                   scProName = ##last;
                Endif;
             Endif;

             effDate = DateTo7(SystemDate);
             Setgt  keyWMARL  PAR01;
             Readpe key1WMARL PAR01;
             DoW not %eof and ARstat = 'I';
                 Readpe key1WMARL PAR01;
             Enddo;

             scAgNewP  = ARnwrt;
             scAgRenew = ARrnrt;


          Endif;

          Clear ##kc#;
          Clear ##kl#;
          ##call = '1';
          ##MODE1 = '*';
          ##QTE  = scEnv;
          ##view = '1';
          #@RRN  = GetHAgtRRN(AHagt#:AHco#:AHfnd:AHemp#:AHdiv:AHfyr);
          ##RRN  = %editc(#@RRN:'X');
          scAgtNmURL=CreateURL('W0040RW':scAGName:##kc#:##kl#:##Call:##RRN:
           ##view:##QTE:##Mode1);

       //* Producer URL
          GetAgtName(AHAgt#:##AGrrn:##AArrn:AHfnd:AHco#);
          Clear ##kc#;
          Clear ##kl#;
          ##call = '1';
          ##RRN  = %editc(##AGrrn:'X');
          scPrdNmURL =CreateURL('W0042RW':scProName:##kc#:##kl#:##Call:##RRN);

          scAHistRRN = dbrrn3;

          rrnAgtHist =  rrnAgtHist + 1;
          If rrnAgtHist = 1;
             scSelAgnHs = 'A';
          Else;
             clear scSelAgnHs;
          Endif;
          Write sflAgencyH;

          Readpe keyWMAHL PAH01;
       Enddo;

       rrnAgtHist = 1;

      /end-free
     P LoadAgtHist     E



       //===================================================================
       // Load Current Payroll Information Grid
       //===================================================================
     P LoadPayrollGrd  B
     D LoadPayrollGrd  PI
      /free

       // Clear the subfile.
       RcdDivPremDsp(*off);
       clrPayroll = *On;
       Write rcdDivPrem;
       clrPayroll = *Off;
       RcdDivPremDsp(*on);
       rrnPayroll = 0;

       // Clear payroll totals
       Clear scTotalEst;
       Clear scTotalRep;
       Clear scTotalAud;
       // Get the Territorial Differential Factor
       GetTDF(scCo#:scFnd:scEmp#:scDiv:scFyr:outTDF);
       If outTDF = 0;
          scTDF = 'none';
       Else;
          scTDF = %EditW(outTDF:'0 .    ');
       EndIf;

       // Populate the subfile
       Setll keyWMEPL PEP00;
       Reade keyWMEPL PEP00;

       Dow not %eof;

          If EPstat = 'A';

             scEPEFDT =  CvtToDate(EPefdt);
             scEPCADT =  CvtToDate(EPcadt);
             scEPCLAS =  EPclas;
             scEPSPEC =  EPspec;
             scEPEXT  =  EPext;
             scEPEAPR =  EPeapr;
             scEPSAPR =  EPsapr;
             scEPAAPR =  EPaapr;
             scEPARAT =  EParat;
             scEPdiv  =  EPdiv;
             If EPSTAT = 'A';
                scEPStatus = 'Active';
             Else;
                scEPStatus = 'Inactive';
             Endif;

             Clear scEPDesc;
             xxType = 'P';
             xxEfDt = SrEffd(scCO#:scFND:scFYR:scEMP#:scDIV:EPEFDT:xxType:
              EPclas);
             Chain(n) key1WDELL PEL00;
             scGr3EfDt = dtMDYY(ELpfdt);
             scGr3ExDt = dtMDYY(Elptdt);
             scGr3RtSt = GetRatingState(SCco#:SCfnd:SCemp#:SCdiv:SCfyr);
             Setgt keyWMPRL1 PPR01;
             Readpe key1WMPRL1 PPR01;
             If not %eof;
                scOEPARAT = GetOrgRate();
                scEPDesc = PRdesc;
                scPayHZGR =PRhzgr;
                If EPEXT <> *blanks;
                   Clear xxPxSt;
                   Setgt keyWMPXL1 PPX01;
                   Readpe key1WMPXL1 PPX01;
                   If not %eof;
                      scEPDesc = PXdesc;
                   Else;
                      xxPxSt = xxSt;
                      Setgt keyWMPXL1 PPX01;
                      Readpe key1WMPXL1 PPX01;
                      If not %eof;
                         scEPDesc = PXdesc;
                      Endif;
                   Endif;
                Endif;
             Endif;

             scTotalEst = scTotalEst + EPeapr;
             scTotalRep = scTotalRep + EPsapr;
             scTotalAud = scTotalAud + EPaapr;

             scPayRRN =  dbrrn6;

             rrnPayroll =  rrnPayroll + 1;
             If rrnPayroll = 1;
                scSelCurPy = 'A';
             Else;
                clear scSelCurPy;
             Endif;
             Write sflCurPyrl;

          Endif;

          Reade keyWMEPL PEP00;
       Enddo;
       If isContribution;
          flexhead13= 'Estimated Manual Contribution';
          flexhead14= 'Audited Manual Contribution';
       Else;
          flexhead13= 'Estimated Manual Premium';
          flexhead14= 'Audited Manual Premium';
       Endif;

       rrnPayroll = 1;

      /end-free
     P LoadPayrollGrd  E



       //===================================================================
       // Load Premium History Grid.
       //===================================================================
     P LoadPrmHistGrd  B
     D LoadPrmHistGrd  PI
      /free

       // Clear the subfile.
       RcdDivPremDsp(*off);
       clrPrmHist = *On;
       Write rcdDivPrem;
       clrPrmHist = *Off;
       RcdDivPremDsp(*on);
       rrnPrmHist = 0;
       scPremInfo = *on;
       If isContribution;
          scPremTitl = 'Contribution Information, Billing Division';
       Else;
          scPremTitl = 'Premium Information, Billing Division';
       Endif;
       scELrrn = xxRRN;

       Setll keyWDEHL14 PEH01;
       Reade keyWDEHL14 PEH01;

       DoW not %eof;
          Clear sflPrmHist;

          scEfdt  = CvtToDate(EHefdt);
          scTrDt  = CvtToDate(EHtrdt);
          Select;
          When EHfaf = 'E';
          SCdesc = 'ORIGINAL BILLING';
          When EHfaf = 'R';
          SCdesc = 'REVISED BILLING';
          When EHfaf = 'A';
          SCdesc = 'AUDIT BILLING';
          When EHfaf = 'X';
          SCdesc = 'RETROSPECTIVE BILLING';
          Endsl;

          scPrHisrrn = dbrrn7;
          rrnPrmHist = rrnPrmHist + 1;
          If rrnPrmHist = 1;
             scSelPrmHs = 'A';
          Else;
             clear scSelPrmHs;
          Endif;
          //*** Beg Add **** 02/27/24 ****************************
          If EHWASTD = 0 and EHWSPR <> 0 and EHWMNP = 'Y';
             scInd = ' ';
          Else;
             scInd = 'Y';
          Endif;
          //*** End Add **** 02/27/24 ****************************

          Write sflPrmHist;

          scPremInfo = *off;
          scPremTitl = 'Premium History, Billing Division';

          Reade keyWDEHL14 PEH01;
       Enddo;

       If isContribution;
          flexhead15 = 'Written Contribution';
          flexhead16 = 'Inforce Contribution';
          flexhead19 = 'Unbilled Contribution';
       Else;
          flexhead15 = 'Written Premium';
          flexhead16 = 'Inforce Premium';
          flexhead19 = 'Unbilled Premium';
       Endif;
       rrnPrmHist = 1;

      /end-free
     P LoadPrmHistGrd  E


       //===================================================================
       // Load Selected State Information.
       //===================================================================
     P LoadSltStInfo   B
     D LoadSltStInfo   PI
      /free

       Clear arrState;
       Clear arrWritten;
       Clear arrEarned;
       Clear arrPaidLos;
       Clear arrTotInc;
       Clear arrClaims#;
       Clear scBotWrtTo;
       Clear scBotEarnd;
       Clear scBotPdLos;
       Clear scBotTotIn;
       Clear scBotClms#;
       Clear scBotLosRa;
       Clear scBotFrqRa;
       z = 0;
       If rrnPrRtRow = 0;
          rrnPrRtRow = 1;
       Endif;
       If isContribution;
          flexhead17= 'Contribution Information';
          flexhead18= 'Contribution Information all States';
       Else;
          flexhead17= 'Premium Information';
          flexhead18= 'Premium Information all States';
       Endif;

       Chain(n) scPrRRN PEL0P;
       Setll key1WDELL1 PEL01;
       Reade key1WDELL1 PEL01;
       DoW not %eof;
          Chain(n) keyWMRSL PRS00;
          If %found;
             xxIndex = %Lookup(rsjrst : arrState);
             If xxIndex = 0;
                z = z + 1;
                arrState(z) = rsjrst;
                LoadOthInfo();
                arrWritten(z)  = decWrtPrm;
                arrEarned(z)   = decEarnd;
                arrPaidLos(z)  = decPaidLos;
                arrTotInc(z)   = decTotInc;
                arrClaims#(z)  = intClaims#;
             Else;
                LoadOthInfo();
                arrWritten(xxIndex)  = arrWritten(xxIndex) + decWrtPrm;
                arrEarned(xxIndex)   = arrEarned(xxIndex)  + decEarnd;
                arrPaidLos(xxIndex)  = arrPaidLos(xxIndex) + decPaidLos;
                arrTotInc(xxIndex)   = arrTotInc(xxIndex)  + decTotInc;
                arrClaims#(xxIndex)  = arrClaims#(xxIndex) + intClaims#;
             Endif;
          Endif;
          Reade key1WDELL1 PEL01;
       Enddo;

       // Populate the subfile
       //* Clear the subfile.
       RcdPrmInfoDsp(*off);
       clrPrRtSt = *On;
       Write rcdPrmInfo;
       clrPrRtSt  = *Off;
       RcdPrmInfoDsp(*on);
       rrnPrmRtSt = 0;
       z = 1;
       DoW y <= 60;
          Clear sflPreRtSt;
          If arrState(z) = ' ';
             Leave;
          Endif;

          kkkey = arrstate(z);
          Chain(n) keySTSTL1 PST01;
          scRtState  = STname;
          scRtSt     = arrstate(z);
          scRtTotWrt = arrWritten(z);
          scRtEarned = arrEarned(z);
          scRtPdLoss = arrPaidLos(z);
          scRtTotInc = arrTotInc(z);
          scRtClms#  = arrClaims#(z);

          If arrEarned(z) <> *Zeros;
             decTotincB = arrTotInc(z) * 100;
             If decTotincB <> *Zero;
                   eval(h) scRtLosRat = decTotincB/arrEarned(Z);
             Else;
                scRtLosRat = *Zeros;
             Endif;
          Endif;

          eval(h) decFrqRatB = arrEarned(Z)/1000;
          If decFrqRatB <> *Zero;
             eval(h) scRtFrq = scRtClms#/decFrqRatB;
          Else;
             scRtFrq = 0;
          Endif;

          scBotWrtTo = scBotWrtTo + scRtTotWrt;
          scBotEarnd = scBotEarnd + scRtEarned;
          scBotPdLos = scBotPdLos + scRtPdLoss;
          scBotTotIn = scBotTotIn + scRtTotInc;
          scBotClms# = scBotClms# + scRtClms#;
          //*** Beg Del *** 12/03/19 ***************************************
          //***scBotLosRa = scBotLosRa + scRtLosRat;
          //***scBotFrqRa = scBotFrqRa + scBotFrqRa;
          //*** End Del *** 12/03/19 ***************************************

          z = z + 1;
          rrnPrmRtSt = rrnPrmRtSt + 1;
          If (intRrn3 = 0 and rrnPrmRtSt = 1) or
           (intRrn3 > 0  and intRrn3 = rrnPrmRtSt);
             scRnwRow = 'A';
          Else;
             clear scRnwRow;
          Endif;
          Write sflPreRtSt;
       Enddo;
       //*** Beg Add *** 12/03/19 ******************************************
       //*** begin chg ***  05/13/20  **************************************
       //*****   scBotLosRa = (scBotTotIn * 100)/scBotEarnd;
          scBotLosRa = 0;
          If scBotEarnd <> 0;
             scBotLosRa = (scBotTotIn * 100)/scBotEarnd;
          Endif;
       //*** end chg *****  05/13/20  **************************************
          scBotFrqRa = scDepFreRt;
       //*** End Add *** 12/03/19 ******************************************
       If intRrn3 > 0;
          rrnPrmRtSt = intRrn3;
       Else;
       rrnPrmRtSt = 1;
       Endif;


       //* Populate Claim Information for state selected.
       If rrnPrRtRow <> 0;
          RcdPrmInfoDsp(*off);
          clrClInSta = *On;
          Write rcdPrmInfo;
          clrClInSta = *Off;
          RcdPrmInfoDsp(*on);
          rrnClInSta = 0;

       // Clear totals
          Clear scPTotPdLs;
          Clear scPTotTtIn;

       //*** Beg Chg *** 12/03/19 ******************************************
       //***   Setll key1WMCML PCM01;
       //***   Reade key1WMCML PCM01;
          Setll keyWMCMLF PCM02;
          Reade keyWMCMLF PCM02;
       //*** End Chg *** 12/03/19 ******************************************

          DoW not %eof;
             Clear sflClSlInf;
             Readc sflPreRtSt;
             Chain rrnPrRtRow sflPreRtSt;
       //*** Beg Chg *** 12/03/19 ******************************************
       //*** If scRtSt = CMjrst;
             Chain(N) (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
             xxRtSt = RatingSt(ELco#:ELrtcd);
             If scRtSt = xxRtSt;
       //*** End Chg *** 12/03/19 ******************************************

                If CMmid <> *Blanks;
                   scPClmName = %trim(CMlsn) + ', ' + %trim(CMmid) + ', ' +
                    %trim(CMfsn);
                Else;
                   scPClmName = %trim(CMlsn) + ', ' + %trim(CMfsn);
                Endif;

                scPClaim#  = %editc(CMFnd:'X') + '-' + %editc(CMfyr:'X') +
                              '-' +  %editc(CMcase:'X');
                scPAccDate = dtMDYY(CMacdt);
                scPClmStat = CMjrst;

                Select;
                When CMltfl = 'I';
                   scPLossTim = 'Indemnity';
                When CMltfl = 'M';
                   scPLossTim = 'Medical';
                When CMltfl = 'R';
                   scPLossTim = 'Record only';
                Endsl;

                Select;
               When CMstat = 'O';
                  scPClmSts  = 'Open';
               When CMstat = 'C';
                  scPClmSts  = 'Closed';
               Other;
                  scPClmSts  = 'Undefined';
               Endsl;

               NameType= 'DBA';
               scPClmDvNm = GetEmpFullName(CmEmp#:CmDiv:x2RRN:CmFnd:CmCo#:
                NameType);
               // xxNmType);

               locFound=*off;
               locFound = GetLocInfo(CMco#:CMfnd:CMemp#:CMdiv:CMctg1:xxMode:
                           xxName:xxMad1:xxMzip:xxMcty:xxMst:xxPhn#:xxArea);
               If locFound=*on;
                 scPClmLcNm = xxName;
               Endif;

               totPayment = 0;
               totReserve = 0;
               totPayment = %xfoot(Cma);
               totReserve = %xfoot(Cmr);
               scPClmPdLs =  totPayment;
               scPClTotIn =  totReserve + totPayment;

               intRRN = GetClaimRRn(CMco#:CMfnd:CMfyr:CMcase);
               clmRRN= %Editc(intRRN:'X');
               ClaimSecurity(%editc(cmCo#:'X'):%editc(cmFnd:'X'):
                             %editc(cmFYR:'X'):%editc(cmCase:'X'):
                                q1User:outRestrictClm);
               if outRestrictClm <> '2';
                 scPClmURL  = CreateURL('STDRVPGM':scPClmName:clmRRN:'D':
                  'WTCLMSUM');
                 flgPClaim# = *on;
               else;
                 scPClmName = Restricted;
                 scPClmURL  = *blanks;
                 flgPClaim# = *off;
               endif;

               scPTotPdLs = scPTotPdLs  + scPClmPdLs;
               scPTotTtIn = scPTotTtIn  + scPClTotIn;

               rrnClInSta = rrnClInSta + 1;
               Write sflClSlInf;
             Endif;
       //*** Beg Chg *** 12/03/19 ******************************************
       //*** Reade key1WMCML PCM01;
             Reade keyWMCMLF PCM02;
       //*** End Chg *** 12/03/19 ******************************************

          Enddo;
       Endif;

       rrnClInSta = 1;



       //* Populate Premium information for state selected.
       If rrnPrRtRow <> 0;
          RcdPrmInfoDsp(*off);
          clrSPyroll = *On;
          Write RcdPrmInfo;
          clrSPyroll = *Off;
          RcdPrmInfoDsp(*on);
          rrnSPyroll = 0;

       // Clear payroll totals
          Clear scSSTotEst;
          Clear scSSTotRep;
          Clear scSSTotAud;
          Clear scSltState;
          Clear scSltClmIn;


       // Populate the subfile
          Setll keyWMEPL1 PEP01;
          Reade keyWMEPL1 PEP01;

          Dow not %eof;
             Clear sflPrmSlSt;
             Readc sflPreRtSt;
             Chain rrnPrRtRow sflPreRtSt;
             kkkey = scRtSt;
             Chain(n) keySTSTL1 PST01;
             Clear stateNamex;
             stateNamex =  STname;
             stateName  = TitleCase(stateNamex);
             If isContribution;
                scSltState = 'Contribution Information for' + ' ' + stateName;
             Else;
                scSltState = 'Premium Information for' + ' ' + stateName;
             Endif;
             scSltClmIn = 'Claims Information for' + ' ' + stateName;
             rateSt = GetRatingState(EPco#:EPfnd:EPemp#:EPdiv:EPfyr);
             If scRtSt = rateSt;
                If EPstat = 'A';
                   scSSEFDT =  dtMDYY(EPefdt);
                   scSSCADT =  dtMDYY(EPcadt);
                   scSSCLAS =  EPclas;
                   scSSSPEC =  EPspec;
                   scSSEXT  =  EPext;
                   scSSEAPR =  EPeapr;
                   scSSSAPR =  EPsapr;
                   scSSAAPR =  EPaapr;
                   scSSARAT =  EParat;
                   scSSDiv  =  EPdiv;
                   If EPSTAT = 'A';
                      scSSStatus = 'Active';
                   Else;
                      scSSStatus = 'Inactive';
                   Endif;

                   // Reset WDELP file pointer back to div concerned
                   // This will give us correct rating state ETRTCD
                   Chain(n) (EPco#:EPfnd:EPfyr:EPemp#:EPdiv) pel01;
                   Clear scSSDesc;
                   xxType = 'P';
                   xxEfDt = SrEffd(scCO#:scFND:scFYR:scEMP#:EPdiv:EPEFDT:xxType:
                    EPclas);
                   Setgt keyWMPRL1 PPR01;
                   Readpe key1WMPRL1 PPR01;
                   If not %eof;
                      scSSDesc = PRdesc;
                      If EPEXT <> *blanks;
                         Clear xxPxSt;
                         Setgt keyWMPXL1 PPX01;
                         Readpe key1WMPXL1 PPX01;
                         If not %eof;
                            scSSDesc = PXdesc;
                         Else;
                            xxPxSt = xxSt;
                            Setgt keyWMPXL1 PPX01;
                            Readpe key1WMPXL1 PPX01;
                            If not %eof;
                               scSSDesc = PXdesc;
                            Endif;
                         Endif;
                      Endif;
                   Endif;

                   scSSTotEst = scSSTotEst + EPeapr;
                   scSSTotRep = scSSTotRep + EPsapr;
                   scSSTotAud = scSSTotAud + EPaapr;

                   rrnSPyroll =  rrnSPyroll + 1;
                   Write sflPrmSlSt;

                Endif;
             Endif;

             Reade keyWMEPL1 PEP01;
          Enddo;
       Endif;

       rrnSPyroll = 1;


      /end-free
     P LoadSltStInfo   E

       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     D Main            PI

     D error           s               n   inz(*off)
     D message         s            100

      /free

       //*  Load action bar
       LoadActionBar();

         dltTab1Vsb = *Off;
         disYrDivT1 = *Off;
       If isContribution;
          TbNmRcdPnl ='All Billing Divisions and Policy Years ,+
           Selected Billing Division Summary ,+
           Selected Billing Division Contribution Information';
       Else;
          TbNmRcdPnl ='All Billing Divisions and Policy Years ,+
           Selected Billing Division Summary ,+
           Selected Billing Division Premium Information';
       Endif;

       //*  Process until exit button is pressed.

       DoU ExitNow = *On;

       //* Check if new division or policy year has been selected.
       If kkFyr <> scFyr or kkDiv <> scDiv;
          scFyr = kkFyr;
          scDiv = kkDiv;
          Chain(n) key1WDELL PEL00;
          If not %found;
             Setgt keyWDELL PEL00;
             Readpe(n) keyWDELL PEL00;
             @@rrn = %EditC(dbrrn1:'X');
             scFyr = ELfyr;
             kkFyr = ELfyr;
             kkDiv = ELdiv;
          Else;
             @@rrn = %EditC(dbrrn1:'X');
          Endif;
             xxRRN = dbrrn1;
          //*  Load action bar
          LoadActionBar();
          //*  Load selection
          LoadSelection();
       Endif;

       If btnShowWin = *on;
          //*** Beg chg ***** 04/17/18****************************************
          //Select;
          //When txtShowWin = 'Certificate Holders' and mode = 'C';
          //   PopupWindow(txtShowWin:scActRRN:'C ':txtWinData);
          //When txtShowWin = 'Certificate Holders' and mode = 'D';
          //   PopupWindow(txtShowWin:scActRRN:'D ':txtWinData);
          //Other;
          //   PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
          //EndSl;
          PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
          //*** End chg ***** 04/17/18****************************************
          btnShowWin = *off;
       Endif;


       //*** Beg Del *** 01/29/20 ******************************************
       // Move all the below code to ExfmtScreens subroutine
       // Load each of these SFL Grids only when requested

       //* Load Filter Clear Button (TAB 1)
       //*** LoadClrFltr();

       //* Load Filter Clear Button (TAB2).
       //*** LoadClrFltr2();

       //* Load Selected Billing Division(TAB 1)
       //*** LoadBillingDiv();

       //* Load State Reporting Division(TAB 1)
       //*** LoadStatRptDiv();

       //* Load Deposit - All Billing Divisions / Policy Years(TAB 1)
       //*** LoadDeposit();

       //* Load Deposit - Billing Divisions / Policy Years(TAB 1)
       //*** LoadDivDepost();

       //* Load Premium Information, All Billing Divisions(TAB 1)
       //*** LoadPremInfo();

       //* Load Billing Divisions.
       //*** LoadBillDiv();

       //* Load Cancellation History, State Reporting Division(TAB 1)
       //*** LoadCxHistory();

       //* Load Locations (Other Workplaces) Information. TAB1
       //*** LoadEnLocat();

       //* Load Locations (Other Workplaces) Information. TAB2
       //*** LoadEnLocT2();

       //* Load Claimant information.(TAB1)
       //*** LoadClaimant();

       //* Load Claimant information.
       //*** If btnFilter3 <> *on;
       //***    LoadClmntBDv();
       //*** Endif;

       //* Load Premium Information, Billing Divisions(TAB 2)
       //*** LoadBPremDiv();

       //* Load Agency History, Billing Location(TAB 2)
       //*** LoadAgtHist();

       //* Load Cancellation History, Billing Division(TAB 2)
       //*** LoadCxBilDivH();

       // Load Current Payroll Information(TAB 3)
       //*** LoadPayrollGrd();

       // Load Premium History(TAB 3)
       //*** LoadPrmHistGrd();
       //*** End Del *** 01/29/20 ******************************************


       //* Load Reinsurance Policy Information
         LoadReinsPol();

       //****** Beg Chg ******* 06/14/13 ************************
       // Hide/Show Unbilled Premium button
       Chain (scCO#:scFND:scEMP#:scDIV:scFYR) pxe00;
       If %found;
          unbillVis = *On;
       Else;
          unbillVis = *Off;
       Endif;
       //****** End Chg ******* 06/14/13 ************************

       //*  Process policy summary screen.
         Select;
         When mteMode = *on;
           Exsr PrcMteMode;
         When revMode = *on;
           Exsr PrcRevMode;
         Other;
           If btnFilter3 <> *on and btnFilter4 <> *on;
              Exsr ExfmtScreens;
           Endif;
         EndSl;

       //* Do nothing - Catch tab click event
         If tabClick=*On;
            tabClick = *Off;
         Endif;

       //* Filter Processing(TAB1).
         If btnFilter = *on;
            DoU BtnEnLAppf = *On or BtnEnLCan = *On;
             //LoadSelection();
              //***** Beg Add ****** 06/22/21 *************************
               scPF1rrn = dbRrn1;
              //***** End Add ****** 06/22/21 *************************
               ExFmt RcdLocFlt;
               If BtnEnLAppf = *On;
                  dsFilter.ssEnLcNm   = ssEnLcNm;
                  dsFilter.ssPhAd1    = ssPhAd1;
                  dsFilter.ssElTp     = ssElTp;
                  dsFilter.ssEnLcFein = ssEnLcFein;
                  dsFilter.ssEnLcEfDt = ssEnLcEfDt;
                  dsFilter.ssEnLcStat = ssEnLcStat;
               Endif;
               If BtnEnLCan  = *On;
                  ssEnLcNm   = dsFilter.ssEnLcNm;
                  ssPhAd1    = dsFilter.ssPhAd1;
                  ssElTp     = dsFilter.ssElTp;
                  ssEnLcFein = dsFilter.ssEnLcFein;
                  ssEnLcEfDt = dsFilter.ssEnLcEfDt;
                  ssEnLcStat = dsFilter.ssEnLcStat;
               Endif;
            EndDo;
         Endif;

         If btnFilter3 = *on;
            btnFilter3 = *off;
            DoU Btn1ClAppf = *On or Btn1ClmCan = *On;
               Dou error = *off or Btn1ClmCan = *on;
                  //***** Beg Add ****** 06/22/21 *************************
                   scPF1rrn = dbRrn1;
                  //***** End Add ****** 06/22/21 *************************
                  ExFmt RcdClm1Flt;
                  Exsr CheckFlt3;
               Enddo;
               If Btn1ClAppf = *On;
                  dsFilter3.ssT1ClStat = ssT1ClStat;
                  dsFilter3.ssT1FAccDt = ssT1FAccDt;
                  dsFilter3.ssT1TAccDt = ssT1TAccDt;
               Endif;
               If Btn1ClmCan = *On;
                  ssT1ClStat = dsFilter3.ssT1ClStat;
                  ssT1FAccDt = dsFilter3.ssT1FAccDt;
                  ssT1TAccDt = dsFilter3.ssT1TAccDt;
               Endif;
            EndDo;
         Endif;

       //* Filter Processing(TAB2).
         If btnT2Fltr = *on;
            DoU Btn2ELAppf = *On or Btn2EnLCan= *On;
              //***** Beg Add ****** 06/22/21 *************************
                Chain(n) key1WDELL PEL00;
                scPFrrn = dbRrn1;
              //***** End Add ****** 06/22/21 *************************
               ExFmt RcdLocFlt2;
               If Btn2ELAppf = *On;
                  dsFilter2.ss2EnLcNm  = ss2EnLcNm;
                  dsFilter2.ss2PhAd1   = ss2PhAd1;
                  dsFilter2.ss2ElTp    = ss2ElTp;
                  dsFilter2.ss2EnLcFen = ss2EnLcFen;
                  dsFilter2.ss2EnLEfDt = ss2EnLEfDt;
                  dsFilter2.ss2EnLStat = ss2EnLStat;
               Endif;
               If Btn2EnLCan = *On;
                  ss2EnLcNm  = dsFilter2.ss2EnLcNm;
                  ss2PhAd1   = dsFilter2.ss2PhAd1;
                  ss2ElTp    = dsFilter2.ss2ElTp;
                  ss2EnLcFen = dsFilter2.ss2EnLcFen;
                  ss2EnLEfDt = dsFilter2.ss2EnLEfDt;
                  ss2EnLStat = dsFilter2.ss2EnLStat;
               Endif;
            EndDo;
         Endif;

         If btnFilter4 = *on;
            btnFilter4 = *off;
            DoU Btn2ClAppf = *On or Btn2ClmCan = *On;
               Dou error = *off or Btn2ClmCan = *on;
                //***** Beg Add ****** 06/22/21 *************************
                 scPF1rrn = dbRrn1;
                //***** End Add ****** 06/22/21 *************************
                  ExFmt RcdClm2Flt;
                  Exsr CheckFlt4;
               Enddo;
               If Btn2ClAppf = *On;
                  dsFilter4.ssT2ClStat = ssT2ClStat;
                  dsFilter4.ssT2FAccDt = ssT2FAccDt;
                  dsFilter4.ssT2TAccDt = ssT2TAccDt;
               Endif;
               If Btn2ClmCan = *On;
                  ssT2ClStat = dsFilter4.ssT2ClStat;
                  ssT2FAccDt = dsFilter4.ssT2FAccDt;
                  ssT2TAccDt = dsFilter4.ssT2TAccDt;
               Endif;
            EndDo;
         Endif;

       //* Reinsurance Policy Information Link.
         If lnkReins = *on;
            DoU BtnRenCan = *On;
               //***** Beg Add ****** 06/22/21 *************************
                scPF1rrn = dbRrn1;
               //***** End Add ****** 06/22/21 *************************
               ExFmt RcdReins;
            EndDo;
         Endif;


       //* Premium information popup.
         Readc sflPremium;
         Dow not %eof;
            If lnkWrttn = *on;
               DoU BtnPrmCan = *On;
                  LoadSltStInfo();
                  //***** Beg Add ****** 06/22/21 *************************
                   Chain(n) key1WDELL PEL00;
                   scPLrrn = dbRrn1;
                  //***** End Add ****** 06/22/21 *************************
                  ExFmt RcdPrmInfo;
                  clear intRrn3;
                  intRrn3 = rrnPrRtRow;
               EndDo;
            Endif;

            //* Delete link TAB 1
            If LnkDltTab1 = *On;
               dltTab1Vsb = *Off;
            //*** begin add ***  07/06/22  **********************************
               Chain(n) key1WDELL PEL00;
               scPDrrn = dbRrn1;
            //*** end add *****  07/06/22  **********************************
               Chain(n) scPrRRN PEL0P;
               k1CO# = ELco#;
               k1FND = ELfnd;
               k1EMP# = ELemp#;
               k1FYR  = ELfyr;
               Setll keyWDELL8 PEL08;
               Reade(n) keyWDELL8 PEL08;
               DoW not %eof;
                  Exsr PreDltCheck;
                  If erDltMsg = *On;
                     Leave;
                  Endif;
                  Reade(n) keyWDELL8 PEL08;
               Enddo;

               DoU BtnDltCan = *On or BtnDltOk = *On;
               // Exsr PreDltCheck;
               //*** begin del ***  07/06/22  **********************************
                  //***** Beg Add ****** 06/22/21 *************************
               //*****    Chain(n) key1WDELL PEL00;
               //*****    scPDrrn = dbRrn1;
                  //***** End Add ****** 06/22/21 *************************
               //*** end del *****  07/06/22  **********************************
                  ExFmt RcdDltMsgW;
               EndDo;
               If BtnDltOk = *On and erDltMsg = *Off;
                  Exsr DltRltFiles;
                  BtnDltOk = *Off;
                  LnkDltTab1 = *Off;
               Endif;
            Endif;
            Readc sflPremium;
         Enddo;

         //* Delete link TAB 2.
         Readc sflPrmDiv;
         Dow not %eof;
            If LnkDltTab2 = *On;
               dltTab1Vsb = *On;
               DoU BtnDltCan = *On or BtnDltOk = *On;
                  Exsr PreDltCheck;
                  //***** Beg Add ****** 06/22/21 *************************
                  scPDrrn = dbRrn1;
                  //***** End Add ****** 06/22/21 *************************
                  ExFmt RcdDltMsgW;
               EndDo;
            Endif;
            If BtnDltOk = *On and erDltMsg = *Off;
               Exsr DltRltFiles;
               BtnDltOk = *Off;
               LnkDltTab2 = *Off;
            Endif;
            Readc sflPrmDiv;
         Enddo;

         If btnRefresh = *On;
           btnRefresh =*Off;
         //*  Load action bar
         LoadActionBar();
         Endif;

         //* When Unbilled Premium button is pressed:
         //* display a popup window displaying the unbilled premium from a calculate and print bill.
         If unBilled = *On;
            elxRRN = dbrrn9;
            DisplayPremium(elxRRN:'U');
            unBilled  =*Off;
         Endif;

       //* Menu Choices
         If scSelOpt <> *blanks;
            @@next=scSelOpt;
            Leave;
         Endif;

       //* Popup Windows
         Popup();

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;
          When TabIndex = 0;
       //    disYrDivT1 = *On;
             TabHeight = 2240;
             APnlHeight = TabHeight + 60;
       //*** Beg Add *****  01/29/20  ****************************************
            //* Load Filter Clear Button (TAB 1)
              LoadClrFltr();

            //* Load Selected Billing Division(TAB 1)
              LoadBillingDiv();

            //* Load State Reporting Division(TAB 1)
              LoadStatRptDiv();

            //* Load Deposit - All Billing Divisions / Policy Years(TAB 1)
              LoadDeposit();

            //* Load Premium Information, All Billing Divisions(TAB 1)
              LoadPremInfo();

            //* Load Claimant information.(TAB1)
              LoadClaimant();

            //* Load Billing Divisions.
              LoadBillDiv();

            //* Load Cancellation History, State Reporting Division(TAB 1)
              LoadCxHistory();

            //* Load Locations (Other Workplaces) Information. TAB1
              LoadEnLocat();
       //*** End Add *****  01/29/20  ****************************************
             Exsr WriteAction;
             Write rcdTabPan;
             Exfmt rcdAllInfo;
             Exsr ReadAction;
             Read rcdTabPan;
          When TabIndex = 1;
       //    disYrDivT1 = *Off;
             TabHeight = 2065;
             APnlHeight = TabHeight + 60;
       //*** Beg Add *****  01/29/20  ****************************************
            //* Load Filter Clear Button (TAB2).
              LoadClrFltr2();

            //* Load Deposit - Billing Divisions / Policy Years(TAB 2)
              LoadDivDepost();

            //* Load Premium Information, Billing Divisions(TAB 2)
              LoadBPremDiv();

            //* Load Agency History, Billing Location(TAB 2)
              LoadAgtHist();

            //* Load Claimant information.
              If btnFilter3 <> *on;
                 LoadClmntBDv();
              Endif;

            //* Load Cancellation History, Billing Division(TAB 2)
              LoadCxBilDivH();

            //* Load Locations (Other Workplaces) Information (TAB 2)
              LoadEnLocT2();
       //*** End Add *****  01/29/20  ****************************************
             Exsr WriteAction;
             Write rcdTabPan;
             Exfmt rcdDivSum;
             Exsr ReadAction;
             Read rcdTabPan;
          When TabIndex = 2;
       //    disYrDivT1 = *Off;
             TabHeight = 1800;
             APnlHeight = TabHeight + 60;
       //*** Beg Add *****  01/29/20  ****************************************
            // Load Current Payroll Information(TAB 3)
             LoadPayrollGrd();

            // Load Premium History(TAB 3)
             LoadPrmHistGrd();
       //*** End Add *****  01/29/20  ****************************************
             Exsr WriteAction;
             Write rcdTabPan;
             Exfmt rcdDivPrem;
             Exsr ReadAction;
             Read rcdTabPan;
       EndSl;

       Endsr;
       //===================================================================
       // Write Screen Formats for mteMode
       //===================================================================
       BegSr PrcMteMode;
       //disYrDivT1 = *On;
         TabHeight = 2215;
         APnlHeight = TabHeight + 60;
       //*** Beg Add *****  07/16/20  ****************************************
       //* Load Filter Clear Button (TAB 1)
         LoadClrFltr();

       //* Load Selected Billing Division(TAB 1)
         LoadBillingDiv();

       //* Load State Reporting Division(TAB 1)
         LoadStatRptDiv();

       //* Load Deposit - All Billing Divisions / Policy Years(TAB 1)
         LoadDeposit();

       //* Load Premium Information, All Billing Divisions(TAB 1)
         LoadPremInfo();

       //* Load Claimant information.(TAB1)
         LoadClaimant();

       //* Load Billing Divisions.
         LoadBillDiv();

       //* Load Cancellation History, State Reporting Division(TAB 1)
         LoadCxHistory();

       //* Load Locations (Other Workplaces) Information. TAB1
         LoadEnLocat();
       //*** End Add *****  07/16/20  ****************************************
         Exsr WriteAction;
         Write rcdTabPan;
         Write rcdAllInfo;
         xxMode = 'C';
         xxNext = ' ';
         xcRrn = %Editc(xxRRN:'X');
         MidTermEndorse(xcRrn:xxMode:xxNext);
         ExitNow = *On;
       Endsr;
       //===================================================================
       // Write Screen Formats for revMode
       //===================================================================
       BegSr PrcRevMode;
       //disYrDivT1 = *On;
         TabHeight = 2215;
         APnlHeight = TabHeight + 60;
       //*** Beg Add *****  07/16/20  ****************************************
       //* Load Filter Clear Button (TAB 1)
         LoadClrFltr();

       //* Load Selected Billing Division(TAB 1)
         LoadBillingDiv();

       //* Load State Reporting Division(TAB 1)
         LoadStatRptDiv();

       //* Load Deposit - All Billing Divisions / Policy Years(TAB 1)
         LoadDeposit();

       //* Load Premium Information, All Billing Divisions(TAB 1)
         LoadPremInfo();

       //* Load Claimant information.(TAB1)
         LoadClaimant();

       //* Load Billing Divisions.
         LoadBillDiv();

       //* Load Cancellation History, State Reporting Division(TAB 1)
         LoadCxHistory();

       //* Load Locations (Other Workplaces) Information. TAB1
         LoadEnLocat();
       //*** End Add *****  07/16/20  ****************************************
         Exsr WriteAction;
         Write rcdTabPan;
         Write rcdAllInfo;
         xxMode = 'C';
         xxNext = ' ';
         xcRrn = %Editc(xxRRN:'X');
         xxTab ='00000';
         xxQte = quoteMode;
         RevisedBill(xcRrn:xxMode:xxNext:xxTab:xxQte);
         ExitNow = *On;
       Endsr;
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
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;

       If qteMode;
          Read rcdQtePnl;
       Else;
          Read rcdPolPnl;
       Endif;

       Endsr;
       //===================================================================
       // Pre-Processing before displaying Delete window.
       //===================================================================
       BegSr PreDltCheck;

          Select;
          When LnkDltTab2 = *On;
         //*** Begin Chg *** 07/11/18 ****************************
             inRRN = scT2rrn;
             Chain(n) inRRN PEL0P;
         //*** End   Chg *** 07/11/18 ****************************
             scDltEmp# = %Editc(ELfnd:'X') + ' - ' + %Editc(ELemp#:'X')
              +' - ' + %Editc(ELdiv:'X') + ' - '+ %Editc(ELfyr:'X');
          When LnkDltTab1 = *On;
             scDltEmp# = %Editc(ELfnd:'X') + ' - ' + %Editc(ELemp#:'X')
              +' - ' + %Editc(ELfyr:'X');
             ValidEmpYr(ELfyr:ELemp#:ELdiv:EL_rrn:ELfnd);
             inRRN = EL_rrn;
             Chain(n) inRRN PEL0P;
          Endsl;

          scDltStDt  = dtMDYY(ELpfdt);
          scDltEndDt = dtMDYY(ELptdt);
          clear xxKey;
          xxKey = %editc(ELbscd : 'X');
          Chain(n) keyTable PBS01;
          scDltBilTy = %trimr(xxKey) + ' ' + BSdesc;
          scDltStat  = GetQuoteStsDesc(ELco#:ELfnd:ELEmp#:ELdiv);

          inQTE = 'W';
          outIND =  0;
          erDltMsg = *Off;
          msDltMsg = '  ';
          SrDltChk(inRRN:inQTE:outIND);
          If outIND <> 0;
             INV = outIND;
             Select;
       //* Payments Exist
             When INV = 72;
             erDltMsg = *On;
             msDltMsg = 'WCR1097';

       //* Claims Exist
             When INV = 71;
             erDltMsg = *On;
             msDltMsg = 'WCR0623';

       //* A/R Exists in prior month
             When INV = 74;
             erDltMsg = *On;
             msDltMsg = 'WCR1095';

       //* Not Authorized
             When INV = 77;
             erDltMsg = *On;
             msDltMsg = 'WC30020';

       //* NCCI Submission Status '4' Exist
             When INV = 80;
             erDltMsg = *On;
             msDltMsg = 'WC30100';

       //* Non x-fer'd A/R Batch Rcds Exist
             When INV = 81;
             erDltMsg = *On;
             msDltMsg = 'WC30101';

       //* State Reporting Div Billings Exist
             When INV = 83;
             erDltMsg = *On;
             msDltMsg = 'WC30335';

       //* Detail History Exist
             When INV = 84;
             erDltMsg = *On;
             msDltMsg = 'WC32390';

       //* Payroll Exist
             When INV = 86;
             erDltMsg = *On;
             msDltMsg = 'WCR4808';
       //* Employer Master Cx'd
             When INV = 87;
             erDltMsg = *On;
             msDltMsg = 'WC32410';

       //*** Begin Add *** 07/02/18 **************************************
       //* Only one Policy year
             When INV = 89;
             erDltMsg = *On;
             msDltMsg = 'WCR4827';
       //*** End   Add *** 07/02/18 **************************************


             Endsl;
          Endif;

          If erDltMsg = *On;
             dltTxtConf = *Off;
             BtnOkConf  = 'OK';
             dltWindTit = '**** Message ****';
          Else;
             BtnOkConf  = 'CONFIRM';
             dltTxtConf = *On;
             dltWindTit = 'Confirm Delete of Employer Policy Years';
          Endif;
       Endsr;

       //===================================================================
       // CALL PROGRAM TO DELETE RELATED FILES
       //===================================================================
       BegSr DltRltFiles;
          Select;
          When LnkDltTab2 = *On;
             Chain inRRN PEL0P;
          When LnkDltTab1 = *On;
             ValidEmpYr(ELfyr:ELemp#:ELdiv:EL_rrn:ELfnd);
             inRRN = EL_rrn;
             Chain(n) inRRN PEL0P;
          Endsl;
          inCO# = ELco#;
          inFND = ELfnd;
          inEMP#= ELemp#;
          indiv = ELdiv;
          infyr = ELfyr;
          inQTE = 'W';

          Select;
          When LnkDltTab2 = *On;
          SrDltRltFiles(inCO#:inFND:inEMP#:inDIV:inFYR:inQTE);
          //*** Begin Add *** 06/14/18 *****************************
          Exsr UpdEmpMst;
          //*** End   Add *** 06/14/18 *****************************
          Delete PEL0P;

          When LnkDltTab1 = *On;
          Setgt  keyWDELLB PEL0B;
          Readpe keyWDELLB PEL0B;
          DoW not %eof;
             SrDltRltFiles(Elco#:Elfnd:Elemp#:ELdiv:Elfyr:inQTE);
             //*** Begin Add *** 06/14/18 *****************************
             Exsr UpdEmpMst;
             //*** End   Add *** 06/14/18 *****************************
             Delete PEL0B;
             Readpe keyWDELLB PEL0B;
          Enddo;

          Endsl;

       Endsr;

       //*** Begin Add *** 06/14/18 *****************************
       //===================================================================
       // Update Employer master: Clear Cancellation date
       //===================================================================
       BegSr UpdEmpMst;

          //Clear CX date on Employer master if Renewal term is cancelled and
          //Renewed year is being deleted and prior year is not cancelled.
          strPrYearCx = 'N';                           //Prior Year CX exist
          strCxFnd    = 'N';                           //Prior Year CX Rcd Found
          Chain(n) (ELco#:ELfnd:ELemp#:ELdiv) pem01;
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
                   Chain (ELco#:ELfnd:ELemp#:ELdiv) pem01;
                   If %Found(wmeml);
                      Clear EMCad1;
                      Update pem01;
                   Endif;
                Endif;
             Endif;
          Endif;

       Endsr;
       //*** End   Add *** 06/14/18 *****************************

       //===================================================================
       // Check Filter 3 (Tab 1 Claim Filter)
       //===================================================================
       BegSr CheckFlt3;

          error = *off;
          erT1FAccdt = *off;
          erT1TAccdt = *off;

       //* Validate dates if one is entered.
          If ssT1FAccDt <> 0 or ssT1TAccDt <> 0;

             //* From date is valid.
             erT1FAccDt = ValidDate(ssT1FAccDt:message);
             If erT1FAccDt = *on;
                msT1FAccDt = message;
                error = *on;
             Endif;

             //* To date is valid.
             erT1TAccDt = ValidDate(ssT1TAccDt:message);
             If erT1TAccDt = *on;
                msT1TAccDt = message;
                error = *on;
             Endif;

             //* Date range must be ascending.
             If erT1FAccDt = *off and erT1TAccDt = *off;
                erT1FAccDt = AscendDates(ssT1FAccDt:ssT1TAccDt:message);
                If erT1FAccDt = *on;
                   erT1TAccDt = *on;
                   msT1FAccDt = message;
                   msT1TAccDt = message;
                   error = *on;
                Endif;
             Endif;

          Endif;

       Endsr;

       //===================================================================
       // Check Filter 4 (Tab 2 Claim Filter)
       //===================================================================
       BegSr CheckFlt4;

          error = *off;
          erT2FAccdt = *off;
          erT2TAccdt = *off;

       //* Validate dates if one is entered.
          If ssT2FAccDt <> 0 or ssT2TAccDt <> 0;

             //* From date is valid.
             erT2FAccDt = ValidDate(ssT2FAccDt:message);
             If erT2FAccDt = *on;
                msT2FAccDt = message;
                error = *on;
             Endif;

             //* To date is valid.
             erT2TAccDt = ValidDate(ssT2TAccDt:message);
             If erT2TAccDt = *on;
                msT2TAccDt = message;
                error = *on;
             Endif;

             //* Date range must be ascending.
             If erT2FAccDt = *off and erT2TAccDt = *off;
                erT2FAccDt = AscendDates(ssT2FAccDt:ssT2TAccDt:message);
                If erT2FAccDt = *on;
                   erT2TAccDt = *on;
                   msT2FAccDt = message;
                   msT2TAccDt = message;
                   error = *on;
                Endif;
             Endif;

          Endif;

       Endsr;
      /end-free
     P Main            E


    P*====================================================================
    P* MeetFilter - Determine if the records meet the Filter criteria
    P*====================================================================
    P*
     PMeetFilter       b
    P*
     dMeetFilter       PI
      /free

       // Check Entity/Loc name against search string
       If ssEnLcNm <> ' ';
          u1pDsc = Upper(ALname + ALnam2);
          u2pDsc = Upper(ssEnLcNm);
          Exsr String;
       Endif;

       // Check Employers Physical street Addr 1 against search string
       If ssPhAd1 <> ' ';
          u1pDsc = Upper(ALpad1);
          u2pDsc = Upper(ssPhAd1);
          Exsr String;
       Endif;

       // Check if Entity/Location/Both
       If dsFilter.ssElTp <> ' ';
       Select;
       When dsFilter.ssElTp = 'L' and ALeltp <> 'L';
          meetss = *off;
       When dsFilter.ssElTp = 'E' and ALeltp <> 'E';
          meetss = *off;
       Endsl;
       Endif;

       // Check Fedearal Id
       If dsFilter.ssEnLcFein <> *zeros and dsFilter.ssEnLcFein <> ALfein;
          meetss = *off;
       Endif;

       // Check Effecive date
       If dsFilter.ssEnLcEfDt <> 0;
          ssfdat7 = DtCYMD(dsFilter.ssEnLcEfDt);
          If ALefdt <> ssfdat7;
             meetss = *off;
          Endif;
       Endif;

       // Check State
       If dsFilter.ssEnLcStat <> ' ' and dsFilter.ssEnLcStat <> ALpst;
          meetss = *off;
       Endif;
       /end-free


      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
     c     String        Begsr
      *
     c                   Call      'QCLSCAN'
     c                   parm                    U1PDSC
     c                   parm      100           STRLEN            3 0
     c                   parm      1             START             3 0
     c                   parm                    U2PDSC
     c                   parm      40            PATLEN            3 0
     c                   parm      '0'           TRANS             1
     c                   parm      '1'           TRIM              1
     c                   parm      ' '           WILD              1
     c                   parm                    SCNRES            3 0
      *
     c     scnres        ifeq      0
     c                   move      *off          meetss
     c                   endif
      *
     c                   Endsr

     PMeetFilter       e

    P*====================================================================
    P* MeetFilter2 - Determine if the records meet the Filter criteria(TAB 2)
    P*====================================================================
    P*
     PMeetFilter2      b
    P*
     dMeetFilter2      PI
      /free

       // Check Entity/Loc name against search string
       If ss2EnLcNm <> ' ';
          u1pDsc = Upper(ALname + ALnam2);
          u2pDsc = Upper(ss2EnLcNm);
          Exsr String2;
       Endif;

       // Check Employers Physical street Addr 1 against search string
       If ss2PhAd1 <> ' ';
          u1pDsc = Upper(ALpad1);
          u2pDsc = Upper(ssPhAd1);
          Exsr String2;
       Endif;

       // Check if Entity/Location/Both
       If dsFilter2.ss2ElTp <> ' ';
       Select;
       When dsFilter2.ss2ElTp = 'L' and ALeltp <> 'L';
          meetss2 = *off;
       When dsFilter2.ss2ElTp = 'E' and ALeltp <> 'E';
          meetss2 = *off;
       Endsl;
       Endif;

       // Check Fedearal Id
       If dsFilter2.ss2EnLcFen <> *zeros and dsFilter2.ss2EnLcFen <> ALfein;
          meetss2 = *off;
       Endif;

       // Check Effecive date
       If dsFilter2.ss2EnLEfDt <> 0;
          ssfdat7 = DtCYMD(dsFilter2.ss2EnLEfDt);
          If ALefdt <> ssfdat7;
             meetss2 = *off;
          Endif;
       Endif;

       // Check State
       If dsFilter2.ss2EnLStat <> ' ' and dsFilter2.ss2EnLStat <> ALpst;
          meetss2 = *off;
       Endif;
       /end-free

      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
     c     String2       Begsr
      *
     c                   Call      'QCLSCAN'
     c                   parm                    U1PDSC
     c                   parm      100           STRLEN            3 0
     c                   parm      1             START             3 0
     c                   parm                    U2PDSC
     c                   parm      40            PATLEN            3 0
     c                   parm      '0'           TRANS             1
     c                   parm      '1'           TRIM              1
     c                   parm      ' '           WILD              1
     c                   parm                    SCNRES            3 0
      *
     c     scnres        ifeq      0
     c                   move      *off          meetss2
     c                   endif
      *
     c                   Endsr
     PMeetFilter2      e

    P*====================================================================
    P* MeetFilter3 - Determine if the records meet the Filter criteria
    P*====================================================================
    P*
     PMeetFilter3      b
    P*
     dMeetFilter3      PI
      /free

       // Check Claim Status.
       If ssT1ClStat <> ' ';
       Select;
       When ssT1ClStat = 'O' and CMstat <> 'O';
          meetss3 = *off;
       When ssT1ClStat = 'C' and CMstat <> 'C';
          meetss3 = *off;
       Endsl;
       Endif;

       // Check Effecive date
       xxAcdtFrmDt =  DtCYMD(dsFilter3.ssT1FAccDt);
       xxAcdtToDt  =  DtCYMD(dsFilter3.ssT1TAccDt);
       If xxAcdtToDt <> *zero and
         ((xxAcdtFrmDt > CMacdt) or
         (xxAcdtToDt <  CMacdt));
         meetSS3 = *Off;
       Endif;

       /end-free

     PMeetFilter3      e

    P*====================================================================
    P* MeetFilter4 - Determine if the records meet the Filter criteria
    P*====================================================================
    P*
     PMeetFilter4      b
    P*
     dMeetFilter4      PI
      /free

       // Check Claim Status.
       If ssT2ClStat <> ' ';
       Select;
       When ssT2ClStat = 'O' and CMstat <> 'O';
          meetss4 = *off;
       When ssT2ClStat = 'C' and CMstat <> 'C';
          meetss4 = *off;
       Endsl;
       Endif;

       // Check Effecive date
       xxAcdtFrmDt2 =  DtCYMD(dsFilter4.ssT2FAccDt);
       xxAcdtToDt2  =  DtCYMD(dsFilter4.ssT2TAccDt);
       If xxAcdtToDt2 <> *zero and
         ((xxAcdtFrmDt2 > CMacdt) or
         (xxAcdtToDt2 <  CMacdt));
         meetSS4 = *Off;
       Endif;

       /end-free

     PMeetFilter4      e


       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // Chain to Employer Detail for Program.
       Chain(n) xxRrn PEL0P;
       scActRrn=xxRrn;
       // Process only if valid employer detail record found.
       If not %found;
          processme = *Off;
          Return;
       Endif;

       APnlHeight = 1450;

       scCo#   = ELco#;
       scFnd   = ELfnd;
       scEmp#  = ELemp#;
       scDiv   = ELdiv;
       scFyr   = ELfyr;
       saveFyr = ELfyr;
       saveEffDt = ELpfdt;
       rrnPrRtRow = 1;


       kkDiv = ELdiv;
       kkFyr = ELfyr;
       isContribution = GetPrmContflg(scCO#:scFND);
       clear dsFilter4;
       clear RcdClm2Flt;

       LoadSelection();
       //Initialize the Filter fields
       LoadIntFilter();
       LoadIntFilter2();
       LoadIntFilter3();
       LoadIntFilter4();
       IF ClientID = '025';
          visbTASB = *On;
       Else;
          visbTASB = *Off;
       Endif;

       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E


       //===================================================================
       // Load display indicators for sub-files in rcdAllInfo
       //===================================================================
     P RcdAllInfoDsp   b
     D RcdAllInfoDsp   pi
     D  indValue                      1n   const
      /Free
       dspBillDvS = indValue;
       dspBillDvC = indValue;
       dspCxHistS = indValue;
       dspCxHistC = indValue;
       dspClmtS   = indValue;
       dspClmtC   = indValue;
       dspEnLocS  = indValue;
       dspEnLocC  = indValue;
       dspPrmInfS = indValue;
       dspPrmInfC = indValue;
      /End-Free
     P RcdAllInfoDsp   e


       //===================================================================
       // Load display indicators for sub-files in rcdDivPrem
       //===================================================================
     P RcdDivPremDsp   b
     D RcdDivPremDsp   pi
     D  indValue                      1n   const
      /Free
       dspPayrolS = indValue;
       dspPayrolC = indValue;
       dspPrmHisS = indValue;
       dspPrmHisC = indValue;
      /End-Free
     P RcdDivPremDsp   e


       //===================================================================
       // Load display indicators for sub-files in rcdDivSum
       //===================================================================
     P RcdDivSumDsp    b
     D RcdDivSumDsp    pi
     D  indValue                      1n   const
      /Free
       dspPrmDivS = indValue;
       dspPrmDivC = indValue;
       dspCxBDHsS = indValue;
       dspCxBDHsC = indValue;
       dspAgtHisS = indValue;
       dspAgtHisC = indValue;
       dspClmSTb2 = indValue;
       dspClmCTb2 = indValue;
       dsp2EnLocS = indValue;
       dsp2EnLocC = indValue;
      /End-Free
     P RcdDivSumDsp    e


       //===================================================================
       // Load display indicators for sub-files in rcdPrmInfo
       //===================================================================
     P RcdPrmInfoDsp   b
     D RcdPrmInfoDsp   pi
     D  indValue                      1n   const
      /Free
       dspPrRtStS = indValue;
       dspPrRtStC = indValue;
       dspSPyrolS = indValue;
       dspSPyrolC = indValue;
       dspClInStS = indValue;
       dspClInStC = indValue;
      /End-Free
     P RcdPrmInfoDsp   e


       //===================================================================
       // Load display indicators for sub-files in rcdReins
       //===================================================================
     P RcdReinsDsp     b
     D RcdReinsDsp     pi
     D  indValue                      1n   const
      /Free
       dspAggPolS = indValue;
       dspAggPolC = indValue;
       dspSpcPolS = indValue;
       dspSpcPolC = indValue;
       dspAggCdeS = indValue;
       dspAggCdeC = indValue;
       dspSpcCdeS = indValue;
       dspSpcCdeC = indValue;
      /End-Free
     P RcdReinsDsp     e


       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0
     d   inFND                        3  0
     d   inFYR                        3  0
     d   inEMP#                       9  0
     d   inDIV                        5  0
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


      * utility procedure definitions
      /COPY SCOPYPOLPI
