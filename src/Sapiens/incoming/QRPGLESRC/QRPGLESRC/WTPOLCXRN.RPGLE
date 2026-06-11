     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLCXRN - Policy Cancellation/Reinstatements.
     H DFTNAME(WTPOLCXRN)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 08/29/12                                         *
    *  Programmer: C:190                                            *
    *  Project   : WC2126                                           *
    *===============================================================*
    *  Revisions:                                                   *
      *                                                               *
      * 07/28/25 C:062 Project: 153WBJ                                *
      *                Add batch# and late fee.                       *
      *                                                               *
      * 04/18/24 C:342 Project: WC2906 Support : 115312               *
      *                Show Fnd/Emp#/Div when called from WTCXBTCH    *
      *                on All Divisions/Years screen.                 *
      *                DSPF Changes:                                  *
      *                1)Added scEmpNo field on to display emp info   *
      *                 on RcdAllInfo.                                *
      *                                                               *
      * 02/26/24 C:340 Support: WC2956                                *
      *                Reset Flag - Cancellation/Reinstatement Process*
      *                             Control                           *
      *                                                               *
      * 01/24/24 C:300 Support: 114919 (AmWins)                       *
      *                1. Deletion of division being overridden by Cx/Rn
      *                This is situation of Cancellation/Deletion gets*
      *                Overlap on Divisions. so WC2880 changes needed *
      *                from WTPOLDLTDV and WTPOLRESDV programs        *
      *                2. Do not allow Revise Cancel and Reinstate of *
      *                Division which has been deleted in Mid-term    *
      *                Endorsement before its Rescind in another MTE  *
      *                3. Do not Process Renewal Year's Reinstatement *
      *                of Division which has been deleted in Mid-term *
      *                Endorsement before its Rescind in another MTE  *
      *                                                               *
      * 07/03/23 C:342 Project: WC2906                                *
      *                1.Show Cancellation History in Display mode    *
      *                when called from program WTCXBTCH. (new        *
      *                Cancellation Batches screen)                   *
      *                DSPF Changes:                                  *
      *                1.Added Cancel Button in DSPF with visbility   *
      *                  indicator VisbCancel.                        *
      *                2.Added Panel on RcdAllInfo record format.     *
      *                                                               *
      * 09/14/22 C:344 Support: WC2880                                *                            *
      *                Remove the calls to WA007R                     *
      *                                                               *
      * 09/13/22 C:344 Support: 103617                                *
      *                Expand the width pixel of panel title.         *
      *                                                               *
      * 09/08/22 C:300 Support: 103583 (FMIC)                         *
      *                Certificate Holder Notices - Not Printing in   *
      *                Reprint                                        *
      *                                                               *
      * 04/11/22 C:333 Project: SY434                                 *
      *                - Update Grids for Sorting Filtering Upgrade   *
      *                                                               *
      * 12/09/21 C:65  Phaseware: 98790 (Sunz)                        *
      *                When checking for all divisions eligible to be *
      *                reinstated, remove the cancellation issue date *
      *                check as that is redundent with the endorsement*
      *                flag also being checked.                       *
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
      *                                                               *
      *  06/23/21 C:318 Project WC2302 - Auto tab closure for
      *                deletion.
      *
      * 07/28/20 C:65  Phaseware: 90375 (SCS)                         *
      *                Use Cancelation or Reinstatement process type  *
      *                for reprints - there is no reprint type.       *
      *                                                               *
      * 10/25/19 C:267 Support: 82032                                 *
      *               -For deleted division via mid-term endorsement, *
      *                Reprint option is NOT valid.                   *
      *               -Fix Reprint option whereby it was not producing*
      *                output via Scriptura for cancellation nor for  *
      *                reinstatement.                                 *
      *                                                               *
      * 02/05/19 C:267 Support: 75291                                 *
      *                Clear cancel and reinstate response buttons.   *
      *                These were inadvertently effecting Darts       *
      *                remarks code value.                            *
      *                                                               *
      * 02/06/18 C:291 Support: 63953                                 *
      *                Change Division dropdown "max choices" property*
      *                to 100,000. Screen change only.                *
      *                                                               *
      * 01/30/17 C:136 Phaseware: 45273                               *
      *                Modify reinstatement edit to prevent renewal   *
      *                year from being reinstated if cx date equals   *
      *                the policy effective date and prior year       *
      *                is not reinstated                              *
      *                                                               *
      * 10/19/16 Support: 43853                                       *
      *          Default division drop down box correctly             *
      *                                                               *
      * 10/06/16  C:65  PROJECT: SY371                                *
      *                 Implement Scriptura output control window     *
      *                 when process is flagged to generate Scriptura *
      *                 output.  The settings are stored/passed via   *
      *                 a local data area which is picked up by       *
      *                 API's and the printing program.               *
      *                                                               *
      * 03/21/16 Phaseware: 39841
      *          Prevent 2nd cancellation from running if state
      *          reporting division is already cancelled
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      *    10/12/15 C:302 Support: 37716                              *
      *                   Do not reinstate divisions that have been   *
      *                   deleted via mid-term endorsment             *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *
      *                                                               *
      * 09/23/15 C:180 Support: 37149                                 *
      *                Do not allow reinstatement of auto-flat        *
      *                renewal year cancellation when cancellation    *
      *                occurred on prior year.                        *
      *                                                               *
      * 07/22/15 C:300 Support: 34308                                 *
      *                WTXCL1 file has DESCEND keyword on efftive date*
      *                Read WTXCL1 file correctly to get the Cancel   *
      *                limit for Reinstatment                         *
      *                                                               *
      * 07/17/15 C:306 Support: 36049                                 *
      *                Changed the SCAMTDUE field length (WTPOLCXRN)  *
      *                Recompile only.                                *
      *                                                               *
      * 05/14/15 C:306 Support: 34305                                 *
      *                Removing the reason code description field i.e.*
      *                SCRSND from Cancellation inforamtion window.   *
      *                                                               *
      * 04/16/15 C:65  Project: SY251                                 *
      *                Add API call to launch new Department          *
    *                letters.                                       *
    *                                                               *
      * 03/27/15 C:298 Support: 32680                                 *
      *                Reinstatement should process for all the       *
      *                divisions even if the Cancellation Date is     *
      *                before the Policy start date.                  *
      *                                                               *
      * 03/04/15 C:65  Phase#:  32680                                 *
      *                Also update cx date used in CanInfoSave.       *
    *                                                               *
      * 02/18/15 C:279 Support: 32695                                 *
      *                Populate Reason Code description using Ajax.   *
      *                                                               *
      * 02/02/15 C:283 Support: 32680                                 *
      *                Cancellation should process for all the        *
      *                divisions even if the Cacncellation Date is    *
      *                before the Policy start date.                  *
      *                                                               *
      * 10/08/14 C:291 RED 3161                                       *
      *                Change the date definition on the grid from    *
      *                Number to date data type.                      *
      *                                                               *
      * 07/28/14 C:295 Project: 114WMD                                *
      *                Compilation only as the changes are            *
      *                in Action Panel                                *
      *
      * 06/20/14 C:291 Redmine: 3009                                  *
      *                Change "Cancel" button text to                 *
      *                "Issue Cancellation".                          *
      *                                                               *
      * 04/05/14 C:295 Redmine: 4093                                  *
      *                Cancellation Information pop up is not         *
      *                defaulting the cancel date.                    *
    *                                                               *
      * 16/01/14 C:295 Support: WA114 84485                           *
      *                Reinstatements after Audit Processing          *
    *                                                               *
      * 11/08/13 C:295 Support: WA133 83334                           *
      *                Policy-Cancellation Date in the Action Panel   *
      *                where there is a new renewal year is populated *
      *                Incorrectly (Code is reverted)                 *
      *                                                               *
      * 10/03/13 C:295 Support: WA133 83334                           *
      *                Policy-Cancellation Date in the Action Panel   *
      *                where there is a new renewal year is populated *
      *                Incorrectly.                                   *
      *                                                               *
      * 09/03/13 C:283 Support: WA099 83359                           *
      *                Reinstatement screen should show the Ent/Loc   *
      *                and Additional Division notification only in   *
      *                valid situation.                               *
      *                                                               *
      * 08/16/13 C:279 Support: WA146 83046                           *
      *                Modified to update Reinstatement fee correctly.*
      *                                                               *
    * 07/24/13 C:282 PROJECT: WC2086                                *
    *                Added the Reinstatement Reason Code            *
    *                in RcdRnstInf.                                 *
    *                                                               *
      * 05/16/13 C:287 Support: WA102 82216                           *
      *                Reprint the correct highlighted grid record    *
      *                                                               *
      * 03/08/13 C:283 Support: WA102 81336                           *
      *                Correctly handle the Short Rate/Prorate        *
      *                Audit Cancellation Radio Button handling.      *
      *                Atleast one of them should be selected.        *
      *                                                               *
      * 03/05/13 C:279 Project: SY266 Redmine: 2505                   *
      *                Display correct Cx Reason code                 *
      *                                                               *
      * 01/16/13 C:136 SUPPORT: WA102 81477                           *
      *                Do not display inactive codes in Cx Reason     *
      *                Drop Down (adjusted selection statement for    *
      *                cdeChoices)                                    *
      *                                                               *
      * 01/16/13 C:282 SUPPORT: WA102 81477                           *
      *                Handle the issue as program was going in loop  *
      *                in case of inactive reason code selected.      *
      *                                                               *
      * 12/26/12 C:283 SUPPORT: WA102 81317                           *
      *                Producer Name and Number not printing          *
      *                correctly on Cx/Rx Notice.                     *
    *===============================================================*

     fwtpolcxrn cf   e             workstn extfile('WTPOLCXRN')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflCxHist:rrnCxHist)
     f                                     Sfile(sflHistory:rrnHistory)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     fwdcxp     uf a e             disk    rename(pcx01:pcx0p)
     f                                     infds(dbfds)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwdell     uf   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds3)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwdell8    if   e           k disk    rename(pel01:pel08)
     fwmall     if   e           k disk    rename(pal01:pal00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fwzexl     if   e           k disk    rename(pex01:pex02)
     fwmrsl     if   e           k disk    rename(prs01:prs00)
     fwmcml11   if   e           k disk    rename(pcm01:pcm11)
     fwtxcl1    if   e           k disk    rename(pxc01:pxc00)
      ***  Beg Add *** 07/24/13  **********************************
     fwtxcl     if   e           k disk    prefix(X1:2)
      ***  End Add *** 07/24/13  **********************************
     fwdcxl     if   e           k disk    rename(pcx01:pcx00)
     f                                     infds(dbfds2)
     fwdcxl2    if   e           k disk    rename(pcx01:pcx02)
     f                                     infds(dbfds1)
     fwdcxl10   if   e           k disk    rename(pcx01:pcx10)
     fwdxhl     if a e           k disk    rename(pxh01:pxh00)
     fwdxhl4    if   e           k disk    rename(pxh01:pxh04)
     fwtcrl     if   e           k disk    rename(pcr01:pcr00)
     fwxcnl     if   e           k disk    rename(pcn01:pcn00)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmeml3    uf   e           k disk    rename(pem01:pem03)
     fwtavl     if   e           k disk    rename(pav01:pav00)
     fwaqtl16   if a e           k disk    rename(pqt01:pqt16)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fssylf     if   e           k disk
     f                                     include(ssy01)
      *** begin add ***  10/06/16  ************************************
     D/copy sPrXML
     D isUseScriptC    S              1n   inz
     D isUseScriptR    S              1n   inz
     d processTypeC    c                   const('Cancellations')
     d processTypeR    c                   const('Reinstatements')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** end add *****  10/06/16  ************************************

      /copy SCOPYPOLPR

     d PSDS          ESDS                  extName(SZQ1P)

     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0

     d                 ds
     dClaimPayments            1     90  2
     d                                     Dim(10)                              CLAIM PMTS
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

     d                 DS
     d  ED                     1    035  0
     d                                     DIM(5) ASCEND                        EFFECT. DATE
     d  EMefd1               001    007  0
     d  EMefd2               008    014  0
     d  EMefd3               015    021  0
     d  EMefd4               022    028  0
     d  EMefd5               029    035  0

     d                 DS
     d  CD                     1    035  0
     d                                     DIM(5) ASCEND                        CANCEL DATE
     d  EMcad1               001    007  0
     d  EMcad2               008    014  0
     d  EMcad3               015    021  0
     d  EMcad4               022    028  0
     d  EMcad5               029    035  0

     d MSG1            c                   CONST('Policy already under-
     d                                      cancellation effect-
     d                                     ive')
     d MSG2            c                   CONST('Policy already cance-
     d                                     lled')
     d AGTAPI          c                   Const('WA002R')
     d REPRTC          c                   Const('W7165C')
     d REPRTR          c                   Const('W7167C')

     d xxRrn           s              9  0
     d ov400Enrolled   s               n   inz(*off)
     d rnstVisb        s               n   inz(*off)
     d reviseFlag      s               n   inz(*off)
     d revRnstFlg      s               n   inz(*off)
     d reinstatement   s               n   inz(*off)
     d cancelDtExist   s               n   inz(*off)
     d addDivison      s               n   inz(*off)
     d addEntLoc       s               n   inz(*off)
     d addMode         s               n   inz(*off)
     d canAddMode      s               n   inz(*off)
     d canEdtMode      s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d clientID        s              3    inz('000')
     d process         s               n   inz(*off)
     d iserror         s               n   inz(*off)
     d mode            s              1
     d @@mode          s              1
     d @@next          s             10
     d xxSt            s              2
     D compName        s             40
     D fundName        s             40
     D secType         s              1
     D secLevel        s              3  0
     d inCO#           s              3  0
     d inFND           s              3  0
     d divCnt          s              5  0
     d ##outq          s             10
     d s1Fyr           s                   like(CXfyr)
     d s1Req#          s                   like(CXreq#)
     d savDiv          s                   like(EMdiv)
     d saveDt          s              7  0
     d saveTm          s              6  0
     d svReq#          s                   like(CXreq#)
     d kkSt            s                   like(CRst)
     d kkKey           s                   like(AVkey)
     d xxAcDt          s                   like(CXcxdt)
     d xxRtCd          s                   like(ELrtcd)
     d xxJrSt          s                   like(xcst)
     d xxkey           s                   like(CRkey)
     d x3Date          s                   like(CXcfdt)
     d xxCxDt          s                   like(CXcxdt)
     d svRsn           s                   like(scrsn)
     d kkEfdt          s                   like(CRefdt)
     d saveFyr         s                   like(ELfyr)
     d saveEffDt       s                   like(ELpfdt)
     d currDate7       s                   like(ELpfdt)
     d scOERR          s                   like(scERR)
     d svCDAT          s                   like(CXcdat)
     d svCXDT          s                   like(CXcxdt)
     d svREIN          s                   like(CXrein)
     d xxreq#          s                   like(CXreq#)
     d currDate8       s              8  0
     d xxWkDt          s              8  0
     d svRPMY          s              6  0
     d REPRT           s             10
     d CanInfoError    s               n   inz(*off)
     d RenInfoError    s               n   inz(*off)
      ***** beg add ***** 10/08/14 **********************************
     d ZeroDate        s               D   inz datfmt(*ISO)
      ***** end add ***** 10/08/14 **********************************
      ***** beg add ***** 12/26/12 **********************************
     d @@Agt#A         s              5  0
     d @@AnamA         s             60
     d @@ProdA         s              5  0
     d @@PnamA         s             60
      ***** end add ***** 12/26/12 **********************************
      ***** Beg Add ***** 07/24/13 **********************************
     d scAlertMsg      c                   'Corresponding amended cancellation +
     d                                      must be transacted during the same +
     d                                      business day. Press OK to Continue +
     d                                      Processing.'
     d xxResAlert      s              1A
      ***** End Add ***** 07/24/13 **********************************
      ***** Beg Add ***** 02/02/15 **********************************
     d savCXDate       s                   like(CXcxdt)
     d savPFDT         s                   like(ELpfdt)
     d savPTDT         s                   like(ELptdt)
     d savFYR          s                   like(ELfyr)
      ***** End Add ***** 02/02/15 **********************************
      // Begin Add ****** 09/23/15 **********************************
     d @CDAT           s                   like(CXcdat) inz
      // End   Add ****** 09/23/15 **********************************
      *** beg add ***** 03/21/16 ************************************
     d kkStRptDiv      s                   like(EMdiv)
      *** end add ***** 03/21/16 ************************************
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
     D CanReiPending   S               n   Inz(*off)
     D StrMsg          S             50A   Inz(' ')
      *** End Add *** 02/26/24 *** WC2956 **********************************

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d* Load Selection fields
     d LoadSelection   pr

     d* Load Cancellation History, State Reporting Division.
     d  LoadCxHistory  pr

     d* Load History Information.
     d  LoadHistory    pr

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr
     d processMe                       n

     D RcdAllInfoDsp   pr
     D  indValue                      1n   const

     D RcdHistoryDsp   pr
     D  indValue1                     1n   const

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
     d   ##Co#                        3
     d   ##Fnd                        3
     d   ##Fyr                        3
     d   ##Emp#                       9
     d   ##Div                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5

     d ComAPI          pr                  extpgm('WA001R')
     d   ##Co#                        3  0
     d   ##Fnd                        3  0
     d   ##Conm                      40
     d   ##Fnam                      40
     d   ##Lvl                        3  0
     d   ##Type                       1
     d   ##OutQ                      10
      ***** beg add ***** 12/26/12 **********************************

     d AgtAPI1         pr                  extpgm('WA002R')
     d   ##Co#A                       3  0
     d   ##FndA                       3  0
     d   ##Emp#A                      9  0
     d   ##DivA                       5  0
     d   ##FyrA                       3  0
     d   @@Agt#A                      5  0
     d   @@AnamA                     60
     d   @@ProdA                      5  0
     d   @@PnamA                     60
      ***** end add ***** 12/26/12 **********************************

     d/copy sPrDate
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrAR
     d/copy wPrUsrPrms
     d/copy sPrString
     d/copy wPrLoc
     d/copy sPrAtrium
     d/copy wPrClaims
      ***  Beg Add *** 07/24/13  **********************************
     d/copy sPrGenPop
      ***  End Add *** 07/24/13  **********************************
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************
       //*** Begin add *  09/17/15
     d/copy wPrPolCpy
       //*** End add *  09/17/15

     c* Entry of Program.
     c*
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@mode                         mode
     c                   Parm                    @@next
     C                   PARM                    @@TabInd          5            tab index
     C                   PARM                    @@Qte             1            quote

     c     Datchk        Plist
     c                   Parm                    Srdti             8 0
     c                   Parm      *BLANKS       Srffmt            8
     c                   Parm      *BLANKS       Srtfmt            8
     c                   Parm                    Srdto            10
     c                   Parm                    Srerr             1

     c     Dtcymdp       Plist
     c                   Parm                    Srdti
     c                   Parm      *BLANKS       Srffmt
     c                   Parm      '*CYMD'       Srtfmt
     c                   Parm                    Srdto
     c                   Parm                    Srerr

     c     Dtmdyyp       Plist
     c                   Parm                    Srdti
     c                   Parm      '*CYMD'       Srffmt
     c                   Parm      *Blanks       Srtfmt
     c                   Parm                    Srdto
     c                   Parm                    Srerr

     c     AGTPL         Plist
     c                   Parm      scCO#         ##CO#             3 0
     c                   Parm      scFND         ##FND             3 0
     c                   Parm      scEMP#        ##EMP#            9 0
     c                   Parm      scDIV         ##DIV             5 0
     c                   Parm      scFYR         ##FYR             3 0
     c     scAGT#        Parm                    ##AGT#            5 0
     c                   Parm                    ##ANAM           60
     c     scPROD        Parm                    ##PROD            5 0
     c                   Parm                    ##PNAM           60

     c* Key List's

     c* Employer Detail File Key List
     c*
     c     keyWDELL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFyr

     c     key1WDELL     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWDELL1     Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scFyr
     c                   Kfld                    scEmp#

     c     key1WDELL1    KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scFyr
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     keyWDELL8     KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scFyr
      *
     c     key1WDELL8    KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#

     c* Cancellation Detail File
     c     keyWDCXL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scFyr
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     key1WDCXL     KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scFyr
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scReq#

     c     key2WDCXL     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELfyr
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv

     c     keyWDCXL2     klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scdiv
     c                   kfld                    scfyr

     c****** Beg Add ******* 07/03/23 **************************
     c     keyWDCXL2A    klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scdiv
     c****** End Add ******* 07/03/23 **************************

     c     keyWDCXL21    klist
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scdiv
     c                   kfld                    scfyr
     c                   kfld                    xxreq#

     c     keyWDXHL4     klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    scEmp#
     c                   kfld                    scdiv
     c                   kfld                    scCxYear
     c                   kfld                    scCxReq#

      *  Rate Set Code File
      *
     c     keyWMRSL      Klist
     c                   Kfld                    scCo#
     c                   Kfld                    xxRtcd

     c     key1WMRSL     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELrtcd

      *  Cancellation/Reinstatement State File
      *
     c     keyWTXCL1     Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFnd
     c                   Kfld                    xxJrSt
     c                   Kfld                    kkEfDt
      *
     c     key1WTXCL1    KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    xxJrSt
      ***  Beg Add ***  07/24/13  **********************************
     c     keyWTXCL      Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFnd
     c                   Kfld                    xxJrSt
     c                   Kfld                    kkEfDt
      *
     c     key1WTXCL     KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    xxJrSt
      ***  End Add ***  07/24/13  **********************************

     c     keyWDCXL10    KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFyr
     c                   Kfld                    x3Date
      *
     c     key1WDCXL10   KLIST
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
     c                   Kfld                    scFYR

     c     keyWTCRL      KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    xxJrSt
     c                   Kfld                    kkkey
     c                   Kfld                    kkEfDt
      *
     c     key1WTCRL     KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    xxJrSt
     c                   Kfld                    kkkey

     c     key2WTCRL     KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    kkSt
     c                   Kfld                    xxKey
     c                   Kfld                    kkEfDt

     c     key3WTCRL     KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    kkSt
     c                   Kfld                    xxKey

      * Claim Master File
      *
     c     keyWMCML11    KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    xxAcDt
      *
     c     key1WMCML11   KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#

      *  Employer Master File
      *
     c     EMKYX         KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#

     c     EMKY          KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scFnd
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv

     c     EMKY1         KLIST
     c                   Kfld                    ELCO#
     c                   Kfld                    ELFND
     c                   Kfld                    ELEMP#
     c                   Kfld                    ELDIV

     c     EMKY3         KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    scEmp#
     c                   Kfld                    scDiv
     c                   Kfld                    scFnd

     c*  Admin/DARTS control defn file
     c     XXKY          KLIST
     c                   KFLD                    scCo#
     c                   KFLD                    scFnd

      *   Mass Cancellation Notice Workfile
      *
     c     keyWXCNL      KLIST
     c                   Kfld                    EMco#
     c                   Kfld                    EMfnd
     c                   Kfld                    EMemp#
     c                   Kfld                    EMdiv

      *  Activity Status
      *
     c     AVKY          KLIST
     c                   Kfld                    scCo#
     c                   Kfld                    kkkey

     C*  Additional entities/locations file
     C     keyWMALL      KLIST
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scDiv

      *  Employer Cross Reference
      *
     c     keyWZEXL      Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV

      *  KEY LIST FOR THE CANCELLATION HISTORY FILE
      *
     c     XHKY          KLIST
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scFYR
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
     c                   Kfld                    scREQ#

     c*   Quote File
     c*
     c     QTKY          KLIST
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
     c                   Kfld                    xxSTAT
     C                   Kfld                    xxAGT#
      *
     c     QTKY1         KLIST
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV

     c                   Move      'W'           xxSTAT            1            ACTIVE ENVIRONMENT

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

       rnstvisb = *On;
       NotDspMode = *On;
       Select;
       When mode = 'D';
          dspMode = *On;
       When mode = 'A';
          addMode = *On;
       When mode = 'C';
          chgMode= *On;
       Endsl;
       scActMode = mode;

       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 0;      //Only tab 0 is valid for this program
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
                xxStat = @@Qte;
          Endsl;
       Endif;
       scEnv = quoteMode;

       GetClientID(ClientID);

       //*** begin add ***  10/06/16  ****************************************
       isUseScriptC = UseScriptura(processTypeC);
       isUseScriptR = UseScriptura(processTypeR);
       //*** end add *****  10/06/16  ****************************************

       //*** Begin Add *** 07/28/25 ****************************************
       Select;
       When clientID = '153';
          is153 = *On;
       Other;
          is153 = *Off;
       EndSL;
       //*** End   Add *** 07/28/25 ****************************************

       ProgramStart(process);

       //***  Beg Add  **** 07/03/23 *******************************
       If @@next <>'CXHist';
       //***  End Add  **** 07/03/23 *******************************
       If process;
          Main();
       Endif;

       //***** Beg Add ******* 07/03/23 **************************
       Endif;
       //***** Beg Add ******* 04/18/24 **************************
       scEmpNo = %editc(ELfnd:'X') + ' - ' + %editc(ELemp#:'X') +
                     ' - ' + %editc(ELdiv:'X');
       //***** End Add ******* 04/18/24 **************************
      // Load cancellation History.
       If @@next = 'CXHist';
          // Panel Heading
          scPnlHdg = 'Cancellation History Details';
          LoadCxHistory();
          rnstvisb = *off;                               // Do not show Reinstatement btn
          NotDspMode = *off;                             // Do not show Issue Cancellation
          visbPnl = *on;                                 // Panel Visibility and show as window
          visbCancel = *on;                              // Show Cancel Button
          Dou btnCXBDCan = *on;
             Exfmt RcdAllInfo;
             Readc sflCxHist;
             If lnkHistory = *on;
                lnkHistory = *off;
                   LoadHistory();
                   ExFmt RcdHistory;
             Endif;
          Enddo;

          If btnCXBDCan = *on;
             btnCXBDCan = *off;
          Endif;
       Endif;
      // End of Cancellation Screen
       //***  Beg Add  **** 07/03/23 *******************************

       *InLr = *On;
       Return;

      /end-free



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

       Clear cdeChoices;
       cdeChoices =
             'CRCO#   = ' + %char(ELCO#)  +
        ' and CRST    = ''' + xxSt + '''' +
        ' and CRSTAT <> ''I'''            +
        ' and CREFDT <= ' + %char(ELPFDT);

      /end-free
     P LoadSelection   E



       //===================================================================
       // Cancellation / Reinstatement
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

       //***** Beg Chg ******* 07/03/23 **************************
       //*****   Setgt  keyWDCXL2 PCX02;
       //*****   Readpe keyWDCXL2 PCX02;
       If @@next = 'CXHist';
          Setgt  keyWDCXL2A PCX02;
          Readpe keyWDCXL2A PCX02;
       Else;
          Setgt  keyWDCXL2 PCX02;
          Readpe keyWDCXL2 PCX02;
       Endif;
       //***** End Chg ******* 07/03/23 **************************

       DoW not %eof;
          Clear sflCxHist;

          scCxYear   = CXfyr;
          scCxReq#   = CXreq#;
          //*** Begin Add *** 07/28/25 *************************************
          scCxBat#   = CXbat#;
          If scCxBat# <> 0;
             scLateFee  = CXlFee;
             scReinFee  = CXrFee;
          Else;
             scLateFee  = 0;
             scReinFee  = 0;
          EndIf;
          //*** End   Add *** 07/28/25 *************************************
          // *** Beg Chg *** 03/05/13 **************************
          xxKey  = CXrsn;
          xxRtCd = ELrtcd;

          Chain keyWMRSL prs00;
          If %found;
             kkSt = RSjrst;
             kkEfDt = CXcdat;

             SetGt key2WTCRL pcr00;
             ReadPe key3WTCRL pcr00;
             Dow not %eof(WTCRL) and CRstat ='I';
                ReadPe key3WTCRL pcr00;
             Enddo;
             scCxDesc = CXrsn + ' - ' + %trim(CRdesc);
          Endif;

          // scCxDesc   = %subst(GetCxRsn(scCO#:scFnd:scEmp#:scDiv:
          //  CXcxdt):1:2) + ' - ' + %subst(GetCxRsn(scCO#:scFnd:scEmp#:
          //  scDiv:CXcxdt):4:62);
          // *** End Chg *** 03/05/13 **************************
          If CXEnfl = 'Y';
             scCxDesc = 'Deleted Division';
          Endif;
        //** Begin Chg *** 10/08/14 *************************
        //scCxIssDt  = dtMDYY(CXcdat);
        //scCxEffDt  = dtMDYY(CXcxdt);
          scCxIssDt  = CvtToDate(CXcdat);
          scCxEffDt  = CvtToDate(CXcxdt);
        //** End Chg *** 10/08/14 **************************
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
        //** Begin Chg *** 10/08/14 *************************
        //scCxRnIsDT = dtMDYY(CXrdat);
        //scCxRnEfDt = dtMDYY(CXredt);
          scCxRnIsDT = CvtToDate(CXrdat);
          scCxRnEfDt = CvtToDate(CXredt);
        //** End Chg *** 10/08/14 *************************

          scCxHRRn# = dbrrn1;
          rrnCxHist = rrnCxHist + 1;
          If rrnCxHist=1;
             selCxHist=*On;
          Else;
             selCxHist=*Off;
          EndIf;
          Write sflCxHist;
          //***** Beg Chg ******* 07/03/23 **************************
          //**** Readpe keyWDCXL2 PCX02;
          If @@next = 'CXHist';
             Readpe keyWDCXL2A PCX02;
          Else;
             Readpe keyWDCXL2 PCX02;
          Endif;
          //***** End End ******* 07/03/23 **************************

       Enddo;

       rrnCxHist = 1;

      /end-free
     P LoadCxHistory   E



       //===================================================================
       // Cancellation History Information(History Link)
       //===================================================================
     P LoadHistory     B
     D LoadHistory     PI
      /free

       // Clear the subfile.
       RcdHistoryDsp(*off);
       clrHistory = *On;
       Write RcdHistory;
       clrHistory = *Off;
       RcdHistoryDsp(*on);
       rrnHistory = 0;
       Setgt  keyWDXHL4 PXH04;
       Readpe keyWDXHL4 PXH04;

       DoW not %eof;
          Clear sflHistory;

          scHstYear  = XHfyr;
          scHstReq#  = XHreq#;
          If XHrvfl = 'R';
             scHstRev   = 'Yes';
          Else;
             scHstRev   = ' ';
          Endif;

          // *** Beg Chg *** 03/05/13 **************************
          xxKey  = XHrsn;
          xxRtCd = ELrtcd;

          Chain keyWMRSL prs00;
          If %found;
             kkSt = RSjrst;
             kkEfDt = XHcdat;

             SetGt key2WTCRL pcr00;
             ReadPe key3WTCRL pcr00;
             Dow not %eof(WTCRL) and CRstat ='I';
                ReadPe key3WTCRL pcr00;
             Enddo;
             scHstDesc = XHrsn + ' - ' + %trim(CRdesc);
          Endif;

          // scHstDesc  = %subst(GetCxRsn(scCO#:scFnd:scEmp#:scDiv:
          //  GetCxDate(scCO#:scFnd:scEmp#:scdiv)):1:2) + ' - ' +
          //  %subst(GetCxRsn(scCO#:scFnd:scEmp#:scDiv:
          //  GetCxDate(scCO#:scFnd:scEmp#:scdiv)):4:62);
          // *** End Chg *** 03/05/13 **************************

       //  XHcxdt):1:2) + ' - ' + %subst(GetCxRsn(scCO#:scFnd:scEmp#:
       //  scDiv:XHcxdt):4:62);
           If XHenfl = 'Y';
              scHstDesc  = 'Deleted Division';
           Endif;

        //** Begin Chg *** 10/08/14 *****************
        //scHstIssDt = dtMDYY(XHcdat);
        //scHstEffDt = dtMDYY(XHcxdt);
          scHstIssDt = CvtToDate(XHcdat);
          scHstEffDt = CvtToDate(XHcxdt);
        //** End Chg *** 10/08/14 *****************
          scCxOccCnt = CXcnc#;

          Select;
          When XHerr = 'Y';
               scHstCanEr = 'Yes';
          When XHerr = 'N';
               scHstCanEr = 'No';
          Other;
             scHstCanEr = ' ';
          Endsl;

        //** Begin Chg *** 10/08/14 *****************
        //scHstRIsDT = dtMDYY(XHrdat);
        //scHstREfDt = dtMDYY(XHredt);
          scHstRIsDT = CvtToDate(XHrdat);
          scHstREfDt = CvtToDate(XHredt);
        //** End Chg *** 10/08/14 *****************

          rrnHistory = rrnHistory + 1;
          Write sflHistory;
          Readpe keyWDXHL4 PXH04;

       Enddo;

       rrnHistory = 1;

      /end-free
     P LoadHistory     E
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     D Main            PI

      /free

       currDate7 = DateTo7(SystemDate);
       currDate8 = DateTo8(SystemDate);

       //*  Load action bar
       LoadActionBar();

       //*  Process until exit button is pressed.

       DoU ExitNow = *On;

       //* Check if new division or policy year has been selected.
         If scVardiv <> ' ';
            kkdiv = %int(scVarDiv);
         Endif;

         If kkFyr <> scFyr or kkDiv <> scDiv;
            scFyr = kkFyr;
            scDiv = kkDiv;
            Chain(n) keyWDELL PEL00;
            If not %found;
               Setgt key1WDELL PEL00;
               Readpe(n) key1WDELL PEL00;
               scFyr = ELFYR;
               kkFyr = ELFYR;
               kkDiv = ELDIV;
            Endif;
            xxRRN = dbrrn3;
            LoadActionBar();
            LoadSelection();
         Endif;
       // * Load Button Values in Grid.
       //Chain(n) keyWDCXL  pcx00;
       setgt  keyWDCXL  pcx00;
       readpe keyWDCXL  pcx00;
       If not %eof;
          If CXcxdt <> 0;
             BtnRnstVal = 'Reinstate';
             BtnCxVal   = 'Revise Cancellation';
             canEdtMode = *On;
             canaddMode = *Off;
             rnstvisb   = *On;
          Else;
             // *** Beg Chg *** 06/20/14 *****************
             //BtnCxVal   = 'Cancel';
             BtnCxVal   = 'Issue Cancellation';
             // *** End Chg *** 06/20/14 *****************
             canaddMode = *On;
             canEdtMode = *Off;
             rnstvisb   = *Off;
          Endif;
          If CXredt <> 0;
             // *** Beg Chg *** 06/20/14 *****************
             //BtnCxVal = 'Cancel';
             BtnCxVal   = 'Issue Cancellation';
             // *** End Chg *** 06/20/14 *****************
             BtnRnstVal = 'Revise Reinstatement';
             canEdtMode = *Off;
             canaddMode = *On;
             rnstvisb   = *On;
          Endif;
       Else;
          // *** Beg Chg *** 06/20/14 *****************
          //BtnCxVal   = 'Cancel';
          BtnCxVal   = 'Issue Cancellation';
          // *** End Chg *** 06/20/14 *****************
          canaddMode = *On;
          canEdtMode = *Off;
          rnstvisb   = *Off;
       Endif;

        //* Beg add ***** 16/01/14 **********************************
          If ELfaf = 'A';
          rnstvisb   = *Off;
          Endif;
        //* End add ***** 16/01/14 **********************************

       //  Load Cancellation History, State Reporting Division(TAB 1)
       If iserror = *off;
          LoadCxHistory();
       Endif;

       // Process policy summary screen.
       Exsr ExfmtScreens;
       iserror = *off;
       //*** begin add ***  07/28/20  ****************************************
       erRepType = *off;
       msRepType = ' ';

       //  RE-PRINT CANCELLATION/REINSTATMENT
       If BtnReprint = *on and scRepType = ' ';
          erRepType = *on;
          msRepType = 'Type of document must be selected for re-print.';
          isError = *on;
          BtnReprint = *off;
       Endif;

       //*** end add *****  07/28/20  ****************************************

       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****If btnDocWndw = *on;
       //*****   btnDocWndw = *off;
       //*****   DocWindow(scActRRN:'PS');
       //*****Endif;
       //*** end add *****  04/16/15  ****************************************
       //*** Begin add *  09/17/15
       //*****If btnShowWin = *on;
       //*****   btnShowWin = *off;
       //*****   Select;
       //*****     When TxtShowWin = 'Copy Employer';
       //*****        CopyPolicy(scActRRN:@@Qte);
       //*****     When TxtShowWin = 'Department Letter';
       //*****        DocWindow(scActRRN:'PS');
       //*****   Endsl;
       //*****Endif;
       //*** End add * 09/17/15
       If btnShowWin = *on;
          PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
          btnShowWin = *off;
       Endif;
       //*** end chg *****  01/07/16  ****************************************

       //  Do nothing - Catch tab click event
       If tabClick=*On;
          tabClick = *Off;
       Endif;

       //  History link popup.
       Readc sflCxHist;
       If lnkHistory = *on;
          DoU BtnHstCan = *On;
             LoadHistory();
             ExFmt RcdHistory;
          EndDo;
       Endif;

       //  RE-PRINT CANCELLATION/REINSTATMENT
       If BtnReprint = *on and scRepType <> ' ';

          // Chain rrnCxHist sflCxHist;
             // Start SEQ82216 - If user clicks on "Reprint" button without clicking on a
             //                  grid selection CompSuite return 0 to the program. The default
             //                  subfile RRN is 1 (top) of list; force a 1 if no selection
         If rrnCxRow = 0 ;
          Chain 1 sflCxHist ;
         Else ;
          Chain rrnCxRow sflCxHist;
         EndIf ;
             // End SEQ82216
          clear scErrmsg1;
          clear scErrmsg2;
          btnOKwdw12 = *Off;
        //***** beg chg ***** 10/08/14 ****************
        //If scCxRnEfDt = 0 and scRepType = 'R';
          If scCxRnEfDt = ZeroDate and scRepType = 'R';
        //***** end chg ***** 10/08/14 ****************
             scErrMsg1 = 'Reprint not allowed. This record has not been +
              reinstated.';
             iserror    = *On;
          EndIf;

        //***** beg chg ***** 10/25/19 ****************
        //** chain to the subfile record the user is on **//
          Chain(n) scCxHRRn# pcx0p;
          If Cxenfl = 'Y' and Cxrsn = '  ';
             Select;
             When scRepType = 'C';
                scErrMsg1 = 'Reprint for cancellations processed via ';
                scErrMsg2 = 'Deletion of Division Endorsements is not allowed';
             When scRepType = 'R';
                scErrMsg1 = 'Reprint for reinstatements processed via ';
                scErrMsg2 = 'Deletion of Division Endorsements is not allowed';
             EndSl;

             iserror    = *On;
          EndIf;
        //***** end chg ***** 10/25/19 ****************

          If iserror = *on;
             Dow btnOKwdw12 = *off;
                Exfmt rcdWdw12;
             Enddo;
          Else;
             DoU BtnCnwdw11= *On or BtnOKwdw11= *On;
                ExFmt RcdWDW11;
                If BtnOKwdw11 = *On;
                   Select;
                   When scRepType = 'C';
                      REPRT = REPRTC;
                      RprtTitle = 'Reprint Cancellation';
                   When scRepType = 'R';
                      REPRT = REPRTR;
                      RprtTitle = 'Reprint Reinstatment';
                   Endsl;
                   Exsr ReprintProc;
                Endif;
             EndDo;
          Endif;
       Endif;

       // Cancellation (Cancel or Revise Cancel)
       If BtnCxCan = *On;

          clear scErrmsg1;
          clear scErrmsg2;
          btnOKwdw12 = *Off;

      *** Beg Add *** 02/26/24 *** WC2956 **********************************
       // Check for Pending/Processing Flag.

          CanReiPending = *Off;

          SetLL (scCo#:scFnd:scEmp#) WDCXL10;
          ReadE (scCo#:scFnd:scEmp#) WDCXL10;
          Dow Not %EOF;
             If CXpdPr = 'Y';
                StrMsg = 'Cancellation can not Proceed!';
                PopOneBtn('This action cannot be taken because there +
                 is a prior Cancellation or Reinstatement action that has not +
                 processed. If no jobs are running or waiting to run, Please +
                 contact support to resolve this situation.':StrMsg:'OK');
                CanReiPending = *On;
                Leave;
             EndIf;
             ReadE (scCo#:scFnd:scEmp#) WDCXL10;
          EndDo;
          If CanReiPending = *Off;
      *** End Add *** 02/26/24 *** WC2956 **********************************

          scReq# = *Hival;
          Setgt  key1WDCXL PCX00;
          Readpe keyWDCXL  PCX00;

          If not %eof;

             // Do not allow revision of an auto cx of renewal year
             If CXcxdt <> 0 and CXcxdt < ELpfdt;
                clear scErrMsg1;
                clear scErrMsg2;
                scErrMsg1 = 'Revised Cancellation not allowed if employer +
                 cancel date';
                scErrMsg2 = 'is less than policy from date.';
                iserror   = *On;
             EndIf;

             //*** Beg Add ***** 01/24/24 ************************************
             // Do not allow Revise Cancellation of Division which has been deleted
             // in Mid-term Endorsement before its Rescind in another MTE
             If CXcxdt <> 0 and CXenfl = 'Y' and CXrein <> 'Y';
                clear scErrMsg1;
                clear scErrMsg2;
                scErrMsg1 = 'Revise Cancellation not allowed if Division +
                 has been';
                scErrMsg2 = 'Deleted in Mid-term Endorsement';
                iserror   = *On;
             EndIf;
             //*** End Add ***** 01/24/24 ************************************

          EndIf;

          If iserror = *on;
             Dow btnOKwdw12 = *off;
                Exfmt rcdWdw12;
             Enddo;
          Else;
             Exsr PreCanInfoChk;
             If btnOKwdw01 <> *On;
                // Populate the popup window with selected record.
                If canEdtMode = *on;
       // *** beg add ***** 03/08/13 **********************************
                   scSRPR = ELsrpr;
       // *** end add ***** 03/08/13 **********************************
                   scReq# = *Hival;
                   Setgt  key1WDCXL PCX00;
                   Readpe keyWDCXL  PCX00;
                   scREQ#   = CXreq#;
                   scCxDate = dtMDYY(CXcxdt);
                   scCCRT   = CXccrt;
                   scRSN    = CXrsn;
                   xxKEY    = CXrsn;
                // SCRSND   = ' ';
                   scCOM1   = CXcom1;
                   scCOM2   = CXcom2;
                   scREV  = 'Y';
                   scRnstFee = CXrfee;
                   scAmtDue   = CXamtd;
                   scEOVR = 'N';
                   svRSN  = scRSN;
                   If CXrpmy <> 0;
                      amtVisb = *Off;
                      srMthvisb = *On;
                      scRPMY = dt6MDYY(CXrpmy);
                      svRPMY = scRPMY;
                   Else;
                      amtVisb = *On;
                      srMthvisb = *Off;
                      scRPMY = 0;
                      svRPMY = 0;
                   Endif;
                Else;
                   Clear scCxDate;
                   Clear scRnstFee;
                   Clear scAmtDue;
                   Clear scRSN;
                //******* Beg del ****** 05/14/15 ****************************
                // Clear scRSND;
                //******* End del ****** 05/14/15 ****************************
                   Clear scCOM1;
                   Clear scCOM2;
                   Clear scSRPR;
                   Clear scRPMY;
                   Clear scCCRT;
                   scREV  = 'N';
                   amtVisb = *On;
                   srMthvisb = *Off;
                Endif;
                //******* beg add ****** 08/16/2013 ****************************
                scCnxRrn = xxRrn;
                scCnxDate= CXcdat;
                scCnxMde = *off;
                If canAddMode = *on;
                   scCnxMde = *on;
                Endif;
                //******* end add ****** 08/16/2013 ****************************

                Exsr ClearCxErrors;
       //       Clear RcdCanInfo;
                DoU BtnCanInfo = *On or BtnCanSave= *On;
                   DoU BtnCanInfo = *On or CanInfoError = *Off;
                      ExFmt RcdCanInfo;
                      If BtnCanInfo = *Off;
                         Exsr CanInfoCheck;
                      Endif;
                   Enddo;
                //*** begin add ***  10/06/16  ****************************************
                   If btnCanSave = *on;
                      Exsr ScriptPrompt;
                   Endif;
                //*** end add *****  10/06/16  ****************************************
                Enddo;
                If BtnCanSave= *On;
                   Exsr CanInfoSave;
                   Exsr PGMENX;
                   // Process Renewal years
                   Exsr SRRNEW;
                   // Process other divs if they exist
                   If *In87 = *On;
                      Exsr SROTHW;
                   Endif;
                Endif;
             Endif;
          Endif;
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
          EndIf;
      *** End Add *** 02/26/24 *** WC2956 **********************************
       Endif;

       // Reinstatement Information
       reinstatement = *Off;
       If BtnReinst = *On;
          reinstatement = *On;
          // Populate the popup window with selected record.
          scReq# = *Hival;
          // Begin Delete **** 09/23/15 ***************************************
          // ** Setgt  key1WDCXL PCX00;
          // ** Readpe keyWDCXL  PCX00;
          //
          // Do not allow reinstatement of renewal year processed as
          // automatic flat cancellation.
          //
          //clear scErrmsg1;
          //clear scErrmsg2;
          //btnOKwdw12 = *Off;
          //If CXcxdt <> 0 and CXcxdt < ELpfdt;
          //   iserror = *On;
          //   scErrMsg1 = 'Renewal year policy cancellation. Reinstate prior +
          //    year.';
          //EndIf;
          // End  Delete **** 09/23/15 ****************************************

      *** Beg Add *** 02/26/24 *** WC2956 **********************************
       // Check for Pending/Processing Flag.

          CanReiPending = *Off;

          SetLL (scCo#:scFnd:scEmp#) WDCXL10;
          ReadE (scCo#:scFnd:scEmp#) WDCXL10;
          Dow Not %EOF;
             If CXpdPr = 'Y';
                StrMsg = 'Reinstatement can not Proceed!';
                PopOneBtn('This action cannot be taken because there +
                 is a prior Cancellation or Reinstatement action that has not +
                 processed. If no jobs are running or waiting to run, Please +
                 contact support to resolve this situation.':StrMsg:'OK');
                CanReiPending = *On;
                Leave;
             EndIf;
             ReadE (scCo#:scFnd:scEmp#) WDCXL10;
          EndDo;
          If CanReiPending = *Off;
      *** End Add *** 02/26/24 *** WC2956 **********************************
          // Begin Add *** 09/23/15 *******************************************

          // Do not allow reinstatement of renewal year processed as
          // automatic flat cancellation.

          clear scErrmsg1;
          clear scErrmsg2;
          btnOKwdw12 = *Off;
          Setgt  (scCO#:scFND:scEMP#:scDIV:scFYR:scREQ#) PCX02;
          Readpe (scCO#:scFND:scEMP#:scDIV:scFYR) PCX02;

          If not %eof and CXAFLT = 'Y';
             @CDAT = cxcdat;

             Dou %eof(WDCXL2) or cxfyr < scFYR;
                 Readpe (scCO#:scFND:scEMP#:scDIV) pcx02;
             Enddo;

             If not %eof and cxcdat = @CDAT;
              //*** beg chg ***** 01/30/17 *******************
              //*** If CXcxdt <> 0 and CXcxdt < ELpfdt;
                If CXcxdt <> 0 and CXcxdt <= ELpfdt;
              //*** end chg ***** 01/30/17 *******************
                   iserror = *On;
                   scErrMsg1 = 'Renewal year policy cancellation. Reinstate +
                   prior year.';
                EndIf;
             Endif;
          Endif;

          Setgt  key1WDCXL PCX00;
          Readpe keyWDCXL  PCX00;
          // End Add   *** 09/23/15 *******************************************

          // Reinstatement not allowed if CX issue date in the future.

          If currDate7 < CXcdat;
             clear scErrmsg1;
             clear scErrmsg2;
             scErrMsg1 = 'Cancellation has not yet been issued.';
             iserror    = *On;
          EndIf;

          //*** Beg Add ***** 01/24/24 ************************************
          // Do not allow Reinstatement of Division which has been deleted in Mid-term
          // Endorsement before its Rescind in another MTE
          If CXcxdt <> 0 and CXenfl = 'Y' and CXrein <> 'Y';
             clear scErrMsg1;
             clear scErrMsg2;
             scErrMsg1 = 'Reinstate not allowed if Division +
              has been';
             scErrMsg2 = 'Deleted in Mid-term Endorsement';
             iserror   = *On;
          EndIf;
          //*** End Add ***** 01/24/24 ************************************

          If iserror = *on;
             Dow btnOKwdw12 = *off;
                Exfmt rcdWdw12;
             Enddo;
          Else;
             scREQ# = CXreq#;
             scCXDTR = dtMDYY(CXcxdt);
             scREIN  = CXrein;
             scRCRT = 'N';

             If CXrdat = 0;
                scREV = 'N';
                scErr = 'N';
                revRnstFlg = *Off;
             Else;
                scREV = 'Y';
                scErr = CXerr;
                revRnstFlg = *On;
             Endif;

             scOERR = scERR;
             scCNC# = CXcnc#;
             scRCM1 = CXrcm1;
             scRCM2 = CXrcm2;
         // *** Beg Add ***** 07/24/13 ****************
             scRrCd = CXreCd;
         // *** End Add ***** 07/24/13 ****************
             scREDT = scCXDTR;

       // *** beg add ***** 09/03/13 **********************************
             // * Check for Additional Divisions
             divcnt = DivCount(scCo#:scFnd:scEmp#);
             addDivison = *Off;
             If divcnt > 1;
                addDivison = *On;
             Endif;

             // * Check for additional entities/locations and set indicator
             addEntLoc = *Off;
             Chain(N) keyWMALL PAL00;
             If %Found;
                addEntLoc = *On;
             Endif;
       // *** end add ***** 09/03/13 **********************************
             Exsr ClearRnErrors;
             DoU BtnReinCan = *On or BtnReinSav = *On;
                 DoU BtnReinCan = *On or RenInfoError = *Off;
                     ExFmt RcdRnstInf;
                     If BtnReinCan = *Off;
                        Exsr RnstInfoCheck;
                     Endif;
                 Enddo;
             //*** begin add ***  10/06/16  ****************************************
                If btnReinSav = *on;
                   Exsr ScriptPrompt;
                Endif;
             //*** end add *****  10/06/16  ****************************************
             Enddo;
             If BtnReinSav = *On;
                // *** beg add ***** 12/26/12 ****************
                @@Agt#A = 0;
                @@AnamA = ' ';
                @@ProdA = 0;
                @@PnamA = ' ';
                AgtAPI1(scCO#:scFND:scEMP#:scDIV:scFYR:@@Agt#A:
                        @@AnamA:@@ProdA:@@PnamA);
                scAGT# = @@Agt#A;
                scPROD = @@ProdA;
                // *** end add ***** 12/26/12 ****************
                Exsr RnstInfoSave;
                Exsr PGMENXR;
                // Process Renewal years
                Exsr SRRNEWR;
                // Process other divs if they exist
                If *In87 = *On;
                   Exsr SROTHWR;
                Endif;
             Endif;
          Endif;
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
          EndIf;
      *** End Add *** 02/26/24 *** WC2956 **********************************
       Endif;

       // Reset Buttons as per latest status
       setgt  keyWDCXL  pcx00;
       readpe keyWDCXL  pcx00;
       If not %eof;
          If CXcxdt <> 0;
             BtnRnstVal = 'Reinstate';
             BtnCxVal   = 'Revise Cancellation';
             canEdtMode = *On;
             canaddMode = *Off;
             rnstvisb   = *On;
          Else;
             // *** Beg Chg *** 06/20/14 *****************
             //BtnCxVal   = 'Cancel';
             BtnCxVal   = 'Issue Cancellation';
             // *** End Chg *** 06/20/14 *****************
             canaddMode = *On;
             canEdtMode = *Off;
             rnstvisb   = *Off;
          Endif;
          If CXredt <> 0;
             // *** Beg Chg *** 06/20/14 *****************
             //BtnCxVal = 'Cancel';
             BtnCxVal   = 'Issue Cancellation';
             // *** End Chg *** 06/20/14 *****************
             BtnRnstVal = 'Revise Reinstatement';
             canEdtMode = *Off;
             canaddMode = *On;
             rnstvisb   = *On;
          Endif;
       Endif;

       If btnRefresh = *On;
          btnRefresh =*Off;
       Endif;

       //*** Beg Add ********** 02/05/19 *************************
       If BtnCanSave = *On;
          BtnCanSave = *Off;
       Endif;

       If BtnReinSav = *On;
          BtnReinSav = *Off;
       Endif;

       If BtnReprint = *On;
          BtnReprint = *Off;
       Endif;
       //*** End Add ********** 02/05/19 *************************

       //* Menu Choices
         If scSelOpt <> *blanks;
            @@next=scSelOpt;
            Leave;
         Endif;

       //* Popup Windows
         Popup();

       Enddo;

       //***************************************************************************//
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;
          When TabIndex = 0;
             TabHeight = 2320;
             APnlHeight = TabHeight + 60;
             Exsr WriteAction;
             Write rcdTabPan;
             If dspMode = *On;
                rnstvisb = *Off;
                NotDspMode = *Off;
             Endif;
             Exfmt rcdAllInfo;
             Exsr ReadAction;
             Read rcdTabPan;
       EndSl;

       Endsr;
       //***************************************************************************//
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
       //***************************************************************************//
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
       //***************************************************************************//
      /end-free

     c*===============================================================
     c* RE-PRINT CANCELLATION/REINSTATMENT
     c*===============================================================
     c*
     c     ReprintProc   Begsr
     c*
      *** beg add *****  10/25/19  *****************************            ***********
      * call scriptura routine to prepare for reprint
     c*
     c                   Exsr      ScriptPrompt
     c*
      *** end add *****  10/25/19  *****************************            ***********
     c*
     c*  Submit Cabcellation/Reinstatement Print Program
     c*
     c                   Move      scCO#         qqCO#             3
     c                   Move      scFND         qqFND             3
     c                   Move      scFYR         qqFYR             3
     c                   Move      scEMP#        qqEMP#            9
     c                   Move      scDIV         qqDIV             5
     c                   Move      scCxReq#      qqREQ#            5
     c*  Get user outq
     c     Q1user        Chain     PUP00
     c                   MoveL(P)  UPoutq        qqOUTQ           10
     c                   Move      scTYPERP      qqTYPE            1
     c*
     c                   Call      REPRT
     c                   PARM                    qqCO#
     c                   PARM                    qqFND
     c                   PARM                    qqFYR
     c                   PARM                    qqEMP#
     c                   PARM                    qqDIV
     c                   PARM                    qqREQ#
     c*** begin chg ***  09/08/22  ************************************
     c*****              PARM                    scTYPERP
     c                   PARM                    scCERTRP
     c*** end chg *****  09/08/22  ************************************
     c                   PARM      'Y'           qqREPR            1
     c                   PARM                    qqOUTQ
     c                   PARM                    qqTYPE
     c*
      *** Begin Update ****
      *** Begin Del *** 09/14/22 WC2880 ******************
     c***       Call      'WA007R'
      *** End   Del *** 09/14/22 WC2880 ******************
     c*
     c                   Endsr
     c*

      *===============================================================
      * PreCanInfoChk - Subroutine before displaying Cancellation Info Window.
      *===============================================================
      *
     c     PreCanInfoChk BEGSR

     c*  Call subroutine to check if account can be cancelled
     c                   Exsr      CHKACT
     c                   If        btnOKwdw01 = *On
     c                   LeaveSr
     c                   Endif
      *
     c                   Move      'N'           xxRnew
      *
      *  Check for Additional Divisions
      *
     c                   eval      divcnt = DivCount(scCo#:scFnd:scEmp#)
     c                   eval      addDivison = *Off
     c                   If        divcnt > 1
     c                   eval      addDivison = *On
     c                   Endif

     c*  Check for additional entities/locations and set indicator
     c                   Eval      addEntLoc = *Off
     c     keyWMALL      Chain(n)  PAL00
     c                   If        %found
     c                   eval      addEntLoc = *On
     c                   Endif
     c*
      *  Get Employer Cross Reference info for header
      *
     c     keyWZEXL      Chain     PEX02
     c                   If        %found
     c                   eval      scOFnd = EXofnd
     c                   eval      scOEmp = EXoemp
     c                   eval      scODiv = EXodiv
     c                   Endif
      *
      *  Flag if Client is Enrolled in OV/400
     c                   MoveL     'SY'          xxSYK             2
     c     xxsyk         Chain(n)  SSY01
     c                   If        %found
     c                   eval      ov400Enrolled = SYofif = 'Y'
     c                   Endif

     c     canaddMode    IFEQ      *On
      *    Set Reprint Flag to 'N'
     c                   Move      'N'           QQrepr            1
      *    Get the next Request #
     c                   Move      *HIVAL        scREQ#
     C     key1WDCXL     Setgt     PCX00
     C     keyWDCXL      Readpe    PCX00                                  60
     C     *IN60         IFEQ      *OFF
     C     CXreq#        Add       1             scREQ#
     C                   Else
     C                   Z-Add     1             scREQ#
     C                   Endif                                                  *IN60=*OFF
     c                   Eval      reviseFlag = *Off
     c                   Else
      *    Set Reprint Flag to 'Y'
     c                   Move      'Y'           QQrepr            1
     c                   Eval      reviseflag = *On
     c                   Endif

      *  Get Agent Records
     c                   Call      AGTAPI        AGTPL
     c                   MoveL     ##ANAM        scANAM
     c                   MoveL     ##PNAM        scPNAM

     c     dbrrn2        CHAIN(N)  PCX0P                              99
     c     QQrepr        IFEQ      'Y'                                          REPRINT
     c     canAddMode    oreq      *Off                                         REVISION
     c                   Z-ADD     CXcnc#        scCNC#
     c                   Else
     c                   Exsr      CancelCount                                  CALC CX COUNT
     c                   Endif
      *
     c                   Endsr                                                  PreCanInfoChk
       //***************************************************************************//
       //===================================================================
       // Cancellation Info popup  Checking
       //===================================================================
     c     CanInfoCheck  Begsr

     c                   Exsr      ClearCxErrors

     c                   If        canaddMode = *On
     c                   Exsr      CancelCount
     c                   Endif

      ********* beg del ****** 08/16/2013 *************************************
     c******             If        svRSN <> scRSN
     c******             MoveL(p)  scRSN         xxKey
     c******             Move      xxSt          kkSt
     c******             z-Add     currDate7     kkEfDt

     c******             If        canaddMode <> *On
     c******             z-Add     CXcdat        kkEfDt
     c******             Endif
      *
     c******             MoveL     scRSN         svRSN
      *
     c*****key2WTCRL     Setgt     pcr00
     c*****key3WTCRL     Readpe    pcr00                                  60

     c******IN60         Doweq     *OFF
     c*****CRstat        andeq     'I'
     c******* beg add ***** 01/16/13 **********************************
     c*****key3WTCRL     Readpe    pcr00
     c*****key3WTCRL     Readpe    pcr00                                  60
     c******* end add ***** 01/16/13 **********************************
     c*****              Enddo

     c*****              Z-add     CRrfee        scRnstFee
     c*****              Endif
      ********* end del ****** 08/16/2013 *************************************
     c*
     c*  Validate if not blank, else calculate date
     c                   If        scCxDate <> 0

      *  Validate Cancel Date

     c                   Z-Add     scCxDate      SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   MOVEL     SRDTO         scCxDate

     c                   If        srerr = *On
     c                   eval      CanInfoError = *On
     c                   eval      errCxDate  = *On
     c                   eval      msCxDate   = 'WCR0500'
     c                   LeaveSr
     c                   Endif                                                  SRERR=*ON

     c                   Else                                                   scCxDate

     c*  Validate the reason code
     c                   MoveL(p)  scrsn         xxKey
     c                   Move      xxst          kkSt
     c                   z-Add     currDate7     kkEfDt
      *
     c                   If        canAddMode <> *On
     c                   eval      kkEfDt = CXcdat
     c                   Endif
      *
     c*
     c     key2WTCRL     SETGT     pcr00
     c     key3WTCRL     READPE    pcr00                                  60
      *
     c     *IN60         DOWEQ     *OFF
     c     CRstat        ANDEQ     'I'
     c     key3WTCRL     READPE    pcr00                                  60
     c                   ENDDO
      *
     c                   If        *In60 = *On or CRstat = 'I'
     c                   eval      CanInfoError = *On
     c                   eval      erCxReason = *On
     c                   eval      msCxReason = 'WCR0513'
     c                   LeaveSr
     c                   Endif                                                  *IN60 IFEQ *ON

     c                   Exsr      SRCALC

     c                   Endif                                                  scCxDate
      *
      *  ENSURE CANCELLATION DATE IS WITHIN POLICY PERIOD.
      *
     c                   Z-ADD     scCxDate      SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMDp
     c                   MOVEL     SRDTO         XXCXDT
     c*
      // *** Beg Add *** 02/02/15 *****************
      /Free
       savCXDate = xxCXDT;
       savPFDT = ELpfdt;
       savPTDT = ELPTDT;
       savFYR = ELfyr;
      /End-Free
      // *** End Add *** 02/02/15 *****************
     c     xxCXDT        IFLT      ELPFDT
     c     xxCXDT        ORGT      ELPTDT
     c                   eval      CanInfoError = *On
     c                   eval      errCxDate1 = *On
     c                   eval      msCxDate1  = 'WCR0930'
     c                   LeaveSr
     c                   Endif                                                  ELPFDT>=XXCXDT>=ELPT
      *
      * CHECK FOR OPEN CLAIMS OR CLOSED CLAIMS WITH PAYMENTS, WHICH
      * HAVE AN ACCIDENT DATE GREATER THAN OR EQUAL TO THE CANCEL DATE
      *
     C     xxCXDT        IFLE      currDate7
     C     SCEOVR        ANDNE     'Y'
      *
     C                   MOVE      XXCXDT        XXACDT
      *
     C     keyWMCML11    SETLL     PCM11
     C     key1WMCML11   READE     PCM11                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     SCEOVR        ANDNE     'Y'
     C     erCxDate2     ANDEQ     *OFF
      *
     C                   XFoot     ClaimPayments xxPmts           13 2
      *
     C     CMstat        IFNE      'C'
     C     CMstat        OREQ      'C'
     C     xxPmTs        ANDNE     *ZEROS
      *
      *  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY
      *
     C                   MOVE      'N'           SCEOVR
      *
     C*
     C     xxky          CHAIN     PEX00                              60
     C                   eval      ovrvisb = *Off
      *
     C     *IN60         IFEQ      *OFF
     C     secLevel      ANDGE     EXOVRL
     C                   eval      ovrvisb = *On
     C                   Endif                                                  secLevel>= EXOVRL
      *
     c                   Dou       btnCnwdw06 = *On or btnOKwdw06 = *On
     C                   Exfmt     rcdWDW06
     c                   Enddo
     c                   If        btnCnwdw06 = *On
     c                   eval      CanInfoError = *On
     c                   Endif                                                  btnCnwdw05 = *On
      *
     C     SCEOVR        IFEQ      'N'
     c                   eval      CanInfoError = *On
     c                   eval      erCxDate2  = *On
     c                   eval      msCxDate2  = 'WC30181'
     c                   LeaveSr
     c                   Endif                                                  SCEOVR IFEQ 'N'
      *
     C                   Endif                                                  CMSTAT IFNE 'C'
      *
     C     key1WMCML11   READE     PCM11                                  60
      *
     C                   Enddo                                                  *IN60 DOWEQ *OFF
      *
     C                   Endif                                                  XXCXDT IFLE currDate
      *
      *
      *  VALIDATE REASON CODE
      *
     C                   MoveL(P)  scRSN         xxKEY
      *
     C*
     C                   MOVE      xxST          kkST
     C                   Z-ADD     currDate7     KKEFDT
      *
     C                   If        canaddMode <> *On
     C                   Z-ADD     CXcdat        kkefdt
     C                   Endif
      *
     C*
     C     key2WTCRL     SETGT     pcr00
     C     key3WTCRL     READPE    pcr00                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRstat        ANDEQ     'I'
     C     key3WTCRL     READPE    pcr00                                  60
     C                   ENDDO
      *
      *
     C     *IN60         IFEQ      *ON
     C     CRSTAT        OREQ      'I'
     c                   eval      CanInfoError = *On
     c                   eval      erCxRsn1   = *On
     c                   eval      msCxRsn1   = 'WCR0513'
     c                   LeaveSr
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
      * Check CX reason code valid for other divisons
      *
     C                   EXSR      CHKRSN
      *
      * REINSTATEMENT FEE CANNOT BE LESS THAN ZERO
      *
     C     scRnstFee     IFLT      0
     c                   eval      CanInfoError = *On
     c                   eval      erRnstFee  = *On
     c                   eval      msRnstFee  = 'WCR0547'
     c                   LeaveSr
     C                   ENDIF                                                  scRnstFeeLT 0
      *
      * AMOUNT DUE CANNOT BE LESS THAN ZERO
      *
     C     scAmtDue      IFLT      0
     c                   eval      CanInfoError = *On
     c                   eval      erAmtDue = *On
     c                   eval      msAmtDue = 'WCR0547'
     c                   LeaveSr
     C                   ENDIF                                                  scAmtDueFLT 0
     c*
     c     SCRPMY        IFNE      *ZERO
     c                   Z-ADD     SCRPMY        SRDTI
     c                   CALL      'SRCVTMY'     DATCHK
     c                   MOVEL     SRDTO         SCRPMY
     c
     c     SRERR         IFEQ      *ON
     c                   eval      CanInfoError = *On
     c                   eval      erSrMth  = *On
     c                   eval      msSrMth  = 'WCR0500'
     c                   Endif
     c                   Endif
      *
      *
      * REVISED FLAG MUST BE 'Y' IF CANCEL DATE, REASON, COMMENTS OR
      * AMOUNT DUE WAS CHANGED.
      *
     C     canaddMode    IFEQ      *OFF
     C     SCREV         ANDEQ     'N'
      *
     C     XXCXDT        IFNE      CXCXDT
     C     SCRSN         ORNE      CXRSN
     C     SCCOM1        ORNE      CXCOM1
     C     SCCOM2        ORNE      CXCOM2
     c     scAmtDue      ORNE      CXAMTD
     c     scRPMY        ORNE      svRPMY
      *
     c                   eval      CanInfoError = *On
     c                   eval      erRevCxN = *On
     c                   eval      msRevCxN = 'WC30124'
     c                   LeaveSr
      *
     c                   ENDIF                                                  XXCXDT IFNE CXCXDT
      *
     c                   ENDIF                                                  canaddMode *Off
      *
      * WARNING if Revised Cancellation
      *
     c                   If        dspMode = *Off and scREV = 'Y'
     c                   Dou       btnCnwdw04 = *On or btnOKwdw04 = *On
     c                   Exfmt     rcdWDW04
     c                   Enddo
     c                   If        btnCnwdw04 = *On
     c                   eval      CanInfoError = *On
     c                   Endif                                                  btnCnwdw05 = *On
     c                   Endif                                                  dspMode = *Off
      *
      *  Display window if renewal year processing to be done
      *
     c                   If        dspMode = *Off and scREV <> 'Y'
     C     keyWDELL8     SETGT     PEL08
     C     key1WDELL8    READE     PEL08                                  60
      *
     C     *IN60         IFEQ      *OFF
     c                   Dou       btnCnwdw05 = *On or btnOKwdw05 = *On
     C                   Exfmt     rcdWDW05
     c                   Enddo
     c                   If        btnCnwdw05 = *On
     c                   eval      CanInfoError = *On
     c                   Endif                                                  btnCnwdw05 = *On
     c                   Endif
      *
     c     xxRrn         CHAIN     WDELP                              60
     c                   ENDIF
     c******* beg add ***** 03/08/13 **********************************
     c                   If        scSRPR = *Blanks
     c                   Eval      CanInfoError = *On
     c                   Eval      erCxRate = *On
     c                   Eval      msCxRate = 'WCR3411'
     c                   LeaveSr
     c                   Endif                                                  SCSRPR = *Blanks
     c******* end add ***** 03/08/13 **********************************
      *
     c*
     c* Call subroutine to check if account can be cancelled
     c                   EXSR      CHKACT
     c*
     c* Check for other divs eligible for cancellation/revision
     c                   EXSR      SROTHD

     c                   Endsr
       //***************************************************************************//
       //===================================================================
       // Clear Cancellation information screen Errors
       //===================================================================
     C     ClearCxErrors Begsr
      /free

       CanInfoError = *Off;

       errCxDate  = *off;
       msCxDate   = *blanks;
       erCxReason = *off;
       msCxReason = *blanks;
       errCxDate1 = *off;
       msCxDate1  = *blanks;
       errCxDate1 = *off;
       msCxDate1  = *blanks;
       erCxDate2  = *off;
       msCxDate2  = *blanks;
       erCxRsn1   = *off;
       msCxRsn1   = *blanks;
       erRnstFee  = *off;
       msRnstFee  = *blanks;
       erAmtDue   = *off;
       msAmtDue   = *blanks;
       erRevCxN   = *off;
       msRevCxN   = *blanks;
       erCxRsn2   = *off;
       msCxRsn2   = *blanks;
       erSrMth  = *Off;
       msSrMth  = *blanks;
       // *** beg add ***** 03/08/13 **********************************
       erCxRate   = *off;
       msCxRate   = *blanks;
       // *** end add ***** 03/08/13 **********************************

      /end-free
     C                   Endsr                                                  ClearErrors
       //===================================================================
       // Clear Reinstatement information screen Errors
       //===================================================================
     c     ClearRnErrors Begsr
      /free

       RenInfoError = *Off;

       erCxError  = *Off;
       msCxError  = *blanks;
       // ***  Beg Add *** 07/24/13  **********************************
       erReError  = *Off;
       msReError  = *blanks;
       // ***  End Add ***  07/24/13  **********************************

      /end-free
     c                   Endsr
     C*===============================================================
     C* SROTHD - Checks for other divs to cancel/revise cancel
     C*===============================================================
     c     SROTHD        BEGSR
     c                   Z-ADD     SCDIV         SAVDIV
     c                   Z-ADD     SCREQ#        SVREQ#
     c                   SETOFF                                       87

     c*   Cycle through all divisions
     c     EMKYX         CHAIN(N)  PEM00                              60
     c     *IN60         DOWEQ     *OFF
     c     *IN87         ANDEQ     *OFF
     c     *IN88         ANDEQ     *OFF
     c     EMDIV         IFNE      SAVDIV
     c                   Z-ADD     EMDIV         SCDIV
     c     key1WDELL1    CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF

     c*     Set indicator if another div is found for cancellation
     c                   EXSR      SRGETX
     c     CD(X)         IFEQ      *ZERO
     c     CD(X)         ORGT      XXCXDT
     c     SCREV         OREQ      'Y'
     c                   SETON                                        87
     c                   Endif                                                  CD,X=*ZERO
     c                   Endif                                                  *IN60=*OFF
     c                   Endif                                                  EMDIV<>SAVDIV
     c     EMKYX         READE(N)  PEM00                                  60
     c                   Enddo

     c*   If there are multiple divs for cancellation then output msg
     c     SCREV         COMP      'Y'                                    88
     c     *IN87         IFEQ      *ON
     c     CanInfoError  ANDNE     *ON
     c                   Dou       btnCnwdw03 = *On or btnOKwdw03 = *On
     c                   Exfmt     rcdWDW03
     c                   Enddo

     c                   If        btnCnwdw03 = *On
     c                   eval      CanInfoError = *On
     c                   Endif
     c                   Endif

     C*  Reset emp master and canx detail files
     C                   Z-ADD     SAVDIV        SCDIV
     C                   Z-ADD     SVREQ#        SCREQ#
     C     EMKY          CHAIN(N)  PEM00                              60
     C     key1WDCXL     CHAIN     PCX00                              60

     C*  Reset emp detail file
     C     key1WDELL1    CHAIN     PEL01                              60
     C                   ENDSR                                                  SROTHD
      *===============================================================
      * CancelCount- Calculate cancellation count
      *===============================================================
     c     CancelCount   Begsr
      *
      *  Get Rating State
      *
     c                   eval      xxJrSt= GetRatingState(scCO#:scFND:scEMP#:
     c                             scDIV:scFYR)
      *
      *  Get Cancellation limit
      *
     c                   eval      kkEfDt = DateTo7(SystemDate)

     c     keyWTXCL1     SetGt     Pxc00
      *** Beg Chg ***** 07/22/15  **********************************
     c*******     key1WTXCL1    Readpe    Pxc00                           60
     c     key1WTXCL1    Reade     Pxc00                                  60
      *** End Chg ***** 07/22/15  **********************************

     c                   If        *In60 = *On
     c                   eval      xccxmn = 0
     c                   Endif                                                  *IN60 IFEQ *ON

      *  Get number of months back to look for CX

     c                   eval      x3Date = 0

     c                   If        xcCxMn <> 0
     c                   Z-Add     xcCXMN        ##MTHS

     c                   Call      'SRPMTH8'
     c                   Parm      currDate8     ##Date            8 0
     c                   Parm                    ##Mths            3 0
     c                   Parm      *Off          ##Err             1

     c                   IF        ##Err = *Off
     c                   Z-Add     ##DATE        SRDTI
     c                   Call      'SRCVTDAT'    DTCYMDp
     c                   MoveL     SRDTO         x3Date
     c                   Endif                                                  ##Err = *OFF

     c                   Endif                                                  xcCxMn <> 0

      *  Count CX for reason of non-payment in number of past months

     c                   Z-ADD     *ZERO         xx#CX            11 0
      *
      *   If called for Renewal year, save off current year values
      *
     c                   If        Reinstatement = *On
     C     *LIKE         DEFINE    SCCNC#        SVCNC#
     c                   If        xxRNEW = 'Y'
     c                   Z-Add     scCNC#        svCNC#
     c                   Endif
     c                   Endif

     c     keyWDCXL10    SETLL     Pcx10
     c     key1WDCXL10   READE     Pcx10                                  60

     c     *IN60         DOWEQ     *OFF

     c                   If        CXerr <> 'Y'
     c                   MoveL(p)  CXrsn         kkkey
     c                   Exsr      SRNCCI

     c                   If        CRncci =  '05'                               NON-PAY
     c                   Add       1             xx#Cx
     c                   Endif                                                  CRNCCI

     c                   Endif                                                  CXERR

     c     key1WDCXL10   ReadE     Pcx10                                  60
     c                   Enddo

     c                   z-add     xx#CX         sccnc#

     c     XXRNEW        IFEQ      'Y'
     c     Reinstatement Andeq     *On
     c                   Z-ADD     SCCNC#        CXCNC#
     c                   Z-ADD     SVCNC#        SCCNC#
     c                   ELSE
      *
      * Include current CX in count if for reason of non-payment
      *
     c                   MoveL(p)  scrsn         kkkey
     c                   Exsr      SRNCCI

     c                   If        CRncci  = '05'                               NON-PAY
     c                   Add       1             sccnc#
     c                   Endif

     c**** @@RRN         CHAIN(N)  DBRCDF                             99
      *
     c                   Endif
     c                   Endsr                                                  CancelCount

      *===============================================================
      * SRNCCI - GET NCCI CX REASON CODE
      *===============================================================
     C     SRNCCI        Begsr
      *
     C     keyWTCRL      Setgt     pcr00
      *
     C     *IN60         DOUEQ     *ON
     C     CRSTAT        ORNE      'I'
     C     key1WTCRL     READPE    pcr00                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *On
     C                   Clear                   CRNCCI
     C                   Endif
      *
     C                   Endsr                                                  SRNCCI
      *
     C*===============================================================
     C* SRCALC - CALCULATES THE NUMBER OF DAYS TO ADD TO THE ISSUE
     C*          DATE TO DETERMINE THE CANX EFFECTIVE DATE
     c*===============================================================
     c     SRCALC        BEGSR
     c*
     c                   Z-ADD     *ZERO         ZZDAYS            4 0

     c*  Cycle through all divs for employer
     c     keyWDELL1     CHAIN     PEL01                              60
     c     *IN60         DOWEQ     *OFF

      /Free
       // Get jurisdiction state
       xxJrSt = GetRatingState(scCO#:scFND:scEMP#:ELdiv:scFyr);
      /End-free

     c                   If        xxJrSt <> ' '

     c*      Chain to canx reason code file
     c                   MOVEL     scRSN         xxKEY
     c                   MOVE      xxJrSt        kkST
     c                   Z-ADD     currDate7     kkEFDT

     c                   If        canaddMode <> *On
     c                   Z-ADD     CXCDAT        kkEFDT
     c                   ENDIF

     c     key2WTCRL     SETGT     pcr00
     c     key3WTCRL     READPE    pcr00                                  60

     c     *IN60         DOWEQ     *OFF
     c     CRSTAT        ANDEQ     'I'
     c     key3WTCRL     READPE    pcr00                                  60
     c                   ENDDO

      ************* Beg del ****** 05/14/15 ****************************
     c**N60              MOVEL(P)  CRdesc        SCRSND
      ************* End del ****** 05/14/15 ****************************
     c*

     c                   MOVE      ' '           POLNEW            1
     c     CRNDNW        IFGT      0
     c* Number of days to consider policy new
     C                   Z-ADD     CRNDNW        QQDAYS

     c** Calculate new work date
     C                   CALL      'SRPAST8'
     C                   PARM      currDate8     QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1

     c     QQERR         IFNE      '1'
     c                   Z-ADD     QQDT          Q@CXDT            8 0
     c                   Z-ADD     QQDT          SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMDp
     c                   MOVEL     SRDTO         X@WRDT            7 0
     c                   ENDIF                                                  QQERR<>'1'

     c                   Z-ADD     scCO#         #@CO#
     c                   Z-ADD     scFND         #@FND
     c                   Z-ADD     scEMP#        #@EMP#
     c                   Z-ADD     scDIV         #@DIV
     c                   Z-ADD     *ZEROS        #@DATE
     c                   CALL      'WA084R'
     c                   PARM                    #@CO#             3 0
     c                   PARM                    #@FND             3 0
     c                   PARM                    #@EMP#            9 0
     c                   PARM                    #@DIV             5 0
     c                   PARM                    #@DATE            7 0

     c                   MOVE      ' '           POLNEW            1
     c     #@DATE        IFGE      X@WRDT
     c                   MOVE      'Y'           POLNEW
     c                   ENDIF                                                  X#DATE > X@WRDT
     c                   ENDIF                                                  CRNDNW > 0
     c*
     c*      Calculate number of days to add to issue date
     c                   Z-ADD     0             XXDAYS            4 0

     c     ELCXDY        IFNE      0
     c                   ADD       ELCXDY        XXDAYS
     c                   ELSE
     c     POLNEW        IFNE      'Y'
     c                   ADD       CRNOTC        XXDAYS
     c                   ELSE
     c                   ADD       CRNTNP        XXDAYS
     c                   ENDIF                                                  POLNEW = 'Y'
     c                   ENDIF                                                  ELCXDY<>0

     c     ELADDM        IFNE      0
     c                   ADD       ELADDM        XXDAYS
     c                   ELSE
     c     POLNEW        IFNE      'Y'
     c                   ADD       CRADDM        XXDAYS
     c                   ELSE
     c                   ADD       CRMDNP        XXDAYS
     c                   ENDIF                                                  POLNEW = 'Y'
     c                   ENDIF                                                  ELADDM<>0

     c                   ENDIF                                                  *IN60=*OFF
     c*
     c     XXDAYS        IFGT      ZZDAYS
     c                   Z-ADD     XXDAYS        ZZDAYS
     c                   ENDIF                                                  XXDAYS>ZZDAYS

     c     keyWDELL1     READE     PEL01                                  60
     c                   ENDDO                                                  *IN60=*OFF

     c*  Add days to system date to get cx effective date
     c                   Z-ADD     ZZDAYS        QQDAYS

     c                   CALL      'SRFUTR8'
     c                   PARM      currDate8     QQDT              8 0
     c                   PARM                    QQDAYS            5 0
     c                   PARM                    QQERR             1

     c     QQERR         IFNE      '1'
     c                   Z-ADD     QQDT          scCxDate
     c                   Z-ADD     QQDT          SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMDp
     c                   MOVEL     SRDTO         XXCXDT
     c                   ENDIF                                                  QQERR<>'1'

     c*  Reset to look at correct employer detail record
     c     key1WDELL1    CHAIN     PEL01                              60

     c*  Check that cancellation date is within policy period.
     c     XXCXDT        IFLT      ELPFDT
     c     XXCXDT        ORGT      ELPTDT
     c                   eval      CanInfoError = *On
     c                   eval      errCxDate1 = *On
     c                   eval      msCxDate1  = 'WCR0930'
      ***** beg chg ***** 04/05/14 **********************************
     c*****              eval      scCxDate = 0
      ***** End chg ***** 04/05/14 **********************************
     c                   Endif                                                  ELPFDT>=XXCXDT>=ELPT

     c*  Display calculated date for confirmation
     c     errCxDate1    IFEQ      *OFF
     c*
     c                   Z-ADD     XXCXDT        SRDTI
     c                   CALL      'SRCVTDAT'    DTMDYYp
     c                   MoveL     SRDTO         SCW1DT
     c                   Dou       btnCnwdw02 = *On or btnOKwdw02 = *On
     c                   Exfmt     rcdWDW02
     c                   Enddo

     c                   If        btnCnwdw02 = *On
     c                   eval      CanInfoError = *On
     c                   Clear                   scCxDate
     c                   Endif

     c                   Endif                                                  errCxDate1 = *Off

     c                   Endsr                                                  SRCALC
     c*===============================================================
     c* CHKACT - Checks that employer can be cancelled.
     c*===============================================================
     c     CHKACT        BEGSR

      *** beg chg ***** 03/21/16 *************************************
     c*  Get Employer master record
     c***     EMKY          CHAIN(N)  PEM00                              60
      /free
       // Check if state reporting division is cancelled
       kkStRptDiv = GetStRptDv(scCo#:scFnd:scEmp#);
       Chain (scCo#:scFnd:scEmp#:kkStRptDiv) pem00;
      /end-free
      *** end chg ***** 03/21/16 *************************************

     c*  The employer must be effective in order to cancel,
     c*  otherwise send an error message displaying the current
     c*  cancel date
     c     canaddMode    IFEQ      *On
     c*
     c                   Eval      visbMSG1 = *Off
     c                   Eval      visbMSG2 = *Off
     c                   Eval      visbMSG3 = *Off
     c                   Z-add     0             scCX1
     c                   Z-add     0             scCX2
     c                   Move      *Blanks       scMSG1
     c                   Move      *Blanks       scMSG2

     c     keyWXCNL      CHAIN     PCN00                              60
     c     *IN60         IFEQ      *OFF
     c                   MOVE      *ON           visbMSG3
     c                   Endif                                                  *IN60=*OFF

     c                   Z-Add     1             X                 3 0
     c     *Zero         LOOKUP    ED(X)                                  60
     c  N60              Z-ADD     5             X
     c   60              SUB       1             X

     c                   If        visbMSG3 = *Off
     c     X             IFNE      *ZERO
     c*
     c     CD(X)         IFNE      *ZERO

     c                   Z-ADD     CD(X)         SRDTI
     c                   CALL      'SRCVTDAT'    DTMDYYp
     c                   MOVEL     SRDTO         SCCX1

     c                   Z-ADD     CD(X)         SRDTI
     c                   CALL      'SRCVTDAT'    DTMDYYp
     c                   MOVEL     SRDTO         SCCX2

     c                   Z-ADD     currDate8     SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMDp
     c                   MOVEL     SRDTO         currDate7

     c     CD(X)         COMP      currDate7                            24
     c                   eval      visbMSG2 = *In24
     c   24              MOVEL     MSG2          SCMSG2
     c  N24              MOVEL     MSG1          SCMSG1
     c  N24              MOVE      *ON           visbMSG1

     c                   ENDIF                                                  CD,X<>*ZERO

     c                   ENDIF                                                  X<>*ZERO
     c                   ENDIF                                                  visbMSG3 = *Off

     c     visbMSG2      IFEQ      *ON
     c     visbMSG1      OREQ      *ON
     c     VisbMSG3      OREQ      *ON
     c                   Dou       btnOKwdw01 = *On
     c                   If        scmsg1 = ' ' and scmsg2 = ' '
     c                             and visbMSG3 = *On
     c                   eval      scmsg1 = 'by the mass cancellation process.'
     c                   Endif
     c                   Exfmt     rcdWDW01
     c                   Enddo
     c                   Endif

     c                   Endif                                                  @@MODE=A

      *** beg add ***** 03/21/16 ********************
      /free
       // Reset Employer Master
       Chain emKy pem00;
      /end-free
      *** end add ***** 03/21/16 ********************
     c                   Endsr                                                  CHKACT

      *===============================================================
      * CHKRSN - Check CX reason code valid for other employer divisons
      *===============================================================
     c     CHKRSN        BEGSR
      *
     c                   Z-ADD     SCDIV         SAVDIV
     c                   eval      erCxRsn2   = *Off
     c                   eval      msCxRsn2   = ' '
      *
      *   Cycle through all divisions
      *
     c     EMKYX         CHAIN(N)  PEM00                              60

     c     *IN60         DOWEQ     *OFF
     c     erCxRsn2      ANDEQ     *OFF

     c     EMDIV         IFNE      SAVDIV
     c                   Z-ADD     EMDIV         SCDIV
     c     key1WDELL1    CHAIN     PEL01                              60

     c     *IN60         IFEQ      *OFF
     c                   EXSR      SRGETX

     c     CD(X)         IFEQ      *ZERO
     c     CD(X)         ORGT      XXCXDT
     c     SCREV         OREQ      'Y'
     c     key1WMRSL     CHAIN     Prs00                              60
      *
      *  VALIDATE REASON CODE
      *
     c     *IN60         IFEQ      *OFF
     c                   MOVEL(P)  SCRSN         XXKEY
     c                   MOVE      RSJRST        KKST
     c                   Z-ADD     currDate7     KKEFDT

     c     canaddMode    IFNE      *On
     c                   Z-ADD     CXCDAT        KKEFDT
     c                   ENDIF

     c     key2WTCRL     SETGT     pcr00
     c     key3WTCRL     READPE    pcr00                                  60

     c     *IN60         DOWEQ     *OFF
     c     CRSTAT        ANDEQ     'I'
     c     key3WTCRL     READPE    pcr00                                  60
     c                   ENDDO

     c     *IN60         IFEQ      *ON
     c     CRSTAT        OREQ      'I'
     c                   eval      CanInfoError = *On
     c                   eval      erCxRsn2   = *On
     c                   eval      msCxRsn2   = 'WC31494'
     c                   ENDIF                                                  *IN60 IFEQ *ON

     c                   ENDIF                                                  keyWMRSL

     c                   ENDIF                                                  CD,X=*ZERO

     c                   ENDIF                                                  key1WDELL1

     c                   ENDIF                                                  EMDIV<>SAVDIV

     c     EMKYX         READE(N)  PEM00                                  60
     c                   ENDDO
      *
      *  Reset files
     c                   Z-ADD     SAVDIV        SCDIV
     c     EMKY          CHAIN(N)  PEM00                              60
     c     key1WDELL1    CHAIN     PEL01                              60
      *
     c                   ENDSR                                                  CHKRSN
      *
     c*===============================================================
     c* SRGETX - Gets appropriate emp master eff and canx dates
     c*===============================================================
     c     SRGETX        BEGSR
     c*
     c*  Find policy effective date
     c                   Z-ADD     1             X
     c     *ZERO         LOOKUP    ED(X)                                  70
     c*
     c     ED(X)         IFNE      *ZERO
     c                   Z-ADD     5             X
     c                   ELSE
     c                   SUB       1             X
     c                   ENDIF                                                  ED,X<>*ZERO
     c*
     c     X             DOWGT     1
     c     ED(X)         IFGT      ELPFDT
     c                   SUB       1             X
     c                   ELSE
     c                   LEAVE
     c                   ENDIF                                                  ED,X>ELPFDT
     c                   ENDDO                                                  X=*ZERO
     c*
     c*  Determine if cancellation date exists, set canx indicator
     c     X             IFEQ      *ZERO
     c                   MOVE      *OFF          cancelDtExist
     c                   ELSE
     c     CD(X)         IFEQ      *ZEROS
     c                   MOVE      *OFF          cancelDtExist
     c                   ELSE
     c                   MOVE      *On           cancelDtExist
     c                   ENDIF                                                  CD,X=*ZEROS
     c                   ENDIF                                                  X=*ZERO
     c*
     c                   ENDSR                                                  SRGETX
     C*===============================================================
     C* PGMENX - Processing to be done for each cancelled emp/div
     C*===============================================================
     C     PGMENX        BEGSR
     C*
     C*  Only do if not in inquiry and enter was pressed
     C     dspMode       IFEQ      *OFF
     C     @@MODE        ANDNE     'E'
     C*
     C*  If this is a revised cancellation, need to re-update the
     C*  cancellation date in WMEMP before printing
     C     SCREV         IFEQ      'Y'
     C                   MOVE      'N'           QQREPR
     C                   ENDIF                                                  SCREV=Y
     C*
     C*  Update the cx date for the employer
     C*
     C     XXRNEW        IFEQ      'N'
     C     EMKY3         CHAIN     PEM03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             XX                1 0
     C     *ZERO         LOOKUP    ED(XX)                                 60
     C*
     C     ED(XX)        IFNE      *ZERO
     C                   Z-ADD     5             XX
     C                   ELSE
     c                   SUB       1             XX
     c                   ENDIF                                                  ED,XX<>*ZERO
     c*
     c                   Z-ADD     XXCXDT        CD(XX)
     c*
     c                   MOVE      Q1USER        EMUSER
     c                   MOVE      Q1PGM         EMPGM
     c*
     c                   Eval      EMdt = DateTo7(SystemDate)
     c                   Eval      EMtime = Timeto6(SystemTime())
     c*
     c                   UPDATE    PEM03
     c*
     c                   ENDIF
      *
     c                   ENDIF                                                  XXRENEW
     c*
     C*  Write a record to the cancellation history file if this is
     C*  the initial entry or if this is a revision
     C     XHKY          CHAIN     PXH00                              60
     C*
     C*
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      Q1USER        XHUSER
     C                   MOVE      Q1PGM         XHPGM
     c                   Eval      XHdt = DateTo7(SystemDate)
     c                   Eval      XHtime = Timeto6(SystemTime())
     C                   MOVE      *BLANKS       XHENFL
     C*
     C     *IN60         IFEQ      *ON
     C*
     c                   Z-ADD     scCO#         XHCO#
     c                   Z-ADD     scFND         XHFND
     c                   Z-ADD     scFYR         XHFYR
     c                   Z-ADD     scEMP#        XHEMP#
     c                   Z-ADD     scDIV         XHDIV
     c                   Z-ADD     scREQ#        XHREQ#
     c                   Z-ADD     scAGT#        XHAGT#
     c*
     c                   MOVE      *BLANK        XHRVFL
     c                   Z-ADD     CXREDT        XHREDT
     c                   Z-ADD     CXRDAT        XHRDAT
     c*
     c                   MOVE      scRSN         XHRSN
     c                   Eval      XHtrdt = DateTo7(SystemDate)
     c                   Z-ADD     CXCXDT        XHCXDT
     c                   Z-ADD     CXCDAT        XHCDAT
     c*
     c                   Z-ADD     CXCNC#        XHCNC#
     c                   WRITE     PXH00
     c*
     c                   ELSE
     c*
     c     SCREV         IFEQ      'Y'
     c*
     c                   MOVE      'R'           XHRVFL
     c*
     c                   MOVE      SCRSN         XHRSN
     c                   Eval      XHtrdt = DateTo7(SystemDate)
     c                   Z-ADD     CXCXDT        XHCXDT
     c                   Z-ADD     CXCDAT        XHCDAT
     c*
     c                   Z-ADD     CXCNC#        XHCNC#
     c                   WRITE     PXH00
     c*
     c                   ENDIF                                                   SCREV=Y
     c*
     c                   ENDIF                                                   *IN61=*ON
      *
     c*                  UNLOCK    WDXHL
     c*
     c*  Submit cancellation print program
     c                   MOVE      scCO#         QQCO#             3
     c                   MOVE      scFND         QQFND             3
     c                   MOVE      scFYR         QQFYR             3
     c                   MOVE      scEMP#        QQEMP#            9
     c                   MOVE      scDIV         QQDIV             5
     c                   MOVE      scREQ#        QQREQ#            5
     c*
     c*  Get user outq
     c                   MOVEL(P)  ##OUTQ        QQOUTQ           10
     c*
     c                   CALL      'W7165C'                             99
     c                   PARM                    QQCO#
     c                   PARM                    QQFND
     c                   PARM                    QQFYR
     c                   PARM                    QQEMP#
     c                   PARM                    QQDIV
     c                   PARM                    QQREQ#
     c                   PARM                    SCCCRT
     c                   PARM                    QQREPR
     c                   PARM                    QQOUTQ
     c                   PARM                    scTYPE
     c*
     c     XXRNEW        IFEQ      'N'
      *** Begin Del *** 09/14/22 WC2880 ******************
     c***  87              CALL      'WA007R'                             99
      *** End   Del *** 09/14/22 WC2880 ******************
     c                   ENDIF                                                  XXRENEW
     c*
     c                   ENDIF                                                  *IN09,KC,KL=*OFF
     c*
     c                   Endsr                                                  PGMENX

      *===============================================================
      * SRRNEW - Process Renewal Years
      *===============================================================
     c     SRRNEW        BEGSR

     c*  Only do if not in inquiry and enter was pressed
     c     dspMode       IFEQ      *OFF
     c     @@MODE        ANDNE     'E'
     c                   MOVE      'Y'           XXRNEW            1

      *  If not revised CX or Reprint
     c     SCREV         IFNE      'Y'
     c     QQREPR        ANDEQ     'N'
     c                   Z-ADD     SCFYR         S1FYR
     c                   Z-ADD     SCREQ#        S1REQ#

      *  For each renewal policy year for employer division
      *
     c     keyWDELL      SETGT     PEL00
     c     key1WDELL     READE     PEL00                                  60

     c     *IN60         DOWEQ     *OFF
     c                   Z-ADD     ELFYR         SCFYR

      *    If not cancelled, process CX of renewal year
     c                   Z-ADD     *HIVAL        SCREQ#
     c     key1WDCXL     SETGT     PCX00
     c     keyWDCXL      READPE    PCX00                                  60
      *
     c     *IN60         IFEQ      *ON                                          NOT CX
     c     CXREIN        OREQ      'Y'                                          OR REINSTATED
     c   60              Z-ADD     1             SCREQ#
     c  N60CXREQ#        ADD       1             SCREQ#
     c                   CALL      AGTAPI        AGTPL
     c                   EXSR      CanInfoSave
     c                   EXSR      PGMENX
      *** Beg Add ***** 01/24/24 ************************************
     c                   ELSE
      * If Divison got deleted in Mid-term endorsement and not rescinded yet
      * Write new record with next seq#
     c                   If        xxcxdt <= CXcxdt and CXenfl = 'Y'
     c     CXREQ#        ADD       1             SCREQ#
     c                   CALL      AGTAPI        AGTPL
     c                   EXSR      CanInfoSave
     c                   EXSR      PGMENX
     c                   Endif
      *** End Add ***** 01/24/24 ************************************
     c                   ENDIF                                                  CXKY
      *
     c     key1WDELL     READE     PEL00                                  60
     c                   ENDDO                                                  *IN90
      *
      * Reset
      *
     c                   Z-ADD     S1FYR         SCFYR
     c                   Z-ADD     S1REQ#        SCREQ#
     c     key1WDELL1    CHAIN     PEL01                              60
     c                   CALL      AGTAPI        AGTPL
     c     key1WDCXL     CHAIN     PCX00                              60
     c                   ENDIF
      *
     c                   MOVE      'N'           XXRNEW            1
      *
     c                   ENDIF
      *
     c                   ENDSR                                                  SRRNEW
      *
     c*===============================================================
     c* SROTHW - Writes/Updates cancellation records for other divs
     c*===============================================================
     c     SROTHW        BEGSR
     c*
     c                   SETOFF                                       87
     c                   Z-ADD     SCDIV         SAVDIV
     c                   Z-ADD     SCREQ#        SVREQ#
     c*
     c*   Cycle through all divisions
     c     EMKYX         CHAIN(N)  PEM00                              60
     c     *IN60         DOWEQ     *OFF
     c*
     c     EMDIV         IFNE      SAVDIV
     c                   Z-ADD     EMDIV         SCDIV
     c*
     c     KEY1WDELL1    CHAIN     PEL01                              60
     c     *IN60         IFEQ      *OFF
      // *** Beg Add *** 02/02/15 *****************
      /Free
       If savCXDate < ELpfdt And ELpfdt > savPFDT And
           ELfyr = savFYR And ELptdt <= savPTDT;
          xxCXDT = ELpfdt;
       Else;
          xxCXDT = savCXDate;
       Endif;
      /End-Free
      // *** End Add *** 02/02/15 *****************
     c     XXCXDT        IFGE      ELPFDT
     c     XXCXDT        ANDLE     ELPTDT
     c*
     c*     Get appropriate effective and cancel dates from emp master
     c                   EXSR      SRGETX
     c*
     c*     Update employer master with cancel date
     c     CD(X)         IFEQ      *ZERO
     c     CD(X)         ORGT      XXCXDT
     c     SCREV         OREQ      'Y'
     c*
     c*     Find the latest cancellation
     c                   Z-ADD     *HIVAL        SCREQ#
     c     key1WDCXL     SETGT     PCX00
     c     keyWDCXL      READPE    PCX00                                  60
     c*
     c*       Add cancellation record
     c     *IN60         IFEQ      *ON
     c     CXREIN        OREQ      'Y'
     c*
     c   60              Z-ADD     1             SCREQ#
     c  N60CXREQ#        ADD       1             SCREQ#
     c*
     c                   CALL      AGTAPI        AGTPL
     c                   Exsr      CanInfoSave
     c                   Exsr      PGMENX
      *
      *    Process Renewal years
      *
     c                   EXSR      SRRNEW
      *
      *** Beg Add ***** 01/24/24 ************************************
     c                   ELSE
      * If Divison got deleted in Mid-term endorsement and not rescinded yet
      * Write new record with next seq#
     c                   If        xxcxdt <= CXcxdt and CXenfl = 'Y'
     c     CXREQ#        ADD       1             SCREQ#
     c                   CALL      AGTAPI        AGTPL
     c                   EXSR      CanInfoSave
     c                   EXSR      PGMENX
      *** End Add ***** 01/24/24 ************************************
     c                   ELSE
      *
      *  Revise a cancellation record
     c*
     c                   Z-ADD     CXREQ#        SCREQ#
     c*
     c                   CALL      AGTAPI        AGTPL
     c                   EXSR      CanInfoSave
     c                   EXSR      PGMENX
     c*
      *** Beg Add ***** 01/24/24 ************************************
     c                   Endif
      *** End Add ***** 01/24/24 ************************************
     c                   ENDIF                                                  *IN60=*OFF
     c*
     c*
     c                   ENDIF                                                  CD,X=*ZERO
     c                   ENDIF                                                  XXCXDT>=ELPFDT
     c                   ENDIF                                                  *IN60=*OFF
     c*
     c                   ENDIF                                                  EMDIV<>SAVDIV
     c     EMKYX         READE(N)  PEM00                                  60
     c                   ENDDO
     c*
     c*  Reset emp master and canx detail files
     c                   Z-ADD     SAVDIV        SCDIV
     c                   Z-ADD     SVREQ#        SCREQ#
     c     EMKY          CHAIN(N)  PEM00                              60
     c     key1WDCXL     CHAIN     PCX00                              60
     c*
     c                   ENDSR                                                  SROTHW
      *
      *
       //***************************************************************************//
       //===================================================================
       // Cancellation Info popup  Save
       //===================================================================
     c     CanInfoSave   Begsr
      /free

       If canaddMode <>  *On;
          Chain DBRRN2 PCX0P;
       Else;
          Clear PCX0P;
          CXco#  = scCO#;
          CXfnd  = scFND;
          CXemp# = scEMP#;
          CXdiv  = scDIV;
          CXfyr  = scFYR;
          CXreq# = scREQ#;
          // *** beg add ***** 12/26/12 **********************************
          CXagt# = scAGT#;
          // *** end add ***** 12/26/12 **********************************

          CXipgm = Q1pgm;
          CXiusr = Q1user;
          CXitm  = Timeto6(SystemTime());
          CXidt  = Dateto7(SystemDate());

          Cxcdat = DateTo7(SystemDate);
          CXcfdt = 0;
          CXCFPD = 0;
          CXCFFL = ' ';
          CXerr  = ' ';
          CXenfl = ' ';
          CXredt = 0;
          CXrdat = 0;
          CXrcrt = 'N';
          CXrdrt = ' ';

       Endif;

       // *** beg add ***** 12/26/12 **********************************
       If CXagt# = *Zeros;
          CXagt# = scAGT#;
       Endif;
       // *** end add ***** 12/26/12 **********************************
       // *** begin add *** 03/04/15 **********************************
       //*******CXcxdt = dtCYMD(scCxDate);
       CXcxdt = xxCXDT;
       // *** end chg ***** 03/04/15 **********************************
       CXrsn  = scRSN;
       CXccrt = scCCRT;
       CXcom1 = scCOM1;
       CXCom2 = scCOM2;
       CXrfee = scRnstFee;
       CXamtd = scAmtDue;
       CXstat = ' ';
       CXrein = 'N';
       CXrpmy = dt5CYMD(scRPMY);
       CXrst  = GetRatingState(scCO#:scFND:scEMP#:scDIV:scFYR);

       //* Renewal Year Processing
       If xxRNEW = 'Y';
          CXaflt = 'Y';
       // *** Beg add ***** 10/03/13 **********************************
       //**************** CXcxdt = ELpfdt;
       // *** End add ***** 10/03/13 **********************************
       // *** Beg add ***** 11/08/13 **********************************
          CXcxdt = ELpfdt;
       // *** End add ***** 11/08/13 **********************************
          CXccrt = 'N';
          CXrfee = 0;
       Endif;
       CXcnc# = scCNC#;

      *** Beg Add *** 02/26/24 *** WC2956 **********************************
       CXpdPr = 'Y';
      *** Beg Add *** 02/26/24 *** WC2956 **********************************

       CXpgm  = Q1PGM;
       CXuser = Q1USER;
       CXtime = Timeto6(SystemTime());
       CXdt   = Dateto7(SystemDate());

       If canaddMode <>  *On;
          If scRev = 'Y'or *In87 = *On;
             If CXcdat <= currDate7;
                CXcdat = currDate7;
             Endif;
          Endif;
          CXenfl = ' ';
          Update pcx0p;
       Else;
          Write pcx0p;
       Endif;

       // Short Rate or Prorate Audit Cancellation: updates the value in Policy Detail
       Chain keyWDELL PEL00;
       If %found;
           ELsrpr = scSRPR;
           Update PEL00;
       Endif;

      /end-free
     c                   Endsr
       //***************************************************************************//

       //===================================================================
       // Reinstatement Info popup  Checking
       //===================================================================
     c     RnstInfoCheck Begsr

     c                   Exsr      ClearRnErrors

      ***  Beg Add ***  07/24/13  **********************************
      /Free
        xxJrSt= GetRatingState(scCO#:scFND:scEMP#:scDIV:scFYR);
        kkEfDt = DateTo7(SystemDate);
        Setgt  keyWTXCL  pxc01;
        Readpe key1WTXCL pxc01;
        If Not %Eof(wtxcl);
           If X1rrCd = 'Y' And scRrCd = *Blanks;
              RenInfoError = *On;
              erReError    = *On;
              msReError    = 'WCR3409';
              LeaveSr;
           EndIf;
        Endif ;
      /End-Free
      ***  End Add ***  07/24/13  **********************************

      *  If a 'Y' is entered in 'CX in error' field, move zero CX count.
     c                   If        scERR = 'Y'
     c                   Eval      scCNC# = 0
     c                   Endif                                                  scERR = 'Y'
      *
      *  If a 'N' is entered in 'CX in error' field and field previously
      *  contained a 'Y', then recalculate cancellation count.
      *
     c                   If        scERR = 'N' and scOERR = 'Y'
     c                   Exsr      CancelCount
     c                   Endif                                                  scERR
      *
      *  If CX number greater than limit and not CX in error, disallow
      *  reinstatement.
      *
     c                   Eval      kkEfDt = DateTo7(SystemDate)
      *
     c     keyWTXCL1     Setgt     PXC00
      *** Beg Chg ***** 07/22/15  **********************************
     c*******     key1WTXCL1    Readpe    Pxc00                           60
     c     key1WTXCL1    Reade     Pxc00                                  60
      *** End Chg ***** 07/22/15  **********************************
     c     *IN60         IFEQ      *On
     c                   Eval      xcCXLM = 0
     c                   Endif                                                  *IN60 IFEQ *ON
      *
     c     xcCXLM        IFNE      *ZERO
     c     scCNC#        ANDGE     xcCXLM
     c     scERR         ANDNE     'Y'
     c                   eval      RenInfoError = *On
     c                   eval      erCxError  = *On
     c                   eval      msCxError  = 'WC31324'
     c                   LeaveSr
     c**********         SETON                                        90  71
     c                   Endif

     c* Check if confirmation date exists
     c                   If        CXcfdt <> 0
     c                   Exsr      SRCONF
     c                   Endif

     c* Check for other divs eligible for reinstatement
     c                   Exsr      SROTHDR
      *
      *  Display window if renewal year exists that was
      *  processed as part of automatic flat cx of renewal year.
      *
     c                   If        dspMode = *Off and scREV <> 'Y'
     c     keyWDELL8     SETGT     PEL08
     c     key1WDELL8    READE     PEL08                                  60
      *
     c     *IN60         DOWEQ     *OFF
     c     keyWDCXL      SETGT     PCX00
     c     keyWDCXL      READPE    PCX00                                  60
      *
     c     *IN60         Ifeq      *OFF
     c     CXaflt        ANDEQ     'Y'
     c                   EXFMT     rcdwdw09
     c                   If        btnCnwdw09 = *On
     c                   eval      RenInfoError = *On
     c                   Endif                                                  btnCnwdw09 = *On
     c                   Leave
     c                   Endif
      *
     c     key1WDELL8    Reade     PEL08                                  60
     c                   Enddo
      *
     c                   Endif
      *
      *  Display prompt window if renewal year exists that was NOT
      *  processed as part of automatic flat cx of renewal year.
      *
     c                   If        dspMode = *Off
     c     keyWDELL8     Setgt     PEL08
     c     key1WDELL8    Reade     PEL08                                  60
      *
     c     *IN60         DOWEQ     *OFF
     c     key2WDCXL     SETGT     PCX00
     c     key2WDCXL     READPE    PCX00                                  60
      *
     c     *IN60         IFEQ      *OFF
     c     CXAFLT        ANDNE     'Y'
     c                   Z-Add     ELemp#        s5EMP#
     c                   Z-Add     ELdiv         s5DIV
     c                   Z-Add     ELfyr         s5FYR
     c     EMKY1         Chain     PEM00                              60
     c  N60              MoveL(P)  EMNAME        S5ENAM
     c   60              MoveL(P)  *BLANKS       S5ENAM
     c                   Move      'N'           S5ANSW
     c*
     c     s5ANSW        DOUEQ     'Y'
     c     s5ANSW        OREQ      'N'
     c     btnCnwdw10    OREQ      *ON
     c     btnOkwdw10    OREQ      *ON
     c                   EXFMT     rcdWDW10
     c                   SETOFF                                       71
      *
     c                   SELECT
      *
     c     s5ANSW        WHENEQ    'N'
     c     btnCnwdw10    ANDEQ     *On
     c                   eval      RenInfoError = *On
     c                   Leave
      *
     c     s5ANSW        WHENEQ    'Y'
     c     btnOkwdw10    ANDEQ     *On
     c     dbrrn2        Chain     PCX0P                              99
     c                   MOVE      'Y'           CXaflt
      *
     c                   Eval      CXpgm  = Q1pgm
     c                   Eval      CXuser = Q1user
     c                   Eval      CXtime = Timeto6(SystemTime())
     c                   Eval      CXdt   = Dateto7(SystemDate())

     c                   Update    PCX0P
     c                   Leave
      *
     c                   Other
     c                   Seton                                        71
      *
     c                   Endsl
     c                   Enddo
      *
     c                   If        btnCnwdw10 = *On
     c                   eval      RenInfoError = *On
     c                   Endif                                                  btnCnwdw07 = *On
     c                   Endif                                                  CXKY2
      *
     c     key1WDELL8    Reade     PEL08                                  60
     c                   Enddo
      *
     c                   Endif                                                  *IN90
      *
      *  Reset
      *
     c     key1WDCXL     Chain     PCX00                              60
     c     EMKY          Chain     PEM00                              60
     c     key1WDELL1    Chain     PEL01                              60
      *
     c*
     c* Check to be sure record has not been updated since entry
     c     dbrrn2        Chain(N)  PCX0P                              99
      *
     c  N90CXDT          IFGT      SAVEDT
     c     CXDT          OREQ      SAVEDT
     c     CXTIME        ANDGT     SAVETM
     c                   Seton                                        86  90
     c                   Endif                                                  AUDT IFGT SAVEDT
     c*
      ***  Beg Add ***  07/24/13  **********************************
      /Free
       If (scRrCd = '02' Or scRrCd = '03');
          xxResAlert = PopTwoBtn(scAlertMsg:'Alert':'OK':'Cancel');
          If xxResAlert = *Off;
             RenInfoError = *On;
          EndIf;
       EndIf;
      /End-Free
      ***  End Add ***  07/24/13  **********************************
     c*  We have passed all edits, goto the next screen
     c*
     c                   Endsr
     c*===============================================================
     c* SROTHDR - Checks for other divs to reinstate
     c*===============================================================
     c     SROTHDR       BEGSR
     c*
     c                   Z-ADD     scDIV         saVDIV
     c                   Z-ADD     CXreq#        svREQ#
     c                   Z-ADD     CXcdat        svCDAT
     c                   Z-ADD     CXcxdt        svCXDT
     c                   Move      CXrein        svREIN
     c*
     c                   SETOFF                                       87
     c*
     c*   Cycle through all divisions
     c     EMKYX         CHAIN     PEM00                              60
     c     *IN60         DOWEQ     *OFF
     c     *IN87         ANDEQ     *OFF
     c*
     c     EMDIV         IFNE      SAVDIV
     c                   Z-ADD     EMDIV         SCDIV
     c*
     c     key1WDELL1    CHAIN     PEL01                              60
     c     *IN60         IFEQ      *OFF
     c*
     c*     Set indicator if another div is found for cancellation
     c                   EXSR      SRGETX
     c     CD(X)         IFEQ      svCXDT
      ***** Beg Add ***** 03/27/15 ************************************
     C     CD(X)         OREQ      ELPFDT
      ***** End Add ***** 03/27/15 ************************************
     c     CD(X)         OREQ      0
     c     scREV         ANDEQ     'Y'
     c*
     c                   Z-ADD     *HIVAL        SCREQ#
     c     key1WDCXL     SETGT     PCX00
     c     keyWDCXL      READPE    PCX00                                  60
     c     *IN60         IFEQ      *OFF
      ***** Beg Add 10/13/15
     c     CXENFL        IFNE      'Y'
      ***** End Add 10/13/15
     c                   Z-ADD     CXREQ#        SCREQ#
     c*
     c     scREIN        IFEQ      CXREIN
     c     scREIN        ORNE      'Y'
     c     CXREIN        ANDNE     'Y'
     c*** begin chg ***  12/09/21  ************************************
     c*****CXCDAT        IFEQ      SVCDAT
     c*****CXCXDT        ANDEQ     SVCXDT
     c     CXCxdt        Ifeq      svCxdt
     c*** end chg *****  12/09/21  ************************************
      ***** Beg Add ***** 03/27/15 ************************************
     C     CXCXDT        OREQ      ELPFDT
      ***** End Add ***** 03/27/15 ************************************
     c                   SETON                                        87
     c                   ENDIF                                                  CXCDAT=SVCDAT
     c                   ENDIF                                                  SCREIN=CXREIN
     c*
      ***** Beg Add 10/13/15
     c                   ENDIF
      ***** End Add 10/13/15
     c                   ENDIF                                                  *IN60=*OFF
     c*
     c                   ENDIF                                                  CD,X=*ZERO
     c*
     c                   ENDIF                                                  *IN60=*OFF
     c*
     c                   ENDIF                                                  EMDIV<>SAVDIV
     c*
     c     EMKYX         READE     PEM00                                  60
     c                   ENDDO
     c*
     c*   If there are multiple divs for cancellation then output msg
     c     *IN87         IFEQ      *ON
     c                   Dou       btnCnwdw07 = *On or btnOKwdw07 = *On
     c                   EXFMT     rcdWDW07
     c                   Enddo
     c*
     c                   If        btnCnwdw07 = *On
     c                   eval      RenInfoError = *On
     c                   Endif                                                  btnCnwdw07 = *On
     c*
     c                   Endif                                                  *IN87=*ON
     c*
     c*  Reset emp master and canx detail files
     c                   Z-Add     SAVDIV        SCDIV
     c                   Z-Add     SVREQ#        SCREQ#
     c     EMKY          Chain     PEM00                              60
     c     key1WDCXL     Chain     PCX00                              60
     c*
     c*  Reset emp detail file
     c     key1WDELL1    Chain     PEL01                              60
     c*
     c                   Endsr                                                  SROTHDR
       //***************************************************************************//
     C*===============================================================
     C* SROTHWR - Writes/Updates reinstatement records for other divs
     C*===============================================================
     C     SROTHWR       BEGSR
     C*
     C                   SETOFF                                       87
     C                   Z-ADD     SCDIV         SAVDIV
     C*
     C*   Cycle through all divisions
     C     EMKYX         CHAIN     PEM00                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMDIV         IFNE      SAVDIV
     C                   Z-ADD     EMDIV         SCDIV
     C*
      *** Beg Chg *** 03/08/13 ****************************************
     C********* key1WDELL1    CHAIN     PEL01                              60
     C     key1WDELL1    CHAIN(N)  PEL01                              60
      *** End Chg *** 03/08/13 ****************************************
     C     *IN60         IFEQ      *OFF
     C*
     C*     Get appropriate effective and cancel dates from emp master
     C                   EXSR      SRGETX
     C     CD(X)         IFEQ      SVCXDT
      ***** Beg Add ***** 03/27/15 ************************************
     C     CD(X)         OREQ      ELPFDT
      ***** End Add ***** 03/27/15 ************************************
     C     CD(X)         OREQ      0
     C     SCREV         ANDEQ     'Y'
     C*
     C                   Z-ADD     *HIVAL        scREQ#
     C     key1WDCXL     SETGT     PCX00
     C     keyWDCXL      READPE    PCX00                                  60
     C     *IN60         IFEQ      *OFF
      ***** Beg Add 10/13/15
     c     CXENFL        IFNE      'Y'
      ***** End Add 10/13/15
     C                   Z-ADD     CXREQ#        scREQ#
     C*
     C     scREIN        IFEQ      'Y'
     C     CXREIN        ANDEQ     'Y'
     C*
     c*** begin chg ***  12/09/21  ************************************
     C*****CXCDAT        IFEQ      svCDAT
     C*****CXCXDT        ANDEQ     svCXDT
     C     CXCxdt        Ifeq      svCxdt
     c*** end chg *****  12/09/21  ************************************
      ***** Beg Add ***** 03/27/15 ************************************
     C     CXCXDT        OREQ      ELPFDT
      ***** End Add ***** 03/27/15 ************************************
     C*
     C                   CALL      AGTAPI        AGTPL
     c                   Exsr      RnstInfoSave
     C                   Exsr      PGMENXR
      *
      * Process Renewal years
      *
     C                   Exsr      SRRNEWR
     C*
     C                   Endif                                                  CXCDAT=SVCDAT
     C                   Endif                                                  @@MODE='C'
     C*
     C     scREIN        IFNE      'Y'
     C     CXREIN        ANDNE     'Y'
     C*
     c*** begin chg ***  12/09/21  ************************************
     C*****CXCDAT        IFEQ      svCDAT
     C*****CXCXDT        ANDEQ     svCXDT
     C     CXCxdt        Ifeq      svCxdt
     c*** end chg *****  12/09/21  ************************************
      ***** Beg Add ***** 03/27/15 ************************************
     C     CXCXDT        OREQ      ELPFDT
      ***** End Add ***** 03/27/15 ************************************
     C*
     C                   CALL      AGTAPI        AGTPL
     c                   Exsr      RnstInfoSave
     C                   Exsr      PGMENXR
      *
      * Process Renewal years
      *
     C                   Exsr      SRRNEWR
      *
     C*
     C                   Endif                                                  CXCDAT=SVCDAT
     C                   Endif                                                  @@MODE='A'
     C*
      ***** Beg Add 10/13/15
     C                   Endif
      ***** End Add 10/13/15
     C                   Endif                                                  *IN60=*OFF
     C                   Endif                                                  CD,X=SVCXDT
     C*
     C                   Endif                                                  *IN60=*OFF
     C*
     C                   Endif                                                  EMDIV<>SAVDIV
     C*
     C     EMKYX         READE     PEM00                                  60
     C                   Enddo
     C*
     C*  Reset emp master and canx detail files
     C                   Z-ADD     SAVDIV        SCDIV
     C                   Z-ADD     SVREQ#        SCREQ#
     C     EMKY          CHAIN     PEM00                              60
     C     key1WDCXL     CHAIN     PCX00                              60
     C*
     C*  Reset emp detail file
      *** Beg Chg *** 03/08/13 ****************************************
     C********* key1WDELL1    CHAIN     PEL01                              60
     C     key1WDELL1    CHAIN(N)  PEL01                              60
      *** End Chg *** 03/08/13 ****************************************
     C*
     c                   Endsr                                                  SROTHWR
      *===============================================================
      * SRRNEWR - Process Renewal Years(Reinst. Info)
      *===============================================================
     c     SRRNEWR       BEGSR
     c                   MOVE      'Y'           XXRNEW            1
      *
      *  If not Reprint
      *
     c     QQrepr        IFNE      'Y'
     c     s5ANSW        ANDNE     'N'
     c                   Z-ADD     scFYR         s1FYR
     c                   Z-ADD     scREQ#        s1REQ#
      *
      *  For each renewal policy year for employer division
      *
     c     keyWDELL      SETGT     PEL00
     c     key1WDELL     READE     PEL00                                  60
      *
     c     *IN60         DOWEQ     *OFF
     c                   Z-ADD     ELfyr         scFYR
      *
      *    If not Reinstated, process reinstatement of renewal year
      *
     c                   Z-ADD     *HIVAL        scREQ#
     c     key1WDCXL     SETGT     PCX00
     c     keyWDCXL      READPE    PCX00                                  60
      *
      *
     c     *IN60         IFEQ      *OFF                                         CANCELLED BUT
     c     CXrein        ANDNE     'Y'                                          NOT REINSTATED
      *** Beg Add ***** 01/24/24 ************************************
     c     CXenfl        ANDNE     'Y'                                          Not Deleted
      *** End Add ***** 01/24/24 ************************************
     c                   MOVE      CXreq#        scREQ#
     c                   Call      AGTAPI        AGTPL
     C                   Exsr      cancelcount
     C                   Exsr      RnstInfoSave
     C                   Exsr      PGMENXR
     C                   Endif                                                  CXKY
      *
     C     key1WDELL     Reade     PEL00                                  60
     C                   Enddo                                                  *IN90
      *
      * Reset
      *
     C                   Z-ADD     s1FYR         scFYR
     C                   Z-ADD     s1REQ#        scREQ#
     C     key1WDELL1    Chain     PEL01                              60
     C                   Call      AGTAPI        AGTPL
     C     key1WDCXL     Chain     PCX00                              60
     C                   Endif
      *
     C                   Move      'N'           XXRNEW            1
     C                   Endsr                                                  SRRNEW

     C*===============================================================
     c* PGMENXR - Processing to be done for each reinstated emp/div
     c*===============================================================
     c     PGMENXR       BEGSR
     c*
     c*  Only do if not in inquiry and enter was pressed
     c     dspMode       IFEQ      *Off
     c     RenInfoError  AndEq     *Off
     c*
     c*  Find out if this is a reprint or not
     c*   -reprint=N if revise flag=Y or original reinstatement
     c     scREV         Ifeq      'Y'
     c     CXredt        orne      *ZERO
     c                   Eval      QQrepr = 'N'
     c                   Endif                                                  SCREV=Y
     c*
     c*  Write a record to the cancellation history file if this is
     c*  the initial entry or if this is a revision
     c     QQrepr        Ifeq      'N'
     c*
     c     XHKY          Chain     PXH00                              60
     c*
     c                   Eval      XHstat = ' '
     c                   Eval      XHuser = Q1user
     c                   Eval      XHpgm  = Q1pgm
     c                   Eval      XHdt = Dateto7(SystemDate())
     c                   Eval      XHtime = Timeto6(SystemTime())
     c*
     c                   If        *In60 = *Off
     c*
     c                   If        SCrev = 'Y'
     c                   Eval      XHrvfl = 'R'
     c                   Else
     c                   Eval      XHrvfl = ' '
     c                   Endif                                                  SCREV=Y
     c*
     c                   Eval      XHrsn = CXrsn
     c                   Eval      XHcxdt = CXcxdt
     c                   Eval      XHcdat = CXcdat
     c                   Eval      XHredt = CXredt
     c                   Eval      XHrdat = CXrdat
     c                   Eval      XHerr =  CXerr
     c*
     c                   Eval      XHtrdt = Dateto7(SystemDate())
     c*
     c                   Eval      XHcnc#  = CXcnc#
      ***** Beg Add ******* 07/24/13 **********************************
      * Moving Reinstatement Code to Cancellation History File.
     c                   Eval      XHreCd = CXreCd
      ***** End Add ******* 07/24/13 **********************************

     c                   WRITE     PXH00
     c*
     c                   Endif                                                   *IN60=*OFF
     c*
     c                   Endif                                                   QQREPR=N
     c*
      *  Write an 'R' type Quote Status record.
      *
      *  Get current agent
      *
     C                   CALL      'WA002R'                             99
     C                   Parm                    scCO#
     C                   Parm                    scFND
     C                   Parm                    scEMP#
     C                   Parm                    scDIV
     C                   Parm      *ZERO         #1FYR             3 0
     C     xxAGT#        Parm      *ZERO         #1AGT#            5 0
     C                   Parm      *BLANK        #1ANAM           60
      *
     C     QTKY1         SETGT     PQT16
     C     QTKY1         READPE    PQT16                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     QTTYPE        IFEQ      'C'                                          CANCELLATION
     C     QTTYPE        OREQ      'T'                                          TRANSFER
     C     QTXFER        ANDEQ     'C'                                          XFER/CX
      *
     C                   Eval      QTagt# = XXagt#
     C                   Eval      QTtype = 'R'
     C                   Eval      QTxfer = ' '
     C*
     c                   Eval      XXwkdt = Dateto8(SystemDate())
     C*
     C                   Z-ADD     XXWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         QTWKDT
     C*
     C                   Eval      QTdt = QTwkdt
     c                   Eval      QTtime = Timeto6(SystemTime())
     C                   Eval      QTUSER = Q1user
     C                   Eval      QTpgm = Q1pgm
     C                   Write     PQT16
     C*
     C                   Endif                                                  QTTYPE=C
     C*
     C                   END                                                    QTKY
     C*
     C*  Submit reinstatement print program
     C                   Move      scCO#         QQCO#             3
     C                   Move      scFND         QQFND             3
     C                   Move      scFYR         QQFYR             3
     C                   Move      scEMP#        QQEMP#            9
     C                   Move      scDIV         QQDIV             5
     C                   Move      scREQ#        QQREQ#            5
     C                   Move      *BLANKS       QQOUTQ           10
     C                   Move      scTYPE        QQTYPE            1
     C*
     C*  Get user outq
     C     Q1USER        CHAIN     PUP00                              60
     C  N60              MOVEL     UPOUTQ        QQOUTQ
     C*
     C                   CALL      'W7167C'                             99
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQREQ#
     C                   PARM                    SCRCRT
     C                   PARM                    QQREPR
     C                   PARM                    QQOUTQ
     C                   PARM                    QQTYPE
     C*
     C     xxRNEW        IFNE      'Y'
      *** Begin Del *** 09/14/22 WC2880 ******************
     C***  87              CALL      'WA007R'                             99
      *** End   Del *** 09/14/22 WC2800 ******************
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN09,KC,KL=*OFF
     C*
     C                   ENDSR                                                  PGMENX
     C*===============================================================
     C* SRCONF - Checks if confirmation date is valid
     C*===============================================================
     c     SRCONF        Begsr
     c                   Eval      scCFDT = dtMDYY(CXcfdt)
     c                   Dou       btnCnwdw08 = *On or btnOKwdw08 = *On
     c                   Exfmt     rcdWDW08
     c                   Enddo
     c                   If        btnCnwdw08 = *On
     c                   eval      RenInfoError = *On
     c                   Endif                                                  btnCnwdw08 = *On
     c                   Endsr                                                  SRCONF

       //***************************************************************************//
       //===================================================================
       // Reinstatement Info popup  Save
       //===================================================================
     c     RnstInfoSave  Begsr
      /free

       Chain DBRRN2 PCX0P;
       //***** Beg Chg ***** 03/27/15 **************************************
       //************CXredt = dtCYMD(scREDT);
       CXredt = CXcxdt;
       //***** End Chg ***** 03/27/15 **************************************
       CXrdat = currDate7;
       CXrcrt = scRCRT;
       CXrdrt = ' ';
       CXrein = 'Y';
       CXerr = scERR;
       CXrcm1 = scRCM1;
       CXrcm2 = scRCM2;
       // *** Beg Add ***** 07/24/13 ****************
       CXreCd = %trim(scRrCd);
       // *** End Add ***** 07/24/13 ****************
       CXcfdt = 0;
       // *** beg chg ***** 12/26/12 ****************
       // ************ CXagt# = scAGT#;
       If CXagt# = *Zeros;
          CXagt# = scAGT#;
       Endif;
       // *** end chg ***** 12/26/12 ****************

       //* Renewal Year Processing
       If xxRNEW = 'Y';
          cxREDT = CXcxdt;
          CXrcrt = 'N';
       Else;
          CXcnc# = scCNC#;
       Endif;

      *** Beg Add *** 02/26/24 *** WC2956 **********************************
       CXpdPr = 'Y';
      *** End Add *** 02/26/24 *** WC2956 **********************************

       CXpgm  = Q1PGM;
       CXuser = Q1USER;
       CXtime = Timeto6(SystemTime());
       CXdt   = Dateto7(SystemDate());

       Update pcx0p;

       // *** Beg Add ***** 03/08/13 ****************
       // Short Rate or Prorate Audit Cancellation: updates the value in Policy Detail
       Chain keyWDELL PEL00;
       If %found;
           ELsrpr = 'P';
           Update PEL00;
       Endif;
       // *** End Add ***** 03/08/13 ****************
      /end-free
     c                   Endsr
     c*** begin add ***  10/06/16  ****************************************
       //===================================================================
       // Scriptura Output Window Prompt
       //===================================================================
     c     ScriptPrompt  Begsr
      /free

       If (isUseScriptC and btnCanSave = *on) or
       //*** beg chg ***  10/25/19  **********************************
        //***(isUseScriptR and btnReinSav = *on);
          (isUseScriptR and btnReinSav = *on) or
          (scRepType = 'C' and BtnOKwdw11 = *On) or
          (scRepType = 'R' and BtnOKwdw11 = *On);

          //***If btnCanSave = *on;
          //***   xxProcType = processTypeC;
          //***Else;
          //***   xxProcType = processTypeR;
          //***Endif;

          select;
            when btnCanSave = *on;
                 xxProcType = processTypeC;
            when btnReinSav = *on;
                 xxProcType = processTypeR;
          //*** begin chg ***  07/28/20  ***************************
          //****  When scRepType = 'C';
          //****       xxProcType = 'Reprint Cancellation';
          //****  When scRepType = 'R';
          //****       xxProcType = 'Reprint Reinstatment';
            When scRepType = 'C';
                 xxProcType = processTypeC;
            When scRepType = 'R';
                 xxProcType = processTypeR;
          //*** end chg *****  07/28/20  ***************************
          EndSl;
       //*** end chg ***  10/25/19  **********************************

          xxMass = ' ';

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) (scCo#:scFnd) pex00;

          Select;
          When %found and btnCanSave = *on and scRev = 'Y' and EXRCCD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXRCCD;
          When %found and btnCanSave = *on and scRev <> 'Y' and EXCXCD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXCXCD;
          When %found and btnReinSav = *on and scRev <> 'Y' and EXRECD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXRECD;
          When %found and btnReinSav = *on and scRev = 'Y' and EXRRCD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXRRCD;
          Other;
             xxDart = 'N';
             xxRMCD = ' ';
          Endsl;

          Out *dtaara;

          //*** begin chg ***  08/13/21  ***********************************
          //***** xxSubmit = OutputWindow(xxProcType:xxMass);
          If xxRRN <> 0 and divCnt = 1;
             xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':xxRRN);
          Else;
             xxSubmit = OutputWindow(xxProcType:xxMass);
          Endif;
          //*** end chg *****  08/13/21  ***********************************

          If xxSubmit = *off;
             btnCanSave = *off;
             btnReinSav = *off;
          Endif;

       Endif;

      /end-free
     c                   Endsr
     c*** end add *****  10/06/16  ****************************************
       //***************************************************************************//
     P Main            E



       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutElRrn = xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;

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


       kkDiv = ELdiv;
       kkFyr = ELfyr;

       // * Load Button Values in Grid.
       setgt  keyWDCXL  pcx00;
       readpe keyWDCXL  pcx00;
       If not %eof;
          If CXcxdt <> 0;
             BtnRnstVal = 'Reinstate';
             BtnCxVal   = 'Revise Cancellation';
             canEdtMode = *On;
             canaddMode = *Off;
             rnstvisb   = *On;
          Else;
             // *** Beg Chg *** 06/20/14 *****************
             //BtnCxVal   = 'Cancel';
             BtnCxVal   = 'Issue Cancellation';
             // *** End Chg *** 06/20/14 *****************
             canaddMode = *On;
             canEdtMode = *Off;
             rnstvisb   = *Off;
          Endif;
          If CXredt <> 0;
             // *** Beg Chg *** 06/20/14 *****************
             //BtnCxVal = 'Cancel';
             BtnCxVal   = 'Issue Cancellation';
             // *** End Chg *** 06/20/14 *****************
             BtnRnstVal = 'Revise Reinstatement';
             canEdtMode = *Off;
             canaddMode = *On;
             rnstvisb   = *On;
          Endif;
       Endif;
       inCo# = scCo#;
       inFnd = scfnd;
       ComAPI(inCo#:inFnd:compName:fundName:secLevel:secType:##outq);

       LoadSelection();

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
       dspCxHistS = indValue;
       dspCxHistC = indValue;
      /End-Free
     P RcdAllInfoDsp   e

       //===================================================================
       // Load display indicators for sub-files in rcdHistory
       //===================================================================
     P RcdHistoryDsp   b
     D RcdHistoryDsp   pi
     D  indValue1                     1n   const
      /Free
       dspHistorS = indValue1;
       dspHistorC = indValue1;
      /End-Free
     P RcdHistoryDsp   e

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
