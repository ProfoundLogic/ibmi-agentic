      * (c) 2025 Sapiens. All Rights Reserved.                             *
     H**********************************************************************
     H/TITLE WTPOLTRACK - Policy Tracking.
     H DFTNAME(WTPOLTRACK)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 08/22/12                                         *
      *  Programmer: C:279, C:33                                      *
      *  Project   : WC2115                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *
      *  03/06/25 C:291 Project: SY450
      *                 Add button on Service Tracking grid should not
      *                 be available in Inquiry mode.
      *
      *  06/25/24 C:340 Project: WC2922
      *                 Add/Remove Invoices based on contested flag
      *                 and Division is either not summarized OR
      *                 its Summarized and state reporting.
      *                                                               *
      * 04/18/24 C:344 Support: 115440                                *
      *                Unexpected error appears when applying Audits  *
      *                Tracking filter. Replaced EXFMT with WRITE and *
      *                READ op codes.                                 *
      *
      * 09/28/23 C:333 Support: 113925 (re-compilation)
      *                Changes in WTVSTTRKSP copybook.
      *
      * 08/21/23 C:300 Support: 113191 (WC2395)
      *                Recompile over changes in WTVSTTRK DSPF
      *
      * 07/28/23 C:300 Support: 113102 (WC2395)
      *                Recompile over changes in WTVSTTRK DSPF
      *
      * 06/08/23 C:342 Project: WC2747
      *                For Recompilation, changes in WTVSTTRKSP copybook.
      *
      * 10/17/22 C:291 Support: 103956
      *                Moved some definitions to WTVSTTRKSD.
      *
      * 09/08/22 C:062 Ticket: 103606(099)
      *                Add data structure to WMVIP.
      *
      * 06/27/22 C:300 Project: WC2395
      *                Add WTVSTTRK program changes to recompile over
      *                WTVSTTRKSD and WTVSTTRKSP copybook changes
      *
      * 04/11/22 C:333 Project: SY434
      *                - Update Grids for Sorting Filtering Upgrade
      *
      * 06/23/21 C:318 Project WC2302 - Auto tab closure for
      *                deletion. Changed WTPOLTRACK/WTVSTTRK DSPF
      *
      * 06/21/21 C:298 Project: SY426
      *                For Recompilation, changes in WTVSTTRKSP copybook.
      *
      * 06/22/20 C:33  Project: SY406
      *                Update EMFQLK based on the quote status change *
      *
      * 06/09/20 C:33  Project: SY420
      *                Add SPRMASK
      *
      * 11/26/19 C:295 Support: 83168
      *                Audit status inactive -unable to update        *
      *                CX column is not getting updated correctly.    *
      *                                                               *
      * 10/01/19 C:295 Support: 82550
      *                Changes in Policy tracking Screen to populate  *
      *                detail box for Renewal Buisness.               *
      *                                                               *
      * 09/26/19 C:291 Project: 129WVN
      *                Add LF WMVIL7 for multiple visit tracking      *
      *                per day.                                       *
      *                                                               *
      * 08/08/19 C:212 Support: 81206
      *                ReCompile only (for WTVstTrkSP changes)        *
      *                                                               *
      * 07/10/19 C:300 Support: 80203 (LUBA)                          *
      *                Allow Add/update when accessing Visit Tracking *
      *                Grid in inquiry mode                           *
      *
      * 06/18/19 C:300 Support: 80204 (LUBA)
      *                Add WTVSTTRK program changes to recompile over *
      *                WTVSTTRKSD and WTVSTTRKSP copybook changes     *
      *                                                               *
      * 12/18/18 C:33  Project: SY406                                 *
      *                Allow for different 'lockout' processes        *
      *                                                               *
      * 10/17/18 C:079 Support: 146/73955                             *
      *                Recompile due to increasing drop down field    *
      *                size                                           *
      *                                                               *
      * 02/06/18 C:291 Support: 63953                                 *
      *                Change Division dropdown "max choices" property*
      *                to 100,000. Screen change only.                *
      *                Modified records: rcdPanel, rcdAudFltr         *
      *                                                               *
      * 01/24/18 C:AZ  Support: PHW48281                              *
      *                Switched places of Cancel and Save buttons on  *
      *                6 record formats of the WTPOLTRACK display     *
      *                file to be consistent with all other windows.  *
      *                                                               *
      *  01/05/18 C:291 Support: 63337                                *
      *                 Recompile over change to WTVSTTRK screen file.*
      *                 Changed scEdtDiv Value property to "Show      *
      *                 Zero Balance".                                *
      *                                                               *
      *  06/23/17 C:062 Support: 47954(129)                           *
      *                 Add group restrictions                        *
      *                                                               *
      *  03/22/17 C:053 Support: 46241                                *
      *                 Recompile over changed copy module            *
      *                                                               *
      * 02/27/17 C:136 Support: 44411                                 *
      *                Order Type of Audit Dropdown selections by     *
      *                RRN(WTTAP)                                     *
      *                                                               *
      * 02/25/16 C:307 Support: 38316                                 *
      *                Audit Tracking Screen Reverts to Top Record    *
    *                After Editing One Record.                      *
      *                                                               *
      * 02/10/16 C:242 PW37116                                        *
      *                DBA/Fed Names and Links for Quote not working  *
      *                correctly.                                     *
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      * 10/14/15 C:300 Support: 37399                                 *
      *                Update UI by updating dollar fields with commas*
      *                on Premium field in Quote Tracking grid        *
      *                                                               *
      * 10/15/15 C:298 Support : 36807                                *
      *                To clear Contested Audit Flag on               *
      *                deleting Audit Tracking Record                 *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *
      *                                                               *
      * 08/28/15 C:298 Support: 36615                                 *
      *                Populate 'CX' in Audit Tracking grid even if   *
    *                cancellation date is in future.                *
    *
      * 06/16/15 C:33  35359                                          *
      *                Don't clear record format for quote status     *
      *                 writes                                        *
      *                                                               *
      * 04/16/15 C:65  Project: SY251                                 *
      *                Add API call to launch new Department          *
    *                letters.                                       *
      *                                                               *
    * 02/04/15  C:062 Project 114WGU                                *
    *                 Incorporate visit tracking.                   *

    * 12/16/14  C:180 Support WA114 87597                           *
      *                 Changed to allow EDIT button to display       *
      *                 New/Renewal Business tab.                     *
      *                                                               *
      *    10/08/14 C:291 RED 3161                                    *
      *                   Change the date definition on the grid from *
      *                   Number to date data type.                   *
      *                                                               *
    * 08/08/14  C:290 Support WC099 86393                           *
    *                   Modified the message appearing when a quote *
    *                   is currently being worked in FastQuote and  *
    *                   CompSuite tries to open that for processing *
    *                                                               *
      * 07/28/14  C:295 Project: 114WMD                               *
      *                 Compilation only as the changes are in        *
      *                 Action Pane l                                 *
      *                                                               *
      * 01/13/14  C:189 Redmine: 3588                                 *
      *                 Only client users should appear in the user   *
      *                 search and not PDS users (display file change)*
      *                                                               *
      *  11/14/13 C:279 Redmine: 3511                                 *
      *                 Correct filter edit for audit tracking based  *
      *                 on date range.                                *
      *                                                               *
      * 10/07/13 C:190  Project: 25WSS                                *
      *                   - Change the program to determine if        *
      *                     Premium or Contribution should be printed *
      *                     /displayed based on Group setup.          *
      *                                                               *
      *  09/27/13 C:136 Support: WA114 83429                          *
      *                 Make Edit and Status buttons non display      *
      *                 in Quote Status subfile if the record         *
      *                 is not in the same env as the pgm (Qte vs Liv)*
      *                                                               *
      *  09/03/13 C:189 Redmine: 3015                                 *
      *                 Use include instead of ignore for action      *
      *                 panel record format                           *
      *                                                               *
      *  08/02/13 C:33  Redmine 3466                                  *
      *                 Optimize filters.                             *
      *                                                               *
      *  07/22/13 C:282 Redmine 2795                                  *
      *                 Added the Blank option as 'True' in field     *
      *                 scRptGrp.                                     *
      *                                                               *
      *  07/08/13 C:189 Redmine 3335                                  *
      *                 If the New audit recieved date is zeroed out  *
      *                 ,it should be allowed to be saved             *
      *                                                               *
      *  05/31/13 C:65  Redmine 925                                   *
      *                 Allow entry of new business date in quote     *
      *                 tracking if its value is zero.  Otherwise     *
      *                 disable the field to prevent editing.         *
      *                                                               *
      *  04/24/13 C:279 Redmine 2477                                  *
      *                 Enable year drop down for year selection.     *
      *                                                               *
      *  03/22/13 C:279 Redmine 2578                                  *
      *                 Correct filter for audit tracking based on    *
      *                 date range                                    *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwtpoltrackcf   e             workstn extfile('WTPOLTRACK')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflQteTrck:rrnQteTrck)
     f                                     Sfile(sflAudTrck:rrnAudTrck)
     f                                     Sfile(sflAudTrk:rrnAudTrck)
     f                                     Sfile(sflHstAud:rrnHstAsn)
     f                                     Sfile(sflRnwBsTr:rrnRnwTrck)
     fwtVstTrk  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflVstTrk:RrnVst)
     f                                     sfile(sflAgtUndw:RrnAgtUnd)
     f                                     sfile(sflAgtProd:RrnAgtPrd)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Include(rcdPolPnl)
     f                                     Include(rcdQtePnl)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmahl1    if   e           k disk
     fwtqsl1    if   e           k disk
     fwaqtp     uf a e             disk    rename(pqt01:pqt0p)
     fwmaul33   if   e           k disk    rename(pau01:pau33)
     f                                     InfDs(auFDS33)
     fwmaup     uf   e             disk    rename(pau01:pau0p)
     fwaqtl12   if   e           k disk    rename(pqt01:pqt12)
     f                                     InfDs(qtFDS12)
     fwdell8    if   e           k disk    rename(pel01:pel08)
     fwdell     uf   e           k disk    rename(pel01:pel00)
     f                                     infds(elFDS)
     fwdrtp     uf   e             disk    rename(prt01:prt0p)
     fwtbsp     if   e             disk    rename(pbs01:pbs0p)
     fwdrtl     if   e           k disk    rename(prt01:prt00)
     f                                     infds(rtFDS)
     fwmnbp     uf a e             disk    rename(pnb01:pnb0p)
     fwmnbl     if   e           k disk    rename(pnb01:pnb00)
     f                                     infds(nbFDS)
     fwmupl     if   e           k disk    rename(pup01:pup0u)
     f                                     prefix(u_)
     fssylf     if   e           k disk
     fsmupl1    if   e           k disk
     fsmupl     if   e           k disk    rename(pup01:pup00)
     fwtacl     if   e           k disk    rename(pac01:pac00)
     fwmaxl     if   e           k disk    rename(pax01:pax00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fstctl1    if   e           k disk
     fwmhap     o    e           k disk    rename(pha01:pha0p)
     fwmhal     if   e           k disk    rename(pha01:pha00)
     fwttal     if   e           k disk    rename(pta01:pta00)
     fwtcnl1    if   e           k disk
     fwmshlq    uf   e           k disk    rename(psh01:psh00q)usrOpn
     fwmvip     uf a e             disk    rename(pvi01:pvi0p)
     f*** Begin Add *** 09/08/22 *******************************************
     f                                     infds(vimds8)
     f*** End   Add *** 09/08/22 *******************************************
     fwmvil6    if   e           k disk    rename(pvi01:pvi06)
     f                                     infds(vimds6)
     fwmvil1    if   e           k disk    rename(pvi01:pvi01)
     f                                     infds(vimds1)
     fwmvil2    if   e           k disk    rename(pvi01:pvi02)
     f                                     infds(vimds2)
     fwmvil3    if   e           k disk    rename(pvi01:pvi03)
     f                                     infds(vimds3)
     fwmvil4    if   e           k disk    rename(pvi01:pvi04)
     f                                     infds(vimds4)
      *** Beg Add ***** 09/26/19 *************************************
     fwmvil7    if   e           k disk    rename(pvi01:pvi07)
     f                                     infds(vimds7)
      *** End Add ***** 09/26/19 *************************************
     fwtvil     if   e           k disk    rename(pvi01:ptvil)
     fwmahl     if   e           k disk    rename(pah01:pah00)
     fwmagl     if   e           k disk    rename(pag01:pag00)
     fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwmaal     if   e           k disk    rename(paa01:paa00)
     fwmapl     if   e           k disk    rename(pap01:pap00)
     fwmcml     if   e           k disk    rename(pcm01:pcm00)
     fwmcdl     if   e           k disk    rename(pcd01:pcd00)
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fwmagp     if   e             disk    rename(pag01:pag0p)
     fwmrpl     if   e           k disk    rename(prp01:prp00)
     fwmall     if   e           k disk    rename(pal01:pal00)
     fwtabl     if   e           k disk
     f                                     include(pcn01)
     f                                     rename(pcn01:pcn00)
     f                                     include(pna01)
     f                                     rename(pna01:pna00)
     f                                     include(ppa01)
     f                                     rename(ppa01:ppa00)
     f                                     include(pca01)
     f                                     rename(pca01:pca00)
     f                                     include(pat01)
     f                                     rename(pat01:pat00)
     fwtagl     if   e           k disk    rename(pag01:pag01)
     fwmael     if   e           k disk    rename(pae01:pae01)
     fstabl     if   e           k disk    rename(pst01:pst00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt01)
     fwdellq    if   e           k disk    rename(pel01:pel00q)
     f                                     infds(ELfdsQ)
     f                                     prefix(E1:2)
      ********* Begin Add *** 06/18/19 **********************************
     Fsdacl     if   e           k disk    rename(pac01:pac04)
      ********* End   Add *** 06/18/19 **********************************
      ********* Beg Add *** 06/21/21 **********************************
     fwtvipcfg  if   e           k disk    rename(pvi01:pvi0p_cfg)
     f                                     prefix(cfg_)
      ********* End Add *** 06/21/21 **********************************

      /copy sCopyPolPr
       //***  Begin Add  ***  06/09/20  *************************
      *****///copy sPrMask
       //***  End   Add  ***  06/09/20  *************************

     d psds          esds                  extname(szq1p)

     d qtFDS12         ds
     d  qtRRN12              397    400b 0
     d auFDS33         ds
     d  auRRN33              397    400b 0
     d rtFDS           ds
     d  rtRRN                397    400b 0
     d nbFDS           ds
     d  nbRRN                397    400b 0

     d                 ds
     d CX                      1     35  0
     d                                     dim(5)
     d EMcad1                  1      7  0
     d EMcad2                  8     14  0
     d EMcad3                 15     21  0
     d EMcad4                 22     28  0
     d EMcad5                 29     35  0

     d                 ds
     d EF                      1     35  0
     d                                     dim(5)
     d EMefd1                  1      7  0
     d EMefd2                  8     14  0
     d EMefd3                 15     21  0
     d EMefd4                 22     28  0
     d EMefd5                 29     35  0

     d ##co            s                   like(ELco#)
     d ##fnd           s                   like(ELfnd)
     d ##emp           s                   like(ELemp#)
     d ##div           s                   like(ELdiv)
     d ##div#          s                   like(ELdiv)
     d ##fyr           s                   like(ELfyr)
     d ##rep           s                   like(AUrep)
     d ##aco           s                   like(AUaco)
     d ##co#           s              3
     d ##fnd#          s              3
     d ##date#         s              7
     d ##date1#        s              7
     d ##date2#        s              7
     d ##que#          s             10
     d ##mode#         s              1
     d ##key           s             10
     d ##key#          s             40
     d ##comm          s            256
     d ##cdat          s              8
     d ##dcid          s             12
     d ##pnm           s             10
     d ##secl          s              3
     d ##user          s             10
     d ##pgm           s             10
     d ##fold          s             80
     d ##difl          s              1
     d ##ipgm          s             10
     d ##fl01          s              1
     d ##fl02          s              1
     d ##rmcd          s              5
     d ##seq#          s              5
     d ##sbyl          s              1
     d ##com2          s             19
     d ##apid          s              2
     d ##idoc          s             12
     d ##ique          s             10
     d ##imgf          s              1
     d ##ipth          s             10
     d ##pidx          s              5
     d ##qpty          s              1
     d ##dctp          s              5
     d ##csid          s              2
     d ##code#         s              2
     d ##cseq          s              5  0
     d ##cpnam         s             10
     d ##cxsts         s              1

     d* indicators
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d isValid         s               n   inz(*off)
     d isVldFlt        s               n   inz(*off)
     d isSelVld        s               n   inz(*off)
     d process         s               n   inz(*off)
     d isContribution  s               n   inz(*off)

     d* work fields
     d @@next          s             10
     d cntFlag         s              1
     D intDate         s              7  0
     D intDate1        s              7  0
     D intToDate       s              7  0
     D intFromDate     s              7  0
     D intZip          s              9  0
     D intApr          s             11  2
     d mode            s              1
     d meetFlt         s              1
     d meetAudFlt      s              1
     d outIndex        s              5
     d clientID        s              3    inz('000')
     D intWkDt         s                   like(QTwkdt)
     d rmkCd           s              5
     d saveFyr         s                   like(ELfyr)
     d svYear          s                   like(ELfyr)
     d scco#           s                   like(ELco#)
     d scfnd           s                   like(ELfnd)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d scfyr           s                   like(ELfyr)
     d saveEffDt       s                   like(ELpfdt)
     d strDate         s              8
     d strOrnr         s             27
     d strrrn3         s              9
     D str9999Msg      s            256
     d tempTyp         s                   like(QTtype)
     d tempFrDt        s              7  0
     d tempToDt        s              7  0
     d x1              s              1  0
     d xxSt            s              2
     d xxRrn           s              9  0
     d xxKey           s             10
     d xxMode          s              1
     d xxPxSt          s              2
     d xxCall          s              1
     d intRrn          s              9  0
     d intRrn#         s              9  0
     d*** Beg Add *** 10/25/23 *** WC2922 ****************
     d stRptDiv        s              5  0
     d*** End Add *** 10/25/23 *** WC2922 ****************
      ***** Beg Del ***** 10/17/22 ****************************
     d*** flgRcdUpdated   s              1
      ***** End Del ***** 10/17/22 ****************************
     d temprrn         s              9  0
     d returnCode      s              1    Inz
     d authorized      s              1    Inz
     d restType        s              1    Inz
     d userGroup#      s                   Like(AUfnd)
      ***** Beg Add ***** 06/27/22 ****************************
      ***** Beg Del ***** 10/17/22 ****************************
     d*** Callpgm         s              1
     d*** Callpgm2        s              1
     d*** saveDT          s                   like(VIdt)
     d*** saveTM          s                   like(VItime)
      ***** End Del ***** 10/17/22 ****************************
      ***** End Add ***** 06/27/22 ****************************

     doutMessage       s            256
     doutTitle         s             50
     doutBtnText       s             10

     d dsPFilter       ds                  likerec(RcdSelFltr)
     d dsAFilter       ds                  likerec(RcdAudFltr)

     d/copy wtVstTrkSD

     d* Get the Audit Cx Status for the employer
     DAuditCxSts       pr             1

     d* Get the Audit Plan Type for the employer
     DAuditPlanType    pr             1

      ***** Beg Add ***** 06/27/22 ****************************
      ***** Beg Del ***** 10/17/22 ****************************
     d*** ChkStack        pr                  extpgm('SRCHKPGM')
     d***  outPgmName                   10    const
     d***  isFound                       1
      ***** End Del ***** 10/17/22 ****************************
      ***** End Add ***** 06/27/22 ****************************

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* Clear Subfiles
     d ClrAudSubFil    pr

     d* Clear Subfiles
     d ClrRnwSubFil    pr

     d* Change Status
     d ChgStatus       pr

     d* Check Group Authority
     d ChkGrpAuth      pr                  extpgm('SA008R')
     d company#                       3  0
     d user                          10
     d application                    1    Const
     d returnCode                     1
     d authorized                     1
     d group#                         3  0

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d* Load filter clear butoon
     d LoadClrFlt      pr

     d* Load Selection fields
     d LoadSelection   pr

     d* Load Selection fields
     d LoadSelFlt      pr

     d* Load quote tracking grid
     d LoadQteTab      pr

     d* Load audit tracking grid
     d LoadAudTab      pr

     d* Load Renewal business tracking grid
     d LoadRnwTab      pr

     d* Load New business tracking grid
     d LoadNewTab      pr

     d* Main Processing
     d Main            pr

     d* Filter for quote tracking
     d MeetFilter      pr

     d* Filter for Audit tracking
     d MeetAudFltr     pr             1n

     d* Program Start
     d ProgramStart    pr
     d processMe                       n

     d* Process other options
     d ProcOther       pr

     d* Process common functionality for option from audit tracking
     d ProcCommon      pr

     d* Process View for Audit Report
     d ProcView        pr

     d* Process Print for Audit Report
     d ProcPrint       pr

     d* Validate Edit screen for quote tracking
     d Validate        pr

     d* Validate Edit screen for audit tracking
     d ValidAud        pr

     d* Validate Edit screen for New Business tracking
     d ValidNew        pr

     d* Validate Edit screen for Renewal Business tracking
     d ValidRnw        pr

     d* Validate Status screen
     d ValidSts        pr

     d* Validate Audit Report selection
     d ValidAudRpt     pr

     d* Edit record for quote tracking
     d EdtQuote        pr

     d* Edit record for audit tracking
     d EdtAudit        pr

     d* Process Filter for Audit tracking record
     d ProcAudFlt      pr

     d* Process Edit for quote tracking record
     d ProcessEdt      pr

     d* Process Edit for audit tracking record
     d ProcEdtAud      pr

     d* Process Edit for New Business tracking record
     d ProcEdtNew      pr

     d* Process Edit for Renewal Business tracking record
     d ProcEdtRnw      pr

     d* Process change for audit representative
     d ProcRep         pr

     d* Process change for audit receiving date
     d ProcRcv         pr

     d* Process change for audit attempt date
     d ProcAtm         pr

     d* Process change for audit mailed date
     d ProcMal         pr

     d* Process change for audit contested date
     d ProcCnt         pr

     d* Process DARTS
     d ProcDarts       pr

     d* Process DARTS for Deletion
     d PrcDelDrt       pr

     d* Process History
     d PrcHstDtl       pr

     d* save filter
     d SavFilter       pr

     d* Reload filter
     d RldFilter       pr

     d* Process Status
     d ProcessSts      pr

     d* Move database record to screen for quote tracking
     d MoveDbSc        pr

     d* Move database record to screen for audit tracking
     d MvDbScAud       pr

     d* Move database record to screen for New Business tracking
     d MvDbScNew       pr

     d* Move database record to screen for Renewal Business tracking
     d MvDbScRnw       pr

     d* Save record for New Business tracking
     d SaveNew         pr

     d* Save record for Renewal Business tracking
     d SaveRnw         pr

     d* Set the value of scChoice
     dSetScChoice      pr                  like(scChoice)
     d inCo#                               like(ELco#)
     d inNewRenew                     1    const options(*nopass)
     d inQuote                        1    const options(*nopass)
     d inExcludeXFer                  1n   const options(*nopass)

     d* Validate filter selection
     d ValidAudFlt     pr

     d* Validate filter selection
     d ValidFilter     pr

     d* Darts
     dWrtDart          pr                  extPgm('DA700R')
     d ##key#                              like(##key#)
     d ##comm                              like(##comm)
     d ##cdat                              like(##cdat)
     d ##dcid                              like(##dcid)
     d ##pnm                               like(##pnm)
     d ##secl                              like(##secl)
     d ##user                              like(##user)
     d ##pgm                               like(##pgm)
     d ##fold                              like(##fold)
     d ##difl                              like(##difl)
     d ##ipgm                              like(##ipgm)
     d ##fl01                              like(##fl01)
     d ##fl02                              like(##fl02)
     d ##rmcd                              like(##rmcd)
     d ##seq#                              like(##seq#)
     d ##sbyl                              like(##sbyl)
     d ##com2                              like(##com2)
     d ##apid                              like(##apid)
     d ##idoc                              like(##idoc)
     d ##ique                              like(##ique)
     d ##imgf                              like(##imgf)
     d ##ipth                              like(##ipth)
     d ##pidx                              like(##pidx)
     d ##qpty                              like(##qpty)
     d ##dctp                              like(##dctp)
     d ##csid                              like(##csid)
     d ##code#                             like(##code#)

     d* Additional notes
     dWrtNotes         pr                  extPgm('SC014R')
     d ##key#                              like(##key#)
     d ##cseq                              like(##cseq)
     d ##cpnam                             like(##cpnam)

     d* Print Audit Report
     dPrtAudRpt        pr                  extPgm('W4700C')
     d ##co#                               like(##co#)
     d ##fnd#                              like(##fnd#)
     d ##date#                             like(##date#)
     d ##date1#                            like(##date1#)
     d ##date2#                            like(##date2#)
     d ##que#                              like(##que#)
     d ##mode#                             like(##mode#)

     d* Batch submit window
     dSubWdw           pr                  extPgm('WA007R')
     d* Claim Search
     d ClmSearch       pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     d QCmdExc         pr                  ExtPgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrRunCmd
     d/copy sPrString

     d/copy wPrAgent
     d/copy wPrAR
     d/copy wPrCarrier
     d/copy wPrClaims
     d/copy wPrEmpLr
     d/copy wPrGroup
     d/copy wPrLoc
     d/copy wPrProduce
     d/copy wPrRateSet
     d/copy wPrUsrPrms
     d/copy wtSearchDs
     d/copy sPrModule
     d/copy sPrLetter
     d/copy wPrPolCpy
     d* *** Beg Add *** 10/25/23 *** WC2922 ****************
     d/copy wPrPmtSvc
     d* *** End Add *** 10/25/23 *** WC2922 ****************

     D @@qte           s              1
     D intCo#          s              3  0
     D strCommand      s            255
    
     c* Employer Master File Key List
     c     keyWMEML      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div

     c     keyWMEML#     klist
     c                   kfld                    ##co
     c                   kfld                    AUfnd
     c                   kfld                    AUemp#
     c                   kfld                    AUdiv

     c     keyWMEML1     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div#

     c     keyWDELL1#    klist
     c                   kfld                    ##co
     c                   kfld                    AUfnd
     c                   kfld                    AUemp#
     c                   kfld                    AUdiv
     c                   kfld                    AUfyr

     c     keyWDELL8     klist
     c                   kfld                    ##co
     c                   kfld                    AUfnd
     c                   kfld                    AUemp#
     c                   kfld                    Aufyr

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

     c     keyWTACL      klist
     c                   kfld                    ##co
     c                   kfld                    ##aco

     c     keyWMAHL1     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##fyr
     c                   kfld                    QTagt#
     c                   kfld                    ##emp
     c                   kfld                    QTdiv

     c     keyWMAUL#     klist
     c                   kfld                    ##co

     c     keyWMAUL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp

     c     keyWAQTL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    scQteDiv

     c     keyWAQTL#     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp

     c     keyWTQSL      klist
     c                   kfld                    ##co
     c                   kfld                    QTtype

     c     keySMUPL      klist
     c                   kfld                    ##rep

     c     keyWMAXL      klist
     c                   kfld                    ##co
     c                   kfld                    AUfnd

     c     keyWMEXL      klist
     c                   kfld                    ##co
     c                   kfld                    AUfnd

     c     keyWMHAL      klist
     c                   kfld                    ##co
     c                   kfld                    AUfnd
     c                   kfld                    AUemp#
     c                   kfld                    AUdiv
     c                   kfld                    AUfyr

     c     keyWMFNL      klist
     c                   kfld                    ##co
     c                   kfld                    AUfnd

     c     keyWTTAL      klist
     c                   kfld                    ##co
     c                   kfld                    HAtypa

     c     keyWTCNL1     klist
     c                   kfld                    ##co
     c                   kfld                    ##key

     c     keyWDRTL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp

     c     keyWDRTL#     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##fyr#            3 0

     c     keyWMNBL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@mode                         mode
     c                   Parm                    @@next
     c                   Parm                    @@TabInd          5
     c                   Parm                    @@Qte             1

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = %int(@@rrn);
          $$RRN = %int(@@rrn);
       Endif;

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
          if (@@Mode = 'C') or (@@Mode = 'A');
            isEditable = *on;
          endif;
       Endif;

       // Tab Index Mode.
       If %parms > 3;
          TabIndex = %int(@@TabInd);
          If TabIndex < 0 or TabIndex > 3;
             TabIndex = 0;
          Endif;
          @@TabInd = '00000';
       Else;
          TabIndex = 0;
       Endif;

       // Quote Environment
       If %parms > 4;
          quoteMode = @@Qte;
          Select;
             When @@Qte = 'Q';
                qteMode = *on;
          Endsl;
       Endif;
       scEnv = quoteMode;

       // Verify if called from menu
       If %parms > 4;
          xxCall = @@Qte;
       Else;
          clear xxCall;
       Endif;

       Select;
       When mode = 'D';
          dspMode = *On;
       When mode = 'A';
          addMode = *On;
       When mode = 'C';
          chgMode= *On;
       Endsl;
       scActMode = mode;

       GetClientID(ClientID);
       scActCltId=ClientID;
       OverrideEmp(@@Qte);

       ProgramStart(process);

       If process;
          Main();
       Endif;

       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // Get the Audit Cx Status for the employer
       //===================================================================
     PAuditCxSts       b
     DAuditCxSts       pi             1

     d strCxSts        s              1

      /Free
       clear strCxSts;
       If %found(WMEML);
          If %found(WDELL);
             //Find policy effective date
             x1 = 1;
             x1 = %lookup(*zero:EF);

             If EF(x1) <> *zero;
                x1 = 5;
             Else;
                x1 = x1 - 1;
             Endif;

             Dow x1 > 0;
                 If EF(x1) > ELpfdt;
                    x1 = x1 - 1;
                 Else;
                    leave;
                 Endif;
             Enddo;

             // Determine if cancellation date exists, set canx flag
             If x1 = 0;
                strCxSts = ' ';
             Else;
                If CX(x1) = *zeros or CX(x1) > DateTo7(SystemDate);
                   strCxSts = ' ';
                Else;
                   strCxSts = 'C';
                Endif;
             Endif;
          Endif;
       Endif;
       If strCxSts = ' ' and AUptdt < DateTo7(SystemDate);
          strCxSts = 'E';
       Endif;

       Return strCxSts;
      /End-free

     PAuditCxSts       e

       //===================================================================
       // Get the Audit Plan Type for the employer
       //===================================================================
     PAuditPlanType    b
     DAuditPlanType    pi             1

     d intRRN          s              9  0

      /Free
       GetBillSch(elRRN:intRRN);
       Clear BSlPgm;
       Chain intRRN pbs0p;
       Select;
       When BSlPgm = 'L'
        or BSlPgm = 'R'
        or BSlPgm = ' ';
          Return BSlPgm;
       Other;
          Return 'O';
       Endsl;
      /End-free

     PAuditPlanType    e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnQteTrck = 0;
       clrQteTrk  = *On;
       dspQteTrSb = *Off;
       Write rcdQteTrck;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // ClrAudSubFil- Clears Audit SubFiles
       //===================================================================
     P ClrAudSubFil    B
     D ClrAudSubFil    PI
      /Free

       rrnAudTrck = 0;
       clrAudTrk  = *On;
       dspAudTrSb = *Off;

       If xxCall = 'M';
          Write rcdAudTrk;
       Else;
          Write rcdAudTrck;
       Endif;

      /End-Free
     P ClrAudSubFil    E
       //===================================================================
       // ClrRnwSubFil- Clear renewal SubFiles
       //===================================================================
     P ClrRnwSubFil    B
     D ClrRnwSubFil    PI
      /Free

       rrnRnwTrck = 0;
       clrRnwTrk  = *On;
       dspRnwTrSb = *Off;
       Write rcdNewTrck;

      /End-Free
     P ClrRnwSubFil    E
       //===================================================================
       // ChgStatus - Change status
       //===================================================================
     PChgStatus        B
     DChgStatus        PI

     DstrCmd           s             80
     DintCmdSize       s              3  0

      /Free

       Select;
       When Upper(scStsNew) = 'T';
          Exsr Transfer;
       When Upper(scStsNew) = 'C' or Upper(scStsNew) = 'R';
          Exsr WriteWAQTP;
       When Upper(scStsNew) = 'D' or Upper(scStsNew) = 'S';
          Exsr CrtNewBusTrk;
          Exsr ChgQuoteType;
       When Upper(scStsNew) = 'L' or Upper(scStsNew) = 'D';
          Exsr CrtNewBusTrk;
          Exsr ChgQuoteType;
          Exsr DltSubmHist;
       Other;
          Exsr ChgQuoteType;
       Endsl;

       //=============================================================
       // Change the quote status type
       //=============================================================
       Begsr ChgQuoteType;

       Chain scQteTrRRN WAQTP;

       If %Found;
          Exsr LoadWAQTP;
          Update pqt0p;
       Endif;

       Endsr;

       //==============================================================
       // Create New Business Tracking Record
       //==============================================================
       Begsr CrtNewBusTrk;

       Clear WCfl26;
       Chain 'WC' swc01;

       Select;
       When WCfl26 = 'Y';
          Clear pqt0p;
          Chain(n) scQteTrRRN pqt0p;
          Chain (QTco#:QTfnd:QTemp#) pnb00;
          If %found(WMNBL);
             Chain nbRRN WMNBP;
          Else;
             Clear pnb0p;
          Endif;
          Select;
          When Upper(scStsNew) = 'L'; // Lost
             NBlsDt = NBdt;
          When Upper(scStsNew) = 'D'; // Declined
             NBdcDt = NBdt;
          Endsl;
          NBuser = q1user;
          NBpgm  = q1pgm;
          NBdt   = DateTo7(SystemDate);
          NBtime = TimeTo6(SystemTime);
          If %found(WMNBP);
             Update pnb0p;
          Else;
             NBco#  = QTco#;
             NBfnd  = QTfnd;
             NBemp# = QTemp#;
             NBiUsr = NBuser;
             NBiPgm = NBpgm;
             NBiDt  = NBdt;
             NBiTm  = NBtime;
             Write pnb0p;
          Endif;
       Endsl;

       Endsr;

       //===============================================================
       // Delete WMSHP records
       //===============================================================
       Begsr DltSubmHist;

       strCmd = 'OVRDBF FILE(WMSHLQ) TOFILE(' +
         %trim(RtvQuoteLib('*QTE')) + '/WMSHLQ)';
       intCmdSize = %Checkr(' ':strCmd);

       Callp RunCmd(strCmd:intCmdSize);

       Open WMSHLQ;

       //Dou %eof(WMSHLQ);
       Dou not %found(WMSHLQ);
          Delete (QTco#:QTfnd:QTemp#) psh00q;
       Enddo;

       Close WMSHLQ;

       strCmd = 'DLTOVR FILE(WMSHLQ) LVL(*JOB)';
       intCmdSize = %Checkr(' ':strCmd);

       Callp(e) RunCmd(strCmd:intCmdSize);

       Endsr;

       //==============================================================
       // Load WAQTP fields (QTTYPE and maintenance)
       //==============================================================
       Begsr LoadWAQTP;

       QTtype = scStsNew;
       QTuser = q1User;
       QTpgm  = q1pgm;
       QTdt   = DateTo7(SystemDate);
       QTtime = TimeTo6(SystemTime);
       QTwkDt = QTdt;

       Endsr;

       //==============================================================
       // Transfer an employer from one agent to another
       //==============================================================
       Begsr Transfer;

      /If Defined (TRANSFER)
       // This code is being skipped so that working code can be promoted;
       Dou not *in90;
          *in90 = *off;
        //Exfmt WdwAgt#;
          If *inkl;
             Leavesr;
          Endif;
     C* * *
     C* * **INKJ         CASEQ     *ON           KJDO                     90
     C* * *              ENDCS
     C* * *
     C* *VALIDATE PRIMARY AGENT NUMBER
     C* * *
     C* * *AAKY1         CHAIN     PAA01                              60
     C* * *
     C* N90*IN60         IFEQ      *ON
     C* * *AASTAT        OREQ      'I'
     C* * *              SETON                                        20  90
     C* * *              ENDIF                                                  *IN60=*ON
     C* * *
     C* *AGENT # CANNOT BE THE SAME AS PREVIOUS AGENT#
     C* * *
     C* N90SCAGT#        IFEQ      SVAGT#
     C* * *              SETON                                        21  90
     C* * *              ENDIF                                                  SCAGT#=SVAGT#
     C* * *
       Enddo;
       Select;
       If qteMode;
          Exsr ChgQTyp;
       Other;
          Select;
          When Upper(QTtype) = 'C' or Upper(QTtype) = 'R';
             saveQTxFer = QTtype;
          Endsl;
          scStsNew = 'X';
          Exsr WriteWAQTP;
          scStsNew = 'T';
          Exsr WriteWAQTP;
          Clear QTxFer;
       Endsl;
      /Endif

       Endsr;

       //==============================================================
       // Write WAQTP record
       //==============================================================
       Begsr WriteWAQTP;

      /If Defined (TRANSFER)
       Select;
       When Upper(scStsNew) = 'T';
          QTagt# = scAgt#;
          QTxFer = saveQTxFer;
       When Upper(scStsNew) = 'X';
          QTagt# = svAgt#;
          QTxFer = saveQTxFer;
       Endsl;
      /Endif

       Exsr LoadWAQTP;

       QTiUsr = QTuser;
       QTiPgm = QTpgm;
       QTiDt  = QTdt;
       QTiTm  = QTtime;

       Write pqt0p;

       Feod WAQTP;

       Endsr;

      /End-free

     PChgStatus        e

       //===================================================================
       // Display/Hide "Clear Filter" button for quote tracking filter
       //===================================================================
     P loadClrFlt      B
     D loadClrFlt      PI

      /free

       // display clear filter button for quote tracking
       If dsPFilter.SSqtlv = *blanks and
        dsPFilter.SSqttr = *blanks and
        dsPFilter.scQteFrmDt = *zeros and
        dsPFilter.scQteToDt = *zeros and
        dsPFilter.SSSelFltr  = *blanks;
          dspclrfltr = *off;
       Else;
          dspclrfltr = *on;
       Endif;

       // display clear filter button for audit tracking
       If xxCall <> 'M';
          dsAFilter.ssEmp# = ##Emp;
          ssEmp# = ##Emp;
          dsAFilter.ssFnd# = %editc(##Fnd:'X');
          ssFnd# = %editc(##Fnd:'X');
          dspssEmp = *on;
       Else;
          dspssEmp = *off;
       Endif;

       If dsAFilter.ssEmp# > *zeros and dsAFilter.ssFnd# <> ' ';
          dspssDiv = *off;
       Else;
          dspssDiv = *on;
       Endif;

       If xxCall='M' and (dsAFilter.ssEmp#<>*zeros or dsAFilter.ssFnd#<>' ');
          dspAudFltr = *on;
       Else;
          dspAudFltr = *off;
       Endif;

       If dspAudFltr = *off and
        dsAFilter.ssAudSts   = *blanks and dsAFilter.ssDiv#     = *blanks and
        dsAFilter.ssFyr      = *zeros  and dsAFilter.ssAudCo    = *blanks and
        dsAFilter.ssAudRep   = *blanks and dsAFilter.ssCntFlg   = *blanks and
        dsAFilter.ssOutSFlg  = *blanks and dsAFilter.ssAudCan   = *blanks and
        dsAFilter.ssAudUnw   = *blanks and dsAFilter.ssAudRvw   = *blanks and
        dsAFilter.ssAudMngr  = *blanks and dsAFilter.ssAudPlnTy = *blanks and
        dsAFilter.ssReqFrDt  = *zeros  and dsAFilter.ssRcvFrDt  = *zeros  and
        dsAFilter.ssAtmpFrDt = *zeros  and dsAFilter.ssExpFrDt  = *zeros  and
        dsAFilter.ssPrtFrDt  = *zeros  and dsAFilter.ssCalFrDt  = *zeros  and
        dsAFilter.ssFnlFrDt  = *zeros  and dsAFilter.ssMldFrDt  = *zeros  and
        dsAFilter.ssReqToDt  = *zeros  and dsAFilter.ssRcvToDt  = *zeros  and
        dsAFilter.ssAtmpToDt = *zeros  and dsAFilter.ssExpToDt  = *zeros  and
        dsAFilter.ssPrtToDt  = *zeros  and dsAFilter.ssCalToDt  = *zeros  and
        dsAFilter.ssFnlToDt  = *zeros  and dsAFilter.ssMldToDt  = *zeros  and
        dsAFilter.ssAudStsF  = *blanks and dsAFilter.ssAudTyp   = *blanks;
          dspAudFltr = *off;
       Else;
          dspAudFltr = *on;
       Endif;


      /end-free
     P loadClrFlt      E
       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     P LoadSelection   B
     D LoadSelection   PI

     dfyrLine          s             31
     ddivLine          s             49
     dfirst            s               n   inz(*on)
      /free
       xxSt = RatingSt(ELco#:ELrtcd);

       clear fyrChoices;
       fyrChoices = 'ELco# = ' + %char(##co) + ' AND ELfnd = '
        + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
        + ' AND ELdiv = ' + %char(kkdiv);

       clear divChoices;
       divChoices = 'EMco# = ' + %char(##co) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

      /end-free
     P LoadSelection   E
       //=================================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes in Filter
       //=================================================================================
     P LoadSelFlt      B
     D LoadSelFlt      PI

      /free

       userGroup# = GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       clear divChoice;
       divChoice = 'EMco# = ' + %char(##co) + ' AND EMfnd = '
        + ssFnd# + ' AND EMemp# = ' + %char(ssEmp#);

       clear scAudCoSel;
       clear scAudTpSel;
       scAudCoSel = 'ACco# = ' + %char(##co);
       scAudTpSel = 'TAco# = ' + %char(##co);

      /end-free
     P LoadSelFlt      E
       //===================================================================
       // Load Quote Tracking Information
       //===================================================================
     P LoadQteTab      B
     d LoadQteTab      PI
      /free

       // clear the subfile.
       clrQteTrk  = *Off;
       dspQteTrSb = *On;

       Setll KeyWAQTL# pqt12;
       Reade KeyWAQTL# pqt12;
       dow not %eof(WAQTL12);
          clear sflQteTrck;

          meetFlt = *on;

          // Validate for filter
          MeetFilter();
          If meetFlt = *on;

             // Write Quote tracking info
             // name
             scQteNam   = %trim(QTname);
             scQteEmp   = QTemp#;
             scQteDiv   = QTdiv;
             If QTstat = 'Q';
                scQteLiv = 'Quote';
             Else;
                scQteLiv = 'Live';
             Endif;

             scQtEffDt  = CvtToDate(QTefdt);
             scQtTypDt  = CvtToDate(QTwkdt);
             scQtSubDt  = CvtToDate(QTordt);
             scQtBusDt  = CvtToDate(QTnbdt);
             scQtSentDt = CvtToDate(QTfxdt);
             scQtPrem   = QTprem;

             scQteTrRRN = qtRRN12;

             clear scQteSts;
             Select;
             When QTtype = 'F';
                QTtype = 'p';
             When QTtype = 'G';
                QTtype = 'q';
             Endsl;
             Chain keyWTQSL pqs01;
             If %found(WTQSL1);
                scQteSts = %trim(QSdesc);
             Endif;

             rrnQteTrck = rrnQteTrck + 1;
             Select;
             When mode = 'D';
                dspMod = *off;
             When qteMode = *on  and QTstat <> 'Q';
                dspMod = *off;
             When qteMode = *off and QTstat <> 'W';
                dspMod = *off;
             Other;
                dspMod = *on;
             Endsl;
             Write sflQteTrck;
          Endif;
          Reade KeyWAQTL# pqt12;
       Enddo;
       isContribution = GetPrmContflg(##co:##fnd);
       If isContribution;
          flexhead1 = 'Name,Employer,Division,Quote/Live,Status,Effective Date,+
           Status Change Date,Original Submit Date,New Business Date,Sent Date,+
           Contribution';
       Else;
          flexhead1 = 'Name,Employer,Division,Quote/Live,Status,Effective Date,+
           Status Change Date,Original Submit Date,New Business Date,Sent Date,+
           Premium';
       Endif;

       rrnQteTrck = 1;

      /end-free
     P LoadQteTab      E
       //===================================================================
       // Load Audit Tracking Information
       //===================================================================
     P LoadAudTab      B
     d LoadAudTab      PI
      /free

       // clear the subfile.
       clrAudTrk  = *Off;
       dspAudTrSb = *On;

       If xxCall <> 'M';
          Setll KeyWMAUL pau33;
          Reade KeyWMAUL pau33;
       Else;
          Setll KeyWMAUL# pau33;
          Reade KeyWMAUL# pau33;
       Endif;
       dow not %eof(WMAUL33);
          clear sflAudTrck;
          clear sflAudTrk;
          meetAudFlt = *on;

          // Validate for filter
          If MeetAudFltr();

             // Write Audit tracking info

             scAudEmp = AUemp#;
             scAudDiv = AUdiv;
             scAudFnd = AUFnd;
             Chain keyWMFNL pfn00;
             If %found(WMFNL);
                scAudFndNm = %trim(FNname);
             Endif;

             Chain keyWMEML# pem00;
             If %found(WMEML);
                scAudNam1= EMname;
                scAudNam = EMname;
                Chain(n) keyWDELL1# pel00;
                If %found(WDELL);
                   strRRN3 = %Editc(elRRN:'X');
                   urlEmpName = CreateURL('STDRVPGM':scAudNam:strRRN3:'C':
                    'WTPOLSUM');
                   //Find policy effective date
                   x1 = 1;
                   x1 = %lookup(*zero:EF);

                   If EF(x1) <> *zero;
                      x1 = 5;
                   Else;
                      x1 = x1 - 1;
                   Endif;

                   Dow x1 > 0;
                      If EF(x1) > ELpfdt;
                         x1 = x1 - 1;
                      Else;
                         leave;
                      Endif;
                   Enddo;

                   scAudCX = ' ';

                   // Determine if cancellation date exists, set canx flag
       //***  Begin Chg  ***  11/26/19  ********************************
       //***       If x1 = 0;
       //***          scAudCX = ' ';
       //***       Else;
       //***          If CX(x1) = *zeros or CX(x1) > ELPtdt;
       //***             scAudCX = ' ';
       //***          Else;
       //***             scAudCX = 'Cancellation';
       //***          Endif;
       //***       Endif;
       //***  End Chg  ***  11/26/19  **********************************
                Else;
                   clear urlEmpName;
                Endif;
             Endif;
       //***  Begin Chg  ***  11/26/19  ********************************
       //*** If scAudCX = ' ' and AUptdt < DateTo7(SystemDate);
       //***    scAudCX = 'Expired';
       //*** Endif;
             If AUCxfl = 'Y';
                scAudCX = 'Cancellation';
             Else;
                scAudCX = 'Expired';
             Endif;
       //***  End Chg  ***  11/26/19  **********************************

             scAudYr  = AUfyr;
             scAudReq = CvtToDate(AUrqdt);
             scAudAtmp= AUattm;
             scAudRec  = CvtToDate(AUrcvd);
             scFnlAud  = CvtToDate(AUprdt);
             scAudCoDt = CvtToDate(AUcntd);
             If AUrefl = 'Y';
                scAudRev = 'Yes';
             Else;
                scAudRev = 'No';
             Endif;
             If AUstat = 'A';
                scAudSts = 'Active';
             Else;
                scAudSts = 'Inactive';
             Endif;

             scAudRp  = AUrep;
             scAudCo  = AUaco;
             If AUrep <> ' ';
                ##rep =  AUrep;
                Chain keySMUPL pup01;
                If %found(SMUPL1);
                   scRepName = UPusnm;
                   If xxCall = 'M';
                      scAudRp = UPusnm;
                   Endif;
                Endif;
             Endif;

             If AUaco <> ' ';
                ##aco = AUaco;
                Chain keyWTACL pac00;
                If %found(WTACL);
                   scCoName = ACname;
                   If xxCall = 'M';
                      scAudCo = ACname;
                   Endif;
                Endif;
             Endif;
             scAudTrRRN = auRRN33;
             rrnAudTrck = rrnAudTrck + 1;
             If rrnAudTrck = 1;
                scAudRow = 'A';
             Else;
                clear scAudRow;
             Endif;
             If temprrn <> 0;
                If rrnAudTrck = temprrn;
                   scAudRow = 'A';
                Else;
                   clear scAudRow;
                Endif;
             Endif;
             If xxCall = 'Q' or mode = 'D';
                dspMod  = *Off;
             Else;
                dspMod  = *On;
             Endif;

             If xxCall <> 'M';
                Write sflAudTrck;
             Else;
                Write sflAudTrk;
             Endif;

             If rrnAudTrck >= 9999 and xxCall = 'M';
                str9999msg = 'Your filter returned too many records. ' +
                 'Click on Results to see first 9999 records Or, click Filter '+
                 'to narrow down your filter results';
                btnAudFltr = PopTwoBtn(str9999msg:'Audit Tracking':'Filter':
                 'Results');
                Leave;
             Endif;
          Endif;
          If xxCall <> 'M';
             Reade KeyWMAUL pau33;
          Else;
             Reade KeyWMAUL# pau33;
          Endif;
       Enddo;

       isContribution = GetPrmContflg(##co:##fnd);
       If isContribution;
          flxDtlHdg1 = 'Contribution Accounting';
       Else;
          flxDtlHdg1 = 'Premium Accounting';
       Endif;
       rrnAudTrck = 1;
       If temprrn <> 0;
          rrnAudTrck = temprrn;
       Endif;
       temprrn = 0;
      /end-free
     P LoadAudTab      E
       //===================================================================
       // Load Renewal Business Tracking Information
       //===================================================================
     P LoadRnwTab      B
     d LoadRnwTab      PI
      /free

       // clear the subfile.
       clrRnwTrk  = *Off;
       dspRnwTrSb = *On;
       clear ##div#;
       ##div# = GetStRptDv(ELco#:ELfnd:ELEmp#);

       SetGt keyWDRTL# prt00;
       Readpe keyWDRTL prt00;
       dow not %eof(WDRTL);
          clear sflRnwBsTr;

          // Write renewal tracking info
          scRwYear  = RTfyr;
          Chain keyWMEML1 pem00;
          If %found(WMEML);
             scRwName = EMname;
          Endif;
          scRwCrtDt = CvtToDate(RTycdt);
          scRwRvwDt = CvtToDate(RTuwdt);
          scRwQtADt = CvtToDate(RTqadt);
          scRwAprDt = CvtToDate(RTaadt);
          scRwGenDt = CvtToDate(RTpgdt);
          scRwPlMDt = CvtToDate(RTpold);
          scRwPnCDt = CvtToDate(RTpcdt);
          scRwPnMDt = CvtToDate(RTpand);
          scRwPnPDt = CvtToDate(RTppdt);

          scRnwTrRrn = rtRRN;

          rrnRnwTrck = rrnRnwTrck + 1;
          If rrnRnwTrck = 1;
             scRnwRow = 'A';
          Else;
             clear scRnwRow;
          Endif;
          If mode = 'D';
             dspMod  = *Off;
          Else;
             dspMod  = *On;
          Endif;
          Write sflRnwBsTr;

          Readpe keyWDRTL prt00;
       Enddo;

       rrnRnwTrck = 1;

      /end-free
     P LoadRnwTab      E
       //===================================================================
       // Load New Business Tracking Information
       //===================================================================
     P LoadNewTab      B
     d LoadNewTab      PI
      /free

       Chain keyWMNBL pnb00;
       If %found(WMNBL);

          // Write New Business tracking info
          scNwRecDt = DtMDYY(NBrcdt);
          scNwEntDt = DtMDYY(NBendt);
          scNwRvwDt = DtMDYY(NBiwdt);
          scNwQteDt = DtMDYY(NBqtdt);
          scNwDclDt = DtMDYY(NBdcdt);
          scNwLstDt = DtMDYY(NBlsdt);
          scNwAprDt = DtMDYY(NBaadt);
          scNwGenDt = DtMDYY(NBpgdt);
          scNwPlMDt = DtMDYY(NBpold);
          scNwPnRDt = DtMDYY(NBprdt);
          scNwPnCDt = DtMDYY(NBpcdt);
          scNwPnMDt = DtMDYY(NBpand);
          scNwPnPDt = DtMDYY(NBppdt);
          scNewTrRrn = nbRRN;
          If mode = 'D';
             dspMod  = *Off;
          Else;
             dspMod  = *On;
          Endif;
       Endif;

      /end-free
     P LoadNewTab      E
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free


       If xxCall = 'M';
          ProcAudFlt();
       Endif;

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          If xxCall <> 'M';
            //* Check if new division or policy year has been selected.
            If kkFyr <> scFyr or kkDiv <> scDiv;
               scFyr = kkFyr;
               scDiv = kkDiv;
               Chain(n) key1WDELL PEL00;
               If not %found;
                  Setgt keyWDELL PEL00;
                  Readpe(n) keyWDELL PEL00;
                  @@rrn = %EditC(elrrn:'X');
                  scFyr = ELfyr;
                  kkFyr = ELfyr;
                  kkDiv = ELdiv;
               Else;
                  @@rrn = %EditC(elrrn:'X');
               Endif;
               xxRRN = elrrn;
               LoadSelection();
            Endif;

             // Load action bar
             LoadActionBar();
          Endif;
          // Load Filter Clear Button.
          loadClrFlt();

          // Clear Sub-files for audit tracking
          ClrAudSubFil();

          // Load Audit Tracking Information
          LoadAudTab();

          If xxCall <> 'M';
             // Clear Sub-files for quote tracking
             ClrSubfiles();

             // Load Quote Tracking Information
             LoadQteTab();

             // Clear Sub-files for New/Renewal business tracking
             ClrRnwSubFil();

             // Load Renewal Business Tracking Information
             LoadRnwTab();

             // Load New Business Tracking Information
             LoadNewTab();
          Endif;
          // Clear Visit Tracking Grids
          ClrVstSubFiles();
          //****** Begin Add *** 07/10/19 **********************************
          visVstAdd  = *on;
          //*** Beg Add ***** 03/06/25 ***********************
          If mode = 'D';
             visVstAdd = *Off;
          Endif;
          //*** End Add ***** 03/06/25 ***********************
          visVstEdit  = *on;
          visVstDel  = *on;
          //****** End   Add *** 07/10/19 **********************************
          LoadVisitGrid();
          if (onLoadRrn <> 0);
             LoadVisitDtl(OnLoadRrn);
          else;
             ClearVisitDtl();
             fSetVstDtX = vstScreenX;
             fSetVstDtH = 50;
          endif;

          // Process policy summary screen.
          If xxCall <> 'M' or (xxCall = 'M' and btnAudFltr = *off);
             Exsr ExfmtScreens;
          Endif;

          Select;
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
             btnShowWin = *off;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // Quote tracking filter Processing.
          When btnFilter = *on;
             Dou (BtnQteAppf = *on and isValid = *off) or BtnFilCan = *on;

                isValid = *off;
                scChoice = SetScChoice(ELco#:'N':'*');
      //**** Begin Add  ***  06/23/21  ****************************************
                outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
                ExFmt RcdSelFltr;
                If BtnQteAppf = *on;
                   ValidFilter();
                   If isValid = *off;
                      dsPFilter.ssQtLv = ssQtLv;
                      dsPFilter.ssQtTr = ssQtTr;
                      dsPFilter.scQteFrmDt = scQteFrmDt;
                      dsPFilter.scQteToDt  = scQteToDt;
                      dsPFilter.SSSelFltr  = SSSelFltr;
                   Endif;
                Endif;
                If BtnFilCan = *on;
                   ssQtLv = dsPFilter.ssQtLv;
                   ssQtTr = dsPFilter.ssQtTr;
                   scQteFrmDt = dsPFilter.scQteFrmDt;
                   scQteToDt  = dsPFilter.scQteToDt;
                   SSSelFltr  = dsPFilter.SSSelFltr;
                Endif;
             Enddo;

          // Quote tracking filter clear processing
          When btnClrFltr = *on;
             clear dsPFilter;
             clear rcdSelFltr;
             dspclrfltr = *off;

          // Audit tracking filter Processing.
          When btnAudFltr = *on;
             ProcAudFlt();

          // Audit filter clear processing
          When btnAudClrF = *on;
             clear dsAFilter;
             clear RcdAudFltr;
             dspAudFltr = *off;

          When btnRefresh = *On;
             btnRefresh =*Off;
             ClrVstSubFiles();
             LoadVisitGrid();
             if (onLoadRrn <> 0);
               LoadVisitDtl(OnLoadRrn);
             else;
               ClearVisitDtl();
               fSetVstDtX = vstScreenX;
               fSetVstDtH = 50;
             endif;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          // Audit report processing
          When scAudReprt = *on;
             scAudReprt = *off;
             If scAudRpt = 'V';
                ProcView();
             Else;
                ProcPrint();
             Endif;

          When TabIndex = 3;

             Select;
             When kkDiv <> polTrakDiv;
                polTrakDiv = kkDiv;
                ssEmpDiv = %subst(ssEmpDiv:1:12) + %EditC(polTrakDiv:'X');

             When vstTypFlt = *On;
                SetVisitFilter();
                InzSubset();
                btnRefresh = *on;

             When clrVstFlt = *On;
                ClrVisitFilter();
                btnRefresh = *on;

             When btnRefresh = *On;
                //Loop will Refresh

             When BtnVstAdd  = *on;
                BtnVstAdd = *off;
                AddVisitDtl();
                btnRefresh = *on;

             When RRNVstRow <> 0;
                ProcessSFL();

             Other;

             Endsl;

          Other;

             //* Popup Windows
             Popup();

             ProcOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       If xxCall <> 'M';

          Select;

          When TabIndex = 0;
             TabHeight = 1050;
             APnlHeight = TabHeight + 50;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdQteTrck;
             Exsr ReadAction;
             Read rcdPanel;
          When TabIndex = 1;
             TabHeight = 1050;
             APnlHeight = TabHeight + 50;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdNewTrck;
             Exsr ReadAction;
             Read rcdPanel;
          When TabIndex = 2;
             TabHeight = 1050;
             APnlHeight = TabHeight + 50;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdAudTrck;
             Exsr ReadAction;
             Read rcdPanel;
          When TabIndex = 3;
             if fSetVstDtY < 320;
                fSetVstDty = 320;
             endif;
             TabHeight = 800 + fSetVstDtH;
             APnlHeight = TabHeight + 50;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdVstTrk;
             Exsr ReadAction;
             Read rcdPanel;
          EndSl;
       Else;
          Write rcdAudPnl;
          //*** Beg Chg *** 04/18/24 *** PHW115440 *************************
          //***Exfmt rcdAudTrk;
          Write rcdAudTrk;
          Read rcdAudTrk;
          //*** End Chg *** 04/18/24 *** PHW115440 *************************
       Endif;

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

      /end-free

     P Main            E
       //==================================================================
       // MeetFilter - Determine if the records meet the Filter criteria
       //==================================================================
     P MeetFilter      b
     d MeetFilter      PI
      /free

       // Check for Policy if in Quote or Live
       If ssQtLv <> ' ' and ssQtLv <> QTstat;
          meetFlt = *off;
       Endif;

       // Check Tracking Type
       Select;
       When QTtype = 'F';
          QTtype = 'p';
       When QTtype = 'G';
          QTtype = 'q';
       Endsl;
       If ssQtTr <> ' ' and Upper(ssQtTr) <> Upper(QTtype);
          meetFlt = *off;
       Endif;

       // Check Agent and year
       Chain keyWMAHL1 pah01;
       If not %found(WMAHL1);
          meetFlt = *off;
       Endif;

       // Check for date range

       TempFrDt = 0;
       TempToDt = 0;

       TempFrDt = dtCYMD(scQteFrmDt);
       TempToDt = dtCYMD(scQteToDt);

       If scQteFrmDt > 0 and scQteToDt > 0;
          If  (SSSelFltr = 'E' and (TempFrDt > QTefdt or TempToDt < QTefdt))
           or (SSSelFltr = 'F' and (TempFrDt > QTfxdt or TempToDt < QTfxdt))
           or (SSSelFltr = 'N' and (TempFrDt > QTnbdt or TempToDt < QTnbdt))
           or (SSSelFltr = 'O' and (TempFrDt > QTordt or TempToDt < QTordt))
           or (SSSelFltr = 'T' and (TempFrDt > QTwkdt or TempToDt < QTwkdt));
             meetFlt = *off;
          Endif;
       Endif;

      /end-free
     P MeetFilter      e
       //==================================================================
       // MeetAudFltr - Determine if the records meet the Filter criteria
       //==================================================================
     P MeetAudFltr     b
     d MeetAudFltr     PI             1n
      /free

       // Check for record status
       If ssAudSts <> ' ' and ssAudSts <> AUstat;
          Return *off;
       Endif;

       // Check for Employer
       If ssEmp# <> *zeros and ssEmp# <> AUemp#;
          Return *off;
       Endif;

       // Check for Group
       If ssFnd# <> ' ';
          If %int(ssFnd#) <> AUfnd;
             Return *off;
          EndIf;
       Else;
          // Only allow user to view groups they are authorized to see
          restType = UsrRstFlg(Q1user);
          Select;
          When restType  = 'M';
             ChkGrpAuth(AUco#:Q1user:'W':returnCode:authorized:AUfnd);
             If authorized <> 'A';
                Return *Off;
             EndIf;
          When restType  = 'S' Or restType = 'Y';
             If AUfnd <> userGroup#;
                Return *Off;
             EndIf;
          EndSl;
       Endif;


       // Check for Division
       If ssDiv# <> ' ' and %int(ssDiv#) <> AUdiv;
          Return *off;
       Endif;

       // Check for Policy year
       If ssfyr <> *zeros and ssfyr <> AUfyr;
          Return *off;
       Endif;

       // Check for Audit Company
       If ssAudCo <> ' ' and ssAudCo <> AUaco;
          Return *off;
       Endif;

       // Check for Audit representative
       If ssAudRep <> ' ' and ssAudRep <> AUrep;
          Return *off;
       Endif;

       // Check for Audit Contested Flag
       If ssCntFlg <> ' ' and ssCntFlg <> AUcnfl;
          Return *off;
       Endif;

       // Check for Audit Outstanding flag
       If ssOutSFlg <> ' ' and ((ssOutSFlg = 'Y' and  AUrcvd >= AUrqdt) or
                                (ssOutSFlg = 'N' and  AUrcvd < AUrqdt));
          Return *off;
       Endif;

       // Only chain to the required files, if they are needed
       If ssAudCan <> *blanks
        or ssAudUnw <> *blanks
        or ssAudRvw <> *blanks
        or ssAudMngr <> *blanks;
          Clear EMaMgr;
          Clear EMaRvw;
          Clear EMuWtr;
          Chain keyWMEML# pem00;
       Endif;

       If ssAudCan <> *blanks
        or ssAudPlnTy <> *blanks;
          Clear elRRN;
          Chain(n) keyWDELL1# pel00;
       Endif;


       If ssAudCan <> *blanks and ssAudCan <> AuditCxSts;
          Return *off;
       Endif;

       // Check for Audit Underwriter
       If ssAudUnw <> ' ' and ssAudUnw <> EMuwtr;
          Return *off;
       Endif;

       // Check for Audit Reviewer
       If ssAudRvw <> ' ' and ssAudRvw <> EMarvw;
          Return *off;
       Endif;

       // Check for Audit Manager
       If ssAudMngr <> ' ' and ssAudMngr <> EMamgr;
          Return *off;
       Endif;

       // Check for Audit Plan type
       If ssAudPlnTy <> ' ' and ssAudPlnTy <> AuditPlanType();
          Return *off;
       Endif;

       If ssReqFrDt > 0
        and (DtCYMD(ssReqFrDt) > AUrqdt or DtCYMD(ssReqToDt) < AUrqdt);
          Return *off;
       Endif;

       // Check for Received date range
       If ssRcvFrDt > 0
        and (DtCYMD(ssRcvFrDt) > AUrcvd or DtCYMD(ssRcvToDt) < AUrcvd);
          Return *off;
       Endif;

       // Check for Attempted date range
       If ssAtmpFrDt > 0
        and (DtCYMD(ssAtmpFrDt) > AUatdt or DtCYMD(ssAtmpToDt) < AUatdt);
          Return *off;
       Endif;

       // Check for Expiration date range
       If ssExpFrDt > 0
        and (DtCYMD(ssExpFrDt) > AUptdt or DtCYMD(ssExpToDt) < AUptdt);
          Return *off;
       Endif;

       // Check for Print date range
       If ssPrtFrDt > 0
        and (DtCYMD(ssPrtFrDt) > AUapdt or DtCYMD(ssPrtToDt) < AUapdt);
          Return *off;
       Endif;

       // Check for Calculation date range
       If ssCalFrDt > 0
        and (DtCYMD(ssCalFrDt) > AUacdt or DtCYMD(ssCalToDt) < AUacdt);
          Return *off;
       Endif;

       // Check for Final Print date range
       If ssFnlFrDt > 0
        and (DtCYMD(ssFnlFrDt) > AUprdt or DtCYMD(ssFnlToDt) < AUprdt);
          Return *off;
       Endif;

       // Check for Mail date range
       If ssMldFrDt > 0
        and (DtCYMD(ssMldFrDt) > AUmail or DtCYMD(ssMldToDt) < AUmail);
          Return *off;
       Endif;

       // Check for Audit type
       If ssAudTyp <> ' ' and ssAudTyp <> AUtypa;
          Return *off;
       Endif;

       // Check for Audit Status Flag
       If ssAudStsF <> ' ' and ((ssAudStsF = 'S' and AUrsgn <> 'Y') or
        (ssAudStsF = 'V' and AUrefl <> 'Y') or
        (ssAudStsF = 'A' and AUrvfl <> 'Y'));
          Return *off;
       Endif;

       Return *on;

      /end-free
     P MeetAudFltr     e
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free
       // Callp PopOneBtn('On hold for for debug':'Debugging':'Continue');

       If xxCall <> 'M';
          // Chain to Employer Detail for Program.
          Chain xxRrn PEL0P;
          scActRrn=xxRrn;

          // Process only if valid employer detail record found.
          If not %found;
             processMe = *Off;
             Return;
          Endif;

          intCo#  = ELco#;
          ##co    = ELco#;
          ##Fnd   = ELfnd;
          ##Emp   = ELemp#;
          ##Fyr   = ELfyr;
          saveFyr = ELfyr;
          saveEffDt = ELpfdt;
          ##fyr#  = *hival;
          scCo#  = ELco#;
          scFnd  = ELfnd;
          scEmp# = ELemp#;
          scDiv  = ELdiv;
          scFyr  = ELfyr;

          kkDiv = ELdiv;
          ##Div   = kkdiv;
          kkFyr = ELfyr;
          LoadSelection();
       Endif;

       clear dsPFilter;
       clear dsAFilter;

       If xxCall = 'M';
          // Get Company
          Chain q1user pup0u;
          If %found(WMUPL);
             ##co = u_UPco#;
             ##que# = u_UPoutq;
          Else;
             ##co = 777;
          Endif;
       Endif;

       APnlHeight = 1100;
       processMe = *on;

       today = DateTo7(%Date());
       // Set the Category
       @@Category = 'Policy';
       // Set the x-coordinate
       vstScreenX = 250;
       fSetPolDtY = 320;
       fSetClmDtY = 320;
       fSetAgtDtY = 320;
       fSetVstDtY = 320;
       // Initialize Setup
       InzSetup();

       Return;

      /end-free
     P ProgramStart    E
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     P ProcOther       B
     d ProcOther       PI
      /free

       ReadC sflQteTrck;
       Select;
       // process for Editing Quote tracking record
       When scEdtQte = *on;
          scEdtQte = *off;
          clear RcdEdtQte;
          BtnCancel = *off;
          BtnSavEdt = *off;
          ProcessEdt();

       // processing for Status
       When scStsQte = *on;
          scStsQte = *off;
       // clear RcdSelSts;
          BtnCanSts = *off;
          BtnConSts = *off;
          ProcessSts();
       Endsl;

       ReadC sflAudTrck;
       Dow not %eof;
          Select;
          // process for Editing Audit tracking record
          When scEdtAud = *on;
             scEdtAud = *off;
             clear RcdEdtAud;
             BtnSavEAud = *off;
             BtnCanAudE = *off;
             ProcEdtAud();

          // process for deleting Audit tracking record
          When scDelAud = *on;
             Chain scAudTrRrn pau0p;
             Chain keyWMEXL pex00;
             If %found(WMEXL) and EXaudl <> *blanks;
                PrcDelDrt();
             Endif;
             scDelAud = *off;
             delete pau0p;
             unlock WMAUP;

             // Clear Audit contested Flag on deleting Audit Tracking record
             Chain keyWDELL1# pel00;
             If %found(WDELL);
                If ELctst = 'Y';
                   ELctst = ' ';
                   ELuser = q1user;
                   ELpgm  = q1pgm;
                   ELdt   = Dateto7(SystemDate());
                   ELtime = Timeto6(SystemTime());
                   Update pel00;
                Endif;
             Endif;
             Unlock WDELL;

          // process for Audit Company Assignment History
          When scHstAud = *on;
             scHstAud = *off;
             PrcHstDtl();
          Endsl;
          ReadC sflAudTrck;
       Enddo;

       ReadC sflAudTrk;
       Dow not %eof;
          Select;
          // process for Editing Audit tracking record
          When scEdtAud = *on;
             scEdtAud = *off;
             clear RcdEdtAud;
             BtnSavEAud = *off;
             BtnCanAudE = *off;
             ProcEdtAud();

          // process for deleting Audit tracking record
          When scDelAud = *on;
             Chain scAudTrRrn pau0p;
             Chain keyWMEXL pex00;
             If %found(WMEXL) and EXaudl <> *blanks;
                PrcDelDrt();
             Endif;
             scDelAud = *off;
             delete pau0p;
             unlock WMAUP;

          // process for Audit Company Assignment History
          When scHstAud = *on;
             scHstAud = *off;
             PrcHstDtl();
          Endsl;
          ReadC sflAudTrk;
       Enddo;

       ReadC sflRnwBsTr;
       Dow not %eof;
          // process for Editing Renewal Business tracking record
          If scRnwEdt = *on;
             scRnwEdt = *off;
             clear RcdEdtRnw;
             BtnSavRnw = *off;
             BtnCanRnw = *off;
             ProcEdtRnw();
          Endif;
          ReadC sflRnwBsTr;
       Enddo;

       // process for Editing New Business tracking record
       If scNewEdt = *on;
          scNewEdt = *off;
          clear RcdEdtNew;
          BtnSavNew = *off;
          BtnCanNew = *off;
          ProcEdtNew();
       Endif;

      /end-free
     P ProcOther       E
       //===================================================================
       // Validate - Validate edit screen for quote tracking
       //===================================================================
     P Validate        B
     d Validate        PI
     c
      /free

       isValid  = *off;

       erEdtChgDt = *off;
       erEdtEffDt = *off;
       erEdtSubDt = *off;
       erEdtSntDt = *off;
       intDate = 0;

       // Check if updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WAQTP':scQteTrRrn:'QTdt':'QTtime':
        'QTuser':QTdt:QTtime);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid = *on;
          Return;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       // If Status Change Date is valid.
       If scEdtChgDt > 0;
          intDate = DtCYMD(scEdtChgDt);
          If intDate = 0;
             msEdtChgDt = 'WCR0500';
             erEdtChgDt = *on;
             isValid = *on;
          Endif;
       Else;
          intDate = 0;
       Endif;

       // If Effective date is valid.
       intDate = 0;
       If scEdtEffDt > 0;
          intDate = DtCYMD(scEdtEffDt);
          If intDate = 0;
             msEdtEffDt = 'WCR0500';
             erEdtEffDt = *on;
             isValid = *on;
          Endif;
       Else;
          intDate = 0;
       Endif;

       // If Submission Date is valid.
       intDate = 0;
       If scEdtSubDt > 0;
          intDate = DtCYMD(scEdtSubDt);
          If intDate = 0;
             msEdtSubDt = 'WCR0500';
             erEdtSubDt = *on;
             isValid = *on;
          Endif;
       Else;
          intDate = 0;
       Endif;

       // If Sent date is valid.
       intDate = 0;
       If scEdtSntDt > 0;
          intDate = DtCYMD(scEdtSntDt);
          If intDate = 0;
             msEdtSntDt = 'WCR0500';
             erEdtSntDt = *on;
             isValid = *on;
          Endif;
       Else;
          intDate = 0;
       Endif;

       // If New Business date is valid.
       intDate = 0;
       If scEdtBusDt > 0 and protNewBus = *off;
          intDate = DtCYMD(scEdtBusDt);
          If intDate = 0;
             msEdtBusDt = 'WCR0500';
             erEdtBusDt = *on;
             isValid = *on;
          Endif;
       Else;
          intDate = 0;
       Endif;

      /end-free
     P Validate        E
       //===================================================================
       // ValidAud - Validate edit screen for audit tracking
       //===================================================================
     P ValidAud        B
     d ValidAud        PI
     c
      /free

       isValid  = *off;

       erAudRep   = *off;

       // Check if updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WMAUP':scAudTrRrn:'AUdt':'AUtime':
        'AUuser':AUdt:AUtime);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid = *on;
          Return;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       // If Audit representative is valid.
       If scAudRep <> ' ';
          ##rep = scAudRep;
          Chain keySMUPL pup01;
          If not %found(SMUPL1);
             msAudRep = 'WC31102';
             erAudRep = *on;
             isValid = *on;
          Endif;
       Endif;

      /end-free
     P ValidAud        E
       //===================================================================
       // ValidNew - Validate edit screen for New Business Tracking
       //===================================================================
     P ValidNew        B
     d ValidNew        PI
     c
      /free

       isValid  = *off;

       erNewRvwDt = *off;
       erNewPlMDt = *off;
       erNewPnRDt = *off;
       erNewPnCDt = *off;
       erNewPnMDt = *off;
       erNewPnPDt = *off;
       erRnwCrtDt = *off;
       erRnwQtADt = *off;
       erRnwAprDt = *off;

       intDate = 0;

       // Check if updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WMNBP':scNewTrRrn:'NBdt':'NBtime':
        'NBuser':NBdt:NBtime);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid = *on;
          Return;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       // If Underwriter Review Date is valid.
       If scNewRevDt > 0;
          intDate = DtCYMD(scNewRevDt);
          If intDate = 0;
             msNewRvwDt = 'WCR0500';
             erNewRvwDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Policy Mailed date is valid.
       intDate = 0;
       If scNewPlMDt > 0;
          intDate = DtCYMD(scNewPlMDt);
          If intDate = 0;
             msNewPlMDt = 'WCR0500';
             erNewPlMDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Panel Requested Date is valid.
       intDate = 0;
       If scNewPnRDt > 0;
          intDate = DtCYMD(scNewPnRDt);
          If intDate = 0;
             msNewPnRDt = 'WCR0500';
             erNewPnRDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Panel created date is valid.
       intDate = 0;
       If scNewPnCDt > 0;
          intDate = DtCYMD(scNewPnCDt);
          If intDate = 0;
             msNewPnCDt = 'WCR0500';
             erNewPnCDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Panel Mailed Date is valid.
       intDate = 0;
       If scNewPnMDt > 0;
          intDate = DtCYMD(scNewPnMDt);
          If intDate = 0;
             msNewPnMDt = 'WCR0500';
             erNewPnMDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Panel Posted date is valid.
       intDate = 0;
       If scNewPnPDt > 0;
          intDate = DtCYMD(scNewPnPDt);
          If intDate = 0;
             msNewPnPDt = 'WCR0500';
             erNewPnPDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Date Application Received is valid.
       intDate = 0;
       If scNewRecDt > 0;
          intDate = DtCYMD(scNewRecDt);
          If intDate = 0;
             msNewRecDt = 'WCR0500';
             erNewRecDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Entered Date is valid.
       intDate = 0;
       If scNewEntDt > 0;
          intDate = DtCYMD(scNewEntDt);
          If intDate = 0;
             msNewEntDt = 'WCR0500';
             erNewEntDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Agency Approved Date is valid.
       intDate = 0;
       If scNewAprDt > 0;
          intDate = DtCYMD(scNewAprDt);
          If intDate = 0;
             msNewAprDt = 'WCR0500';
             erNewAprDt = *on;
             isValid = *on;
          Endif;
       Endif;

      /end-free
     P ValidNew        E
       //===================================================================
       // ValidRnw - Validate edit screen for Renewal Business Tracking
       //===================================================================
     P ValidRnw        B
     d ValidRnw        PI
     c
      /free

       isValid  = *off;

       erRnwPnPDt = *off;
       erRnwRvwDt = *off;
       erRnwPlMDt = *off;
       erRnwPnCDt = *off;
       erRnwPnMDt = *off;
       erRnwCrtDt = *off;
       erRnwQtADt = *off;
       erRnwAprDt = *off;

       intDate = 0;

       // Check if updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WDRTP':scRnwTrRrn:'RTdt':'RTtime':
        'RTuser':RTdt:RTtime);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid = *on;
          Return;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       // If Underwriter Review Date is valid.
       If scRnwRvwDt > 0;
          intDate = DtCYMD(scRnwRvwDt);
          If intDate = 0;
             msRnwRvwDt = 'WCR0500';
             erRnwRvwDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Policy Mailed date is valid.
       intDate = 0;
       If scRnwPlMDt > 0;
          intDate = DtCYMD(scRnwPlMDt);
          If intDate = 0;
             msRnwPlMDt = 'WCR0500';
             erRnwPlMDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Panel created date is valid.
       intDate = 0;
       If scRnwPnCDt > 0;
          intDate = DtCYMD(scRnwPnCDt);
          If intDate = 0;
             msRnwPnCDt = 'WCR0500';
             erRnwPnCDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Panel Mailed Date is valid.
       intDate = 0;
       If scRnwPnMDt > 0;
          intDate = DtCYMD(scRnwPnMDt);
          If intDate = 0;
             msRnwPnMDt = 'WCR0500';
             erRnwPnMDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Panel Posted date is valid.
       intDate = 0;
       If scRnwPnPDt > 0;
          intDate = DtCYMD(scRnwPnPDt);
          If intDate = 0;
             msRnwPnPDt = 'WCR0500';
             erRnwPnPDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Policy Year Created date is valid.
       intDate = 0;
       If scRnwCrtDt > 0;
          intDate = DtCYMD(scRnwCrtDt);
          If intDate = 0;
             msRnwCrtDt = 'WCR0500';
             erRnwCrtDt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Quoted to Agency date is valid.
       intDate = 0;
       If scRnwQtADt > 0;
          intDate = DtCYMD(scRnwQtADt);
          If intDate = 0;
             msRnwQtADt = 'WCR0500';
             erRnwQtADt = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Agency Approval date is valid.
       intDate = 0;
       If scRnwAprDt > 0;
          intDate = DtCYMD(scRnwAprDt);
          If intDate = 0;
             msRnwAprDt = 'WCR0500';
             erRnwAprDt = *on;
             isValid = *on;
          Endif;
       Endif;

      /end-free
     P ValidRnw        E
       //===================================================================
       // ValidSts - Validate status
       //===================================================================
     P ValidSts        B
     D ValidSts        PI
      /free

       erNewSts = *off;
       isValid  = *off;

       // Check if updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WAQTP':scQteTrRrn:'QTdt':'QTtime':
        'QTuser':QTdt:QTtime);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid = *on;
          Return;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       Select;
       When Upper(scStsNew) ='T';
          If Upper(scCurrSts) <> 'W' and Upper(scCurrSts) <> 'P' and
           Upper(scCurrSts) <> 'Q' and Upper(scCurrSts) <> 'B' and
           Upper(scCurrSts) <> 'M' and Upper(scCurrSts) <> 'T';
             msNewSts = 'WC30141';
             erNewSts = *on;
             isValid  = *on;
          Endif;

       // Check for prior Cancellation/Reinstatement
       When Upper(scStsNew) ='C' or Upper(scStsNew) ='R';

          // Read all WAQTP records for this employer, if the last recorded entry
          // is 'C'/'R' (regardless of the agent number assigned to the WAQTP
          // record) then the current status is Cancelled/Reinstated.
          Clear intWkDt;
          Clear TempTyp;
          Unlock WAQTP;
          Setll KeyWAQTL pqt12;
          Reade KeyWAQTL pqt12;
          Dow not %eof(WAQTL12);
             If Upper(QTtype) = 'C' or Upper(QTtype) = 'R' or
              Upper(QTtype) = 'W' or Upper(QTtype) = 'T';
                If QTwkdt > intWkDt;
                   Clear intWkDt;
                   TempTyp = Upper(QTtype);
                   intWkDt = QTwkdt;
                Endif;
             Endif;
             Reade KeyWAQTL pqt12;
          Enddo;
          If (TempTyp = 'C' and Upper(scStsNew) = 'C') or
           (TempTyp = 'R' and Upper(scStsNew) = 'R');
             msNewSts = 'WC30086';
             erNewSts = *on;
             isValid  = *on;
          Endif;

          If isValid = *off and TempTyp <> 'C' and Upper(scStsNew) = 'R';
             msNewSts = 'WC30089';
             erNewSts = *on;
             isValid  = *on;
          Endif;
          Chain(n) scQteTrRrn pqt0p;

       Endsl;

      /end-free
     P ValidSts        E
       //===================================================================
       // ValidAudRpt - Validate audit report selection
       //===================================================================
     P ValidAudRpt     B
     D ValidAudRpt     PI
      /free

       erRptGrp  = *off;
       erRptFrDt = *off;
       erRptToDt = *off;
       isSelVld  = *off;

       // validation for group
       If scRptGrp = 0;
          erRptGrp = *on;
          isSelVld = *on;
       Endif;

       // Date validation for expiration Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If scRptFrmDt > 0;
          intFromDate = DtCYMD(scRptFrmDt);
          If intFromDate = 0;
             msRptFrDt = 'WCR0500';
             erRptFrDt = *on;
             isSelVld = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If scRptToDt > 0;
          intToDate = DtCYMD(scRptToDt);
          If intToDate = 0;
             msRptToDt = 'WCR0500';
             erRptToDt = *on;
             isSelVld = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isSelVld = *off and scRptFrmDt = 0 and scRptToDt > 0;
          msRptFrDt = 'WCR2246';
          erRptFrDt = *on;
          isSelVld = *on;
       Endif;

       // If only From date entered.
       If isSelVld = *off and scRptFrmDt > 0 and scRptToDt = 0;
          msRptToDt = 'WCR2247';
          erRptToDt = *on;
          isSelVld = *on;
       Endif;

       // If From date > To date
       If isSelVld = *off and scRptFrmDt > 0 and scRptToDt > 0
                          and scRptFrmDt > scRptToDt;
          msRptFrDt = 'WCR1502';
          msRptToDt = 'WCR1502';
          erRptFrDt = *on;
          erRptToDt = *on;
          isSelVld = *on;
       Endif;

      /end-free
     P ValidAudRpt     E
       //===================================================================
       // EdtQuote - Edit quote record
       //===================================================================
     P EdtQuote        B
     d EdtQuote        PI
     c
      /free

       Chain scQteTrRrn pqt0p;
       QTefdt  = DtCYMD(scEdtEffDt);
       QTwkdt  = DtCYMD(scEdtChgDt);
       QTordt  = DtCYMD(scEdtSubDt);
       QTnbdt  = DtCYMD(scEdtBusDt);
       QTfxdt  = DtCYMD(scEdtSntDt);
       QTcomm  = scEdtCmnt1;
       QTcom2  = scEdtCmnt2;
       QTcarr  = scEdtCrInf;
       QTchap  = scEdtChptr;
       QTprem  = scEdtPrem;
       QTuser  = q1user;
       QTpgm   = q1pgm;
       QTdt    = Dateto7(SystemDate());
       QTtime  = Timeto6(SystemTime());
       Update pqt0p;
       Unlock WAQTP;

      /end-free
     P EdtQuote        E
       //===================================================================
       // EdtAudit - Edit Audit record
       //===================================================================
     P EdtAudit        B
     d EdtAudit        PI
     d intPrml         s              9  0
     c
      /free
       clear intPrml;
       Chain scAudTrRrn pau0p;
       If AUaco <> scAudCmp or AUtypa <> scAudTyp;
          Setll keyWDELL8 pel08;
          Reade keyWDELL8 pel08;
          Dow not %eof(WDELL8);
             If ELfaf = 'A';
                intPrml = intPrml + ELaapr;
             Else;
                intPrml = intPrml + ELanpr;
             Endif;
             Reade keyWDELL8 pel08;
          Enddo;
          Chain keyWMEML# pem00;
          If EMpzip <> *zeros;
             intZip = EMpzip;
          Else;
             intZip = EMmzip;
          Endif;

          If ELfaf = 'A';
             intApr = ELaapr;
          Else;
             intApr = ELanpr;
          Endif;
          Clear pha0p;
          HAco#  = AUco#;
          HAfnd  = AUfnd;
          HAemp# = AUemp#;
          HAdiv  = AUdiv;
          HAfyr  = AUfyr;
          HApac  = AUaco;
          HAnac  = scAudCmp;
          HAtypa = scAudTyp;
          If EMpst <> *blanks;
             HAst = EMpst;
          Else;
             HAst = EMmst;
          Endif;

          HAzip  = intZip;
          HAcnty = EMcnty;
          HAtypb = ELtypb;
          HAprem = intPrml;
          HAstat = AUstat;
          HAuser = q1user;
          HApgm  = q1pgm;
          HAdt   = Dateto7(SystemDate());
          HAtime = Timeto6(SystemTime());
          HAiusr = q1user;
          HAipgm = q1pgm;
          HAidt  = Dateto7(SystemDate());
          HAitm  = Timeto6(SystemTime());
          write pha0p;
       Endif;

       AUaco  = scAudCmp;
       AUtypa = scAudTyp;
       AUrep  = scAudRep;
       AUuser = q1user;
       AUpgm  = q1pgm;
       AUdt   = Dateto7(SystemDate());
       AUtime = Timeto6(SystemTime());
       Update pau0p;
       Unlock WMAUP;

      /end-free
     P EdtAudit        E
       //===================================================================
       // ProcAudFlt - process filter for audit tracking
       //===================================================================
     P ProcAudFlt      B
     d ProcAudFlt      PI

      /free

       isVldFlt = *off;
       btnAudFltr = *off;
       LoadSelFlt();
       Dou (BtnAudAppf = *on and isVldFlt = *off) or BtnAudCan = *on;
          isVldFlt = *off;
      //**** Begin Add  ***  06/23/21  ****************************************
          outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
          ExFmt RcdAudFltr;
          Select;
          When BtnAudAppf = *on;
             ValidAudFlt();
             If isVldFlt = *off;
                SavFilter();
             Endif;

          When BtnAudCan = *on;
             RldFilter();

          Other;
             If ssEmp# > *zeros and ssFnd# <> ' ';
                dspssDiv = *off;
             Else;
                dspssDiv = *on;
             Endif;
             LoadSelFlt();

          Endsl;
       Enddo;

      /end-free
     P ProcAudFlt      E
       //===================================================================
       // ProcessEdt - process edit for Quote tracking
       //===================================================================
     P ProcessEdt      B
     d ProcessEdt      PI

     DedtMaster        ds                  LikeRec(pem00)

      /free

       isValid  = *off;

       // processing for Edit mode
       Chain(n) scQteTrRrn pqt0p;
       Clear edtMaster;
       Chain(n) (QTco#:QTfnd:QTemp#:QTdiv) pem00 edtMaster;
       Select;
       //***  Begin Chg  ***  12/18/18  ********************************
       //When edtMaster.EMfqlk = 'Y';
       //   Exsr PopRcdLockMsg;
       //When edtMaster.EMfqlk <> 'Y';
       When GetFQLockSts(QTco#:QTfnd:QTemp#:QTdiv:quoteMode:*on) = *on;
       Other;
       //***  End   Chg  ***  12/18/18  ********************************
          MoveDbSc();

          Dou BtnCancel = *on or (BtnSavEdt = *on and isValid = *off);
          isContribution = GetPrmContflg(##co:##fnd);
          If isContribution;
               flxPopHdg1 = 'Contribution';
          Else;
               flxPopHdg1 = 'Premium';
          Endif;
      //**** Begin Add  ***  06/23/21  ****************************************
             outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
             Exfmt rcdEdtQte;
             isValid = *off;
             Select;
             When BtnSavEdt = *on;
                Validate();
                If isValid = *off;
                   EdtQuote();
                Endif;
             When BtnCancel = *on;
                BtnCancel = *off;
                Leave;
             Endsl;
          Enddo;
       Endsl;
       unlock WAQTP;

       Begsr PopRcdLockMsg;

       outMessage = 'This quote is being processed within FastQuote and' +
        ' will open for your review in inquiry.';
       outTitle   = 'Information';
       outBtnText = 'Cancel';

       Callp PopOneBtn(outMessage:outTitle:outBtnText);

       Endsr;
      /end-free
     P ProcessEdt      E
       //===================================================================
       // ProcEdtAud - process edit for Audit tracking
       //===================================================================
     P ProcEdtAud      B
     d ProcEdtAud      PI
      /free

       isValid   = *off;
       prAudDate = *on;
       clear scAudCoSel;
       clear scAudTpSel;
       scAudCoSel = 'ACco# = ' + %char(ELco#);
       scAudTpSel = 'TAco# = ' + %char(ELco#);

       // processing for Edit mode
       Chain(n) scAudTrRrn pau0p;
       MvDbScAud();

       Dou BtnCanAudE = *on or (BtnSavEAud = *on and isValid = *off);
          scAudRepId = *off;
          scAudAtmId = *off;
          scAudRcvId = *off;
          scAudMalId = *off;
          scAudCntId = *off;
          temprrn = rrnaudtrck;
      //**** Begin Add  ***  06/23/21  ****************************************
          outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
          Exfmt rcdEdtAud;
          isValid = *off;
          Select;
          When BtnSavEAud = *on;
             ValidAud();
             If isValid = *off;
                EdtAudit();
             Endif;
          When scAudRepId = *on;
             ProcRep();
          When scAudRcvId = *on;
             ProcRcv();
          When scAudAtmId = *on;
             ProcAtm();
          When scAudMalId = *on;
             ProcMal();
          When scAudCntId = *on;
             ProcCnt();
          When BtnCanAudE = *on;
             BtnCanAudE = *off;
             Leave;
          Endsl;
       Enddo;

       unlock WMAUP;
      /end-free
     P ProcEdtAud      E
       //===================================================================
       // ProcEdtNew - process edit for New Business tracking
       //===================================================================
     P ProcEdtNew      B
     d ProcEdtNew      PI
      /free

       isValid   = *off;

       // processing for Edit mode
       Chain(n) scNewTrRrn pnb0p;
       MvDbScNew();

       Dou BtnCanNew = *on or (BtnSavNew = *on and isValid = *off);
      //**** Begin Add  ***  06/23/21  ****************************************
          outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
          Exfmt rcdEdtNew;
          isValid = *off;
          Select;
          When BtnSavNew = *on;
             ValidNew();
             If isValid = *off;
                SaveNew();
             Endif;
          When BtnCanNew = *on;
             BtnCanNew = *off;
             Leave;
          Endsl;
       Enddo;

       unlock WMNBP;
      /end-free
     P ProcEdtNew      E
       //===================================================================
       // ProcEdtRnw - process edit for Renewal Business tracking
       //===================================================================
     P ProcEdtRnw      B
     d ProcEdtRnw      PI
      /free

       isValid   = *off;

       // processing for Edit mode
       Chain(n) scRnwTrRrn prt0p;
       MvDbScRnw();

       Dou BtnCanRnw = *on or (BtnSavRnw = *on and isValid = *off);
      //**** Begin Add  ***  06/23/21  ****************************************
          outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
          Exfmt rcdEdtRnw;
          isValid = *off;
          Select;
          When BtnSavRnw = *on;
             ValidRnw();
             If isValid = *off;
                SaveRnw();
             Endif;
          When BtnCanRnw = *on;
             BtnCanRnw = *off;
             Leave;
          Endsl;
       Enddo;

       unlock WDRTP;
      /end-free
     P ProcEdtRnw      E
       //===================================================================
       // ProcRep - process Reassign representative
       //===================================================================
     P ProcRep         B
     d ProcRep         PI
      /free

       Chain(n) scAudTrRrn pau0p;
       // processing for reassign audit representative
       If scAudRep <> ' ';
          ##rep = scAudRep;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             scRepOld = scAudRep + ' - ' +  UPusnm;
          Endif;
       Endif;
       BtnCanRep = *off;
       BtnSavRep = *off;
       clear scRepNew;

       Dou BtnCanRep = *on or BtnSavRep = *on;
          Exfmt rcdRepAud;
          Select;
          When BtnSavRep = *on;
             If scAudRep = scRepNew;
                msNewRep = 'WC31101';
                erNewRep = *on;
                BtnSavRep = *off;
             Else;
                ProcCommon();
                Chain scAudTrRrn pau0p;
                scAudRep  = scRepNew;
                AUrep  = scRepNew;
                AUrsgn = 'Y';
                AUuser = q1user;
                AUpgm  = q1pgm;
                AUdt   = Dateto7(SystemDate());
                AUtime = Timeto6(SystemTime());
                Update pau0p;
                If AUrep = ' ';
                   prAudRep  = *off;
                   dspAudRep = *off;
                Else;
                   prAudRep  = *on;
                   dspAudRep = *on;
                Endif;
             Endif;
          When BtnCanRep = *on;
             BtnCanRep = *off;
             Leave;
          Endsl;
       Enddo;
       unlock WMAUP;

      /end-free
     P ProcRep         E
       //===================================================================
       // ProcRcv - process Receiving date change
       //===================================================================
     P ProcRcv         B
     d ProcRcv         PI
      /free

       Chain(n) scAudTrRrn pau0p;
       // processing for audit receiving date
       scAudDate = scAudRcvDt;
       scChgDtDs = 'Current Audit Received Date';
       scNewDtDs = 'New Audit Received Date';
       clear scNewDate;
       clear scChgDtLbl;
       scChgDtLbl = 'Audit Received Date';
       If scAudDate = 0;
          scNewDate = DtMDYY(Dateto7(SystemDate()));
       Endif;
       BtnCanDt  = *off;
       BtnSavDt  = *off;

       Dou BtnCanDt = *on or (BtnSavDt = *on and erNewDate = *off);
          Exfmt rcdDateChg;
          erNewDate = *off;
          Select;
          When BtnSavDt = *on;
             // If Audit received date is valid.
             intDate  = 0;
             intDate1 = 0;
             intDate1 = DtCYMD(AUrqdt);
             If scNewDate > 0;
                intDate = DtCYMD(scNewDate);
                If intDate = 0;
                   msNewDate = 'WCR0500';
                   erNewDate = *on;
                   BtnSavDt = *off;
                Endif;
             Else;
                intDate = 0;
             Endif;

             If erNewDate = *off and intDate < intDate1 and intDate <> 0;
                msNewDate = 'WC31103';
                erNewDate = *on;
                BtnSavDt = *off;
             Endif;
             If erNewDate = *off;
                ProcCommon();
                Chain scAudTrRrn pau0p;
                scAudRcvDt = scNewDate;
                AUrcvd = DtCYMD(scNewDate);
                AUuser = q1user;
                AUpgm  = q1pgm;
                AUdt   = Dateto7(SystemDate());
                AUtime = Timeto6(SystemTime());
                Update pau0p;
             Endif;
          When BtnCanDt = *on;
             BtnCanDt = *off;
             Leave;
          Endsl;
       Enddo;

       unlock WMAUP;

      /end-free
     P ProcRcv         E
       //===================================================================
       // ProcAtm - process Attempt date change
       //===================================================================
     P ProcAtm         B
     d ProcAtm         PI
      /free

       Chain(n) scAudTrRrn pau0p;
       // processing for audit attempt date
       scAudDate = scAudAtmDt;
       scChgDtDs  = 'Current Audit Attempt Date';
       scNewDtDs  = 'New Audit Attempt Date';
       clear scNewDate;
       clear scChgDtLbl;
       scChgDtLbl = 'Audit Attempt';
       If scAudDate = 0;
          scNewDate = DtMDYY(Dateto7(SystemDate()));
       Endif;
       BtnCanDt  = *off;
       BtnSavDt  = *off;

       Dou BtnCanDt = *on or BtnSavDt = *on;
          Exfmt rcdDateChg;
          erNewDate = *off;
          Select;
          When BtnSavDt = *on;
             intDate = 0;
             If scNewDate > 0;
                intDate = DtCYMD(scNewDate);
                If intDate = 0;
                   msNewDate = 'WCR0500';
                   erNewDate = *on;
                   BtnSavDt  = *off;
                Endif;
             Else;
                intDate = 0;
             Endif;

             If erNewDate = *off;
                ProcCommon();
                Chain scAudTrRrn pau0p;
                scAudAtmDt = scNewDate;
                AUattm = AUattm + 1;
                AUatdt = DtCYMD(scNewDate);
                AUuser = q1user;
                AUpgm  = q1pgm;
                AUdt   = Dateto7(SystemDate());
                AUtime = Timeto6(SystemTime());
                Update pau0p;
             Endif;
          When BtnCanDt = *on;
             BtnCanDt = *off;
             Leave;
          Endsl;
       Enddo;
       unlock WMAUP;

      /end-free
     P ProcAtm         E
       //===================================================================
       // ProcMal - process Mailed date change
       //===================================================================
     P ProcMal         B
     d ProcMal         PI
      /free

       Chain(n) scAudTrRrn pau0p;
       If AUprdt = *zeros;
          Exsr PopNotValidMsg;
       Else;
          // processing for audit attempt date
          scAudDate = scAudMalDt;
          scChgDtDs  = 'Current Audit Mail Date';
          scNewDtDs  = 'New Audit Mail Date';
          clear scNewDate;
          clear scChgDtLbl;
          scChgDtLbl = 'Audit Mail Date';
          If scAudDate = 0;
             scNewDate = DtMDYY(Dateto7(SystemDate()));
          Endif;
          BtnCanDt  = *off;
          BtnSavDt  = *off;

          Dou BtnCanDt = *on or BtnSavDt = *on;
             Exfmt rcdDateChg;
             erNewDate = *off;
             Select;
             When BtnSavDt = *on;
                intDate = 0;
                If scNewDate > 0;
                   intDate = DtCYMD(scNewDate);
                   If intDate = 0;
                      msNewDate = 'WCR0500';
                      erNewDate = *on;
                      BtnSavDt  = *off;
                   Endif;
                Else;
                   intDate = 0;
                Endif;

                If erNewDate = *off;
                   ProcCommon();
                   Chain scAudTrRrn pau0p;
                   scAudMalDt = scNewDate;
                   If scAudRcvDt = 0;
                      scAudRcvDt = scAudFnlDt;
                   Endif;
                   AUmail = DtCYMD(scNewDate);
                   AUuser = q1user;
                   AUpgm  = q1pgm;
                   AUdt   = Dateto7(SystemDate());
                   AUtime = Timeto6(SystemTime());
                   Update pau0p;
                Endif;
             When BtnCanDt = *on;
                BtnCanDt = *off;
                Leave;
             Endsl;
          Enddo;
       Endif;
       unlock WMAUP;

       Begsr PopNotValidMsg;

       outMessage = 'Audit Mailed can not be processed when ''Audit Print' +
        ' Date'' is blank. Run an audit bill before choosing this option' +
        ' again.';
       outTitle   = 'Information';
       outBtnText = 'Ok';

       Callp PopOneBtn(outMessage:outTitle:outBtnText);

       Endsr;

      /end-free
     P ProcMal         E
       //===================================================================
       // ProcCnt - process Contested date change
       //===================================================================
     P ProcCnt         B
     d ProcCnt         PI
      /free

       Chain(n) scAudTrRrn pau0p;
       // processing for audit attempt date
       scAudDate = scAudCntDt;
       scChgDtDs  = 'Current Audit Contested Date';
       scNewDtDs  = 'New Audit Contested Date';
       clear scNewDate;
       clear scChgDtLbl;
       scChgDtLbl = 'Audit Contested Date';
       If scAudDate = 0;
          scNewDate = DtMDYY(Dateto7(SystemDate()));
       Endif;
       BtnCanDt  = *off;
       BtnSavDt  = *off;
       If AUcnfl <> 'Y' and AUcntd = *zeros;
          cntFlag = 'N';
       Else;
          cntFlag = 'Y';
       Endif;
       If AUcnfl = 'Y';
          ProcCommon();
          Chain scAudTrRrn pau0p;
          AUcnfl = 'N';
          AUcntd = *zeros;
          AUuser = q1user;
          AUpgm  = q1pgm;
          AUdt   = Dateto7(SystemDate());
          AUtime = Timeto6(SystemTime());
          scAudCntDt = *zeros;
          Update pau0p;

          Chain keyWDELL1# pel00;
          If %found(WDELL);
             //*** Beg Add *** 10/25/23 *** WC2922 *********************
             // Call ResendInvoices when the division is flagging as contested
             // audit and the division is either not summarized or is
             // summarized and it's the state reporting division
             Select;
             When ELARSM = ' ';
                ResendInvoices(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:'A');
             When ELARSM = 'Y';
                // Get The state reporting division
                stRptDiv = GetStRptDv(ELco#:ELfnd:ELemp#);
                If stRptDiv = ELdiv;
                   ResendInvoices(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:'A');
                EndIf;
             EndSl;
             //*** End Add *** 10/25/23 *** WC2922 *********************
             ELctst = ' ';
             ELuser = q1user;
             ELpgm  = q1pgm;
             ELdt   = Dateto7(SystemDate());
             ELtime = Timeto6(SystemTime());
             Update pel00;
          Endif;
          Unlock WDELL;

       Else;
          Dou BtnCanDt = *on or BtnSavDt = *on;
             Exfmt rcdDateChg;
             erNewDate = *off;
             Select;
             When BtnSavDt = *on;
                intDate = 0;
                If scNewDate > 0;
                   intDate = DtCYMD(scNewDate);
                   If intDate = 0;
                      msNewDate = 'WCR0500';
                      erNewDate = *on;
                      BtnSavDt  = *off;
                   Endif;
                Else;
                   intDate = 0;
                Endif;

                If erNewDate = *off;
                   ProcCommon();
                   Chain scAudTrRrn pau0p;
                   scAudCntDt = scNewDate;
                   AUcntd = DtCYMD(scNewDate);
                   cntFlag = 'Y';
                   AUcnfl = 'Y';
                   AUuser = q1user;
                   AUpgm  = q1pgm;
                   AUdt   = Dateto7(SystemDate());
                   AUtime = Timeto6(SystemTime());
                   Update pau0p;

                   Chain keyWDELL1# pel00;
                   If %found(WDELL);
                      //*** Beg Add *** 10/25/23 *** WC2922 ***************
                      // Call RemoveInvoices when the division is flagging as contested
                      // audit and the division is either not summarized or is
                      // summarized and it?s the state reporting division
                      Select;
                      When ELARSM = ' ';
                         RemoveInvoices(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:'A');
                      When ELARSM = 'Y';
                         // Get The state reporting division
                         stRptDiv = GetStRptDv(scCo#:scFnd:scEmp#);
                         If stRptDiv = ELdiv;
                            RemoveInvoices(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:'A');
                         EndIf;
                      EndSl;
                      //*** End Add *** 10/25/23 *** WC2922 ***************
                      ELctst = 'Y';
                      ELuser = q1user;
                      ELpgm  = q1pgm;
                      ELdt   = Dateto7(SystemDate());
                      ELtime = Timeto6(SystemTime());
                      Update pel00;
                   Endif;
                   Unlock WDELL;
                Endif;
             When BtnCanDt = *on;
                BtnCanDt = *off;
                Leave;
             Endsl;
          Enddo;
       Endif;
       If AUcnfl = 'Y';
          scAudCntLb = 'Clear Contested Flag';
       Else;
          scAudCntLb = 'Contested Date';
       Endif;

       unlock WMAUP;
      /end-free
     P ProcCnt         E
       //===================================================================
       // ProcCommon - process common option functionality
       //===================================================================
     P ProcCommon      B
     d ProcCommon      PI
     d isProc          s               n
      /free

       isProc = *off;

       Chain keyWMAXL pax00;
       If %found(WMAXL);
          Clear rmkCd;
          Clear strDate;
          Clear strOrnr;
          Select;
          When scAudRepId = *on;
             If AXrear <> *blanks;
                rmkCd =  AXrear;
                isProc = *on;
                strDate = %editc(DtMDYY(Dateto7(SystemDate())):'X');
                strOrnr = 'Old Rep: ' + scAudRep + ', New Rep: ' + scRepNew;
             Endif;
          When scAudRcvId = *on;
             If AXrccd <> *blanks and scAudDate <> scNewDate;
                rmkCd =  AXrccd;
                strDate = %editc(scNewDate:'X');
                isProc = *on;
             Endif;
          When scAudAtmId = *on;
             If AXaacd <> *blanks and scAudDate <> scNewDate;
                rmkCd =  AXaacd;
                strDate = %editc(scNewDate:'X');
                isProc = *on;
             Endif;
          When scAudMalId = *on;
             If AXfmcd <> *blanks and scAudDate <> scNewDate;
                rmkCd =  AXfmcd;
                strDate = %editc(scNewDate:'X');
                isProc = *on;
             Endif;
          When scAudCntId = *on;
             If AUcnfl <> 'Y';
                If AXaccd <> *blanks and scAudDate <> scNewDate;
                   rmkCd =  AXaccd;
                   strDate = %editc(scNewDate:'X');
                   isProc = *on;
                Endif;
             Else;
                If AXcccd <> *blanks;
                   rmkCd =  AXcccd;
                   strDate = %editc(DtMDYY(Dateto7(SystemDate())):'X');
                   isProc = *on;
                Endif;
             Endif;
          Endsl;
       Endif;

       If isProc = *on;
          ProcDarts();
       Endif;

       Chain keyWDELL1# pel00;
       If ELctst <> cntFlag;
          If cntFlag = 'Y';
             ELctst = 'Y';
          Else;
             ELctst = ' ';
          Endif;
          ELuser = q1user;
          ELpgm  = q1pgm;
          ELdt   = Dateto7(SystemDate());
          ELtime = Timeto6(SystemTime());
          Update pel00;
       Endif;
       Unlock WDELL;

      /end-free
     P ProcCommon      E
       //===================================================================
       // ProcView - process View for Audit Report
       //===================================================================
     P ProcView        B
     d ProcView        PI
      /free

       clear ##kc#;
       clear rcdSelAud;
       BtnRptCan = *off;
       BtnRptApp = *off;
       ##co# = %editc(##co:'X');
       scRptDspS = *off;
       userGroup# = GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
       Dou BtnRptCan = *on or BtnRptApp = *on;
          Exfmt rcdSelAud;

          Select;
          When BtnRptCan = *on;
             BtnRptCan = *off;
             leave;

          Other;
             ##fnd# = %editc(scRptGrp:'X');
             scRptUrl = CreateURL('W4700R21':'Audit Report':##kc#:##co#:##fnd#);

          Endsl;
       Enddo;

      /end-free
     P ProcView        E
       //===================================================================
       // ProcPrint - process Print for Audit Report
       //===================================================================
     P ProcPrint       B
     d ProcPrint       PI
      /free

       clear rcdSelAud;
       BtnRptCan = *off;
       BtnRptApp = *off;
       clear scRptUrl;
       ##co# = %editc(##co:'X');
       scRptDspS = *on;
       userGroup# = GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
       Dou BtnRptCan = *on or (BtnRptApp = *on and isSelVld = *off);
          Exfmt rcdSelAud;
          isSelVld = *off;

          Select;
          When BtnRptApp = *on;
             ValidAudRpt();
             If isSelVld = *off;
                ##fnd# = %editc(scRptGrp:'X');
                ##mode# = 'D';
                ##date# = %editc(DateTo7(SystemDate):'X');
                ##date1# = %editc(DtCYMD(scRptFrmDt):'X');
                ##date2# = %editc(DtCYMD(scRptToDt):'X');
                PrtAudRpt(##co#:##fnd#:##date#:##date1#:##date2#:##que#:
                 ##mode#);
                SubWdw();
             Endif;

          When BtnRptCan = *on;
             BtnRptCan = *off;
             leave;

          Other;
             ##fnd# = %editc(scRptGrp:'X');

          Endsl;
       Enddo;


      /end-free
     P ProcPrint       E
       //===================================================================
       // PrcDelDrt - process DARTS for Deletion
       //===================================================================
     P PrcDelDrt       B
     d PrcDelDrt       PI
      /free

       RmkCd = EXaudl;
       // User Name
       Chain q1user pup00;
       ##comm = UPusnm + ' deleted Audit Trk record.';

       // Audit company name
       If AUaco <> ' ';
          ##aco = AUaco;
          Chain keyWTACL pac00;
          If %found(WTACL) and ACname <> *blanks;
             ##comm = %trim(##comm) + '  Audit Company: ' + ACname;
          Endif;
       Endif;

       // Audit Representative Name
       If AUrep <> ' ';
          ##rep =  AUrep;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             ##comm = %trim(##comm) + '  Audit Rep: ' + UPusnm;
          Endif;
       Endif;

       // Audit Request Date
       If AUrqdt <> *zero;
          ##comm = %trim(##comm) + '  Audit Req Date: ' +
           %subst(%editc(DtMDYY(AUrqdt):'X'):1:2) + '/' +
           %subst(%editc(DtMDYY(AUrqdt):'X'):3:2) + '/' +
           %subst(%editc(DtMDYY(AUrqdt):'X'):5:4);
       Endif;
       ##com2 = *blanks;
       ProcDarts();

      /end-free
     P PrcDelDrt       E
       //===================================================================
       // PrcHstDtl - process for history link
       //===================================================================
     P PrcHstDtl       B
     d PrcHstDtl       PI
      /free

       rrnHstAsn = 0;
       clrHstAsn = *On;
       dspHstAsn = *Off;
       Write rcdHstAud;
       clrHstAsn = *Off;
       dspHstAsn = *On;
       BtnHstCan = *Off;

       // load grid
       Chain(n) scAudTrRrn pau0p;
       Setll keyWMHAL pha00;
       Reade keyWMHAL pha00;
       Dow not %eof(WMHAL);
          clear sflHstAud;

          ##aco = HApac;
          Chain keyWTACL pac00;
          If %found(WTACL) and ACname <> *blanks;
             scHstPrCmp = %trim(HApac) + '-' + ACname;
          Else;
             scHstPrCmp = HApac;
          Endif;

          clear ##aco;
          ##aco = HAnac;
          Chain keyWTACL pac00;
          If %found(WTACL) and ACname <> *blanks;
             scHstNwCmp = %trim(HAnac) + '-' + ACname;
          Else;
             scHstNwCmp = HAnac;
          Endif;

          Chain keyWTTAL pta00;
          If %found(WTTAL) and TAtypd <> *blanks;
             scHstAudTy = %trim(HAtypa) + '-' + TAtypd;
          Else;
             scHstAudTy = HAtypa;
          Endif;

          clear ##key;
          If HAcnty <> ' ';
             scHstCnt = HAcnty;
          Endif;
          If HAst <> ' ' and HAcnty <> ' ';
             ##key = HAst + HAcnty;
             Chain keyWTCNL1 pcn01;
             If %found(WTCNL1) and CNdesc <> *blanks;
                clear scHstCnt;
                scHstCnt = %trim(HAcnty) + '-' + %trim(CNdesc);
             Endif;
          Endif;

          scHstZip   = HAzip;
          scHstPrm   = HAprem;
          scHstNwDt  = CvtToDate(HAdt);
          read pha00;
          If scHstPrCmp <> ' ';
             scHstOrgDt = CvtToDate(HAdt);
          Else;
             clear scHstOrgDt;
          Endif;
          readp pha00;

          rrnHstAsn = rrnHstAsn + 1;
          write sflHstAud;
          Reade keyWMHAL pha00;
       Enddo;
       isContribution = GetPrmContflg(##co:##fnd);
       If isContribution;
          flexHead2 = 'Prior Audit Company,New Audit Company,Type of Audit,+
                       County,Zip Code,Contribution,Assignment New Date,+
                       Assignment Original Date';
       Else;
          flexHead2 = 'Prior Audit Company,New Audit Company,Type of Audit,+
                       County,Zip Code,Premium,Assignment New Date,+
                       Assignment Original Date';
       Endif;
       rrnHstAsn = 1;


       Dow BtnHstCan = *off;
          temprrn = rrnaudtrck;
      //**** Begin Add  ***  06/23/21  ****************************************
          outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
          Exfmt rcdHstAud;
       Enddo;

      /end-free
     P PrcHstDtl       E
       //===================================================================
       // ProcDarts - process DARTS
       //===================================================================
     P ProcDarts       B
     d ProcDarts       PI
     d outDate         s             10

     c     keySTCTL      klist
     c                   kfld                    RmkCd

      /free
       // Call API to write a DARTS Remark
       clear ##key#;
       clear ##cdat;
       clear ##dcid;

       ##key# = %editc(AUco#:'X') + %editc(AUfnd:'X') + %editc(AUemp#:'X') +
        %editc(AUdiv:'X');
       If scDelAud  = *off;
          clear ##Comm;
          clear ##com2;
          clear outDate;
          outDate = %subst(strDate:1:2) + '/' + %subst(strDate:3:2) + '/' +
           %subst(strDate:5:4);

          Chain keySTCTL pct01;
          ##Comm = %trim(CTdesc) + ' YEAR: ' + %subst(%editc(AUfyr:'X'):2:2) +
           ' ' + outDate + ' ' + strOrnr;
       Endif;
       ##cdat = %editc(DtMDYY(Dateto7(SystemDate())):'X');
       ##pnm = 'W0050R';
       ##secl = '000';
       ##user = q1user;
       ##pgm  = q1pgm;
       ##fold = *blanks;
       ##difl = *blanks;
       ##ipgm = q1pgm;
       ##fl01 = *blank;
       ##fl02 = *blank;
       ##rmcd = rmkCd;
       ##seq# = '00000';
       ##sbyl = 'N';
       ##apid = *blanks;
       ##idoc = *blanks;
       ##ique = *blanks;
       ##imgf = *blank;
       ##ipth = *blanks;
       ##pidx = '00000';
       ##qpty = *blank;
       ##dctp = *blanks;
       ##csid = *blanks;
       ##code#= *blanks;
       If ##rmcd <> *blanks;
          WrtDart(##key#:##comm:##cdat:##dcid:##pnm:##secl:##user:##pgm:##fold:
           ##difl:##ipgm:##fl01:##fl02:##rmcd:##seq#:##sbyl:##com2:##apid:
           ##idoc:##ique:##imgf:##ipth:##pidx:##qpty:##dctp:##csid:##code#);

         clear ##cseq;
         If ##seq# <> ' ' or ##seq# <> '00000';
            ##cseq = %dec(##seq#:5:0);
         Endif;

         ##cpnam = ##pnm;
         WrtNotes(##key#:##cseq:##cpnam);
       Endif;

      /end-free
     P ProcDarts       E
       //===================================================================
       // SavFilter - Save audit tracking filter selection
       //===================================================================
     P SavFilter       B
     d SavFilter       PI
     c
      /free

       dsAFilter.ssAudSts   = ssAudSts;
       dsAFilter.ssEmp#     = ssEmp#;
       dsAFilter.ssFnd#     = ssFnd#;
       dsAFilter.ssDiv#     = ssDiv#;
       dsAFilter.ssFyr      = ssFyr;
       dsAFilter.ssAudCo    = ssAudCo;
       dsAFilter.ssAudRep   = ssAudRep;
       dsAFilter.ssCntFlg   = ssCntFlg;
       dsAFilter.ssOutSFlg  = ssOutSFlg;
       dsAFilter.ssAudCan   = ssAudCan;
       dsAFilter.ssAudUnw   = ssAudUnw;
       dsAFilter.ssAudRvw   = ssAudRvw;
       dsAFilter.ssAudMngr  = ssAudMngr;
       dsAFilter.ssAudPlnTy = ssAudPlnTy;
       dsAFilter.ssReqFrDt  = ssReqFrDt;
       dsAFilter.ssRcvFrDt  = ssRcvFrDt;
       dsAFilter.ssAtmpFrDt = ssAtmpFrDt;
       dsAFilter.ssExpFrDt  = ssExpFrDt;
       dsAFilter.ssPrtFrDt  = ssPrtFrDt;
       dsAFilter.ssCalFrDt  = ssCalFrDt;
       dsAFilter.ssFnlFrDt  = ssFnlFrDt;
       dsAFilter.ssMldFrDt  = ssMldFrDt;
       dsAFilter.ssReqToDt  = ssReqToDt;
       dsAFilter.ssRcvToDt  = ssRcvToDt;
       dsAFilter.ssAtmpToDt = ssAtmpToDt;
       dsAFilter.ssExpToDt  = ssExpToDt;
       dsAFilter.ssPrtToDt  = ssPrtToDt;
       dsAFilter.ssCalToDt  = ssCalToDt;
       dsAFilter.ssFnlToDt  = ssFnlToDt;
       dsAFilter.ssMldToDt  = ssMldToDt;
       dsAFilter.ssAudStsF  = ssAudStsF;
       dsAFilter.ssAudTyp   = ssAudTyp;

      /end-free
     P SavFilter       E
       //===================================================================
       // RldFilter - Reload audit tracking filter selection
       //===================================================================
     P RldFilter       B
     d RldFilter       PI
     c
      /free

       ssAudSts   = dsAFilter.ssAudSts;
       ssFnd#     = dsAFilter.ssFnd#;
       ssDiv#     = dsAFilter.ssDiv#;
       ssFyr      = dsAFilter.ssFyr;
       ssAudCo    = dsAFilter.ssAudCo;
       ssAudRep   = dsAFilter.ssAudRep;
       ssCntFlg   = dsAFilter.ssCntFlg;
       ssOutSFlg  = dsAFilter.ssOutSFlg;
       ssAudCan   = dsAFilter.ssAudCan;
       ssAudUnw   = dsAFilter.ssAudUnw;
       ssAudRvw   = dsAFilter.ssAudRvw;
       ssAudMngr  = dsAFilter.ssAudMngr;
       ssAudPlnTy = dsAFilter.ssAudPlnTy;
       ssReqFrDt  = dsAFilter.ssReqFrDt;
       ssRcvFrDt  = dsAFilter.ssRcvFrDt;
       ssAtmpFrDt = dsAFilter.ssAtmpFrDt;
       ssExpFrDt  = dsAFilter.ssExpFrDt;
       ssPrtFrDt  = dsAFilter.ssPrtFrDt;
       ssCalFrDt  = dsAFilter.ssCalFrDt;
       ssFnlFrDt  = dsAFilter.ssFnlFrDt;
       ssMldFrDt  = dsAFilter.ssMldFrDt;
       ssReqToDt  = dsAFilter.ssReqToDt;
       ssRcvToDt  = dsAFilter.ssRcvToDt;
       ssAtmpToDt = dsAFilter.ssAtmpToDt;
       ssExpToDt  = dsAFilter.ssExpToDt;
       ssPrtToDt  = dsAFilter.ssPrtToDt;
       ssCalToDt  = dsAFilter.ssCalToDt;
       ssFnlToDt  = dsAFilter.ssFnlToDt;
       ssMldToDt  = dsAFilter.ssMldToDt;
       ssAudStsF  = dsAFilter.ssAudStsF;
       ssAudTyp   = dsAFilter.ssAudTyp;

      /end-free
     P RldFilter       E
       //===================================================================
       // ProcessSts - process status change
       //===================================================================
     P ProcessSts      B
     d ProcessSts      PI

     DstsMaster        ds                  LikeRec(pem00)
     DstsDetail        ds                  LikeRec(pel00)

      /free
       Chain(n) scQteTrRrn pqt0p;
       Clear stsMaster;
       Chain(n) (QTco#:QTfnd:QTemp#:QTdiv) pem00 stsMaster;
       Select;
       When GetFQLockSts(QTco#:QTfnd:QTemp#:QTdiv:quoteMode:*on) = *on;
       Other;
          Chain(n) scQteTrRrn pqt0p;
          Clear stsDetail;
          Setgt     (##co:##fnd:##emp:##div:##fyr) pel00;
          Readpe(n) (##co:##fnd:##emp:##div)       pel00 stsDetail;
          scChoice =
           SetScChoice(stsDetail.ELco#:stsDetail.ELnwRw:scQteLiv:*on);
          Chain keyWTQSL pqs01;
          If %found(WTQSL1);
             scCurrSts = %trim(QSdesc);
          Endif;
          scCurrSts = scQteSts;
          scStsNew = QTtype;
          isValid = *off;

          Dou BtnCanSts = *on or (BtnConSts = *on and isValid = *off);
      //**** Begin Add  ***  06/23/21  ****************************************
             outElRrn=xxRrn;
      //**** End Add  ***  06/23/21  ****************************************
             Exfmt rcdSelSts;
             isValid = *off;
             Select;
             When BtnConSts = *on;
                ValidSts();
                If isValid = *off;
                   ChgStatus();
                   //***  Begin Add  ***  06/22/20  ***************
                   UpdFQLockSts(##co:##fnd:##emp:##div:scStsNew);
                   //***  End   Add  ***  06/22/20  ***************
                Endif;
             When BtnCanSts = *on;
                BtnCanSts = *off;
                Leave;
             Endsl;
          Enddo;
       Endsl;

       Begsr PopRcdLockMsg;

       outMessage = 'This quote is being processed within FastQuote and' +
        ' will open for your review in inquiry.';
       outTitle   = 'Information';
       outBtnText = 'Cancel';

       Callp PopOneBtn(outMessage:outTitle:outBtnText);

       Endsr;

      /end-free
     P ProcessSts      E
       //===================================================================
       // MoveDbSc - Move database record to screen for quote tracking
       //===================================================================
     P MoveDbSc        B
     d MoveDbSc        PI
     c
      /free

       Chain keyWTQSL pqs01;
       If %found(WTQSL1);
          scEdtTyp = %trim(QSdesc);
       Endif;

       scEdtEffDt = DtMDYY(QTefdt);
       scEdtChgDt = DtMDYY(QTwkdt);
       scEdtSubDt = DtMDYY(QTordt);
       scEdtBusDt = DtMDYY(QTnbdt);
       If scEdtBusDt = 0;
          protNewBus = *off;
       Else;
          protNewBus = *on;
       Endif;
       scEdtSntDt = DtMDYY(QTfxdt);
       scEdtCmnt1 = QTcomm;
       scEdtCmnt2 = QTcom2;
       scEdtCrInf = QTcarr;
       scEdtChptr = QTchap;
       If QTprem > 0;
          dspEdtPrem = *on;
       Else;
          dspEdtPrem = *off;
       Endif;
       scEdtPrem  = QTprem;

      /end-free
     P MoveDbSc        E
       //===================================================================
       // MvDbScAud - Move database record to screen for audit tracking
       //===================================================================
     P MvDbScAud       B
     d MvDbScAud       PI
     c
      /free

       If AUstat = 'A';
          scAudStsDs = 'Active';
       Else;
          scAudStsDs = 'Inactive';
       Endif;
       scAudCmp   = AUaco;
       If AUrep = ' ';
          prAudRep  = *off;
          dspAudRep = *off;
       Else;
          prAudRep  = *on;
          dspAudRep = *on;
       Endif;
       scAudRep   = AUrep;
       scAudTyp   = AUtypa;
       scAudReqDt = DtMDYY(AUrqdt);
       scAudRcvDt = DtMDYY(AUrcvd);
       scAudAtmDt = DtMDYY(AUatdt);
       scAudCalDt = DtMDYY(AUacdt);
       scAudPrtDt = DtMDYY(AUapdt);
       scAudFnlDt = DtMDYY(AUprdt);
       scAudMalDt = DtMDYY(AUmail);
       scAudCntDt = DtMDYY(AUcntd);

       If AUcnfl = 'Y';
          scAudCntLb = 'Clear Contested Flag';
       Else;
          scAudCntLb = 'Contested Date';
       Endif;

      /end-free
     P MvDbScAud       E
       //=====================================================================
       // MvDbScNew - Move database record to screen for New Business Tracking
       //=====================================================================
     P MvDbScNew       B
     d MvDbScNew       PI
     c
      /free

       scNewRecDt = DtMDYY(NBrcdt);
       scNewEntDt = DtMDYY(NBendt);
       scNewRevDt = DtMDYY(NBiwdt);
       scNewQutDt = DtMDYY(NBqtdt);
       scNewDclDt = DtMDYY(NBdcdt);
       scNewLstDt = DtMDYY(NBlsdt);
       scNewAprDt = DtMDYY(NBaadt);
       scNewGenDt = DtMDYY(NBpgdt);
       scNewPlMDt = DtMDYY(NBpold);
       scNewPnRDt = DtMDYY(NBprdt);
       scNewPnCDt = DtMDYY(NBpcdt);
       scNewPnMDt = DtMDYY(NBpand);
       scNewPnPDt = DtMDYY(NBppdt);

       If scNewRecDt > 0;
          dsNewRecDt = *on;
       Else;
          dsNewRecDt = *off;
       Endif;

       If scNewEntDt > 0;
          dsNewEntDt = *on;
       Else;
          dsNewEntDt = *off;
       Endif;

       If scNewAprDt > 0;
          dsNewAprDt = *on;
       Else;
          dsNewAprDt = *off;
       Endif;

      /end-free
     P MvDbScNew       E
       //=========================================================================
       // MvDbScRnw - Move database record to screen for Renewal Business Tracking
       //=========================================================================
     P MvDbScRnw       B
     d MvDbScRnw       PI
     c
      /free

       scRnwCrtDt = DtMDYY(RTycdt);
       scRnwRvwDt = DtMDYY(RTuwdt);
       scRnwQtADt = DtMDYY(RTqadt);
       scRnwAprDt = DtMDYY(RTaadt);
       scRnwGenDt = DtMDYY(RTpgdt);
       scRnwPlMDt = DtMDYY(RTpold);
       scRnwPnCDt = DtMDYY(RTpcdt);
       scRnwPnMDt = DtMDYY(RTpand);
       scRnwPnPDt = DtMDYY(RTppdt);

       If scRnwCrtDt > 0;
          dsRnwCrtDt = *on;
       Else;
          dsRnwCrtDt = *off;
       Endif;

       If scRnwQtADt > 0;
          dsRnwQtADt = *on;
       Else;
          dsRnwQtADt = *off;
       Endif;

       If scRnwAprDt > 0;
          dsRnwAprDt = *on;
       Else;
          dsRnwAprDt = *off;
       Endif;

      /end-free
     P MvDbScRnw       E
       //===================================================================
       // SaveNew - Save New business tracking record
       //===================================================================
     P SaveNew         B
     d SaveNew         PI
     c
      /free

       Chain scNewTrRrn pnb0p;
       NBiwdt = DtCYMD(scNewRevDt);
       NBpold = DtCYMD(scNewPlMDt);
       NBprdt = DtCYMD(scNewPnRDt);
       NBpcdt = DtCYMD(scNewPnCDt);
       NBpand = DtCYMD(scNewPnMDt);
       NBppdt = DtCYMD(scNewPnPDt);
       NBrcdt = DtCYMD(scNewRecDt);
       NBendt = DtCYMD(scNewEntDt);
       NBaadt = DtCYMD(scNewAprDt);

       If %found(WMNBP);
          NBuser = q1user;
          NBpgm  = q1pgm;
          NBdt   = Dateto7(SystemDate());
          NBtime = Timeto6(SystemTime());
          Update pnb0p;
          Unlock WMNBP;
       Else;
          NBco#  = ##co;
          NBfnd  = ##fnd;
          NBemp# = ##emp;
          NBuser = q1user;
          NBpgm  = q1pgm;
          NBdt   = Dateto7(SystemDate());
          NBtime = Timeto6(SystemTime());
          Write pnb0p;
       Endif;

      /end-free
     P SaveNew         E
       //===================================================================
       // SaveRnw - Save Renewal business tracking record
       //===================================================================
     P SaveRnw         B
     d SaveRnw         PI

      /free

       Chain scRnwTrRrn prt0p;
       RTuwdt = DtCYMD(scRnwRvwDt);
       RTpold = DtCYMD(scRnwPlMDt);
       RTpcdt = DtCYMD(scRnwPnCDt);
       RTpand = DtCYMD(scRnwPnMDt);
       RTppdt = DtCYMD(scRnwPnPDt);
       RTycdt = DtCYMD(scRnwCrtDt);
       RTqadt = DtCYMD(scRnwQtADt);
       RTaadt = DtCYMD(scRnwAprDt);
       RTuser = q1user;
       RTpgm  = q1pgm;
       RTdt   = Dateto7(SystemDate());
       RTtime = Timeto6(SystemTime());
       Update prt0p;
       Unlock WDRTP;

      /end-free
     P SaveRnw         E

       //===================================================================
       // Set the value of scChoice
       //===================================================================
     pSetScChoice      b
     dSetScChoice      pi                  like(scChoice)
     d inCo#                               like(ELco#)
     d inNewRenew                     1    const options(*nopass)
     d inQuote                        1    const options(*nopass)
     d inExcludeXFer                  1n   const options(*nopass)
     d*
     dflgNewRenew      s              1    inz
     dflgQuote         s              1    inz('W')
     disExcludeXFer    s              1n   inz
     dchoiceSelect     s                   like(scChoice)

      /free
       If %parms >= 2;
          flgNewRenew = inNewRenew;
       Endif;
       If %parms >= 3;
          flgQuote = inQuote;
       Endif;
       If %parms >= 4;
          isExcludeXFer = inExcludeXFer;
       Endif;

       choiceSelect = 'QSco# = ' + %char(ELco#);

       Select;
       When flgQuote = '*';
        // No selection criteria
       When flgQuote = 'Q';
          choiceSelect = %trim(choiceSelect) + ' and QSqte = ''Q''';
       Other;
          choiceSelect = %trim(choiceSelect) + ' and QSqte = ''W''';
          flgNewRenew = '*';
       Endsl;

       Select;
       When flgNewRenew = '*';
        // No selection criteria
       When flgNewRenew = 'R';
          choiceSelect = %trim(choiceSelect) + ' and QSrenew = ''Y''';
       When flgNewRenew = 'N';
          choiceSelect = %trim(choiceSelect) + ' and QSrenew = '' ''';
       Endsl;

       Select;
       When isExcludeXFer;
          choiceSelect = %trim(choiceSelect) +
           ' and Upper(QSkey) <> ''T''' +
           ' and Upper(QSkey) <> ''X''';
       Endsl;

       Return choiceSelect;

      /end-free
     PSetScChoice      e

       //===================================================================
       // ValidFilter - Validate filter selection
       //===================================================================
     P ValidFilter     B
     d ValidFilter     PI

      /free

       erFltrSfDt = *off;
       erFltrStDt = *off;
       erFltrSel  = *off;
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If scQteFrmDt > 0;
          intFromDate = DtCYMD(scQteFrmDt);
          If intFromDate = 0;
             msFltrSfDt = 'WCR0500';
             erFltrSfDt = *on;
             isValid = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If scQteToDt > 0;
          intToDate = DtCYMD(scQteToDt);
          If intToDate = 0;
             msFltrStDt = 'WCR0500';
             erFltrStDt = *on;
             isValid = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isValid = *off and scQteFrmDt = 0 and scQteToDt > 0;
          msFltrSfDt = 'WCR2246';
          erFltrSfDt = *on;
          isValid = *on;
       Endif;

       // If only From date entered.
       If isValid = *off and scQteFrmDt > 0 and scQteToDt = 0;
          msFltrStDt = 'WCR2247';
          erFltrStDt = *on;
          isValid = *on;
       Endif;

       // If From date > To date
       If isValid = *off and scQteFrmDt > 0 and scQteToDt > 0
        and intFromDate > intToDate;
          msFltrStDt = 'WCR1502';
          msFltrSfDt = 'WCR1502';
          erFltrStDt = *on;
          erFltrSfDt = *on;
          isValid = *on;
       Endif;

       // If From date and To date selected, selection type should be selected.
       If isValid = *off and scQteFrmDt > 0 and scQteToDt > 0 and
        SsSelFltr = *blanks;
          msFltrSel = 'Selection Type should be entered for the Date +
           Range entry.';
          erFltrSel = *on;
          isValid = *on;
       Endif;

      /end-free
     P ValidFilter     E
       //===================================================================
       // ValidAudFlt - Validate filter selection
       //===================================================================
     P ValidAudFlt     B
     d ValidAudFlt     PI

      /free

       erReqFrDt  = *off;
       erRcvFrDt  = *off;
       erAtmpFrDt = *off;
       erExpFrDt  = *off;
       erPrtFrDt  = *off;
       erCalFrDt  = *off;
       erFnlFrDt  = *off;
       erMldFrDt  = *off;
       erReqToDt  = *off;
       erRcvToDt  = *off;
       erAtmpToDt = *off;
       erExpToDt  = *off;
       erPrtToDt  = *off;
       erCalToDt  = *off;
       erFnlToDt  = *off;
       erMldToDt  = *off;

       clear intFromDate;
       clear intToDate;

       // If Requsted From date is valid.
       If ssReqFrDt > 0;
          intFromDate = DtCYMD(ssReqFrDt);
          If intFromDate = 0;
             msReqFrDt = 'WCR0500';
             erReqFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If Requested To date is valid.
       If ssReqToDt > 0;
          intToDate = DtCYMD(ssReqToDt);
          If intToDate = 0;
             msReqToDt = 'WCR0500';
             erReqToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only Requested To date entered.
       If isVldFlt = *off and ssReqFrDt = 0 and ssReqToDt > 0;
          msReqFrDt = 'WCR2246';
          erReqFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only Requested From date entered.
       If isVldFlt = *off and ssReqFrDt > 0 and ssReqToDt = 0;
          msReqToDt = 'WCR2247';
          erReqToDt = *on;
          isVldFlt = *on;
       Endif;

       // If Requested From date > To date
       If isVldFlt = *off and ssReqFrDt > 0 and ssReqToDt > 0
        and intFromDate > intToDate;
          msReqFrDt = 'WCR1502';
          msReqToDt = 'WCR1502';
          erReqFrDt = *on;
          erReqToDt = *on;
          isVldFlt = *on;
       Endif;

       // Date validation for Received Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If ssRcvFrDt > 0;
          intFromDate = DtCYMD(ssRcvFrDt);
          If intFromDate = 0;
             msRcvFrDt = 'WCR0500';
             erRcvFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If ssRcvToDt > 0;
          intToDate = DtCYMD(ssRcvToDt);
          If intToDate = 0;
             msRcvToDt = 'WCR0500';
             erRcvToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isVldFlt = *off and ssRcvFrDt = 0 and ssRcvToDt > 0;
          msRcvFrDt = 'WCR2246';
          erRcvFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only From date entered.
       If isVldFlt = *off and ssRcvFrDt > 0 and ssRcvToDt = 0;
          msRcvToDt = 'WCR2247';
          erRcvToDt = *on;
          isVldFlt = *on;
       Endif;

       // If From date > To date
       If isVldFlt = *off and ssRcvFrDt > 0 and ssRcvToDt > 0
        and intFromDate > intToDate;
          msRcvFrDt = 'WCR1502';
          msRcvToDt = 'WCR1502';
          erRcvFrDt = *on;
          erRcvToDt = *on;
          isVldFlt = *on;
       Endif;

       // Date validation for Attempt Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If ssAtmpFrDt > 0;
          intFromDate = DtCYMD(ssAtmpFrDt);
          If intFromDate = 0;
             msAtmpFrDt = 'WCR0500';
             erAtmpFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If ssAtmpToDt > 0;
          intToDate = DtCYMD(ssAtmpToDt);
          If intToDate = 0;
             msAtmpToDt = 'WCR0500';
             erAtmpToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isVldFlt = *off and ssAtmpFrDt = 0 and ssAtmpToDt > 0;
          msAtmpFrDt = 'WCR2246';
          erAtmpFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only From date entered.
       If isVldFlt = *off and ssAtmpFrDt > 0 and ssAtmpToDt = 0;
          msAtmpToDt = 'WCR2247';
          erAtmpToDt = *on;
          isVldFlt = *on;
       Endif;

       // If From date > To date
       If isVldFlt = *off and ssAtmpFrDt > 0 and ssAtmpToDt > 0
       and intFromDate > intToDate;
          msAtmpFrDt = 'WCR1502';
          msAtmpToDt = 'WCR1502';
          erAtmpFrDt = *on;
          erAtmpToDt = *on;
          isVldFlt = *on;
       Endif;

       // Date validation for Expiration Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If ssExpFrDt > 0;
          intFromDate = DtCYMD(ssExpFrDt);
          If intFromDate = 0;
             msExpFrDt = 'WCR0500';
             erExpFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If ssExpToDt > 0;
          intToDate = DtCYMD(ssExpToDt);
          If intToDate = 0;
             msExpToDt = 'WCR0500';
             erExpToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isVldFlt = *off and ssExpFrDt = 0 and ssExpToDt > 0;
          msExpFrDt = 'WCR2246';
          erExpFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only From date entered.
       If isVldFlt = *off and ssExpFrDt > 0 and ssExpToDt = 0;
          msExpToDt = 'WCR2247';
          erExpToDt = *on;
          isVldFlt = *on;
       Endif;

       // If From date > To date
       If isVldFlt = *off and ssExpFrDt > 0 and ssExpToDt > 0
       and intFromDate > intToDate;
          msExpFrDt = 'WCR1502';
          msExpToDt = 'WCR1502';
          erExpFrDt = *on;
          erExpToDt = *on;
          isVldFlt = *on;
       Endif;

       // Date validation for Print Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If ssPrtFrDt > 0;
          intFromDate = DtCYMD(ssPrtFrDt);
          If intFromDate = 0;
             msPrtFrDt = 'WCR0500';
             erPrtFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If ssPrtToDt > 0;
          intToDate = DtCYMD(ssPrtToDt);
          If intToDate = 0;
             msPrtToDt = 'WCR0500';
             erPrtToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isVldFlt = *off and ssPrtFrDt = 0 and ssPrtToDt > 0;
          msPrtFrDt = 'WCR2246';
          erPrtFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only From date entered.
       If isVldFlt = *off and ssPrtFrDt > 0 and ssPrtToDt = 0;
          msPrtToDt = 'WCR2247';
          erPrtToDt = *on;
          isVldFlt = *on;
       Endif;

       // If From date > To date
       If isVldFlt = *off and ssPrtFrDt > 0 and ssPrtToDt > 0
       and intFromDate > intToDate;
          msPrtFrDt = 'WCR1502';
          msPrtToDt = 'WCR1502';
          erPrtFrDt = *on;
          erPrtToDt = *on;
          isVldFlt = *on;
       Endif;

       // Date validation for Calculation Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If ssCalFrDt > 0;
          intFromDate = DtCYMD(ssCalFrDt);
          If intFromDate = 0;
             msCalFrDt = 'WCR0500';
             erCalFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If ssCalToDt > 0;
          intToDate = DtCYMD(ssCalToDt);
          If intToDate = 0;
             msCalToDt = 'WCR0500';
             erCalToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isVldFlt = *off and ssCalFrDt = 0 and ssCalToDt > 0;
          msCalFrDt = 'WCR2246';
          erCalFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only From date entered.
       If isVldFlt = *off and ssCalFrDt > 0 and ssCalToDt = 0;
          msCalToDt = 'WCR2247';
          erCalToDt = *on;
          isVldFlt = *on;
       Endif;

       // If From date > To date
       If isVldFlt = *off and ssCalFrDt > 0 and ssCalToDt > 0
       and intFromDate > intToDate;
          msCalFrDt = 'WCR1502';
          msCalToDt = 'WCR1502';
          erCalFrDt = *on;
          erCalToDt = *on;
          isVldFlt = *on;
       Endif;

       // Date validation for Final Print Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If ssFnlFrDt > 0;
          intFromDate = DtCYMD(ssFnlFrDt);
          If intFromDate = 0;
             msFnlFrDt = 'WCR0500';
             erFnlFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If ssFnlToDt > 0;
          intToDate = DtCYMD(ssFnlToDt);
          If intToDate = 0;
             msFnlToDt = 'WCR0500';
             erFnlToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isVldFlt = *off and ssFnlFrDt = 0 and ssFnlToDt > 0;
          msFnlFrDt = 'WCR2246';
          erFnlFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only From date entered.
       If isVldFlt = *off and ssFnlFrDt > 0 and ssFnlToDt = 0;
          msFnlToDt = 'WCR2247';
          erFnlToDt = *on;
          isVldFlt = *on;
       Endif;

       // If From date > To date
       If isVldFlt = *off and ssFnlFrDt > 0 and ssFnlToDt > 0
       and intFromDate > intToDate;
          msFnlFrDt = 'WCR1502';
          msFnlToDt = 'WCR1502';
          erFnlFrDt = *on;
          erFnlToDt = *on;
          isVldFlt = *on;
       Endif;

       // Date validation for Mail Date range
       clear intFromDate;
       clear intToDate;

       // If From date is valid.
       If ssMldFrDt > 0;
          intFromDate = DtCYMD(ssMldFrDt);
          If intFromDate = 0;
             msMldFrDt = 'WCR0500';
             erMldFrDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If ssMldToDt > 0;
          intToDate = DtCYMD(ssMldToDt);
          If intToDate = 0;
             msMldToDt = 'WCR0500';
             erMldToDt = *on;
             isVldFlt = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isVldFlt = *off and ssMldFrDt = 0 and ssMldToDt > 0;
          msMldFrDt = 'WCR2246';
          erMldFrDt = *on;
          isVldFlt = *on;
       Endif;

       // If only From date entered.
       If isVldFlt = *off and ssMldFrDt > 0 and ssMldToDt = 0;
          msMldToDt = 'WCR2247';
          erMldToDt = *on;
          isVldFlt = *on;
       Endif;

       // If From date > To date
       If isVldFlt = *off and ssMldFrDt > 0 and ssMldToDt > 0
       and intFromDate > intToDate;
          msMldFrDt = 'WCR1502';
          msMldToDt = 'WCR1502';
          erMldFrDt = *on;
          erMldToDt = *on;
          isVldFlt = *on;
       Endif;

      /end-free
     P ValidAudFlt     E
       //===================================================================

      * utility procedure definitions
      /COPY SCOPYPOLPI
       //===================================================================
       // * General Visit Tracking Procedures
       //===================================================================
      /Copy wtVstTrkSP
