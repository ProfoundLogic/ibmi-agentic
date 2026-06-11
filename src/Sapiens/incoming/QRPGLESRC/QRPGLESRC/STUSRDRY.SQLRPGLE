      * (c) 2024 Sapiens. All Rights Reserved.                             *
     H/TITLE STUSRDRY - Work with User/team diary items
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Work With User/Team diary items         *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 04/26/12  C:191 PROJECT: Transform                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/31/24 C:342  Support: 115625                              *
     H*                  DSPF Changes.                                *
     H*                  1.Used "Send to Back" option for id ConItem2,*
     H*                  ConItem3 and ConItem4.(Container No 2,3,4)   *
     H*                                                               *
     H*  07/16/24 C:340  Project: WC2972                              *
     H*                  Update Hover Help/Tool Tip for Top 10 Agents.*
     H*                  Break out Group from Policy number as a -    *
     H*                  separate column on Policy Expiring Grid,     *
     H*                  Pending Audit grid and Quote Grids.          *
     H*                                                               *
     H* 09/28/23 C:333 Support: 113925 (re-compilation)               *
     H*                Changes in WTVSTTRKSP copybook.                *
     H*                                                               *
     H* 08/21/23 C:300 Support: 113191 (WC2395)                       *
     H*                Recompile over changes in WTVSTTRK DSPF        *
     H*                                                               *
     H* 07/28/23 C:300 Support: 113102 (WC2395)                       *
     H*                Recompile over changes in WTVSTTRK DSPF        *
     H*                                                               *
     H*  06/08/23 C:342 Project: WC2747                               *
     H*                 For Recompilation, changes in WTVSTTRKSP      *
     H*                 copybook                                      *
     H*                                                               *
     H* 04/06/23 C:333 Support: 107461 (re-compilation)               *
     H*                - Changing Tab Name from 'Visit Track Report'  *
     H*                  to 'Service Request Tracking Report'.        *
     H*                                                               *
     H* 06/21/21 C:298 Project: SY426                                 *
     H*                Re-Brand and Config for Visit Tracking         *
     H*                                                               *
     H* 12/19/19 C:300 PHW86006                                       *
     H*                Users Home Page Loading Issue                  *
     H*                                                               *
     H* 09/26/19 C:291 Project: 129WVN                                *
     H*                Add LF WMVIL7 for multiple visit tracking      *
     H*                per day.                                       *
     H*                                                               *
     H* 09/18/19 C:33  Support: 77348                                 *
     H*                Don't control tabs based on radio button       *
     H*                                                               *
     h* 06/18/19 C:33  Support: 80204 (LUBA)
     h*               -Missed when added in V19R2
     h*
     H* 03/15/18 C:33 -Account for PDSQA                              *
     H*                                                               *
     H* 01/05/18 C:291 Support: 63337                                 *
     H*                Recompile over change to WTVSTTRK screen file. *
     H*                Changed scEdtDiv Value property to "Show       *
     H*                Zero Balance".                                 *
     H*                                                               *
     H* 04/04/17 C:33  Proejct: SY388                                 *
     H*               -Account for Chart&Grids being only tab         *
     H*               -Include an alternate Full Page URL             *
     H*                                                               *
     H* 12/26/16 C:300 Support: 44682                                 *
     H*                Home Page Error - Executive Dept               *
     H*                After changing user department to Executive    *
     H*                Home Page Errors at CoveRisk                   *
     H*                Due to Large Percentage amount (175891.64)     *
     H*                                                               *
     H* 12/06/16 C:33  Proejct: SY388                                 *
     H*                Include tabbed iFrames                         *
     H*                                                               *
     H* 03/16/16 C:309 Support: PHW40304                              *
     H*                Fixed User Diary Count displayed on Chart      *
     H*                                                               *
     H* 02/27/16 C:310 Support: PHW36857                              *
     H*                Modified code to get policy number along with  *
     H*                name in subject of email.                      *
     H*                                                               *
     H* 02/18/16 C:308 Support: PHW34857                              *
     H*                The assignments counts in graph are same for   *
     H*                MyWork & My Team's Work.                       *
     H*                                                               *
     H* 11/16/15 C:33  Project: SY285                                 *
     H*                Add Item type buttons.                         *
     H*                                                               *
     H* 11/03/15 C:306 Project: WC2379                                *
     H*                Policy Assignments - Display Parameters        *
     H*                                                               *
     H* 08/06/15 C:65  Support: Phw# 36367 (TASB)                     *
     H*                Remove duplicate my work/my team radio buttons *
     H*                from the rcdAssign record format. The value    *
     H*                returned from them was always being overridden *
     H*                by the buttons on rcdUser record format.       *
     H*                                                               *
     H* 06/09/15 C:33  35110                                          *
     H*                Load diary items correctly.                    *
     H*                                                               *
     H* 05/04/15 C:279 Support: WA099 85602                           *
     H*                Corrected logic for support #85602.            *
     H*                                                               *
     H* 02/05/15 C:062 Project:114WGU                                 *
     H*                Add visit tracking                             *
     H*                                                               *
     H* 10/07/14 C:062 Support: OP114 86900                           *
     H*                Correct GETCXDATE parameter.                   *
     H*                                                               *
     H* 09/09/14 C:279 Support: WA099 85602                           *
     H*                Added group authority for recent item grid.    *
     H*                                                               *
     H* 08/22/14 C:279 Support: OP099 86449                           *
     H*                Modified to not display duplicate recent items.*
     H*                                                               *
     H* 03/13/14 C:267 Redmine: 1243                                  *
     H*                UI-Added hover message back to the radio button*
     H*                for 'My Work' and 'My Teams work' via tooltip. *
     H*                Additionally, hover message should be displayed*
     H*                only when the radio button is disabled.        *
     H*                                                               *
     H* 03/03/14 C:267 Redmine: 1243                                  *
     H*                Seperate text for 'My Work' and 'My Teams work'*
     H*                radio button and create a new label widget for *
     H*                the text. Move the hover message to the label  *
     H*                widget instead of the radio button.            *
     H*                                                               *
     H* 01/24/14 C:33  Project: SY336                                 *
     H*                Add environment information.                   *
     H*                                                               *
     H* 11/27/13 C:279 Support: OP099 84293                           *
     H*                Modified to display 'Assigned To' field        *
     H*                correctly on Assignments Tab.                  *
     H*                                                               *
     H* 11/13/13 C:267 Redmine: 1243                                  *
     H*               -Check if user is a team member when REFRESH    *
     H*                button is ON.                                  *
     H*               -Use bound field to control hover text when     *
     H*                My Work and My Teams Work radio buttons are    *
     H*                both disabled.                                 *
     H*                                                               *
     H* 10/31/13 C:295 Support 82536                                  *
     H*                If Image Module is not installed dont display  *
     H*                My image Queue and Shared Image Queue          *
     H*                                                               *
     H* 10/18/13 C:267 Redmine: 1243                                  *
     H*                Gray out My Team and My Team's Work radio      *
     H*                buttons based on if the user is a leader.      *
     H*                                                               *
     H* 10/07/13 C:282 Redmine: 2832                                  *
     H*                Assignments count/list will include only       *
     H*                active Policies (Live/Quote) based on selection*
     H*                criteria.                                      *
     H*                                                               *
     H* 05/13/13 C:279 Redmine: 2832                                  *
     H*                Modified to consider the Quote env assignment  *
     H*                as well.                                       *
     H*                                                               *
     H* 04/05/13 C:33  Project: SY316                                 *
     H*                Add CLTID information.                         *
     H*                                                               *
     H* 03/25/13 C:279 Redmine: 2577                                  *
     H*                Modify to not access PDSQTE items if Claims    *
     H*                Only.                                          *
     H*****************************************************************

     fSTUSRDRY  cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflAssign:intRRNa)
     f                                     sfile(SflRecent:intRRNr)
     f                                     sfile(SflItem1:intRRNg)
     f                                     sfile(SflItem2:intRRNg)
     f                                     sfile(SflItem3:intRRNg)
     f                                     sfile(SflItem4:intRRNg)
     f                                     sfile(SflItem5:intRRNg)
     f                                     sfile(SflItem6:intRRNg)
     f                                     sfile(SflItem7:intRRNg)
     f                                     sfile(SflItem8:intRRNg)
     f                                     sfile(SflItem9:intRRNg)
     f                                     sfile(SflItem10:intRRNg)
     f                                     sfile(SflItem11:intRRNg)
     f                                     sfile(SflItem12:intRRNg)
     f                                     sfile(SflItem13:intRRNg)
     f                                     sfile(SflItem14:intRRNg)
     f                                     sfile(SflItem15:intRRNg)
     f                                     sfile(SflItem16:intRRNg)
     fwtVstTrk  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflVstTrk:RrnVst)
     f                                     sfile(sflAgtUndw:RrnAgtUnd)
     f                                     sfile(sflAgtProd:RrnAgtPrd)
     f                                     rename(QSFLCTL006:QSFLCTLV06)
     f                                     rename(QSFLCTL007:QSFLCTLV07)
     f                                     rename(QSFLCTL008:QSFLCTLV08)
     fsdrcl1    if   e           k disk    infds(RCfds)
     fsdrcp     uf   e             disk    rename(prc01:prc0p)
     Fwdell     if   e           k disk    rename(pel01:pel00)
     F                                     infds(ELfds)
     Fwdellq    if   e           k disk    rename(pel01:pel00q) usropn
     F                                     infds(ELfdsQ)
     F                                     prefix(E1:2)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fwmemlq    if   e           k disk    rename(pem01:pem00q)  usropn
     F                                     prefix(pm:2)
     Fwmeml9    if   e           k disk    rename(pem01:pem09q)  usropn
     F                                     prefix(p9:2)
     Fwmempq    if   e           k disk    rename(pem01:pem0pq)  usropn
     F                                     prefix(eq:2)
     Fwdcxl1    if   e           k disk
     Fssylf     if   e           k disk    Prefix(s_)
     fwmvip     uf a e             disk    rename(pvi01:pvi0p)
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
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwdelp     if   e             disk    rename(pel01:pel0p)
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
     fsmupl1    if   e           k disk    rename(pup01:pup01)
     fsmupl     if   e           k disk    rename(pup01:pup00)
     Fwdell8    if   e           k disk    rename(pel01:pel08)
      ********* Begin Add *** 06/18/19 **********************************
     Fsdacl     if   e           k disk    rename(pac01:pac04)
      ********* End   Add *** 06/18/19 **********************************
     fsmdbp     if   e           k disk    rename(pdb01:pdb0p)
     f                                     infds(dbfdsp)
     fsmdrp     if   e           k disk    rename(pdr01:pdr0p)
     f                                     infds(drfdsp)
      ********* Beg Add *** 06/21/21 **********************************
     fwtvipcfg  if   e           k disk    rename(pvi01:pvi0p_cfg)
     f                                     prefix(cfg_)
      ********* End Add *** 06/21/21 **********************************
     f*
     d RCfds           ds
     d  RCrrn                397    400b 0
     d dbfdsp          ds
     d  dbRRNp               397    400b 0
     d drfdsp          ds
     d  drRRNp               397    400b 0
     D*
     D**ENTRY
     DUserDiaryItems   pr                  extpgm('STUSRDRY')
     D                               10    options(*nopass)
     D                                1    options(*nopass)
     DUserDiaryItems   pi
     D inUserId                      10    options(*nopass)
     D inMode                         1    options(*nopass)
     D*
     D/copy scpyUsrDry
     D/copy sprAtrium
     D/copy sprCompany
     D/copy sprDate
     D***  Begin Add  ***  03/15/18  **********************************
     D/copy sPrPrprty
     D***  End   Add  ***  03/15/18  **********************************
     D/copy sprString
     D/copy sprTeam
     D/copy wprEmplr
     D/copy wprClaims
     d/copy wPrGroup
     D/copy sPrTable
     d/copy wprAgent
     d/copy wtSearchDs
     D/copy wprUsrPrms
     D/copy sPrModule
     D/copy sPrGenPop
     D/copy sPrRunCmd
     D/copy wPrSecure

     d* Claim Search
     d ClmSearch       pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     d* Time Delay
     d TimeDelay       pr                  ExtPgm('TIMEDELAY')
     D @@minute                       3    Const
     D @@seconds                      3    Const

     DChangeEnv        pr                  ExtPgm('SETLIBL')

     DResetEnv         pr

     DenvDataArea      ds           450    dtaara('MLIVXA')
     D scCurEnv                      40    overlay(envDataArea:401)

     DholdDataArea     ds           450    dtaara('MLIVXA')
     D holdCurEnv                    40    overlay(holdDataArea:401)

     DDevSpecificTask  pr                  ExtPgm('PUIATEXDEV')
     D task                          20    const
     D rtnCode                        1    options(*nopass)

     DAtriumLaunch     pr                  EXTPGM('STLAUNCH')
     D                               10    const
     D                               26    const
     D                             3250    const
     D                                1    options(*nopass)

     DChgCltId         pr                  ExtPgm('CHGCLTID')
     D clientId                       3

     DChkDevMachine    pr                  ExtPgm('SACHKPDS')
     D dev                            1n

     DGetGridInfo      pr                  ExtPgm('STDASHBRD')
     D outRrn                         9  0 const
     D outGenParm                   512    const
     D inJSON                     32767    const
     D inRtnCode                      1    const

     DExtractJson      pr          5000
     D inTag                        100    const
     D inJSON                     32767    const

     DLoadGrid         pr           115
     D index                          5  0 const
     D inJSON                     32767    const

     DRtvCltId         pr                  ExtPgm('RTVCLTID')
     D clientId                       3

     DRtvObjLib        pr                  ExtPgm('RTVOBJLIB')
     D object                        10    const
     D objType                        7    const
     D library                       10
     D*
     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const
     d/copy wtVstTrkSD

     D*
     D psds          esds                  extname(szq1p)
     D*
     DstrCommand       s            255
     d arrClmFlds      s             10    dim(conSize) based(ptrToClmFld)
     d arrClmDesc      s             40    dim(conSize) based(ptrToClmDsc)
     d arrEmpFlds      s             10    dim(conSize) based(ptrToEmpFld)
     d arrEmpDesc      s             40    dim(conSize) based(ptrToEmpDsc)
     D ptrToEmpFld     s               *   inz
     D ptrToEmpDsc     s               *   inz
     D ptrToClmFld     s               *   inz
     D ptrToClmDsc     s               *   inz
      ***  Begin Add  ***  09/18/19
     D aryTabs         s                   like(DRitem) dim(999)
      ***  End   Add  ***  09/18/19
     D***  Begin Add  ***  12/06/16
     D delimiter       s              1
      ***  Begin Add  ***  09/18/19
     D a1              s             10i 0  inz(1)
      ***  End   Add  ***  09/18/19
     D i1              s              5  0
     D i2              s              5  0
     D***  End   Add  ***  12/06/16
     D intRRNr         s              5  0 inz
     D intRRNa         s              5  0 inz
     D intRRNg         s              5  0 inz
     D intRRN          s              9  0 inz
     D intCo#          s              3  0 inz
     D intI            s              5  0 inz
     D intR            s              5  0 inz
     D recCount        s              9  0 inz
     D myAsgnCount     s              9  0 inz
     D myTmAsgnCount   s              9  0 inz
     D rtnCode         s              1
     D strRRNp         s              9    inz
     D strCallPgm      s             10    inz
     D strUser         s             10    inz
     D strCurUsr       s             10    inz
     D strCurStaf      s              3    inz
     D strCurNm        s             50    inz
     D strCurEmail     s             60    inz
     D strOKey         s             79    inz
     D strContext      s             79    inz
      ***  Begin Add  ***  12/06/16
     D strItemKey      s                   like(DRrole)
      ***  End   Add  ***  12/06/16
      ***  Begin Add  ***  09/18/19
     D strItemType     s                   like(DRrole)
      ***  End   Add  ***  09/18/19
     D strLiveLib      s             10    inz
     D strJson         s          32767
      ***  Begin Add  ***  12/06/16
     D strVisCht       s             10
     D strVisGrd       s             10
     D strVisPnl       s             10
      ***  End   Add  ***  12/06/16
     d***  Beg Add  ***  07/16/24  *** WC2972 ************************
     D strVisDte       s             10
     D strVisBtn       s             10
     D intToDate       s              7  0 inz
     D intFromDate     s              7  0 inz
     D Idx             s              2  0
     D strFrDT         s              8    inz
     D strToDT         s              8    inz
     D strGrp          s              3    inz
     D intPos          s              5  0 inz
     D CurTab          s             10A   inz
     D pretab          s             10A   inz
     d***  End Add  ***  07/16/24  *** WC2972 ************************
     d***  Begin Add  ***  03/15/18  *********************************
     d svCltId         s                   like(scCltid)
     d***  End   Add  ***  03/15/18  *********************************
     D svItemType      S                   like(scItemType)
     D svMyWork        s                   like(ScMyWork)
     D svSummBottom    s              5  0 inz
     D flgRtcd         s              1    inz
     D flgLoadSfl      s               n   inz
     D dateToday       s              7  0 inz
     D dateT30         s              7  0 inz
     D dateT60         s              7  0 inz
     D dateT90         s              7  0 inz
     D futureCount     s              9  0 inz
     D todayCount      s              9  0 inz
     D day30Count      s              9  0 inz
     D day60Count      s              9  0 inz
     D day90Count      s              9  0 inz
     D over90Count     s              9  0 inz
     D isClmOnly       s               n
     DisDevMachine     s               n
     d isError         s               n
     D*
     d dsEmplrFile   e ds                  ExtName(WMEMP) based(ptrEmpDs)
     d dsClaimFile   e ds                  ExtName(WMCMP) based(ptrClmDs)
     D dsQuoteFile   e ds                  ExtName(WMEMPQ) based(ptrQteDs)
     D                                     prefix(EQ:2)
     D @@TabIndex      s              5    inz('00000')
     D @@Qte           s              1    inz('Q')
     D isCxConfirm     s               n   inz(*Off)
     D isCntstAudt     s               n   inz(*Off)
     D intCanclDt      s              7  0 inz
      ***  Begin Add  ***  12/06/16
     D isUseGenTabs    s               n   inz(*off)
     D isUseSumTabs    s               n   inz(*off)
      ***  End   Add  ***  12/06/16
     D charQteStDesc   s             30
     D isQteStsFlg     s              1
     d svDspCltId      s                   like(isDspCltId)
     d***  Begin Add  ***  03/15/18  **************************************
     d svDspEnv        s                   like(isDspEnv)
     d***  End   Add  ***  03/15/18  **************************************
     D futureCountV    s              9  0 inz
     D todayCountV     s              9  0 inz
     D day30CountV     s              9  0 inz
     D day60CountV     s              9  0 inz
     D day90CountV     s              9  0 inz
     D over90CountV    s              9  0 inz
     d***  Begin Add  ***  12/19/19  *********************************
     D ctstCount       s              9  0 inz
     d***  End   Add  ***  12/19/19  *********************************
     D*
     D                 ds
     D  claimKey               1     40
     D  clmCo#                 1      3  0
     D  clmFund                4      6  0
     D  year                   7      9  0
     D  case                  10     16  0

     D                 ds
     D  employerKey            1     40
     D  empCo#                 1      3  0
     D  empFund                4      6  0
     D  employer               7     15  0
     D  div                   16     20  0
     D  quote                 40     40

      /copy stUsrDryDS
      /copy sPrDashBrd

     D conMyWork       c                   const('MYWORK')
     D conTeamWork     c                   const('TEAMWORK')
     D conAgency       c                   const('Agency')
     D conClaims       c                   const('Claim')
     D conPolicy       c                   const('Policy')
     D conQuote        c                   const('Quote')
     D conOther        c                   const('Other')
     D conRnwQuote     c                   const('Renewal Quote')
     D ##keyIM         s              2    Inz
     d charGrpAccess   s              1    Inz

     D HvrTxt          S             80    DIM(2) CTDATA PERRCD(1)
      * Key List on Cancellation Detail File
     c     keyWDCXL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELFnd
     c                   Kfld                    ELEmp#
     c                   Kfld                    ELDiv

      /Free

         // TimeDelay('001':'000');
         Exsr Initialize;
         DoU 1=0;
         //***  Begin Add  ***  03/15/18  *********************************
           svCltId = scCltid;
         //***  End   Add  ***  03/15/18  *********************************

      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
      * Reset Date & group Filters

       If CurTab <> scItemType;
          PreTab = CurTab;
          CurTab = scItemType;

          For Idx = 1 to 16;
              scFDate(Idx) = 0;
              scTDate(Idx) = 0;
              scFrDt(Idx)  = 0;
              scToDt(Idx)  = 0;
              scGrp(Idx)   = 0;
              scBtn(Idx)   = ' ';
          EndFor;

       EndIf;

      ***  End Add  ***  07/16/24  *** WC2972 ************************

           isCltId = '0';
           isCurEnv = '0';
           // Clear and Load Subfiles
           ExSr ClearRecentSubFile;
           ExSr ClearAssgnSubFile;
           ClrVstSubFiles();
           ExSr LoadSummaryTab;
           IF scMyWork = conMyWork;
              @@Assign = q1User;
              ssVstAsgn= q1User;
           ELSE;
              @@Assign = ' ';
              ssVstAsgn= ' ';
           ENDIF;
           LoadVisitGrid(ptrTeamList:intCount);

           // Read Record Formats
           Select;
            //***  Begin Add  ***  12/06/16
           When TabIndex = 0 and isUseSumTabs = *on;
              isDspCltId = svDspCltId;
              //***  Begin Add  ***  03/15/18  ****************************
              isDspEnv = svDspEnv;
              //***  End   Add  ***  03/15/18  ****************************
              Write RcdUser;
              Write rcdHome;
              Exfmt rcdSumTabs;
              Read  rcdHome;
              Read  RcdUser;
              ExSr  ReadLinks;
            //***  End   Add  ***  12/06/16
           When TabIndex = 0;
              isDspCltId = svDspCltId;
              //***  Begin Add  ***  03/15/18  ****************************
              isDspEnv = svDspEnv;
              //***  End   Add  ***  03/15/18  ****************************
            //aPnlHeight = 730;
              Write RcdUser;
              Exfmt rcdHome;
              Read  RcdUser;
              ExSr  ReadLinks;
           When TabIndex = 1;
              isDspCltId = svDspCltId;
              //***  Begin Add  ***  03/15/18  ****************************
              isDspEnv = svDspEnv;
              //***  End   Add  ***  03/15/18  ****************************
              aPnlHeight = 730;
              Write RcdUser;
              Exfmt rcdDiary;
              Read  RcdUser;
           When TabIndex = 2;
              isDspCltId = svDspCltId;
              //***  Begin Add  ***  03/15/18  ****************************
              isDspEnv = svDspEnv;
              //***  End   Add  ***  03/15/18  ****************************
              aPnlHeight = 730;
              Write RcdUser;
              Exfmt rcdAssign;
              Read  RcdUser;
           When TabIndex = 3;
              If (onLoadRrn <> 0);
                LoadVisitDtl(OnLoadRrn);
              else;
                ClearVisitDtl();
                fSetVstDtX = vstScreenX;
                fSetVstDtH = 50;
              EndIf;
              isDspCltId = *off;
              //***  Begin Add  ***  03/15/18  ****************************
              isDspEnv = *off;
              //***  End   Add  ***  03/15/18  ****************************
              if fSetVstDtY < 320;
                 fSetVstDty = 320;
              endif;
              tabHeight =  fSetVstDtH + fSetVstDty - 55;
              if tabHeight < 800;
                 tabHeight = 800;
              endif;
              aPnlHeight = tabHeight + 60;
              Write RcdUser;
              Exfmt rcdVstTrk;
              Read  RcdUser;
           Other;
              isDspCltId = svDspCltId;
              //***  Begin Add  ***  03/15/18  ****************************
              isDspEnv = svDspEnv;
              //***  End   Add  ***  03/15/18  ****************************
              aPnlHeight = 730;
              ExFmt RcdUser;
           EndSl;

           Select;
           When scItemType <> svItemType;
              svItemType = scItemType;
              flgLoadSfl = *on;
           When ScMyWork <> svMyWork;
             svMyWork = ScMyWork;
             flgLoadSfl = *On;
           When btnRefresh = *On;
             btnRefresh = *off;
             flgLoadSfl = *On;
           When isCltIdChg = *on;
              ChgCltId(scCltId);
         //***  Begin Add  ***  03/15/18  *********************************
              If svCltId <> scCltid and isDspEnv = *off;
                 Exsr EndDueToCltid;
              Endif;
         //***  End   Add  ***  03/15/18  *********************************
              isCltId = '1';
           When isEnvChg = *on;
              ChangeEnv();
              Exsr EndDueToChg;
              In envDataArea;
              isCurEnv = '1';
           When isEnvReset = *on;
              ResetEnv();
              Exsr EndDueToReset;

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
      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
           Other;
           For Idx = 1 to 16;

              If ClrFltr(Idx) = *On;
                 ClrFltr(Idx) = *Off;
                 scFDate(Idx) = 0;
                 scTDate(Idx) = 0;
                 scFrDt(Idx)  = 0;
                 scToDt(Idx)  = 0;
                 scGrp(Idx)   = 0;
              EndIf;

              If RefreshC(Idx) = *On;
                 Clear scSetF;
                 Clear scBtn;
                 scBtn(Idx)    = 'On';
                 scSetF(Idx)   = *On;
                 erFDate(Idx)  = *Off;
                 erTDate(Idx)  = *Off;
                 RefreshC(Idx) = *Off;
                 If scFDate(Idx)  > 0;
                    intFromDate = DtCYMD(scFDate(Idx));
                    If intFromDate = 0;
                       msFDate(Idx)  = 'From Date is required';
                       erFDate(Idx) = *on;
                    Endif;
                 Else;
                    intFromDate = 0;
                 Endif;

                 If scTDate(Idx) > 0;
                    intToDate = DtCYMD(scTDate(Idx));
                    If intToDate = 0;
                       msTDate(Idx) = 'To Date is required';
                       erTDate(Idx) = *on;
                    Endif;
                 Else;
                    intFromDate = 0;
                 Endif;

                 If erFDate(Idx) = *Off and erTDate(Idx) = *Off and
                    intFromDate > intToDate;
                    msFDate(Idx) = 'From Date should be less than To Date';
                    erFDate(Idx) = *on;
                 EndIf;

                 If erFDate(Idx) = *Off and erTDate(Idx) = *Off;
                    scFrDt(Idx)  = intFromDate ;
                    scToDt(Idx)  = intToDate   ;
                 EndIf;

              EndIf;
           EndFor;

           //    GetGridInfo(scPnlRRN1:','+DBproc+','+scPnlTst(index)+','
           //                + %Char(intFromDate) +',' + %Char(intToDate)  +
           //                ',':strJson:rtnCode);
      ***  End Add  ***  07/16/24  *** WC2972 ************************

           Endsl;
         RtvCltId(scCltId);
         EndDo;

         *InLr = *On;
         If isClmOnly = *off;
         close Wdellq;
         close Wmemlq;
         close Wmeml9;
         Endif;
         return;

       //*******************************************************************************************
       //  Initialize  Subroutine
       //*******************************************************************************************
       BegSr Initialize;
         If %Parms >= 1;
           strUser = inUserId;
         Else;
           strUser = Q1User;
         EndIf;
         IsLeaderIn = Not(IsLeader(strUser:' '));
         Clear HvrTxtMyTm;
         If  IsLeaderIn = *on;
             HvrTxtMyTm = HvrTxt(1);
         Else;
             HvrTxtMyTm = *BLANKS;
         EndIf;
         ##KeyIM = 'SY';
         Chain  ##keyIM  ssy01;
           If %found(ssylf) and s_SYimif = 'Y';
             dspImgQ = *on;
           Else;
             dspImgQ = *off;
           EndIf;

         SCUsNm = GetUsrName(strUser);
         intCo# = userCo#(strUser);
         today = DateTo7(%date());
         SCusrRrn = GetUsrRrn(strUser);
         SCMyWork = conMyWork;
         svMyWork = conMyWork;
         SCActTab = 0;
         flgLoadSfl = *On;
         LoadFieldArray(intCo#:ptrToEmpFld:ptrToEmpDsc:ptrToClmFld:ptrToClmDsc);
         // If CLTID library is added, call again to use correct LIBL
         DevSpecificTask('Add CLTID library':flgRtCd);
         If flgRtCd = '#';
            Exsr RestartThis;
         Endif;
         ChkDevMachine(isDevMachine);
         RtvObjLib('CLTID':'*DTAARA':scCltIdLib);
         RtvCltId(scCltId);
         //***  Begin Chg  ***  03/15/18  *********************************
         //isDspCltId = %subst(scCltIdLib:1:5) = 'CLTID';
         isDspCltId = SysProperty('ShowClientIDSelector');
         isDspEnv = SysProperty('ShowEnvironmentSelector');
         //***  End   Chg  ***  03/15/18  *********************************
         In envDataArea;
         In holdDataArea;
         isClmOnly = ClaimsOnly();
         If isClmOnly = *off;
         strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WDELLQ;
          clear strCommand;
          strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WMEMLQ;
          clear strCommand;
          strCommand = 'OVRDBF FILE(WMEML9) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEML9)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WMEML9;
          clear strCommand;
          strCommand = 'OVRDBF FILE(WMEMPQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEMPQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WMEMPQ;
         Endif;
       // Set the Category
       @@Category = '      ';
       vstScreenX = 20;
       fSetPolDtY = 320;
       fSetClmDtY = 320;
       fSetAgtDtY = 320;
       fSetVstDtY = 320;
       svDspCltId = isDspCltId;
       //***  Begin Add  ***  03/15/18  **********************
       svDspEnv = isDspEnv;
       //***  End   Add  ***  03/15/18  **********************
       isEditable = *on;
       vstClndrCall = 'Y';
       InzSetup();
       scLdiDarts = 'Y';
       // Set the item types for the widgets on Summary tab
       Chain q1User smupl;
       isItmTpVis = *off;
       Select;
       //***  Begin Add  ***  12/06/16  **************************************
       // Tabbed summary possible
       When UPdept = *blank and IsModEnabled('Dashboard Config');
          scItemType = 'Tabbed';
       //***  End   Add  ***  12/06/16  **************************************
       // No user department specified
       When UPdept = *blank;
          scItemType = 'Base';
       // Department Executives
       When %subst(UPdept:1:3) = 'Exc';
          isItmTpVis = *on;
          scItemType = %subst(UPdept:4);
       //  Executives
       When UPdept = 'Executive';
          isItmTpVis = *on;
          isExcTpVis = *on;
          scItemType = UPdept;
       // Claim or Policy
       Other;
          scItemType = UPdept;
       Endsl;
       svItemType = scItemType;
       //***  Begin Add  ***  09/18/19  ***************************
       strItemType = scItemType;
       //***  End   Add  ***  09/18/19  ***************************


       Select;
       When q1User = '/ROGULSKI';
        //scItemType = 'Test';
          scItemType = 'Testing';
          scTestBtn = scItemType;
          scTestVis = '1';
       When q1User = '/TVALLEY';
          scItemType = 'Test';
          scTestVis = '1';
       When %subst(scItemType:1:4) = 'Test';
          scTestBtn = scItemType;
          scTestVis = '1';
       Other;
          scTestVis = '0';
       Endsl;
       //***  Begin Add  ***  12/06/16
       Select;
       When q1User = 'CGREEN' or q1User = 'MILLERD' or q1User = '/ROGULSKI';
          isUseGenTabs = *on;
       Endsl;
       isUseGenTabs = *off;
       //***  End   Add  ***  12/06/16
       EndSr;

       //================================================================== ************************
       //  End this program due to environment change
       //================================================================== ************************
       Begsr EndDueToChg;
         In envDataArea;
         If scCurEnv <> holdCurEnv;
            PopOneBtn('You changed your environment from "' +
             %trim(holdCurEnv) + '" to "' + %trim(scCurEnv) +
             '". The information on the home page is no longer valid. +
             Reopen from the menu.':
             'Environment Changed':'Close');
            *inlr = *on;
            Return;
         Endif;
       Endsr;

       //***  Begin Add  ***  03/15/18  *********************************
       //================================================================== ************************
       //  End this program due to client id change
       //================================================================== ************************
       Begsr EndDueToCltid;
         PopOneBtn('You changed your client Id from "' +
          %trim(svCltId) + '" to "' + %trim(scCltId) +
          '". The information on the home page is no longer valid. +
          Reopen from the menu.':
          'Client ID Changed':'Close');
         *inlr = *on;
         Return;
       Endsr;
       //***  End   Add  ***  03/15/18  *********************************

       //================================================================== ************************
       //  End this program due to environment reset
       //================================================================== ************************
       Begsr EndDueToReset;
         PopOneBtn('You reset your environment from "' +
          %trim(holdCurEnv) + '" to the default instance envrionment. +
          The information on the home page is no longer valid. +
          Reopen from the menu.':
          'Environment Reset':'Close');
         *inlr = *on;
         Return;
       Endsr;

       //*******************************************************************************************
       //  The LoadDetailsPages  Subroutine
       //*******************************************************************************************
       BegSr LoadDetailsPages;

         clrAssign = *Off;
         dspAssign = *On;
         intRRNa = 0;
         intCount = 0;
         Clear arrTeamList;
         myAsgnCount = 0;
         myTmAsgnCount = 0;
         futureCount  = 0;
         todayCount   = 0;
         day30Count   = 0;
         day60Count   = 0;
         day90Count   = 0;
         over90Count  = 0;
         futureCountV = 0;
         todayCountV  = 0;
         day30CountV  = 0;
         day60CountV  = 0;
         day90CountV  = 0;
         over90CountV = 0;
         ptrTeamList = %Addr(arrTeamList);
         GetUserTeamList(strUser :' ' :ptrTeamList :intCount :'Y');
         IsLeaderIn = Not(IsLeader(strUser:' '));
         Clear HvrTxtMyTm;
         If  IsLeaderIn = *on;
             HvrTxtMyTm = HvrTxt(1);
         Else;
             HvrTxtMyTm = *BLANKS;
         EndIf;
         If intCount > 0;
           For intI = 1 to intCount;
             strCurUsr = arrTeamList(intI);
             strCurNm = GetUsrName(strCurUsr);
             strCurEmail = GetUsrEmail(strCurUsr);
             strCurStaf = GetUsrStaf(strCurUsr);
             ExSr WriteAssgnSubfile;
             If (scMyWork = conMyWork And strCurUsr = strUser)
              Or scMyWork = conTeamWork;
               ExSr GetDiaryCounts;
               ExSr GetVisitCounts;
             Endif;
           EndFor;
         EndIf;
         If scMyWork = conMyWork;
           SCAResults = myAsgnCount;
         Else;
           SCAResults = myAsgnCount + myTmAsgnCount;
         Endif;
         scAsgnCnt = scAresults;
       EndSr;

       //==================================================================
       // Load BASE dashboard items
       //==================================================================
       Begsr LoadItemsBase;
         Exsr LoadChartData;
         svSummBottom = 785;
         isBaseSumm = *on;
       Endsr;

       //==================================================================
       // Load FILE dashboard items
       //==================================================================
       Begsr LoadItemsFile;
         // Load calendar, workload & recent items
         Exsr LoadChartData;
         Clear scChtVis;
         Clear scGrdVis;
         Clear scPnlVis;
         //***  Begin Add  ***  12/06/16
         Clear aryiFrmInfo;
         aryiFrmTab = *hival;
         isUseSumTabs = *off;
         //***  End   Add  ***  12/06/16
         // Read through Dashboard associations for role & load appropriate
         index = 0;
       //***  Begin Chg  ***  12/06/16
       //Setll (scItemType) pdr0p;
         i1 = 0;
         i2 = 0;
       //***  Begin Add  ***  09/18/19  ******************
         Reset a1;
         Reset aryTabs;
       //***  End   Add  ***  09/18/19  ******************
         strItemKey = q1User;
         Setll (strItemKey) pdr0p;
       //***  End   Chg  ***  12/06/16
         Dou %eof(smdrp);
          //***  Begin Chg  ***  12/06/16
          //Reade (scItemType) pdr0p;
            Reade (strItemKey) pdr0p;
            Select;
            When %eof and strItemKey = q1User;
               i2 = 10000;
             //***  Begin Chg  ***  09/18/19  ******************
             //strItemKey = scItemType;
               strItemKey = strItemType;
             //***  Begin Chg  ***  09/18/19  ******************
               Setll (strItemKey) pdr0p;
               Reade (strItemKey) pdr0p;
          //***  Begin Add  ***  09/18/19  ******************
            When %eof and strItemKey = strItemType
             and strItemKey <> scItemType;
               strItemKey = scItemType;
               Setll (strItemKey) pdr0p;
               Reade (strItemKey) pdr0p;
          //***  End   Add  ***  09/18/19  ******************
            Endsl;
          //***  End   Chg  ***  12/06/16
          //***  Begin Add  ***  09/18/19  ******************
            Select;
            // Only show charts/grids for selected item type
            When not %eof and
             (strItemKey <> scItemType and DRtype <> 'Tab');
               DRstat = *blank;
            When not %eof and DRtype = 'Tab' and
             (strItemKey <> strItemType and strItemKey <> q1User);
               DRstat = *blank;
            // Check for tab already available
            When not %eof and DRtype = 'Tab' and DRstat = 'A';
               If a1 > 1;
                  Sorta %subarr(aryTabs:1:a1-1);
               Endif;
               If %lookup(DRitem:aryTabs:1:a1) <= 0;
                  aryTabs(a1) = DRitem;
                  a1 += 1;
               Else;
                  DRstat = *blank;
               Endif;
            Endsl;
          //***  End   Add  ***  09/18/19  ******************
            If not %eof and DRstat = 'A';
               Setll (DRitem) pdb0p;
               Dou %eof(smdbp);
                  Reade (DRitem) pdb0p;
                  If not %eof and DBstat <> 'I';
                     If DBteamLead = 'Y' and isLeaderIn = *on;
                        Iter;
                     Endif;
                   //***  Begin Add  ***  09/18/19  ******************
                     Select;
                     // Only show charts/grids for selected item type
                     When not %eof and
                      (strItemKey <> scItemType
                      and (%subst(DBtype:1:5) = 'Chart' or DBtype = 'Grid'));
                      //  Iter;
                     Endsl;
                   //***  End   Add  ***  09/18/19  ******************
                     //***  Begin Add  ***  12/06/16
                     If DRtype = 'Tab';
                        isUseSumTabs = *on;
                     Endif;
                     If DBtype = 'iFrame';
                        i1 += 1;
                        aryiFrmTab(i1) = DRposX + i2;
                        aryiFrmHgt(i1) = GetWidgetHeight('iFrame':DBsizeH);
                        aryiFrmWth(i1) = GetWidgetWidth('iFrame':DBsizeW);
                        aryiFrmURL(i1) = DBurl;
                        aryFullPage(i1) = DBfullPage;
                        aryiTabHgt(i1) = GetWidgetHeight('iTab':DBsizeH);
                        aryiTabNam(i1) = DBdesc;
                        aryiFrmItem(i1) = DBitem;
                        //***  Begin Add  ***  04/04/17  ******************
                        aryUrlFull(i1) = DBurlFull;
                        If DBurlFull = *blanks;
                           aryUrlFull(i1) = DBurl;
                        Endif;
                        //***  End   Add  ***  04/04/17  ******************
                        Iter;
                     Endif;
                     //***  End   Add  ***  12/06/16
                     index += 1;
                     scPnlVal(index) = 'Loading...';
                     scPnlTop(index) = GetWidgetTop('panel':DRposY);
                     scPnlLft(index) = GetWidgetLeft('panel':DRposX);
                     scPnlHgt(index) = GetWidgetHeight('panel':DBsizeH);
                     scPnlWth(index) = GetWidgetWidth('panel':DBsizeW);
                     scPnlVis(index) = '1';
                     scPnlRRN(index) = dbRRNp;
                     scPnlPro(index) = DBproc;
                     scPnlTst(index) = %editc(index:'Z');
                     Select;
                     When %subst(DBtype:1:5) = 'Chart';
                        scChtPro(index) = DBproc;
                        scChtUrl(index) = DBurl;
                        scChtTop(index) = GetWidgetTop('Chart':DRposY);
                        scChtLft(index) = GetWidgetLeft('Chart':DRposX);
                        scChtHgt(index) = GetWidgetHeight('Chart':DBsizeH);
                        scChtWth(index) = GetWidgetWidth('Chart':DBsizeW);
                        scChtVis(index) = '1';
                        scDrpVal(index) = DRtype;
      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
                        scDteTop(index) = scChtTop(index) - 15;
                        scDteLft(index) = scChtLft(index);
                        scBtnLft(index) = scChtLft(index) + 290;
                        scDteVis(index) = '1';
                        scBtnVis(index) = '1';

                        If scChtPro(index) = 'LitigationClaim' Or
                           scChtPro(index) = 'TotalIncurred'   Or
                           scChtPro(index) = 'Top10AgentInfo'  Or
                           scChtPro(index) = 'ClosedSettle'    Or
                           scChtPro(index) = 'LargeLossRpt'       ;
                           scDteVis(index) = '0';
                        EndIf;

                        GetGridInfo(dbRRNp:','+DBproc+','+scPnlTst(index)+','+
                                    %Char(scFrDt(Index)) +','+
                                    %Char(scToDt(Index)) +','+
                                    %Char(scGrp(Index))  +
                                    ',':strJson:rtnCode);
              strFrDT = ExtractJson('fltFrDate':strJson);
              strToDT = ExtractJson('fltToDate':strJson);
              strGrp  = ExtractJson('fltGroup':strJson);


              If %Len(%Trim(strFrDT)) > 6 and %Len(%Trim(strToDT)) > 6;
                 Monitor;
                   scFDate(Index) = %dec(strFrDT:8:0);
                   scTDate(Index) = %dec(strToDT:8:0);
                 On-Error;
                   scFDate(Index) = 0;
                   scTDate(Index) = 0;
                 EndMon;
                 Monitor;
                   scGrp(Index) = %dec(strGrp :3:0);
                 On-Error;
                   scGrp(Index) = 0;
                 EndMon;
              Else;
                 If %Len(%Trim(strGrp)) > 0;
                    Monitor;
                      scGrp(Index) = %dec(strGrp :3:0);
                    On-Error;
                      scGrp(Index) = 0;
                    EndMon;
                 EndIf;
              Endif;

              intPos  = %scan('fltToDate':%Trim(strJson):1);
              If intPos <=0;
                 intPos  = %scan('fltGroup':%Trim(strJson):1);
              Endif;
              If intPos > 0;
                 strJson = %SubSt(%Trim(strJson):1:intPos - 3);
              Endif;
      ***  End Add  ***  07/16/24  *** WC2972 ************************
                     When DBtype = 'Grid';
                        scChtPro(index) = DBproc;
      ***  Beg Chg  ***  07/16/24  *** WC2972 ************************
                       // scGrdTop(index) = GetWidgetTop('Grid':DRposY);
                       // scGrdLft(index) = GetWidgetLeft('Grid':DRposX);
                       // scGrdVis(index) = '1';
                       // scPnlVis(index) = '0';
                       // GetGridInfo(dbRRNp:','+DBproc+','+scPnlTst(index)+',':
                       //  strJson:rtnCode);
                        scGrdTop(index) = GetWidgetTop('Grid':DRposY);
                        scGrdTop(index) = scGrdTop(index) + 35;
                        scDteTop(index) = scGrdTop(index) - 28;
                        scGrdLft(index) = GetWidgetLeft('Grid':DRposX);
                        scDteLft(index) = scGrdLft(index);
                        scBtnLft(index) = scGrdLft(index) + 290;
                        scDteVis(index) = '1';
                        scBtnVis(index) = '1';
                        scGrdVis(index) = '1';
                        scPnlVis(index) = '0';
                        scGrdTop(index) = scGrdTop(index) + (DRposY-1) * 10;
                        scDteTop(index) = scGrdTop(index) - 28;

                        GetGridInfo(dbRRNp:','+DBproc+','+scPnlTst(index)+','+
                                    %Char(scFrDt(Index)) +','+
                                    %Char(scToDt(Index)) +','+
                                    %Char(scGrp(Index))  +
                                    ',':strJson:rtnCode);

      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
              strFrDT = ExtractJson('fltFrDate':strJson);
              strToDT = ExtractJson('fltToDate':strJson);
              strGrp  = ExtractJson('fltGroup':strJson);

              If %Len(%Trim(strFrDT)) > 6 and %Len(%Trim(strToDT)) > 6;
                 Monitor;
                   scFDate(Index) = %dec(strFrDT:8:0);
                   scTDate(Index) = %dec(strToDT:8:0);
                 On-Error;
                   scFDate(Index) = 0;
                   scTDate(Index) = 0;
                 EndMon;
                 Monitor;
                   scGrp(Index) = %dec(strGrp :3:0);
                 On-Error;
                   scGrp(Index) = 0;
                 EndMon;
              Else;
                 If %Len(%Trim(strGrp)) > 0;
                    Monitor;
                      scGrp(Index) = %dec(strGrp :3:0);
                    On-Error;
                      scGrp(Index) = 0;
                    EndMon;
                 EndIf;
              Endif;

              intPos  = %scan('fltToDate':%Trim(strJson):1);
              If intPos <=0;
                 intPos  = %scan('fltGroup':%Trim(strJson):1);
              Endif;
              If intPos > 0;
                 strJson = %SubSt(%Trim(strJson):1:intPos - 3);
              Endif;
      ***  End Add  ***  07/16/24  *** WC2972 ************************
                        scPnlVal(index) = LoadGrid(index:strJson);
                        If rtnCode = 'X'
                         or %subst(scPnlVal(index):1:15) = 'Empty';
                           scPnlVal(index) = %subst(scPnlVal(index):16);
                           scGrdVis(index) = '0';
      ***  Beg Chg  ***  07/16/24  *** WC2972 ************************
                           scDteVis(index) = '0';
                           scBtnVis(index) = '0';
      ***  End Chg  ***  07/16/24  *** WC2972 ************************
                           scPnlVis(index) = '1';
                        Endif;
                     Endsl;
                     If svSummBottom < scPnlTop(index)+scPnlHgt(index);
                        svSummBottom = scPnlTop(index)+scPnlHgt(index);
                     Endif;
                     //***  Begin Add  ***  12/06/16
                     Select;
                     When DBtype = 'iFrame';
                        index -= 1;
                     Endsl;
                     //***  End   Add  ***  12/06/16
                  Endif;
               Enddo;
            Endif;
         Enddo;
         //***  Begin Add  ***  12/06/16
         isSumTabs = isUseSumTabs;
         Select;
         When isUseSumTabs = *on;
          //svSummBottom = conFirstTop + conSpacing;
            Clear sciFrmHgt;
            Clear sciFrmWth;
            Clear sciFrmUrl;
            Clear sciFrmItem;
            Clear sciTabHgt;
            Clear sciTabNam;
            Clear scFullPage;
            //***  Begin Add  ***  04/04/17  ******************************
            Clear scUrlFull;
            //***  End   Add  ***  04/04/17  ******************************
            Sorta %subarr(aryiFrmTab:1);
            Clear delimiter;
            //***  Begin Add  ***  04/04/17  ******************************
            If i1 = 1 and aryiFrmItem(i1) = 'ChartsGrids';
               isUseSumTabs = *off;
               isSumTabs = isUseSumTabs;
            Endif;
            If i1 > 1 or aryiFrmItem(i1) <> 'ChartsGrids';
            //***  End   Add  ***  04/04/17  ******************************
            For i2 = 1 to i1;
               If i2 > 1;
                  delimiter = ',';
               Endif;
NP01           If aryiFrmItem(i2) = 'ChartsGrids';
                  If svSummBottom < 770 + 60 - conAddTopTab;
                     svSummBottom = 770 + 60 - conAddTopTab;
                  Endif;
               // aryiTabHgt(i2) = svSummBottom - conFirstTop + conAddTopTab
                  aryiTabHgt(i2) = svSummBottom - 320;

NP01           Endif;
               If aryFullPage(i2) = *blanks;
                  aryFullPage(i2) = 'visible';
               Endif;
NP01           aryiFrmHgt(i2) = aryiTabHgt(i2) - conSpacing;
               sciFrmHgt = %trim(sciFrmHgt) + %trim(delimiter) +
                %editc(aryiFrmHgt(i2):'X') + 'px';
               sciFrmWth = %trim(sciFrmWth) + %trim(delimiter) +
                %editc(aryiFrmWth(i2):'X') + 'px';
               sciFrmUrl = %trim(sciFrmUrl) + %trim(delimiter) +
                aryiFrmURL(i2);
               sciTabHgt = %trim(sciTabHgt) + %trim(delimiter) +
                %editc(aryiTabHgt(i2):'X') + 'px';
               sciTabNam = %trim(sciTabNam) + %trim(delimiter) +
                aryiTabNam(i2);
               sciFrmItem = %trim(sciFrmItem) + %trim(delimiter) +
                aryiFrmItem(i2);
               scFullPage = %trim(scFullPage) + %trim(delimiter) +
                aryFullPage(i2);
               //***  Begin Add  ***  04/04/17  ***************************
               scUrlFull = %trim(scUrlFull) + %trim(delimiter) +
                aryUrlFull(i2);
               //***  End   Add  ***  04/04/17  ***************************
            Endfor;
            sciTabHgtI = aryiTabHgt(1);
            //***  Begin Add  ***  04/04/17  ******************************
            Endif;
            //***  End   Add  ***  04/04/17  ******************************
         When isUseGenTabs;
          //scIFrmURL = ',' +
            scIFrmURL =
             'https://www.profoundlogic.com,'+
             'https://app.powerbi.com/view?r='+
              'eyJrIjoiYWM2NmJlZGItYTM4Yi00ZTBkLTkzYzEtMzMyMWUzOTY1NTV'+
              'iIiwidCI6IjJkYjU0YjA4LWU5YzktNDNjNC1hYmM5LTNlMDk4NjkxZj'+
              'E2MiIsImMiOjZ9,'+
             'https://www.stoneriver.com,'+
             'https://public.tableau.com/views/AnalyzeSuperstore_7/'+
              'Product?:embed=y&:display_count=yes&:showTabs=y';
          //scIFrmHgt = %editc(GetWidgetHeight('iFrame':4):'X') + 'px,' +
            scIFrmHgt =
             '770px,770px,380px,770px';
          //scIFrmWth = %editc(GetWidgetWidth('iFrame':4):'X') + 'px' +
            scIFrmWth =
             '1190px,1190px,590px,1190px';
          //scIFrmHgt = %editc(GetWidgetHeight('iFrame':4):'X') + ',' +
            scIFrmHgt =
             '770,770,380,770';
          //scIFrmWth = %editc(GetWidgetWidth('iFrame':4):'X') + ',' +
            scIFrmWth =
             '1190,1190,590,1190';
          //scITabNam = 'Charts & Grids,' +
            scITabNam =
             'Profound Logic,PowerBI,StoneRiver,Tableau';
          //scITabHgt = %editc(GetWidgetHeight('iTab':4):'X') + 'px,' +
            scITabHgt =
             '30px,830px,440px,830px';
            isUseSumTabs = *on;
         Endsl;
         // Update graphs/grids for use in a tab?
         Clear scVisPnl;
         Clear scVisCht;
         Clear scVisGrd;
         Clear scVisDte;
         Clear scVisBtn;
         Clear delimiter;
         For i2 = 1 to #ofItems;
            If i2 > 1;
               delimiter = ',';
            Endif;
            strVisPnl = 'hidden';
            strVisCht = 'hidden';
      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
            strVisDte = 'hidden';
            strVisBtn = 'hidden';
      ***  End Add  ***  07/16/24  *** WC2972 ************************
            strVisGrd = 'hidden';
            If scPnlVis(i2) = *on;
               strVisPnl = 'visible';
            Endif;
            If scChtVis(i2) = *on;
               strVisCht = 'visible';
      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
               strVisDte = 'visible';
               strVisBtn = 'visible';
      ***  End Add  ***  07/16/24  *** WC2972 ************************
            Endif;
            If scGrdVis(i2) = *on;
               strVisGrd = 'visible';
            Endif;
            scVisPnl = %trim(scVisPnl) + %trim(delimiter) + strVisPnl;
            scVisCht = %trim(scVisCht) + %trim(delimiter) + strVisCht;
            scVisGrd = %trim(scVisGrd) + %trim(delimiter) + strVisGrd;
      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
            scVisDte = %trim(scVisDte) + %trim(delimiter) + strVisDte;
            scVisBtn = %trim(scVisBtn) + %trim(delimiter) + strVisBtn;
      ***  End Add  ***  07/16/24  *** WC2972 ************************
            If isUseSumTabs = *on;
               scPnlVis(i2) = *off;
               scPnlTop(i2) += conAddTopTab;
               scChtVis(i2) = *off;
               scChtTop(i2) += conAddTopTab;
               scGrdVis(i2) = *off;
               scGrdTop(i2) += conAddTopTab;
      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
            // scDteVis(i2) = *off;
               scDteTop(i2) += conAddTopTab;
      ***  End Add  ***  07/16/24  *** WC2972 ************************
            Endif;
         Endfor;
         //***  End   Add  ***  12/06/16
         If index < 1;
            Exsr LoadItemsBase;
         Endif;
       Endsr;

       //==================================================================
       // Load TEST dashboard items
       //==================================================================
       Begsr LoadItemsTest;
         Exsr LoadChartData;
         // 1st chart
         scPnlVal1  = 'Loading...';
         scPnlTop1  = 455;
         scPnlLft1  = 20;
         scPnlHgt1  = 330;
         scPnlWth1  = 590;
         scPnlVis1  = '1';
         scChtPro1  = 'TestChart';
         scChtTyp1  = '1';
         scChtTop1  = scPnlTop1 + 30;
         scChtLft1  = scPnlLft1 + 10;
         scChtHgt1  = scPnlHgt1 - 40;
         scChtWth1  = scPnlWth1 - 20;
         scChtVis1  = scPnlVis1;
         scChtRRN1  = 999999999;
         // 2nd chart
         scPnlVal2  = 'Loading...';
         scPnlTop2  = 455;
         scPnlLft2  = 620;
         scPnlHgt2  = 330;
         scPnlWth2  = 590;
         scPnlVis2  = '0';
         scChtPro2  = 'TestChart';
         scChtTyp2  = '2';
         scChtTop2  = scPnlTop2 + 30;
         scChtLft2  = scPnlLft2 + 10;
         scChtHgt2  = scPnlHgt2 - 40;
         scChtWth2  = scPnlWth2 - 20;
         scChtVis2  = scPnlVis2;
         svSummBottom = scPnlTop1 + scPnlHgt1 - 55;
       Endsr;

       //==================================================================
       // Load summary tab with appropriate widgets
       //==================================================================
       Begsr LoadSummaryTab;
         scCalVis   = *on;
         scWkLodVis = *on;
         scRcntVis  = *on;
         isBaseSumm = *off;
         svSummBottom = 0;
         // Turn off visibility to all 16 panels and charts (and grids);
         Clear scPnlVal;
         Clear scPnlTop;
         Clear scPnlLft;
         Clear scPnlHgt;
         Clear scPnlWth;
         Clear scPnlVis;
         Clear scChtTyp;
         Clear scChtTop;
         Clear scChtLft;
         Clear scChtHgt;
         Clear scChtWth;
         Clear scChtVis;
         Clear scDrpVal;
      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
         Clear scDteTop;
         Clear scDteLft;
         Clear scBtnLft;
         Clear scDteVis;
         Clear scBtnVis;
      ***  End Add  ***  07/16/24  *** WC2972 ************************

         Exsr LoadDetailsPages;

         // Load Dashboard Items
         Select;
         When scItemType = 'Base';
            Exsr LoadItemsBase;
         When scItemType = 'Test';
            Exsr LoadItemsTest;
         Other;
            Exsr LoadItemsFile;
         Endsl;

         If svSummBottom < conFirstTop;
            svSummBottom = conFirstTop;
         Endif;
         aPnlHeight = svSummBottom - 55;
NP01     aPnlHeight = svSummBottom + 20;
         //***  Begin Del  *** 03/15/18  **********************************
         //scCltIdTop = svSummBottom + 15;
         //scCltIdLft = 5;
         //***  Begin Add  ***  12/06/16
         //scCltIdTop = 806;
         //***  End   Add  ***  12/06/16
         //If scCltIdTop > 805;
         //   scCltIdTop = 0;
         //   scCltIdLft = 285;
         //Endif;
         //***  End   Del  *** 03/15/18  **********************************
         If scRcntVis = *on;
            Exsr LoadRecentSubfile;
         Endif;
         If scWkLodVis = *on;
            Exsr LoadWorkLoadData;
         Endif;
         If scWkLodVis = *off;
            dspImgQ = *off;
         Endif;
       Endsr;

       //*******************************************************************************************
       //  ClearAssgnSubfile Subroutine - To Clear subfile
       //*******************************************************************************************
       BegSr ClearAssgnSubFile;
         intRRNa = 0;
         clrAssign = *On;
         dspAssign = *Off;
         Write rcdAssign;
       EndSr;

       //*******************************************************************************************
       //  ReadLinks - Determines which links are clicked.
       //*******************************************************************************************
       BegSr ReadLinks;

         Select;
         When scAlert = *On;
            TabIndex = 1;
            scActTab = 1;
            scLdiDarts = 'N';
         When scAsgn  = *On;
            TabIndex = 2;
            scActTab = 2;
         When scVisit = *On;
            TabIndex = 3;
            scActTab = 3;
         EndSl;

       EndSr;

       //*******************************************************************************************
       //  WriteAssgnSubFile subroutine -
       //*******************************************************************************************
       BegSr WriteAssgnSubFile;
         For intR = 1 to conSize;
           If arrEmpFlds(intR) <> *Blanks Or arrClmFlds(intR) <> *Blanks;
             BuildSQLs(strCurStaf:arrEmpFlds(intR):arrClmFlds(intR));
             Exsr ProcessRoles;
           EndIf;
         EndFor;
       EndSr;

       //*******************************************************************************************
       //  Process Roles Subroutine -
       //*******************************************************************************************
       BegSr ProcessRoles;
         If arrEmpFlds(intR) <> *Blanks;
           If (scMyWork = conMyWork And strCurUsr = strUser) Or
            (scMyWork = conMyWork And strCurUsr <> strUser) Or
            scMyWork = conTeamWork;
             If OpenCurSor('E');
                 DoW FetchNextEmp(ptrEmpDs) = *Zeros;
                   If intRRNa >=9999;
                     Leave;
                   Endif;
                   SCAContext = %Trim(EMname)+ ' : ' + %Editc(EMfnd:'X')+ '-'
                   + %Editc(EMemp#:'X') + '-' + %Editc(EMdiv:'X');
                   isCntstAudt = *Off;
                  // To get Contested Audit for an employer.

         //***  Begin Chg  ***  12/19/19  *********************************
         //*****   Setll (EMCo#:EMFnd:EMEmp#:*loval) pel08;
         //*****   Reade (EMCo#:EMFnd:EMEmp#) pel08;
         //*****   Dow Not %eof(wdell8);
         //*****      If ELctst = 'Y';
         //*****         isCntstAudt = *On;
         //*****         Leave;
         //*****      EndIf;
         //*****      Reade (EMCo#:EMFnd:EMEmp#) pel08;
         //*****   EndDo;
                   Exec sql
                    select count(*) into :ctstCount
                     from WDELL8
                     where ELco# = :EMco# and ELfnd = :EMfnd and
                     ELemp# = :EMemp# and ELctst = 'Y';
                   isCntstAudt = ctstCount > 0;
         //***  End   Chg  ***  12/19/19  *********************************

                   Clear ELrrn;
                   Setgt (EMCo#:EMFnd:EMEmp#:EMDiv) pel00;
                   Readpe (EMCo#:EMFnd:EMEmp#:EMDiv) pel00;

                     isCxConfirm = *off;
                     intCanclDt = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);

                  // To get the latest cancellation record to retrive the confirmed
                  // cancellation flag status.
                     Setgt keyWDCXL1 pcx01;
                     If %found(wdcxl1);
                        Readpe keyWDCXL1 pcx01;
                        If CXcffl = 'Y';
                           isCxConfirm = *On;
                        EndIf;
                     EndIf;

                     If (intCanclDt = 0) Or
                     (intCanclDt <> 0 And intCanclDt > DateTo7(SystemDate)
                     And isCxConfirm = *off) Or
                     (intCanclDt <> 0 And isCxConfirm = *Off) Or
                     (intCanclDt <> 0 And isCxConfirm And
                     ELfaf <> 'A' ) Or
                     (intCanclDt <> 0 And isCxConfirm And
                     isCntstAudt );

                       strRRNp=%Editc(ELrrn:'X');
                       SCALnkURL=CreateURL('STDRVPGM':SCAContext:strRRNp:'C'
                       :conPolicyPgm);
                       SCAtype = conPolicy;
                       SCANumber = %Editc(EMfnd:'X') + '-' +
                       %Editc(EMemp#:'X') + '-' + %Editc(EMdiv:'X');
                       SCAUser = strCurNm;
                       SCAEmail = %Trim(strCurEmail) + '?subject=' +
                       %Trim(SCAtype) + ': ' + %Trim(SCAContext);
                       Clear scAMailTTp;
                       scAMailTTp = %Trim(strCurEmail);
                       If scAMailTTp = *blanks;
                          scAMailTTp = 'No E-Mail Available';
                       Endif;
                       SCARole = arrEmpDesc(intR);
                       Exsr PopulateAsgnSfl;

                   EndIf;
                 Enddo;
               CloseCurSor();
             Endif;
           EndIf;
           If isClmOnly = *off;
              If (scMyWork = conMyWork And strCurUsr = strUser) Or
                 (scMyWork = conMyWork And strCurUsr <> strUser) Or
                 scMyWork = conTeamWork;
                 If OpenCurSor('Q');
                    DoW FetchNextQte(ptrQteDs) = *Zeros;
                       If intRRNa >=9999;
                          Leave;
                       Endif;
                       SCAContext = %Trim(EQname);
                       Clear ELrrnQ;
                       Setgt (EQCo#:EQFnd:EQEmp#:EQDiv) pel00q;
                       Readpe (EQCo#:EQFnd:EQEmp#:EQDiv) pel00q;
                       charQteStDesc = *Blanks;
                       isQteStsFlg = *Blank;
                       charQteStDesc = GetQuoteStsDesc(E1Co#:E1Fnd:E1Emp#
                       :E1Div:E1fyr:'Q');
                       If ((%trim(Upper(charQteStDesc))) = 'LOST'      Or
                          (%trim(Upper(charQteStDesc)))  = 'DECLINED'  Or
                          (%trim(Upper(charQteStDesc)))  = 'CLOSED'    Or
                          (%trim(Upper(charQteStDesc)))  = 'DUPLICATE');
                          isQteStsFlg = 'Y';
                       EndIf;
                       If E1ptdt >= DateTo7(SystemDate) And isQteStsFlg = ' ';
                          strRRNp=%Editc(ELrrnQ:'X');
                          SCALnkURL=CreateQteURL('STDRVPGM':SCAContext:strRRNp
                          :'C':conPolicyPgm:@@TabIndex:@@Qte);
                          If (%trim(Upper(charQteStDesc))) = 'RENEWAL QUOTE';
                             SCAtype = conRnwQuote;
                          Else;
                             SCAtype = conQuote;
                          EndIf;
                          SCANumber = %Editc(EQfnd:'X') + '-' +
                          %Editc(EQemp#:'X') + '-' + %Editc(EQdiv:'X');
                          SCAUser = strCurNm;
                          SCAEmail = %Trim(strCurEmail) + '?subject=' +
                          %Trim(SCAtype) + ': ' + %Trim(SCAContext);
                          Clear scAMailTTp;
                          scAMailTTp = %Trim(strCurEmail);
                          If scAMailTTp = *blanks;
                             scAMailTTp = 'No E-Mail Available';
                          Endif;
                          SCARole = arrEmpDesc(intR);
                          Exsr PopulateAsgnSfl;
                       EndIf;
                    Enddo;
                    CloseCurSor();
                 Endif;
              EndIf;
           EndIf;
         EndIf;

         If arrClmFlds(intR) <> *Blanks;
           recCount = GetSQLCount('C');
           If strCurUsr = strUser;
             myAsgnCount += recCount;
           Else;
             myTmAsgnCount += recCount;
           Endif;
           If (scMyWork = conMyWork And strCurUsr = strUser) Or
            scMyWork = conTeamWork;
             If OpenCurSor('C');
               DoW FetchNextClm(ptrClmDs) = *Zeros;
                 If intRRNa >=9999;
                   Leave;
                 Endif;
                 SCAContext = %Trim(CMlsn) + ', ' + %Trim(CMFsn);
                 strRRNp=%Editc(GetClaimRRN(CMCo#:CMFnd:CMFyr:CMCase):'X');
                 SCALnkURL=CreateURL('STDRVPGM':SCAContext:strRRNp:'C'
                  :conClaimPgm);
                 SCAtype = conClaims;
                 SCANumber = %Editc(CMfnd:'X') + '-' +
                    %Editc(CMfyr:'X') + '-' + %Editc(CMcase:'X');
                 SCAUser = strCurNm;
                 SCAEmail = %Trim(strCurEmail) + '?subject=' +
                  %Trim(SCAtype) + ': ' + %Trim(SCAContext);
                 Clear scAMailTTp;
                 scAMailTTp = %Trim(strCurEmail);
                 If scAMailTTp = *blanks;
                    scAMailTTp = 'No E-Mail Available';
                 Endif;
                 SCARole = arrClmDesc(intR);
                 If scmywork = conmywork and strCurUsr = strUser;
                   intRRNa +=1;
                   Write SflAssign;
                 Endif;

                 If scmywork = conTeamWork;
                   intRRNa +=1;
                   Write SflAssign;
                 Endif;
               Enddo;
               CloseCurSor();
             EndIf;
           EndIf;
         EndIf;
       EndSr;

       //*******************************************************************************************
       //  The LoadRecentSubFile  Subroutine
       //*******************************************************************************************
       BegSr LoadRecentSubFile;

         clrRecent = *Off;
         dspRecent = *On;
         intRRNr = 0;
         Setgt struser prc01;
         DoU %Eof(sdrcl1);
           Readpe struser prc01;
           If %Eof(sdrcl1);
             Leave;
           Endif;
           scRcMode = RCmdsc;
           If scRcMode = *Blanks;
             Select;
             //When RCmode = 'A';
             //  scRcMode = 'Add';
             When RCmode = 'C';
               scRcMode = 'Update';
             When RCmode = 'D';
               scRcMode = 'Inquiry';
             Other;
               RCmode = 'C';
               scRcMode = 'Update';
             EndSl;
           Endif;
           isError = *off;
           charGrpAccess = ' ';
           isGrpAuth = 'N';
           scRcUrl = *blanks;
           Select;
           When RCtype = 'A';
           When RCtype = 'C';
             claimKey = RCkey;
             If GetClaimRrn(clmCo#:clmFund:year:case) > 0;
             strConText = GetClaimName(clmCo#:clmFund:year:case:intRrn);
             strCallPgm = conClaimPgm;
             scRcType = conClaims;
             strRRNp=%Editc(intRrn:'X');
             If GroupAuth(clmCo#:clmFund:q1User:charGrpAccess) = '1';
                Exsr chkGrpAcs;
                scRcUrl = CreateURL('STDRVPGM':strContext:strRRNp:RCmode
                 :strCallPgm);
             Endif;
             Else;
                isError = *on;
             Endif;
           When RCtype = 'E';
             employerKey = RCkey;
             // If quote = 'Q';
             If quote = 'Q' and isClmOnly = *off;
               Exsr ProcessQuote;
             Else;
               Exsr ProcessLive;
             Endif;
           EndSl;
           If isError = *off;
           SCrcontext = %Trim(strContext);
           intRRNr +=1;
           Write Sflrecent;
           Else;
              delete RCrrn prc0p;
           Endif;
         Enddo;
       EndSr;

       //---------------------------------------------------------------------
       //  Check if Group access for Inquiry mode only.
       //---------------------------------------------------------------------
       BegSr ChkGrpAcs;

       isGrpAuth = ' ';
       If charGrpAccess = 'I' and RCmode <> 'D';
          scRcMode = 'Inquiry';
          RCmode   = 'D';
       Endif;

       EndSr;

       //---------------------------------------------------------------------
       //  PopUp Window
       //---------------------------------------------------------------------
       BegSr PopUp;

       // If user not authorized to group, display popup
       If isGrpAuth = 'N';
          PopOneBtn('You do not have authority to access this item.'
           :'Error');
       Endif;

       EndSr;

       //*******************************************************************************************
       //  ProcessQuote URL
       //*******************************************************************************************
       BegSr ProcessQuote;
         Chain (empCo#:empFund:employer:div) pem00q;
         If %found(WMEMLQ);
         strContext = %Trim(PMname);
         Clear ELrrnq;
         Setgt (PMCo#:PMFnd:PMEmp#:PMDiv) pel00q;
         Readpe (PMCo#:PMFnd:PMEmp#:PMDiv) pel00q;
         intRrn = ELrrnq;
         strCallPgm = conPolicyPgm;
         scRcType = conQuote;
         strRRNp=%Editc(intRrn:'X');
         If GroupAuth(empCo#:empFund:q1User:charGrpAccess) = '1';
            Exsr chkGrpAcs;
            scRcUrl = CreateQteURL('STDRVPGM':strContext:strRRNp:RCmode
             :strCallPgm:'00000':'Q');
         Endif;
         Else;
            isError = *on;
         Endif;
       EndSr;

       //*******************************************************************************************
       //  ProcessLive URL
       //*******************************************************************************************
       BegSr ProcessLive;
         overrideEmp(LiveFlag);
         Chain (empCo#:empFund:employer:div) pem00 dsWMEML;
         If %found(WMEML);
         strContext = %Trim(dsWMEML.EMname);
         Clear ELrrn;
         Setgt (dsWMEML.EMco#:dsWMEML.EMfnd:dsWMEML.EMemp#:dsWMEML.EMdiv) pel00;
         Readpe (dsWMEML.EMco#:dsWMEML.EMfnd:
                 dsWMEML.EMemp#:dsWMEML.EMdiv) pel00;
         intRrn = ELrrn;
         strCallPgm = conPolicyPgm;
         scRcType = conPolicy;
         strRRNp=%Editc(intRrn:'X');
         If GroupAuth(empCo#:empFund:q1User:charGrpAccess) = '1';
            Exsr chkGrpAcs;
            scRcUrl = CreateURL('STDRVPGM':strContext:strRRNp:RCmode
             :strCallPgm);
         Endif;
         Else;
            isError = *on;
         Endif;
       EndSr;

       //================================================================== ************************
       //  Restart this program and end
       //================================================================== ************************
       Begsr RestartThis;
         RtvObjLib('MLIVXA':'*DTAARA':strLiveLib);
       //AtriumLaunch('STUSRDRY':'LIV':'()*End');
         If %subst(strLiveLib:1:5) = 'CLTID';
            In envDataArea;
            PopOneBtn('Your environment is set as "' +
             %trim(scCurEnv) +
             '". In order for the home page to display information from +
             this environment it must be opened from the menu.':
             'Environment Exists');
            *inlr = *on;
            Return;
         Endif;
       Endsr;

       //*******************************************************************************************
       //  ClearRecentSubfile Subroutine - To Clear subfile
       //*******************************************************************************************
       BegSr ClearRecentSubFile;
         clrRecent = *On;
         dspRecent = *Off;
         Write rcdHome;
         intRRNr = 0;
       EndSr;

       //*******************************************************************************************
       //  LoadChartData      Subroutine
       //*******************************************************************************************
       BegSr LoadChartData;
         asgnNames = 'My Assignments, My Team''s Assignments';
         asgnValues = %Char(myAsgnCount) + ', ' + %Char(myTmAsgnCount);
         diaryNames = 'Due Future, Due Today, Due Last 30 days,' +
          '31 - 60 days, 61 - 90 days, Over 90 days';

         diaryValue = %Char(futureCount) + ', ' +
          %Char(todayCount) + ', ' +
          %Char(day30Count) + ', ' +
          %Char(day60Count) + ', ' +
          %Char(day90Count) + ', ' +
          %Char(over90Count);
       EndSr;

       //*******************************************************************************************
       //  GetDiaryCounts     Subroutine
       //*******************************************************************************************
       BegSr GetDiaryCounts;
         dateToday = DateTo7(SystemDate());
         futureCount += GetDartCount(strCurUsr:dateToday:'>');
         todayCount += GetDartCount(strCurUsr:dateToday:'=');

         dateT30 = DateTo7(SystemDate() - %Days(30));
         day30Count += GetDartCount(strCurUsr:dateT30:' ':dateToday);

         dateT60 = DateTo7(SystemDate() - %Days(60));
         day60Count += GetDartCount(strCurUsr:dateT60:' ':dateT30);

         dateT90 = DateTo7(SystemDate() - %Days(90));
         day90Count += GetDartCount(strCurUsr:dateT90:' ':dateT60);
         over90Count += GetDartCount(strCurUsr:dateT90:'<');
       EndSr;

       //*******************************************************************************************
       //  GetVisitCounts     Subroutine
       //*******************************************************************************************
       BegSr GetVisitCounts;

         dateToday = DateTo7(SystemDate());
         futureCountV += GetVisitCount(strCurUsr:dateToday:'>');
         todayCountV += GetVisitCount(strCurUsr:dateToday:'=');

         dateT30 = DateTo7(SystemDate() - %Days(30));
         day30CountV += GetVisitCount(strCurUsr:dateT30:' ':dateToday);

         dateT60 = DateTo7(SystemDate() - %Days(60));
         day60CountV += GetVisitCount(strCurUsr:dateT60:' ':dateT30);

         dateT90 = DateTo7(SystemDate() - %Days(90));
         day90CountV += GetVisitCount(strCurUsr:dateT90:' ':dateT60);
         over90CountV += GetVisitCount(strCurUsr:dateT90:'<');

       EndSr;

       //*******************************************************************************************
       //  LoadWorkLoadData   Subroutine
       //*******************************************************************************************
       BegSr LoadWorkLoadData;
         scAlertCnt = todayCount + day30Count + day60Count +
          day90Count + over90Count;
         scDiaryCnt = futureCount;
         scTotalCnt = scAlertCnt + scDiaryCnt;
         scImageCnt = GetImageCounts(strUser:'U');
         scShImgCnt = GetImageCounts(strUser:'S');
         scVstAlCnt = todayCountV + day30CountV + day60CountV +
          day90CountV + over90CountV;
         scVisitCnt = futureCountV;
         scVsTtlCnt = scVstAlCnt + scVisitCnt;
       EndSr;
      /End-free

       //*******************************************************************************************
       //  PopulateAsgnSfl - Populate Assignment subfile based on "My Work" or "My Team Work" radio
       //                    button selection
       //*******************************************************************************************
      /Free
       BegSr PopulateAsgnSfl;

         Select;

         // When user press "MY Work" radio button
         When scmywork = conmywork;
            If strCurUsr = strUser;
              intRRNa +=1;
              Write SflAssign;
              myAsgnCount += 1;
            Else;
              myTmAsgnCount += 1;
            Endif;

         // When user press "MY Team Work" radio button
         When scmywork = conTeamWork;
            intRRNa +=1;
            Write SflAssign;
            If strCurUsr = strUser;
              myAsgnCount += 1;
            Else;
              myTmAsgnCount += 1;
            Endif;
         Other;
             // Do Nothing
         EndSl;

       EndSr;
      /End-free




     P*====================================================================
     P* Extract values from JSON
     P*====================================================================
     PExtractJson      b
     DExtractJson      pi          5000
     D inTag                        100    const
     D inJSON                     32767    const

     DintEnd           s              5  0
     DintLen           s              5  0
     DintStart         s              5  0
      /Free
       intStart = %scan(%trim(inTag):%trim(inJSON):1) + %len(%trim(inTag))+2;
       // Tag not found
       If intStart = 0;
          Return *blanks;
       Endif;
       intStart = %scan('"':%trim(inJSON):intStart) + 1;
       intEnd = %scan('"':%trim(inJSON):intStart) - 1;
       intLen = intEnd - intStart + 1;
       Return %subst(%trim(inJSON):intStart:intLen);
      /End-free
     PExtractJson      e

     P*====================================================================
     P* Load grids from the SQL cursor
     P*====================================================================
     PLoadGrid         b
     DLoadGrid         pi           115
     D inGrid#                        5  0 const
     D inJSON                     32767    const

     DstrSqlCursor     s           5000
     DsqlIndex         s              5  0 inz
     Dindex            s              5  0 inz
     D#ofCol           s              2  0
     DconEmpty         c                   'Empty          '
     DdecColWidth      s              5  0
     DdecMonies        s             17  2
     D***  Beg Chg  ***  12/26/16 *************************
     D*****decPercents      s              7  2
     DdecPercents      s              9  2
     D***  End Chg  ***  12/26/16 **************************
     DposEnd           s              5  0
     DposStr           s              5  0
     DflgInqUpd        s              1    inz('C')
     DflgQuote         s              1    inz
     DintGrid#         s              5  0
     DintRRN           s              9  0
     DstrLnkPgm        s             10
     DstrKey           s             40
     DstrRRN           s              9
     DstrRtnVal        s            100    inz
     DstrColWidth      s           1000
     DptrSflLnk        s               *   inz(%addr(isSflLnk))
     D strSflLnk       s             10    based(ptrSflLnk)
     DptrSflDt         s               *   inz(%addr(isSflDt))
     D strSflDt        s             10    based(ptrSflDt)
     DptrSfl$          s               *   inz(%addr(isSfl$))
     D strSfl$         s             10    based(ptrSfl$)
     DptrSflPer        s               *   inz(%addr(isSflPer))
     D strSflPer       s             10    based(ptrSflPer)
     DptrSflJustify    s               *   inz(%addr(arySflJustify))
     D strSflJustify   s             10    based(ptrSflJustify)

     D dsData1         ds                  inz qualified
     D  field1                      250
     D  rrn                          15  0
     D  key                          40

     D dsData2         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  rrn                          15  0
     D  key                          40

     D dsData3         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  rrn                          15  0
     D  key                          40

     D dsData4         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  field4                      250
     D  rrn                          15  0
     D  key                          40

     D dsData5         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  field4                      250
     D  field5                      250
     D  rrn                          15  0
     D  key                          40

     D dsData6         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  field4                      250
     D  field5                      250
     D  field6                      250
     D  rrn                          15  0
     D  key                          40

     D dsData7         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  field4                      250
     D  field5                      250
     D  field6                      250
     D  field7                      250
     D  rrn                          15  0
     D  key                          40

     D dsData8         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  field4                      250
     D  field5                      250
     D  field6                      250
     D  field7                      250
     D  field8                      250
     D  rrn                          15  0
     D  key                          40

     D dsData9         ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  field4                      250
     D  field5                      250
     D  field6                      250
     D  field7                      250
     D  field8                      250
     D  field9                      250
     D  rrn                          15  0
     D  key                          40

     D dsData10        ds                  inz qualified
     D  field1                      250
     D  field2                      250
     D  field3                      250
     D  field4                      250
     D  field5                      250
     D  field6                      250
     D  field7                      250
     D  field8                      250
     D  field9                      250
     D  field10                     250
     D  rrn                          15  0
     D  key                          40

     D dsKeyClm        ds                  inz
     D  clmCo#                        3  0
     D  clmFnd                        3  0
     D  clmFyr                        3  0
     D  clmCase                       7  0
     D  clmEmp#                       9  0
     D  clmDiv                        5  0

     D dsKeyEmp        ds                  inz
     D  empCo#                        3  0
     D  empFnd                        3  0
     D  empEmp#                       9  0
     D  empDiv                        5  0

      /Free
       Select;
       When scItemType = 'Policy';
          strLnkPgm = 'WTPOLSUM';
       When scItemType = 'Claim';
          strLnkPgm = 'WTCLMSUM';
       Endsl;
       intGrid# = inGrid#;
       strSqlCursor = ExtractJson('sql':inJSON);
       If strSqlCursor = *blanks or strSqlCursor = ':';
          Return *off;
       Endif;

      ***  Beg Add  ***  07/16/24  *** WC2972 ************************
       strSqlCursor = %ScanRpl('\':' ':strSqlCursor);
      ***  End Add  ***  07/16/24  *** WC2972 ************************

       #ofCol = %dec(ExtractJson('gridCols':inJSON):2:0);
       flgQuote = ExtractJson('quoteFlag':inJSON);
       strColWidth   = ExtractJson('gridColWidths':inJSON);
       strSflLnk     = ExtractJson('gridLinks':inJSON);
       strSflDt      = ExtractJson('gridDates':inJSON);
       strSfl$       = ExtractJson('gridMonies':inJSON);
       strSflPer     = ExtractJson('gridPercents':inJSON);
       strSflJustify = ExtractJson('gridJustify':inJSON);
       #ofCol -= 1;
       posStr = 1;
       posEnd = 0;
       For index = 1 to #ofCol;
        //scSflLft(index) = '5px';
          scSflLft(index) = '5';
          posStr = posEnd+1;
          posEnd = %scan(',':strColWidth:posStr);
          If posEnd = 0;
             posEnd = %scan(' ':strColWidth:posStr);
          Endif;
          decColWidth = %dec(%subst(strColWidth:posStr:posEnd-posStr):5:0);
        //scSflWth(index) = %trim(%editc(decColWidth-10:'Z')) + 'px';
          scSflWth(index) = %trim(%editc(decColWidth-10:'Z'));
          Select;
          When arySflJustify(index) = 'R';
             scSflJst(index) = 'right';
          When arySflJustify(index) = 'C';
             scSflJst(index) = 'center';
          Other;
             scSflJst(index) = 'left';
          Endsl;
          Select;
          When isSflLnk(index) = *on;
             isSflOut(index) = *off;
NP01    //When isSflDt(index) = *on;
NP01    //   isSflOut(index) = *off;
          Other;
             isSflOut(index) = *on;
          Endsl;
       Endfor;

       Exsr ClearGrid;

       Exec SQL Prepare sqlGrid From :strSqlCursor;
       If SQLSTT = '00000';
          Exec SQL Declare cursorGrid Cursor For sqlGrid;
          Exec SQL Open cursorGrid;
          Dou SQLSTT <> '00000';
             Exsr FetchGrid;
             Select;
             // User not authorized
             When flgInqUpd = '*';
             // No more records
             When SQLSTT = '02000' and sqlIndex = 0;
                strRtnVal = ExtractJson('emptyGridHdg':inJSON);
             // Record found
             When SQLSTT = '00000';
                sqlIndex += 1;
                If sqlIndex > 9999;
                   Leave;
                Endif;
                Exsr LoadGrid;
                intRRNg = sqlIndex;
                Exsr WriteGrid;
             Endsl;
          Enddo;
       Endif;
       Exec SQL Close cursorGrid;

       Select;
       When strRtnVal <> *blank;
       When sqlIndex <= 0;
          strRtnVal = conEmpty + 'Grid Data Not Loaded - ' + %trim(DBproc);
       Other;
          strRtnVal = 'Loaded';
       Endsl;

       Return strRtnVal;

       //==================================================================
       // Build date(s)
       //==================================================================
       Begsr BuildDates;
        Select;
        When isSflDt(index) = *on;
           scSflDt(index) = %date(%trim(scSflVal(index)):*usa);
           //Clear scSflVal(index);
           scSflVal(index) = %char(scSflDt(index):*iso);
        Endsl;
       Endsr;

       //==================================================================
       // Build link(s)
       //==================================================================
       Begsr BuildLinks;
        Select;
        When isSflLnk(index) = *on;
           Select;
           When flgQuote = 'Q';
              scSflUrl(index) = CreateQteURL('STDRVPGM':
               %subst(scSflVal(index):1:50):%editc(intRRN:'X'):
               flgInqUpd:strLnkPgm:'00000':'Q');
           Other;
              scSflUrl(index) = CreateURL('STDRVPGM':
               %subst(scSflVal(index):1:50):%editc(intRRN:'X'):
               flgInqUpd:strLnkPgm);
           Endsl;
        Endsl;
       Endsr;

       //==================================================================
       // Build money(s)
       //==================================================================
       Begsr BuildMonies;
        Select;
        When isSfl$(index) = *on;
           decMonies = %dec(scSflVal(index):17:2);
           scSflVal(index) = %trimr(%editc(decMonies:'J'));
        Endsl;
       Endsr;

       //==================================================================
       // Build percentage(s)
       //==================================================================
       Begsr BuildPercents;
        Select;
        When isSflPer(index) = *on;
           //***  Beg Chg  ***  12/26/16 ********************************
           //***** decPercents = %dec(scSflVal(index):7:2);
           decPercents = %dec(scSflVal(index):9:2);
           //***  End Chg  ***  12/26/16 ********************************
           scSflVal(index) = %trimr(%editc(decPercents:'J')) + '%';
        Endsl;
       Endsr;

       //==================================================================
       // Check the authority on the record
       //==================================================================
       Begsr CheckAuth;
        Select;
        When scItemType = 'Policy';
           dsKeyEmp = strKey;
           flgInqUpd = InqUpdAccessEmp(empCo#:empFnd:empEmp#:empDiv);
        When scItemType = 'Claim';
           dsKeyClm = strKey;
           flgInqUpd = InqUpdAccessClm(clmCo#:clmFnd:clmFyr:clmCase:
            clmEmp#:clmDiv);
        Endsl;
       Endsr;

       //==================================================================
       // Clear the correspoding grid
       //==================================================================
       Begsr ClearGrid;
        Clear scSflVal;
        Clear scSflUrl;
        Clear scSflDt;
        intRRNg = 0;
        scGrdDsp(intGrid#) = *off;
        scGrdCtl(intGrid#) = *off;
        scGrdClr(intGrid#) = *on;
        Write rcdHome;
        scGrdDsp(intGrid#) = *on;
        scGrdCtl(intGrid#) = *on;
        scGrdClr(intGrid#) = *off;
       Endsr;

       //==================================================================
       // Fetch the next record
       //==================================================================
       Begsr FetchGrid;
        Select;
        When #ofCol = 1;
           Exec SQL Fetch next From cursorGrid into :dsData1;
           strKey = dsData1.key;
        When #ofCol = 2;
           Exec SQL Fetch next From cursorGrid into :dsData2;
           strKey = dsData2.key;
        When #ofCol = 3;
           Exec SQL Fetch next From cursorGrid into :dsData3;
           strKey = dsData3.key;
        When #ofCol = 4;
           Exec SQL Fetch next From cursorGrid into :dsData4;
           strKey = dsData4.key;
        When #ofCol = 5;
           Exec SQL Fetch next From cursorGrid into :dsData5;
           strKey = dsData5.key;
        When #ofCol = 6;
           Exec SQL Fetch next From cursorGrid into :dsData6;
           strKey = dsData6.key;
        When #ofCol = 7;
           Exec SQL Fetch next From cursorGrid into :dsData7;
           strKey = dsData7.key;
        When #ofCol = 8;
           Exec SQL Fetch next From cursorGrid into :dsData8;
           strKey = dsData8.key;
        When #ofCol = 9;
           Exec SQL Fetch next From cursorGrid into :dsData9;
           strKey = dsData9.key;
        When #ofCol = 10;
           Exec SQL Fetch next From cursorGrid into :dsData10;
           strKey = dsData10.key;
        Endsl;
        Clear flgInqUpd;
        If SQLSTT = '00000';
           Exsr CheckAuth;
        Endif;
       Endsr;

       //==================================================================
       // Load the correspoding grid data
       //==================================================================
       Begsr LoadGrid;
        Select;
        When #ofCol = 1;
           scSflVal1  = %trim(dsData1.field1);
           intRRN = dsData1.rrn;
        When #ofCol = 2;
           scSflVal1  = %trim(dsData2.field1);
           scSflVal2  = %trim(dsData2.field2);
           intRRN = dsData2.rrn;
        When #ofCol = 3;
           scSflVal1  = %trim(dsData3.field1);
           scSflVal2  = %trim(dsData3.field2);
           scSflVal3  = %trim(dsData3.field3);
           intRRN = dsData3.rrn;
        When #ofCol = 4;
           scSflVal1  = %trim(dsData4.field1);
           scSflVal2  = %trim(dsData4.field2);
           scSflVal3  = %trim(dsData4.field3);
           scSflVal4  = %trim(dsData4.field4);
           intRRN = dsData4.rrn;
        When #ofCol = 5;
           scSflVal1  = %trim(dsData5.field1);
           scSflVal2  = %trim(dsData5.field2);
           scSflVal3  = %trim(dsData5.field3);
           scSflVal4  = %trim(dsData5.field4);
           scSflVal5  = %trim(dsData5.field5);
           intRRN = dsData5.rrn;
        When #ofCol = 6;
           scSflVal1  = %trim(dsData6.field1);
           scSflVal2  = %trim(dsData6.field2);
           scSflVal3  = %trim(dsData6.field3);
           scSflVal4  = %trim(dsData6.field4);
           scSflVal5  = %trim(dsData6.field5);
           scSflVal6  = %trim(dsData6.field6);
           intRRN = dsData6.rrn;
        When #ofCol = 7;
           scSflVal1  = %trim(dsData7.field1);
           scSflVal2  = %trim(dsData7.field2);
           scSflVal3  = %trim(dsData7.field3);
           scSflVal4  = %trim(dsData7.field4);
           scSflVal5  = %trim(dsData7.field5);
           scSflVal6  = %trim(dsData7.field6);
           scSflVal7  = %trim(dsData7.field7);
           intRRN = dsData7.rrn;
        When #ofCol = 8;
           scSflVal1  = %trim(dsData8.field1);
           scSflVal2  = %trim(dsData8.field2);
           scSflVal3  = %trim(dsData8.field3);
           scSflVal4  = %trim(dsData8.field4);
           scSflVal5  = %trim(dsData8.field5);
           scSflVal6  = %trim(dsData8.field6);
           scSflVal7  = %trim(dsData8.field7);
           scSflVal8  = %trim(dsData8.field8);
           intRRN = dsData8.rrn;
        When #ofCol = 9;
           scSflVal1  = %trim(dsData9.field1);
           scSflVal2  = %trim(dsData9.field2);
           scSflVal3  = %trim(dsData9.field3);
           scSflVal4  = %trim(dsData9.field4);
           scSflVal5  = %trim(dsData9.field5);
           scSflVal6  = %trim(dsData9.field6);
           scSflVal7  = %trim(dsData9.field7);
           scSflVal8  = %trim(dsData9.field8);
           scSflVal9  = %trim(dsData9.field9);
           intRRN = dsData9.rrn;
        When #ofCol = 10;
           scSflVal1  = %trim(dsData10.field1);
           scSflVal2  = %trim(dsData10.field2);
           scSflVal3  = %trim(dsData10.field3);
           scSflVal4  = %trim(dsData10.field4);
           scSflVal5  = %trim(dsData10.field5);
           scSflVal6  = %trim(dsData10.field6);
           scSflVal7  = %trim(dsData10.field7);
           scSflVal8  = %trim(dsData10.field8);
           scSflVal9  = %trim(dsData10.field9);
           scSflVal10 = %trim(dsData10.field10);
           intRRN = dsData10.rrn;
        Endsl;

        // Format cells
        For index = 1 to #ofCol;
           Exsr BuildLinks;
NP01     //Exsr BuildDates;
           Exsr BuildMonies;
           Exsr BuildPercents;
        Endfor;
       Endsr;

       //==================================================================
       // Write to the correspoding grid
       //==================================================================
       Begsr WriteGrid;
        scSflTop = '5px';
NP01    // scSflTop = '25px';
        Select;
        When intGrid# = 1;
           Write SflItem1;
        When intGrid# = 2;
           Write SflItem2;
        When intGrid# = 3;
           Write SflItem3;
        When intGrid# = 4;
           Write SflItem4;
        When intGrid# = 5;
           Write SflItem5;
        When intGrid# = 6;
           Write SflItem6;
        When intGrid# = 7;
           Write SflItem7;
        When intGrid# = 8;
           Write SflItem8;
        When intGrid# = 9;
           Write SflItem9;
        When intGrid# = 10;
           Write SflItem10;
        When intGrid# = 11;
           Write SflItem11;
        When intGrid# = 12;
           Write SflItem12;
        When intGrid# = 13;
           Write SflItem13;
        When intGrid# = 14;
           Write SflItem14;
        When intGrid# = 15;
           Write SflItem15;
        When intGrid# = 16;
           Write SflItem16;
        Endsl;
       Endsr;

      /End-free
     PLoadGrid         e





     P*====================================================================
     P* Reset the environment
     P*====================================================================
     PResetEnv         b
     DResetEnv         pi
      /Free
       Callp(e) RunCmd('DLTOBJ OBJ(' + %trim(scCltIdLib) + '/MLIVXA) +
        OBJTYPE(*DTAARA)':80);
       Callp(e) RunCmd('DLTOBJ OBJ(' + %trim(scCltIdLib) + '/MQTEXA) +
        OBJTYPE(*DTAARA)':80);
      /End-free
     PResetEnv         e
     P
       //===================================================================
       // * General Visit Tracking Procedures
       //===================================================================
      /Copy wtVstTrkSP
** Hover Text - 11/13/13
These buttons are disabled because you are not set up as a leader on any team.
You are setup as a leader on a team.
