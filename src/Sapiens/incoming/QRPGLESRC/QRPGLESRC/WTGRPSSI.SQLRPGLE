     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h/title WTGRPSSI - State Specific Information
     h dftname(WTGRPSSI)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/22/20                                         *
      //  ProgERRSVPV110300                                            *
      //  Project   : WC2196- Redesign of Group Configuration program  *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      // 04/01/25 C:300  Support: 118767 (Prescient)                   *
      //                 Add Filter processing to Small Deductible Grid*
      //                 on rcdAddPrm recod format in DSPF             *
      //                                                               *
      // 09/24/24 C:079  Support: 99/117066                            *
      //                 Dont require both S/R and Estimated Inst Fee  *
      //                                                               *
      // 09/18/24 C:333  Support: 117657 (Prescient)                   *
      //                 Group Maintenance- Harbor Exposure Tables     *
      //                 - DSPF Changes:                               *
      //                  - Grid fields aligments are not correct for  *
      //                    sflHrEx and sflLsCn.                       *
      //                                                               *
      // 08/23/24 C:344  Support: 117377 (Prescient)                   *
      //                 Added condition and message when attempting   *
      //                 to write +9,999 to the subfile.               *
      //                                                               *
      // 02/19/24 C:333  Project: WC2942                               *
      //                 HI Endorsement WC520602 Schedule Data         *
      //                 - Adding new container 'CntEndInfWC520602'    *
      //                 - rcdEndors and rcdEDCDtl, onload changes.    *
      //                                                               *
      // 02/14/24 C:333  Project: WC2957                               *
      //                 Suppress WC99* Forms for WCPOLS by State      *
      //                 - Adding checkbox scExWc99E.                  *
      //                 - Adding Detailbox CbxDtlExcWC99*.            *
      //                                                               *
      // 12/27/23 C:342  Project: WC2932                               *
      //                 1.Added Copy option hyperlink for below grids.*
      //                   1) Small Deductibles                        *
      //                   2) Premium Discount                         *
      //                   3) Increased Limits                         *
      //                                                               *
      // 11/13/23 C:340  Project: WC2929                               *
      //                 Add AZ Endorsements ? WC020603A, WC020604     *
      //                                                               *
      //  08/28/23 C:300 Support: 113191                               *
      //                 Action Hyperlinks like Activate/Inactivate    *
      //                 should not be included as last column of excel*
      //                 spreadsheet export                            *
      //                 DSPF: Removed 3 hyperlinks for scActivate,    *
      //                 scActSmDe and scActEDC, then added 6 new      *
      //                 hyperlinks for Activate/Inactivate            *
      //                 on Increased Limits, Small Deductibles and    *
      //                 Endorsements Grids
      //                                                               *
      //  08/09/23 C:300 Support: 113471                               *
      //                 Find and Filter option No Longer Available    *
      //                 in all subfile grids of this Disply File      *
      //                                                               *
      //  08/07/23 C:300 Support: 113225                               *
      //                 Allow entry of data in Upper/Lower case in    *
      //                 - Description field on Small Deductible screen*
      //                 - (State Department Name) and (ACCORD Remark 1*
      //                 Default) fields on Cancellation/Reinstatement *
      //                 State Definition screen                       *
      //                                                               *
      //  06/27/23 C:300 Project: WC2910                               *
      //                 Check User Security from Application Configuration
      //                 instead of User Parameters file               *
      //                                                               *
      //  06/23/23 C:300 Project: WC2910, Support: 111868              *
      //                 Display all screens in Inquiry mode, even if  *
      //                 user option security level does not allow them*
      //                 to Edit group information.                    *
      //                                                               *
      // 05/23/23 C:300  Project: WC2910                               *
      //                 - Add User Access Security                    *
      //                                                               *
      // 04/21/23 C:300  Project:W2196 Support:109146                  *
      //                 Rename 2nd Tab "Additional Premium Adjustments"
      //                 to "Rating Factors"                           *
      //                                                               *
      // 04/04/23 C:300  Project:W2196 Support:108882                  *
      //                 State Specific - Endorsements Error           *
      //                 in DSPF: changed function defination in onload*
      //                 property of rcdEndors and rcdEDCDtl rcdformats*
      //                                                               *
      // 11/17/22 C:340  Project: WC2886                               *
      //                 Add AZ Endorsements ? WC020603A, WC020604     *
      //                                                               *
      // 09/26/22 C:342  Support: 101193                               *
      //                 -Removed State Compensation Benefit grid      *
      //                  and added this grid under Recurring          *
      //                  Compensation payment TAB in Claim            *
      //                  Configuration.                               *
      //                 -Rename the program WTGRPSSI09 as WTGRPCLM04  *
      //                 -Rename the program WTGRPSSI10 as WTGRPCLM05  *
      //                                                               *
      // 08/08/22 C:079  Project: WC2859                               *
      //                 Add Death Benefit PV1 Codes                   *
      //                                                               *
      // 01/17/22 C:291  Project: WC2788                               *
      //                 Add "Copy Configuration" option to Group      *
      //                 Toolbar.                                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTGRPSSI  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflStRt:rrnStRt)
     f                                     Sfile(sflPrmDis:rrnPrmDis)
     f                                     Sfile(sflExpCon:rrnExpCon)
     f                                     Sfile(sflIncLmt:rrnIncLmt)
     f                                     Sfile(sflStRC:rrnStRC)
     f                                     Sfile(sflSmDe:rrnSmDe)
     f                                     Sfile(sflHrEx:rrnHrEx)
     f                                     Sfile(sflLsCn:rrnLsCn)
     f                                     Sfile(sflPolPg:rrnPolPg)
     f                                     Sfile(sflExSt:rrnExSt)
     f                                     Sfile(sflInFe:rrnInFe)
     f                                     Sfile(sflCRSD:rrnCRSD)
     f                                     Sfile(sflEDC:rrnEDC)
     f                                     sfile(sflSRT:rrnSRT)
     f                                     sfile(sflSRTDE:rrnSRTDE)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
      * Premium Discount File
     fwmdsp     uf a e             disk    rename(pds01:pds0p)
     f                                     infds(dbfds24)
     fwmdsl1    if   e           k disk    rename(pds01:pds00)
     fwmdsl2    if   e           k disk    rename(pds01:pds02)
     f                                     infds(dbfds)
      * Loss Constant Definition File
     fwdscp     uf a e             disk    rename(psc01:psc0p)  prefix(sc_)
     f                                     infds(dbfds23)
     fwdscl     if   e           k disk    rename(psc01:psc00)  prefix(sc_)
     f                                     infds(dbfds2)
      * State Rate Change File
     fwmrtp     uf a e             disk    rename(prt01:prt0p)
     f                                     infds(dbfds28)
     fwmrtl     if   e           k disk    rename(prt01:prt00)
     f                                     infds(dbfds3)
      * Small Deductible Discount Code File
     fwtddp     uf a e             disk    rename(pdd01:pdd0p)
     f                                     infds(dbfds27)
     fwtddl3    if   e           k disk    rename(pdd01:pdd00)
     f                                     infds(dbfds4)
      * Group Printing Text File (Policy Page - 3c)
     fwdf4p     uf a e             disk    rename(pf401:pf40p)
     fwdf4l     if   e           k disk    rename(pf401:pf400)
     f                                     infds(dbfds5)
      * Excluded States Detail File
     fwdxsp     uf a e             disk    rename(pxs01:pxs0p)
     fwdxsl     if   e           k disk    rename(pxs01:pxs00)
     f                                     infds(dbfds6)
      * Installment Fee Info File
     fwmifp     uf a e             disk    rename(pif01:pif0p)
     f                                     infds(dbfds21)
     fwmifl1    if   e           k disk    rename(pif01:pif00)
     f                                     infds(dbfds7)
      * Expense Constant Definition File
     fwdecp     uf a e             disk    rename(pec01:pec0p)
     f                                     infds(dbfds19)
     fwdecl1    if   e           k disk    rename(pec01:pec00)
     f                                     infds(dbfds8)
      * Incresed Limit Code File
     fwtlmp     uf a e             disk    rename(plm01:plm0p)
     f                                     infds(dbfds22)
     fwtlml2    if   e           k disk    rename(plm01:plm00)
     f                                     infds(dbfds9)
      * Harbor Exposure File
     fwdhep     uf a e             disk    rename(phe01:phe0p)
     f                                     infds(dbfds20)
     fwdhel1    if   e           k disk    rename(phe01:phe00)
     f                                     infds(dbfds10)
      * Statistical Report/Rating File
     fwdrrp     uf a e             disk    rename(prr01:prr0p)
     fwdrrl1    if   e           k disk    rename(prr01:prr00)
     f                                     infds(dbfds11)
      * Cancellation Maintenance File
     fwtxcp     uf a e             disk    rename(pxc01:pxc0p)
     f                                     infds(dbfds17)
     fwtxcl1    if   e           k disk    rename(pxc01:pxc00)
     f                                     infds(dbfds12)
      * Endorsement (Controls) Forms Statistical Info File
     fwmnxp     uf a e             disk    rename(pnx01:pnx0p)
     f                                     infds(dbfds18)
     fwmnxl1    if   e           k disk    rename(pnx01:pnx00)
     f                                     infds(dbfds14)
      * Short Rate Master File
     fwmstp     uf a e             disk    rename(pst01:pst0p) prefix(m_)
     fwmstl     if   e           k disk    rename(pst01:pst00) prefix(m_)
     f                                     infds(dbfds15)
      * Short Rate Detail File
     fwdstp     uf a e             disk    rename(pst01:pst0p2)
     f                                     infds(dbfds26)
     fwdstl     uf   e           k disk    rename(pst01:pst02)
     f                                     infds(dbfds16)
      * State Code File
     fststl3    if   e           k disk
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
     f*** Beg Add *** 08/08/22 **********************************************
     fwtabl     if   e           k disk
     f                                     ignore(prt01)
     f*** End Add *** 08/08/22 **********************************************
     f*** Beg Add *** 05/23/23 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/23/23 *************************************************
     f*** Beg Add *** 02/14/24 *************************************************
     fwtgtl     if   e           k disk
     f*** End Add *** 02/14/24 *************************************************

     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  dbrrn                397    400b 0

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

     d dbfds10         ds
     d  dbrrn10              397    400b 0

     d dbfds11         ds
     d  dbrrn11              397    400b 0

     d dbfds12         ds
     d  dbrrn12              397    400b 0

     d dbfds14         ds
     d  dbrrn14              397    400b 0

     d dbfds15         ds
     d  dbrrn15              397    400b 0

     d dbfds16         ds
     d  dbrrn16              397    400b 0

     d dbfds17         ds
     d  dbrrn17              397    400b 0

     d dbfds18         ds
     d  dbrrn18              397    400b 0

     d dbfds19         ds
     d  dbrrn19              397    400b 0

     d dbfds20         ds
     d  dbrrn20              397    400b 0

     d dbfds21         ds
     d  dbrrn21              397    400b 0

     d dbfds22         ds
     d  dbrrn22              397    400b 0

     d dbfds23         ds
     d  dbrrn23              397    400b 0

     d dbfds24         ds
     d  dbrrn24              397    400b 0

     d dbfds26         ds
     d  dbrrn26              397    400b 0

     d dbfds27         ds
     d  dbrrn27              397    400b 0

     d dbfds28         ds
     d  dbrrn28              397    400b 0


      // work variables
     d saveDT          s                   like(FNdt)
     d saveTM          s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d saveDT3         s                   like(FNdt)
     d saveTM3         s                   like(FNtime)
     d saveDT4         s                   like(FNdt)
     d saveTM4         s                   like(FNtime)
     d saveDT5         s                   like(FNdt)
     d saveTM5         s                   like(FNtime)
     d saveDT6         s                   like(FNdt)
     d saveTM6         s                   like(FNtime)
     d saveDT7         s                   like(FNdt)
     d saveTM7         s                   like(FNtime)
     d saveDT8         s                   like(FNdt)
     d saveTM8         s                   like(FNtime)
     d saveDT9         s                   like(FNdt)
     d saveTM9         s                   like(FNtime)
     d saveDT10        s                   like(FNdt)
     d saveTM10        s                   like(FNtime)
     d saveDT11        s                   like(FNdt)
     d saveTM11        s                   like(FNtime)
     d saveDT12        s                   like(FNdt)
     d saveTM12        s                   like(FNtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inFnd           s              3  0
     D indexTab        s              5  0
     d svrrnPrmDis     s              9  0 inz
     d svrrnExpCon     s              9  0 inz
     d svrrnIncLmt     s              9  0 inz
     d svrrnLsCn       s              9  0 inz
     d svrrnStRC       s              9  0 inz
     d svrrnStRt       s              9  0 inz
     d svrrnSmDe       s              9  0 inz
     d svrrnHrEx       s              9  0 inz
     d svrrnInFe       s              9  0 inz
     d svrrnCRSD       s              9  0 inz
     d svrrnEDC        s              9  0 inz
     d svrrnSRT        s              9  0 inz
     d svrrnSRTDE      s              9  0 inz
     D xxRrn           s              9  0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d cpyMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d KeyEfDt         s                   like(DSefdt)inz
     d keyEmp#         s                   like(XSemp#)inz
     d KeyFyr          s                   like(XSfyr)inz
     d charMsg         s            100
     d isDelResp       s               n   inz
     d xxchid          s              4    inz
     d keyPolPg        s             10    inz
     d keyExSt         s             10    inz
     d keyIncLmt       s             10    inz
     d isFl03          s               n   inz
     d i               s              2  0 inz
     d j               s              2  0 inz
     d strUrlRRN       s              9    inz
     d strUrlTabName   s            100    inz
     d outMode         s              1
     d outNext         s             10    inz
     d outTabIndex     s              5    inz
     d processTypeC    c                   const('Cancellations')
     d processTypeR    c                   const('Reinstatements')
     d outDoc          s             12    inz
     d outFld          s             80    inz
     d outFold         s             40    inz
     d outRtcd         s              1    inz
      *
     d TotalSfRcd      s              9  0 inz
     d Counter         s              9  0 inz
     d*** Beg Add *** 08/08/22 ************************************************
       dcl-s xxKey Like(B2key);
     d*** End Add *** 08/08/22 ************************************************
      *** Beg Add *** 05/23/23 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
      *
     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
      *** End Add *** 05/23/23 *************************************************
      *** Begin Add ***** 04/01/25 *****************************
     d str9999Msg      s            256
     d sav_dspClrFltr  s                   like(dspClrFltr)
      // Filter screen variable
     d meetFltr        s               n
     d dsFilter        ds                  likerec(rcdSmDFltr) Inz
      *** End   Add ***** 04/01/25 *****************************
      *
     d* DS for defining MINPRM Data Area (for Add/Edit Increased Limit)
     d usrdat          ds            20
     d  usrID                        10
     d  runDate                      10
     d*
     d* Screen Deductible Reserves
     d*
     d                 ds
     d dsScDedRsv              1     20    dim(10)
     d  scDc01E                1      2
     d  scDc02E                3      4
     d  scDc03E                5      6
     d  scDc04E                7      8
     d  scDc05E                9     10
     d  scDc06E               11     12
     d  scDc07E               13     14
     d  scDc08E               15     16
     d  scDc09E               17     18
     d  scDc10E               19     20
     d*
     d* Deductible Reserves Error Indicator
     d*
     d                 ds
     d dsErDedRsv              1     10    dim(10)
     d  erDc01E                1      1
     d  erDc02E                2      2
     d  erDc03E                3      3
     d  erDc04E                4      4
     d  erDc05E                5      5
     d  erDc06E                6      6
     d  erDc07E                7      7
     d  erDc08E                8      8
     d  erDc09E                9      9
     d  erDc10E               10     10
     d*
     d* Deductible Reserves Message Id
     d*
     d                 ds
     d dsMsDedRsv              1    100    dim(10)
     d  msDc01E                1     10
     d  msDc02E               11     20
     d  msDc03E               21     30
     d  msDc04E               31     40
     d  msDc05E               41     50
     d  msDc06E               51     60
     d  msDc07E               61     70
     d  msDc08E               71     80
     d  msDc09E               81     90
     d  msDc10E               91    100
     d*** Beg Add *** 08/08/22 ************************************************
     d*
     d* Deductible Payment Value 1 Error Indicator
     d*
     d                 ds
     d dsErDbPv1               1     10    dim(10)
     d  erDp01E                1      1
     d  erDp02E                2      2
     d  erDp03E                3      3
     d  erDp04E                4      4
     d  erDp05E                5      5
     d  erDp06E                6      6
     d  erDp07E                7      7
     d  erDp08E                8      8
     d  erDp09E                9      9
     d  erDp10E               10     10
     d*
     d* Screen Deductible Reserves
     d*
     d                 ds
     d dsScDbPv1               1     20    dim(10)
     d  scPv1Db01              1      2
     d  scPv1Db02              3      4
     d  scPv1Db03              5      6
     d  scPv1Db04              7      8
     d  scPv1Db05              9     10
     d  scPv1Db06             11     12
     d  scPv1Db07             13     14
     d  scPv1Db08             15     16
     d  scPv1Db09             17     18
     d  scPv1Db10             19     20
     d*** End Add *** 08/08/22 ************************************************
      //
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
      //

      // Proto-types

     d CallRatInfo     pr                  ExtPgm('WTGRPSTRAT')
     d @@Rrn                          9
     d @@Mode                         1
     d @@Next                        10
     d @@Tabindex                     5

      * Check Document
     dChkDoc           pr                  extpgm('SVALDOC')
     d outDoc                              like(outDoc)
     d outFld                              like(outFld)
     d outRtcd                             like(outRtcd)

      * Check Folder
     dChkFolder        pr                  extpgm('SVALFLR')
     d outFold                             like(outFold)
     d outRtcd                             like(outRtcd)

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      * Initialize the Screen fields for Additional Premium Adjustments Tab
      * or "Rating Factors" Tab
     d InzAddPrmScVal  pr

      * Initialize the Screen fields for Cancellation/Reinstatement State Definition
     d InzCRSDScVal    pr

      * Initialize the Screen fields for Rating Tab
     d InzRatingScVal  pr

      // Load Cancellation/Reinstatement State Definition Grid
     d LoadCRSDGrid    pr

      // Load Endorsement Controls Grid
     d LoadEDCGrid     pr

      // Load Expense Constant Grid
     d LoadExCoGrid    pr

      // Load Excluded States Grid
     d LoadExStGrid    pr

      // Load Harbor Exposure Grid
     d LoadHrExGrid    pr

      // Load Installment Fees Grid
     d LoadInFeGrid    pr

      // Load Increased Limit Grid
     d LoadInLtGrid    pr

      // Load Loss Constant Grid
     d LoadLsCnGrid    pr

      // Load Policy Page - 3c Grid
     d LoadPolPgGrid   pr

      // Load Premium Discount Grid
     d LoadPrmDisGrid  pr

      // Load Short Rate Master Grid
     d LoadShRtGrid    pr

      // Load Short Rate Detail Grid
     d LoadShRtDtGrid  pr

      // Load Small Deductible Grid
     d LoadSmDeGrid    pr

      // Load State Rate Change Grid
     d LoadStRCGrid    pr

      // Load Statistical Report/Rating Grid
     d LoadStRtGrid    pr

      * Main Processing
     d Main            pr

      *** Begin Add ***** 04/01/25 *****************************
      // Meet Filter for Small Deductibles Grid
     d MeetFlt         pr
      *** End   Add ***** 04/01/25 *****************************

      * Process Add Cancellation/Reinstatement State Definition Grid
     d ProcAddCRSD     pr

      * Process Add Endorsement Controls Info
     d ProcAddEDC      pr

      * Process Add Expense Constant information
     d ProcAddExCo     pr

      * Process Add Excluded States Information
     d ProcAddExSt     pr

      * Process Add Harbor Exposure Information
     d ProcAddHrEx     pr

      * Process Add Installment Fees Information
     d ProcAddInFe     pr

      * Process Add Increased Limit information
     d ProcAddInLt     pr

      * Process Add Loss Constant Information
     d ProcAddLsCn     pr

      * Policy Page - 3c Addition
     d ProcAddPolPg    pr

      * Premium Discount Addition
     d ProcAddPrmDis   pr

      * Process Add Short Rate Master Info
     d ProcAddShRt     pr

      * Process Add Short Rate Details Info
     d ProcAddShRtDt   pr

      * Process Add Small Deductible Information
     d ProcAddSmDe     pr

      * Process other Options for Cancellation/Reinstatement State Definition Grid
     d ProcOthrCRSD    pr

      * Process other Options for Endorsement Controls
     d ProcOthrEDC     pr

      * Process other Options for Expense Constant
     d ProcOthrExCo    pr

      * Process other Options for Excluded States
     d ProcOthrExSt    pr

      * Process other Options for Harbor Exposure
     d ProcOthrHrEx    pr

      * Process other Options for Installment Fees
     d ProcOthrInFe    pr

      * Process other Options for Increased Limit
     d ProcOthrInLt    pr

      * Process other Options for Loss Constant
     d ProcOthrLsCn    pr

      * Process other Options for Policy Page - 3c
     d ProcOthrPolPg   pr

      * Process other Options for Premium Discount
     d ProcOthrPrmDis  pr

      * Process other Options for Short Rate Masters
     d ProcOthrShRt    pr

      * Process other Options for Short Rate Details
     d ProcOthrShRtDt  pr

      * Process other Options for Small Deductible
     d ProcOthrSmDe    pr

      * Process other Options for State Rate Change
     d ProcOthrStRC    pr

      * Process other Options for Statistical Report/Rating
     d ProcOthrStRt    pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * On/Off Subfile Display and Display Control Indicators for rcdAddPrm
     d RcdAddPrmDsp    pr
     d  indValue                      1n   const

     * On/Off Subfile Display and Display Control Indicators for rcdCxRein
     d RcdCxReinDsp    pr
     d  indValue                      1n   const

     * On/Off Subfile Display and Display Control Indicators for rcdPolPag
     d RcdPolPagDsp    pr
     d  indValue                      1n   const

     * On/Off Subfile Display and Display Control Indicators for rcdRating
     d RcdRatingDsp    pr
     d  indValue                      1n   const

     * Validate Cancellation/Reinstatement State Definition
     d ValidateCRSD    pr              n
     d strmode                        1

     * Validate Endorsement Controls info
     d ValidateEDC     pr              n
     d strmode                        1

     * Validate Expense Constant info procedure.
     d ValidateExCo    pr              n
     d strmode                        1

     * Validate Excluded States info procedure.
     d ValidateExSt    pr              n
     d strmode                        1

     * Validate Harbor Exposure info procedure.
     d ValidateHrEx    pr              n
     d strmode                        1

     * Validate Installment Fees info procedure.
     d ValidateInFe    pr              n
     d strmode                        1

     * Validate Increased Limit info procedure.
     d ValidateInLt    pr              n
     d strmode                        1

     * Validate Loss Constant info procedure.
     d ValidateLsCn    pr              n
     d strmode                        1

      *** Begin Add ***** 04/01/25 *****************************
     * Validate value entered on filter screen for Small Deductibles Grid
     d ValidateFlt     pr              n
      *** End   Add ***** 04/01/25 *****************************

     * Validate Policy Page - 3c Info procedure.
     d ValidatePolPg   pr              n
     d strmode                        1

     * Validate Premium Discount Info procedure.
     d ValidatePrmDis  pr              n
     d strmode                        1

     * Validate Short Rates Master info
     d ValidateShRt    pr              n
     d strmode                        1

     * Validate Short Rates Details info
     d ValidateShRtDt  pr              n
     d strmode                        1

     * Validate Small Deductible Info procedure.
     d ValidateSmDe    pr              n
     d strmode                        1

     * Validate State Rate Change Info procedure.
     d ValidateStRC    pr              n
     d strmode                        1


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrMessage
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy wPrTable
     d/copy sPrAtrium
     d/copy wPrPolErr
     d/copy wPrGroup
     d/copy sPrXML
      //*** Beg Add ***** 01/17/22 *****************************
     d/copy sCopyGrpPr
      //*** End Add ***** 01/17/22 *****************************
      *** Beg Add *** 05/23/23 ************************************************
     d/copy sPrSecure
      *** End Add *** 05/23/23 ************************************************


      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9
     c                   parm                    @@mode            1
     c                   parm                    @@next           10
     c                   parm                    @@TabInd          5

     c     keyWMDSL1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStE
     c                   kfld                    scCodeE
     c                   kfld                    scTypeE
     c                   kfld                    KeyEfdt
     c                   kfld                    scLmtE

     c     keyWdscl      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStELC
     c                   kfld                    KeyEfdt

     c     keyWmrtl      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStESRC
     c                   kfld                    KeyEfdt

     c     keyWtddl3     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStESD
     c                   kfld                    ScCodeESD
     c                   kfld                    KeyEfdt

     c     keyWdf4l      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    keyPolPg
     c                   kfld                    KeyEfdt

     c     keyWdxsl      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    keyEmp#
     c                   kfld                    KeyFyr

     c     keyWdxsl2     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    keyEmp#
     c                   kfld                    KeyFyr
     c                   kfld                    scStEExSt

     c     keyWmifl1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStEIF
     c                   kfld                    KeyEfdt

     c     keyWdecl1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStEEC
     c                   kfld                    KeyEfdt

     c     keyWtlml2     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStEIL
     c                   kfld                    KeyEfdt
     c                   kfld                    KeyIncLmt

     c     keyWdhel1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scstEHR
     c                   kfld                    KeyEfdt

     c     keyWtxcl1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStECRSD
     c                   kfld                    KeyEfdt

     c     keyWmnxl1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStEEDC
     c                   kfld                    KeyEfdt

     c     keyWmstl      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStESRT
     c                   kfld                    KeyEfdt

     c     keyWdstl      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    scStESRT
     c                   kfld                    KeyEfdt
     c                   kfld                    scDaysSRDE


     c     *dtaara       define    minprm        usrdat

      /free
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inFnd = FNfnd;
       scCo# = FNco#;
       scFnd = FNfnd;

       GetClientID(clientID);
       scCltid = clientID;

       ProgramStart(process);

       //*** Begin Add ***** 04/01/25 *****************************
       str9999msg = 'Your request returned too many records. ' +
        'Click Continue to see the first 9999 records. ' +
        'Click Filter on the display screen to narrow your results.';
       //*** End   Add ***** 04/01/25 *****************************

       If process;
          //*** Beg Add *** 05/23/23 ******************************************
          If TbNmRcdPnl <> *Blanks;
          //*** End Add *** 05/23/23 ******************************************

          Main();

          //*** Beg Add *** 05/23/23 ******************************************
          Else;
             LoadActionBar();
             isNoAccess = *on;
             TbNmRcdPnl = 'Security';
             pnlHeight = 560;
             aPnlHeight = 620;
             Write rcdGrpPnl;
             Exfmt rcdPanel;
             Read rcdGrpPnl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
          //*** End Add *** 05/23/23 ******************************************
       Endif;


       *InLr = *On;
      /end-free

       //*** Beg Add *** 05/23/23 ******************************************
       //===================================================================
       // Add information to list
       //===================================================================
     PAddToList        b
     DAddToList        pi           101a   varying
     D inString                     100a   varying const
     D inArray                      100a   varying dim(20)
     D inIndex                       10i 0

     DoutString        s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     PAddToList        e
       //*** End Add *** 05/23/23 ******************************************

       //===================================================================
       // InzAddPrmScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzAddPrmScVal  b
     d InzAddPrmScVal  pi

      /Free
       scCo# = intCo#;
       scFnd = FNfnd;
       scCltid = clientID;
       scCo#E = intCo#;

       visVaraE = *off;                     // Variable Checkbox
       visDedRsv = *off;                    // Deductible Reserves fields
       htSmDePnl = 230;                     // Height of Small Deductibles Info Add/Edit Panel
       topBtnSmDe = 320;                    // Top of Save, Cancel Buttons

      // Set Selecttion Criteria for Deductible Plan Drop down field
       xxchid = scStESD + 'DT';
        scPlnESDCH = 'GTco# = ' + %EditC(inCo#:'X') + ' and (GTtbid = '
        +''''+xxchid+''''+ ' or (GTtbid = '+ '''  DT'''+
        'and GTkey not in (select GTkey from WTGTP where GTtbid = '+
        ''''+xxchid+''''+')))' +
        ' and GTstat<>' + '''I''';

      // Set visibility for Small Deductibles screen fields
       If clientID = '150';
          visVaraE = *On;
       EndIf;

       Chain 'WC' SWC01;
       If %found();
          // Get Electronic Submission Flag
          If WCfl03 = 'Y';
             IsFl03 = *on;
          Endif;
          // Determine if Client is Admin only or Claims only
          If WCfl08 <> 'A';
             visDedRsv = *on;
             htSmDePnl = 320;     // Height of Small Deductibles Info Add/Edit Panel
             topBtnSmDe = 410;    // Top of Save, Cancel Buttons
          EndIf;
       Endif;


       //=====================Below visibility codes need to be removed=====
       // visVaraE = *on;
       // visDedRsv = *on;
       //=====================Above visibility codes need to be removed=====

      /End-Free
     p InzAddPrmScVal  e

       //===================================================================
       //===================================================================
       // InzCRSDScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzCRSDScVal    b
     d InzCRSDScVal    pi

      /Free
       // Check If Scriptura is enabled for Cancellation and Set Disable indicator for CX fields
       isScriptCX = UseScriptura(processTypeC);
       // Check If Scriptura is enabled for Reinstatement and Set Disable indicator for Re fields
       isScriptRE = UseScriptura(processTypeR);

       Clear tipScriptC;
       Clear tipScriptR;

       // If Scriptura has been enabled for cancellation then show tooltip
       If isScriptCX = *on;
          tipScriptC = 'Scriptura has been enabled for Cancellations in ' +
           'Application Configuration. Templates are specified there.';
       Endif;

       // If Scriptura has been enabled for reinstatement then show tooltip
       If isScriptRE = *on;
          tipScriptR = 'Scriptura has been enabled for Reinstatements in ' +
           'Application Configuration. Templates are specified there.';
       Endif;

       // Set selection criteria for Cancellation Cover Letter Reason Code
        scCcrsECH = 'GTco# = ' + %EditC(inCo#:'X') + ' and GTTBID = '
        +'''CXCD'''+' and GTstat<>' + '''I''';


       //=====================Below            codes need to be removed=====
       // isScriptCX= *off;
       // tipScriptC = 'Scriptura has been enabled for Cancellations in ' +
       //  'Application Configuration. Templates are specified there.';
       // isScriptRE= *off;
       // tipScriptR = 'Scriptura has been enabled for Reinstatements in ' +
       //  'Application Configuration. Templates are specified there.';
       //=====================Above visibility codes need to be removed=====


      /End-Free
     p InzCRSDScVal    e

       //===================================================================
       // InzRatingScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzRatingScVal  b
     d InzRatingScVal  pi

      /Free
       scCo# = intCo#;
       scFnd = FNfnd;
       scCltid = clientID;

       visMinE =  *off;                     // Minimum Premium
       visMineE = *off;                     // Minimum Premium Expanded

      // Set visibility for Minimum Premium and Minimum Premium Expanded
      // from Add/Edit Increased Limit Definitions
       In usrdat;
       If usrid <> *blank;
          visMineE = *on;
       Else;
          visMinE = *on;
       Endif;

      /End-Free
     p InzRatingScVal  e

       //===================================================================
       // Load Cancellation/Rein State Definition Grid
       //===================================================================
     p LoadCRSDGrid    b
     d LoadCRSDGrid    pi

      /free

       rrnCRSD = 0;
       RcdCxReinDsp(*off);
       clrCRSD = *on;
       Write rcdCxRein;
       clrCRSD = *off;
       RcdCxReinDsp(*on);
       scRowSel = 1;

       If dspMode;
          inquiry_3 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wtxcl1;
       Reade (inCo#:inFnd) wtxcl1;
       Dow not %eof(wtxcl1);
          rrnCRSD += 1;
          scCRSDSel = *off;

          // set the selection on 1st record for first load
          If svrrnCRSD = *zeros;
             If rrnCRSD = 1;
                scCRSDSel = *on;
             Endif;
          Else;
             If rrnCRSD = svrrnCRSD;
                scCRSDSel = *on;
             Endif;
          Endif;

          scStCRSD= XCst;
          scEfDtCRSD = CvtToDate(XCefdt);
          scNotc = XCnotc;
          scCxlm = XCcxlm;
          scCxmn = XCcxmn;

          scCRSDRRN = dbrrn12;

          Write sflCRSD;
          Reade (inCo#:inFnd) wtxcl1;
       Enddo;

       rrnCRSD = 1;
       If svrrnCRSD <> 0;
          rrnCRSD = svrrnCRSD;
          scRowSel = svrrnCRSD;
          svrrnCRSD = 0;
       Endif;

      /end-free
     p LoadCRSDGrid    e

       //===================================================================
       // Load Endorsement Controls Grid
       //===================================================================
     p LoadEDCGrid     b
     d LoadEDCGrid     pi

      /free

       rrnEDC = 0;
       dspEDC = *off;
       clrEDC = *on;
       Write rcdEndors;
       clrEDC = *off;
       dspEDC = *on;
       scRowSel = 1;

       If dspMode;
          inquiry_4 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wmnxl1;
       Reade (inCo#:inFnd) wmnxl1;
       Dow not %eof(wmnxl1);
          rrnEDC += 1;
          scEDCSel = *off;

          // set the selection on 1st record for first load
          If svrrnEDC = *zeros;
             If rrnEDC = 1;
                scEDCSel = *on;
             Endif;
          Else;
             If rrnEDC = svrrnEDC;
                scEDCSel = *on;
             Endif;
          Endif;

          scStEDC = NXst;
          scEfDtEDC = CvtToDate(NXefdt);
          scEDCSts = *Blanks;

          //*** Begin Chg ***** 08/28/23 *****************************
          // scActEDC = *Blanks;
          // If NXstat = 'A';
          //    scEDCSts = 'Active';
          //    scActEDC = 'Inactivate';
          // Endif;
          // If NXstat = 'I';
          //    scEDCSts = 'Inactive';
          //    scActEDC = 'Activate';
          // Endif;

          visActEDC = *off;                // New Hyperlink for Activate
          visIActEDC = *off;               // New Hyperlink for Inactivate
          If NXstat = 'A';
             scEDCSts = 'Active';
             visIActEDC = *on;
          Endif;
          If NXstat = 'I';
             scEDCSts = 'Inactive';
             visActEDC = *on;
          Endif;

          If dspMode;
             visActEDC = *off;
             visIActEDC = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          scEDCRRN = dbrrn14;

          Write sflEDC;
          Reade (inCo#:inFnd) wmnxl1;
       Enddo;

       rrnEDC = 1;
       If svrrnEDC <> 0;
          rrnEDC = svrrnEDC;
          scRowSel = svrrnEDC;
          svrrnEDC = 0;
       Endif;

      /end-free
     p LoadEDCGrid     e

       //===================================================================
       // Load Expense Constant Grid
       //===================================================================
     p LoadExCoGrid    b
     d LoadExCoGrid    pi

      /free

       rrnExpCon = 0;
       RcdRatingDsp(*off);
       clrExpCon = *on;
       Write rcdRating;
       clrExpCon = *off;
       RcdRatingDsp(*on);
       scRowSel = 1;

       If dspMode;
          inquiry = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wdecl1;
       Reade (inCo#:inFnd) wdecl1;
       Dow not %eof(wdecl1);
          rrnExpCon += 1;
          scExCoSel = *off;

          // set the selection on 1st record for first load
          If svrrnExpCon = *zeros;
             If rrnExpCon = 1;
                scExCoSel = *on;
             Endif;
          Else;
             If rrnExpCon = svrrnExpCon;
                scExCoSel = *on;
             Endif;
          Endif;

          scStEC = ECst;
          scEfDtEC = CvtToDate(ECefdt);
          scExco = ECexco;
          scMpec = ECmpec;
          scSful = ECsful;

          scExCoRRN = dbrrn8;

          Write sflExpCon;
          Reade (inCo#:inFnd) wdecl1;
       Enddo;

       rrnExpCon = 1;
       If svrrnExpCon <> 0;
          rrnExpCon = svrrnExpCon;
          scRowSel = svrrnExpCon;
          svrrnExpCon = 0;
       Endif;

      /end-free
     p LoadExCoGrid    e

       //===================================================================
       // Load Excluded States Grid
       //===================================================================
     p LoadExStGrid    b
     d LoadExStGrid    pi

      /free
       // Clear the subfile
       rrnExSt = 0;
       RcdPolPagDsp(*off);
       clrExSt = *on;
       Write rcdPolPag;
       clrExSt = *off;
       RcdPolPagDsp(*on);

       If dspMode;
          inquiry_2 = *on;
       Endif;

       // Populate the subfile
       keyEmp# = 0;
       KeyFyr = 0;
       Setll keyWdxsl wdxsl;
       Reade keyWdxsl wdxsl;
       Dow not %eof(wdxsl);
          rrnExSt += 1;
          scExStSel = *off;
          If rrnExSt = 1;
             scExStSel = *on;
          Endif;

          scStExSt = XSst;                        // State Code

          keyExSt = scStExSt;
          Chain keyExSt ststl3;
          If %found(ststl3);
             scState = %trim(STname);             // State
          Endif;

          scExStRRN = dbrrn6;

          Write sflExst;
          Reade keyWdxsl wdxsl;
       Enddo;

       rrnExSt = 1;

      /end-free
     p LoadExStGrid    e

       //===================================================================
       // Load Expense Constant Grid
       //===================================================================
     p LoadHrExGrid    b
     d LoadHrExGrid    pi

      /free

       rrnHrEx = 0;
       RcdAddPrmDsp(*off);
       clrHrEx = *on;
       write rcdAddPrm;
       clrHrEx = *off;
       RcdAddPrmDsp(*on);

       If dspMode;
          inquiry_1 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wdhel1;
       Reade (inCo#:inFnd) wdhel1;
       Dow not %eof(wdhel1);
          rrnHrEx += 1;
          scHrExSel = *off;

          // set the selection on 1st record for first load
          If svrrnHrEx = *zeros;
             If rrnHrEx = 1;
                scHrExSel = *on;
             Endif;
          Else;
             If rrnHrEx = svrrnHrEx;
                scHrExSel = *on;
             Endif;
          Endif;

          scStHR  = HEst;
          scEfdtHR = CvtToDate(HEefdt);
          scLhnf = HElhnf;
          scHxmn = HEhxmn;
          scHemc = HEhemc;

          scHrExRRN = dbrrn10;

          Write sflHrEx;
          Reade (inCo#:inFnd) wdhel1;
       Enddo;

       rrnHrEx = 1;
       If svrrnHrEx <> 0;
          rrnHrEx = svrrnHrEx;
          svrrnHrEx = 0;
       Endif;

      /end-free
     p LoadHrExGrid    e

       //===================================================================
       // Load Installment Fees Grid
       //===================================================================
     p LoadInFeGrid    b
     d LoadInFeGrid    pi

      /free

       rrnInFe = 0;
       dspInFe = *off;
       clrInFe = *on;
       Write rcdInsFee;
       clrInFe = *off;
       dspInFe = *on;

       If dspMode;
          inquiry_5 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wmifl1;
       Reade (inCo#:inFnd) wmifl1;
       Dow not %eof(wmifl1);
          rrnInFe += 1;
          scInFeSel = *off;

          // set the selection on 1st record for first load
          If svrrnInFe = *zeros;
             If rrnInFe = 1;
                scInFeSel = *on;
             Endif;
          Else;
             If rrnInFe = svrrnInFe;
                scInFeSel = *on;
             Endif;
          Endif;

          scStIF = IFst;
          scEfDtIF = CvtToDate(IFefdt);
          scExpdIF = CvtToDate(IFexpd);
          ScIfbl = IFifbl;
          ScIfee = IFifee;
          scIsfe = IFisfe;
          scIfpd = IFifpd;
          scIftr = IFiftr;
          scIftc = IFiftc;

          scInFeRRN = dbrrn7;

          Write sflInFe;
          Reade (inCo#:inFnd) wmifl1;
       Enddo;

       rrnInFe = 1;
       If svrrnInFe <> 0;
          rrnInFe = svrrnInFe;
          svrrnInFe = 0;
       Endif;

      /end-free
     p LoadInFeGrid    e

       //===================================================================
       // Load Increased Limit Grid
       //===================================================================
     p LoadInLtGrid    b
     d LoadInLtGrid    pi

      /free

       rrnIncLmt = 0;
       RcdRatingDsp(*off);
       clrIncLmt = *on;
       Write rcdRating;
       clrIncLmt = *off;
       RcdRatingDsp(*on);
       scRowSel2 = 1;

       If dspMode;
          inquiry = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wtlml2;
       Reade (inCo#:inFnd) wtlml2;
       Dow not %eof(wtlml2);
          rrnIncLmt += 1;
          scInLtSel = *off;

          // set the selection on 1st record for first load
          If svrrnIncLmt = *zeros;
             If rrnIncLmt = 1;
                scInLtSel = *on;
             Endif;
          Else;
             If rrnIncLmt = svrrnIncLmt;
                scInLtSel = *on;
             Endif;
          Endif;

          scEfDtIL = CvtToDate(LMefdt);
          scStIL   = LMst;
          scKey  = LMkey;
          scDescIL = LMdesc;
          scInLmSts = *Blanks;

          //*** Begin Chg ***** 08/28/23 *****************************
          // scActivate = *Blanks;
          // If LMstat = 'A';
          //    scInLmSts = 'Active';
          //    scActivate = 'Inactivate';
          // Endif;
          // If LMstat = 'I';
          //    scInLmSts = 'Inactive';
          //    scActivate = 'Activate';
          // Endif;

          visActSts = *off;                // New Hyperlink for Activate
          visIActSts = *off;               // New Hyperlink for Inactivate
          If LMstat = 'A';
             scInLmSts = 'Active';
             visIActSts = *on;
          Endif;
          If LMstat = 'I';
             scInLmSts = 'Inactive';
             visActSts = *on;
          Endif;

          If dspMode;
             visActSts = *off;
             visIActSts = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          scInLmtRRN = dbrrn9;

          Write sflIncLmt;
          Reade (inCo#:inFnd) wtlml2;
       Enddo;

       rrnIncLmt = 1;
       If svrrnIncLmt <> 0;
          rrnIncLmt = svrrnIncLmt;
          scRowSel2 = svrrnIncLmt;
          svrrnIncLmt = 0;
       Endif;

      /end-free
     p LoadInLtGrid    e

       //===================================================================
       // Load Loss Constants Grid
       //===================================================================
     p LoadLsCnGrid    b
     d LoadLsCnGrid    pi

      /free

       rrnLsCn = 0;
       RcdAddPrmDsp(*off);
       clrLsCn = *on;
       Write rcdAddPrm;
       clrLsCn = *off;
       RcdAddPrmDsp(*on);

       If dspMode;
          inquiry_1 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wdscl;
       Reade (inCo#:inFnd) wdscl;
       Dow not %eof(wdscl);
          rrnLsCn += 1;
          scLsCnSel = *off;

          // set the selection on 1st record for first load
          If svrrnLsCn = *zeros;
             If rrnLsCn = 1;
                scLsCnSel = *on;
             Endif;
          Else;
             If rrnLsCn = svrrnLsCn;
                scLsCnSel = *on;
             Endif;
          Endif;

          scStLC = sc_SCst;
          scEfDtLC = CvtToDate(sc_SCefdt);
          scPrbsLC = sc_SCprbs;
          scStatLC = sc_SCstat;

          scLsCnRRN = dbrrn2;

          Write sflLsCn;
          Reade (inCo#:inFnd) wdscl;
       Enddo;

       rrnLsCn = 1;
       If svrrnLsCn <> 0;
          rrnLsCn = svrrnLsCn;
          svrrnLsCn = 0;
       Endif;

      /end-free
     p LoadLsCnGrid    e

       //===================================================================
       // Load Policy Page - 3c Grid
       //===================================================================
     p LoadPolPgGrid   b
     d LoadPolPgGrid   pi

      /free
       // Clear the subfile
       rrnPolPg = 0;
       RcdPolPagDsp(*off);
       clrPolPg = *on;
       Write rcdPolPag;
       clrPolPg = *off;
       RcdPolPagDsp(*on);

       If dspMode;
          inquiry_2 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wdf4l;
       Reade (inCo#:inFnd) wdf4l;
       Dow not %eof(wdf4l);
          If F4stat = 'A';
             rrnPolPg += 1;
             scPolPgSel = *off;
             If rrnPolPg = 1;
                scPolPgSel = *on;
             Endif;

             scVerbTyp = F4key;
             scEfDtPP = CvtToDate(F4efdt);

             Select;
             When F4key = 'PP3C';
                scTextPP = %trim(F4txt1) + ' ' + %trim(F4txt2);
             Other;
                scTextPP = %trim(F4vDesc);
             Endsl;

             scPolPgRRN = dbrrn5;

             Write sflPolPg;
          Endif;
          Reade (inCo#:inFnd) wdf4l;
       Enddo;

       rrnPolPg = 1;

      /end-free
     p LoadPolPgGrid   e

       //===================================================================
       // Load Premium Discount Grid
       //===================================================================
     p LoadPrmDisGrid  b
     d LoadPrmDisGrid  pi

      /free
       // Clear the subfile
       rrnPrmDis = 0;
       RcdRatingDsp(*off);
       clrPrmDis = *on;
       Write rcdRating;
       clrPrmDis = *off;
       RcdRatingDsp(*on);

       If dspMode;
          inquiry = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wmdsl2;
       Reade (inCo#:inFnd) wmdsl2;
       Dow not %eof(wmdsl2);
          rrnPrmDis += 1;
          scPrDiSel = *off;

          // set the selection on 1st record for first load
          If svrrnPrmDis = *zeros;
             If rrnPrmDis = 1;
                scPrDiSel = *on;
             Endif;
          Else;
             If rrnPrmDis = svrrnPrmDis;
                scPrDiSel = *on;
             Endif;
          Endif;

          scSt = DSst;
          scEfDt = CvtToDate(DSefdt);
          scCode = DScode;
          If DStype = 'A';
             scType = 'Sliding Scale';
          ElseIf DStype = 'B';
             scType = 'Specific Lookup';
          Else;
             scType = DStype;
          Endif;
          scLmt = DSlmt;
          scPct = DSpct;

          scPrDiRRN = dbrrn;

          Write sflPrmDis;
          Reade (inCo#:inFnd) wmdsl2;
       Enddo;

       rrnPrmDis = 1;
       If svrrnPrmDis <> 0;
          rrnPrmDis = svrrnPrmDis;
          svrrnPrmDis = 0;
       Endif;

      /end-free
     p LoadPrmDisGrid  e


       //===================================================================
       // Load Short Rate Master Grid
       //===================================================================
     p LoadShRtGrid    b
     d LoadShRtGrid    pi

      /free

       rrnSRT = 0;
       RcdCxReinDsp(*off);
       clrSRT = *on;
       Write rcdCxRein;
       clrSRT = *off;
       RcdCxReinDsp(*on);
       scRowSel2 = 1;

       If dspMode;
          inquiry_3 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) pst00;
       Reade (inCo#:inFnd) pst00;
       Dow not %eof(wmstl);
          rrnSRT += 1;
          scSRTSel = *off;

          // set the selection on 1st record for first load
          If svrrnSRT = *zeros;
             If rrnSRT = 1;
                scSRTSel = *on;
             Endif;
          Else;
             If rrnSRT = svrrnSRT;
                scSRTSel = *on;
             Endif;
          Endif;

          scStSRT = m_STst;
          scEfdtSRT = CvtToDate(m_STefdt);
          scEfdtFile = m_STefdt;

          scSRTRRN = dbrrn15;

          Write sflSRT;
          Reade (inCo#:inFnd) pst00;
       Enddo;

       rrnSRT = 1;
       If svrrnSRT <> 0;
          rrnSRT = svrrnSRT;
          scRowSel2 = svrrnSRT;
          svrrnSRT = 0;
       Endif;

      /end-free
     p LoadShRtGrid    e

       //===================================================================
       // Load Short Rate Detail Grid
       //===================================================================
     p LoadShRtDtGrid  b
     d LoadShRtDtGrid  pi

      /free

       rrnSRTDE = 0;
       dspSRTDE = *off;
       clrSRTDE = *on;
       Write rcdSRTDtl;
       clrSRTDE = *off;
       dspSRTDE = *on;
       scRowSel3 = 1;

       If dspMode or cpyMode;
          Dinquiry_3 = *on;
       Endif;

       // Populate the subfile
       KeyEfdt = dtCYMD(scEfDtESRT);
       Setll keyWmstl pst02;
       Reade keyWmstl pst02;
       Dow not %eof(wdstl);
          rrnSRTDE += 1;
          scSRTDESel = *off;

          // set the selection on 1st record for first load
          If svrrnSRTDE = *zeros;
             If rrnSRTDE = 1;
                scSRTDESel = *on;
             Endif;
          Else;
             If rrnSRTDE = svrrnSRTDE;
                scSRTDESel = *on;
             Endif;
          Endif;

          scDaysSRD = STdays;
          scPercSRD = STperc;

          scSRTDERRN = dbrrn16;

          Write sflSRTDE;
          Reade keyWmstl pst02;
       Enddo;

       TotalSfRcd = rrnSRTDE;      // Save total record count
       rrnSRTDE = 1;
       If svrrnSRTDE <> 0;
          rrnSRTDE = svrrnSRTDE;
          scRowSel3 = svrrnSRTDE;
          svrrnSRTDE = 0;
       Endif;

      /end-free
     p LoadShRtDtGrid  e

       //===================================================================
       // Load Small Deductibles Grid
       //===================================================================
     p LoadSmDeGrid    b
     d LoadSmDeGrid    pi

      /free

       rrnSmDe = 0;
       RcdAddPrmDsp(*off);
       clrSmDe = *on;
       Write rcdAddPrm;
       clrSmDe = *off;
       RcdAddPrmDsp(*on);
       scRowSel = 1;

       If dspMode;
          inquiry_1 = *on;
       Endif;

       // Populate the Small Deductible subfile
       Setll (inCo#:inFnd) wtddl3;
       Reade (inCo#:inFnd) wtddl3;
       Dow not %eof(wtddl3);
          //*** Begin Add ***** 04/01/25 *****************************
          // Validate for filter for Small Deductibles Grid
          meetFltr = *on;
          If scStSDFlt <> *blanks or scFDtSDFlt <> 0 or scTDtSDFlt <> 0;
             MeetFlt();
          Endif;

          If meetFltr = *on;
          //*** End   Add ***** 04/01/25 *****************************

          rrnSmDe += 1;
          scSmDeSel = *off;

          // set the selection on 1st record for first load
          If svrrnSmDe = *zeros;
             If rrnSmDe = 1;
                scSmDeSel = *on;
             Endif;
          Else;
             If rrnSmDe = svrrnSmDe;
                scSmDeSel = *on;
             Endif;
          Endif;

          scStSD = DDst;
          scEfDtSD = CvtToDate(DDefdt);
          scCodeSD = DDcode;
          scDescSD = DDdesc;
          scTypeSD = DDtype;
          scPctSD = DDpct;
          scAmag = DDamag;
          scAmpo = DDampo;
          scclasSD = DDclas;
          scPlanSD = DDplan;

          //*** Begin Chg ***** 08/28/23 *****************************
          // scActSmDe  = *Blanks;
          // If DDstat = 'A';
          //    scSmDeSts = 'Active';
          //    scActSmDe  = 'Inactivate';
          // Endif;
          // If DDstat = 'I';
          //    scSmDeSts = 'Inactive';
          //    scActSmDe = 'Activate';
          // Endif;

          visActSmD = *off;                // New Hyperlink for Activate
          visIActSmD = *off;               // New Hyperlink for Inactivate
          If DDstat = 'A';
             scSmDeSts = 'Active';
             visIActSmD = *on;
          Endif;
          If DDstat = 'I';
             scSmDeSts = 'Inactive';
             visActSmD = *on;
          Endif;

          If dspMode;
             visActSmD = *off;
             visIActSmD = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          scSmDeRRN = dbrrn4;

          //*** Begin Add ***** 04/01/25 *****************************
          If rrnSmDe > 9999;
             PopOneBtn(str9999msg:'Small Deductibles':'Continue');
             Leave;
          Endif;
          //*** End   Add ***** 04/01/25 *****************************

          Write sflSmDe;

          //*** Begin Add ***** 08/23/24 ***** 117377 ****************
          // Condition number of recs written to the Small Deductibles SFL
          // to avoid exceeded RRN max error.
          //*** Begin Del ***** 04/01/25 *****************************
          // Following code not needed now after Filter Button
          //*** If rrnSmDe >= 9999;
          //***    scFSDMsg  = 'Too many records returned; first 9999 rows are +
          //***                 shown. Right click the specific column heading +
          //***                 and choose a filter.';
          //***
          //***    vsbFSDMsg = *On;
          //***    rrnSmDe   = 1;
          //***    Leave;
          //*** EndIf;
          //*** End   Del ***** 04/01/25 *****************************
          //*** End   Chg ***** 08/23/24 ***** 117377 ****************

          //*** Begin Add ***** 04/01/25 *****************************
          Endif;                                // If meetFltr = *on
          //*** End   Add ***** 04/01/25 *****************************

          Reade (inCo#:inFnd) wtddl3;
       Enddo;

       rrnSmDe = 1;
       If svrrnSmDe <> 0;
          rrnSmDe = svrrnSmDe;
          scRowSel = svrrnSmDe;
          svrrnSmDe = 0;
       Endif;

      /end-free
     p LoadSmDeGrid    e

       //===================================================================
       // Load State Rate Change Grid
       //===================================================================
     p LoadStRCGrid    b
     d LoadStRCGrid    pi

      /free

       rrnStRC = 0;
       RcdRatingDsp(*off);
       clrStRC = *on;
       Write rcdRating;
       clrStRC = *off;
       RcdRatingDsp(*on);

       If dspMode;
          inquiry = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wmrtl;
       Reade (inCo#:inFnd) wmrtl;
       Dow not %eof(wmrtl);
          rrnStRC += 1;
          scStRCSel = *off;

          // set the selection on 1st record for first load
          If svrrnStRC = *zeros;
             If rrnStRC = 1;
                scStRCSel = *on;
             Endif;
          Else;
             If rrnStRC = svrrnStRC;
                scStRCSel = *on;
             Endif;
          Endif;

          scStSRC = RTst;
          scEfDtSRC = CvtToDate(RTefdt);
          scStatSRC = RTstat;
          scOpt = RTopt;

          scStRcRRN = dbrrn3;

          Write sflStRC;
          Reade (inCo#:inFnd) wmrtl;
       Enddo;

       rrnStRC = 1;
       If svrrnStRC <> 0;
          rrnStRC = svrrnStRC;
          svrrnStRC = 0;
       Endif;

      /end-free
     p LoadStRCGrid    e

       //===================================================================
       // Load Statistical Report/Rating Grid
       //===================================================================
     p LoadStRtGrid    b
     d LoadStRtGrid    pi

      /free

       rrnStRt = 0;
       RcdRatingDsp(*off);
       clrStRt = *on;
       Write rcdRating;
       clrStRt = *off;
       RcdRatingDsp(*on);

       If dspMode;
          inquiry = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wdrrl1;
       Reade (inCo#:inFnd) wdrrl1;
       Dow not %eof(wdrrl1);
          rrnStRt += 1;
          scStRtSel = *off;

          // set the selection on 1st record for first load
          If svrrnStRt = *zeros;
             If rrnStRt = 1;
                scStRtSel = *on;
             Endif;
          Else;
             If rrnStRt = svrrnStRt;
                scStRtSel = *on;
             Endif;
          Endif;

          scStSR = RRst;
          scEfdtSR = CvtToDate(RRefdt);

          keyExSt = scStSR;
          Chain keyExSt ststl3;
          If %found(ststl3);
             scDescSR = %trim(STname);             // State Description
          Endif;

          scStRtRRN = dbrrn11;
          strUrlRRN = %Editc(scStRtRRN:'X');

          // Populate Edit Statistical Report/Rating Link
          Clear scEdtURL;
          strUrlTabName = 'Edit Statistical Report/Rating Info';
          scEdtURL    = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'C':
           'WTGRPSTRAT');

          // Populate Inquiry Statistical Report/Rating Link
          Clear scInqUrl;
          strUrlTabName = 'Display Statistical Report/Rating Info';
          scInqUrl    = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'D':
           'WTGRPSTRAT');

          // Populate Copy Statistical Report/Rating Link
          Clear scCpyUrl;
          strUrlTabName = 'Copy Statistical Report/Rating Info';
          scCpyUrl    = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'X':
           'WTGRPSTRAT');

          Write sflStRt;
          Reade (inCo#:inFnd) wdrrl1;
       Enddo;

       // Populate Add Statistical Report/Rating Link
       Clear scAddURL;
       Clear strUrlRRN;
       // strUrlRRN = '000000000';
       // Pass Company and Group for ADD mode
       strUrlRRN = %Editc(inCo#:'X') + %Editc(inFnd:'X') + '000';
       strUrlTabName = 'Add Statistical Report/Rating Info';
       scAddURL  = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'A':
        'WTGRPSTRAT');

       rrnStRt = 1;
       If svrrnStRt <> 0;
          rrnStRt = svrrnStRt;
          svrrnStRt = 0;
       Endif;

      /end-free
     p LoadStRtGrid    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d  isexit         s               n   Inz(*off)
       //*** Begin Add ***** 04/01/25 *****************************
     d isexitFltr      s               n   Inz(*off)
       //*** End   Add ***** 04/01/25 *****************************
      /free

       //LoadActionBar();

       BtnCancel = *off;

       // Process until exit button is pressed.
       DoW BtnCancel = *off;

          // display screen
          Exsr ExfmtScreens;

          error = *off;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh =*Off;

          //*** Begin Add ***** 04/01/25 *****************************
          // Filter Processing for Small Deductibles Grid
          When BtnFilter = *on;
             BtnFilter = *off;
             isexitFltr = *off;
             Dow isexitFltr = *off;
                ExFmt rcdSmDFltr;
                Select;
                When BtnFltrApp = *on;
                   BtnFltrApp = *off;
                   If ValidateFlt();
                      dsFilter.scStSDFlt = scStSDFlt;
                      dsFilter.scFDtSDFlt = scFDtSDFlt;
                      dsFilter.scTDtSDFlt = scTDtSDFlt;
                      isExitFltr = *on;
                      dspClrFltr = *on;
                      sav_dspClrFltr = *on;
                   Endif;

                When BtnFltrCan = *on;
                   BtnFltrCan = *off;
                   scStSDFlt = dsFilter.scStSDFlt;
                   scFDtSDFlt = dsFilter.scFDtSDFlt;
                   scTDtSDFlt = dsFilter.scTDtSDFlt;
                   isExitFltr = *on;

                   msFDtSDFlt = *blanks;
                   erFDtSDFlt = *off;
                   msTDtSDFlt = *blanks;
                   erTDtSDFlt = *off;
                Endsl;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             BtnClrFltr = *off;
             Clear rcdSmDFltr;
             dspClrFltr = *off;
             sav_dspClrFltr = *off;
             Clear dsFilter;
          //*** End   Add ***** 04/01/25 *****************************

          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;

          // Add Premium Discount Button
          When BtnPrDiAdd = *On;
             BtnPrDiAdd = *Off;
             ProcAddPrmDis();

          // Add Loss Constant info screen
          When BtnLsCnAdd = *On;
             BtnLsCnAdd = *Off;
             ProcAddLsCn();

          // Add Small Deductible info screen
          When BtnSmDeAdd = *On;
             BtnSmDeAdd = *Off;
             ProcAddSmDe();

          // Add Policy Page - 3c info screen
          When BtnPPgAdd = *On;
             BtnPPgAdd = *Off;
             ProcAddPolPg();

          // Add Excluded States info screen
          When BtnExStAdd = *On;
             BtnExStAdd = *Off;
             ProcAddExSt();

          // Add Installment Fees info screen
          When BtnInFeAdd = *On;
             BtnInFeAdd = *Off;
             ProcAddInFe();

          // Add Expense Constant info screen
          When BtnExCoAdd = *On;
             BtnExCoAdd = *Off;
             ProcAddExCo();

          // Add Increased Limit info screen
          When BtnInLtAdd = *On;
             BtnInLtAdd = *Off;
             ProcAddInLt();

          // Add Harbor Exposure info screen
          When BtnHrExAdd = *On;
             BtnHrExAdd = *Off;
             ProcAddHrEx();

          // Add Cancellation/Reinstatement state def info
          When BtnCRSDAdd = *On;
             BtnCRSDAdd = *Off;
             ProcAddCRSD();

          // Add Endorsement Controls Info
          When BtnEDCAdd = *On;
             BtnEDCAdd = *Off;
             ProcAddEDC();

          // Add Short Rate Info
          When BtnSRTAdd = *On;
             BtnSRTAdd = *Off;
             ProcAddShRt();

          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          //*** End Add ***** 01/17/22 *******************************

          //*** Beg Add *** 05/23/23 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTGRPSSI';
             inPgmName     = 'Group Configuration - State Specific Information';
             inTabName     =  arrTabNames(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/23/23 ******************************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;
             ProcOthrLsCn();                //other option for Loss Constant
             ProcOthrPrmDis();              //other option for Premium Discount
             ProcOthrSmDe();                //other option for Small Deductible
             ProcOthrStRC();                //other option for State Rate Change
             ProcOthrPolPg();               //other option for Policy Page - 3c
             ProcOthrExSt();                //other option for Excluded States
             ProcOthrInFe();                //other option for Installment Fees
             ProcOthrExCo();                //other option for Expense Constant
             ProcOthrInLt();                //other option for Increased Limit
             ProcOthrHrEx();                //other option for Harbor Exposure
             ProcOthrStRt();                //other option for Statistical Report/Rating
             ProcOthrCRSD();          //other option for Cancellation/Reinstatement state def
             ProcOthrEDC();                 //other option for Endorsement Controls
             ProcOthrShRt();                //other option for Short Rate Masters

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 1;
       When arrTabNames(TabIndex+1) = 'Rating';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 1330;
          APnlHeight = pnlHeight + 60;
          Clear rcdRating;

         // Load Statistical Report/Rating Grid info
          LoadStRtGrid();

         // Load Premium Discount Grid info
          LoadPrmDisGrid();

         // Load Expense Constant Grid info
          LoadExCoGrid();

         // Load Increased Limit Grid info
          LoadInLtGrid();

         // Load State Rate Change Grid
          LoadStRCGrid();

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdRating;
          Read rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 2;
       When arrTabNames(TabIndex+1) = 'Rating Factors';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 980;
          APnlHeight = pnlHeight + 60;
          Clear rcdAddPrm;

          //*** Begin Add ***** 04/01/25 *****************************
          dspClrFltr = sav_dspClrFltr;         // Restore Indicator value
          //*** End   Add ***** 04/01/25 *****************************

          // Load Initial Values
          InzAddPrmScVal();

          // Load Small Deductibles grid
          LoadSmDeGrid();

          // Load Harbor Exposure grid
          LoadHrExGrid();

          // Load Loss Constant limit grid
          LoadLsCnGrid();

          If dspMode;
             inquiry_1 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdAddPrm;
          Read rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 3;
       When arrTabNames(TabIndex+1) = 'Policy Page';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;
          Clear rcdPolPag;

         // Load Policy Page - 3c Grid info
          LoadPolPgGrid();

         // Load Excluded States Grid
          LoadExStGrid();

          If dspMode;
             inquiry_2 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdPolPag;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 4;
       When arrTabNames(TabIndex+1) = 'Cancellation / Reinstatement';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 1010;
          APnlHeight = pnlHeight + 60;
          Clear rcdCxRein;

          scCo# = inco#;
          scFnd = infnd;

          // Load Cancellation/Reinstatement State def Grid
          LoadCRSDGrid();

          // Load Short Rate Master Subfile Grid
          LoadShRtGrid();

          If dspMode;
             inquiry_3 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdCxRein;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 5;
       When arrTabNames(TabIndex+1) = 'Endorsements';
       //*** End Chg *** 05/23/23 ******************************************
          //*** Beg Chg *** 02/19/24 ***************************************
          //***pnlHeight = 520;
          pnlHeight = 700;
          //*** End Chg *** 02/19/24 ***************************************
          APnlHeight = pnlHeight + 60;
          Clear rcdEndors;

          // Load Endorsement Controls Grid
          LoadEDCGrid();

          If dspMode;
             inquiry_4 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdEndors;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 6;
       When arrTabNames(TabIndex+1) = 'Installment Fees';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;
          Clear rcdInsFee;

          // Load Installment Fees grid
          LoadInFeGrid();

          If dspMode;
             inquiry_5 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdInsFee;
          Read  rcdGrpPnl;
          Read rcdPanel;


       EndSl;

       Endsr;


      /end-free
     p Main            e

      *** Begin Add ***** 04/01/25 *****************************
       //==================================================================
       // MeetFlt -  Determine if the records meet the Filter criteria for Small Deductibles Grid
       //==================================================================
     p MeetFlt         b
     d MeetFlt         pi
      /free

       // Check for State
       If scStSDFlt <> *Blanks and scStSDFlt <> DDst;
          meetFltr = *off;
       Endif;

       // Check for Effective Date Range
       If scFDtSDFlt <> *zeros and DDefdt < dtCYMD(scFDtSDFlt);
          meetFltr = *off;
       Endif;
       If scTDtSDFlt <> *zeros and DDefdt > dtCYMD(scTDtSDFlt);
          meetFltr = *off;
       Endif;

      /end-free
     p MeetFlt         e
      *** End   Add ***** 04/01/25 *****************************

       //===================================================================
       // ProcAddCRSD  - process addition of Cancellation/Reinstatement state def
       //===================================================================
     P ProcAddCRSD     B
     D ProcAddCRSD     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdCRSDDtl;
       mode = 'A';
       isexit = *off;
       BtnCRSDCan = *off;
       CRSDEdt = *off;
       InzCRSDScVal();

       Dow BtnCRSDCan = *off and isexit = *off;

          scCRSDHdg = 'Add Cancellation/Reinstatement +
                      State Definition Information';
          visCRSDAdS = *On;                           // Show Save/Add next Button

          Exfmt rcdCRSDDtl;

          Select;
          // Save/Add Next Button
          When BtnCRSDAdS = *on;
             BtnCRSDAdS = *off;
             If ValidateCRSD(mode);
                Exsr WrtCRSD;
          //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdCRSDDtl;
                BtnCRSDCan = *off;
                CRSDEdt = *off;
                InzCRSDScVal();
             EndIf;

          // Save Button
          When BtnCRSDSav = *on;
             BtnCRSDSav = *off;
             If ValidateCRSD(mode);
                Exsr WrtCRSD;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnCRSDCan = *on;
             BtnCRSDCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        XCco#  = inCo#;                 // Company Number
        XCfnd  = inFnd;                 // Group Number
        XCst   = scStECRSD;            // State
        XCefdt = dtCYMD(scEfDtECRS);     // Effective date
        XCnotc = scNotcE;             //Cancellation Notice Days
        XCcxlm = ScCxlmE;            // Description
        XCcxmn = ScCxmnE;             // Cancellation Limit
        XCstnm = scStnmE;             //State Department Name
        XCdrem = scDremE;             // ACCORD Remark 1 Default
        XCcxfl = scCxflE;             // State Cancellation Form  Folder
        XCcxdc = scCxdcE;             // State Cancellation Form Document
        XCrefl = scReflE;             // State Reinstatement Form Folder
        XCredc = scRedcE;             // State Reinstatement Form Document
        XCrrcd = scRrcdE;             // Require Reinstatement Reason Code
        XCstcx = scStcxE;         // Generate State Cancellation Transaction on State Removal
        XC#1lf = sc#1LFE;              // Folder 1
        XC#1ld = sc#1LDE;              // Document 1
        XC#1dr = sc#1DRE;              // Remarks 1
        XC#2lf = sc#2LFE;              // Folder 2
        XC#2ld = sc#2LDE;              // Document 2
        XC#2dr = sc#2DRE;              // Remarks 2
        XC#3lf = sc#3LFE;              // Folder 3
        XC#3ld = sc#3LDE;              // Document 3
        XC#3dr = sc#3DRE;              // Remarks 3
        XC#4lf = sc#4LFE;              // Folder 4
        XC#4ld = sc#4LDE;              // Document 4
        XC#4dr = sc#4DRE;              // Remarks 4
        XCcclf = scCclfE;              // Cancellation Cover Letter Folder
        XCccld = scCcldE;              // Cancellation Cover Letter Document
        XCccrs = scCcrsE;              // Cancellation Cover Letter Reason
        XCcflf = scCflfE;              // Client Specific Cancellation Confirmation Certificate fld
        XCcfld = scCfldE;              // Client Specific Cancellation Confirmation Certificate Doc
        XCcscf = scCscfE;              // Client Specific Cancellation Certificate Letter fld
        XCcscd = scCscdE;              // Client Specific Cancellation Certificate Letter doc
        XCcsrf = scCsrfE;              // Client Specific Reinstatement Certificate Letter fld
        XCcsrd = scCsrdE;              // Client Specific Reinstatement Certificate Letter doc
        XCrclf = scRclfE;              // Reinstatement Cover Letter fld
        XCrcld = scRcldE;              // Reinstatement Cover Letter doc
        XCuser = q1user;
        XCpgm  = q1pgm;
        XCtime = Timeto6(SystemTime());
        XCdt   = Dateto7(SystemDate());
        XCiusr = q1user;
        XCipgm = q1pgm;
        XCidt  = Dateto7(SystemDate());
        XCitim = Timeto6(SystemTime());
       EndSr;

       // Writing Cancellation/Reinstatement state details Subroutine
       BegSr WrtCRSD;
        Clear pxc0p;
        Exsr MoveScDb;
        Write pxc0p;
       EndSr;

      /end-free
     P ProcAddCRSD     E

       //===================================================================
       // ProcAddEDC   - process addition of Endorsement Controls
       //===================================================================
     P ProcAddEDC      B
     D ProcAddEDC      PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdEDCDtl;
       mode = 'A';
       isexit = *off;
       BtnEDCCan= *off;
       EDCEdt = *off;
       scEDCStE = 'A';

       Dow BtnEDCCan = *off and isexit = *off;

          scEDCHdg = 'Add Endorsement Controls Information';

          Exfmt rcdEDCDtl;

          Select;

          // Save Button
          When BtnEDCSav = *on;
             BtnEDCSav = *off;
             If ValidateEDC(mode);
                Exsr WrtEDC;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnEDCCan = *on;
             BtnEDCCan= *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        NXco#  = inCo#;                 // Company Number
        NXfnd  = inFnd;                 // Group Number
        NXst  = ScStEEDC;             // State
        NXefdt = dtCYMD(scEfdtEEDC);       // Effective Date
        NXstat = scEDCStE;              // Status
        NXprcf = scPrcfE;              // Pending Rate Change Flag
        NXprcd = dtCYMD(scPrcdE);       // Pending Rate Change Date
        NXcpct = scCpctE;              //State Coverage Percent Change
        NXrcef = scRcefE;              //Rate Change Flag
        NXrced = dtCYMD(scRcedE);       // Rate Change Date
        NXlpct = scLpctE;             // Longshore and Harbor Workers Coverage  Percentage
        NXspln = scSplnE;             // Suppress Policy Notes
        NXllmt = scLlmtE;               // Lower Salary Limit
        NXhlmt = scHlmtE;               // Higher Salary Limit
        NXdept = scDeptE;               // Dept Name to print on State form
        If scPhn#EEDC > *zeros;
           NXdpar = %dec(%subst(%editc(scPhn#EEDC:'X'):1:3):3:0); // Phone  Area Code
           NXdppn = %dec(%subst(%editc(scPhn#EEDC:'X'):4):7:0);   // Phone  Number
        EndIf;
        If scPhn#EEDC = *zeros;
           NXdpar = *zeros;
           NXdppn = *zeros;
        EndIf;
        NXofft = scOfftE;               // Require Owner/Officer Title
        NXdedf = scDedfE;               // WC020602 Consder adjust expenses (Y/N)
        NXuser = q1user;
        NXpgm  = q1pgm;
        NXtime = Timeto6(SystemTime());
        NXdt   = Dateto7(SystemDate());
        NXitim = Timeto6(SystemTime());
        NXidt  = Dateto7(SystemDate());
        NXiusr = q1user;
        NXipgm = q1pgm;
       //***** Beg Add *** 11/17/22 **********************************
        NXinsrCxDy = scInsrCxDy;
        NXinsdCxDy = scInsdCxDy;
       //***** End Add *** 11/17/22 **********************************
       //***** Beg Add *** 11/13/23 *** WC2929 ***********************
        NXStatCdMe = ScStatCdMe;
       //***** End Add *** 11/13/23 *** WC2929 ***********************
       //***** Beg Add *** 02/14/24 **********************************
        NXexcl99 = scExWc99E;
       //***** End Add *** 02/14/24 **********************************
       //***** Beg Add *** 02/19/24 **********************************
        If scStEEDC = 'HI';
           NXMCareA = scMCareA;
           NXMCareB = scMCareB;
           NXWgLossA = scWgLossA;
           NXWgLossB = scWgLossB;
           NXIBenDisA = scIBenDisA;
           NXIBenDisB = scIBenDisB;
           NXDBenA = scDBenA;
           NXDBenB = scDBenB;
           NXAdmLglA = scAdmLglA;
           NXAdmLglB = scAdmLglB;
           NXMiscA = scMiscA;
           NXMiscB = scMiscB;
        Endif;
       //***** End Add *** 02/19/24 **********************************
       EndSr;

       // Writing Endorsement Controls Info
       BegSr WrtEDC;
        Clear pnx0p;
        Exsr MoveScDb;
        Write pnx0p;
       EndSr;

      /end-free
     P ProcAddEDC      E

       //===================================================================
       // ProcAddExCo  - process addition of Expense Constant
       //===================================================================
     P ProcAddExCo     B
     D ProcAddExCo     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdExCoDtl;
       mode = 'A';
       isexit = *off;
       BtnExCoCan = *off;
       ExpConEdt = *off;

       Dow BtnExCoCan = *off and isexit = *off;

          scExCoHdg = 'Add Expense Constant Information';
          visExCoAdS = *On;                           // Show Save/Add next Button

          Exfmt rcdExCoDtl;

          Select;

          // Save/Add Next Button
          When BtnExCoAdS = *on;
             BtnExCoAdS = *off;
             If ValidateExCo(mode);
                Exsr WrtExCo;
          //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdExCoDtl;
                BtnExCoCan = *off;
                ExpConEdt = *off;
             EndIf;

          // Save Button
          When BtnExCoSav = *on;
             BtnExCoSav = *off;
             If ValidateExCo(mode);
                Exsr WrtExCo;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnExCoCan = *on;
             BtnExCoCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        ECco#   = inCo#;                 // Company Number
        ECfnd   = inFnd;                 // Group Number
        ECst    = ScStEEC;               // State
        ECefdt  = dtCYMD(scEfDtEEC);       // Effective date
        ECexco  = scExcoE;               // Amount
        ECmpec  = scMpecE;               // Minimum Amount
        ECsful  = scSfulE;               // Charge Full Expense Constant On Short Term Policies
        ECexc2  = scExc2E;               // Expense Constant
        ECstpr  = scStprE;              // Standard Premium
        ECexc3  = scExc3E;               // Expense Constant
        ECstp3  = scStp3E;              // Standard Premium
        ECuser = q1user;
        ECpgm  = q1pgm;
        ECtime = Timeto6(SystemTime());
        ECdt   = Dateto7(SystemDate());
        ECitim = Timeto6(SystemTime());
        ECidt  = Dateto7(SystemDate());
        ECiusr = q1user;
        ECipgm = q1pgm;
       EndSr;

       // Writing Expense Constant details subroutine
       BegSr WrtExCo;
        Clear pec0p;
        Exsr MoveScDb;
        Write pec0p;
       EndSr;

      /end-free
     P ProcAddExCo     E

       //===================================================================
       // ProcAddExSt - process addition of Excluded States
       //===================================================================
     P ProcAddExSt     B
     D ProcAddExSt     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdExStDtl;
       mode = 'A';
       isexit = *off;
       BtnExStCan = *off;

       Dow BtnExStCan = *off and isexit = *off;
          scExStHdg = 'Add Excluded States/WCPOLS Information';
          Exfmt rcdExStDtl;

          Select;
          // Save/Add Next Button
          When BtnExStAdS = *on;
             BtnExStAdS = *off;
             If ValidateExSt(mode);
                Exsr WrtExSt;
          //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdExStDtl;
                BtnExStCan = *off;
             EndIf;

          // Saved Button
          When BtnExStSav = *on;
             BtnExStSav = *off;
             If ValidateExSt(mode);
                Exsr WrtExSt;
                isexit = *on;
             EndIf;
          // Cancel Button
          When BtnExStCan = *on;
             BtnExStCan = *off;
             isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        XSco#   = inCo#;                 // Company Number
        XSfnd   = inFnd;                 // Group Number
        XSemp#  = *zeros;                // Employer
        XSfyr   = *zeros;                // Year
        XSst    = scStEExSt;             // State

        XSuser = q1user;
        XSpgm  = q1pgm;
        XStime = Timeto6(SystemTime());
        XSdt   = Dateto7(SystemDate());
        XSiUsr = q1user;
        XSiPgm = q1pgm;
        XSiTim = Timeto6(SystemTime());
        XSiDt  = Dateto7(SystemDate());
       EndSr;

       // Writing Excluded States subroutine
       BegSr WrtExSt;
        Clear pxs0p;
        Exsr MoveScDb;
        Write pxs0p;
       EndSr;

      /end-free
     P ProcAddExSt     E

       //===================================================================
       // ProcAddHrEx  - process addition of Harbor Exposure
       //===================================================================
     P ProcAddHrEx     B
     D ProcAddHrEx     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdHrExDtl;
       mode = 'A';
       isexit = *off;
       BtnHrExCan= *off;
       HrExEdt = *off;

       Dow BtnHrExCan = *off and isexit = *off;

          scHrExHdg = 'Add Harbor Exposure Information';

          Exfmt rcdHrExDtl;

          Select;

          // Save Button
          When BtnHrExSav = *on;
             BtnHrExSav = *off;
             If ValidateHrEx(mode);
                Exsr WrtHrEx;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnHrExCan = *on;
             BtnHrExCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb;
        HEco# = inCo#;
        HEfnd  = inFnd;
        HEst   = scstEHR;
        HEefdt = dtCYMD(scEfdtEHR);
        HElhnf = scLhnfE;
        HEhxmn = scHxmnE;
        HEhemc = scHemcE;
        HEuser = q1user;
        HEpgm  = q1pgm;
        HEtime = Timeto6(SystemTime());
        HEdt   = Dateto7(SystemDate());
        HEitim = Timeto6(SystemTime());
        HEidt  = Dateto7(SystemDate());
        HEiusr = q1user;
        HEipgm = q1pgm;
       EndSr;

       // Writing Harbor Exposure details subroutine
       BegSr WrtHrEx;
        Clear phe0p;
        Exsr MoveScDb;
        Write phe0p;
       EndSr;

      /end-free
     P ProcAddHrEx     E

       //===================================================================
       // ProcAddInFe  - process addition of Installment Fees
       //===================================================================
     P ProcAddInFe     B
     D ProcAddInFe     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdInFeDtl;
       mode = 'A';
       isexit = *off;
       BtnInFeCan = *off;
       InFeEdt = *off;

      // Set selection criteria for A/R Codes
       scIfblECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''F''';

       scIfpdECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

       scIftrECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

       scIftcECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

       Dow BtnInFeCan = *off and isexit = *off;

          scInFeHdg = 'Add Installment Fees Information';

          Exfmt rcdInFeDtl;

          Select;

          // Save Button
          When BtnInFeSav = *on;
             BtnInFeSav = *off;
             If ValidateInFe(mode);
                Exsr WrtInFe;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnInFeCan = *on;
             BtnInFeCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        IFco#  = inCo#;                 // Company Number
        IFfnd  = inFnd;                 // Group Number
        IFst   = scStEIF;              // State
        IFefdt = dtCYMD(scEfDtEIF);     // Effective date
        IFexpd = dtCYMD(scExpdEIF);     // Expiration date
        IFifee = scIfeeE;             //Estimating Fee
        IFisfe = scIsfeE;             //Self Reporting Fee
        IFifbl = scIfblE;            // Bill A/R Code
        IFifpd = scIfpdE;             // Paid A/R Code
        IFiftr = scIftrE;             //Flat Cancellation A/R Transfer Code
        IFiftc = scIftcE;              //Payment A/R Transfer Code
        IFuser = q1user;
        IFpgm  = q1pgm;
        IFtime = Timeto6(SystemTime());
        IFdt   = Dateto7(SystemDate());
        IFitim = Timeto6(SystemTime());
        IFidt  = Dateto7(SystemDate());
        IFiusr = q1user;
       EndSr;

       // Writing Installment Fees details subroutine
       BegSr WrtInFe;
        Clear pif0p;
        Exsr MoveScDb;
        Write pif0p;
       EndSr;

      /end-free
     P ProcAddInFe     E

       //===================================================================
       // ProcAddInLt  - process addition of Increased limits
       //===================================================================
     P ProcAddInLt     B
     D ProcAddInLt     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdInLtDtl;
       mode = 'A';
       isexit = *off;
       BtnInLtCan = *off;
       IncLmtEdt = *off;
       scInLmStE = 'A';


       Dow BtnInLtCan = *off and isexit = *off;

          // Load Initial Values
          InzRatingScVal();

          scInLtHdg = 'Add Increased Limits Information';
          visInLtAdS = *On;                           // Show Save/Add next Button

          Exfmt rcdInLtDtl;

          Select;

          // Save/Add Next Button
          When BtnInLtAdS = *on;
             BtnInLtAdS = *off;
             If ValidateInLt(mode);
                Exsr WrtInLt;
          //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdInLtDtl;
               // Load Initial Values
                InzRatingScVal();
                BtnInLtCan = *off;
                IncLmtEdt = *off;
                scInLmStE = 'A';
             EndIf;


          // Save Button
          When BtnInLtSav = *on;
             BtnInLtSav = *off;
             If ValidateInLt(mode);
                Exsr WrtInLt;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnInLtCan = *on;
             BtnInLtCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        LMco#   = inCo#;                 // Company Number
        LMfnd   = inFnd;                 // Group Number
        LMst    = scStEIL;               // State
        LMefdt  = dtCYMD(scEfDtEIL);       // Effective date
        LMkey   = scKeyE;               // Code
        LMdesc  = scDescEIL;               // Description
        LMacc   = scAccE;               // Bodily Injury by accident
        LMdis   = scDisE;               // Bodily Injury by disease
        LMagg   = scAggE;              // Bodily Injury by disease Agg
        LMmin   = scMinE;               // Minimum Premium
        LMminE  = scMineE;               // Minimum Premium
        LMclas  = scClasE;              // Stastical Reporting Code
        LMrnw   = scRnwE;              // Auto Renew
        LMpct   = scPctEIL;              // Auto Renew
        LMstat  = scInLmStE;           // Status
        LMuser = q1user;
        LMpgm  = q1pgm;
        LMtime = Timeto6(SystemTime());
        LMdt   = Dateto7(SystemDate());
        LMitim = Timeto6(SystemTime());
        LMidt  = Dateto7(SystemDate());
        LMiusr = q1user;
        LMipgm = q1pgm;
       EndSr;

       // Writing increased limit details subroutine
       BegSr WrtInLt;
        Clear plm0p;
        Exsr MoveScDb;
        Write plm0p;
       EndSr;

      /end-free
     P ProcAddInLt     E

       //===================================================================
       // ProcAddLsCn  - process addition of Loss Constant
       //===================================================================
     P ProcAddLsCn     B
     D ProcAddLsCn     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdLsCnDtl;
       mode = 'A';
       isexit = *off;
       BtnLsCnCan = *off;
       LsCnEdt = *off;

       Dow BtnLsCnCan = *off and isexit = *off;

          scLsCnHdg = 'Add Loss Constant Information';
          visLsCnAdS = *On;                           // Show Save/Add next Button

          Exfmt rcdLsCnDtl;

          Select;

          // Save/Add Next Button
          When BtnLsCnAdS = *on;
             BtnLsCnAdS = *off;
             If ValidateLsCn(mode);
                Exsr WrtLsCn;
          //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdLsCnDtl;
                BtnLsCnCan = *off;
             EndIf;

          // Save Button
          When BtnLsCnSav = *on;
             BtnLsCnSav = *off;
             If ValidateLsCn(mode);
                Exsr WrtLsCn;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnLsCnCan = *on;
             BtnLsCnCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        sc_SCco#  = inCo#;                 // Company Number
        sc_SCfnd  = inFnd;                 // Group Number
        sc_SCst   = scStELC;              // State
        sc_SCefdt = dtCYMD(scEfDtELC);     // Effective date
        sc_SCprbs = ScPrbsELC;             //Premium Basis Amount
        sc_SCstat = scStatELC;            // Statistical Code
        sc_SCuser = q1user;
        sc_SCpgm  = q1pgm;
        sc_SCtime = Timeto6(SystemTime());
        sc_SCdt   = Dateto7(SystemDate());
        sc_SCidt  = Dateto7(SystemDate());
        sc_SCiusr = q1user;
       EndSr;

       // Writing Loss Constant subroutine
       BegSr WrtLsCn;
        Clear psc0p;
        Exsr MoveScDb;
        Write psc0p;
       EndSr;

      /end-free
     P ProcAddLsCn     E

       //===================================================================
       // ProcAddPolPg - process addition of Policy Page - 3c
       //===================================================================
     P ProcAddPolPg    B
     D ProcAddPolPg    PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdPolPDtl;
       mode = 'A';
       isexit = *off;
       BtnPolPCan = *off;

       Dow BtnPolPCan = *off and isexit = *off;
          scPolPgHdg = 'Add Policy Page Information';
          Exfmt rcdPolPDtl;

          Select;
          // Saved Button
          When BtnPolPSav = *on;
             BtnPolPSav = *off;
             If ValidatePolPg(mode);
                Exsr WrtPolPg;
                isexit = *on;
             EndIf;
          // Cancel Button
          When BtnPolPCan = *on;
             BtnPolPCan = *off;
             isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        F4co#   = inCo#;                 // Company Number
        F4fnd   = inFnd;                 // Group Number
        F4stat  = 'A';                  // Status
        F4key   = scVerbType;           // Key
        F4efdt  = dtCYMD(scEfDtEPP);

        Select;
        When scVerbType = 'PP3C';
           F4desc = 'Policy Page 3c';
           F4txt1  = scTxt1;
           F4txt2  = scTxt2;
        Other;
           F4desc = *Blanks;
           F4vDesc = scBlWVDsc;
        Endsl;

        F4user = q1user;
        F4pgm  = q1pgm;
        F4time = Timeto6(SystemTime());
        F4dt   = Dateto7(SystemDate());
        F4iUsr = q1user;
        F4iPgm = q1pgm;
        F4iTim = Timeto6(SystemTime());
        F4iDt  = Dateto7(SystemDate());
       EndSr;

       // Writing Policy Page - 3c subroutine
       BegSr WrtPolPg;
        Clear pf40p;
        Exsr MoveScDb;
        Write pf40p;
       EndSr;

      /end-free
     P ProcAddPolPg    E

       //===================================================================
       // ProcAddPrmDis - process addition of Premium Discount
       //===================================================================
     P ProcAddPrmDis   B
     D ProcAddPrmDis   PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdPrDiDtl;
       mode = 'A';
       isexit = *off;
       BtnPrDiCan = *off;

       Dow BtnPrDiCan = *off and isexit = *off;

          scPrDiHdg = 'Add Premium Discount Information';
          visPrDiAdS = *On;                           // Show Save/Add next Button

          Exfmt rcdPrDiDtl;

          Select;
          // Save/Add Next Button
          When BtnPrDiAdS = *on;
             BtnPrDiAdS = *off;
             If ValidatePrmDis(mode);
                Exsr WrtPrmDis;
          //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdPrDiDtl;
                BtnPrDiCan = *off;
             EndIf;

          // Save Button
          When BtnPrDiSav = *on;
             BtnPrDiSav = *off;
             If ValidatePrmDis(mode);
                Exsr WrtPrmDis;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnPrDiCan = *on;
             BtnPrDiCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        DSco#   = inCo#;                 // Company Number
        DSfnd   = inFnd;                 // Group Number
        DSst    = scStE;                // State
        DStype  = scTypeE;              // Type
        DSpct   = scPctE;
        DSlmt   = scLmtE;
        DScode  = scCodeE;
        DSefdt  = dtCYMD(scEfDtE);

        DSuser = q1user;
        DSpgm  = q1pgm;
        DStime = Timeto6(SystemTime());
        DSdt   = Dateto7(SystemDate());
        DSiUsr = q1user;
        DSiPgm = q1pgm;
        DSiTim = Timeto6(SystemTime());
        DSiDt  = Dateto7(SystemDate());
       EndSr;

       // Writing Premium Discount subroutine
       BegSr WrtPrmDis;
        Clear pds0p;
        Exsr MoveScDb;
        Write pds0p;
       EndSr;

      /end-free
     P ProcAddPrmDis   E

       //===================================================================
       // ProcAddShRt - process addition of Short Rate Masters Info
       //===================================================================
     P ProcAddShRt     B
     D ProcAddShRt     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdSRTDtl;
       mode = 'A';
       isexit = *off;
       BtnSRTCan = *off;
       SRTEdt = *off;

       Dow BtnSRTCan = *off and isexit = *off;

          scSRTHdg = 'Add Short Rate Information';

          If SRTEdt = *on;      // User has pressed Add Button to Add detail records
             // Load Short Rate Detail Subfile Grid
             LoadShRtDtGrid();
          Else;
             rrnSRTDE = 0;
             dspSRTDE = *off;
             clrSRTDE = *on;
             Write rcdSRTDtl;
             clrSRTDE = *off;
             dspSRTDE = *on;
          EndIf;

          Exfmt rcdSRTDtl;

          Select;

          // Add Short Rate Details Info
          When BtnSRTDAdd = *On;
             BtnSRTDAdd = *Off;
             If ValidateShRt(mode);
                ProcAddShRtDt();
                SRTEdt = *on;
             EndIf;

          // Save Button
          When BtnSRTSav = *on;
             BtnSRTSav = *off;
             If ValidateShRt(mode);
                Exsr WrtSRT;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnSRTCan = *on;
             BtnSRTCan = *off;
             isexit = *on;
          // If User has Added any detail records in Subfile, then add record to master file
             If SRTEdt = *on and TotalSfRcd <> 0;
                Exsr WrtSRT;
             Endif;

          Other;
             ProcOthrShRtDt();              //other option for Short Rate Details

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        m_STco# = inCo#;                 // Company Number
        m_STfnd = inFnd;                 // Group Number
        m_STst  = scStESRT;              // State
        m_STefdt = dtCYMD(scEfDtESRT);     // Effective Date
        m_STuser = q1user;
        m_STpgm = q1pgm;
        m_STtime = Timeto6(SystemTime());
        m_STdt = Dateto7(SystemDate());
        m_STitim = Timeto6(SystemTime());
        m_STidt = Dateto7(SystemDate());
        m_STiusr = q1user;
        m_STipgm = q1pgm;
       EndSr;

       // Writing State Compensation Benefit Info
       BegSr WrtSRT;
        Clear pst0p;
        Exsr MoveScDb;
        Write pst0p;
       EndSr;

      /end-free
     P ProcAddShRt     E

       //===================================================================
       // ProcAddShRtDt - process addition of Short Rate Details Info
       //===================================================================
     P ProcAddShRtDt   B
     D ProcAddShRtDt   PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdSRTDDtl;
       mode = 'A';
       isexit = *off;
       BtnSRTDCan = *off;
       SRTDEdt = *Off;

       Dow BtnSRTDCan = *off and isexit = *off;

          scSRTDHdg = 'Add Short Rate Details Information';
          visSRTDAdS = *On;                // Show Save/Add Next Button

          Exfmt rcdSRTDDtl;

          Select;

          // Save/Add Next Button
          When BtnSRTDAdS = *On;
             BtnSRTDAdS = *Off;
             If ValidateShRtDt(mode);
                Exsr WrtSRTD;
                //Initialize fields for next add screen if save/addnext is pressed
                Clear scDaysSRDE;
                Clear scPercSRDE;
                SRTDEdt = *Off;
             EndIf;

          // Save Button
          When BtnSRTDSav = *on;
             BtnSRTDSav = *off;
             If ValidateShRtDt(mode);
                Exsr WrtSRTD;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnSRTDCan = *on;
             BtnSRTDCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        STco# = inCo#;                 // Company Number
        STfnd = inFnd;                 // Group Number
        STst  = scStESRT;              // State
        STefdt = dtCYMD(scEfDtESRT);     // Effective Date
        STdays   = scDaysSRDE;
        STperc   = scPercSRDE;
        STuser = q1user;
        STpgm = q1pgm;
        STtime = Timeto6(SystemTime());
        STdt = Dateto7(SystemDate());
        STitim = Timeto6(SystemTime());
        STidt = Dateto7(SystemDate());
        STiusr = q1user;
        STipgm = q1pgm;
       EndSr;

       // Writing State Compensation Benefit Info
       BegSr WrtSRTD;
        Clear pst0p2;
        Exsr MoveScDb;
        Write pst0p2;
       EndSr;

      /end-free
     P ProcAddShRtDt   E

       //===================================================================
       // ProcAddSmDe  - process addition of Small Deductibles
       //===================================================================
     P ProcAddSmDe     B
     D ProcAddSmDe     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdSmDeDtl;
       mode = 'A';
       isexit = *off;
       BtnSmDeCan = *off;
       SmDeEdt = *off;
       scSmDeStE = 'A';

       Dow BtnSmDeCan = *off and isexit = *off;

          // Load Initial Values
          InzAddPrmScVal();
          scIncMdInE = 'B';      // Initialize with Medical and Indemnity

          scSmDeHdg = 'Add Small Deductibles Information';
          visSmDeAdS = *On;                           // Show Save/Add next Button
          //*** Beg Add *** 08/08/22 *******************************************
          htSmDePnl  = 420;
          topBtnSmDe = 520;

          // If type = Gross and Total incurred not checked then enable PV1
          // Disable PV1 Death Benefit fields on Add
          disPv1Db = *on;
          //*** End Add *** 08/08/22 ********************************************

          Exfmt rcdSmDeDtl;

          Select;
          // Save/Add Next Button
          When BtnSmDeAdS = *on;
             BtnSmDeAdS = *off;
             If ValidateSmDe(mode);
                Exsr WrtSmDe;
                //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdSmDeDtl;
                // Load Initial Values
                InzAddPrmScVal();
                scIncMdInE = 'B';      // Initialize with Medical and Indemnity
                BtnSmDeCan = *off;
                SmDeEdt = *off;
                scSmDeStE = 'A';
             EndIf;

          // Save Button
          When BtnSmDeSav = *on;
             BtnSmDeSav = *off;
             If ValidateSmDe(mode);
                Exsr WrtSmDe;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnSmDeCan = *on;
             BtnSmDeCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        DDco#  = inCo#;                 // Company Number
        DDfnd  = inFnd;                 // Group Number
        DDst   = scStESD;              // State
        DDefdt = dtCYMD(scEfDtESD);     // Effective date
        DDcode = scCodeESD;             //Discount Code
        DDdesc = scDescECD;            // Description
        DDtype = scTypeESD;             // Type
        DDpct  = scPctESD;             //Discount Percentage
        DDamag = scAmagE;              //Amount Aggregate
        DDvara = scVaraE;             // Variable
        DDampo = scAmpoE;             // Amount per occurence
        IF scVerbE <> 'C';
           scVerbE = ' ';             // Save Blank for O (per Occurrence)
        EndIf;
        DDverb = scVerbE;               // Verbiage
        DDclas = scClasESD;             // Class Code
        DDalae = scALAEE;              // Include ALAE
        IF scRnwESD <> 'N';
           scRnwESD = ' ';            // Save Blank for Y
        EndIf;
        DDrnw  = scRnwESD;             // Auto Renew
        DDplan = scPlanESD;             // Deductible Plan
        DDincMdIn = scIncMdInE;         // include medical indemnity
        If DDIncMdIn = 'B';
           DDIncMdIn = *Blank;
        Endif;
        DDtoti = scTotiE;              // Use total Incurred
        DDdc01 = scDc01E;              // Deductible Reserves
        DDdc02 = scDc02E;              // Deductible Reserves
        DDdc03 = scDc03E;              // Deductible Reserves
        DDdc04 = scDc04E;              // Deductible Reserves
        DDdc05 = scDc05E;              // Deductible Reserves
        DDdc06 = scDc06E;              // Deductible Reserves
        DDdc07 = scDc07E;              // Deductible Reserves
        DDdc08 = scDc08E;              // Deductible Reserves
        DDdc09 = scDc09E;              // Deductible Reserves
        DDdc10 = scDc10E;              // Deductible Reserves
        //*** Beg Add *** 08/08/22 ********************************************
        If scTypeEsd = 'G' and scTotiE <> 'Y';
           DDpv1Db01 = scPv1Db01;
           DDpv1Db02 = scPv1Db02;
           DDpv1Db03 = scPv1Db03;
           DDpv1Db04 = scPv1Db04;
           DDpv1Db05 = scPv1Db05;
           DDpv1Db06 = scPv1Db06;
           DDpv1Db07 = scPv1Db07;
           DDpv1Db08 = scPv1Db08;
           DDpv1Db09 = scPv1Db09;
           DDpv1Db10 = scPv1Db10;
        Else;
           DDpv1Db01 = *blanks;
           DDpv1Db02 = *blanks;
           DDpv1Db03 = *blanks;
           DDpv1Db04 = *blanks;
           DDpv1Db05 = *blanks;
           DDpv1Db06 = *blanks;
           DDpv1Db07 = *blanks;
           DDpv1Db08 = *blanks;
           DDpv1Db09 = *blanks;
           DDpv1Db10 = *blanks;
           scPv1Db01 = *blanks;
           scPv1Db02 = *blanks;
           scPv1Db03 = *blanks;
           scPv1Db04 = *blanks;
           scPv1Db05 = *blanks;
           scPv1Db06 = *blanks;
           scPv1Db07 = *blanks;
           scPv1Db08 = *blanks;
           scPv1Db09 = *blanks;
           scPv1Db10 = *blanks;
        Endif;
        //*** End Add *** 08/08/22 ********************************************
        DDstat = scSmDeStE;           // Status
        DDuser = q1user;
        DDpgm  = q1pgm;
        DDtime = Timeto6(SystemTime());
        DDdt   = Dateto7(SystemDate());
        DDitim = Timeto6(SystemTime());
        DDidt  = Dateto7(SystemDate());
        DDiusr = q1user;
        DDipgm = q1pgm;
       EndSr;

       // Writing Small Deductibles details subroutine
       BegSr WrtSmDe;
        Clear pdd0p;
        Exsr MoveScDb;
        Write pdd0p;
       EndSr;

      /end-free
     P ProcAddSmDe     E

       //===================================================================
       // ProcOthrCRSD  - Process other Options for Cancellation/Reinsatement
       //===================================================================
     P ProcOthrCRSD    B
     D ProcOthrCRSD    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdCRSDDtl;
       InzCRSDScVal();
       isexit = *off;
       ReadC sflCRSD;

       Dow not %eof;

          Select;
          When scCRSDEdt = *On; // Processing for Edit Cancellation/Reinsatement state def
             scCRSDEdt = *Off;
             mode = 'C';

             Chain(n) scCRSDRRN WTXCP;

             Exsr MovDbSc;

             scCRSDHdg = 'Edit Cancellation/Reinstatement State +
                          Definition Information';
             CRSDEdt = *on;   //disable non-editable field
             svrrnCRSD = rrnCRSD;   //save rrn value

             Dow isexit = *off;
                Exfmt rcdCRSDDtl;

                Select;
                // Save Button
                When BtnCRSDSav = *on;
                   BtnCRSDSav = *off;
                   // Check if WTXCP updated since screen displayed (Cancellation)
                   flgRcdUpdated=RcdUpdatedErr('WTXCP':dbrrn17:'XCdt':'XCtime':
                    'XCuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scCRSDRRN WTXCP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateCRSD(mode);
                         Chain scCRSDRRN WTXCP;
                         Exsr MoveScDb;
                         Update pxc0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnCRSDCan = *on;
                   BtnCRSDCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scCRSDDlt = *On; // Processing for Delete Cancellation/reinstatement state def link
             scCRSDDlt = *Off;

             Chain(e) scCRSDRRN WTXCP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scCRSDRRN WTXCP;
             Exsr MovDbSc;

             scCRSDHdg = 'Delete Cancellation/Reinstatement State +
                          Definition Information';

             Dow isexit = *off;

                Write rcdCRSDDtl;
                isDelresp = PopTwoBtn('Do you want to delete this Cancellation+
                /Reinstatement State Definition?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Delete pxc0p;
                   PopOneBtn('Cancellation/Reinstatement State Definition +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelresp = *Off;
                   svrrnCRSD = rrnCRSD;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WTXCP;

          ReadC sflCRSD;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStECRSD = XCst;
        scEfDtECRS = dtMDYY(XCefdt);
        scNotcE = XCnotc;
        scCxlmE = XCcxlm;
        scCxmnE = XCcxmn;
        scStnmE = XCstnm;
        scDremE = XCdrem;
        scCxflE = XCcxfl;
        scCxdcE = XCcxdc;
        scReflE = XCrefl;
        scRedcE = XCredc;
        scRrcdE = XCrrcd;
        scStcxE = XCstcx;
        sc#1LFE = XC#1lf;
        sc#1LDE = XC#1ld;
        sc#1DRE = XC#1dr;
        sc#2LFE = XC#2lf;
        sc#2LDE = XC#2ld;
        sc#2DRE = XC#2dr;
        sc#3LFE = XC#3lf;
        sc#3LDE = XC#3ld;
        sc#3DRE = XC#3dr;
        sc#4LFE = XC#4lf;
        sc#4LDE = XC#4ld;
        sc#4DRE = XC#4dr;
        scCclfE = XCcclf;
        scCcldE = XCccld;
        scCcrsE = XCccrs;
        scCflfE = XCcflf;
        scCfldE = XCcfld;
        scCscfE = XCcscf;
        scCscdE = XCcscd;
        scCsrfE = XCcsrf;
        scCsrdE = XCcsrd;
        scRclfE = XCrclf;
        scRcldE = XCrcld;
        saveDT  = XCdt;
        saveTM  = XCtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        XCnotc = scNotcE;             //Cancellation Notice Days
        XCcxlm = ScCxlmE;            // Description
        XCcxmn = ScCxmnE;             // Cancellation Limit
        XCstnm = scStnmE;             //State Department Name
        XCdrem = scDremE;             // ACCORD Remark 1 Default
        XCcxfl = scCxflE;             // State Cancellation Form  Folder
        XCcxdc = scCxdcE;             // State Cancellation Form Document
        XCrefl = scReflE;             // State Reinstatement Form Folder
        XCredc = scRedcE;             // State Reinstatement Form Document
        XCrrcd = scRrcdE;             // Require Reinstatement Reason Code
        XCstcx = scStcxE;         // Generate State Cancellation Transaction on State Removal
        XC#1lf = sc#1LFE;              // Folder 1
        XC#1ld = sc#1LDE;              // Document 1
        XC#1dr = sc#1DRE;              // Remarks 1
        XC#2lf = sc#2LFE;              // Folder 2
        XC#2ld = sc#2LDE;              // Document 2
        XC#2dr = sc#2DRE;              // Remarks 2
        XC#3lf = sc#3LFE;              // Folder 3
        XC#3ld = sc#3LDE;              // Document 3
        XC#3dr = sc#3DRE;              // Remarks 3
        XC#4lf = sc#4LFE;              // Folder 4
        XC#4ld = sc#4LDE;              // Document 4
        XC#4dr = sc#4DRE;              // Remarks 4
        XCcclf = scCclfE;              // Cancellation Cover Letter Folder
        XCccld = scCcldE;              // Cancellation Cover Letter Document
        XCccrs = scCcrsE;              // Cancellation Cover Letter Reason
        XCcflf = scCflfE;              // Client Specific Cancellation Confirmation Certificate fld
        XCcfld = scCfldE;              // Client Specific Cancellation Confirmation Certificate Doc
        XCcscf = scCscfE;              // Client Specific Cancellation Certificate Letter fld
        XCcscd = scCscdE;              // Client Specific Cancellation Certificate Letter doc
        XCcsrf = scCsrfE;              // Client Specific Reinstatement Certificate Letter fld
        XCcsrd = scCsrdE;              // Client Specific Reinstatement Certificate Letter doc
        XCrclf = scRclfE;              // Reinstatement Cover Letter fld
        XCrcld = scRcldE;              // Reinstatement Cover Letter doc
        XCuser = q1user;
        XCpgm  = q1pgm;
        XCtime = Timeto6(SystemTime());
        XCdt   = Dateto7(SystemDate());
        saveDT = XCdt;
        saveTM = XCtime;
       EndSr;

      /end-free
     P ProcOthrCRSD    E

       //===================================================================
       // ProcOthrEDC - Process other Options for Endorsement Controls
       //===================================================================
     P ProcOthrEDC     B
     D ProcOthrEDC     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdEDCDtl;
       isexit = *off;
       ReadC sflEDC;

       Dow not %eof;

          Select;
          When scEDCEdt = *On; // Processing for Edit Endorsement Controls
             scEDCEdt = *Off;
             mode = 'C';

             Chain(n) scEDCRRN WMNXP;

             Exsr MovDbSc;

             scEDCHdg = 'Edit Endorsement Controls Information';
             EDCEdt = *on ;   //disable non-editable field
             svrrnEDC = rrnEDC;   //save rrn value

             Dow isexit = *off;
                Exfmt rcdEDCDtl;

                Select;
                // Save Button
                When BtnEDCSav = *on;
                   BtnEDCSav = *off;
                   // Check if WMNXP updated since screen displayed (Endorsement Controls)
                   flgRcdUpdated=RcdUpdatedErr('WMNXP':dbrrn18:'NXdt':'NXtime':
                    'NXuser':saveDT2:saveTM2:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scEDCRRN WMNXP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateEDC(mode);
                         Chain scEDCRRN WMNXP;
                         Exsr MoveScDb;
                         Update pnx0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnEDCCan = *on;
                   BtnEDCCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scEDCDlt = *On; // Processing for Delete Endorsement Controls
             scEDCDlt = *Off;

             Chain(e) scEDCRRN WMNXP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scEDCRRN WMNXP;
             Exsr MovDbSc;

             scEDCHdg = 'Delete Endorsement Controlse Info';

             Dow isexit = *off;

                Write rcdEDCDtl;
                isDelresp = PopTwoBtn('Do you want to delete this +
                Endorsement Controls Information?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                   Delete pnx0p;
                   PopOneBtn('Endorsement Controls Information +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelResp = *Off;
                   svrrnEDC = rrnEDC;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WMNXP;

          ReadC sflEDC;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStEEDC =NXst;
        scEfdtEEDC = dtMDYY(NXefdt);
        scEDCStE = NXstat;
        scPrcfE = NXprcf;
        scPrcdE = dtMDYY(NXprcd);
        scCpctE = NXcpct;
        scRcefE = NXrcef;
        scRcedE = dtMDYY(NXrced);
        scLpctE = NXlpct;
        scSplnE = NXspln;
        scLlmtE = NXllmt;
        scHlmtE = NXhlmt;
        scDeptE = NXdept;
        If NXdpar <> *zeros or NXdppn <> *zeros;
           scPhn#EEDC = %int(%editc(NXdpar:'X') + %editc(NXdppn:'X'));
        Endif;
        scOfftE = NXofft;
        scDedfE = NXdedf;
        saveDT2 = NXdt;
        saveTM2 = NXtime;
       //***** Beg Add *** 11/17/22 **********************************
        scInsrCxDy = NXinsrCxDy;
        scInsdCxDy = NXinsdCxDy;
       //***** End Add *** 11/17/22 **********************************
       //***** Beg Add *** 11/13/23 *** WC2929 ***********************
        ScStatCdMe = NXStatCdMe;
       //***** End Add *** 11/13/23 *** WC2929 ***********************
       //***** Beg Add *** 02/14/24 **********************************
        scExWc99E = NXexcl99;
       //***** End Add *** 02/14/24 **********************************
       //***** Beg Add *** 02/19/24 **********************************
        scMCareA = NXMCareA;
        scMCareB = NXMCareB;
        scWgLossA = NXWgLossA;
        scWgLossB = NXWgLossB;
        scIBenDisA = NXIBenDisA;
        scIBenDisB = NXIBenDisB;
        scDBenA = NXDBenA;
        scDBenB = NXDBenB;
        scAdmLglA = NXAdmLglA;
        scAdmLglB = NXAdmLglB;
        scMiscA = NXMiscA;
        scMiscB = NXMiscB;
       //***** End Add *** 02/19/24 **********************************
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        NXstat = scEDCStE;              // Status
        NXprcf = scPrcfE;              // Pending Rate Change Flag
        NXprcd = dtCYMD(scPrcdE);       // Pending Rate Change Date
        NXcpct = scCpctE;              //State Coverage Percent Change
        NXrcef = scRcefE;              //Rate Change Flag
        NXrced = dtCYMD(scRcedE);       // Rate Change Date
        NXlpct = scLpctE;             // Longshore and Harbor Workers Coverage  Percentage
        NXspln = scSplnE;             // Suppress Policy Notes
        NXllmt = scLlmtE;               // Lower Salary Limit
        NXhlmt = scHlmtE;               // Higher Salary Limit
        NXdept = scDeptE;               // Dept Name to print on State form
        If scPhn#EEDC > *zeros;
           NXdpar  = %dec(%subst(%editc(scPhn#EEDC:'X'):1:3):3:0); // Phone  Area Code
           NXdppn  = %dec(%subst(%editc(scPhn#EEDC:'X'):4):7:0);   // Phone  Number
        EndIf;
        If scPhn#EEDC = *zeros;
           NXdpar = *zeros;
           NXdppn = *zeros;
        EndIf;
        NXofft = scOfftE;               // Require Owner/Officer Title
        NXdedf = scDedfE;               // WC020602 Consder adjust expenses (Y/N)
        NXuser = q1user;
        NXpgm  = q1pgm;
        NXtime = Timeto6(SystemTime());
        NXdt   = Dateto7(SystemDate());
        saveDT2 = NXdt;
        saveTM2 = NXtime;
       //***** Beg Add *** 11/17/22 **********************************
        NXinsrCxDy = scInsrCxDy;
        NXinsdCxDy = scInsdCxDy;
       //***** End Add *** 11/17/22 **********************************
       //***** Beg Add *** 11/13/23 *** WC2929 ***********************
        NXStatCdMe = ScStatCdMe;
       //***** End Add *** 11/13/23 *** WC2929 ***********************
       //***** Beg Add *** 02/14/24 **********************************
        NXexcl99 = scExWc99E;
       //***** End Add *** 02/14/24 **********************************
       //***** Beg Add *** 02/19/24 **********************************
        NXMCareA = scMCareA;
        NXMCareB = scMCareB;
        NXWgLossA = scWgLossA;
        NXWgLossB = scWgLossB;
        NXIBenDisA = scIBenDisA;
        NXIBenDisB = scIBenDisB;
        NXDBenA = scDBenA;
        NXDBenB = scDBenB;
        NXAdmLglA = scAdmLglA;
        NXAdmLglB = scAdmLglB;
        NXMiscA = scMiscA;
        NXMiscB = scMiscB;
       //***** End Add *** 02/19/24 **********************************
       EndSr;

      /end-free
     P ProcOthrEDC     E

       //===================================================================
       // ProcOthrExCo  - Process other Options for Expense Constant
       //===================================================================
     P ProcOthrExCo    B
     D ProcOthrExCo    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdExCoDtl;
       isexit = *off;
       ReadC sflExpCon;

       Dow not %eof;

          Select;
          When scExCoEdt = *On; // Processing for Edit Expense Constant Info
             scExCoEdt = *Off;
             mode = 'C';

             Chain(n) scExCoRRN WDECP;

             Exsr MovDbSc;

             scExCoHdg = 'Edit Expense Constant Information';
             ExpConEdt = *on ;   //disable non-editable field
             svrrnExpCon = rrnExpCon;   //Save Rrn value  for edited record

             Dow isexit = *off;
                Exfmt rcdExCoDtl;

                Select;
                // Save Button
                When BtnExCoSav = *on;
                   BtnExCoSav = *off;
                   // Check if WDECP updated since screen displayed (Expense Constant)
                   flgRcdUpdated=RcdUpdatedErr('WDECP':dbrrn19:'ECdt':'ECtime':
                    'ECuser':saveDT3:saveTM3:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scExCoRRN WDECP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateExCo(mode);
                         Chain scExCoRRN WDECP;
                         Exsr MoveScDb;
                         Update pec0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnExCoCan = *on;
                   BtnExCoCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WDECP;

          Endsl;

          ReadC sflExpCon;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStEEC = ECst;
        scEfDtEEC = dtMDYY(ECefdt);
        scExcoE = ECexco;
        scMpecE = ECmpec;
        scSfulE = ECsful;
        scExc2E = ECexc2;
        scStprE = ECstpr;
        scExc3E = ECexc3;
        scStp3E = ECstp3;
        saveDT3 = ECdt;
        saveTM3 = ECtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        ECexco = scExcoE;               // Amount
        ECmpec = scMpecE;               // Minimum Amount
        ECsful = scSfulE;               // Charge Full Expense Constant On Short Term Policies
        ECexc2 = scExc2E;               // Expense Constant
        ECstpr = scStprE;              // Standard Premium
        ECexc3 = scExc3E;               // Expense Constant
        ECstp3 = scStp3E;              // Standard Premium
        ECuser = q1user;
        ECpgm  = q1pgm;
        ECtime = Timeto6(SystemTime());
        ECdt   = Dateto7(SystemDate());
        saveDT3 = ECdt;
        saveTM3 = ECtime;
       EndSr;

      /end-free
     P ProcOthrExCo    E

       //===================================================================
       // ProcOthrExSt - Process other Options for Excluded States
       //===================================================================
     P ProcOthrExSt    B
     D ProcOthrExSt    PI
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdExStDtl;
       isexit = *off;
       ReadC sflExSt;

       Dow not %eof;

          Select;
          // Processing for Delete Excluded States info
          When scExStDlt = *On;
             ScExStDlt = *Off;

             Chain(e) scExStRRN wdxsp;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scExStRRN wdxsp;
             scStEExSt = XSst;

             scExStHdg = 'Delete Excluded States/WCPOLS Information';

             Dow isexit = *off;

                Write rcdExStDtl;
                isDelResp = PopTwoBtn('Do you want to delete this Excluded +
                            States/WCPOLS Info?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                   Delete pxs0p;
                   PopOneBtn('Excluded States Info deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelResp = *Off;
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock wdxsp;

          ReadC sflExSt;
       Enddo;

      /end-free
     P ProcOthrExSt    E

       //===================================================================
       // ProcOthrHrEx  - Process other Options for Harbor Exposure
       //===================================================================
     P ProcOthrHrEx    B
     D ProcOthrHrEx    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdHrExDtl;
       isexit = *off;
       ReadC sflHrEx;

       Dow not %eof;

          Select;
          When scHrExEdt = *On;        // Processing for Edit Harbor Exposure
             scHrExEdt = *Off;

             mode = 'C';

             Chain(n) scHrExRRN WDHEP;

             Exsr MovDbSc;

             scHrExHdg = 'Edit Harbor Exposure Information';
             HrExEdt = *on ;           //disable non-editable field
             svrrnHrEx = rrnHrEx;      //Save Rrn value for edited record

             Dow isexit = *off;
                Exfmt rcdHrExDtl;

                Select;
                // Save Button
                When BtnHrExSav = *on;
                   BtnHrExSav = *off;
                   // Check if WDHEP updated since screen displayed (Harbor Exposure)
                   flgRcdUpdated=RcdUpdatedErr('WDHEP':dbrrn20:'HEdt':'HEtime':
                    'HEuser':saveDT4:saveTM4:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scHrExRRN WDHEP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateHrEx(mode);
                         Chain scHrExRRN WDHEP;
                         Exsr MoveScDb;
                         Update phe0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnHrExCan = *on;
                   BtnHrExCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scHrExDlt = *On; // Processing for Delete Harbor Exposure Link
             scHrExDlt = *Off;

             Chain(e) scHrExRRN WDHEP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scHrExRRN WDHEP;
             Exsr MovDbSc;

             scHrExHdg = 'Delete Harbor Exposure Definition';

             Dow isexit = *off;
                Write rcdHrExDtl;
                isDelresp = PopTwoBtn('Do you want to delete this +
                Harbor Exposure Definition?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                   Delete phe0p;
                   PopOneBtn('Harbor Exposure Definition +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelResp = *Off;
                   svrrnHrEx = rrnHrEx;      //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WDHEP;

          ReadC sflHrEx;
       Enddo;

       // Subroutine to move database fields into screen fields
       BegSr MovDbSc;
        scstEHR   = HEst;                // State
        scEfdtEHR = dtMDYY(HEefdt);      // Effective date
        scLhnfE   = HElhnf;              // L&H LOAD,NON-F
        scHxmnE   = HEhxmn;              // Harbor Exposure Minimum
        scHemcE   = HEhemc;              // Harbor Exposure Minimum Status
        saveDT4   = HEdt;
        saveTM4   = HEtime;
       EndSr;

       // Subroutine to move Screen fields into database fields
       BegSr MoveScDb;
        HElhnf  = scLhnfE;              // L&H LOAD,NON-F
        HEhxmn  = scHxmnE;              // Harbor Exposure Minimum
        HEhemc  = scHemcE;              // Harbor Exposure Minimum Status
        HEpgm   = q1pgm;
        HEuser  = q1user;
        HEdt    = Dateto7(SystemDate());
        HEtime  = Timeto6(SystemTime());
        saveDT4 = HEdt;
        saveTM4 = HEtime;
       EndSr;

      /end-free
     P ProcOthrHrEx    E

       //===================================================================
       // ProcOthrInFe  - Process other Options for Installment Fees
       //===================================================================
     P ProcOthrInFe    B
     D ProcOthrInFe    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdInFeDtl;
       isexit = *off;
       ReadC sflInFe;

       Dow not %eof;

          Select;
          When scInFeEdt = *On; // Processing for Edit Installment Fees info

             scInFeEdt = *Off;
             mode = 'C';
             InFeEdt = *on ;   //disable non-editable field
             svrrnInFe = rrnInFe;   //Save Rrn value  for edited record

             Chain(n) scInFeRRN WMIFP;

             Exsr MovDbSc;

            // Set selection criteria for A/R Codes
             scIfblECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' + 'TTstat=' +
              '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''F''';

             scIfpdECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' + 'TTstat=' +
              '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

             scIftrECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' + 'TTstat=' +
              '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

             scIftcECH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' + 'TTstat=' +
              '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

             scInFeHdg = 'Edit Installment Fees Information';

             Dow isexit = *off;
                Exfmt rcdInFeDtl;

                Select;
                // Save Button
                When BtnInFeSav = *on;
                   BtnInFeSav = *off;
                   // Check if WMIFP updated since screen displayed (Installment Fees)
                   flgRcdUpdated=RcdUpdatedErr('WMIFP':dbrrn21:'IFdt':'IFtime':
                    'IFuser':saveDT5:saveTM5:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scInFeRRN WMIFP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateInFe(mode);
                         Chain scInFeRRN WMIFP;
                         Exsr MoveScDb;
                         Update pif0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnInFeCan = *on;
                   BtnInFeCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WMIFP;

          Endsl;

          ReadC sflInFe;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStEIF = IFst;
        scEfDtEIF = dtMDYY(IFefdt);
        scExpdEIF = dtMDYY(IFexpd);
        ScIfblE = IFifbl;
        ScIfeeE = IFifee;
        scIsfeE = IFisfe;
        scIfpdE = IFifpd;
        scIftrE = IFiftr;
        scIftcE = IFiftc;
        saveDT5 = IFdt;
        saveTM5 = IFtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        IFexpd = dtCYMD(scExpdEIF);     // Expiration date
        IFifee = scIfeeE;             //Estimating Fee
        IFisfe = scIsfeE;             //Self Reporting Fee
        IFifbl = scIfblE;            // Bill A/R Code
        IFifpd = scIfpdE;             // Paid A/R Code
        IFiftr = scIftrE;             //Flat Cancellation A/R Transfer Code
        IFiftc = scIftcE;              //Payment A/R Transfer Code
        IFuser = q1user;
        IFpgm  = q1pgm;
        IFtime = Timeto6(SystemTime());
        IFdt   = Dateto7(SystemDate());
        saveDT5 = IFdt;
        saveTM5 = IFtime;
       EndSr;

      /end-free
     P ProcOthrInFe    E

       //===================================================================
       // ProcOthrInLt  - Process other Options for Increased limits
       //===================================================================
     P ProcOthrInLt    B
     D ProcOthrInLt    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdInLtDtl;
       isexit = *off;
       ReadC sflIncLmt;

       Dow not %eof;

          Select;
          When scInLmtEdt = *On; // Processing for Edit Increased Limits Info
             scInLmtEdt = *Off;

             // Load Initial Values
             InzRatingScVal();

             mode = 'C';

             Chain(n) scInLmtRRN WTLMP;

             Exsr MovDbSc;

             scInLtHdg = 'Edit Increased Limits Information';
             IncLmtEdt = *on ;   //disable non-editable field
             svrrnIncLmt = rrnIncLmt;  //Save Rrn value  for edited record

             Dow isexit = *off;
                Exfmt rcdInLtDtl;

                Select;
                // Save Button
                When BtnInLtSav = *on;
                   BtnInLtSav = *off;
                   // Check if WTLMP updated since screen displayed (Increased Limits)
                   flgRcdUpdated=RcdUpdatedErr('WTLMP':dbrrn22:'LMdt':'LMtime':
                    'LMuser':saveDT6:saveTM6:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scInLmtRRN WTLMP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateInLt(mode);
                         Chain scInLmtRRN WTLMP;
                         Exsr MoveScDb;
                         Update plm0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnInLtCan = *on;
                   BtnInLtCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scInLmtDlt = *On; // Processing for Delete Increased limits Link
             scInLmtDlt = *Off;

             Chain(e) scInLmtRRN WTLMP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scInLmtRRN WTLMP;
             Exsr MovDbSc;

             scInLtHdg = 'Delete Increased limits Information';

             Dow isexit = *off;

                Write rcdInLtDtl;
                isDelresp = PopTwoBtn('Do you want to delete this Increased +
                            Limit?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                    Delete plm0p;
                    PopOneBtn('Increased Limit Deleted!':'Delete':'Ok');
                    isexit = *on;
                When isDelResp = *Off;
                   svrrnIncLmt = rrnIncLmt;  //Save Rrn value  for edited record
                   isexit = *on;
                EndSl;
             EndDo;
          //*** Beg Add ***** 12/27/23 *****************************
          When scInLmtCpy = *On; // Processing for Copy Increased Limits Info
             scInLmtCpy = *Off;

             // Load Initial Values
             InzRatingScVal();

             mode = 'P';

             Chain(n) scInLmtRRN WTLMP;

             Exsr MovDbSc;

             scInLtHdg = 'Copy Increased Limits Information';
             IncLmtEdt = *off;         // enable fields
             visInLtAdS = *off;

             Dow isexit = *off;
                Exfmt rcdInLtDtl;

                Select;
                // Save Button
                When BtnInLtSav = *on;
                   BtnInLtSav = *off;
                   If ValidateInLt(mode);
                      Exsr MoveScDb;
                      write plm0p;
                      isexit = *on;
                   EndIf;

                // Cancel Button
                When BtnInLtCan = *on;
                   BtnInLtCan = *off;
                   svrrnIncLmt = rrnIncLmt;  //Save Rrn value for copy cancel
                   isexit = *on;
                EndSl;
             EndDo;
          //*** End Add ***** 12/27/23 *****************************
          Endsl;

          Unlock WTLMP;

          ReadC sflIncLmt;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scEfDtEIL = dtMDYY(LMefdt);
        scStEIL = LMst;
        scKeyE = LMkey;
        scDescEIL = LMdesc;
        scInLmStE = LMstat;           // Status
        //*** Beg Add ***** 12/27/23 *****************************
        If mode = 'P';
           scInLmStE = 'A';
        Endif;
        //*** End Add ***** 12/27/23 *****************************
        scAccE = LMacc;
        scDisE = LMdis;
        scAggE = LMagg;
        scMinE= LMmin;
        scMineE= LMmine;
        scPctEIL = LMpct;
        scclasE = LMclas;
        scRnwE = LMrnw;
        saveDT6 = LMdt;
        saveTM6 = LMtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        LMdesc = scDescEIL;               // Amount
        LMstat = scInLmStE;        // Status
        LMacc = scAccE;               // Minimum Amount
        LMdis  = scdisE;               // Charge Full Expense Constant On Short Term Policies
        LMagg = scAggE;               // Expense Constant
        LMmin = scMinE;              // Standard Premium
        LMmine= scMineE;              // Standard Premium
        LMpct = scPctEIL;               // Expense Constant
        LMclas = scClasE;              // Standard Premium
        LMrnw = scRnwE;              // Standard Premium
        LMuser = q1user;
        LMpgm  = q1pgm;
        LMtime = Timeto6(SystemTime());
        LMdt   = Dateto7(SystemDate());
        //*** Beg Add ***** 12/27/23 *****************************
        If mode = 'P';
           LMst    = scStEIL;
           LMefdt  = dtCYMD(scEfDtEIL);
           LMkey   = scKeyE;
           LMitim = Timeto6(SystemTime());
           LMidt  = Dateto7(SystemDate());
           LMiusr = q1user;
           LMipgm = q1pgm;
        Endif;
        //*** End Add ***** 12/27/23 *****************************
        saveDT6 = LMdt;
        saveTM6 = LMtime;
       EndSr;

      /end-free
     P ProcOthrInLt    E

       //===================================================================
       // ProcOthrLsCn  - Process other Options for Loss Constant
       //===================================================================
     P ProcOthrLsCn    B
     D ProcOthrLsCn    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdLsCnDtl;
       isexit = *off;
       ReadC sflLsCn;

       Dow not %eof;

          Select;
          When scLsCnEdt = *On; // Processing for Edit Loss Constant info

             scLsCnEdt = *Off;
             mode = 'C';
             LsCnEdt = *on ;        //disable non-editable field
             svrrnLsCn = rrnLsCn;   //Save Rrn value  for edited record

             Chain(n) scLsCnRRN WDSCP;

             Exsr MovDbSc;

             scLsCnHdg = 'Edit Loss Constant Information';

             Dow isexit = *off;
                Exfmt rcdLsCnDtl;

                Select;
                // Save Button
                When BtnLsCnSav = *on;
                   BtnLsCnSav = *off;
                   // Check if WDSCP updated since screen displayed (Loss Constant)
                   flgRcdUpdated=RcdUpdatedErr('WDSCP':dbrrn23:'SCdt':'SCtime':
                    'SCuser':saveDT7:saveTM7:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scLsCnRRN WDSCP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateLsCn(mode);
                         Chain scLsCnRRN WDSCP;
                         Exsr MoveScDb;
                         Update psc0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnLsCnCan = *on;
                   BtnLsCnCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WDSCP;

          Endsl;

          ReadC sflLsCn;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStELC =  sc_SCst;
        scEfDtELC  = dtMDYY(sc_SCefdt);
        scPrbsELC = sc_SCprbs;
        ScStatELC  = sc_SCstat;
        saveDT7 = sc_SCdt;
        saveTM7 = sc_SCtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        sc_SCprbs = scPrbsELC;            // Premium Basis Amount
        sc_SCstat = ScStatELC;             // Statistical Code
        sc_SCuser = q1user;
        sc_SCpgm  = q1pgm;
        sc_SCtime = Timeto6(SystemTime());
        sc_SCdt   = Dateto7(SystemDate());
        saveDT7 = sc_SCdt;
        saveTM7 = sc_SCtime;
       EndSr;

      /end-free
     P ProcOthrLsCn    E

       //===================================================================
       // ProcOthrPolPg - Process other Options for Policy Page - 3c
       //===================================================================
     P ProcOthrPolPg   B
     D ProcOthrPolPg   PI
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdPolPDtl;
       isexit = *off;
       ReadC sflPolPg;

       Dow not %eof;

          Select;
          // Processing for Delete Policy Page - 3c info
          When scPolPgDlt = *On;
             ScPolPgDlt = *Off;

             Chain(e) scPolPgRRN wdf4p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scPolPgRRN wdf4p;
             scEfDtEPP = dtMDYY(F4efdt);
             scVerbType = F4key;
             scTxt1 = F4txt1;
             scTxt2 = F4txt2;
             scBlWVDsc = F4vDesc;

             scPolPgHdg = 'Delete Policy Page Information';

             Dow isexit = *off;

                Write rcdPolPDtl;
                isDelResp = PopTwoBtn('All Processed Policies must be +
                            Regenerated, if you continue with Delete. +
                            Do you want to continue with Delete. +
                            ':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                   Exsr DltPolPg;
                   PopOneBtn('Policy Page Info deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelResp = *Off;
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock wdf4p;

          ReadC sflPolPg;
       Enddo;

       // Delete Policy Page - 3c subroutine
       BegSr DltPolPg;
        F4stat  = 'D';                  // Status
        F4user = q1user;
        F4pgm  = q1pgm;
        F4time = Timeto6(SystemTime());
        F4dt   = Dateto7(SystemDate());

        Update pf40p;
       EndSr;

      /end-free
     P ProcOthrPolPg   E

       //===================================================================
       // ProcOthrPrmDis - Process other Options for Premium Discount
       //===================================================================
     P ProcOthrPrmDis  B
     D ProcOthrPrmDis  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdPrDiDtl;
       isexit = *off;
       ReadC sflPrmDis;

       Dow not %eof;

          // Processing for Edit Premium Discount Link
          Select;
          When scPrDiEdt = *On;
             scPrDiEdt = *Off;
             mode = 'C';

             Chain(n) scPrDiRRN wmdsp;
             scStE = DSst;
             scEfdtE = dtMDYY(DSefdt);
             scCodeE = DScode;
             scLmtE = DSlmt;
             scTypeE = DStype;
             scPctE  = DSpct;
             saveDT8 = DSdt;
             saveTM8 = DStime;

             scPrDiHdg = 'Edit Premium Discount Information';
             PrmDisEdt = *on;
             svrrnPrmDis = rrnPrmDis;   //save rrn value

             Dow isexit = *off;
                Exfmt rcdPrDiDtl;

                Select;
                // Save Button
                When BtnPrDiSav = *on;
                   BtnPrDiSav = *off;
                   // Check if WMDSP updated since screen displayed (Premium Discount)
                   flgRcdUpdated=RcdUpdatedErr('WMDSP':dbrrn24:'DSdt':'DStime':
                    'DSuser':saveDT8:saveTM8:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scPrDiRRN wmdsp;
                      scStE = DSst;
                      scEfdtE = dtMDYY(DSefdt);
                      scCodeE = DScode;
                      scLmtE = DSlmt;
                      scTypeE = DStype;
                      scPctE  = DSpct;
                      saveDT8 = DSdt;
                      saveTM8 = DStime;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidatePrmDis(mode);
                         Chain scPrDiRRN wmdsp;
                         Exsr MoveScDb;
                         Update pds0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnPrDiCan = *on;
                   BtnPrDiCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          // Processing for Delete premium discount info
          When scPrDiDlt = *On;
             scPrDiDlt = *Off;

             Chain(e) scPrDiRRN wmdsp;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scPrDiRRN wmdsp;
             scStE = DSst;
             scEfdtE = dtMDYY(DSefdt);
             scCodeE = DScode;
             scLmtE = DSlmt;
             scTypeE = DStype;
             scPctE  = DSpct;

             scPrDiHdg = 'Delete Premium Discount Information';

             Dow isexit = *off;

                Write rcdPrDiDtl;
                isDelResp = PopTwoBtn('Do you want to delete this Premium +
                            Discount Info?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                   Delete pds0p;
                   PopOneBtn('Premium Discount Info deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelResp = *Off;
                   svrrnPrmDis = rrnPrmDis;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          //*** Beg Add ***** 12/27/23 *****************************************
          When scPrDiCpy = *On;
             scPrDiCpy = *Off;
             mode = 'P';

             Chain(n) scPrDiRRN wmdsp;
             scStE = DSst;
             scEfdtE = dtMDYY(DSefdt);
             scCodeE = DScode;
             scLmtE = DSlmt;
             scTypeE = DStype;
             scPctE  = DSpct;

             scPrDiHdg = 'Copy Premium Discount Information';
             PrmDisEdt = *off;
             visPrDiAdS = *off;

             Dow isexit = *off;
                Exfmt rcdPrDiDtl;

                Select;
                // Save Button
                When BtnPrDiSav = *on;
                   BtnPrDiSav = *off;
                   If ValidatePrmDis(mode);
                      Exsr MoveScDb;
                      Write pds0p;
                      isexit = *on;
                   EndIf;

                // Cancel Button
                When BtnPrDiCan = *on;
                   BtnPrDiCan = *off;
                   svrrnPrmDis = rrnPrmDis;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          //*** End Add ***** 12/27/23 *****************************************
          Endsl;

          Unlock wmdsp;

          ReadC sflPrmDis;
       Enddo;


       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        DStype = scTypeE;              // Type
        DSpct = scPctE;
        DSlmt = scLmtE;

        DSuser = q1user;
        DSpgm = q1pgm;
        DStime = Timeto6(SystemTime());
        DSdt = Dateto7(SystemDate());
        //*** Beg Add ***** 12/27/23 *****************************
        If mode = 'P';
           DSst    = scStE;
           DSefdt  = dtCYMD(scEfDtE);
           DScode  = scCodeE;
           DSiUsr = q1user;
           DSiPgm = q1pgm;
           DSiTim = Timeto6(SystemTime());
           DSiDt  = Dateto7(SystemDate());
        Endif;
        //*** End Add ***** 12/27/23 *****************************
        saveDT8 = DSdt;
        saveTM8 = DStime;
       EndSr;

      /end-free
     P ProcOthrPrmDis  E

       //===================================================================
       // ProcOthrShRt - Process other Options for Short Rate Master Info
       //===================================================================
     P ProcOthrShRt    B
     D ProcOthrShRt    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdSRTDtl;
       isexit = *off;
       ReadC sflSRT;
       cpyMode = *Off;

       Dow not %eof;

          Select;
          When scSRTEdt = *On; // Processing for Edit Short Rate Link
             scSRTEdt = *Off;
             mode = 'C';

             Chain(n) scSRTRRN WMSTP;

             Exsr MovDbSc;

             scSRTHdg = 'Edit Short Rate Information';
             SRTEdt = *on;        //disable non-editable field
             svrrnSRT = rrnSRT;   //save rrn value

             Dow isexit = *off;

                // Load Short Rate Detail Subfile Grid
                LoadShRtDtGrid();

                Exfmt rcdSRTDtl;

                Select;

                // Add Short Rate Details Info
                When BtnSRTDAdd = *On;
                   BtnSRTDAdd = *Off;
                // If ValidateShRt(mode);
                   ProcAddShRtDt();
                // EndIf;

                // Save Button
                When BtnSRTSav = *on;
                   BtnSRTSav = *off;
                // If ValidateShRt(mode);
                //    Exsr MoveScDb;
                //    Update pst0p;
                   isexit = *on;
                // EndIf;

                // Cancel Button
                When BtnSRTCan = *on;
                   BtnSRTCan = *off;
                   isexit = *on;

                Other;
                   ProcOthrShRtDt();              //other option for Short Rate Details

                EndSl;
             EndDo;

          When scSRTDlt = *On; // Processing for Delete Short Rate Link
             scSRTDlt = *Off;

             Chain(e) scSRTRRN WMSTP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scSRTRRN WMSTP;
             Exsr MovDbSc;

             // Load Short Rate Detail Subfile Grid
             LoadShRtDtGrid();

             scSRTHdg = 'Delete Short Rate Info';

             Dow isexit = *off;

                Write rcdSRTDtl;
                isDelresp = PopTwoBtn('Do you want to delete this Short +
                Rate Information?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                    Exsr DelShRtDt;          //Delete Short Rates Detail Records
                    Delete pst0p;            //Delete Short Rates Master Record
                    PopOneBtn('Short Rate Information +
                    Deleted!':'Delete':'Ok');
                    isexit = *on;
                When isDelResp = *Off;
                   svrrnSRT = rrnSRT;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;

          When scSRTCpy = *On; // Processing for Copy Short Rate Link
             scSRTCpy = *Off;
             mode = 'A';
             cpyMode = *On;

             Chain scSRTRRN WMSTP;

             Exsr MovDbSc;

             // Load Short Rate Detail Subfile Grid
             LoadShRtDtGrid();

             scSRTHdg = 'Copy Short Rate Information';
             svrrnSRT = rrnSRT;   //save rrn value

             Dow isexit = *off;
                Exfmt rcdSRTDtl;

                Select;
                // Save Button
                When BtnSRTSav = *on;
                   BtnSRTSav = *off;
                   If ValidateShRt(mode);
                      Exsr CopyData;
                      Exsr MoveScDb;
                      Write pst0p;
                      isexit = *on;
                   EndIf;

                // Cancel Button
                When BtnSRTCan = *on;
                   BtnSRTCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          Endsl;

          Unlock WMSTP;

          ReadC sflSRT;
       Enddo;

       // Subrotine to Copy Short Rate Master and Details data
       BegSr CopyData;
        For Counter = 1 to TotalSfRcd;
           Chain(E) Counter sflSRTDE;
           If %Found();
              Clear pst0p2;
              STco#  = inCo#;
              STfnd  = inFnd;
              STst   = scStESRT;
              STefdt = dtCYMD(scEfDtESRT);
              STdays = scDaysSRD;
              STperc = scPercSRD;
              STuser = Q1user;
              STpgm  = Q1pgm;
              STdt   = DateTo7(SystemDate);
              STtime = TimeTo6(SystemTime);
              STiusr = Q1user;
              STipgm = Q1pgm;
              STidt  = DateTo7(SystemDate);
              STitim = TimeTo6(SystemTime);
              Write pst0p2;
           Endif;
        EndFor;
       EndSr;

       // Subrotine to Delete Short Rate Details data
       BegSr DelShRtDt;
        KeyEfdt = dtCYMD(scEfDtESRT);
        Setll keyWmstl pst02;
        Dou %Eof(WDSTL);
           Reade keyWmstl pst02;
           If %Eof;
              Leave;
           Endif;
           Delete pst02;
        Enddo;
       EndSr;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scStESRT = m_STst;
        scEfdtESRT = dtMDYY(m_STefdt);
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        m_STco# = inCo#;                 // Company Number
        m_STfnd = inFnd;                 // Group Number
        m_STst  = scStESRT;              // State
        m_STefdt = dtCYMD(scEfDtESRT);     // Effective Date
        m_STuser = q1user;
        m_STpgm  = q1pgm;
        m_STtime = Timeto6(SystemTime());
        m_STdt   = Dateto7(SystemDate());
        m_STitim = Timeto6(SystemTime());
        m_STidt = Dateto7(SystemDate());
        m_STiusr = q1user;
        m_STipgm = q1pgm;
       EndSr;

      /end-free
     P ProcOthrShRt    E

       //===================================================================
       // ProcOthrShRtDt - Process other Options for Short Rate Details Info
       //===================================================================
     P ProcOthrShRtDt  B
     D ProcOthrShRtDt  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdSRTDDtl;
       isexit = *off;
       ReadC sflSRTDE;

       Dow not %eof;

          Select;
          When scSRTDEdt = *On; // Processing for Edit Short Rate Detail Link
             scSRTDEdt = *Off;
             mode = 'C';

             Chain(n) scSRTDERRN WDSTP;

             Exsr MovDbSc;

             scSRTDHdg = 'Edit Short Rate Details Information';
             SRTDEdt = *on;        //disable non-editable field
             svrrnSRTDE = rrnSRTDE;   //save rrn value

             Dow isexit = *off;
                Exfmt rcdSRTDDtl;

                Select;
                // Save Button
                When BtnSRTDSav = *on;
                   BtnSRTDSav = *off;
                   // Check if WDSTP updated since screen displayed (Short Rate Details)
                   flgRcdUpdated=RcdUpdatedErr('WDSTP':dbrrn26:'STdt':'STtime':
                    'STuser':saveDT10:saveTM10:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scSRTDERRN WDSTP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateShRtDt(mode);
                         Chain scSRTDERRN WDSTP;
                         Exsr MoveScDb;
                         Update pst0p2;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnSRTDCan = *on;
                   BtnSRTDCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scSRTDDlt = *On; // Processing for Delete Short Rate Link
             scSRTDDlt = *Off;

             Chain(e) scSRTDERRN WDSTP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scSRTDERRN WDSTP;
             Exsr MovDbSc;

             scSRTDHdg = 'Delete Short Rate Details Info';

             Dow isexit = *off;

                Write rcdSRTDDtl;
                isDelresp = PopTwoBtn('Do you want to delete this Short +
                Rate Details Information?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                   Delete pst0p2;
                   PopOneBtn('Short Rate Details Information +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelResp = *Off;
                   svrrnSRTDE = rrnSRTDE;   //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WDSTP;

          ReadC sflSRTDE;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scDaysSRDE = STdays;
        scPercSRDE = STperc;
        saveDT10 = STdt;
        saveTM10 = STtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        STdays = scDaysSRDE;
        STperc = scPercSRDE;
        STuser = q1user;
        STpgm  = q1pgm;
        STtime = Timeto6(SystemTime());
        STdt   = Dateto7(SystemDate());
        saveDT10 = STdt;
        saveTM10 = STtime;
       EndSr;

      /end-free
     P ProcOthrShRtDt  E

       //===================================================================
       // ProcOthrSmde  - Process other Options for Small Deductibles
       //===================================================================
     P ProcOthrSmDe    B
     D ProcOthrSmDe    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdSmDeDtl;
       isexit = *off;
       ReadC sflSmDe;

       Dow not %eof;

          Select;
          When scSmDeEdt = *On; // Processing for Edit Small Deductibles info
             scSmDeEdt = *Off;

             // Load Initial Values
             InzAddPrmScVal();

             mode = 'C';

             Chain(n) scSmDeRRN WTDDP;

             Exsr MovDbSc;

             scSmDeHdg = 'Edit Small Deductibles Information';
             SmDeEdt = *on ;   //disable non-editable field
             svrrnSmDe = rrnSmDe;   //Save Rrn value  for edited record

             Dow isexit = *off;
                //*** Beg Add *** 08/08/22 ************************************
                If scTypeEsd = 'G' and scTotiE <> 'Y';
                   disPv1Db = *off;
                Else;
                   disPv1Db = *on;
                Endif;
                //*** End Add *** 08/08/22 ************************************
                Exfmt rcdSmDeDtl;

                Select;
                // Save Button
                When BtnSmDeSav = *on;
                   BtnSmDeSav = *off;
                   // Check if WTDDP updated since screen displayed (Small Deductibles)
                   flgRcdUpdated=RcdUpdatedErr('WTDDP':dbrrn27:'DDdt':'DDtime':
                    'DDuser':saveDT11:saveTM11:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scSmDeRRN WTDDP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateSmDe(mode);
                         Chain scSmDeRRN WTDDP;
                         Exsr MoveScDb;
                         Update pdd0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnSmDeCan = *on;
                   BtnSmDeCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scSmDeDlt = *On; // Processing for Delete Small Deductible details link
             scSmDeDlt = *Off;

             Chain(e) scSmDeRRN WTDDP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scSmDeRRN WTDDP;
             Exsr MovDbSc;

             scSmDeHdg = 'Delete Small Deductibles Info';

             Dow isexit = *off;
                Write rcdSmDeDtl;
                isDelresp = PopTwoBtn('Do you want to delete this Small +
                 Deductibles Information?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelResp = *On;
                   Delete pdd0p;
                   PopOneBtn('Small Deductibles Information +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelResp = *Off;
                   svrrnSmDe = rrnSmDe;   //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          //*** Beg Add ***** 12/27/23 *****************************
          When scSmDeCpy = *On; // Processing for Copy Small Deductibles info
             scSmDeCpy = *Off;

             // Load Initial Values
             InzAddPrmScVal();

             mode = 'P';

             Chain(n) scSmDeRRN WTDDP;

             Exsr MovDbSc;

             scSmDeHdg = 'Copy Small Deductibles Information';
             SmDeEdt = *off;   //Do not disable non-editable field
             visSmDeAdS = *off; // hide Save/Add Next button

             Dow isexit = *off;
                If scTypeEsd = 'G' and scTotiE <> 'Y';
                   disPv1Db = *off;
                Else;
                   disPv1Db = *on;
                Endif;
                Exfmt rcdSmDeDtl;

                Select;
                // Save Button
                When BtnSmDeSav = *on;
                   BtnSmDeSav = *off;
                   If ValidateSmDe(mode);
                      Exsr MoveScDb;
                      Write pdd0p;
                      isexit = *on;
                   EndIf;

                // Cancel Button
                When BtnSmDeCan = *on;
                   BtnSmDeCan = *off;
                   svrrnSmDe = rrnSmDe;   //Save Rrn value copy cancel
                   isexit = *on;
                EndSl;
             EndDo;
          //*** End Add ***** 12/27/23 *****************************
          Endsl;

          Unlock WTDDP;

          ReadC sflSmDe;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scCo#E = DDco#;
        scStESD = DDst;
        scEfDtESD = dtMDYY(DDefdt);
        ScCodeESD = DDcode;
        scDescECD = DDdesc;
        scTypeESD = DDtype;
        scPctESD = DDpct;
        scAmagE = DDamag;
        scVaraE = DDvara;
        scAmpoE = DDampo;
        scVerbE = DDverb;
        IF scVerbE = ' ';
           scVerbE = 'O';             // Blank for O (per Occurrence)
        EndIf;
        scclasESD = DDclas;
        scALAEE = DDalae;
        scRnwESD = DDrnw;
        scPlanESD = DDplan;
        scIncMdInE= DDincMdIn;
        If scIncMdInE = *blank;
           scIncMdInE = 'B';
        Endif;
        scTotiE   = DDtoti;
        scDc01E   = DDdc01;
        scDc02E   = DDdc02;
        scDc03E   = DDdc03;
        scDc04E   = DDdc04;
        scDc05E   = DDdc05;
        scDc06E   = DDdc06;
        scDc07E   = DDdc07;
        scDc08E   = DDdc08;
        scDc09E   = DDdc09;
        scDc10E   = DDdc10;
        //*** Beg Add *** 08/08/22 ********************************************
        scPv1Db01 = DDpv1Db01;
        scPv1Db02 = DDpv1Db02;
        scPv1Db03 = DDpv1Db03;
        scPv1Db04 = DDpv1Db04;
        scPv1Db05 = DDpv1Db05;
        scPv1Db06 = DDpv1Db06;
        scPv1Db07 = DDpv1Db07;
        scPv1Db08 = DDpv1Db08;
        scPv1Db09 = DDpv1Db09;
        scPv1Db10 = DDpv1Db10;
        htSmDePnl  = 420;
        topBtnSmDe = 520;

        // If type = Gross and Total incurred not checked then enable PV1
        // fields death benefit fields
        if scTypeEsd = 'G' and scTotiE <> 'Y';
           disPv1Db = *off;
        Else;
           disPv1Db = *on;
        Endif;
        //*** End Add *** 08/08/22 ********************************************
        scSmDeStE = DDstat;
        //*** Beg Add ***** 12/27/23 *****************************
        If mode = 'P';
           scSmDeStE = 'A';             // Status should be A while copying
        Endif;
        //*** End Add ***** 12/27/23 *****************************
        saveDT11 = DDdt;
        saveTM11 = DDtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        DDdesc = scDescECD;            // Description
        DDtype = scTypeESD;             // Type
        DDpct  = scPctESD;             //Discount Percentage
        DDamag = scAmagE;              //Amount Aggregate
        DDvara = scVaraE;             // Variable
        DDampo = scAmpoE;             // Amount per occurence
        If scVerbE <> 'C';
           scVerbE = ' ';             // Save Blank for O (per Occurrence)
        EndIf;
        DDverb = scVerbE;               // Verbiage
        DDclas = scClasESD;             // Class Code
        DDalae =  scALAEE;              // Include ALAE
        IF scRnwESD <> 'N';
           scRnwESD = ' ';            // Save Blank for Y
        EndIf;
        DDrnw  =  scRnwESD;             // Auto Renew
        DDplan = scPlanESD;             // Deductible Plan
        DDincMdIn = scIncMdInE;         // include medical indemnity
        If DDIncMdIn = 'B';
           DDIncMdIn = *Blank;
        Endif;
        DDtoti = scTotiE;              // Use total Incurred
        DDdc01 = scDc01E;              // Deductible Reserves
        DDdc02 = scDc02E;              // Deductible Reserves
        DDdc03 = scDc03E;              // Deductible Reserves
        DDdc04 = scDc04E;              // Deductible Reserves
        DDdc05 = scDc05E;              // Deductible Reserves
        DDdc06 = scDc06E;              // Deductible Reserves
        DDdc07 = scDc07E;              // Deductible Reserves
        DDdc08 = scDc08E;              // Deductible Reserves
        DDdc09 = scDc09E;              // Deductible Reserves
        DDdc10 = scDc10E;              // Deductible Reserves
        //*** Beg Add *** 08/08/22 ********************************************
        //*** Beg Chg ***** 12/27/23 *****************************
        //*** If scTypeEsd = 'G' and scTotiE <> 'Y';
        If (scTypeEsd = 'G' and scTotiE <> 'Y') or mode = 'P';
        //*** End Chg ***** 12/27/23 *****************************
           DDpv1Db01 = scPv1Db01;
           DDpv1Db02 = scPv1Db02;
           DDpv1Db03 = scPv1Db03;
           DDpv1Db04 = scPv1Db04;
           DDpv1Db05 = scPv1Db05;
           DDpv1Db06 = scPv1Db06;
           DDpv1Db07 = scPv1Db07;
           DDpv1Db08 = scPv1Db08;
           DDpv1Db09 = scPv1Db09;
           DDpv1Db10 = scPv1Db10;
        Else;
           DDpv1Db01 = *blanks;
           DDpv1Db02 = *blanks;
           DDpv1Db03 = *blanks;
           DDpv1Db04 = *blanks;
           DDpv1Db05 = *blanks;
           DDpv1Db06 = *blanks;
           DDpv1Db07 = *blanks;
           DDpv1Db08 = *blanks;
           DDpv1Db09 = *blanks;
           DDpv1Db10 = *blanks;
           scPv1Db01 = *blanks;
           scPv1Db02 = *blanks;
           scPv1Db03 = *blanks;
           scPv1Db04 = *blanks;
           scPv1Db05 = *blanks;
           scPv1Db06 = *blanks;
           scPv1Db07 = *blanks;
           scPv1Db08 = *blanks;
           scPv1Db09 = *blanks;
           scPv1Db10 = *blanks;
        Endif;
        //*** End Add *** 08/08/22 ********************************************
        DDstat = scSmDeStE;            // Status
        DDuser = q1user;
        DDpgm  = q1pgm;
        DDtime = Timeto6(SystemTime());
        DDdt   = Dateto7(SystemDate());
        //*** Beg Add ***** 12/27/23 *****************************
        If mode = 'P';
           DDst   = scStESD;               // State
           DDefdt = dtCYMD(scEfDtESD);     // Effective date
           DDcode = scCodeESD;             //Discount Code
           DDitim = Timeto6(SystemTime());
           DDidt  = Dateto7(SystemDate());
           DDiusr = q1user;
           DDipgm = q1pgm;
        Endif;
        //*** End Add ***** 12/27/23 *****************************
        saveDT11 = DDdt;
        saveTM11 = DDtime;
       EndSr;

      /end-free
     P ProcOthrSmDe    E

       //===================================================================
       // ProcOthrStRc  - Process other Options for State Rate Change
       //===================================================================
     P ProcOthrStRC    B
     D ProcOthrStRC    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdStRCDtl;
       isexit = *off;
       ReadC sflStRC;

       Dow not %eof;

          Select;
          When scStRCEdt = *On; // Processing for Edit State Rate Change Info
             scStRCEdt = *Off;
             mode = 'C';

             Chain(n) scStRcRRN WMRTP;

             Exsr MovDbSc;

             scStRCHdg = 'Edit State Rate Change Information';
             StRCEdt = *on ;   //disable non-editable field
             svRrnStRC = rrnStRC;   //Save Rrn value  for edited record

             Dow isexit = *off;
                Exfmt rcdStRCDtl;

                Select;
                // Save Button
                When BtnStRCSav = *on;
                   BtnStRCSav = *off;
                   // Check if WMRTP updated since screen displayed (State Rate Change)
                   flgRcdUpdated=RcdUpdatedErr('WMRTP':dbrrn28:'RTdt':'RTtime':
                    'RTuser':saveDT12:saveTM12:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scStRcRRN WMRTP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateStRC(mode);
                         Chain scStRcRRN WMRTP;
                         Exsr MoveScDb;
                         Update prt0p ;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnStRCCan = *on;
                   BtnStRCCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WMRTP;

          ReadC sflStRC;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scstESRC = RTst;
        scEfDtESRC = dtMDYY(RTefdt);
        scStatESRC = RTstat;
        scOptE = RTopt;
        saveDT12 = RTdt;
        saveTM12 = RTtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        RTstat = scStatESRC;         // Flat Rate Adjustment code
        RTopt = scOptE;              // Opt Out
        RTuser = q1user;
        RTpgm  = q1pgm;
        RTtime = Timeto6(SystemTime());
        RTdt   = Dateto7(SystemDate());
        saveDT12 = RTdt;
        saveTM12 = RTtime;
       EndSr;

      /end-free
     P ProcOthrStRC    E

       //===================================================================
       // ProcOthrStRt  - Process other Options for Statistical Report/Rating
       //===================================================================
     P ProcOthrStRt    B
     D ProcOthrStRt    PI
      /free

       ReadC sflStRt;
       Dow not %eof;
          Select;
          When scStRtDlt = *On; // Processing for Delete Statistical Report/Rating Link
             scStRtDlt = *Off;
             clear strUrlRRN;
             strUrlRRN = %Editc(scStRtRRN:'X');
             If PopTwoBtn('Do you want to delete this Statistical +
              Report/Rating Infomation?':'Confirm Delete':'Yes':'No');
                outMode = 'Y';
                outNext = ' ';
                outTabIndex = ' ';
                CallRatInfo(strUrlRRN:outMode:outNext:outTabIndex);
             Else;
                svRrnStRt = rrnStRt;   //Save Rrn value
             Endif;
          Endsl;
          ReadC sflStRt;
       Enddo;

      /end-free
     P ProcOthrStRt    E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          //*** Beg Add ***** 01/17/22 *****************************
          scActMode = @@mode;
          //*** End Add ***** 01/17/22 *****************************
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       apShowMenu = *on;

       // Set Menu options
       scMenuChoi = 'Primary Group Information';
       scMenuValu = 'WTGRPPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Administration / DARTS Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPADMIN';

       scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPPOLCG';

       scMenuChoi = %trim(scMenuChoi) + ',Claims Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPCLMCG';

       scMenuChoi = %trim(scMenuChoi) + ',State Specific Information';
       scMenuValu = %trim(scMenuValu) + ',WTGRPSSI';

       scMenuChoi = %trim(scMenuChoi) + ',Data Reporting';
       scMenuValu = %trim(scMenuValu) + ',WTGRPDATA';

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);


       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 6;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
     // Update Action Panel Info (Rrn, Group Name, Group Number)
       scActRrn = xxRrn;
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

       scCo# = intCo#;

       processMe = *on;

       //*** Beg Add *** 05/23/23 ******************************************
       isNoAccess = *off;

       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       //*** Beg Chg *** 06/27/23 ******************************************
       //*** If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
       // based on Users Security Type and Level defined in Application Confguration
       If CheckSecLevel('GrpCfgAut':UPtype:UPlvl);
       //*** End Chg *** 06/27/23 ******************************************
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       //*** Beg Add *** 06/23/23 ******************************************
       // Display Group Information in Inquiry Mode even if Users Security
       // level does not allow them to Edit Group Information.
       If dspMode = *on;
          isUsrAcc = *on;
       Endif;
       //*** End Add *** 06/23/23 ******************************************

       // Determine if user is in the Group Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Rating tab
       Chain (intCo#:'WTGRPSSI':
        'Group Configuration - State Specific Information':'Rating'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Rating':arrTabNames:n1);
       Endif;

       // Rating Factors Tab
       Chain (intCo#:'WTGRPSSI':
        'Group Configuration - State Specific Information':'Rating Factors'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Rating Factors':arrTabNames:n1);
       Endif;

       // Policy Page tab
       Chain (intCo#:'WTGRPSSI':
        'Group Configuration - State Specific Information':'Policy Page'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Policy Page':arrTabNames:n1);
       Endif;

       // Cancellation / Reinstatement tab
       Chain (intCo#:'WTGRPSSI':
        'Group Configuration - State Specific Information':
        'Cancellation / Reinstatement':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Cancellation / Reinstatement':arrTabNames:
           n1);
       Endif;

       // Endorsements Tab
       Chain (intCo#:'WTGRPSSI':
        'Group Configuration - State Specific Information':'Endorsements'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Endorsements':arrTabNames:n1);
       Endif;

       // Installment Fees tab
       Chain (intCo#:'WTGRPSSI':
        'Group Configuration - State Specific Information':'Installment Fees'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Installment Fees':arrTabNames:n1);
       Endif;

       TbNmRcdPnl = strTabNames;
       //*** End Add *** 05/23/23 ******************************************

       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // Load display indicators for sub-files in rcdAddPrm Record Format
       //===================================================================
     P RcdAddPrmDsp    b
     D RcdAddPrmDsp    pi
     D  indValue                      1n   const
      /Free
       dspSmDe = indValue;
       dspHrEx = indValue;
       dspLsCn = indValue;
      /End-Free
     P RcdAddPrmDsp    e

       //===================================================================
       // Load display indicators for sub-files in rcdCxRein Record Format
       //===================================================================
     P RcdCxReinDsp    b
     D RcdCxReinDsp    pi
     D  indValue                      1n   const
      /Free
       dspCRSD = indValue;
       dspSRT  = indValue;
       dspSRTD = indValue;
      /End-Free
     P RcdCxReinDsp    e

       //===================================================================
       // Load display indicators for sub-files in rcdPolPag Record Format
       //===================================================================
     P RcdPolPagDsp    b
     D RcdPolPagDsp    pi
     D  indValue                      1n   const
      /Free
       dspPolPg = indValue;
       dspExSt = indValue;
      /End-Free
     P RcdPolPagDsp    e

       //===================================================================
       // Load display indicators for sub-files in rcdRating Record Format
       //===================================================================
     P RcdRatingDsp    b
     D RcdRatingDsp    pi
     D  indValue                      1n   const
      /Free
       dspStRt = indValue;
       dspPrmDis = indValue;
       dspExpCon = indValue;
       dspIncLmt = indValue;
       dspStRC = indValue;
      /End-Free
     P RcdRatingDsp    e

      //===================================================================
      // Validate Small Deductibles Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateCRSD    b
     d ValidateCRSD    pi              n
     d strmode                        1
     d isErrCRSDEdt    s               n   inz
      /Free

        isErrCRSDEdt = *off;
       // Intialize all the messageId and error indicators.
        erEfdtECRS = *off;
        msEfdtECRS = *blanks;
        erStECRSD = *off;
        msStECRSD = *blanks;
        erNotcE = *off;
        msNotcE = *blanks;
        erCxmnE = *off;
        msCxmnE = *blanks;
        erCxflE = *off;
        msCxflE = *blanks;
        flCxflE = *blanks;
        erCxdcE = *off;
        msCxdcE = *blanks;
        erReflE = *off;
        msReflE = *blanks;
        flReflE = *blanks;
        erRedcE = *off;
        msRedcE = *blanks;
        er#1LFE = *off;
        ms#1LFE = *blanks;
        er#1LDE = *off;
        ms#1LDE = *blanks;
        er#2LFE = *off;
        ms#2LFE = *blanks;
        er#2LDE = *off;
        ms#2LDE = *blanks;
        er#3LFE = *off;
        ms#3LFE = *blanks;
        er#3LDE = *off;
        ms#3LDE = *blanks;
        er#4LFE = *off;
        ms#4LFE = *blanks;
        er#4LDE = *off;
        ms#4LDE = *blanks;
        erCclfE = *off;
        msCclfE = *blanks;
        erCcldE = *off;
        msCcldE = *blanks;
        erCflfE = *off;
        msCflfE = *blanks;
        erCfldE = *off;
        msCfldE = *blanks;
        erCscfE = *off;
        msCscfE = *blanks;
        erCscdE = *off;
        msCscdE = *blanks;
        erCsrfE = *off;
        msCsrfE = *blanks;
        erCsrdE = *off;
        msCsrdE = *blanks;
        erRclfE = *off;
        msRclfE = *blanks;
        erRcldE = *off;
        msRcldE = *blanks;

        If strmode = 'A';      //Add Mode
        // Effective Date must be valid
           erEfdtECRS = ValidDate(scEfdtECRS:charMsg);
           If erEfdtECRS = *on;
              msEfdtECRS = charMsg;
              isErrCRSDEdt = *on;
           Endif;
        // State must be valid
           If scStECRSD = *Blanks;
              erStECRSD = *on;
              msStECRSD = 'WCR0503';
              isErrCRSDEdt = *on;
           Endif;
        // Record must not already exist for st/efdt
           If isErrCRSDEdt = *off;
              KeyEfdt = dtCYMD(scEfDtECRS);
              Chain keyWtxcl1 pxc00;
              If %found(wtxcl1);
                 erStECRSD = *on;
                 msStECRSD = 'WC30005';
                 erEfDtECRS = *on;
                 msEfDtECRS = 'WC30005';
                 isErrCRSDEdt = *on;
              Endif;
           Endif;
        Endif;                      //Add mode

        // Cancellation Notice Day should not be zero
        If scNotcE = *Zeros;
           erNotcE = *on;
           msNotcE = 'WCR0502';
           isErrCRSDEdt = *on;
        Endif;
        // Number of Months can not be zero if cancellation limit is entered
        If ScCxlmE <> *Zeros and ScCxmnE = *Zeros;
           erCxmnE = *On;
           msCxmnE = 'WC32384';
           isErrCRSDEdt = *on;
        Endif;

        // If entered, state cancellation form folder and document must be valid
        If isScriptCx = *off;
           If scCxflE <> *Blanks;
              outFold = scCxflE;
              outRtcd = ' ';
              ChkFolder(outFold:outRtcd);
           // Folder must exist
              If outRtcd = '1';
                 erCxflE = *On;
                 msCxflE = 'WCR0826';
                 flCxflE = 'WMESGG';
              Endif;
           // User must have authority to folder
              If outRtcd = '2';
                 erCxflE = *On;
                 msCxflE = 'SCM0517';
                 flCxflE = 'SCMESG';
                 isErrCRSDEdt = *on;
              Endif;
           Else;
        // folder cannot be blank if document is entered
              If scCxdcE <> *Blanks;
                 erCxflE = *On;
                 msCxflE = 'WCR0826';
                 flCxflE = 'WMESGG';
                 isErrCRSDEdt = *on;
              Endif;
           Endif;
        // Docuement must be valid if entered
           If scCxdcE <> *Blanks;
              outFld = scCxflE;
              outDoc = scCxdcE;
              outRtcd = *Blanks;
              ChkDoc(outDoc:outFld:outRtcd);
              If outRtcd = '1';
                 erCxdcE = *On;
                 msCxdcE = 'WCR0825';
                 isErrCRSDEdt = *on;
              Endif;
           Else;
        // Document cannot be blank if folder is entered
              If scCxfle<> *Blanks;
                 erCxdcE = *On;
                 msCxdcE = 'WCR0825';
                 isErrCRSDEdt = *on;
              Endif;
           Endif;
        Endif;

        // If entered, state Reinstatement form folder and document must be valid
        If isScriptRe = *off;
           If scReflE <> *Blanks;
              outFold = scReflE;
              outRtcd = *Blanks;
              ChkFolder(outFold:outRtcd);
        // Folder must exist
              If outRtcd = '1';
                 erReflE = *On;
                 msReflE = 'WCR0826';
                 flReflE = 'WMESGG';
                 isErrCRSDEdt = *on;
              Endif;
        // User must have authority to folder
              If outRtcd = '2';
                 erReflE = *On;
                 msReflE = 'SCM0517';
                 flReflE = 'SCMESG';
                 isErrCRSDEdt = *on;
              Endif;
           Else;
        // Folder cannot be blank if document is entered
              If scRedcE <> *Blanks;
                 erReflE = *On;
                 msReflE = 'WCR0826';
                 flReflE = 'WMESGG';
                 isErrCRSDEdt = *on;
              Endif;
           Endif;
        // Document must be valid if entered
           If scRedcE <> *Blanks;
              outFld = scReflE;
              outDoc = scRedcE;
              outRtcd = *Blanks;
              ChkDoc(outDoc:outFld:outRtcd);
              If outRtcd = '1';
                 erRedcE = *On;
                 msRedcE = 'WCR0825';
                 isErrCRSDEdt = *on;
              Endif;
           Else;
        // Document cannot be blank if folder is entered
              If scReflE <> *Blanks;
                 erRedcE = *On;
                 msRedcE = 'WCR0825';
                 isErrCRSDEdt = *on;
              Endif;
           Endif;
        Endif;

        // 1st notice of cancellation document info :
        // Folder and Document mutually inclusive
        If (sc#1LFE = *Blanks and sc#1LDE <> *Blanks) Or
           (sc#1LFE <> *Blanks and sc#1LDE = *Blanks);
           er#1LFE = *on;
           ms#1LFE = 'WCR0544';
           er#1LDE = *on;
           ms#1LDE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If sc#1LFE <> *Blanks;
           outFold = sc#1LFE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              er#1LFE = *On;
              ms#1LFE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If sc#1LDE <> *Blanks;
           outFld = sc#1LFE;
           outDoc = sc#1LDE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              er#1LDE = *On;
              ms#1LDE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // 2nd notice of cancellation document info :
        // Folder and Document mutually inclusive
        If (sc#2LFE = *Blanks and sc#2LDE <> *Blanks) Or
           (sc#2LFE <> *Blanks and sc#2LDE = *Blanks);
           er#2LFE = *on;
           ms#2LFE = 'WCR0544';
           er#2LDE = *on;
           ms#2LDE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If sc#2LFE <> *Blanks;
           outFold = sc#2LFE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              er#2LFE = *On;
              ms#2LFE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If sc#2LDE <> *Blanks;
           outFld = sc#2LFE;
           outDoc = sc#2LDE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              er#2LDE = *On;
              ms#2LDE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // 3rd notice of cancellation document info :
        // Folder and Document mutually inclusive
        If (sc#3LFE = *Blanks and sc#3LDE <> *Blanks) Or
           (sc#3LFE <> *Blanks and sc#3LDE = *Blanks);
           er#3LFE = *on;
           ms#3LFE = 'WCR0544';
           er#3LDE = *on;
           ms#3LDE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If sc#3LFE <> *Blanks;
           outFold = sc#3LFE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              er#3LFE = *On;
              ms#3LFE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If sc#3LDE <> *Blanks;
           outFld = sc#3LFE;
           outDoc = sc#3LDE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              er#3LDE = *On;
              ms#3LDE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // 4th notice of cancellation document info :
        // Folder and Document mutually inclusive
        If (sc#4LFE = *Blanks and sc#4LDE <> *Blanks) Or
           (sc#4LFE <> *Blanks and sc#4LDE = *Blanks);
           er#4LFE = *on;
           ms#4LFE = 'WCR0544';
           er#4LDE = *on;
           ms#4LDE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If sc#4LFE <> *Blanks;
           outFold = sc#4LFE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              er#4LFE = *On;
              ms#4LFE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If sc#4LDE <> *Blanks;
           outFld = sc#4LFE;
           outDoc = sc#4LDE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              er#4LDE = *On;
              ms#4LDE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // Cancellation Cover letter
        // Folder and Document mutually inclusive
        If (scCclfE = *Blanks and scCcldE <> *Blanks) Or
           (scCclfE <> *Blanks and scCcldE = *Blanks);
           erCclfE = *on;
           msCclfE = 'WCR0544';
           erCcldE = *on;
           msCcldE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If scCclfE <> *Blanks;
           outFold = scCclfE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              erCclfE = *On;
              msCclfE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If scCcldE <> *Blanks;
           outFld = scCclfE;
           outDoc = scCcldE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              erCcldE = *On;
              msCcldE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // Client Specific CX Confirmation Cover Letter
        // Folder and Document mutually inclusive
        If (scCflfE = *Blanks and scCfldE <> *Blanks) Or
           (scCflfE <> *Blanks and scCfldE = *Blanks);
           erCflfE = *on;
           msCflfE = 'WCR0544';
           erCfldE = *on;
           msCfldE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If scCflfE <> *Blanks;
           outFold = scCflfE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              erCflfE = *On;
              msCflfE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If scCfldE <> *Blanks;
           outFld = scCflfE;
           outDoc = scCfldE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              erCfldE = *On;
              msCfldE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // Client Specific CX Certificate Letter:
        // Folder and Document mutually inclusive
        If (scCscfE = *Blanks and scCscdE <> *Blanks) Or
           (scCscfE <> *Blanks and scCscdE = *Blanks);
           erCscfE = *on;
           msCscfE = 'WCR0544';
           erCscdE = *on;
           msCscdE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If scCscfE <> *Blanks;
           outFold = scCscfE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              erCscfE = *On;
              msCscfE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If scCscdE <> *Blanks;
           outFld = scCscfE;
           outDoc = scCscdE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              erCscdE = *On;
              msCscdE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // Client Specific Reinstatement Certificate Letter:
        // cclder and Document mutually inclusive
        If (scCsrfE = *Blanks and scCsrdE <> *Blanks) Or
           (scCsrfE <> *Blanks and scCsrdE = *Blanks);
           erCsrfE = *on;
           msCsrfE = 'WCR0544';
           erCsrdE = *on;
           msCsrdE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If scCsrfE <> *Blanks;
           outFold = scCsrfE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              erCsrfE = *On;
              msCsrfE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If scCsrdE <> *Blanks;
           outFld = scCsrfE;
           outDoc = scCsrdE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              erCsrdE = *On;
              msCsrdE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;

        // Reinstatement Cover Letter:
        // cclder and Document mutually inclusive
        If (scRclfE = *Blanks and scRcldE <> *Blanks) Or
           (scRclfE <> *Blanks and scRcldE = *Blanks);
           erRclfE = *on;
           msRclfE = 'WCR0544';
           erRcldE = *on;
           msRcldE = 'WCR0544';
           isErrCRSDEdt = *on;
        Endif;
        // Folder must be valid
        If scRclfE <> *Blanks;
           outFold = scRclfE;
           outRtcd = *Blanks;
           ChkFolder(outFold:outRtcd);
           If outRtcd <> *Blanks;
              erRclfE = *On;
              msRclfE = 'WC31490';
              isErrCRSDEdt = *on;
           Endif;
        Endif;
        // Document must be valid
        If scRcldE <> *Blanks;
           outFld = scRclfE;
           outDoc = scRcldE;
           outRtcd = *Blanks;
           ChkDoc(outDoc:outFld:outRtcd);
           If outRtcd = '1';
              erRcldE = *On;
              msRcldE = 'WC31492';
              isErrCRSDEdt = *on;
           Endif;
        Endif;


      // Return *on indicator when No error exists.
        If isErrCRSDEdt = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateCRSD    e

      //===================================================================
      // Validate State Compensation Benefit Info.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateEDC     b
     d ValidateEDC     pi              n
     d strmode                        1
     d isErrEDCEdt     s               n   inz
      /Free

       isErrEDCEdt = *off;
      // Intialize all the messageId and error indicators.
       erEfdtEEDC = *off;
       msEfdtEEDC = *blanks;
       erStEEDC = *off;
       msStEEDC = *blanks;
       erPrcdE = *off;
       msPrcdE = *blanks;
       erPrcfE = *off;
       msPrcfE = *blanks;
       erRcedE = *off;
       msRcedE = *blanks;
       erRcefE = *off;
       msRcefE = *blanks;
       erLlmtE = *off;
       msLlmtE = *blanks;
       erHlmtE = *off;
       msHlmtE = *blanks;
       erDeptE = *off;
       msDeptE = *blanks;
       erPhn#EEDC = *off;
       msPhn#EEDC = *blanks;
       //*** Beg Add ***** 02/14/24 *********************************
       erExWC99 = *off;
       msExWC99 = *blanks;
       //*** End Add ***** 02/14/24 *********************************
       //*** Beg Add ***** 02/19/24 *********************************
       errEnd602A = *off;
       msEnd602A = *blanks;
       errEnd602B = *off;
       msEnd602B = *blanks;
       //*** End Add ***** 02/19/24 *********************************

       If strmode = 'A';      //Add Mode
       // State can not be blank
          If scStEEDC = *Blanks;
             erStEEDC = *on;
             msStEEDC = 'WCR0503';
             isErrEDCEdt = *on;
          Endif;
       // Validation Check for Effective Date
          erEfdtEEDC = ValidDate(scEfdtEEDC:charMsg);
          If erEfdtEEDC = *on;
             msEfdtEEDC = charMsg;
             isErrEDCEdt = *on;
          Endif;
        // Record Must Not Already Exist for st/efdt
          If erEfdtEEDC = *off;
             KeyEfdt = dtCYMD(scEfDtEEDC);
             Chain keyWmnxl1 pnx00;
             If %found(wmnxl1);
                erStEEDC = *on;
                msStEEDC = 'WC30005';
                erEfDtEEDC = *on;
                msEfDtEEDC = 'WC30005';
                isErrEDCEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

       //*** Beg Add ***** 02/14/24 *********************************
       If strmode = 'A' or strmode = 'C';           //Add or Change Mode
          Chain(n) (intco#:'NCCI':scStEEDC) pgt01;  // Edit for checkbox Supress WC99*
          If %found(wtgtl);
             If scExWc99E = 'Y';
                erExWC99 = *on;
                msExWC99 = 'This checkbox cannot be checked for an NCCI '+
                            'state; only for states of independent bureaus.';
                isErrEDCEdt = *on;
             Endif;
          Endif;
          //*** Beg Add ***** 02/19/24 ******************************
          If scstEEDC = 'HI';
                                                              // Column A edits
             If scMCareA <> *zeros or scWgLossA <> *zeros
              or scIBenDisA <> *zeros or scDBenA <> *zeros
              or scAdmLglA <> *zeros or scMiscA <> *zeros;
                If (scMCareA + scWgLossA + scIBenDisA + scDBenA
                    + scAdmLglA + scMiscA) <> 100;
                   errEnd602A = *on;
                   msEnd602A = 'Column A categories 1-6 must '+
                               'total 100%.';
                   isErrEDCEdt = *on;
                Endif;
             Endif;
                                                              // Column B edits
             If scMCareB <> *zeros or scWgLossB <> *zeros
              or scIBenDisB <> *zeros or scDBenB <> *zeros
              or scAdmLglB <> *zeros or scMiscB <> *zeros;
                If (scMCareB + scWgLossB + scIBenDisB + scDBenB
                    + scAdmLglB + scMiscB) > 100;
                   errEnd602B = *on;
                   msEnd602B = 'Column B categories 1-6 total ' +
                            'is more than 100%.';
                   isErrEDCEdt = *on;
                Endif;
             Endif;
          Endif;
          //*** End Add ***** 02/19/24 ******************************
       Endif;                                       //Add or Change Mode
       //*** End Add ***** 02/14/24 *********************************

       // Validation Check for Pending Rate Change Date
       If scPrcdE <> *zeros and scPrcfE = 'Y';
          erPrcdE = ValidDate(scPrcdE:charMsg);
          If erPrcdE = *on ;
             msPrcdE = charMsg;
             isErrEDCEdt = *on;
          Else;
             If dtCYMD(scPrcdE) < dtCYMD(scEfDtEEDC);
                erPrcdE = *on;
                msPrcdE = 'WC30642';
                isErrEDCEdt = *on;
             Endif;
          Endif;
       Endif;
       // If Pending Rate Change flag is Y, date is needed. If Flag is N, date must be zero
       // These fields are mutually inclusive.
       If (scPrcfE = 'Y' and scPrcdE = *zeros) or
          (scPrcfE <> 'Y' and scPrcdE <> *zeros);
          erPrcdE = *on;
          msPrcdE = 'WCR1083';
          erPrcfE = *on;
          msPrcfE = 'WCR1083';
          isErrEDCEdt = *on;
       Endif;
       // Validation Check for Rate Change Date
       If scRcedE <> *zeros and scRcefE = 'Y';
          erRcedE = ValidDate(scRcedE:charMsg);
          If erRcedE = *on;
             msRcedE = charMsg;
             isErrEDCEdt = *on;
          Else;
             If dtCYMD(scRcedE) < dtCYMD(scEfDtEEDC);
                erRcedE = *on;
                msRcedE = 'WC30643';
                isErrEDCEdt = *on;
             Endif;
          Endif;
       Endif;
       // If Rate Change flag is Y, date is needed. If Flag is N, date must be zero
       // These fields are mutually inclusive.
       If (scRcefE = 'Y' and scRcedE = *zeros) or
          (scRcefE <> 'Y' and scRcedE <> *zeros);
          erRcedE = *on;
          msRcedE = 'WCR1083';
          erRcefE = *on;
          msRcefE = 'WCR1083';
          isErrEDCEdt = *on;
       Endif;
       // Range Entered must be in ascending order.
       If scLlmtE > scHlmtE;
          erLlmtE = *on;
          msLlmtE = 'WCR0665';
          erHlmtE = *on;
          msHlmtE = 'WCR0665';
          isErrEDCEdt = *on;
       Endif;
       // If Range Entered, both Limits must be greater than zero
       If scLlmtE <> 0 or scHlmtE <> 0;
          If scLlmtE = 0;
             erLlmtE = *on;
             msLlmtE = 'WCR0502';
             isErrEDCEdt = *on;
          Endif;
          If scHlmtE = 0;
             erHlmtE = *on;
             msHlmtE = 'WCR0502';
             isErrEDCEdt = *on;
          Endif;
       Endif;
       // If State is AR then, Department name and Phone number is mandatory.
       If scStEEDC = 'AR' and scDeptE = *Blanks;
          erDeptE = *on;
          msDeptE = 'WCR0503';
          isErrEDCEdt = *on;
       Endif;
       If scStEEDC = 'AR' and scPhn#EEDC = *Zeros;
          erPhn#EEDC = *on;
          msPhn#EEDC = 'WCR0556';
          isErrEDCEdt = *on;
       Endif;


      // Return *on indicator when No error exists.
       If isErrEDCEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateEDC     e

      //===================================================================
      // Validate Expense Constant Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateExCo    b
     d ValidateExCo    pi              n
     d strmode                        1
     d isErrExCoEdt    s               n   inz
      /Free

       isErrExCoEdt = *off;
      // Intialize all the messageId and error indicators.
       erEfdtEEC = *off;
       msEfdtEEC = *blanks;
       erStEEC = *off;
       msStEEC = *blanks;
       erStp3E = *off;
       msStp3E = *blanks;

       If strmode = 'A';      //Add Mode
          If scStEEC = *Blanks;
             erStEEC = *on;
             msStEEC = 'WCR0503';
             isErrExCoEdt = *on;
          Endif;
       // Validation Check for Effective Date
          erEfdtEEC = ValidDate(scEfdtEEC:charMsg);
          If erEfdtEEC = *on;
             msEfdtEEC = charMsg;
             isErrExCoEdt = *on;
          Endif;

       // Validation check for Duplicate record
          If isErrExCoEdt = *Off;
             KeyEfdt = dtCYMD(scEfDtEEC);
             Chain keyWdecl1 pec00;
             If %found(wdecl1);
                erStEEC = *on;
                msStEEC = 'WC30005';
                erEfdtEEC = *on;
                msEfdtEEC = 'WC30005';
                isErrExCoEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

       If scStprE = *zeros and scStp3E <> *zeros;
          erStp3E = *on;
          msStp3E = 'WC40047';
          isErrExCoEdt = *on;
       Endif;
       If scStp3E <> *zeros and scStprE > scStp3E ;
          erStp3E = *on;
          msStp3E = 'WC40048';
          isErrExCoEdt = *on;
       Endif;


      // Return *on indicator when No error exists.
       If isErrExCoEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateExCo    e

      //===================================================================
      // Validate Excluded States addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateExSt    b
     d ValidateExSt    pi              n
     d strmode                        1
     d isError         s               n   Inz
     d  mode           s              1    Inz
      /Free

       isError = *off;
      // Intialize all the messageId and error indicators.
       erStEExSt = *off;
       msStEExSt = *blanks;

       If strmode = 'A';

       // Validation Check for State
          If scStEExSt = *Blanks;
             erStEExSt = *on;
             msStEExSt = 'WCR0503';
             isError = *on;
          Endif;

       // Validation check for Duplicate record
          If isError = *Off;
             keyEmp# = 0;
             KeyFyr = 0;
             Chain(n) keyWdxsl2 pxs00;
             If %found(wdxsl);
                erStEExSt = *on;
                msStEExSt = 'WC30661';
                isError = *on;
             Endif;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateExSt    e

      //===================================================================
      // Validate Harbor Exposure Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateHrEx    b
     d ValidateHrEx    pi              n
     d strmode                        1
     d isErrHrExEdt    s               n   inz
      /Free

       isErrHrExEdt = *off;
      // Intialize all the messageId and error indicators.
       erStEHR   = *off;
       msStEHR   = *blanks;
       erEfdtEHR = *off;
       msEfdtEHR = *blanks;
       erLhnfE   = *off;
       msLhnfE   = *blanks;

       If strmode ='A';      //Add Mode
         // State must not be blank
          If scStEHR = *Blanks;
             erStEHR = *on;
             msStEHR = 'WCR0503';
             isErrHrExEdt = *on;
          Endif;

         // Validation Check for Effective Date
          erEfdtEHR = ValidDate(scEfDtEHR:charMsg);
          If erEfdtEHR = *on;
             msEfdtEHR = charMsg;
             isErrHrExEdt = *on;
          Endif;

         // Record must not already exist for State/Effective date
          If isErrHrExEdt = *off;
             KeyEfdt = dtCYMD(scEfdtEHR);
             Chain keyWdhel1 phe00;
             If %found(wdhel1);
                erStEHR = *on;
                msStEHR = 'WC30005';
                erEfdtEHR  = *on;
                msEfdtEHR = 'WC30005';
                isErrHrExEdt = *on;
             Endif;
          Endif;
       Endif;

       // Validation Check for Factor Increase
       If scLhnfe = *Zeros;
          erLhnfE = *on;
          msLhnfE = 'WCR0502';
          isErrHrExEdt = *on;
       Endif;



      // Return *on indicator when No error exists.
       If isErrHrExEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateHrEx    e

      //===================================================================
      // Validate Installment Fees Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateInFe    b
     d ValidateInFe    pi              n
     d strmode                        1
     d isErrInFeEdt    s               n   inz
      /Free

       isErrInFeEdt = *off;
      // Intialize all the messageId and error indicators.
       erEfdtEIF= *off;
       msEfdtEIF = *blanks;
       erExpdEIF= *off;
       msExpdEIF = *blanks;
       erStEIF = *off;
       msStEIF = *blanks;
       erIfblE = *off;
       msIfblE = *blanks;
       erIfpdE = *off;
       msIfpdE = *blanks;
       //*** Beg Chg *** 09/24/24 ******************************************
       //***erIfeeE = *off;
       //***msIfeeE = *blanks;
       //***erIsfeE = *off;
       //***msIsfeE = *blanks;
       erFeeE  = *off;
       msFeeE  = *blanks;
       //*** End Chg *** 09/24/24 ******************************************

       If strmode = 'A';      //Add Mode
       // State must not be blank
          If scStEIF = *Blanks;
             erStEIF = *on;
             msStEIF = 'WCR0503';
             isErrInFeEdt = *on;
          Endif;
       // Validation Check for Effective Date
          erEfdtEIF = ValidDate(scEfdtEIF:charMsg);
          If erEfdtEIF = *on;
             msEfdtEIF = charMsg;
             isErrInFeEdt = *on;
          Endif;
       // Record must not already exist for State/Effective date
          If isErrInFeEdt = *off;
             KeyEfdt = dtCYMD(scEfDtEIF);
             Chain keyWmifl1 pif00;
             If %found(wmifl1);
                erStEIF= *on;
                msStEIF = 'WC30005';
                erEfDtEIF = *on;
                msEfDtEIF = 'WC30005';
                isErrInFeEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

      //Billing A/R code cannot be blank
       If scIfblE = *Blanks;
          erIfblE = *on;
          msIfblE = 'WCR0503';
          isErrInFeEdt = *on;
       Endif;
       //Paid A/R code cannot be blank
       If scIfpdE = *Blanks;
          erIfpdE = *on;
          msIfpdE = 'WCR0503';
          isErrInFeEdt = *on;
       Endif;
       //*** Beg Chg *** 09/24/24 ******************************************
       //Installment fee amount cannot be zero
       //***If scIfeeE = *Zeros;
       //***   erIfeeE = *on;
       //***   msIfeeE = 'WCR0502';
       //***   isErrInFeEdt = *on;
       //***Endif;
       // Self reporting fee amount cannot be zero
       //***If scIsfeE = *Zeros;
       //***   erIsfeE = *on;
       //***   msIsfeE = 'WCR0502';
       //***   isErrInFeEdt = *on;
       //***Endif;
       // Enter either Estimated or S/R Installment fee amount
       If scIfeeE = *Zeros and scIsFeE = *Zeros;
          erFeeE = *on;
          msFeeE = 'Estimated or Self Reporting Fee must be entered';
          isErrInFeEdt = *on;
       Endif;
       //*** End Chg *** 09/24/24 ******************************************
       // Validation Check for Expiry Date
       If scExpdEIF <> *Zeros;
          erExpdEIF = ValidDate(scExpdEIF:charMsg);
          If erExpdEIF = *on;
             msExpdEIF = charMsg;
             isErrInFeEdt = *on;
          Endif;
       Endif;
       // Dates must be in Ascending order
       If scEfDtEIF <> 0 and scExpdEIF <> 0;
          If erExpdEIF = *off and erEfDtEIF = *off;
             erExpdEIF = AscendDates(scEfDtEIF:scExpdEIF:charMsg);
             If erExpdEIF = *on;
                erEfDtEIF = *on;
                msExpdEIF = charMsg;
                msEfDtEIF = charMsg;
                isErrInFeEdt = *on;
             Endif;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isErrInFeEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateInFe    e

      //===================================================================
      // Validate Increased Limit Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateInLt    b
     d ValidateInLt    pi              n
     d strmode                        1
     d isErrInLtEdt    s               n   inz
      /Free

        isErrInLtEdt = *off;
       // Intialize all the messageId and error indicators.
        erEfdtEIL = *off;
        msEfdtEIL = *blanks;
        erStEIL = *off;
        msStEIL = *blanks;
        erKeyE = *off;
        msKeyE = *blanks;
        erDescEIL = *off;
        msDescEIL = *blanks;
        erClasE = *off;
        msClasE = *blanks;

        //*** Beg Chg ***** 12/27/23 *****************************
        //*** If strmode = 'A';      //Add Mode
        If strmode = 'A' or strmode = 'P';      //Add Mode or Copy Mode
        //*** End Chg ***** 12/27/23 *****************************

          // Validation Check for State
           If scStEIL = *Blanks;
              erStEIL = *on;
              msStEIL = 'WCR0503';
              isErrInLtEdt = *on;
           Endif;
          // Validation Check for Effective Date
           erEfdtEIL = ValidDate(scEfdtEIL:charMsg);
           If erEfdtEIL = *on;
              msEfdtEIL = charMsg;
              isErrInLtEdt = *on;
           Endif;
          // Validation Check for Limit Code
           If scKeyE = *Blanks;
              erKeyE = *on;
              msKeyE = 'WCR0503';
              isErrInLtEdt = *on;
           Endif;
          // Check for Duplicate Record
           If isErrInLtEdt = *off;
              KeyEfdt = dtCYMD(scEfDtEIL);
              KeyIncLmt = scKeyE;
              Chain keyWtlml2 plm00;
              If %found(wtlml2);
                 erStEIL = *on;
                 msStEIL = 'WC30005';
                 erEfdtEIL = *on;
                 msEfdtEIL = 'WC30005';
                 erKeyE = *on;
                 msKeyE = 'WC30005';
                 isErrInLtEdt = *on;
              Endif;
           Endif;
        Endif;                      //Add mode

      // Validation Check for Description
       If scDescEIL = *blanks;
          erDescEIL = *on;
          msDescEIL = 'WCR0503';
          isErrInLtEdt = *on;
       Endif;

      // Validation Check for Stastical Reporting Code (or Class Code)
      // Its Required if Reporting NCCI (WCFL03='Y') (Electronic Submission Flag)
       Chain 'WC' SWC01;
       If %found() and WCfl03 = 'Y' and scClasE = *zeros;
           erClasE = *on;
           msClasE = 'WCR0503';
           isErrInLtEdt = *on;
       Endif;



      // Return *on indicator when No error exists.
       If isErrInLtEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateInLt    e

      //===================================================================
      // Validate Loss Constant Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateLsCn    b
     d ValidateLsCn    pi              n
     d strmode                        1
     d isErrLsCnEdt    s               n   inz
      /Free

       isErrLsCnEdt = *off;
      // Intialize all the messageId and error indicators.
       erStELC = *off;
       msStELC = *blanks;
       erEfdtELC= *off;
       msEfdtELC = *blanks;

       If strmode = 'A';      //Add Mode
       // Validation Check for Effective Date
          erEfdtELC = ValidDate(scEfdtELC:charMsg);
          If erEfdtELC = *on;
             msEfdtELC = charMsg;
             isErrLsCnEdt = *on;
          Endif;

       // Validation Check for State
          If scStELC = *Blanks;
             erStELC = *on;
             msStELC = 'WCR0503';
             isErrLsCnEdt = *on;
          Endif;

       // Validation check for Duplicate record
          If isErrLsCnEdt = *Off;
             KeyEfdt = dtCYMD(scEfDtELC);
             Chain keyWdscl psc00;
             If %found(wdscl);
                erStELC = *on;
                msStELC = 'WC30005';
                erEfdtELC = *on;
                msEfdtELC = 'WC30005';
                isErrLsCnEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

      // Return *on indicator when No error exists.
       If isErrLsCnEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateLsCn    e

      *** Begin Add ***** 04/01/25 *****************************
      //===================================================================
      // Validate Filter criteria for Small Deductibles Grid
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateFlt     b
     d ValidateFlt     pi              n
     d isError         s               n   inz
      /Free

       isError = *off;

       msFDtSDFlt = *blanks;
       erFDtSDFlt = *off;
       msTDtSDFlt = *blanks;
       erTDtSDFlt = *off;

      // Validate Effective Date Range
       If scFDtSDFlt <> *zeros;
          erFDtSDFlt = ValidDate(scFDtSDFlt:charMsg);
          if erFDtSDFlt = *on;
             msFDtSDFlt = charMsg;
             isError = *on;
          Endif;
       Endif;
       If scTDtSDFlt <> *zeros;
          erTDtSDFlt = ValidDate(scTDtSDFlt:charMsg);
          if erTDtSDFlt = *on;
             msTDtSDFlt = charMsg;
             isError = *on;
          Endif;
       Endif;
      // Dates must be in Ascending order
       If scFDtSDFlt <> 0 or scTDtSDFlt <> 0;
          If erTDtSDFlt = *off and erFDtSDFlt = *off;
             erTDtSDFlt = AscendDates(scFDtSDFlt:scTDtSDFlt:charMsg);
             If erTDtSDFlt = *on;
                erFDtSDFlt = *on;
                msFDtSDFlt = charMsg;
                msTDtSDFlt = charMsg;
                isError = *on;
             Endif;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateFlt     e
      *** End   Add ***** 04/01/25 *****************************

      //===================================================================
      // Validate Policy Page - 3c addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidatePolPg   b
     d ValidatePolPg   pi              n
     d strmode                        1
     d isError         s               n   Inz
     d  mode           s              1    Inz
      /Free

       isError = *off;
      // Intialize all the messageId and error indicators.
       erEfdtEPP = *off;
       msEfdtEPP = *blanks;
       erVerbType = *off;
       msVerbType = *blanks;

       If strmode = 'A';

         // Validation Check for Effective Date
          erEfdtEPP = ValidDate(scEfdtEPP:charMsg);
          If erEfdtEPP = *on;
             msEfdtEPP = charMsg;
             isError = *on;
          Endif;

         // Validation Check for Verbiage Type
          If scVerbType = *blank;
             erVerbType = *on;
             msVerbType = 'WCR0503';
             isError = *on;
          Endif;

         // Validation Check for Duplicate Record
          If isError = *Off;
             keyPolPg = scVerbType;
             KeyEfdt = dtCYMD(scEfDtEPP);
             Chain keyWdf4l pf400;
             If %Found(wdf4l);
                If F4stat = 'A';
                   erEfdtEPP = *on;
                   msEfdtEPP = 'WC30005';
                   isError = *on;
                Endif;
             Endif;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidatePolPg   e

      //===================================================================
      // Validate Premium Discount addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidatePrmDis  b
     d ValidatePrmDis  pi              n
     d strmode                        1
     d isError         s               n   Inz
     d  mode           s              1    Inz
      /Free

       isError = *off;
      // Intialize all the messageId and error indicators.
       erStE = *off;
       msStE = *blanks;
       erEfdtE = *off;
       msEfdtE = *blanks;
       erCodeE = *off;
       msCodeE = *blanks;
       erTypeE = *off;
       msTypeE = *blanks;
       erLmtE = *off;
       msLmtE = *blanks;

       //*** Beg Chg ***** 12/27/23 *****************************
       //*** If strmode = 'A';
       If strmode = 'A' or strmode = 'P';
       //*** End Chg ***** 12/27/23 *****************************

         // Validation Check for State
          If scStE = *blank;
             erStE = *on;
             msStE = 'WCR0503';
             isError = *on;
          Endif;

         // Validation Check for Effective Date
          erEfdtE = ValidDate(scEfdtE:charMsg);
          If erEfdtE = *on;
             msEfdtE = charMsg;
             isError = *on;
          Endif;

         // Validation Check for Discount Code
          If scCodeE = *blank;
             erCodeE = *on;
             msCodeE = 'WCR0503';
             isError = *on;
          Endif;
       Endif;

       // Check for Duplicate Record
       If isError = *Off;
          KeyEfdt = dtCYMD(scEfDtE);
          Chain keyWMDSL1 pds00;
          If %Found(wmdsl1);
             erStE = *on;
             erEfdtE = *on;
             erLmtE = *on;
             erTypeE = *on;
             erLmtE = *on;
             msStE = 'WC30005';
             msEfdtE = 'WC30005';
             msLmtE = 'WC30005';
             msTypeE = 'WC30005';
             msLmtE = 'WC30005';
             isError = *on;
          Endif;
       Endif;

       // Validation Check for High Limit
       If scLmtE <= *zeros;
          erLmtE = *on;
          msLmtE = 'WCR0502';
          isError = *on;
       Endif;

       // Validation Check for Equation Type
       If scTypeE = *Blanks;
          erTypeE = *on;
          msTypeE = 'WCR0624';
          isError = *on;
       Endif;


      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidatePrmDis  e

      //===================================================================
      // Validate Short Rates Master Info.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateShRt    b
     d ValidateShRt    pi              n
     d strmode                        1
     d isErrSRTEdt     s               n   inz
      /Free

       isErrSRTEdt = *off;
      // Intialize all the messageId and error indicators.
       erEfdtESRT = *off;
       msEfdtESRT = *blanks;
       erStESRT = *off;
       msStESRT = *blanks;

       If strmode = 'A';      //Add Mode
         // State can not be blank
          If scStESRT = *Blanks;
             erStESRT = *on;
             msStESRT = 'WCR0503';
             isErrSRTEdt = *on;
          Endif;
         // Validation Check for Effective Date
          erEfdtESRT = ValidDate(scEfdtESRT:charMsg);
          If erEfdtESRT = *on;
             msEfdtESRT = charMsg;
             isErrSRTEdt = *on;
          Endif;
         // Record Must Not Already Exist for st/efdt
          If isErrSRTEdt = *off;
             KeyEfdt = dtCYMD(scEfDtESRT);
             Chain keyWmstl pst00;
             If %found(wmstl);
                erStESRT = *on;
                msStESRT = 'WC30005';
                erEfDtESRT = *on;
                msEfDtESRT = 'WC30005';
                isErrSRTEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode


      // Return *on indicator when No error exists.
       If isErrSRTEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateShRt    e

      //===================================================================
      // Validate Short Rates Details Info.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateShRtDt  b
     d ValidateShRtDt  pi              n
     d strmode                        1
     d isErrSRDEdt     s               n   inz
      /Free

       isErrSRDEdt = *off;
      // Intialize all the messageId and error indicators.
       erDaysSRDE = *off;
       msDaysSRDE = *Blanks;
       erPercSRDE = *off;
       msPercSRDE = *Blanks;

       If strmode = 'A';      //Add Mode
         // Days can not be less than or equal to zero
          If scDaysSRDE <= *zeros;
             erDaysSRDE = *on;
             msDaysSRDE = 'WCR0502';
             isErrSRDEdt = *on;
          Endif;
         // Record Must Not Already Exist for Days policy in force
          If isErrSRDEdt = *off;
             KeyEfdt = dtCYMD(scEfDtESRT);
             Chain keyWdstl pst02;
             If %found(wdstl);
                erDaysSRDE = *on;
                msDaysSRDE = 'WC30005';
                isErrSRDEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

       // Percentage of one year premium can not be less than or equal to zero
       If scPercSRDE <= *zeros;
          erPercSRDE = *on;
          msPercSRDE = 'WCR0502';
          isErrSRDEdt = *on;
       Endif;



      // Return *on indicator when No error exists.
       If isErrSRDEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateShRtDt  e

      //===================================================================
      // Validate Small Deductibles Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateSmDe    b
     d ValidateSmDe    pi              n
     d strmode                        1
     d isErrSmDeEdt    s               n   inz
      /Free

       isErrSmDeEdt = *off;
      // Intialize all the messageId and error indicators.
       erEfdtESD = *off;
       msEfdtESD = *blanks;
       erCodeESD = *off;
       msCodeESD = *blanks;
       erStESD = *off;
       msStESD = *blanks;
       erDescECD = *off;
       msDescECD = *blanks;
       erTypeESD = *off;
       msTypeESD = *blanks;
       erDc01E = *off;
       msDc01E = *blanks;
       clear dsErDedRsv;
       clear dsMsDedRsv;
       //*** Beg Add *** 08/08/22 *********************************************
       clear erDbPv1Rt1;
       clear erDbPv1Rt2;
       clear erDbPv1Rt3;
       clear erDbPv1Rt4;
       clear erDbPv1Rt5;
       clear erDbPv1Rt6;
       clear erDbPv1Rt7;
       clear erDbPv1Rt8;
       clear erDbPv1Rt9;
       clear erDbPv1R10;
       clear dsErDbPv1;
       clear erRsvPv101;
       clear erRsvPv102;
       clear erRsvPv103;
       clear erRsvPv104;
       clear erRsvPv105;
       clear erRsvPv106;
       clear erRsvPv107;
       clear erRsvPv108;
       clear erRsvPv109;
       clear erRsvPv110;
       msDpPv1      = 'Duplicate Payment Value 1 Codes';
       msDBpv1Rsv = 'This PV1 code has a reserve type already included ' +
         'in the deductible reserve hierarchy.';
       msDedPv1Rt   = 'This reserve type is assigned to a death benefit ' +
         'PV1 code.';
       //*** End Add *** 08/08/22 *********************************************

       //*** Beg Chg ***** 12/27/23 *****************************
       //*** If strmode = 'A';      //Add Mode
       If strmode = 'A' or strmode = 'P';    //Add Mode or Copy Mode
       //*** End Chg ***** 12/27/23 *****************************
          // Validation Check for State
          If scStESD = *Blanks;
             erStESD = *on;
             msStESD = 'WCR0503';
             isErrSmDeEdt = *on;
          Endif;

          // Validation Check for Discount Code
          If ScCodeESD = *Blanks;
             erCodeESD = *on;
             msCodeESD = 'WCR0503';
             isErrSmDeEdt = *on;
          Endif;

          // Validation Check for Effective Date
          erEfdtESD = ValidDate(scEfdtESD:charMsg);
          If erEfdtESD = *on;
             msEfdtESD = charMsg;
             isErrSmDeEdt = *on;
          Endif;

          // Check for Duplicate record for State/Code/Date
          If isErrSmDeEdt = *off;
             KeyEfdt = dtCYMD(scEfDtESD);
             Chain keyWtddl3 pdd00;
             If %found(wtddl3) and ddstat = 'A';
                erStESD= *on;
                msStESD = 'WC30005';
                erCodeESD = *on;
                msCodeESD = 'WC30005';
                erEfDtESD = *on;
                msEfDtESD = 'WC30005';
                isErrSmDeEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

       // Validation Check for Description
       If scDescECD = *Blanks;
          erDescECD = *on;
          msDescECD = 'WCR0503';
          isErrSmDeEdt = *on;
       Endif;

       // Validation Check for Discount Type (Gross/Net)
       If scTypeESD = *Blanks;
          erTypeESD = *on;
          msTypeESD = 'WCR1162';
          isErrSmDeEdt = *on;
       Endif;

      // Validate Deductible Reserve Code fields
       // First Deductible Reserve Code Can't be Blank
       If visDedRsv = *on and isFL03 = *on and scDc01E = *Blanks;
          // Since we are re-arranging Deductible Reserve Code before Save
          // so this error is required when all 10 Reserve Code are Blank
          If scDc02E = *Blanks and scDc03E = *Blanks and scDc04E = *Blanks and
           scDc05E = *Blanks and scDc06E = *Blanks and scDc07E = *Blanks and
           scDc08E = *Blanks and scDc09E = *Blanks and scDc10E = *Blanks;
             erDc01E = *on;
             msDc01E = 'WCR0503';
             isErrSmDeEdt = *on;
          Endif;
       Endif;

       // Check for Duplicate Deductible Reserve Code
       For i = 1 to 9;
          For j = 2 to 10;
             If i<>j and dsScDedRsv(i)<>*blanks and dsScDedRsv(i)=dsScDedRsv(j);
                dsErDedRsv(i) = *on;
                dsErDedRsv(j) = *on;
                dsMsDedRsv(i) = 'WC30122';
                dsMsDedRsv(j) = 'WC30122';
                isErrSmDeEdt = *on;
             Endif;
          Endfor;
       Endfor;

       //*** Beg Add *** 08/08/22 **********************************************
       // Check all 10 possible reserves codes against Death Benefit
       // 10 possible reserve codes. Give error If you get a match
       If visDedRsv = *on and isErrSmDeEdt = *off and scTypeEsd = 'G' and
        scTotiE <> 'Y';
          ExSr srCmpRsvCodes;
       Endif;
       //*** End Add *** 08/08/22 **********************************************

       // If No Error, for saving in the database file
      // Re-arrange Deductible Reserve Code to Eliminate Blank codes
       If isErrSmDeEdt = *off;
          For i = 1 to 9;
             For j = i+1 to 10;
                If dsScDedRsv(i) = *blanks and dsScDedRsv(j) <> *blanks;
                   dsScDedRsv(i) = dsScDedRsv(j);
                   dsScDedRsv(j) = *blanks;
                Endif;
             Endfor;
          Endfor;
       Endif;

       //*** Beg Add *** 08/08/22 **********************************************
       // Check for Duplicate Payment Value 1 Codes if type = Gross
       If scTypeEsD = 'G' and scTotiE <> 'Y';
          If isErrSmDeEdt = *off;
             For i = 1 to 9;
                For j = 2 to 10;
                   If i<>j and dsScDbPv1(i)<>*blanks and
                    dsScDbPv1(i)=dsScDbPv1(j);
                      dsErDbPv1(i) = *on;
                      dsErDbPv1(j) = *on;
                      isErrSmDeEdt = *on;
                   Endif;
                Endfor;
             Endfor;
          Endif;
       // If No Error, for saving in the database file
      // Re-arrange Death Benefit PV1 Codes to Eliminate Blank codes
          If isErrSmDeEdt = *off;
             For i = 1 to 9;
               For j = i + 1 to 10;
                   If dsScDbPv1(i) = *blanks and dsScDbPv1(j) <> *blanks;
                      dsScDbPv1(i) = dsScDbPv1(j);
                      dsScDbPv1(j) = *blanks;
                   Endif;
                Endfor;
             Endfor;
          Endif;
       Endif;
       //*** End Add *** 08/08/22 **********************************************


      // Return *on indicator when No error exists.
       If isErrSmDeEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;
       //*** Beg Add *** 08/08/22 *********************************************
       //======================================================================
       // srCmpRsvCodes - Compare PV1 Reserves Codes to Death Benefit Res Codes
       //======================================================================
       Begsr srCmpRsvCodes;

       // Cycle through the PV1 Death Benefits array for all 10 elements
       For i = 1 to 10;
          For j = 1 to 10;
             xxKey = dsScDbPv1(j);
             Chain (intCo#:xxKey) pb201;
             // Check for duplicate reserve codes for all 10 fields
             If %Found(WTABL) And dsScDedRsv(i) <> *blanks and
              (dsScDedRsv(i) = B2rC01 Or dsScDedRsv(i) = B2rC02 Or
               dsScDedRsv(i) = B2rC03 Or dsScDedRsv(i) = B2rC04 Or
               dsScDedRsv(i) = B2rC05 Or dsScDedRsv(i) = B2rC06 Or
               dsScDedRsv(i) = B2rC07 Or dsScDedRsv(i) = B2rC08 Or
               dsScDedRsv(i) = B2rC09 Or dsScDedRsv(i) = B2rC10);
                // Set on the correct errOr indicator for one of the 10 Death
                // Benefit fields
                Select;
                   When j = 1;
                      erDbPv1Rt1 = *On;
                   When j = 2;
                      erDbPv1Rt2 = *On;
                   When j = 3;
                      erDbPv1Rt3 = *On;
                   When j = 4;
                      erDbPv1Rt4 = *On;
                   When j = 5;
                      erDbPv1Rt5 = *On;
                   When j = 6;
                      erDbPv1Rt6 = *On;
                   When j = 7;
                      erDbPv1Rt7 = *On;
                   When j = 8;
                      erDbPv1Rt8 = *On;
                   When j = 9;
                      erDbPv1Rt9 = *On;
                   When j = 10;
                      erDbPv1R10 = *On;
                Endsl;
                // Set on the correct error indicator for one of the 10 Reserve
                // Code fields
                Select;
                   When i = 1;
                      erRsvPv101 = *On;
                   When i = 2;
                      erRsvPv102 = *On;
                   When i = 3;
                      erRsvPv103 = *On;
                   When i = 4;
                      erRsvPv104 = *On;
                   When i = 5;
                      erRsvPv105 = *On;
                   When i = 6;
                      erRsvPv106 = *On;
                   When i = 7;
                      erRsvPv107 = *On;
                   When i = 8;
                      erRsvPv108 = *On;
                   When i = 9;
                      erRsvPv109 = *On;
                   When i = 10;
                      erRsvPv110 = *On;
                Endsl;
                isErrSmDeEdt = *On;
                Leave;
             Endif;
          Endfor;
          If isErrSmDeEdt = *on;
             Leave;
          Endif;
       Endfor;

       Endsr;
       //*** End Add *** 08/08/22 *********************************************
     p ValidateSmDe    e

      //===================================================================
      // Validate State Rate Change Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateStRC    b
     d ValidateStRC    pi              n
     d strmode                        1
     d isErrStRCEdt    s               n   inz
      /Free

       isErrStRCEdt = *off;
      // Intialize all the messageId and error indicators.
       erStESRC = *off;
       msStESRC = *blanks;
       erEfdtESRC = *off;
       msEfdtESRC = *blanks;

       If strmode = 'A';      //Add Mode
       //  State should not be blank
          If scStESRC = *Blanks;
             erStESRC = *on;
             msStESRC = 'WCR0503';
             isErrStRCEdt = *on;
          Endif;

       // Validation Check for Effective Date
          erEfdtESRC = ValidDate(scEfDtESRC:charMsg);
          If erEfDtESRC = *on;
             msEfDtESRC = charMsg;
             isErrStRCEdt = *on;
          Endif;

       // Validation check for Duplicate record
          If isErrStRCEdt = *off;
             KeyEfdt = dtCYMD(scEfDtESRC);
             Chain keyWmrtl prt00;
             If %found(wmrtl);
                erStESRC = *on;
                msStESRC = 'WC30005';
                erEfDtESRC = *on;
                msEfDtESRC = 'WC30005';
                isErrStRCEdt = *on;
             Endif;
          Endif;
       Endif;                      //Add mode

      // Return *on indicator when No error exists.
       If isErrStRCEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateStRC    e
      //===================================================================


      // utility procedure definitions
      /COPY SCOPYCOMPI

