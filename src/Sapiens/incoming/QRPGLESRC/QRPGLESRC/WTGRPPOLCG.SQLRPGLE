     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h/title WTGRPPOLCG - Policy Configuration
     h dftname(WTGRPPOLCG)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/22/20                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2196- Redesign of Group Configuration program  *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  08/06/24 C:300 Support: 117247                               *
      //                 1) Group Config / Auto Cash                   *
      //                 Saving info on Billing options 1st Tab-Clears *
      //                 Auto Cash info on Accounting Controls 3rd Tab *
      //                 2) WDCDL file getting locked                  *
      //                                                               *
      //  08/01/24 C:053 Project: WC2975                               *
      //                 Inforce Premium Calc Config                   *
      //                                                               *
      //  06/24/24 C:300 Project: 146WKR (Prescient)                   *
      //                 Collateral Processing                         *
      //                 DSPF: Added new fields on rcPolInf record     *
      //                 format and changes in its onload property     *
      //                                                               *
      //  05/23/24 C:300 Support: 115634 (SUNZ)                        *
      //                 Refresh Button should clear all errors and    *
      //                 Re-load screen with latest file information   *
      //                                                               *
      //  01/25/24 C:344 Project: 133WTD                               *
      //                 Load and store to the new field EXICPSGRP     *
      //                 'IVC Separate 3PY Batch by Group' in the WMEXP*
      //                 file.                                         *
      //                                                               *
      //  10/20/23 C:300 Project: WC2196, Support: 113314              *
      //                 On clicking SAVE button, 'Auto Cash Dissolve' *
      //                 screen data not getting saved due to errors in*
      //                 "Category Codes Included in Hierarchy" Grid.  *
      //                 And these grid errors not showing on screen.  *
      //                 So, showing these errors using popup window   *
      //                                                               *
      //  08/28/23 C:300 Support: 113191                               *
      //                 Action Hyperlinks like Activate/Inactivate    *
      //                 should not be included as last column of excel*
      //                 spreadsheet export                            *
      //                 DSPF: Removed 2 hyperlinks for scActivate and *
      //                 added 4 new hyperlinks for Activate/Inactivate*
      //                 on Rate Set Codes and Agent Rates grids       *
      //                                                               *
      //  06/27/23 C:300 Project: WC2910                               *
      //                 Check User Security from Application Configuration
      //                 instead of User Parameters file               *
      //                                                               *
      //  06/23/23 C:300 Project: WC2196                               *
      //                 "Category Codes" Grids on Accounting Control  *
      //                 tab should not allowed to Edit in Inquiry Mode*
      //                 In DSPF - Added indicator alwdrag on both grids
      //                                                               *
      //  06/23/23 C:300 Project: WC2910, Support: 111868              *
      //                 Display all screens in Inquiry mode, even if  *
      //                 user option security level does not allow them*
      //                 to Edit group information.                    *
      //                                                               *
      // 06/02/23 C:342 Support:108934                                 *
      //                Increased height of panel on Connect Tab       *
      //                                                               *
      //  05/23/23 C:300 Project: WC2910                               *
      //                 - Add User Access Security                    *
      //                                                               *
      //  05/10/23 C:300 Project:W2196 Support:109128                  *
      //                 Do not reload when user re-visit same Tab, if *
      //                 screen is pending with un-saved changes       *
      //                                                               *
      //  04/20/23 C:300 Project:W2196 Support:108859                  *
      //                 Auto-Cash Dissolve Setup issue                *
      //                                                               *
      //  03/24/23 C:300 Project:W2196 Support:107629                  *
      //                 Missing Billing Auxiliary (opt 15)            *
      //                 DSPF: Made changes to rcdAccCnt record format *
      //                  to add sflBilAux subfile grid                *
      //                 PGM: Use sflBilAux grid for search screen     *
      //                                                               *
      //  04/28/22 C:33  Project: WC2670                               *
      //                 Disable the generate premium notice checkboxes*
      //                  if spool file output is not being used       *
      //                                                               *
      //  03/03/22 C:300 Project:W2196                                 *
      //                 For Connect Tab Remove Proc_Tmplt procedure   *
      //                 and use TemplateAssociations procedure        *
      //                                                               *
      // 01/17/22 C:291  Project: WC2788                               *
      //                 Add "Copy Configuration" option to Group      *
      //                 Toolbar.                                      *
      //                                                               *
      //  11/08/21 C:300 Support: 99266 (InHouse)                      *
      //                 Policy page endorsements should be first and  *
      //                 should be the default.                        *
      //                                                               *
      //  08/29/21 c:079 Project WC2663                                *
      //                 Make IC billing option headings flexible      *
      //                 Edit IC billing option checkboxes             *
      //                                                               *
      //  08/02/21 c:318 Project WC2820                                *
      //                 Added New fields for scoring vendors.         *
      //                                                               *
      //  03/11/21 c:62 Project WC2663                                 *
      //                Add Invoice Cloud billing options.             *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTGRPPOLCGcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRtStCd:rrnRtStCd)
     f                                     Sfile(sflPrFnCo:rrnPrFnCo)
     f                                     Sfile(sflAgtRt:rrnAgtRt)
     f                                     Sfile(sflRcdAutC:rrnAutoCod)
     f                                     Sfile(sflRcdAutI:rrnAutoI)
     f                                     Sfile(sflBilAux:rrnBilAux)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
      * Group Auxiliary File
     fwdf2l     uf a e           k disk    rename(pf201:pf200)
     f                                     infds(dbfds11)
      * Admin/Darts Control File
     fwmexl     uf a e           k disk    rename(pex01:pex00)
     f                                     infds(dbfds5)
      * Group Rate Set Code File
     fwmgrp     uf a e             disk    rename(pgr01:pgr0p)
     f                                     infds(dbfds)
     fwmgrl     if   e           k disk    rename(pgr01:pgr00)
     f                                     infds(dbfds2)
      * Rate Set Code Master File
     fwmrsl     if   e           k disk    rename(prs01:prs00)
      * Cancellations Confirmation Control File
     fwmcfl     uf a e           k disk    rename(pcf01:pcf00)
     f                                     infds(dbfds6)
      * Premium Finance Company File
     fwmpfp     uf a e             disk    rename(ppf01:ppf0p)
     f                                     infds(dbfds7)
     fwmpfl     if   e           k disk    rename(ppf01:ppf00)
     f                                     infds(dbfds3)
      * Agent Rate File
     fwmarp     uf a e             disk    rename(par01:par0p)
     f                                     infds(dbfds8)
     fwmarl     if   e           k disk    rename(par01:par00)
     f                                     infds(dbfds4)
      * Cash Dissolve Hierarchy File
     fwdcdl     uf   e           k disk    rename(pcd01:pcd00)
     f                                     infds(dbfds9)
      * A/R Premium Category Information File
     fwtc3l     uf a e           k disk    rename(pc301:pc300)
     f                                     infds(dbfds10)
     f*  Category Table File
     fwtctl     if   e           k disk
      * Logical over all table files
      * WTTTL - A/R code Table File
      * WTBSP - Billing Schedule Code File
     fwtabl     if   e           k disk
      * Billing Auxiliary Codes File
     fwmbxp     uf a e             disk    rename(pbx01:pbx0p)
     f                                     infds(dbfds12)
     fwmbxl     if   e           k disk    rename(pbx01:pbx00)
     f                                     infds(dbfds13)
     fwmbxl1    if   e           k disk    infds(dbfds14)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
     f*** Beg Add *** 05/23/23 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/23/23 *************************************************

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

     d dbfds13         ds
     d  dbrrn13              397    400b 0

     d dbfds14         ds
     d  dbrrn14              397    400b 0


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
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inFnd           s              3  0
     D indexTab        s              5  0
      ***  Begin Add  ***  04/28/22  *****************
     disDisPrmNotAudit...
     d                 s              1n   inz
     disDisPrmNotEstimated...
     d                 s              1n   inz
     disDisPrmNotRevised...
     d                 s              1n   inz
      ***  End   Add  ***  04/28/22  *****************
     d svrrnRtStCd     s              9  0 inz
     d svrrnPrFnCo     s              9  0 inz
     d svrrnBilAux     s              9  0 inz
     d svrrnAgtRt      s              9  0 inz
     D xxRrn           s              9  0 inz
     D xxmode          s              1    inz
     d charMsg         s            100
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d error2          s               n   inz(*off)
     d error3          s               n   inz(*off)
     d process         s               n   inz(*off)
     d xxDelResp       s               n   inz(*off)
      ***  Begin Add  ***  03/24/23  *****************
     D @@bsty          s              1    inz
     D intBlAxCnt      s              5  0 inz
     D svBilFocus      s               n   inz
     D svCatFocus      s               n   inz
      ***  End   Add  ***  03/24/23  *****************
      //
     d intCount        s              2  0 inz
     d #ofARSec        c                   10
     d KeyEfDtE        s                   like(ARefdt)inz
      //
     d keyARCode       s             10    inz
     d countact        s              5  0
     d countinact      s              5  0
     d decStart        s              5  0
     d decEnd          s              5  0
     d i1              s              5  0
     d xxifct          s              1  0 inz(*zeros)
      ***  Begin Add  ***  04/20/23  *****************
     d i2              s              2  0 inz(*zeros)
     d i3              s              2  0 inz(*zeros)
     d xxifct2         s              1  0 inz(*zeros)
     d*
     d* Array to store RRN and Category Description
     d CatRRN          s              9  0 dim(4)
     d CatDesc         s             20    dim(4)
      ***  End   Add  ***  04/20/23  *****************
      ***  Begin Add  ***  05/10/23  *****************
     d t2              s              2  0 inz(*zeros)
     d t3              s              2  0 inz(*zeros)
     d* Array to store Tab index numbers, which is clicked by user before SAVE button
     d ClickTab        s              5  0 dim(4)
      ***  End   Add  ***  05/10/23  *****************
      *** Beg Add *** 05/23/23 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
      *
     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
      *** End Add *** 05/23/23 *************************************************
     d xxdtax          c                   const('DEPOSIT TAX')
     d xxifee          c                   const('INSTALLMENT FEES')
     d strP2GoHdr      s             40
      ***  Begin Add  ***  04/28/22  *****************
     dtipPrmNotDis     c                   'This option is disabled as -
     d                                     spooled file output is no longer -
     d                                     being generated.'
      ***  End   Add  ***  04/28/22  *****************
     d*
     d* Data Base Security Type 1-10
     d*
     d                 ds
     d dsArSecTyp              1     10    dim(#ofARSec)
     d  ARsectyp01             1      1
     d  ARsectyp02             2      2
     d  ARsectyp03             3      3
     d  ARsectyp04             4      4
     d  ARsectyp05             5      5
     d  ARsectyp06             6      6
     d  ARsectyp07             7      7
     d  ARsectyp08             8      8
     d  ARsectyp09             9      9
     d  ARsectyp10            10     10
     d*
     d* Data Base Security Level 1-10
     d*
     d                 ds
     d dsArSecLvl              1     30  0 dim(#ofARSec)
     d  ARseclvl01             1      3  0
     d  ARseclvl02             4      6  0
     d  ARseclvl03             7      9  0
     d  ARseclvl04            10     12  0
     d  ARseclvl05            13     15  0
     d  ARseclvl06            16     18  0
     d  ARseclvl07            19     21  0
     d  ARseclvl08            22     24  0
     d  ARseclvl09            25     27  0
     d  ARseclvl10            28     30  0
     d*
     d* Screen Security Type 1-10
     d*
     d                 ds
     d dsScSecTypE             1     10    dim(#ofARSec)
     d  scSECTYP01             1      1
     d  scSECTYP02             2      2
     d  scSECTYP03             3      3
     d  scSECTYP04             4      4
     d  scSECTYP05             5      5
     d  scSECTYP06             6      6
     d  scSECTYP07             7      7
     d  scSECTYP08             8      8
     d  scSECTYP09             9      9
     d  scSECTYP10            10     10
     d*
     d* Screen Security Level 1-10
     d*
     d                 ds
     d dsScSecLvlE             1     30  0 dim(#ofARSec)
     d  scSECLVL01             1      3  0
     d  scSECLVL02             4      6  0
     d  scSECLVL03             7      9  0
     d  scSECLVL04            10     12  0
     d  scSECLVL05            13     15  0
     d  scSECLVL06            16     18  0
     d  scSECLVL07            19     21  0
     d  scSECLVL08            22     24  0
     d  scSECLVL09            25     27  0
     d  scSECLVL10            28     30  0
     d*
     d* Security Type Error Indicator
     d*
     d                 ds
     d dsErSecTypE             1     10    dim(#ofARSec)
     d  erSECTYP01             1      1
     d  erSECTYP02             2      2
     d  erSECTYP03             3      3
     d  erSECTYP04             4      4
     d  erSECTYP05             5      5
     d  erSECTYP06             6      6
     d  erSECTYP07             7      7
     d  erSECTYP08             8      8
     d  erSECTYP09             9      9
     d  erSECTYP10            10     10
     d*
     d* Security Level Error Indicator
     d*
     d                 ds
     d dsErSecLvlE             1     10    dim(#ofARSec)
     d  erSECLVL01             1      1
     d  erSECLVL02             2      2
     d  erSECLVL03             3      3
     d  erSECLVL04             4      4
     d  erSECLVL05             5      5
     d  erSECLVL06             6      6
     d  erSECLVL07             7      7
     d  erSECLVL08             8      8
     d  erSECLVL09             9      9
     d  erSECLVL10            10     10
     d*
     d* Security Type Message Id
     d*
     d                 ds
     d dsMsSecTypE             1    100    dim(#ofARSec)
     d  msSECTYP01             1     10
     d  msSECTYP02            11     20
     d  msSECTYP03            21     30
     d  msSECTYP04            31     40
     d  msSECTYP05            41     50
     d  msSECTYP06            51     60
     d  msSECTYP07            61     70
     d  msSECTYP08            71     80
     d  msSECTYP09            81     90
     d  msSECTYP10            91    100
     d*
     d* Security Level Message Id
     d*
     d                 ds
     d dsMsSecLvlE             1    100    dim(#ofARSec)
     d  msSecLvl01             1     10
     d  msSecLvl02            11     20
     d  msSecLvl03            21     30
     d  msSecLvl04            31     40
     d  msSecLvl05            41     50
     d  msSecLvl06            51     60
     d  msSecLvl07            61     70
     d  msSecLvl08            71     80
     d  msSecLvl09            81     90
     d  msSecLvl10            91    100
     d*
      //
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1

      // Proto-types

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      // Load New codes for Auto Cash Dissolve Hierarchy
     d Load_NewCodes   pr                  extpgm('FIXWC1179B')
     d inCo#                          3  0
     d inFnd                          3  0

      // Mid-term Endorsements
     d Proc_MidEnd     pr                  extpgm('WTGRPMEND')
     d @@mode                         1
     d xxRrn                          9  0
     d @@next                        10

      // Policy Page Endorsements
     d Proc_PolEnd     pr                  extpgm('WTGRPEND')
     d inCo#                          3  0
     d inFnd                          3  0
     d xxmode                         1
     d xxRrn                          9  0

      * Initialize the Screen fields for Policy Information Tab
     d InzPolInfScVal  pr

      // Load Agent Rate Grid
     d LoadAgtRtGrid   pr

      * Include enough records in the AutoCash subfiles for movement
     d LoadAutoBlank   pr

      * Load AutoCash Grid Active codes
     d LoadAutoCode    pr

      * Load AutoCash Flags
     d LoadAutoFlags   pr

      * Load AutoCash Grid Inactive codes
     d LoadAutoInact   pr

      * Load AutoCash Premium Flags
     d LoadAutoPrem    pr

      // Load Billing Auxiliary Codes Grid
     d LoadBilAuxGrid  pr

      // Load Billing Auxiliary Codes Search Screen
     d LoadBilAuxSrch  pr

      // Load Billing Options
     d LoadBillOpt     pr

      // Load Cancellations Info
     d LoadCancel      pr

      // Load Premium Finance Company Grid
     d LoadPrFnCoGrid  pr

      // Load Rate Set Code Grid
     d LoadRtStCdGrid  pr

      * Main Processing
     d Main            pr

      * Process Agent Rate addition
     d ProcAddAgtRt    pr

      * Process Billing Auxiliary Codes Addition
     d ProcAddBilAux   pr

      * Process Premium Finance Company Addition
     d ProcAddPrFnCo   pr

      * Process Rate Set Code Addition
     d ProcessAddRtSt  pr

      * Process other Options for Agent Rate
     d ProcOthrAgtRt   pr

      * Process other Options for Billing Auxiliary Codes
     d ProcOthrBilAux  pr

      * Process other Options for Premium Finance Company
     d ProcOthrPrFnCo  pr

      * Process other Options for Rate Set Code Grid
     d ProcOtherRtSt   pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * On/Off Subfile Display and Display Control Indicators for rcdAccCnt
     d RcdAccCntDsp    pr
     d  indValue                      1n   const

     * Validate Agent Rate info  procedure.
     d ValidateAgtRt   pr              n
     d strmode                        1

     * Validate Auto Cash Dissolve procedure.
     d ValidAutoCash   pr              n

      * Validate Billing Auxiliary Codes
     d ValidBilAux     pr              n
     d strmode                        1

      * Validate Billing Options
     d ValidBillOpt    pr              n

      * Validate Cancellations Info
     d ValidCancel     pr              n

     * Validate Premium Finance Company info procedure.
     d ValidatePrFnCo  pr              n
     d strmode                        1

     * Validate Rate Set Code Details info. procedure.
     d ValidateRtStCd  pr              n
     d strmode                        1


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrMessage
     d/copy sPrModule
      ***  Begin Add  ***  04/28/22  *****************
      /copy sPrXML
      ***  End   Add  ***  04/28/22  *****************
     d/copy sCopyCompr
     d/copy sPrCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy wPrTable
     d/copy sPrAtrium
     d/copy wPrPolErr
     d/copy wPrGroup
     d/copy witmpltd
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
      ***  Begin Add  ***  03/24/23  *****************
     c                   parm                    @@mmdd            4 0
     c                   parm                    @@barrn           9 0
      ***  End   Add  ***  03/24/23  *****************

     c     keyWMGRL1     klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    scRtCdAE

     c     KeyWMPFL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd
     c                   kfld                    scFcodE

     c     keyWMARL      klist
     c                   kfld                    FNco#
     c                   kfld                    FNfnd
     c                   kfld                    scRt#E
     c                   kfld                    KeyEfDtE

     c     keyWTTTL      klist
     c                   kfld                    FNco#
     c                   kfld                    keyARCode

      /free
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inFnd = FNfnd;
       scCo# = FNco#;
       scFnd = FNfnd;

       //***  Begin Add  ***  03/24/23  *****************
       If (@@next = *Blanks or @@next = 'WTGRPPOLCG');
       //***  End   Add  ***  03/24/23  *****************
       GetClientID(clientID);
       scCltid = clientID;

       ProgramStart(process);

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
       //***  Begin Add  ***  03/24/23  *****************
       Endif;

      // Load Billing Auxiliary Search Screen
       If @@next = 'BILAUX' and @@TabInd <> *Blanks;

          @@bsty = %trim(@@TabInd);
          intBlAxCnt = 0;

         // Load Billing Auxiliary Search Screen
          LoadBilAuxSrch();

          If intBlAxCnt = 1;
             // If called as a search with a specified type, Automatically
             // Return the value IF only one record exists for that type
             @@barrn = scBlAxRRN;
          Else;
             visbPnl = *on;                           // Show Panel for Search Screen
             scTopLay2 = 105;                         // Top of Billing Auxiliary Grid
             Exfmt rcdAccCnt;

             ReadC sflBilAux;
             If btnBlAxSel = *on;
                btnBlAxSel = *off;
                @@barrn = scBlAxRRN;
             Endif;
          Endif;
       Endif;
       //***  End   Add  ***  03/24/23  *****************


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
       // InzPolInfScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzPolInfScVal  b
     d InzPolInfScVal  pi

      /Free
       vis3Rdb  = *off;
       visTaxBlAu  = *off;
       visBillAdd  = *off;
       visFeeInfo = *off;
       visRDevFac = *off;
       visIndp  = *off;

       scCo# = intCo#;
       scFnd = FNfnd;
       scCltid = clientID;

      // Set visibilty indicators
       If clientID = '146' or clientID = '114' or clientID = '152';
          vis3Rdb = *on;
       Endif;

       If clientID = '129';
          visTaxBlAu = *on;
       Endif;

       If clientID = '071';
          visBillAdd  = *on;
       Endif;

       Chain 'WC' swc01;
       If WCfl44 = 'Y';
          visFeeInfo = *on;
       Endif;

       If WCfl11 = 'Y';
          visRDevFac = *on;
       Endif;

       If WCfl38 = 'Y';
          visIndp = *on;
       Endif;

       // Let's see if Invoice Cloud is Installed
       isInvCloud = isModEnabled('Invoice Cloud');
       // Let's get the Invoice Cloud Header
       scICHeader = GetModuleProperty('Invoice Cloud':'Name');
       // Let's get the S/R Online Header and build the flexible headings for Invoice Cloud
       strP2GoHdr = GetModuleProperty('S/R Online':'Name');
       lblICgrp = 'Include Group for ' + %Trim(scICHeader);
       lblICqte = 'Include Quote Bills for ' + %Trim(scICHeader);
       lblICPayAR = %Trim(scICHeader) + ' Payment Direct to A/R';
       lblICp2GAR = %Trim(strP2GoHdr) + ' Payment Direct to A/R';
       lblICcxInv = %Trim(scICHeader) + ' Inactivate Invoices after CX';
       //*** Beg Add *** 01/25/24 *** 133WTD *******************************
       lblICpsGrp = 'Separate 3PY Batches by Group';
       //*** End Add *** 01/25/24 *** 133WTD *******************************
       //*** Beg Add *** 06/24/24 ***************************************
       visCollat = *off;
       If clientID = '146';
          visCollat = *on;
       Endif;
       //*** End Add *** 06/24/24 ***************************************
      /End-Free
     p InzPolInfScVal  e

       //===================================================================
       // Load Agent Rate Grid
       //===================================================================
     p LoadAgtRtGrid   b
     d LoadAgtRtGrid   pi

      /free

       rrnAgtRt = 0;
       dspAgtRt = *off;
       clrAgtRt = *on;
       Write rcdComm;
       clrAgtRt = *off;
       dspAgtRt = *on;
       scRowSel = 1;

       If dspMode;
          inquiry_2 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wmarl;
       Reade (inCo#:inFnd) wmarl;
       Dow not %eof(wmarl);
          rrnAgtRt  += 1;
          scAgtRtSel= *off;

          // set the selection on 1st record for first load
          If svrrnAgtRt = *zeros;
             If rrnAgtRt = 1;
                scAgtRtSel = *on;
             Endif;
          Else;
             If rrnAgtRt = svrrnAgtRt;
                scAgtRtSel = *on;
             Endif;
          Endif;

          scRt# = ARrt#;
          scEfDt = CvtToDate(ARefdt);
          scNwRt = ARnwrt;
          scRnRt = ARrnrt;
          scDesc = ARdesc;
          scPlan = ARplan;

          scAgtRtSts = *Blanks;

          //*** Begin Chg ***** 08/28/23 *****************************
          // scActivate = *Blanks;
          // If ARstat = 'A';
          //    scAgtRtSts= 'Active';
          //    scActivate = 'Inactivate';
          // Endif;
          // If ARstat = 'I';
          //    scAgtRtSts= 'Inactive';
          //    scActivate = 'Activate';
          // Endif;

          visActAgt = *off;                // New Hyperlink for Activate
          visIActAgt = *off;               // New Hyperlink for Inactivate
          If ARstat = 'A';
             scAgtRtSts= 'Active';
             visIActAgt = *on;
          Endif;
          If ARstat = 'I';
             scAgtRtSts= 'Inactive';
             visActAgt = *on;
          Endif;

          If dspMode;
             visActAgt = *off;
             visIActAgt = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          scAgtRtRRN = dbrrn4;

          Write sflAgtRt;
       Reade (inCo#:inFnd) wmarl;
       Enddo;

       rrnAgtRt = 1;
       If svrrnAgtRt <> 0;
          rrnAgtRt = svrrnAgtRt;
          scRowSel = svrrnAgtRt;
          svrrnAgtRt = 0;
       Endif;

      /end-free
     p LoadAgtRtGrid   e

       //===================================================================
       // LoadAutoBlank - Load enough records in the AutoCash subfiles for movement
       //===================================================================
     p LoadAutoBlank   b
     d LoadAutoBlank   pi

      /Free

       decEnd   = countact + countinact;
       decStart = countact + 1;
       For i1 = decStart to decEnd;
          Clear scCtyp;
          Clear scDescAC;
          Clear schier;
          Clear scnbcd;
          Clear sccpcd;
          Clear scGridRRN;
          Clear scRrnCod;
          disTRCodeA = *on;             // Disable dropdown fields
          toolNbcd = *Blanks;
          toolCpcd = *Blanks;
          rrnAutoCod += 1;
          Write sflRcdAutC;
       Endfor;

       decStart = countinact + 1;
       For i1 = decStart to decEnd;
          Clear scCtyp;
          Clear scDescAC;
          Clear schier;
          Clear scnbcd;
          Clear sccpcd;
          Clear scGridRRN;
          Clear scRrnCod;
          rrnAutoI += 1;
          Write sflRcdAutI;
       Endfor;

       rrnAutoCod = 1;
       rrnAutoI = 1;

      /End-Free
     p LoadAutoBlank   e

       //===================================================================
       // LoadAutoCode - Load Autocash Grid
       //===================================================================
     p LoadAutoCode    b
     d LoadAutoCode    pi

      /Free

       rrnAutoCod = 0;
       RcdAccCntDsp(*off);
       clrAutoCod = *on;
       write rcdAccCnt;
       clrAutoCod = *off;
       RcdAccCntDsp(*on);

       // Populate the subfile
       Setll (inCo#:inFnd) wdcdl;
       Reade (inCo#:inFnd) wdcdl;
       Dow not %eof(wdcdl);
          If CDhier < 10000;
             rrnAutoCod += 1;

             scHier = CDhier;
             scNbcd = CDnbcd;
             scCpcd = CDcpcd;
             scCtyp = CDctyp;

             chain (inCo#:scCtyp) Wtctl;
             if %found(wtctl);
                scDescAC = CTdesc;
             endif;
          //***  Begin Chg  ***  04/20/23  *****************
          // keyARCode = scNbcd;
          // chain keyWTTTL ptt01;
          // if %found;
          //    if CDctyp = 'D' and TTtype = 'Q';
          //       scDescAC = xxdtax;
          //    endif;
          // endif;
          // if xxifct = 1 and CDctyp = 'F';
          //    scDescAC = xxifee;
          //    XXifct = 2;
          // endif;


          // Use Array to Re-Store the desription
          if CDctyp = 'D' or CDctyp = 'F';
             i3 = 0;
             i3 = %lookup(dbrrn9:CatRRN);
             If i3 <> 0;
                scDescAC = CatDesc(i3);
             Else;

             // Set Desription for D Category
                keyARCode = scNbcd;
                chain keyWTTTL ptt01;
                if %found;
                   if CDctyp = 'D' and TTtype = 'Q';
                      scDescAC = xxdtax;
                      xxifct2 = 2;
                   endif;
                else;
                   if xxifct2 = 1 and CDctyp = 'D';
                      scDescAC = xxdtax;
                      xxifct2 = 2;
                   endif;
                endif;

             // Set Desription for F Category
                if CDctyp = 'F' and (scNbcd = 'IP' or scNbcd = 'IT');
                   scDescAC = xxifee;
                   XXifct = 2;
                else;
                   if xxifct = 1 and CDctyp = 'F' and scNbcd = '  ';
                      scDescAC = xxifee;
                      XXifct = 2;
                   endif;
                endif;

             // Store the desription in Array
                CatRRN(i2) = dbrrn9;
                CatDesc(i2) = scDescAC;
                i2 = i2 + 1;

             endif;
          endif;
          //***  End   Chg  ***  04/20/23  *****************

            // Set selection criteria for Transfer and Cash Payment A/R codes
             scNbcdCH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
              'TTstat = ' + '''A''' + ' and TTctyp = ' + '''' + scCtyp + '''' +
              ' and TTtype in ('+ '''P''' + ',' + '''Q''' + ',' + '''I''' +')';

             scCpcdCH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
              'TTstat = ' + '''A''' + ' and TTctyp = ' + '''' + scCtyp + '''' +
              ' and TTtype in ('+ '''P''' + ',' + '''Q''' + ',' + '''I''' +')';

             scLastRRNA = rrnAutoCod;
             scRrncod   = dbrrn9;
             scGridrrn  = *zeros;
             disTRCodeA = *off;
             toolNbcd = *Blanks;
             toolCpcd = *Blanks;
             If CDctyp = 'P';
                VisPreCode = *on;
                disTRCodeA = *on;
                toolNbcd = 'Transfer Codes for Premium Categorty are +
                 Maintained Separately';
                toolCpcd='Cash Payment Codes for Premium Categorty are +
                 Maintained Separately';
             Endif;

             //***  Begin Add  ***  06/23/23  *****************
             // Do not Allow to Change Transfer and Cash Payment Codes in Inquiry Mode
             If dspMode;
                disTRCodeA = *on;
             Endif;
             //***  End   Add  ***  06/23/23  *****************

             Write sflRcdAutC;
          Endif;
          Reade (inCo#:inFnd) wdcdl;
       Enddo;

       countact = rrnAutoCod;

      /End-Free
     p LoadAutocode    e

       //===================================================================
       // LoadAutoflags - Load Autocash Flags.
       //===================================================================
     p LoadAutoflags   b
     d LoadAutoflags   pi

      /Free

      // Set selection criteria for New Business Payment/Transfer A/R Code
       scPrcdCH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat = ' + '''A''' + ' and TTctyp = ' + '''P''' +
        ' and TTtype = ' + '''P''';

      // Set selection criteria for Deductible Payment A/R Code
       scDpcdCH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat = ' + '''A''' + ' and TTctyp = ' + '''R''' +
        ' and TTtype = ' + '''Q''';

      // Set selection criteria for Retrospective Payment A/R Code
       scRpcdCH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat = ' + '''A''' + ' and TTctyp = ' + '''C''' +
        ' and TTtype = ' + '''P''';

       //check that record is already present in file.
       chain(n) (inCo#:inFnd) pf200;
       if %found(wdf2l);
          scCaud = F2caud;
          scNpcd = F2npcd;
          scNxcd = F2nxcd;
          scDpcd = F2dpcd;
          scRpcd = F2rpcd;
          scCpyf = F2cpyf;
          scCcyf = F2ccyf;
          scCryf = F2cryf;
          scCpdt = F2cpdt;
          scRpyf = F2rpyf;
          scRcyf = F2rcyf;
          scRryf = F2rryf;
          scRpdt = F2rpdt;
          scApyf = F2apyf;
          scAcyf = F2acyf;
          scAryf = F2aryf;
          scApdt = F2apdt;
          scLpyf = F2lpyf;
          scLcyf = F2lcyf;
          scLryf = F2lryf;
          scLpdt = F2lpdt;

          saveDT7 = F2dt;
          saveTM7 = F2time;
       Endif;

      /End-Free
     p LoadAutoflags   e

       //===================================================================
       // LoadAutoInact - Load Autocash Grid Inactive codes
       //===================================================================
     p LoadAutoInact   b
     d LoadAutoInact   pi

      /Free

       rrnAutoI = 0;
       RcdAccCntDsp(*off);
       clrAutoI = *on;
       write rcdAccCnt;
       clrAutoI = *off;
       RcdAccCntDsp(*on);

       // Populate the subfile
       Setll (inCo#:inFnd) wdcdl;
       Reade (inCo#:inFnd) wdcdl;
       Dow not %eof(wdcdl);
          If CDhier >= 10000;
             rrnAutoI += 1;

             schier = CDhier;
             scnbcd = CDnbcd;
             sccpcd = CDcpcd;
             scCtyp = CDctyp;

             chain (inCo#:scCtyp) Wtctl;
             if %found;
                scDescAC = CTdesc;
             endif;
          //***  Begin Chg  ***  04/20/23  *****************
          // keyARCode = scNbcd;
          // chain keyWTTTL ptt01;
          // if %found;
          //    if CDctyp = 'D' and TTtype = 'Q';
          //       scDescAC = xxdtax;
          //    endif;
          // endif;
          //
          // if xxifct = 1 and CDctyp = 'F';
          //    scDescAC = xxifee;
          //    xxifct = 2;
          // endif;


          // Use Array to Re-Store the desription
          if CDctyp = 'D' or CDctyp = 'F';
             i3 = 0;
             i3 = %lookup(dbrrn9:CatRRN);
             If i3 <> 0;
                scDescAC = CatDesc(i3);
             Else;

             // Set Desription for D Category
                keyARCode = scNbcd;
                chain keyWTTTL ptt01;
                if %found;
                   if CDctyp = 'D' and TTtype = 'Q';
                      scDescAC = xxdtax;
                      xxifct2 = 2;
                   endif;
                else;
                   if xxifct2 = 1 and CDctyp = 'D';
                      scDescAC = xxdtax;
                      xxifct2 = 2;
                   endif;
                endif;

             // Set Desription for F Category
                if CDctyp = 'F' and (scNbcd = 'IP' or scNbcd = 'IT');
                   scDescAC = xxifee;
                   XXifct = 2;
                else;
                   if xxifct = 1 and CDctyp = 'F' and scNbcd = '  ';
                      scDescAC = xxifee;
                      XXifct = 2;
                   endif;
                endif;

             // Store the desription in Array
                CatRRN(i2) = dbrrn9;
                CatDesc(i2) = scDescAC;
                i2 = i2 + 1;

             endif;
          endif;
          //***  End   Chg  ***  04/20/23  *****************

             scLastRRNI = rrnAutoI;
             scRrncod   = dbrrn9;
             scGridrrn  = rrnAutoI;

             Write sflRcdAutI;
          Endif;
          Reade (inCo#:inFnd) wdcdl;
       Enddo;

       countinact = rrnAutoI;

      /End-Free
     p LoadAutoInact   e

       //===================================================================
       // LoadAutoPrem - Load Premium Flags.
       //===================================================================
     p LoadAutoPrem    b
     d LoadAutoPrem    pi

      /Free

      // Set selection criteria for Premium Category A/R codes
       scPrcdCH = 'TTco# = ' + %EditC(inCo#:'X') + ' and ' +
        'TTstat = ' + '''A''' + ' and TTctyp = ' + '''P''' +
        ' and TTtype = ' + '''P''';

       Chain(n) (inCo#:inFnd) wtc3l;
       if %found(wtc3l);
          scCncdPC = C3cncd;
          scCpcdPC = C3cpcd;
          scApcdPC = C3apcd;
          scSpcdPC = C3spcd;

          saveDT6 = C3dt;
          saveTM6 = C3time;
       Endif;

      /End-Free
     p LoadAutoPrem    e

       //===================================================================
       // Load Billing Auxiliary Codes Grid
       //===================================================================
     p LoadBilAuxGrid  b
     d LoadBilAuxGrid  pi

      /free

       rrnBilAux = 0;
       RcdAccCntDsp(*off);
       clrBilAux = *on;
       Write rcdAccCnt;
       clrBilAux = *off;
       RcdAccCntDsp(*on);
       scRowSel2 = 1;

       If dspMode;
          inquiry_1 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wmbxl1;
       Reade (inCo#:inFnd) wmbxl1;
       Dow not %eof(wmbxl1);
          rrnBilAux += 1;
          scBlAxSel = *off;

          // set the selection on 1st record for first load
          If svrrnBilAux = *zeros;
             If rrnBilAux = 1;
                scBlAxSel = *on;
             Endif;
          Else;
             If rrnBilAux = svrrnBilAux;
                scBlAxSel = *on;
             Endif;
          Endif;

          scBCode = BXbscd;
          scBEfDt = BXefdt;
          scExDt = CvtToDate(PFexdt);
          scBType = *Blanks;
          Select;
          When BXtype = 'M';
             scBType = 'Monthly';
          When BXtype = 'Q';
             scBType = 'Quarterly';
          When BXtype = 'A';
             scBType = 'Annually';
          When BXtype = 'O';
             scBType = 'Other';
          When BXtype = 'W';
             scBType = 'Weekly';
          When BXtype = 'T';
             scBType = 'Twice a month';
          Endsl;

          // Get more info from WTBSP (Billing Schedule Code file)
          scBDesc = *Blanks;
          scBPlan = *Blanks;
          scBDepP = *zeros;
          Chain (inCo#:BXbscd) pbs01;
          If %found();
             scBDesc = BSdesc;

             Select;
             When BSlpgm = 'L';
                scBPlan = 'Large Deductible';
             When BSlpgm = 'P';
                scBPlan = 'Paid Loss Retro';
             When BSlpgm = 'I';
                scBPlan = 'Incurred Loss Retro';
             Other;
                scBPlan = BSlpgm;
             Endsl;

             If BSsefl = 'E';               // For Estimating Plan
                Select;
                When BSesdp <> 0;
                     scBDepP = BSesdp;
                Other;
                     scBDepP = BSesd2;
                Endsl;
             Endif;
             If BSsefl = 'S';                 // For Self Reporting Plan
                Select;
                When BSab01 <> 0;
                     scBDepP = BSab01 * 100;
                Other;
                     scBDepP = BSab02 * 100;
                Endsl;
             Endif;
          Endif;

          scBlAxRRN = dbrrn14;

          Write sflBilAux;
          Reade (inCo#:inFnd) wmbxl1;
       Enddo;

       rrnBilAux = 1;
       If svrrnBilAux <> 0;
          rrnBilAux = svrrnBilAux;
          scRowSel2 = svrrnBilAux;
          svrrnBilAux = 0;
       Endif;

      /end-free
     p LoadBilAuxGrid  e

       //***  Begin Add  ***  03/24/23  *****************
       //===================================================================
       // Load Billing Auxiliary Codes Search Screen
       //===================================================================
     p LoadBilAuxSrch  b
     d LoadBilAuxSrch  pi

      /free

       rrnBilAux = 0;
       RcdAccCntDsp(*off);
       clrBilAux = *on;
       Write rcdAccCnt;
       clrBilAux = *off;
       RcdAccCntDsp(*on);

       inquiry_1 = *on;                       //Hide Edit, Delete, Add
       visbPnl = *on;                         //Show Select Button on Search screen

       // Get the Effective MMDD
       Setgt (inCo#:inFnd:@@bsty:@@mmdd) wmbxl;
       Readpe (inCo#:inFnd:@@bsty) wmbxl;
       @@mmdd = BXefdt;

       // Populate the subfile
       Setll (inCo#:inFnd:@@bsty:@@mmdd) wmbxl;
       Reade (inCo#:inFnd:@@bsty:@@mmdd) wmbxl;
       Dow not %eof(wmbxl);
          intBlAxCnt += 1;

          rrnBilAux += 1;
          scBlAxSel = *off;

          If rrnBilAux = 1;
             scBlAxSel = *on;
          Endif;

          scBCode = BXbscd;
          scBEfDt = BXefdt;
          scExDt = CvtToDate(PFexdt);
          scBType = *Blanks;
          Select;
          When BXtype = 'M';
             scBType = 'Monthly';
          When BXtype = 'Q';
             scBType = 'Quarterly';
          When BXtype = 'A';
             scBType = 'Annually';
          When BXtype = 'O';
             scBType = 'Other';
          When BXtype = 'W';
             scBType = 'Weekly';
          When BXtype = 'T';
             scBType = 'Twice a month';
          Endsl;

          // Get more info from WTBSP (Billing Schedule Code file)
          scBDesc = *Blanks;
          scBPlan = *Blanks;
          scBDepP = *zeros;
          Chain (inCo#:BXbscd) pbs01;
          If %found();
             scBDesc = BSdesc;

             Select;
             When BSlpgm = 'L';
                scBPlan = 'Large Deductible';
             When BSlpgm = 'P';
                scBPlan = 'Paid Loss Retro';
             When BSlpgm = 'I';
                scBPlan = 'Incurred Loss Retro';
             Other;
                scBPlan = BSlpgm;
             Endsl;

             If BSsefl = 'E';               // For Estimating Plan
                Select;
                When BSesdp <> 0;
                     scBDepP = BSesdp;
                Other;
                     scBDepP = BSesd2;
                Endsl;
             Endif;
             If BSsefl = 'S';                 // For Self Reporting Plan
                Select;
                When BSab01 <> 0;
                     scBDepP = BSab01 * 100;
                Other;
                     scBDepP = BSab02 * 100;
                Endsl;
             Endif;
          Endif;

          scBlAxRRN = dbrrn13;

          Write sflBilAux;
          Reade (inCo#:inFnd:@@bsty:@@mmdd) wmbxl;
       Enddo;

       rrnBilAux = 1;

      /end-free
     p LoadBilAuxSrch  e
       //***  End   Add  ***  03/24/23  *****************

       //===================================================================
       // Load Billing Options
       //===================================================================
     p LoadBillOpt     b
     d LoadBillOpt     pi

      /free
       //***  Begin Add  ***  04/28/22  *****************
       // Disable premium notice flags?
       isDisPrmNotAudit = not UseSpoolOut('Audit Bill');
       isDisPrmNotEstimated = not UseSpoolOut('Estimated Bill');
       isDisPrmNotRevised = not UseSpoolOut('Revised Bill');
       Clear tipABFl;
       Clear tipOBFl;
       Clear tipRBFl;
       Select;
       When dspMode;
          disABFl = *on;
          disOBFl = *on;
          disRBFl = *on;
       Other;
          disABFl = isDisPrmNotAudit;
          disOBFl = isDisPrmNotEstimated;
          disRBFl = isDisPrmNotRevised;
          If isDisPrmNotAudit;
             tipABFl = tipPrmNotDis;
          Endif;
          If isDisPrmNotEstimated;
             tipOBFl = tipPrmNotDis;
          Endif;
          If isDisPrmNotRevised;
             tipRBFl = tipPrmNotDis;
          Endif;
       Endsl;
       //***  End   Add  ***  04/28/22  *****************

       Chain(n) (inCo#:inFnd) pex00;
       If %found(wmexl);
          scCmsg  = EXcmsg;
          scDmsg  = EXdmsg;
          scBexp  = EXbexp;
          scBest  = EXbest;
          scBasm  = EXbasm;
          scBtyp  = EXbtyp;
          sc#dys  = EX#dys;
          scAjfl  = EXajfl;
          sc3rdb  = EX3rdb;
          scAyfl  = EXayfl;
          scSgra  = EXsgra;
          scAuid  = EXauid;
          scRvid  = EXrvid;
          scAaid  = EXaaid;
          scAoid  = EXaoid;
          scEscd  = EXescd;
          scSrcd  = EXsrcd;
          scDacd  = EXdacd;
          scAacd  = EXaucd;
          scTbac  = EXtbac;
          scQuot  = EXqte ;
          scOrg   = EXorg ;
          scRev   = EXrev ;
          scAud   = EXaud ;
          scAudsr = EXaudsr;
          scRfaf  = EXrfaf;
          scSpcd  = EXspcd;
          scSrtn  = EXsrtn;
          scElp   = EXelp;
          scElpf  = EXelpf;
          scRdf1  = EXrdf1;
          scRdf2  = EXrdf2;
          scRdf3  = EXrdf3;
          scLump  = EXlump;

          scRndi  = EXrndi;
          scObfl  = EXobfl;
          scRbfl  = EXrbfl;
          scAbfl  = EXabfl;
          scBlar  = EXblar;
          scAtar  = EXatar;
          scDdar  = EXddar;
          scQad1  = EXqad1;
          scQad2  = EXqad2;
          scQcty  = EXqcty;
          scQst   = EXqst ;
          scQzp   = EXqzip;
          scRetg  = EXretg;
          scDpac  = EXdpac;
          scDpad  = EXdpad;
          scDfid  = EXdfid;
          scAfid  = EXafid;
          scDe08  = EXde08;
          scArri  = EXarri;
          scPodu  = EXpodu;
          scPybs  = EXpybs;
          scIndp  =  EXindp;
          //*** Beg Add *** 08/01/24 ***************************************
          scBinf  =  EXbinf;
          //*** End Add *** 08/01/24 ***************************************
          scEstAdj=  EXeacd;
          scSupprBal = EXspyb;
          scExcArBl = EXExcArBl;
          scBtx1  = %subst(EXbtxt:1:80);
          scBtx2  = %subst(EXbtxt:81:80);
          scBtx3  = %subst(EXbtxt:161:80);
          // Load Invoice Cloud fields
          scICGrp   = EXicGrp;
          scICQte   = EXicQte;
          scICPayAR = EXicPayAR;
          scICCXInv = EXicCxInv;
          scICP2GAR = EXicP2GAR;
          scICAmPAR = EXicAmPAR;
          scScrVnd  = EXscrVnd;
          scScrVndEf  = dtMDYY(EXscrVndEf);
          scScrVndEx  = dtMDYY(EXscrVndEx);
          //*** Beg Add *** 01/25/24 *** 133WTD ****************************
          scICpsGrp = EXicPsGrp;
          //*** End Add *** 01/25/24 *** 133WTD ****************************
          //*** Beg Add *** 06/24/24 ***************************************
          // Load Collateral Processing fields
          scOcol  = EXocol;
          scColwp = EXcolwp;
          scColrp = EXcolrp;
          scColad = EXcolad;
          scColb  = EXcolb;
          scColp  = EXcolp;
          scColt  = EXcolt;
          scColo  = EXcolo;
          scCold  = EXcold;
          //*** End Add *** 06/24/24 ***************************************
          saveDT  = EXdt;
          saveTM  = EXtime;
          // Load hidden screen fields for IC Edits on Check boxes
          Chain(n) (EXco#:EXfnd) pf200;
          If %Found(WDF2L);
             scIcCpYf = F2cPyf;
             scIcCcYf = F2cCyf;
             scIcCrYf = F2cRyf;
             scIcNpCd = F2nPcd;
             scIcNxCd = F2nXcd;
          Endif;
       Endif;

      /end-free
     p LoadBillOpt     e

       //===================================================================
       // Load Cancellations Info
       //===================================================================
     p LoadCancel      b
     d LoadCancel      pi

      /free

       Chain(n) (inCo#:inFnd) pcf00;
       If %found(wmcfl);
          scCfsd  = DtMDYY(CFcfsd);
          scCfdr  = CFcfdr;
          scNotc  = CFnotc;
          scAurq  = CFaurq;
          scQtrk  = CFqtrk;
          scNecx  = CFnecx;
          scDart  = CFdart;
          scSrfl  = CFsrfl;
          scG301E = CFg301E;
          scG311E = CFg311E;
          scGaSrD = CFgaSrD;

          saveDT2 = CFdt;
          saveTM2 = CFtime;
       Endif;

      /end-free
     p LoadCancel      e

       //===================================================================
       // Load Premium Finance Company Grid
       //===================================================================
     p LoadPrFnCoGrid  b
     d LoadPrFnCoGrid  pi

      /free

       rrnPrFnCo = 0;
       RcdAccCntDsp(*off);
       clrPrFnCo = *on;
       Write rcdAccCnt;
       clrPrFnCo = *off;
       RcdAccCntDsp(*on);
       scRowSel = 1;

       If dspMode;
          inquiry_1 = *on;
       Endif;

       // Populate the subfile
       Setll (inCo#:inFnd) wmpfl;
       Reade (inCo#:inFnd) wmpfl;
       Dow not %eof(wmpfl);
          rrnPrFnCo += 1;
          scPrFnSel = *off;

          // set the selection on 1st record for first load
          If svrrnPrFnCo = *zeros;
             If rrnPrFnCo = 1;
                scPrFnSel = *on;
             Endif;
          Else;
             If rrnPrFnCo = svrrnPrFnCo;
                scPrFnSel = *on;
             Endif;
          Endif;

          scFcod = PFfcod;
          scName = PFname;
          scEfDt = CvtToDate(PFefdt);
          scExDt = CvtToDate(PFexdt);
          scAreaPhn = %int(%editc(PFarea:'X') + %editc(PFphn#:'X'));

          scPrFnRRN = dbrrn3;

          Write sflPrFnCo;
          Reade (inCo#:inFnd) wmpfl;
       Enddo;

       rrnPrFnCo = 1;
       If svrrnPrFnCo <> 0;
          rrnPrFnCo = svrrnPrFnCo;
          scRowSel = svrrnPrFnCo;
          svrrnPrFnCo = 0;
       Endif;

      /end-free
     p LoadPrFnCoGrid  e

       //===================================================================
       // Load Rate Set Code Grid
       //===================================================================
     p LoadRtStCdGrid  b
     d LoadRtStCdGrid  pi

      /free
        // Clear the subfile
        rrnRtStCd = 0;
        dspRtStCd = *off;
        clrRtStCd = *on;
        Write rcdPolInf;
        clrRtStCd = *off;
        dspRtStCd = *on;

        If dspMode;
           inquiry = *on;
        Endif;

        // Populate the subfile
        Setll (inCo#:inFnd) wmgrl;
        Reade (inCo#:inFnd) wmgrl;
        Dow not %eof(wmgrl);
           rrnRtStCd += 1;
           scRtStSel = *off;

           // set the selection on 1st record for first load
           If svrrnRtStCd = *zeros;
              If rrnRtStCd = 1;
                 scRtStSel = *on;
              Endif;
           Else;
              If rrnRtStCd = svrrnRtStCd;
                 scRtStSel = *on;
              Endif;
           Endif;

           scRtcd = GRrtcd;
           scRtStStat = *Blanks;

           //*** Begin Chg ***** 08/28/23 *****************************
           // scActivate = *Blanks;
           // If GRstat = 'A';
           //    scRtStStat= 'Active';
           //    scActivate = 'Inactivate';
           // Endif;
           // If GRstat = 'I';
           //    scRtStStat= 'Inactive';
           //    scActivate = 'Activate';
           // Endif;

           visActSts = *off;                // New Hyperlink for Activate
           visIActSts = *off;               // New Hyperlink for Inactivate
           If GRstat = 'A';
              scRtStStat= 'Active';
              visIActSts = *on;
           Endif;
           If GRstat = 'I';
              scRtStStat= 'Inactive';
              visActSts = *on;
           Endif;

           If dspMode;
              visActSts = *off;
              visIActSts = *off;
           Endif;
           //*** End Chg ***** 08/28/23 *****************************

           //load Rate Set Code Description
           Chain (inCo#:GRrtcd) prs00;
           If %found(wmrsl);
              scDesc = RSdesc;
              ScJrst = RSjrst;
           Endif;

           scRtStRRN = dbrrn2;

           Write sflRtStCd;
        Reade (inCo#:inFnd) wmgrl;
        Enddo;

        rrnRtStCd = 1;
        If svrrnRtStCd <> 0;
           rrnRtStCd = svrrnRtStCd;
           svrrnRtStCd = 0;
        Endif;

      /end-free
     p LoadRtStCdGrid  e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d  isexit         s               n   Inz(*off)
      /free

       //LoadActionBar();

       BtnCancel = *off;

       // Process until exit button is pressed.
       DoW BtnCancel = *Off;

          isEndorse = *off;               // Hide Radion Button

          // Goto tab with errors
          Select;
          When error = *on;
             Tabindex = 0;
          When error3 = *on;
             Tabindex = 2;
          When error2 = *on;
             Tabindex = 4;
          Endsl;

          // display screen
          Exsr ExfmtScreens;

          //***  Begin Add  ***  03/24/23  *****************
          If scCatFoc = 'Y';
             scCatFocus = *On;
             svCatFocus = scCatFocus;         // Repostion the grid after Drag-N-Drop
          Endif;
          //***  End   Add  ***  03/24/23  *****************

          error = *off;
          error2 = *off;
          error3 = *off;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             //***  Begin Del  ***  05/23/24  *****************
             //***  btnRefresh = *Off;
             //***  End   Del  ***  05/23/24  *****************

          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;
             If @@mode <> 'D';
                // Check if WMEXP updated since screen displayed (Billing options)
                flgRcdUpdated = RcdUpdatedErr('WMEXP':dbrrn5:'EXdt':'EXtime':
                 'EXuser':saveDT:saveTM);

                if flgRcdUpdated = '0' and saveDT2 <> 0; // No error
                   // Check if WMCFP updated since screen displayed (Cancellations)
                   flgRcdUpdated = RcdUpdatedErr('WMCFP':dbrrn6:'CFdt':
                    'CFtime':'CFuser':saveDT2:saveTM2);
                EndIf;

                if flgRcdUpdated = '0' and saveDT6 <> 0; // No error
                   // Check if WTC3P updated since screen displayed (A/R Premium Category)
                   flgRcdUpdated = RcdUpdatedErr('WTC3P':dbrrn10:'C3dt':
                    'C3time':'C3user':saveDT6:saveTM6);
                EndIf;

                if flgRcdUpdated = '0' and saveDT7 <> 0; // No error
                   // Check if WDF2P updated since screen displayed (Group Auxiliary)
                   flgRcdUpdated = RcdUpdatedErr('WDF2P':dbrrn11:'F2dt':
                    'F2time':'F2user':saveDT7:saveTM7);
                EndIf;

                if flgRcdUpdated = '0'; // No error

                   // Validate Billing Options (Tab 1)
                   If ValidBillOpt();
                      Exsr UpdPolInfo;
                   Else;
                      error = *on;
                   EndIf;

                   // Validate Auto Cash Dissolve (Tab 3)
                   //***  Begin Add  ***  08/06/24  *****************
                   // Validate only if user has visited Tab 3
                   t3 = 0;
                   t3 = %lookup(3:ClickTab);
                   If t3 <> 0;
                   //***  End   Add  ***  08/06/24  *****************
                   If ValidAutoCash();
                      Exsr UpdAutoCashF;
                      Exsr UpdAutoCodes;
                      Exsr UpdAutoPrmCt;
                   Else;
                      error3 = *on;
                   EndIf;
                   //***  Begin Add  ***  08/06/24  *****************
                   Endif;

                   // Validate only if user has visited Tab 5
                   t3 = 0;
                   t3 = %lookup(5:ClickTab);
                   If t3 <> 0;
                   //***  End   Add  ***  08/06/24  *****************

                   // Validate Cancellations Info (Tab 5)
                   If ValidCancel();
                      Exsr UpdCancel;
                   Else;
                      error2 = *on;
                   EndIf;
                   //***  Begin Add  ***  08/06/24  *****************
                   Endif;
                   //***  End   Add  ***  08/06/24  *****************

                Else;
                   error = *on;
                   error2 = *on;
                   error3 = *on;
                EndIf;

               // Show Data Saved Sucessfully Message
                If error = *off and error2 = *off and error3 = *off;
                   PopDataSaved();

                   //*** Beg Add ***** 05/10/23 *****************************
                   Clear ClickTab;
                   Clear t2;
                   //*** End Add ***** 05/10/23 *****************************
                EndIf;
             Endif;

          // Add Agent Rate info
          When BtnAgRtAdd = *On;
             BtnAgRtAdd = *Off;
             ProcAddAgtRt();

          // Add Rate Set Code Button
          When BtnRtStAdd = *On;
             BtnRtStAdd =*Off;
             ProcessAddRtSt();

          // Add Billing Auxiliary Codes info
          When BtnBlAxAdd = *On;
             BtnBlAxAdd=*Off;
             ProcAddBilAux();

          // Add Premium Finance Company info
          When BtnPrFnAdd = *On;
             BtnPrFnAdd=*Off;
             ProcAddPrFnCo();

          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          //*** End Add ***** 01/17/22 *******************************

          //*** Beg Add *** 05/23/23 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTGRPPOLCG';
             inPgmName     = 'Group Policy Configuration';
             inTabName     =  arrTabNames(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/23/23 ******************************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;
             ProcOthrAgtRt();               //other option for Agent rates
             ProcOthrBilAux();              //other option for Billing Auxiliary Codes
             ProcOthrPrFnCo();              //other option for Premium Finance Company
             ProcOtherRtSt();               // Rate Set Code Edit

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
       When arrTabNames(TabIndex+1) = 'Policy Information';
       //*** End Chg *** 05/23/23 ******************************************
          //*** Beg Chg *** 06/24/24 ***************************************
          //*** pnlHeight = 1390;
          pnlHeight = 1450;
          //*** End Chg *** 06/24/24 ***************************************
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error = *off;
             Clear rcdPolInf;

            // Loading intial values
             InzPolInfScVal();

            // Load Billing Options
             LoadBillOpt();

             //***  Begin Del  ***  05/10/23  *****************
            // Load Rate Set Code Grid
             //*** LoadRtStCdGrid();
             //***  End   Del  ***  05/10/23  *****************
          Endif;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;

          // Load Rate Set Code Grid
          LoadRtStCdGrid();
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdPolInf;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 2;
       When arrTabNames(TabIndex+1) = 'Endorsements/Policy Forms';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 750;
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************
          isEndorse = *on;                // Show Radion Button
          scEndorse = 'P';                // Show Policy Page Endorsements
        //Clear rcdEndorse;

          Write rcdGrpPnl;
          Write rcdPanel;
        //Exfmt rcdEndorse;
          xxmode = @@mode;
          If @@mode = 'D';
             xxmode = '1';
          Endif;
          Proc_PolEnd(inCo#:inFnd:xxmode:xxRrn);
          Read rcdGrpPnl;
          Read rcdPanel;
          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          If btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          Endif;
          //*** End Add ***** 01/17/22 *******************************

          //*** Beg Add *** 05/23/23 ******************************************
          // Add User Access Window Logic
          If btnUsrAcc = *on;
             inDspFileName = 'WTGRPPOLCG';
             inPgmName     = 'Group Policy Configuration';
             inTabName     =  arrTabNames(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          Endif;
          //*** End Add *** 05/23/23 ******************************************

        //Processing of Radio Button
          Dow TabIndex = 1 and scSelOpt = *blanks;
             Select;
             When scEndorse = 'M';        // Show Mid-term Endorsements
                Write rcdGrpPnl;
                Write rcdPanel;
                Proc_MidEnd(@@mode:xxRrn:@@next);
                Read rcdGrpPnl;
                Read rcdPanel;
                //*** Beg Add ***** 01/17/22 *******************************
                // Copy Configuration
                If btnShowWin = *on;
                   PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
                   btnShowWin = *off;
                Endif;
                //*** End Add ***** 01/17/22 *******************************
             When scEndorse = 'P';        // Show Policy Page Endorsements
                Write rcdGrpPnl;
                Write rcdPanel;
                xxmode = @@mode;
                If @@mode = 'D';
                   xxmode = '1';
                Endif;
                Proc_PolEnd(inCo#:inFnd:xxmode:xxRrn);
                Read rcdGrpPnl;
                Read rcdPanel;
                //*** Beg Add ***** 01/17/22 *******************************
                // Copy Configuration
                If btnShowWin = *on;
                   PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
                   btnShowWin = *off;
                Endif;
                //*** End Add ***** 01/17/22 *******************************
             EndSl;

             //*** Beg Add *** 05/23/23 ******************************************
             // Add User Access Window Logic
             If btnUsrAcc = *on;
                inDspFileName = 'WTGRPPOLCG';
                inPgmName     = 'Group Policy Configuration';
                inTabName     =  arrTabNames(TabIndex + 1);
                UserAccessWdw(inDspFileName:inPgmName:inTabName);
                btnUsrAcc = *off;
             Endif;
             //*** End Add *** 05/23/23 ******************************************
          EndDo;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 3;
       When arrTabNames(TabIndex+1) = 'Accounting Controls';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 1410;
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error3 = *off;
             Clear rcdAccCnt;
             // Load Autocash Flags.
             LoadAutoflags();

          //***  Begin Add  ***  05/10/23  *****************
          Endif;
          Endif;
          //***  End   Add  ***  05/10/23  *****************

             // Load Billing Auxiliary Codes Grid
             LoadBilAuxGrid();

             // Load Premium Finance Company Grid
             LoadPrFnCoGrid();

            // Load Auto Cash Dissolve screen
             scCo# = intCo#;
             xxifct = 1;
             //***  Begin Add  ***  04/20/23  *****************
             i2 = 1;
             xxifct2 = 1;
             //***  End   Add  ***  04/20/23  *****************
             scSrcId = *blank;
             scSrcRRN  = 0;
             scTgtId = *blank;
             scTgtRRN = 0;
             // Load New codes for Auto Cash Dissolve Hierarchy
             Load_NewCodes(inCo#:inFnd);
             // Load AutoCash Grid for Active codes.
             LoadAutoCode();
             // Load AutoCash Grid for Inactive codes.
             LoadAutoInact();
             // Include enough records in the AutoCash subfiles for movement
             LoadAutoBlank();

             //***  Begin Add  ***  03/24/23  *****************
             scTopLay2 = 785;                    // Top position of Layout2 Container
             scTopLay3 = 1020;                   // Top position of Layout3 Container
             scBilFocus = svBilFocus;
             svBilFocus = *off;
             scCatFocus = svCatFocus;            // Repostion the grid after Drag-N-Drop
             svCatFocus = *off;
             //***  End   Add  ***  03/24/23  *****************

             // Load Premium Codes for active codes.
             If visPrecode = *on;
                LoadAutoPrem();
                //***  Begin Add  ***  03/24/23  *****************
                scTopLay2 = 915;                    // Top position of Layout2 Container
                scTopLay3 = 1150;                   // Top position of Layout3 Container
                //***  End   Add  ***  03/24/23  *****************
             Endif;
          //***  Begin Del  ***  05/10/23  *****************
          //*** Endif;
          //***  End   Del  ***  05/10/23  *****************

          //***  Begin Add  ***  06/23/23  *****************
          // Allow Drag-N-Drop between Category Codes Grids
          alwdrag = *on;
          //***  End   Add  ***  06/23/23  *****************

          If dspMode;
             inquiry_1 = *on;
             //***  Begin Add  ***  06/23/23  *****************
             // Do not Allow Drag-N-Drop between Category Codes Grids in Inquiry Mode
             alwdrag = *off;
             //***  End   Add  ***  06/23/23  *****************
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdAccCnt;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 4;
       When arrTabNames(TabIndex+1) = 'Commissions';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************
          Clear rcdComm;

         // Load Agent Rate grid
          LoadAgtRtGrid();

          If dspMode;
             inquiry_2 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdComm;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 5;
       When arrTabNames(TabIndex+1) = 'Cancellations';
       //*** End Chg *** 05/23/23 ******************************************
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error2 = *off;
             Clear rcdCancel;

            // Load Cancellations Info
             LoadCancel();
          Endif;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry_3 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdCancel;
          Read  rcdGrpPnl;
          Read rcdPanel;

       //*** Beg Chg *** 05/23/23 ******************************************
       //*** When indexTab = 6;
       When arrTabNames(TabIndex+1) = 'Connect';
       //*** End Chg *** 05/23/23 ******************************************
          //*** Beg Chg *** 06/02/23 ******************************************
          //*** pnlHeight = 750;
          pnlHeight = 880;
          //*** End Chg *** 06/02/23 ******************************************
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************
        //Clear rcdConect;

          Reset dsWITMPLTparms;
          dsWITMPLTparms.co# = inCo#;
          dsWITMPLTparms.fnd = inFnd;
          dsWITMPLTparms.state = *Blanks;

          Write rcdGrpPnl;
          Write rcdPanel;
        //Exfmt rcdConect;
          TemplateAssociations(dsWITMPLTparms:'Group/State':@@Mode);
          Read  rcdGrpPnl;
          Read rcdPanel;


       EndSl;

       Endsr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb;
        EXcmsg  = scCmsg;
        EXdmsg  = scDmsg;
        EXbexp  = scBexp;
        EXbest  = scBest;
        EXbasm  = scBasm;
        EXbtyp  = scBtyp;
        EX#dys  = sc#dys;
        EXajfl  = scAjfl;
        EX3rdb  = sc3rdb;
        EXayfl  = scAyfl;
        EXsgra  = scSgra;
        EXauid  = scAuid;
        EXrvid  = scRvid;
        EXaaid  = scAaid;
        EXaoid  = scAoid;
        EXescd  = scEscd;
        EXsrcd  = scSrcd;
        EXdacd  = scDacd;
        EXaucd  = scAacd;
        EXtbac  = scTbac;
        EXqte   = scQuot;
        EXorg   = scOrg;
        EXrev   = scRev;
        EXaud   = scAud;
        EXaudsr = scAudsr;
        EXrfaf  = scRfaf;
        EXspcd  = scSpcd;
        EXsrtn  = scSrtn;
        EXelp   = scElp ;
        EXelpf  = scElpf;
        EXrdf1  = scRdf1;
        EXrdf2  = scRdf2;
        EXrdf3  = scRdf3;
        EXlump  = scLump;

        EXrndi  = scRndi;
        EXobfl  = scObfl;
        EXrbfl  = scRbfl;
        EXabfl  = scAbfl;
        EXblar  = scBlar;
        EXatar  = scAtar;
        EXddar  = scDdar;
        EXqad1  = scQad1;
        EXqad2  = scQad2;
        EXqcty  = scQcty;
        EXqst   = scQst ;
        EXqzip  = scQzp;
        EXretg  = scRetg;
        EXdpac  = scDpac;
        EXdpad  = scDpad;
        EXdfid  = scDfid;
        EXafid  = scAfid;
        EXde08  = scDe08;
        EXarri  = scArri;
        EXpodu  = scPodu;
        EXpybs  = scPybs;
        EXindp  = scIndp;
        //*** Beg Add *** 08/01/24 ***************************************
        EXbinf  =  scBinf;
        //*** End Add *** 08/01/24 ***************************************
        EXeacd  = scEstAdj;
        EXspyb  = scSupprBal;
        EXExcArBl = scExcArBl;
        EXbtxt  = scBtx1  +  scBtx2  + scBtx3 ;
        // Update Invoice Cloud fields
        EXicGrp   = scICGrp;
        EXicQte   = scICQte;
        EXicPayAR = scICPayAR;
        EXicCxInv = scICCXInv;
        EXicP2GAR = scICP2GAR;
        EXicAmPAR = scICamPAR;
        EXscrVnd  = scScrVnd;
        EXscrVndEf = dtCYMD(scScrVndEf);
        EXscrVndEx = dtCYMD(scScrVndEx);
       //*** Beg Add *** 01/25/24 *** 133WTD *******************************
        EXicPsGrp = scICpsGrp;
       //*** End Add *** 01/25/24 *** 133WTD *******************************
        //*** Beg Add *** 06/24/24 ***************************************
        // Update Collateral Processing fields
        EXocol  = scOcol;
        EXcolwp = scColwp;
        EXcolrp = scColrp;
        EXcolad = scColad;
        EXcolb  = scColb;
        EXcolp  = scColp;
        EXcolt  = scColt;
        EXcolo  = scColo;
        EXcold  = scCold;
        //*** End Add *** 06/24/24 ***************************************
       EndSr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb2;
        CFcfsd  = dtCYMD(scCfsd);
        CFnotc  = scNotc;
        CFaurq  = scAurq;
        CFqtrk  = scQtrk;
        CFnecx  = scNecx;
        CFcfdr  = scCfdr;
        CFdart  = scDart;
        CFsrfl  = scSrfl;
        CFg301E = scG301E;
        CFg311E = scG311E;
        CFgaSrD = scGaSrD;
       EndSr;

       // Subroutine to move Screen fields into database fields.
       BegSr MoveScDb3;
        F2caud  = scCaud;
        F2npcd  = scNpcd;
        F2nxcd  = scNxcd;
        F2dpcd  = scDpcd;
        F2rpcd  = scRpcd;
        F2cpyf  = scCpyf;
        F2ccyf  = scCcyf;
        F2cryf  = scCryf;
        F2cpdt  = scCpdt;
        F2apyf  = scApyf;
        F2acyf  = scAcyf;
        F2aryf  = scAryf;
        F2apdt  = scApdt;
        F2rpyf  = scRpyf;
        F2rcyf  = scRcyf;
        F2rryf  = scRryf;
        F2rpdt  = scRpdt;
        F2lpyf  = scLpyf;
        F2lcyf  = scLcyf;
        F2lryf  = scLryf;
        F2lpdt  = scLpdt;
        //Clear Post Date Type, if Current Year flag is Blank
        If scCcyf = *Blanks;
           F2cpdt = *Blanks;
        Endif;
        If scAcyf = *Blanks;
           F2apdt = *Blanks;
        Endif;
        If scRcyf = *Blanks;
           F2rpdt = *Blanks;
        Endif;
        If scLcyf = *Blanks;
           F2lpdt = *Blanks;
        Endif;
       EndSr;

       // Update Auto Cash Flags subroutine
       BegSr UpdAutoCashF;
        // Load Current info
        Chain (inCo#:inFnd) pf200;
        Exsr MoveScDb3;
        F2user  = q1user;                // User
        F2pgm   = q1pgm;                 // Program
        F2time   = Timeto6(SystemTime()); // Time
        F2dt    = Dateto7(SystemDate()); // Date Mintained
        saveDT7 = F2dt;
        saveTM7 = F2time;
        If %found(wdf2l);
           Update pf200;
        Else;
           F2co#   = FNco#;
           F2fnd   = FNfnd;
           F2iusr  = q1user;                // Initial User
           F2ipgm  = q1pgm;                 // Initial Program
           F2itim  = Timeto6(SystemTime()); // Initial Time
           F2idt   = Dateto7(SystemDate()); // Initial Date
           Write pf200;
        EndIf;
       EndSr;

       // Update Auto Cash Codes subroutine
       BegSr UpdAutoCodes;
        // Read the changed record and update in file.
        For i1 = 1 to decEnd;
           Chain i1 sflRcdAutC;
           If %found;
              If schier > 0;
                 Chain (inCo#:inFnd:schier) pcd00;
                 If %found(wdcdl);
                    If scNbcd <> CDnbcd or scCpcd <> CDcpcd;
                       CDnbcd = scnbcd;
                       CDcpcd = sccpcd;
                       CDpgm  = Q1pgm;
                       CDuser = Q1user;
                       CDdt   = Dateto7(SystemDate());
                       CDtime = Timeto6(SystemTime());
                       Update pcd00;
                    Endif;
                 Endif;
                 //***  Begin Add  ***  08/06/24  *****************
                 Unlock wdcdl;
                 //***  End   Add  ***  08/06/24  *****************
              Endif;
           Endif;
        Endfor;
       EndSr;

       // Update Premium Category Codes
       BegSr UpdAutoPrmCt;
        // Load Current info
        Chain (inCo#:inFnd) pc300;
        C3cncd = scCncdPC;
        C3cpcd = scCpcdPC;
        C3apcd = scApcdPC;
        C3spcd = scSpcdPC;

        C3user  = q1user;                // User
        C3pgm   = q1pgm;                 // Program
        C3time   = Timeto6(SystemTime()); // Time
        C3dt    = Dateto7(SystemDate()); // Date Mintained
        saveDT6 = C3dt;
        saveTM6 = C3time;
        If %found(wtc3l);
           Update pc300;
        Else;
           C3co#   = FNco#;
           C3fnd   = FNfnd;
           C3iusr  = q1user;                // Initial User
           C3ipgm  = q1pgm;                 // Initial Program
           C3idt   = Dateto7(SystemDate()); // Initial Date
           Write pc300;
        EndIf;
       EndSr;

       // Update Cancellations Info Tab subroutine
       BegSr UpdCancel;
        // Load Cancellations info
        Chain (inCo#:inFnd) pcf00;
        Exsr MoveScDb2;
        CFuser  = q1user;                // User
        CFpgm   = q1pgm;                 // Program
        CFtime  = Timeto6(SystemTime()); // Time
        CFdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT2 = CFdt;
        saveTM2 = CFtime;
        If %found(wmcfl);
           Update pcf00;
        Else;
           CFco#   = FNco#;
           CFfnd   = FNfnd;
           CFiusr  = q1user;                // Initial User
           CFipgm  = q1pgm;                 // Initial Program
           CFitim  = Timeto6(SystemTime()); // Initial Time
           CFidt   = Dateto7(SystemDate()); // Initial Date
           Write pcf00;
        EndIf;
       EndSr;

       // Update Policy Info Tab subroutine
       BegSr UpdPolInfo;
        // Load Billig options info
        Chain (inCo#:inFnd) pex00;
        Exsr MoveScDb;
        EXuser  = q1user;                // User
        EXpgm   = q1pgm;                 // Program
        EXtime  = Timeto6(SystemTime()); // Time
        EXdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT  = EXdt;
        saveTM  = EXtime;
        If %found(wmexl);
           Update pex00;
        Else;
           EXco#   = FNco#;
           EXfnd   = FNfnd;
           EXiusr  = q1user;                // Initial User
           EXipgm  = q1pgm;                 // Initial Program
           EXitm   = Timeto6(SystemTime()); // Initial Time
           EXidt   = Dateto7(SystemDate()); // Initial Date
           Write pex00;
        EndIf;
       EndSr;


      /end-free
     p Main            e

       //===================================================================
       // ProcAddAgtRt  - process addition of Agent Rate
       //===================================================================
     P ProcAddAgtRt    B
     D ProcAddAgtRt    PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAgRtDtl;
       mode = 'A';
       isexit = *off;
       BtnAgRtCan = *off;
       AgtRtEdit = *off;

       Dow BtnAgRtCan = *off and isexit = *off;

          scAgtRtHdg = 'Add Agent Rate Master Information';
          scAgtRtStE = 'A';

          Exfmt rcdAgRtDtl;

          Select;

          // Save Button
          When BtnAgRtSav = *on;
             BtnAgRtSav = *off;
             If ValidateAgtRt(mode);
                Exsr WrtAgtRt;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnAgRtCan = *on;
             BtnAgRtCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        ARco# = inCo#;                 // Company Number
        ARfnd = inFnd;                 // Group Number
        ARrt# = scRt#E;               // Rate Number
        ARefdt = dtCYMD(scEfDtE);       // Effective date
        ARdesc = scDescE;               // Description
        ARnwrt = scNwRtE;               // New Rate
        ARrnrt = scRnRtE;               // Renew rate
        ARstat = scAgtRtStE;           // status
        ARplan = scPlanE;             // Plan Type
        ARexpConst = scExConstE;       //Expense Constant
        dsArSecTyp = dsScSecTypE;      //Security Types DS
        dsArSecLvl = dsScSecLvlE;      //Secutity Levels DS
        ARuser = q1user;
        ARpgm = q1pgm;
        ARtime = Timeto6(SystemTime());
        ARdt = Dateto7(SystemDate());
       EndSr;

       // Writing Agent Rate details subroutine
       BegSr WrtAgtRt;
        Clear par0p;
        Exsr MoveScDb;
        Write par0p;
       EndSr;

      /end-free
     P ProcAddAgtRt    E

       //===================================================================
       // ProcAddBilAux - process addition of Billing Auxiliary Codes
       //===================================================================
     P ProcAddBilAux   B
     D ProcAddBilAux   PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdBlAxDtl;
       mode = 'A';
       isexit = *off;
       BtnBlAxCan = *off;
       BilAuxEdt  = *off;
       //***  Begin Add  ***  03/24/23  *****************
       svBilFocus = *On;
       //***  End   Add  ***  03/24/23  *****************

       Dow BtnBlAxCan = *off and isexit = *off;

          scBlAxHdg = 'Add Billing Auxiliary Codes';

          Exfmt rcdBlAxDtl;

          Select;

          // Save Button
          When BtnBlAxSav = *on;
             BtnBlAxSav = *off;
             If ValidBilAux(mode);
                Exsr WrtBilAux;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnBlAxCan = *on;
             BtnBlAxCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        BXco#   = inCo#;                 // Company Number
        BXfnd   = inFnd;                 // Group Number
        BXtype  = scTypeBA;              // Bill Type
        BXefdt  = scEfDtBA;              // Effective Month/Day
        BXbscd  = scBscdBA;              // Billing Code
        BXstat  = 'A';                   // Status

        BXuser = q1user;
        BXpgm  = q1pgm;
        BXtime = Timeto6(SystemTime());
        BXdt   = Dateto7(SystemDate());
        BXiUsr = q1user;
        BXiPgm = q1pgm;
        BXiTm  = Timeto6(SystemTime());
        BXiDt  = Dateto7(SystemDate());
       EndSr;

       // Writing Billing Auxiliary Codes Info
       BegSr WrtBilAux;
        Clear pbx0p;
        Exsr MoveScDb;
        Write pbx0p;
       EndSr;

      /end-free
     P ProcAddBilAux   E

       //===================================================================
       // ProcAddPrFnCo - process addition of premium finance company
       //===================================================================
     P ProcAddPrFnCo   B
     D ProcAddPrFnCo   PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdPrFnDtl;
       mode = 'A';
       isexit = *off;
       BtnPrFnCan = *off;
       PrFnCoEdit = *off;
       //***  Begin Add  ***  03/24/23  *****************
       svBilFocus = *On;
       //***  End   Add  ***  03/24/23  *****************

       Dow BtnPrFnCan = *off and isexit = *off;

          scPrmFnHdg = 'Add Premium Finance Company';

          Exfmt rcdPrFnDtl;

          Select;

          // Save Button
          When BtnPrFnSav = *on;
             BtnPrFnSav = *off;
             If ValidatePrFnCo(mode);
                Exsr WrtPrFnCo;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnPrFnCan = *on;
             BtnPrFnCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        PFco#   = inCo#;                 // Company Number
        PFfnd   = inFnd;                 // Group Number
        PFfcod  = scFcodE;             // Finance company code
        PFefdt  = dtCYMD(scEfDtE);       // Effective date
        PFexdt  = dtCYMD(scExDtE);       // Expiration date
        PFname  = scNameE;               // Name
        PFadr1  = scAdr1E;               // Address 1
        PFadr2  = scAdr2E;               // Address 2
        PFcty   = scCtyE;               // City
        PFst    = scStE;               // State
        PFzip   = scZipE;               //Zip code
        PFcont  = scContE;               //Contact Name

        If scAreaPhnE > *zeros;
           PFarea  = %dec(%subst(%editc(scAreaPhnE:'X'):1:3):3:0); // Phone  Area Code
           PFphn#  = %dec(%subst(%editc(scAreaPhnE:'X'):4):7:0);   // Phone  Number
        EndIf;
        If scAreaPhnE = *zeros;
           PFarea  = *zeros;
           PFphn#  = *zeros;
        EndIf;

        PFtype = scTypeE;
        PFuser = q1user;
        PFpgm  = q1pgm;
        PFtime = Timeto6(SystemTime());
        PFdt   = Dateto7(SystemDate());
        PFiUsr = q1user;
        PFiPgm = q1pgm;
        PFiTm  = Timeto6(SystemTime());
        PFiDt  = Dateto7(SystemDate());
       EndSr;

       // Writing Premium Finance Company Info
       BegSr WrtPrFnCo;
        Clear ppf0p;
        Exsr MoveScDb;
        Write ppf0p;
       EndSr;

      /end-free
     P ProcAddPrFnCo   E

       //===================================================================
       // ProcessAddRtSt- process addition of Rate Set Code
       //===================================================================
     P ProcessAddRtSt  B
     D ProcessAddRtSt  PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdRtStDtl;
       mode = 'A';
       isexit = *off;
       BtnRtStCan = *off;

       Dow BtnRtStCan = *off and isexit = *off;

          scRtStHdg = 'Add Rate Set Code Information';
          visRtStAdS = *On;                           // Show Save/Add next Button
          scRtStAE = 'A';

          Exfmt rcdRtStDtl;

0         Select;
          When BtnRtStAdS = *on;
             BtnRtStAdS = *off;
             If ValidateRtStCd(mode);
                Exsr WrtRtStCd;
          //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdRtStDtl;
                BtnRtStCan = *off;
                scRtStAE = 'A';
             EndIf;

          // Save Button
          When BtnRtStSav = *on;
             BtnRtStSav = *off;
             If ValidateRtStCd(mode);
                Exsr WrtRtStCd;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnRtStCan = *on;
             BtnRtStCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        GRco#   = scCo#;                 // Company Number
        GRfnd   = scFnd;                 // Group Number
        GRrtcd  = scRtCdAE;              // Rate set Code
        GRstat  = scRtStAE;              // Status

        GRuser = q1user;
        GRpgm  = q1pgm;
        GRtime = Timeto6(SystemTime());
        GRdt   = Dateto7(SystemDate());
        GRiUsr = q1user;
        GRiPgm = q1pgm;
        GRiTim = Timeto6(SystemTime());
        GRiDt  = Dateto7(SystemDate());
       EndSr;

       // Writing Rate Set Code subroutine
       BegSr WrtRtStCd;
        Clear pgr0p;
        Exsr MoveScDb;
        Write pgr0p;
       EndSr;

      /end-free
     P ProcessAddRtSt  E

       //===================================================================
       // ProcOthrAgtRt - Process other Options for Agent Rates
       //===================================================================
     P ProcOthrAgtRt   B
     D ProcOthrAgtRt   PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAgRtDtl;
       isexit = *off;
       ReadC sflAgtRt;

       Dow not %eof;

          Select;
          When scAgtRtEdt = *On; // Processing for Edit Agent Rate Info Link

             scAgtRtEdt = *Off;
             mode = 'C';
             AgtRtEdit = *on;           //disable non-editable field
             svrrnAgtRt = rrnAgtRt;     //Save Rrn value

             Chain(n) scAgtRtRRN WMARP;

             Exsr MovDbSc;

             scAgtRtHdg = 'Edit Agent Rate Master Information';

             Dow isexit = *off;
                Exfmt rcdAgRtDtl;

                Select;
                // Save Button
                When BtnAgRtSav = *on;
                   BtnAgRtSav = *off;
                   // Check if WMARP updated since screen displayed (Agent Rate)
                   flgRcdUpdated=RcdUpdatedErr('WMARP':dbrrn8:'ARdt':'ARtime':
                    'ARuser':saveDT3:saveTM3:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scAgtRtRRN WMARP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateAgtRt(mode);
                         Chain scAgtRtRRN WMARP;
                         Exsr MoveScDb;
                         Update par0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnAgRtCan = *on;
                   BtnAgRtCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WMARP;

          Endsl;

          ReadC sflAgtRt;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scRt#E = ARrt#;
        scEfDtE = dtMDYY(ARefdt);
        scDescE = ARdesc;
        scNwRtE = ARnwrt;
        scRnRtE = ARrnrt;
        scAgtRtStE = ARStat;
        scPlanE = ARplan;
        scExConstE = ARexpConst;
        dsScSecTypE = dsArSecTyp;
        dsScSecLvlE = dsArSecLvl;
        saveDT3 = ARdt;
        saveTM3 = ARtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        ARdesc = scDescE;               // Description;
        ARnwrt = scNwRtE;               // New Rate
        ARrnrt = scRnRtE;               // Renew rate
        ARstat = scAgtRtStE;           // status
        ARplan = scPlanE;             // Plan Type

        If scPlanE = 'W';
        ARexpConst = scExConstE;       //Expense Constant
        Else;
        ARexpConst = *Blanks;
        Endif;

        dsArSecTyp = dsScSecTypE;      //Security Types DS
        dsArSecLvl = dsScSecLvlE;      //Secutity Levels DS

        ARuser = q1user;
        ARpgm  = q1pgm;
        ARtime = Timeto6(SystemTime());
        ARdt   = Dateto7(SystemDate());
        saveDT3 = ARdt;
        saveTM3 = ARtime;
       EndSr;

      /end-free
     P ProcOthrAgtRt   E

       //===================================================================
       // ProcOthrPrFnCo- Process other Options for Billing Auxiliary Codes
       //===================================================================
     P ProcOthrBilAux  B
     D ProcOthrBilAux  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdBlAxDtl;
       isexit = *off;
       ReadC sflBilAux;

       Dow not %eof;

          Select;
          When scBlAxEdt= *On; // Processing for Edit Billing Auxiliary Codes Link

             scBlAxEdt = *Off;
             //***  Begin Add  ***  03/24/23  *****************
             svBilFocus = *On;
             //***  End   Add  ***  03/24/23  *****************
             mode = 'C';
             BilAuxEdt = *on ;   //disable non-editable field
             svrrnBilAux = rrnBilAux;   //Save Rrn value

             Chain(n) scBlAxRRN WMBXP;

             Exsr MovDbSc;

             scBlAxHdg = 'Edit Billing Auxiliary Codes';

             Dow isexit = *off;
                Exfmt rcdBlAxDtl;

                Select;
                // Save Button
                When BtnBlAxSav = *on;
                   BtnBlAxSav = *off;
                   // Check if WMBXP updated since screen displayed (Billing Auxiliary)
                   flgRcdUpdated=RcdUpdatedErr('WMBXP':dbrrn12:'BXdt':'BXtime':
                    'BXuser':saveDT8:saveTM8:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scBlAxRRN WMBXP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidBilAux(mode);
                         Chain scBlAxRRN WMBXP;
                         Exsr MoveScDb;
                         Update pbx0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnBlAxCan = *on;
                   BtnBlAxCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scBlAxDlt= *On; // Processing for Delete Billing Auxiliary Codes Link

             scBlAxDlt = *Off;
             //***  Begin Add  ***  03/24/23  *****************
             svBilFocus = *On;
             //***  End   Add  ***  03/24/23  *****************

             Chain(e) scBlAxRRN  WMBXP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scBlAxRRN WMBXP;
             Exsr MovDbSc;

             scBlAxHdg = 'Delete Billing Auxiliary Codes';

             Dow isexit = *off;

                Write rcdBlAxDtl;
                xxDelresp = PopTwoBtn('Do you want to delete this Billing +
                            Auxiliary Codes?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;
                   Delete pbx0p;
                   PopOneBtn('Billing Auxiliary Codes deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svrrnBilAux = rrnBilAux;   //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WMBXP;

          ReadC sflBilAux;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scBscdBA = BXbscd;
        scEfDtBA = BXefdt;
        scTypeBA = BXtype;
        saveDT8 = BXdt;
        saveTM8 = BXtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        BXtype  = scTypeBA;              // Bill Type
        BXefdt  = scEfDtBA;              // Effective Month/Day
        BXbscd  = scBscdBA;              // Billing Code
        BXuser = q1user;
        BXpgm  = q1pgm;
        BXtime = Timeto6(SystemTime());
        BXdt   = Dateto7(SystemDate());
        saveDT8 = BXdt;
        saveTM8 = BXtime;
       EndSr;

      /end-free
     P ProcOthrBilAux  E

       //===================================================================
       // ProcOthrPrFnCo- Process other Options for Premium Finance Company
       //===================================================================
     P ProcOthrPrFnCo  B
     D ProcOthrPrFnCo  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdPrFnDtl;
       isexit = *off;
       ReadC sflPrFnCo;

       Dow not %eof;

          Select;
          When scPrFnEdt= *On; // Processing for Edit Premium finance Company Link

             scPrFnEdt= *Off;
             //***  Begin Add  ***  03/24/23  *****************
             svBilFocus = *On;
             //***  End   Add  ***  03/24/23  *****************
             mode = 'C';
             PrFnCoEdit = *on ;   //disable non-editable field
             svrrnPrFnCo = rrnPrFnCo;   //Save Rrn value

             Chain(n) scPrFnRRN WMPFP;

             Exsr MovDbSc;

             scPrmFnHdg = 'Edit Premium Finance Company';

             Dow isexit = *off;
                Exfmt rcdPrFnDtl;

                Select;
                // Save Button
                When BtnPrFnSav = *on;
                   BtnPrFnSav = *off;
                   // Check if WMPFP updated since screen displayed (Premium Finance)
                   flgRcdUpdated=RcdUpdatedErr('WMPFP':dbrrn7:'PFdt':'PFtime':
                    'PFuser':saveDT4:saveTM4:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scPrFnRRN WMPFP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidatePrFnCo(mode);
                         Chain scPrFnRRN WMPFP;
                         Exsr MoveScDb;
                         Update ppf0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnPrFnCan = *on;
                   BtnPrFnCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When scPrFnDlt= *On; // Processing for Delete Premium Finance Company Link

             scPrFnDlt= *Off;
             //***  Begin Add  ***  03/24/23  *****************
             svBilFocus = *On;
             //***  End   Add  ***  03/24/23  *****************

             Chain(e) scPrFnRRN  WMPFP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scPrFnRRN WMPFP;
             Exsr MovDbSc;

             scPrmFnHdg = 'Delete Premium Finance Company';

             Dow isexit = *off;

                Write rcdPrFnDtl;
                xxDelresp = PopTwoBtn('Do you want to delete this Premium +
                            Finance Company?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;
                   Delete ppf0p;
                   PopOneBtn('Premium Finance Company deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svrrnPrFnCo = rrnPrFnCo;   //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          Unlock WMPFP;

          ReadC sflPrFnCo;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scFcodE     =   PFfcod;
        scEfDtE     =   dtMDYY(PFefdt);
        scExDtE     =   dtMDYY(PFexdt);
        scNameE     =   PFname;
        scAdr1E     =   PFadr1;
        scAdr2E     =   PFadr2;
        scCtyE      =   PFcty;
        scStE       =   PFst;
        scZipE      =   PFzip;
        scContE     =   PFcont;
        If PFarea <> *zeros or PFphn# <> *zeros;
           scAreaPhnE = %int(%editc(PFarea:'X') + %editc(PFphn#:'X'));
        Endif;
        scTypeE     =   PFtype;
        saveDT4 = PFdt;
        saveTM4 = PFtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        PFexdt  = dtCYMD(scExDtE);       // Expiration date
        PFname  = scNameE;               // Name
        PFadr1  = scAdr1E;               // Address 1
        PFadr2  = scAdr2E;               // Address 2
        PFcty   = scCtyE;               // City
        PFst    = scStE;               // State
        PFzip   = scZipE;               //Zip code
        PFcont  = scContE;               //Contact Name

        If scAreaPhnE > *zeros;
           PFarea  = %dec(%subst(%editc(scAreaPhnE:'X'):1:3):3:0); // Phone  Area Code
           PFphn#  = %dec(%subst(%editc(scAreaPhnE:'X'):4):7:0);   // Phone  Number
        EndIf;
        If scAreaPhnE = *zeros;
           PFarea  = *zeros;
           PFphn#  = *zeros;
        EndIf;

        PFtype = scTypeE;
        PFuser = q1user;
        PFpgm  = q1pgm;
        PFtime = Timeto6(SystemTime());
        PFdt   = Dateto7(SystemDate());
        saveDT4 = PFdt;
        saveTM4 = PFtime;
       EndSr;

      /end-free
     P ProcOthrPrFnCo  E

       //===================================================================
       // ProcOtherRtSt - Process other Options for Rate Set Code Grid
       //===================================================================
     P ProcOtherRtSt   B
     D ProcOtherRtSt   PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdRtStDtl;
       isexit = *off;
       ReadC sflRtStCd;

       Dow not %eof;

          // Processing for Edit Rate Set Code Link
          If scRtStEdt = *On;
             scRtStEdt = *Off;
             mode = 'C';

             Chain(n) scRtStRRN wmgrp;
             ScRtCdAE = GRrtcd;
             scRtStAE = GRstat;
             saveDT5 = GRdt;
             saveTM5 = GRtime;

             scRtStHdg= 'Edit Rate Set Code Information';
             svrrnRtStCd = rrnRtStCd;                   //Save RRN value

             Dow isexit = *off;
                Exfmt rcdRtStDtl;

                Select;
                // Save Button
                When BtnRtStSav = *on;
                   BtnRtStSav = *off;
                   // Check if WMGRP updated since screen displayed (Rate Set Code)
                   flgRcdUpdated=RcdUpdatedErr('WMGRP':dbrrn:'GRdt':'GRtime':
                    'GRuser':saveDT5:saveTM5:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRtStRRN wmgrp;
                      ScRtCdAE = GRrtcd;
                      scRtStAE = GRstat;
                      saveDT5 = GRdt;
                      saveTM5 = GRtime;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateRtStCd(mode);
                         Chain scRtStRRN wmgrp;
                         Exsr MoveScDb;
                         Update pgr0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnRtStCan = *on;
                   BtnRtStCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock wmgrp;
          EndIf;

          ReadC sflRtStCd;
       Enddo;


       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        GRrtcd  = scRtCdAE;              // Rate set Code
        GRstat  = scRtStAE;              // Status

        GRuser = q1user;
        GRpgm  = q1pgm;
        GRtime = Timeto6(SystemTime());
        GRdt   = Dateto7(SystemDate());
        saveDT5 = GRdt;
        saveTM5 = GRtime;
       EndSr;

      /end-free
     P ProcOtherRtSt   E

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
             If tabIndex < 0 or tabIndex > 4;
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

       // Policy Information tab
       Chain (intCo#:'WTGRPPOLCG':'Group Policy Configuration':
        'Policy Information':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Policy Information':arrTabNames:n1);
       Endif;

       // Endorsements/Policy Forms tab
       Chain (intCo#:'WTGRPPOLCG':'Group Policy Configuration':
        'Endorsements/Policy Forms':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Endorsements/Policy Forms':arrTabNames:n1);
       Endif;

       // Accounting Controls tab
       Chain (intCo#:'WTGRPPOLCG':'Group Policy Configuration':
        'Accounting Controls':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Accounting Controls':arrTabNames:n1);
       Endif;

       // Commissions tab
       Chain (intCo#:'WTGRPPOLCG':'Group Policy Configuration':
        'Commissions':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Commissions':arrTabNames:n1);
       Endif;

       // Cancellations tab
       Chain (intCo#:'WTGRPPOLCG':'Group Policy Configuration':
        'Cancellations':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Cancellations':arrTabNames:n1);
       Endif;

       // Connect tab
       Chain (intCo#:'WTGRPPOLCG':'Group Policy Configuration':
        'Connect':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Connect':arrTabNames:n1);
       Endif;

       TbNmRcdPnl = strTabNames;
       //*** End Add *** 05/23/23 ******************************************

       Return;


      /end-free
     p ProgramStart    e

       //===================================================================
       // Load display indicators for sub-files in rcdAccCnt Record Format
       //===================================================================
     P RcdAccCntDsp    b
     D RcdAccCntDsp    pi
     D  indValue                      1n   const
      /Free
       dspBilAux = indValue;
       dspPrFnCo = indValue;
       dspAutocod = indValue;
       dspAutoI = indValue;
      /End-Free
     P RcdAccCntDsp    e

      //===================================================================
      // Validate Agent Rate Master Information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAgtRt   b
     d ValidateAgtRt   pi              n
     d strmode                        1
     d isErrAgRtEdt    s               n   inz
      /Free

       isErrAgRtEdt=*off;
      // Intialize all the messageId and error indicators.
       erRt#E= *off;
       msRt#E = *blanks;
       erEfdtE = *off;
       msEfdtE = *blanks;
       erDescE = *off;
       msDescE = *blanks;
       clear     dsErSecTypE;
       clear     dsErSecLvlE;
       clear     dsMsSecTypE;
       clear     dsMsSecLvlE;
       Eval      KeyEfdtE= dtCYMD(scEfDtE);

       If strmode = 'A';      //Add Mode
          // Validation Check for Effective Date
          erEfdtE = ValidDate(scEfdtE:charMsg);
          If erEfdtE = *on;
             msEfdtE = charMsg;
             isErrAgRtEdt = *on;
          Endif;
          //Rate can not be less than zero
          If scRt#E <= 0;
             erRt#E = *on;
             msRt#E = 'WCR0502';
             isErrAgRtEdt = *on;
          Endif;
          //  check to be sure rate has not been used previously with same Eff date
          Chain keyWMARL wmarl;
          If %found(wmarl);
             erRt#E = *on;
             msRt#E = 'WC30005';
             erEfdtE = *on;
             msEfdtE = 'WC30005';
             isErrAgRtEdt = *on;
          Endif;
       Endif;                      //Add mode

       // Rate Description Can't be Blank
       If scDescE = *blanks;
          erDescE = *on;
          msDescE ='WCR0503';
          isErrAgRtEdt = *on;
       Endif;

       // Valiation for Security Type and Level
       For intCount = 1 to #ofARSec;

          // Security Levels and Types are mutually inclusive
          If dsScSecTypE(intCount) = *blanks and dsScSecLvlE(intCount) > 0
           or dsScSecTypE(intCount) <> *blanks and dsScSecLvlE(intCount) = 0;
              dsErSecTypE(intCount) = *on;
              dsErSecLvlE(intCount) = *on;
              dsMsSecTypE(intCount) = 'WCR0544';
              dsMsSecLvlE(intCount) = 'WCR0544';
              isErrAgRtEdt = *on;
          Endif;

       Endfor;

      // Return *on indicator when No error exists.
       If isErrAgRtEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateAgtRt   e

      //===================================================================
      // Validate Auto Cash Dissolve Info
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidAutoCash   b
     d ValidAutoCash   pi              n
     d isError4        s               n   inz
      ***  Begin Add  ***  10/20/23  *****************
     d showerror       s               n   inz('0')
      ***  End   Add  ***  10/20/23  *****************

      /Free
       isError4 = *Off;
      // Intialize all the messageId and error indicators.
       erCcyf = *off;
       mgCcyf = *blanks;
       erAcyf = *off;
       mgAcyf = *blanks;
       erRcyf = *off;
       mgRcyf = *blanks;
       erLcyf = *off;
       mgLcyf = *blanks;
       erCpdt = *off;
       msCpdt = *blanks;
       erApdt = *off;
       msApdt = *blanks;
       erRpdt = *off;
       msRpdt = *blanks;
       erLpdt = *off;
       msLpdt = *blanks;
       ernpcd = *off;
       msnpcd = *blanks;
       ernxcd = *off;
       msnxcd = *blanks;
       erNbcd = *off;
       msNbcd = *blanks;
       erCpcd = *off;
       msCpcd = *blanks;
       erCncdPC = *off;
       msCncdPC = *blanks;
       erCpcdPC = *off;
       msCpcdPC = *blanks;
       erApcdPC = *off;
       msApcdPC = *blanks;
       erSpcdPC = *off;
       msSpcdPC = *blanks;

       // Validation Current Year values for Cash payment,Audit bill,Revised bill,Lockbox.
       If scCcyf = *blank and (sccpyf <> *blank or sccryf <> *blank);
          mgCcyf = 'Current Year must be ''Y'' if Prior Year or +
           Renewal Year is ''Y''';
          erCcyf = *On;
          isError4 = *On;
       Endif;
       If scAcyf = *blank and (scApyf <> *blank or scAryf <> *blank);
          mgAcyf = 'Current Year must be ''Y'' if Prior Year or +
           Renewal Year is ''Y''';
          erAcyf = *On;
          isError4 = *On;
       Endif;
       If scRcyf = *blank and (scRpyf <> *blank or scRryf <> *blank);
          mgRcyf = 'Current Year must be ''Y'' if Prior Year or +
           Renewal Year is ''Y''';
          erRcyf = *On;
          isError4 = *On;
       Endif;
       If scLcyf = *blank and (scLpyf <> *blank or scLryf <> *blank);
          mgLcyf = 'Current Year must be ''Y'' if Prior Year or +
           Renewal Year is ''Y''';
          erLcyf = *On;
          isError4 = *On;
       Endif;

       // Post Date Type can not be blank, If Current Year selected
       // for Cash payment,Audit bill,Revised bill,Lockbox.
       If scCpdt = *blank and scCcyf <> *blanks;
          erCpdt = *On;
          msCpdt = 'WCR0503';
          isError4 = *On;
       Endif;
       If scApdt = *blank and scAcyf <> *blanks;
          erApdt = *On;
          msApdt = 'WCR0503';
          isError4 = *On;
       Endif;
       If scRpdt = *blank and scRcyf <> *blanks;
          erRpdt = *On;
          msRpdt = 'WCR0503';
          isError4 = *On;
       Endif;
       If scLpdt = *blank and scLcyf <> *blanks;
          erLpdt = *On;
          msLpdt = 'WCR0503';
          isError4 = *On;
       Endif;

       // Validate New Business Payment A/R Code
       // Mandatory if 'Include Quote Bills for Invoice Cloud' checkbox 'Y' on 1st tab
       If scNpcd = *blank and scICQte = 'Y';
          erNpcd = *On;
          msNpcd = 'WCR0503';
          isError4 = *On;
       Endif;

       // Validate New Business Payment Transfer A/R Code
       // Mandatory if 'Include Quote Bills for Invoice Cloud' checkbox 'Y' on 1st tab
       If scNxcd = *blank and scICQte = 'Y';
          erNxcd = *On;
          msNxcd = 'WCR0503';
          isError4 = *On;
       Endif;


      // Validate Active Category Codes
       ReadC sflRcdAutC;
       Dow not %eof;
          If scCtyp <> 'P' and schier <> 0;
          // Validate Transfer A/R Codes
             If scNbcd = *Blanks;
                erNbcd = *on;
                msNbcd = 'WCR0503';
                isError4 = *On;
                //***  Begin Add  ***  10/20/23  *****************
                showerror = *on;
                //***  End   Add  ***  10/20/23  *****************
             Endif;
          //***  Begin Del  ***  04/20/23  *****************
          // If scNbcd <> *Blanks;
          //    keyARCode = scNbcd;
          //    chain keyWTTTL ptt01;
          //    If %found();
          //       If TTstat <> 'A' or TTctyp <> scCtyp;
          //          erNbcd = *on;
          //          msNbcd = 'WC30352';
          //          isError4 = *On;
          //       Endif;
          //    Else;
          //       erNbcd = *on;
          //       msNbcd = 'WC30352';
          //       isError4 = *On;
          //    Endif;
          // Endif;
          //***  End   Del  ***  04/20/23  *****************

          // Validate Cash Payment A/R Codes
          //***  Begin Del  ***  04/20/23  *****************
          // If scCpcd <> *Blanks;
          //    keyARCode = scCpcd;
          //    chain keyWTTTL ptt01;
          //    If %found();
          //       If TTstat <> 'A' or TTctyp <> scCtyp;
          //          erCpcd = *on;
          //          msCpcd = 'WC30352';
          //          isError4 = *On;
          //       Endif;
          //    Else;
          //       erCpcd = *on;
          //       msCpcd = 'WC30352';
          //       isError4 = *On;
          //    Endif;
          // Endif;
          //***  End   Del  ***  04/20/23  *****************
          Endif;

          If erNbcd = *on or erCpcd = *on;
             Update sflRcdAutC;
          Endif;

          ReadC sflRcdAutC;
       Enddo;


      // Validate Premium Category Codes
       //***  Begin Add  ***  04/20/23  *****************
       If VisPreCode = *on;
       //***  End   Add  ***  04/20/23  *****************
       If scCncdPC = *Blanks;
          erCncdPC = *on;
          msCncdPC = 'WCR0503';
          isError4 = *On;
       Endif;
       If scCpcdPC = *Blanks;
          erCpcdPC = *on;
          msCpcdPC = 'WCR0503';
          isError4 = *On;
       Endif;
       If scApcdPC = *Blanks;
          erApcdPC = *on;
          msApcdPC = 'WCR0503';
          isError4 = *On;
       Endif;
       If scSpcdPC = *Blanks;
          erSpcdPC = *on;
          msSpcdPC = 'WCR0503';
          isError4 = *On;
       Endif;

       // Make sure that New Business codes aren't used for the premium payment codes.
       // F2npcd and F2nxcd of wdf2l file
       If scCncdPC = scNpcd or scCncdPC = scNxcd;
          erCncdPC = *on;
          msCncdPC = 'WC30357';
          isError4 = *On;
       Endif;
       If scCpcdPC = scNpcd or scCpcdPC = scNxcd;
          erCpcdPC = *on;
          msCpcdPC = 'WC30357';
          isError4 = *On;
       Endif;
       If scApcdPC = scNpcd or scApcdPC = scNxcd;
          erApcdPC = *on;
          msApcdPC = 'WC30357';
          isError4 = *On;
       Endif;
       If scSpcdPC = scNpcd or scSpcdPC = scNxcd;
          erSpcdPC = *on;
          msSpcdPC = 'WC30357';
          isError4 = *On;
       Endif;
       //***  Begin Add  ***  04/20/23  *****************
       Endif;
       //***  End   Add  ***  04/20/23  *****************

       //***  Begin Add  ***  10/20/23  *****************
       If showerror = *on;
          PopOneBtn('Please Correct Errors in Included Category Codes Grid.':
           'Data Error');
       Endif;
       //***  End   Add  ***  10/20/23  *****************


      // Return *on indicator when No error exists.
       If isError4 = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidAutoCash   e

      //===================================================================
      // Validate Billing Auxiliary Codes information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidBilAux     b
     d ValidBilAux     pi              n
     d strmode                        1
     d isErrBlAxEdt    s               n   inz
      /Free

       isErrBlAxEdt=*off;
      // Intialize all the messageId and error indicators.
       erBscdBA = *off;
       msBscdBA = *blanks;
       erEfDtBA = *off;
       msEfDtBA = *blanks;
       erTypeBA = *off;
       msTypeBA = *blanks;


       If strmode <> 'C';                       //Add Mode
         // Billing Auxiliary Code can't be blank
          If scBscdBA = *blanks;
             erBscdBA = *on;
             msBscdBA = 'WCR0503';
             isErrBlAxEdt = *on;
          Endif;

       // Billing Auxiliary Code must be unique
          If scBscdBA <> *blanks;
             Chain(n) (inCo#:inFnd:scBscdBA) wmbxl1;
             If %found(wmbxl1);
                erBscdBA = *on;
                msBscdBA = 'WC30005';
                isErrBlAxEdt = *on;
             Endif;
          Endif;
       Endif;

      // Effective Month/Day can't be Zero
       If scEfDtBA = *zeros;
          erEfDtBA = *on;
          msEfDtBA = 'WCR0556';
          isErrBlAxEdt = *on;
       Endif;

      // Bill Type can't be blank
       If scTypeBA = *blanks;
          erTypeBA = *on;
          msTypeBA = 'WCR0503';
          isErrBlAxEdt = *on;
       Endif;

      // Return *on indicator when No error exists.
       If isErrBlAxEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidBilAux     e

      //===================================================================
      // Validate Billing Options
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidBillOpt    b
     d ValidBillOpt    pi              n
     d isError         s               n   inz

      /Free
       isError = *off;
       erQad1   = *off;
       msQad1   = *blanks;
       erQcty   = *off;
       msQcty   = *blanks;
       erQst    = *off;
       msQst    = *blanks;
       erQzp    = *off;
       msQzp    = *blanks;
       erPodu   = *off;
       msPodu   = *blanks;
       erScrVndEx = *off;
       erScrVndEf = *off;
       msScrVndEx = *blanks;
       msScrVndEf = *blanks;
       msScrVnd   = *blanks;
       erScrVnd   = *off;
       //*** Beg Add *** 06/24/24 ***************************************
       erColb = *off;
       msColb = *blanks;
       erColp = *off;
       msColp = *blanks;
       erColt = *off;
       msColt = *blanks;
       erColo = *off;
       msColo = *blanks;
       erCold = *off;
       msCold = *blanks;
       erColb2= *off;
       mgColb = *blanks;
       erColp2= *off;
       mgColp = *blanks;
       erColt2= *off;
       mgColt = *blanks;
       erColo2= *off;
       mgColo = *blanks;
       //*** End Add *** 06/24/24 ***************************************
       //Validate Quote Billing Address
       If scQad1 <> *blanks or scQad2 <> *blanks or
        scQcty <> *blanks or scQst <> *blanks or scQzp <> 0;
          If scQad1 = *Blanks;
             erQad1 = *on;
             msQad1 = 'WCR0503';
             isError = *on;
          Endif;
          If scQcty = *blanks;
             erQcty = *on;
             msQcty = 'WCR0503';
             isError = *on;
          Endif;
          If scQst  = *blanks;
             erQst  = *on;
             msQst  = 'WCR0503';
             isError = *on;
          Endif;
          If scQzp = *zeros;
             erQzp = *on;
             msQzp = 'WCR0556';
             isError = *on;
          Endif;
       Endif;

       // Deductible due date Based on - Can't be Blank
       If scPodu = ' ';
          erPodu = *On;
          msPodu = 'WCR0503';
          isError = *On;
       Endif;
       If (scScrVnd <> *Blanks and scScrVndEf = *Zeros) or
           (scScrVnd = *Blanks and scScrVndEf <> *Zeros);
          msScrVnd = 'WCR0544';
          erScrVnd = *on;
          msScrVndEf = 'WCR0544';
          erScrVndEf = *on;
          isError = *On;
       Endif;
       //Scoring Vendor  effective date and expiry date are mutually inclusive
       If (scScrVndEf = *zeros and scScrVndEx <> *Zeros);
          msScrVndEx = 'WCR0544';
          erScrVndEx = *on;
          msScrVndEf = 'WCR0544';
          erScrVndEf = *on;
          isError = *On;
       Endif;
       // Validation Check for Effective Date
       If scScrVndEf <> *Zeros;
          erScrVndEf = ValidDate(scScrVndEf:charMsg);
          If erScrVndEf = *on;
             msScrVndEf = charMsg;
             isError = *On;
          Endif;
       Endif;
       // Validation Check for Expiration Date
       If scScrVndEx <> *Zeros;
          erScrVndEx = ValidDate(scScrVndEx:charMsg);
          If erScrVndEx = *on;
             msScrVndEx = charMsg;
             isError = *On;
          Endif;
       Endif;
       // Dates must be in Ascending order
       If scScrVndEf <> *Zeros and scScrVndEx <> *Zeros and
        erScrVndEf = *off and erScrVndEx = *off;
          erScrVndEx = AscendDates(scScrVndEf:scScrVndEx:charMsg);
          If erScrVndEx = *on;
             erScrVndEf = *on;
             msScrVndEf = charMsg;
             msScrVndEx = charMsg;
             isError = *On;
          Endif;
       Endif;
       //*** Beg Add *** 06/24/24 ***************************************
       // Validation Check for Collateral Processing fields
       If scOcol = 'Y';              // Offer Collateral
          If scColb = *blanks;
             erColb = *On;
             msColb = 'WCR0503';
             isError = *On;
          Endif;
          If scColp = *blanks;
             erColp = *On;
             msColp = 'WCR0503';
             isError = *On;
          Endif;
          If scColt = *blanks;
             erColt = *On;
             msColt = 'WCR0503';
             isError = *On;
          Endif;
          If scColo = *blanks;
             erColo = *On;
             msColo = 'WCR0503';
             isError = *On;
          Endif;
          If scCold = *blanks;
             erCold = *On;
             msCold = 'WCR0503';
             isError = *On;
          Endif;
          If scColb = scColo and scColb <> ' ';
             erColb2= *On;
             mgColb = 'The same code cannot be selected for both.';
             erColo2= *On;
             mgColo = 'The same code cannot be selected for both.';
             isError = *On;
          Endif;
          If scColp = scColt and scColp <> ' ';
             erColp2= *On;
             mgColp = 'The same code cannot be selected for both.';
             erColt2= *On;
             mgColt = 'The same code cannot be selected for both.';
             isError = *On;
          Endif;
       Endif;
       //*** End Add *** 06/24/24 ***************************************

      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free

     p ValidBillOpt    e

      //===================================================================
      // Validate Cancellations Info
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidCancel     b
     d ValidCancel     pi              n
     d isError3        s               n   inz

      /Free
       isError3 = *off;
       erCfsd = *off;
       msCfsd = *Blanks;
       erCfdr = *off;
       msCfdr = *Blanks;

       //Validate Cancellations Effective Start Date, Only if any of the field is input
       If scCfDr <> ' ' or scNotc <> ' ' or scDart <> ' ' or scAurq <> ' '
        or scNecx <> ' ' or scQtrk <> ' ' or scG301E <> ' ' or scG311E <> ' '
        or scSrfl <> ' ' or scGaSrD <> ' ';
          erCfsd = ValidDate(scCfsd:charMsg);
          If erCfsd = *on;
             msCfsd = charMsg;
             isError3 = *on;
          Endif;
       Endif;

       // Confirmation Remark Code can't be Blank, if
       // 'Resolve Document to iDarts' checkbox is 'Y'
       If scCfDr = ' ';
          If scDart <> ' ';
             erCfdr = *on;
             msCfdr = 'WCR0878';
             isError3 = *on;
          Endif;
       Endif;


      // Return *on indicator when No error exists.
       If isError3 = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free

     p ValidCancel     e

      //===================================================================
      // Validate Premium Finance Company information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidatePrFnCo  b
     d ValidatePrFnCo  pi              n
     d strmode                        1
     d isErrPrFnEdt    s               n   inz
      /Free

       isErrPrFnEdt=*off;
      // Intialize all the messageId and error indicators.
       erFcodE= *off;
       msFcodE = *blanks;
       erEfdtE= *off;
       msEfdtE = *blanks;
       erExdtE= *off;
       msExdtE = *blanks;
       erNameE= *off;
       msNameE = *blanks;
       erAdr1E= *off;
       msAdr1E = *blanks;
       erCtyE= *off;
       msCtyE = *blanks;
       erStE= *off;
       msStE = *blanks;
       erZipE= *off;
       msZipE = *blanks;

       // Validation Check for Effective Date
       erEfdtE = ValidDate(scEfdtE:charMsg);
       If erEfdtE = *on;
          msEfdtE = charMsg;
          isErrPrFnEdt = *on;
       Endif;

       // Validation Check for Expiration Date
       If scExdtE <> 0;
          erExdtE = ValidDate(scExdtE:charMsg);
          If erExdtE = *on;
             erExdtE = charMsg;
             isErrPrFnEdt = *on;
          Endif;
       Endif;

       // Dates must be in Ascending order
       If scEfdtE <> 0 and scExdtE <> 0;
          If erEfdtE = *off and erExdtE = *off;
             erExdtE = AscendDates(scEfdtE:scExdtE:charMsg);
             If erExdtE = *on;
                erEfdtE = *on;
                msEfDtE = charMsg;
                msExDtE = charMsg;
                isErrPrFnEdt = *on;
             Endif;
          Endif;
       Endif;

      // Finance Company Code can't be blank
       If scFcodE = *blanks;
          erFcodE = *on;
          msFcodE = 'WCR0503';
          isErrPrFnEdt = *on;
       Endif;

      // Finance Company Name can't be blank
       If scNameE = *blanks;
          erNameE = *on;
          msNameE = 'WCR0503';
          isErrPrFnEdt = *on;
       Endif;

      // Address can't be blank
       If scAdr1E = *blanks;
          erAdr1E = *on;
          msAdr1E = 'WCR0503';
          isErrPrFnEdt = *on;
       Endif;

       If scCtyE = *blanks;
          erCtyE = *on;
          msCtyE = 'WCR0503';
          isErrPrFnEdt = *on;
       Endif;

       If scStE = *blanks;
          erStE = *on;
          msStE = 'WCR0503';
          isErrPrFnEdt = *on;
       Endif;

       If scZipE = *zeros;
          erZipE = *on;
          msZipE = 'WCR0556';
          isErrPrFnEdt = *on;
       Endif;

       //Finance company code must be unique for eff date
       If strmode <> 'C';                       //Add Mode
          If scFcodE <> *blanks and erEfdtE = *off;
             Setll keyWMPFL wmpfl;
             Reade keyWMPFL wmpfl;
             Dow not %eof(wmpfl);
                If dtCYMD(scEfDtE) = PfEfdt;
                   erFcodE= *on;
                   msFcodE= 'WCR1382';
                   isErrPrFnEdt = *on;
                Endif;
                Reade keyWMPFL wmpfl;
             Enddo;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isErrPrFnEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidatePrFnCo  e

      //===================================================================
      // Validate Rate Set Code Details addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateRtStCd  b
     d ValidateRtStCd  pi              n
     d strmode                        1
     d isError2        s               n   inz
      /Free

       isError2 = *off;
      // Initialize all the messageId and error indicators.
       erRtCdAE= *off;
       msRtCdAE = *blanks;

      // Rate Set Code should not be blank
       If scRtCdAE = *blanks;
          msRtCdAE = 'WCR0503';
          erRtCdAE = *on;
          isError2 = *on;
       else;
          //Group Rate Set Code must be unique for Group
          //same key record should not present for add mode
          If strmode = 'A';
             Chain keyWMGRL1 wmgrl;
             If %found(wmgrl);
                erRtcdAE = *on;
                msRtcdAE ='WC30108';
                isError2 = *on;
             Endif;
          Endif;

          If strmode = 'C';
             Setll keyWMGRL1 wmgrl;
             Reade keyWMGRL1 wmgrl;
             Dow not %eof(wmgrl);
                If dbrrn2 <> dbrrn ;
                   erRtcdAE = *on;
                   msRtcdAE = 'WC30108';
                   isError2 = *on;
                Endif;
                Reade keyWMGRL1 wmgrl;
             Enddo;
          Endif;
       Endif;



      // Return *on indicator when No error exists.
       If isError2 = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateRtStCd  e

      // utility procedure definitions
      /COPY SCOPYCOMPI

