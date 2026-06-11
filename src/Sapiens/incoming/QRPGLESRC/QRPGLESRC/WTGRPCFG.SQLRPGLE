     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTGRPCFG- Group Configuration(Profound)
     h dftname(WTGRPCFG)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 10/10/19                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2196- Redesign of Group Configuration program  *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  09/09/25 C:342 Project: CN036                                *
      //                 Multi-Group Processing                        *
      //                 DSPF Changes: Recompile only                  *
      //                 1. Removed Exclude from Comparative Rating    *
      //                    checkbox from RcdGrpCfg                    *
      //                 2. Removed Exclude from Comparative Rating    *
      //                    checkbox from RcdGrpAdd                    *
      //                 3. Removed Javascript changes from RcdGrpAdd  *
      //                    from Onload event property for ID          *
      //                    CbxExcludeComparativeRating                *
      //                                                               *
      //  07/22/25 C:300 Support: 119421                               *
      //                 In "Include Group for Connect" section on     *
      //                 rcdGrpAdd record format - "Default and Exclude*
      //                 from Comparative Rating" checkboxes are       *
      //                 visible, when Connect Portal Block is Hidden  *
      //                 DSPF: Added changes in onload of rcdGrpAdd    *
      //                                                               *
      //  05/08/25 C:333 Project: 153WBD                               *
      //                 Note of Cancel Format-Contact Info Updt.      *
      //                                                               *
      //  12/11/24 C:342 Support: PHW118174                            *
      //                 Change iDARTS URL in Group Config to Use Pgm  *
      //                 Key = W0010R                                  *
      //                                                               *
      //  09/25/24 C:65  Project: SY446                                *
      //                 Recompile for screen file.                    *
      //                                                               *
      //  05/14/24 C:062 Project: 146WKQ                               *
      //                 Add Assessments for S/R.                      *
      //                                                               *
      //  05/08/24 C:053 Support: 115213                               *
      //                 Add ASO flag for EDI (PEO/ASO)                *
      //                                                               *
      //  04/29/24 C:340 Project: WC2965                               *
      //                 Add Email Address Required.                   *
      //                                                               *
      //  01/16/24 C:340 PROJECT: 153WAD                               *
      //                 Add 2 New Field.                              *
      //                 1. Renewal Reminder Notice Flag               *
      //                 2. Renewal Reminder CX Reason Code            *
      //                                                               *
      //  11/17/23 C:300 Support: 114591                               *
      //                 Add 'Include Group in Nightly iDARTS extract' *
      //                 checkbox on Group Auxiliary Information Section
      //                                                               *
      //  09/18/23 C:300 Support: 113869 (SUNZ)                        *
      //                 Getting error 'Job queue GRP614 in QGPL not   *
      //                 found' when saving Group Auxiliary Information*
      //                 with value in 'Group Billing Job Queue Name'  *
      //                 To fix - Include 10/19/21 changes from W0010R07
      //                  (check for job queue object in same library  *
      //                  as the subsystem exists in)                  *
      //                                                               *
      //                 Do not call W0010C02 at data center clients   *
      //                                                               *
      //                 DSPF: Modified error message of scJobq and    *
      //                 scSbs to define variable for Replacement Data *
      //                 And text transform to uppercase in both fields*
      //                                                               *
      //  07/28/23 C:300 Support: 113102 (WC2395)                      *
      //                 Apply Persist state property in sflGrpCfg Grid*
      //                                                               *
      //  05/09/23 C:300 Project:W2196                                 *
      //                 Incldue NCCI Carrier # (from Group Auxiliary) *
      //                 into main Group Configuration grid            *
      //                                                               *
      //  04/26/23 C:300 Project:W2196 Support:110192                  *
      //                 Remove Lockbox Name from Auxiliary Information*
      //                                                               *
      //  03/23/23 C:300 Support:107460                                *
      //                 User Parms/Group Restrictions not Applied     *
      //                 Implemented CheckGrpRst()                     *
      //                                                               *
      //  02/13/23 C:300 Project: WC2196                               *
      //                 Merge WTGRPPRI DSPF into WTGRPCFG DSPF        *
      //                 (as per review comments by Wes Winner)        *
      //                                                               *
      //  01/07/22 C:062 Project: WC2837                               *
      //                 Add Net Rate For SR Policies                  *
      //                                                               *
      //  11/04/21 C:062 Project 146WJV                                *
      //                 Add Exclude from Comparative Rating           *
      //                                                               *
      //  06/18/21 C:300 Support:96286                                 *
      //                 Remove 'Summarize A/R for Deductible Billing' *
      //                 Checkbox from Group Master Information screen *
      //                                                               *
      //  11/18/20 C:180 Project WC2757                                *
      //                 Recompiled only.                              *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTGRPCFG  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflGrpCfg:scSflRRN)
     f                                     Sfile(sflPolYr:rrnPolYr)
     f                                     Sfile(sflSpecAR:rrnSpecAR)
     f                                     Sfile(sflDepPrm:rrnDepPrm)
     f                                     Sfile(sflFlatCx:rrnFlatCx)
     f                                     Sfile(sflCrAuBl:rrnCrAuBl)
     f                                     Sfile(sflDepoSR:rrnDepoSR)
     f                                     Sfile(sflCollAR:rrnCollAR)
     f                                     Sfile(sflDivPln:rrnDivPln)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     fwmfnl     if a e           k disk    rename(pfn01:pfn00)
     f                                     infds(dbfds)
      * Group Master - Web Interface
     fwmfip     uf a e           k disk    rename(pfi01:pfi00)
      * Fund Detail file for Policy Years
     fwdf1p     uf a e             disk    rename(pf101:pf100)
     fwdf1l     if   e           k disk    rename(pf101:pf101)
     f                                     infds(dbfds2)
      * Group Auxiliary Information
     fwdf2p     uf a e           k disk    rename(pf201:pf200)
      *** Beg Add ***** 05/09/23 *****************************
     fwdf2l     if   e           k disk    rename(pf201:pf202)
      *** End Add ***** 05/09/23 *****************************
      * Group Carrier Information
     fwmgcp     uf a e           k disk    rename(pgc01:pgc00)
      * Company Master - Web Interface
     fsmcip     if   e           k disk
      * User Parameter File
     fsmupl     if   e           k disk
      * Logical over all table files
     fwtabl     if   e           k disk
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     D
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     D

     D SbsDA           ds            10    dtaara(JOBQSBS)
     D strSbsD                 1     10


      // work variables
     d i               s              3  0
     d j               s              3  0
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     D indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d isOptaComp      s               n   inz(*off)
     d reload          s               n   inz(*off)
     d charMsg         s            100
     d stky            s             10    inz
     d xxkey           s             10    inz
     d strDartsKey     s             40
     d strUrlRRN       s              9    inz
     d strUrlTabName   s             50    inz
     DdecFnd           s                   like(FNfnd) inz
     DUsrDftFnd        s                   like(FNfnd) inz
      // Define Arrays for A/R Codes, Headings and Selection Criteria
      // Grid 1 -- Specific A/R Transaction Codes
     d strARcod1       s              2    dim(18)
     d strHeads1       s             40    dim(18)
     d strSelCr1       s            100    dim(18)
      // Grid 2 -- Deposit to Premium Transfer
     d strARcod2       s              2    dim(8)
     d strHeads2       s             40    dim(8)
     d strSelCr2       s            100    dim(8)
      // Grid 3 -- A/R Transfer Codes for Flat Cancellation
     d strARcod3       s              2    dim(11)
     d strHeads3       s             40    dim(11)
     d strSelCr3       s            100    dim(11)
      // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
     d strARcod4       s              2    dim(17)
     d strHeads4       s             40    dim(17)
     d strSelCr4       s            100    dim(17)
      // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
     d strARcod5       s              2    dim(8)
     d strHeads5       s             40    dim(8)
     d strSelCr5       s            100    dim(8)
      // Grid 6 -- Collateral A/R Codes
     d strARcod6       s              2    dim(3)
     d strHeads6       s             40    dim(3)
     d strSelCr6       s            100    dim(3)
      // Grid 7 -- Dividend Plan A/R Codes
     d strARcod7       s              2    dim(3)
     d strHeads7       s             40    dim(3)
     d strSelCr7       s            100    dim(3)
     d*
     d* Screen Claim Reserves Codes
     d*
     d                 ds
     d dsScClmRsv              1     20    dim(10)
     d  scRs01                 1      2
     d  scRs02                 3      4
     d  scRs03                 5      6
     d  scRs04                 7      8
     d  scRs05                 9     10
     d  scRs06                11     12
     d  scRs07                13     14
     d  scRs08                15     16
     d  scRs09                17     18
     d  scRs10                19     20
     d*
     d* Claim Reserves Error Indicator
     d*
     d                 ds
     d dsErClmRsv              1     10    dim(10)
     d  erRs01                 1      1
     d  erRs02                 2      2
     d  erRs03                 3      3
     d  erRs04                 4      4
     d  erRs05                 5      5
     d  erRs06                 6      6
     d  erRs07                 7      7
     d  erRs08                 8      8
     d  erRs09                 9      9
     d  erRs10                10     10
     d*
     d* Claim Reserves Message Id
     d*
     d                 ds
     d dsMsClmRsv              1    100    dim(10)
     d  msRs01                 1     10
     d  msRs02                11     20
     d  msRs03                21     30
     d  msRs04                31     40
     d  msRs05                41     50
     d  msRs06                51     60
     d  msRs07                61     70
     d  msRs08                71     80
     d  msRs09                81     90
     d  msRs10                91    100
      //
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
      //
     d @@co#           s              3  0
     d @@fnd           s              3  0
     d svnewfnd        s              3  0 inz
     d svnewrrn        s              5  0 inz
     d svdftrrn        s              5  0 inz
     d @@kc            s              1
     d @@kl            s              1
     d @@rtcd          s              1
     d @@objname       s             10
     d @@objtype       s              7
     d @@err           s              1
     d @@folder        s             80
     d @@docid         s             12
     d @@jobq          s             10
     d @@jobqlib       s             10
     d @@jobqsts       s             10
     d @@sbs           s             10
     d @@nojobs        s             10  0
      ***  Begin Add ***** 09/18/23 *****************************
     d ##obj           s             10
     d ##type          s              7
     d ##lib           s             10
     d @@lib           s             10
     d suffix          s              3    inz

     d outCo           s              3    inz
     d outFnd#         s              3    inz
     d outJobQ         s             10    inz
     d outName         s             10    inz
     d outSbs          s             10    inz
     d outSbs1         s             10    inz
     d outMsg          s            132    inz
     d outRtcd         s              1    inz
      ***  End   Add ***** 09/18/23 *****************************

     d $$co#           s              3  0
     d $$usr           s             10
     d $$appl          s              1
     d $$rtcd          s              1
     d $$auth          s              1
     d $$fnd           s              3  0
     d MultiGrpRst     s              1n   inz(*off)

      // Proto-types

      // Check Object
     d CheckObj        pr                  extpgm('SRCHKOBJ')
     d @@objname                     10
     d @@objtype                      7
     d @@err                          1

      // Check Object Name is Valid
     d CheckObjName    pr                  extpgm('SRCKOBJN')
     d @@objname                     10
     d @@err                          1

      // Check Group Restriction
     d CheckGrpRst     pr                  extpgm('SA008R')
     d $$co#                          3  0
     d $$usr                         10
     d $$appl                         1    Const
     d $$rtcd                         1
     d $$auth                         1    options(*nopass)
     d $$fnd                          3  0 options(*nopass)

      // Copy Group
     d CopyGroup       pr                  extpgm('W0010R03')
     d @@fnd                          3  0
     d @@kc                           1
     d @@kl                           1

      ***  Begin Add ***** 09/18/23 *****************************
      // Check Object with Library
     d CheckObj2       pr                  extpgm('SRCHKOB2')
     d @@lib                         10
     d @@objname                     10
     d @@objtype                      7
     d @@err                          1

      // Create JobQ
     dcrtJobQ          pr                  extpgm('W0010C02')
     d outCo                               like(outCo)
     d outFnd#                             like(outFnd#)
     d outJobQ                             like(outJobQ)
     d outName                             like(outName)
     d outSbs                              like(outSbs)
     d outSbs1                             like(outSbs1)
     d outMsg                              like(outMsg)
     d outRtcd                             like(outRtcd)
      ***  End   Add ***** 09/18/23 *****************************

      // WCMC Add Group Pompt
     d F0062R          pr                  extpgm('F0062R')
     d @@kc                           1
     d @@kl                           1
     d @@fnd                          3  0

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      // Determine if EDI Format ID exists in any group
     d GetEDIF         pr                  extpgm('WA075R')
     d @@co#                          3  0
     d @@fnd                          3  0
     d @@rtcd                         1

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      ***  Begin Add ***** 09/18/23 *****************************
      // Retrieve Client Suffix
     d RtvCltSuffix    pr                  ExtPgm('SA030R')
     d suffix                         3

      // Retrieve Object Library
     d RtvObjLib       pr                  extpgm('RTVOBJLIB')
     d ##obj                         10
     d ##type                         7
     d ##lib                         10
      ***  End   Add ***** 09/18/23 *****************************

      // Retrieve JobQ attributes
     d RtvJobQA        pr                  extpgm('RTVJOBQA')
     d @@jobq                        10
     d @@jobqlib                     10
     d @@jobqsts                     10
     d @@sbs                         10
     d @@nojobs                      10  0

      // Validate the Existance of a Document
     d ValidDoc        pr                  extpgm('SVALDOC')
     d @@docid                       12
     d @@folder                      80
     d @@rtcd                         1

      // Validate the Existance of a Folder
     d ValidFlr        pr                  extpgm('SVALFLR')
     d @@folder                      80
     d @@rtcd                         1

      * Initialize the Screen fields for Group Configuration (1st)
     d InzGrpCfgScVal  pr

      * Initialize the Screen fields for Policy Years/ Group Details
     d InzGrpDtlScVal  pr

      * Initialize the Screen fields for Add Group
     d InzGrpInfScVal  pr

      // Load Group Grid info
     d LoadGroupGrid   pr

      // Load Policy Years Grid info
     d LoadYearsGrid   pr

      * Main Processing
     d Main            pr

      * Process Group Addition
     d ProcessAdd      pr

      * Process Policy Years/Group Details Addition
     d ProcessAddYear  pr

      * Process other Options
     d ProcOther       pr

      * Process other Options for Policy Years Grid
     d ProcOther2      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate Group Addition info. procedure.
     d ValidateGroup   pr              n

     * Validate Group Details Addition info. procedure.
     d ValidateGrpDtl  pr              n
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
      *** Beg Add ***  04/29/24  *** WC2965 *************************
     d/copy sPrGenErr
      *** End Add ***  04/29/24  *** WC2965 *************************

      //
      //   Key List for the Documents Table File
      //

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index

     c     keyWDF1L      klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    scFyr

      /free
       Exec Sql Set Option Commit = *None;

       GetClientID(clientID);

       ProgramStart(process);

       If process;

          Main();

       Endif;


       *InLr = *On;
      /end-free

       //===================================================================
       // InzGrpCfgScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzGrpCfgScVal  b
     d InzGrpCfgScVal  pi
     d
      /Free
       scCo# = intCo#;
       scCltid = clientID;

       // Determine if client is Admin only or Claims only
       visbAdmin = *off;
       visbClaim = *off;
       Chain 'WC' SWC01;
       If %found();
          If WCfl08 = 'A';                          // Admin Only
             visbAdmin = *on;
          Endif;
          If WCfl08 = 'C';                          // Claims only
             visbClaim = *on;
          Endif;
       Endif;

      // Set visibilty for Max extended year policy (in months)
      // Set visibilty for Commercial Group
        visbMxe  = *off;
        visbCgfl = *off;
        if WCfl07 = 'Y';                               //WCfl07=Y Commercial Application Installed
           visbMxe  = *on;
           visbCgfl = *on;
        Endif;

      // Set visibilty for Include Group for CICenter
        visbCicf = *off;
        if WCfl19 = 'Y';
           visbCicf = *on;
        Endif;

      // Set visibilty for Group Association
        visbGrpAsc = *off;
        If clientID = '114' or clientID = '147' Or clientID = '152';
           visbGrpAsc = *on;
        Endif;

      // Set visibilty for Excess Insurance Group
        visbExIns = *off;
        //*** Begin Add *** 05/14/24 ***************************************
        vis146    = *Off;
        //*** End   Add *** 05/14/24 ***************************************
        If clientID = '146';
           visbExIns = *on;
           //*** Begin Add *** 05/14/24 ************************************
           vis146    = *On;
           //*** End   Add *** 05/14/24 ************************************
        Endif;

      // Set visibilty for PEO Group - Policy Type
        visbPeof = *off;
        If WCfl30 = 'Y' or clientID = '114';
           visbPeof = *on;
        Endif;

      // Set visibilty for checkbox - Charge Expense Constant to Emp #1 for PEO Group
      // and Calculate Stock Discount Across Entire PEO Group
        visbPeoe = *off;
        visbPeoa = *off;
        If WCfl30 = 'Y';
           visbPeoe = *on;
           visbPeoa = *on;
        Endif;

      // Set visibilty and Heading for Portal Group fields
        isPortal = *off;
        isPortal = IsModEnabled('Portal Module');
        If isPortal = *on;
           scPortFlag = 'Include Group for ' +
            GetModuleProperty('Portal Module':'Name');
        // Get company default values for Portal Group fields
           tpCmpDft = *blanks;
           tpCmpDftC = *blanks;
           vsCmpOvr = *off;
           isCmpDft = *off;
           tpCmpDftC = 'Clear both fields to reset to company information.';
           Chain @@Co# pci01;
           If %found(smcip);
              tpCmpDft = 'Data shown is defaulted from company information.';
              tpCmpDft = %trim(tpCmpDft) + ' ' +
               'Click ''Override Company Defaults'' to set values for ' +
               'this group.';
              vsCmpOvr = *on;
              isCmpDft = *on;
              scAsOfP = DtMDYY(CIasOf);
              scDaysP = CIdays;
              udAsOfP = DtMDYY(CIasOf);
              udDaysP = CIdays;
           Endif;
        Endif;

      // Set visibilty for Due Date for Weekly Interim Payments
        visbWidy = *off;
        If clientID = '148';
           visbWidy = *on;
        Endif;

      // Set visibilty for Claims Elig Flag
        visbClElig = *off;
        if isModEnabled('Claims Export');
           visbClElig = *on;
        Endif;

      // Initialize Checkbox - Premium Verbiage
        scPrmv = 'P';


        @@fnd = scGrp;




       // Need to remove following later //
        // visbMxe  = *on;
        // visbCgfl = *on;
        // visbCicf = *on;
        // visbGrpAsc = *on;
        // visbExIns = *on;
        // visbWidy = *on;
        // visbClElig = *on;




      /End-Free
     p InzGrpCfgScVal  e

       //===================================================================
       // InzGrpDtlScVal - Initialize the Screen fields and indicators for Group Details
       //===================================================================
     p InzGrpDtlScVal  b
     d InzGrpDtlScVal  pi
     d
      /Free
       scCo# = intCo#;
       scCltid = clientID;

       // Determine if client is Admin only or Claims only
       visbAdmin = *off;
       visbClaim = *off;
       Chain 'WC' SWC01;
       If %found();
          If WCfl08 = 'A';                          // Admin Only
             visbAdmin = *on;
          Endif;
          If WCfl08 = 'C';                          // Claims only
             visbClaim = *on;
          Endif;
       Endif;

      // Set selection criteria for Self-reporting Bill
        strSelCr1(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for New Business Payment
        strSelCr1(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Payment Transfer
        strSelCr1(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for S/R Discount Code
        strSelCr1(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Membership Dues Transfer
        strSelCr1(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''M''';

      // Set selection criteria for Safety Fees Transfer
        strSelCr1(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''S''';

      // Set selection criteria for Premium Transfer
        strSelCr1(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Deposit Transfer
        strSelCr1(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Expense Constant Transfer
        strSelCr1(9) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''E''';

      // Set selection criteria for First Assessment Transfer
        strSelCr1(10) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';

      // Set selection criteria for Second Assessment Transfer
        strSelCr1(11) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';

      // Set selection criteria for Third Assessment Transfer
        strSelCr1(12) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';

      // Set selection criteria for Fourth Assessment Transfer
        strSelCr1(13) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';

      // Set selection criteria for Fifth Assessment Transfer
        strSelCr1(14) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';

      // Set selection criteria for Sixth Assessment Transfer
        strSelCr1(15) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';

      // Set selection criteria for Seventh Assessment Transfer
        strSelCr1(16) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // Set selection criteria for State Tax Transfer
        strSelCr1(17) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for State Tax on Deposit Transfer
        strSelCr1(18) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit Billing Offset
        strSelCr2(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''H''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Billing Offset
        strSelCr2(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit Payment Offset
        strSelCr2(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Payment Offset
        strSelCr2(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit to Premium Transfer
        strSelCr2(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Tax on Deposit to Premium Transfer
        strSelCr2(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Deductible Tax Bill
        strSelCr2(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''R''';

      // Set selection criteria for Deductible Tax Paid
        strSelCr2(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''R''';

      // Set selection criteria for Membership Fees
        strSelCr3(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''M''';

      // Set selection criteria for Safety Fees
        strSelCr3(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''S''';

      // Set selection criteria for State Tax
        strSelCr3(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Premium Payment
        strSelCr3(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for 1st Assessment
        strSelCr3(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';

      // Set selection criteria for 2nd Assessment
        strSelCr3(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';

      // Set selection criteria for 3rd Assessment
        strSelCr3(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';

      // Set selection criteria for 4th Assessment
        strSelCr3(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';

      // Set selection criteria for 5th Assessment
        strSelCr3(9) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';

      // Set selection criteria for 6th Assessment
        strSelCr3(10) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';

      // Set selection criteria for 7th Assessment
        strSelCr3(11) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // Set selection criteria for Deductible
        strSelCr4(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''R''';

      // Set selection criteria for Expense Constant
        strSelCr4(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''E''';

      // Set selection criteria for General Fees
        strSelCr4(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

      // Set selection criteria for Late Fees
        strSelCr4(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''L''';

      // Set selection criteria for Membership Fees
        strSelCr4(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''M''';

      // Set selection criteria for Deposit
        strSelCr4(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Safety Fees
        strSelCr4(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''S''';

      // Set selection criteria for Tax
        strSelCr4(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Tax on Deposit
        strSelCr4(9) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for 1st Assessment
        strSelCr4(10) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';

      // Set selection criteria for 2nd Assessment
        strSelCr4(11) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';

      // Set selection criteria for 3rd Assessment
        strSelCr4(12) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';

      // Set selection criteria for 4th Assessment
        strSelCr4(13) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';

      // Set selection criteria for 5th Assessment
        strSelCr4(14) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';

      // Set selection criteria for 6th Assessment
        strSelCr4(15) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';

      // Set selection criteria for 7th Assessment
        strSelCr4(16) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // Set selection criteria for Premium Transfer
        strSelCr4(17) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Deposit Billing Offset SR
        strSelCr5(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''H''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Billing Offset SR
        strSelCr5(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit Paid Offset SR
        strSelCr5(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Paid Offset SR
        strSelCr5(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit to Premium Transfer SR
        strSelCr5(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Tax on Deposit to Premium Transfer SR
        strSelCr5(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Deposit Premium Offset SR
        strSelCr5(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Tax on Deposit Premium Offset SR
        strSelCr5(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Collateral Billed
        strSelCr6(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''H''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Collateral A/R Paid
        strSelCr6(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Collateral A/R Transferred
        strSelCr6(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Dividend Billed
        strSelCr7(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Dividend Paid
        strSelCr7(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Dividend Payment Transfer
        strSelCr7(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';



      // Set Headings for A/R Codes
        strHeads1(1) = 'Self-reporting Bill';
        strHeads1(2) = 'New Business Payment';
        strHeads1(3) = 'Payment Transfer';
        strHeads1(4) = 'S/R Discount Code';
        strHeads1(5) = 'Membership Dues Transfer';
        strHeads1(6) = 'Safety Fees Transfer';
        strHeads1(7) = 'Premium Transfer';
        strHeads1(8) = 'Deposit Transfer';
        strHeads1(9) = 'Expense Constant Transfer';
        strHeads1(10) = 'First Assessment Transfer';
        strHeads1(11) = 'Second Assessment Transfer';
        strHeads1(12) = 'Third Assessment Transfer';
        strHeads1(13) = 'Fourth Assessment Transfer';
        strHeads1(14) = 'Fifth Assessment Transfer';
        strHeads1(15) = 'Sixth Assessment Transfer';
        strHeads1(16) = 'Seventh Assessment Transfer';
        strHeads1(17) = 'State Tax Transfer';
        strHeads1(18) = 'State Tax on Deposit Transfer';

        strHeads2(1) = 'Deposit Billing Offset';
        strHeads2(2) = 'Tax on Deposit Billing Offset';
        strHeads2(3) = 'Deposit Payment Offset';
        strHeads2(4) = 'Tax on Deposit Payment Offset';
        strHeads2(5) = 'Deposit to Premium Transfer';
        strHeads2(6) = 'Tax on Deposit to Premium Transfer';
        strHeads2(7) = 'Deductible Tax Bill';
        strHeads2(8) = 'Deductible Tax Paid';

        strHeads3(1) = 'Membership Fees';
        strHeads3(2) = 'Safety Fees';
        strHeads3(3) = 'State Tax';
        strHeads3(4) = 'Premium Payment';
        strHeads3(5) = '1st Assessment';
        strHeads3(6) = '2nd Assessment';
        strHeads3(7) = '3rd Assessment';
        strHeads3(8) = '4th Assessment';
        strHeads3(9) = '5th Assessment';
        strHeads3(10) = '6th Assessment';
        strHeads3(11) = '7th Assessment';

        strHeads4(1) = 'Deductible';
        strHeads4(2) = 'Expense Constant';
        strHeads4(3) = 'General Fees';
        strHeads4(4) = 'Late Fees';
        strHeads4(5) = 'Membership Fees';
        strHeads4(6) = 'Deposit';
        strHeads4(7) = 'Safety Fees';
        strHeads4(8) = 'Tax';
        strHeads4(9) = 'Tax on Deposit';
        strHeads4(10) = '1st Assessment';
        strHeads4(11) = '2nd Assessment';
        strHeads4(12) = '3rd Assessment';
        strHeads4(13) = '4th Assessment';
        strHeads4(14) = '5th Assessment';
        strHeads4(15) = '6th Assessment';
        strHeads4(16) = '7th Assessment';
        strHeads4(17) = 'Premium Transfer';

        strHeads5(1) = 'Deposit Billed Offset';
        strHeads5(2) = 'Tax on Deposit Billed Offset';
        strHeads5(3) = 'Deposit Paid Offset';
        strHeads5(4) = 'Tax on Deposit Paid Offset';
        strHeads5(5) = 'Deposit to Premium Transfer';
        strHeads5(6) = 'Tax on Deposit to Premium Transfer';
        strHeads5(7) = 'Deposit Premium Offset';
        strHeads5(8) = 'Tax on Deposit Premium Offset';

        strHeads6(1) = 'Billed';
        strHeads6(2) = 'Paid';
        strHeads6(3) = 'Transferred';

        strHeads7(1) = 'Dividend Billed';
        strHeads7(2) = 'Dividend Paid';
        strHeads7(3) = 'Dividend Payment Transfer';



      // Set visibilty for Percentage of Manual Premium Target
        visbManp = *off;
        If clientID = '150';
           visbManp = *on;
        Endif;

      // Set visibilty for Bill Expense Constant on all Divisions
        visbExdv = *off;
        If clientID = '088' or clientID = '090';
           visbExdv = *on;
        Endif;

      // Set visibilty for Collateral A/R Codes fields
        visbCollAR = *off;
        If clientID = '150';
           visbCollAR = *on;
        Endif;

      // Set visibilty for Do not process 10/1/07 NY rate decrease
        visbNy07 = *off;
        If clientID = '129';
           visbNy07 = *on;
        Endif;

      // Set visibilty for Dividend Plan (3) fields
        visbCSIOPT  = *off;
        isOptaComp = *Off;
        isOptaComp = GetGrpAssFlg(FNco#:FNfnd);
        If isOptaComp or clientID = '114';
           visbCSIOPT = *On;
        Endif;

      // Set visibilty for Do not process 10/1/07 NY rate decrease
        visbScdt = *off;
        if WCfl44 = 'Y';
           visbScdt = *on;
        Endif;





       // Need to remove following later //
        // visbManp = *on;
        // visbExdv = *on;
        // visbCollAR = *on;
        // visbNy07 = *on;
        // visbCSIOPT = *On;
        // visbScdt = *on;





      /End-Free
     p InzGrpDtlScVal  e

       //===================================================================
       // InzGrpInfScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzGrpInfScVal  b
     d InzGrpInfScVal  pi
     d
      /Free
       scCo# = intCo#;
       scCltid = clientID;

       // Determine if client is Admin only or Claims only
       visbAdmin = *off;
       visbClaim = *off;
       Chain 'WC' SWC01;
       If %found();
          If WCfl08 = 'A';                          // Admin Only
             visbAdmin = *on;
          Endif;
          If WCfl08 = 'C';                          // Claims only
             visbClaim = *on;
          Endif;
       Endif;

      // Set selection criteria for Default Vendor Number
        scVndrCH = 'VPco# = ' + %EditC(scCo#:'X');
      //scVndrCH = 'VPco# = ' + %EditC(scCo#:'X') + ' and ' +
      // '(VPeXdt = 0 Or VPeXdt >= ' + %Editc(saveDt:'X') + ')';

      // Set selection criteria for Group Association
        scGrpAscCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'GTstat<>' + '''I''' + 'and GTtbid=' + '''GRPA''';

      // Set selection criteria for TPA Contact Code
        scTpacCH = 'TCco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TCstat<>' + '''I''';

      // Set selection criteria for Certification of Insurance Overlay
        scCeovCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'GTstat<>' + '''I''' + 'and GTtbid=' + '''COVL''';

      // Set selection criteria for A/R Deposit Payment Write-off Code
        scDbcdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for A/R Premium Payment Write-off Code
        scPbcdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for A/R Fee Payment Write-off Code
        scFbcdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

      // Set selection criteria for A/R Assessment Write-off Code (1 to 7)
        sc1acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';
        sc2acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';
        sc3acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';
        sc4acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';
        sc5acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';
        sc6acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';
        sc7acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // call API to Check the fund file to see if EDI Format ID is required of not
        visbEdif = *off;
        @@co# = scCo#;
        @@fnd = 0;
        @@rtcd = ' ';
        GetEDIF(@@co#:@@fnd:@@rtcd);
        If @@rtcd = 'F';
           visbEdif = *on;
        Endif;

      // Set visibilty for Add Group Window Panel and Save/Cancel Buttons
       visGrpCfg = *On;

      // Set visibilty for Max extended year policy (in months)
      // Set visibilty for Commercial Group
        visbMxe  = *off;
        visbCgfl = *off;
        if WCfl07 = 'Y';                               //WCfl07=Y Commercial Application Installed
           visbMxe  = *on;
           visbCgfl = *on;
        Endif;

      // Set visibilty for Include Group for CICenter
        visbCicf = *off;
        if WCfl19 = 'Y';
           visbCicf = *on;
        Endif;

      // Set visibilty for Group Association
        visbGrpAsc = *off;
        If clientID = '114' or clientID = '147' Or clientID = '152';
           visbGrpAsc = *on;
        Endif;

      // Set visibilty for Excess Insurance Group
        visbExIns = *off;
        If clientID = '146';
           visbExIns = *on;
        Endif;

      // Set visibilty for PEO Group - Policy Type
        visbPeof = *off;
        If WCfl30 = 'Y' or clientID = '114';
           visbPeof = *on;
        Endif;

      // Set visibilty for checkbox - Charge Expense Constant to Emp #1 for PEO Group
      // and Calculate Stock Discount Across Entire PEO Group
        visbPeoe = *off;
        visbPeoa = *off;
        If WCfl30 = 'Y';
           visbPeoe = *on;
           visbPeoa = *on;
        Endif;

      // Set visibilty and Heading for Portal Group fields
        isPortal = *off;
        isPortal = IsModEnabled('Portal Module');
        If isPortal = *on;
           scPortFlag = 'Include Group for ' +
            GetModuleProperty('Portal Module':'Name');
        // Get company default values for Portal Group fields
           tpCmpDft = *blanks;
           tpCmpDftC = *blanks;
           vsCmpOvr = *off;
           isCmpDft = *off;
           tpCmpDftC = 'Clear both fields to reset to company information.';
           Chain @@Co# pci01;
           If %found(smcip);
              tpCmpDft = 'Data shown is defaulted from company information.';
              tpCmpDft = %trim(tpCmpDft) + ' ' +
               'Click ''Override Company Defaults'' to set values for ' +
               'this group.';
              vsCmpOvr = *on;
              isCmpDft = *on;
              scAsOfP = DtMDYY(CIasOf);
              scDaysP = CIdays;
              udAsOfP = DtMDYY(CIasOf);
              udDaysP = CIdays;
           Endif;
        Endif;

      // Set visibilty for Due Date for Weekly Interim Payments
        visbWidy = *off;
        If clientID = '148';
           visbWidy = *on;
        Endif;

      // Set visibilty for Claims Elig Flag
        visbClElig = *off;
        if isModEnabled('Claims Export');
           visbClElig = *on;
        Endif;

      // Set visibilty for WCPOLS electronic Reporting
        visbWcpl = *off;
        if WCfl03 = 'Y' and WCfl08 <> 'C';             //WCfl03=Y WCPOLS, WCfl08=C Claims only
           visbWcpl = *on;
        Endif;

      // Set visibilty for Free Form Text for Check Processing
        visbFref = *off;
        If clientID = '114';
           visbFref = *on;
        Endif;

      // Set visibilty for Disable SMS Texting
        visbDissms = *off;
        if isModEnabled('RelaySMS');
           visbDissms = *on;
        Endif;

      // Set visibilty for Output Logo File Name and Path
        visbLogo = *off;
        if isModEnabled('Scriptura-Documents')
         or isModEnabled('Scriptura-Various')
         or isModEnabled('3rd Party XML');
           visbLogo = *on;
        Endif;

      // Set visibilty for JobQ and Subsystem
        visbJobSbs = *off;
        if WCfl51 = 'Y';                               //JobQ/Subsystem
           visbJobSbs = *on;
        Endif;

      // Set visibilty for NCCI Carrier Group Code
        visbNCarGr = *off;
        If isModEnabled('IDC');
           visbNCarGr = *on;
        Endif;

      // Initialize Checkbox - Schedule of Operations Division & Print Order (A/D)
        scDepo = 'A';

      // Initialize Checkbox - Premium Verbiage
        scPrmv = 'P';

      // Initialize Default Subsystem for Billing
        // Read Date from JOBQSBS Data Area
        In SbsDA;
        scSbsD = strSbsD;


        @@fnd = scGrp;




       // Need to remove following later //
        // visbGrpAsc = *on;
        // visbExIns = *on;
        // visbWidy = *on;
        // visbFref = *on;
        // visbDissms = *on;
        // visbJobSbs = *on;




      /End-Free
     p InzGrpInfScVal  e

       //===================================================================
       // Load Group Grid info
       //===================================================================
     p LoadGroupGrid   b
     d LoadGroupGrid   pi

      /free
         // Clear the subfile
         scSflRRN = 0;
         scSflDsp = *off;
         scSflClr = *on;
         Write rcdGrpCfg;
         scSflClr = *off;
         scSflDsp = *on;
         scRowSel = 1;

         // Populate the subfile
         Setll *loval wmfnl;
         Read wmfnl;
         Dow not %eof(wmfnl);
            // Check for Group Restriction
            If UPxfnd = 'Y';
               If MultiGrpRst = *off and UPfnd <> FNfnd;
                  Read wmfnl;
                  Iter;
               Endif;

               If MultiGrpRst = *on;
                  // Check for list of Restricted Groups
                  $$co#  = intCo#;
                  $$usr  = q1user;
                  $$appl = 'W';
                  $$rtcd = *off;
                  $$auth = 'A';
                  $$fnd  = FNfnd;
                  CheckGrpRst($$co#:$$usr:$$appl:$$rtcd:$$auth:$$fnd);

                  If $$rtcd = *on;
                     Read wmfnl;
                     Iter;
                  Endif;
               Endif;
            Endif;


            scSflRRN += 1;
            scSflSel = *off;
          // User Default Group at the top of the Grid
            If UsrDftFnd = 0;
               If scSflRRN = 1;
                  scSflSel = *on;
               Endif;
            Else;
               If UsrDftFnd = FNfnd;
                  scSflSel = *on;
                  svdftrrn = scSflRRN;
               Endif;
            Endif;
          // Select newly added Group at the top of the Grid
            If svnewfnd <> 0 and svnewfnd = FNfnd;
               svnewfnd = 0;
               svnewrrn = scSflRRN;
               scSflSel = *on;
            Endif;
            scGrp  = Fnfnd;
            scGrpNm = FNname;
            scShNm = FNshnm;
            scEfdt = CvtToDate(FNefdt);
            scExdt = CvtToDate(FNexdt);
            scRRN  = dbrrn;

           //*** Beg Add ***** 05/09/23 *****************************
          // Get NCCI Carrier # from Group Auxiliary File
            Chain(n) (FNCo#:FNFnd) pf202;
            If %found(wdf2l);
               scNcar# = F2car#;
            Endif;
            //*** End Add ***** 05/09/23 *****************************


            // Populate Edit Group Link
            Clear scEdtURL;
            strUrlRRN = %Editc(scRRN:'X');
            strUrlTabName = FNname;
            scEdtURL = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'C':
             'WTGRPPRI');


            // Populate Inquiry Group Link
            Clear scInqryURL;
            strUrlRRN = %Editc(scRRN:'X');
            strUrlTabName = FNname;
            scInqryURL = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'D':
             'WTGRPPRI');


            // Populate iDARTS link
            Clear scDartURL;
            strUrlTabName = 'iDARTS';
            strDartsKey = %editc(FNCo#:'X') + %editc(FNFnd:'X');
            scDartURL  = strUrlTabName +
             CreateGenUrl('/iDartsWeb/profound.do':
             'user=' + %Trim(q1User):
             //*** Beg Chg *** 12/11/24 *******************************************
             //*** '&keyProgram=WTGRPCFG':
             '&keyProgram=W0010R':
             //*** End Chg *** 12/11/24 *******************************************
             '&key=' + %trim(strDartsKey):
             '&action=listing':
             '&mode=remarks');

            Write sflGrpCfg;
            Read wmfnl;
         Enddo;

         scSflRRN = 1;
       // User Default Group at the top of the Grid
         If svdftrrn <> 0;
            scSflRRN = svdftrrn;
            scRowSel = svdftrrn;
            svdftrrn = 0;
         Endif;
       // Select newly added Group at the top of the Grid
         If svnewrrn <> 0;
            scSflRRN = svnewrrn;
            scRowSel = svnewrrn;
            svnewrrn = 0;
         Endif;

      /end-free
     p LoadGroupGrid   e

       //===================================================================
       // Load Policy Years Grid info
       //===================================================================
     p LoadYearsGrid   b
     d LoadYearsGrid   pi

      /free
         // Clear the subfile
         rrnPolYr = 0;
         dspPolYr = *off;
         clrPolYr = *on;
         Write rcdGrpAdd;
         clrPolYr = *off;
         dspPolYr = *on;

         // Populate the subfile in Descending order
         Setgt (scCo#:scFnd) wdf1l;
         Readpe (scCo#:scFnd) wdf1l;
         Dow not %eof(wdf1l);
            rrnPolYr += 1;
            scpolyrSel = *off;
            If rrnPolYr = 1;
               scPolYrSel = *on;
            Endif;
            scPolYr = F1fyr;
            scPolEfdt = CvtToDate(F1stpo);
            scPolExdt = CvtToDate(F1enpo);
            scPolRRN  = dbrrn2;

            Write sflPolYr;
         Readpe (scCo#:scFnd) wdf1l;
         Enddo;

         rrnPolYr = 1;

      /end-free
     p LoadYearsGrid   e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       LoadActionBar();

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load Group Grid Info
          LoadGroupGrid();
          // display screen
          Exsr ExfmtScreens;

          error = *off;

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh =*Off;

          // Add Group
          When BtnUsrAdd = *On;
             BtnUsrAdd =*Off;
             ProcessAdd();

          Other;
             ProcOther();

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When indexTab = 1;
          //*** Beg Chg ***** 05/08/25 **********************
          //***pnlHeight = 1000;
          pnlHeight = 1060;
          //*** End Chg ***** 05/08/25 **********************
          APnlHeight = pnlHeight + 60;

          // Loading initial values
          InzGrpCfgScVal();

          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdGrpCfg;
          Read rcdSysTbl;
          Read rcdPanel;

       EndSl;

       Endsr;


      /end-free
     p Main            e

       //===================================================================
       // processAdd - process addition of Group
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d*
     d  isexit         s               n   Inz(*off)

      /free

       Clear RcdGrpAdd;
       isexit = *off;
       BtnGrpCan = *off;

       // Loading initial values
       InzGrpInfScVal();

       Dow BtnGrpcan = *off and isexit = *off;

          // Load Policy Years Grid Info
          LoadYearsGrid();

          Exfmt RcdGrpAdd;

          Select;
          // Savd Button
          When BtnSaveGrp = *on;
             BtnSaveGrp = *off;
             If ValidateGroup();
                Exsr WrtGroup;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnGrpCan = *on;
               BtnGrpCan = *off;
               isexit = *on;

         // Delete all the Policy Years If group not saved
          Exec SQL
           Delete from WDF1P
            where F1iusr = :q1user and F1itm = :q1jobn;



          // Add Policy Years Button
          When BtnYrAdd = *On;
             BtnYrAdd =*Off;
             ProcessAddYear();

          Other;
             ProcOther2();

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        FNco#   = scCo#;                 // Company Number
        FNfnd   = scFnd;                 // Group Number
        FNname  = scName;                // Name
        FNshnm  = scShnm;                // Short Name
        FNmad1  = scMad1;                // Mailing Addres 1
        FNmad2  = scMad2;                // Mailing Addres 2
        FNmcty  = scMcty;                // Mailing City
        FNmst   = scMst;                 // Mailing State
        FNmzip  = scMzip;                // Mailing Zipcode
        FNpad1  = scPad1;                // Physical Addres 1
        FNpad2  = scPad2;                // Physical Addres 2
        FNpcty  = scPcty;                // Physical City
        FNpst   = scPst;                 // Physical State
        FNpzip  = scPzip;                // Physical Zipcode
        FNanam  = scAnam;                // Alternate Company Name
        FNaad1  = scAad1;                // Alternate Addres 1
        FNaad2  = scAad2;                // Alternate Addres 2
        FNacty  = scActy;                // Alternate City
        FNast   = scAst;                 // Alternate State
        FNazip  = scAzip;                // Alternate Zipcode

        If scPhone1 > *zeros;
           FNph1a  = %dec(%subst(%editc(scPhone1:'X'):1:3):3:0); // Phone 1 Area Code
           FNph1   = %dec(%subst(%editc(scPhone1:'X'):4):7:0);   // Phone 1 Number
        EndIf;
        If scPhone1 = *zeros;
           FNph1a  = *zeros;
           FNph1   = *zeros;
        EndIf;
        If scPhone2 > *zeros;
           FNarea  = %dec(%subst(%editc(scPhone2:'X'):1:3):3:0); // Phone 2 Area Code
           FNph2   = %dec(%subst(%editc(scPhone2:'X'):4):7:0);   // Phone 2 Number
        EndIf;
        If scPhone2 = *zeros;
           FNarea  = *zeros;
           FNph2   = *zeros;
        EndIf;
        If scFax > *zeros;
           FNfaxa  = %dec(%subst(%editc(scFax:'X'):1:3):3:0); // Fax Area Code
           FNfax   = %dec(%subst(%editc(scFax:'X'):4):7:0);   // Fax Number
        EndIf;
        If scFax = *zeros;
           FNfaxa  = *zeros;
           FNfax   = *zeros;
        EndIf;

        FNiadr  = scIadr;                // Internet Address
        FNefdt  = dtCYMD(scEfDta);       // Effective date
        FNexdt  = dtCYMD(scExDta);       // Effective date
        FNmxex  = scMxex;                // Max extended year policy (in months)
        FNnacs  = scNacs;                // Next Available Case Number
        FNnaen  = scNaen;                // Next Available Employer Number
        FNnaqn  = scNaqn;                // Next Available Quote Number
        FNaute  = scAute;                // Auto Assign Employer Number
        FNautd  = scAutd;                // Auto Assign Division Number
        FNdiv0  = scDiv0;                // Allow Division Zero

        FNmetr  = scMetr;                // Net Deductible Medical Transaction Code
        FNmep1  = scMep1;                // Net Deductible Medical PV1
        FNextr  = scExtr;                // Net Deductible Expense Transaction Code
        FNexp1  = scExp1;                // Net Deductible Expense PV1
        FNcotr  = scCotr;                // Net Deductible Compensation Transaction Code
        FNcop1  = scCop1;                // Net Deductible Compensation PV1
        FNletr  = scLetr;                // Net Deductible Legal Transaction Code
        FNlep1  = scLep1;                // Net Deductible Legal PV1
        FNdfrc  = scDfrc;                // Medical Default Reserve Code
        FNdfra  = scDfra;                // Medical Default Amount
        FNdfxc  = scDfxc;                // Expense Default Reserve Code
        FNdfxa  = scDfxa;                // Expense Default Amount

        FNportdft  = scPortDft;          // Default Group for Connect
        FNsfin  = scSfin;                // Employers Self-insured
        FNcicf  = scCicf;                // Include Group for CICenter
        FNedi   = scEdi;                 // Exclude Group from EDI Processing
        FNclelig   = scClElig;           // Exclude Group from Base Claims Eligibility Export
        FNcgfl  = scCgfl;                // Commercial Group
        If FNcgfl = ' ';
           FNcgfl = 'N';
        Endif;
        //***** Beg Del ***** 06/18/21 ****************************
        //*** FNSumDed= scSumDed;              // Summarize A/R for Deductible Billing
        //***** End Del ***** 06/18/21 ****************************
        FNtest  = scTest;                // Test group
        FNexins = scExIns;               // Excess Insurance Group
        FNpeoe  = scPeoe;                // Charge Expense Constant to Emp #1 for PEO Group
        FNpeoa  = scPeoa;                // Calculate Stock Discount Across Entire PEO Group
        FNpeof  = scPeof;                // PEO Group - Policy Type
        FNaer   = scAer;                 // Auto Endorse Re-attach with Policy Page issue
        FNpeos  = scPeos;                // Mid-term Endorsements as Re-submissions
        FNpde   = scPde;                 // Print Division and Entity/Location Numbers on Schedule
                                         // of Operations
        //*** Beg Add ***  05/08/24  *** WC2965 *************************
        FNasof  = scAsof;                // PEO/ASO Group
        //*** End Add ***  05/08/24  *** WC2965 *************************
        //*** Beg Add ***  04/29/24  *** WC2965 *************************
        FNEmail = scEmail;
        //*** End Add ***  04/29/24  *** WC2965 *************************
        FNdftvend# = scDftVend#;         // Default Vendor Number
        FNsior  = scSior;                // Self-insured Organization Type Code
        FNsiau  = scSiau;                // Self-insured Authorization Type Code
        FNedif  = scEdif;                // EDI Format Identifier
        FNgrpasc = scGrpAsc;             // Group Association
        FNprmv  = scPrmv;                // Premium Verbiage
        If FNprmv = ' ';
           FNPRMV = 'P';
        Endif;
        FNdepo  = scDepo;                // Schedule of Operations Division & Entity/Location Print
        If FNdepo = ' ';                 // Order (A/D)
           FNdepo = 'A';
        Endif;
        FNchk1  = scChk1;                // Check# Range Allowable for Manual Claim Checks
        FNchk2  = scChk2;                // Check# Range Allowable for Manual Claim Checks
        FNmidy  = scMidy;                // Due Date for Monthly Interim Payments
        FNwidy  = scWidy;                // Due Date for Weekly Interim Payments
        FNlost  = scLost;                // Auto Update Quote to Lost Status, # of Days
        FNaclr  = scAclr;                // Auto-Create Entity/Location on Employer
        FNattn  = scAttn;                // Policy Notice Attention Name
        //*** Beg Add ***** 05/08/25 ******************************
        If scACPhn# > *zeros;
           FNaara  = %dec(%subst(%editc(scACPhn#:'X'):1:3):3:0); // Area Code
           FNaphn  = %dec(%subst(%editc(scACPhn#:'X'):4):7:0);  // Phone Number
        Endif;

        FNamail = scACEmlAd;
        //*** End Add ***** 05/08/25 ******************************

        FNuser  = q1user;                // User
        FNpgm   = q1pgm;                 // Program
        FNtime  = Timeto6(SystemTime()); // Time
        FNdt    = Dateto7(SystemDate()); // Date Mintained
        FNiusr  = q1user;                // Initial User
        FNipgm  = q1pgm;                 // Initial Program
        FNitim  = Timeto6(SystemTime()); // Initial Time
        FNidt   = Dateto7(SystemDate()); // Initial Date

        FNlhf   = 1.00;                  // L&H Load F type

        // Valid Claim Reserve Types
        FNrs01  = scRs01;
        FNrs02  = scRs02;
        FNrs03  = scRs03;
        FNrs04  = scRs04;
        FNrs05  = scRs05;
        FNrs06  = scRs06;
        FNrs07  = scRs07;
        FNrs08  = scRs08;
        FNrs09  = scRs09;
        FNrs10  = scRs10;

        //***  Beg Add  ***  04/29/24  *** WC2965 ***********************
        FNEmail = scEmail;
        //***  End Add  ***  04/29/24  *** WC2965 ***********************

       // Web Interface - Include Group for Connect
       // Update WMFIP file fields
        FIquote  = scQuote;
        //*** Begin Add *** 11/04/21 ***************************************
        //*** Begin Del *** 09/09/25 ***************************************
        //*** FIexcCmpRt = scExcCmprt;
        //*** End   Del *** 09/09/25 ***************************************
        //*** End   Add *** 11/04/21 ***************************************
        FIpolicy = scPolicy;
        FIclaim  = scClaim;
        FIsr     = scSR;
        FIfroi   = scFROI;
        If isCmpDft = *off;
           FIasOf   = DtCYMD(scAsOfP);
           FIdays   = scDaysP;
        Endif;

        FIco#  = FNco#;
        FIfnd  = FNfnd;
        //FIstat = 'A';
        FIuser = FNuser;
        FIpgm  = FNpgm;
        FItime = FNtime;
        FIdt   = FNdt;
        FIiUsr = FNuser;
        FIiPgm = FNpgm;
        FIiDt  = FNdt;
        FIiTim = FNtime;


       // Update WDF2P file fields - Auxillary file info
        F2co#  = FNco#;
        F2fnd  = FNfnd;
        F2fein = scFein;
        F2tcc  = scTcc;
        F2iaud = scIaud;
        F2sctc = scSctc;
        If scSphone > *zeros;
           F2area  = %dec(%subst(%editc(scSphone:'X'):1:3):3:0); // Phone Area Code
           F2sph#  = %dec(%subst(%editc(scSphone:'X'):4):7:0);   // Phone Number
        EndIf;
        If scSphone = *zeros;
           F2area  = *zeros;
           F2sph#  = *zeros;
        EndIf;
        F2car# = scCar#;
        F2wcpl = scWcpl;
        F2clck = scClck;
        F2agck = scAgck;
        F2macc = scMacc;
        F2pp3c = scPp3c;
        F2ppc3 = scPpc3;
        F2fold = scFold;
        F2dcid = scDcid;

        //F2stat = 'A';
        F2user = FNuser;
        F2pgm  = FNpgm;
        F2time = FNtime;
        F2dt   = FNdt;
        F2iUsr = FNuser;
        F2iPgm = FNpgm;
        F2iTim = FNtime;
        F2iDt  = FNdt;

        F2ctyp = scCtyp;
        F2tpa  = scTpa;
        //*** Beg Del ***** 04/26/23 *****************************
        //*** F2lbnm = scLbnm;
        //*** End Del ***** 04/26/23 *****************************
        F2lad1 = scLad1;
        F2lad2 = scLad2;
        F2lcty = scLcty;
        F2lst  = scLst;
        F2lzip = scLzip;
        F2blog = scBlog;
        F2ceov = scCeov;
        F2docl = scDocl;
        F2docs = scDocs;
        F2naic = scNaic;
        F2rre# = scRre#;
        F2tpac = scTpac;
        F2fysm = scFysm;
        F2spsg = scSpsg;
        F2jobq = scJobq;
        F2sbs  = scSbs;
        F2thrl = scThrl;
        F2thrh = scThrh;
        F2dbcd = scDbcd;
        F2pbcd = scPbcd;
        F2fbcd = scFbcd;
        F21acd = sc1acd;
        F22acd = sc2acd;
        F23acd = sc3acd;
        F24acd = sc4acd;
        F25acd = sc5acd;
        F26acd = sc6acd;
        F27acd = sc7acd;
        F2dissms = scDissms;
        F2slogo = scSlogo;
        F2spath = scSpath;
        F2fref = scFref;
        F2bsi# = scBsi#;
        F2ncargrp = scNCarGrp;
        F21099fein = sc1099fein;
        //*** Beg Add ***** 11/17/23 *****************************
        F2dxtr = scDxtr;
        If scDxtr = *blank;
           F2dxtr = 'N';
        Endif;
        //*** End Add ***** 11/17/23 *****************************
        //***  Beg Add  ***  01/16/24  *** 153WAD ***********************
        F2Rnote  = scRnote;
        F2RcxRsn = scRcxRsn;
        //***  End Add  ***  01/16/24  *** 153WAD ***********************


       // Update WMGCP file fields - Group Carrier Information
        GCco#  = FNco#;
        GCfnd  = FNfnd;
        GCdcar = scDcar;
        GCpsym = scPsym;
        GCccod = scCcod;
        GCpgid = scPgid;
        GCcfmt = scCfmt;
        GCfrcn = scFrcn;
        GCtocn = scTocn;
        GCnacn = scNacn;
        GCcrfl = scCrfl;
        GCacfl = scAcfl;
        GCucfl = scUcfl;
        GCdcfl = scDcfl;

        GCpgm  = FNpgm;
        GCuser = FNuser;
        GCdt   = FNdt;
        GCtime = FNtime;
        GCiPgm = FNpgm;
        GCiUsr = FNuser;
        GCiDt  = FNdt;

       EndSr;

       // Writing new Group subroutine
       BegSr WrtGroup;
        Clear pfn00;
        Exsr MoveScDb;
        Write pfn00;
        If scQuote <> ' ' or scPolicy <> ' ' or scClaim <> ' ' or
         scSR <> ' ' or scFROI <> ' ' or scAsOfP <> 0 or scDaysP <> 0;
           Write pfi00;
        EndIf;
        // WCMC Add Group Pompt
        If ##MCIF = 'Y';
           F0062R(@@kc:@@kl:@@fnd);
        EndIf;
        // Write Group Auxillary file info
        Write pf200;
        // Write Group Carrier Information file
        Write pgc00;

      // Replace Job number with Time in Policy years file
          Exec SQL
           Update WDF1P
            Set F1fnd = :scFnd, F1itm = :FNtime, F1time = :FNtime
            where F1iusr = :q1user and F1itm = :q1jobn;


      // Select newly added Group at the top of the Grid
          svnewfnd = scfnd;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // processAddYear - process addition of policy year/Group Detail
       //===================================================================
     P ProcessAddYear  B
     D ProcessAddYear  PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear RcdGrpDtl;
       mode = 'A';
       isexit = *off;
       reload = *on;
       BtnDtlCan = *off;
       scGrpHdg = 'Add Group Detail Information';

       Dow BtnDtlCan = *off and isexit = *off;

          // Loading initial values
          InzGrpDtlScVal();

          // Loading Default values from Prior years
          If reload = *on;
             Exsr MoveDbSc;
             reload = *off;
          Endif;

          // Load A/R Codes subfiles
          Exsr LoadARsfl;

          Exfmt RcdGrpDtl;

          Select;
          // Savd/Add Next Button
          When BtnDtlSvAd = *on;
             BtnDtlSvAd = *off;
             If ValidateGrpDtl(mode);
                reload = *on;
                Exsr WrtGrpDtl;
             EndIf;

          // Savd Button
          When BtnDtlSave = *on;
             BtnDtlSave = *off;
             If ValidateGrpDtl(mode);
                reload = *on;
                Exsr WrtGrpDtl;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnDtlCan = *on;
               BtnDtlCan = *off;
               isexit = *on;

          EndSl;
       Enddo;

       // Load A/R Codes subfile
       Begsr LoadARsfl;

        // Clear the subfile
        rrnSpecAR = 0;
        dspSpecAR = *off;
        clrSpecAR = *on;
        rrnDepPrm = 0;
        dspDepPrm = *off;
        clrDepPrm = *on;
        rrnFlatCx = 0;
        dspFlatCx = *off;
        clrFlatCx = *on;
        rrnCrAuBl = 0;
        dspCrAuBl = *off;
        clrCrAuBl = *on;
        rrnDepoSR = 0;
        dspDepoSR = *off;
        clrDepoSR = *on;
        rrnCollAR = 0;
        dspCollAR = *off;
        clrCollAR = *on;
        rrnDivPln = 0;
        dspDivPln = *off;
        clrDivPln = *on;

        Write rcdGrpDtl;

        clrSpecAR = *off;
        dspSpecAR = *on;
        clrDepPrm = *off;
        dspDepPrm = *on;
        clrFlatCx = *off;
        dspFlatCx = *on;
        clrCrAuBl = *off;
        dspCrAuBl = *on;
        clrDepoSR = *off;
        dspDepoSR = *on;
        clrCollAR = *off;
        dspCollAR = *on;
        clrDivPln = *off;
        dspDivPln = *on;

        If dspMode;
           inquiry_0 = *on;
        Endif;

        // Populate the subfile

        // Grid 1 -- Specific A/R Transaction Codes
        i=1;
        Dow i <= 18;
           rrnSpecAR += 1;
           scHdSpecAR = strHeads1(i);
           scVaSpecAR = strARcod1(i);
           scSpecARCH = strSelCr1(i);

           Write sflSpecAR;
           i +=1;
        Enddo;
        rrnSpecAR = 1;

        // Grid 2 -- Deposit to Premium Transfer
        i=1;
        Dow i <= 8;
           rrnDepPrm += 1;
           scHdDepPrm = strHeads2(i);
           scVaDepPrm = strARcod2(i);
           scDepPrmCH = strSelCr2(i);

           Write sflDepPrm;
           i +=1;
        Enddo;
        rrnDepPrm = 1;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i=1;
        Dow i <= 11;
           rrnFlatCx += 1;
           scHdFlatCx = strHeads3(i);
           scVaFlatCx = strARcod3(i);
           scFlatCxCH = strSelCr3(i);

           Write sflFlatCx;
           i +=1;
        Enddo;
        rrnFlatCx = 1;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i=1;
        Dow i <= 17;
           rrnCrAuBl += 1;
           scHdCrAuBl = strHeads4(i);
           scVaCrAuBl = strARcod4(i);
           scCrAuBlCH = strSelCr4(i);

           Write sflCrAuBl;
           i +=1;
        Enddo;
        rrnCrAuBl = 1;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i=1;
        Dow i <= 8;
           rrnDepoSR += 1;
           scHdDepoSR = strHeads5(i);
           scVaDepoSR = strARcod5(i);
           scDepoSRCH = strSelCr5(i);

           Write sflDepoSR;
           i +=1;
        Enddo;
        rrnDepoSR = 1;

        // Grid 6 -- Collateral A/R Codes
        i=1;
        Dow i <= 3;
           rrnCollAR += 1;
           scHdCollAR = strHeads6(i);
           scVaCollAR = strARcod6(i);
           scCollARCH = strSelCr6(i);

           Write sflCollAR;
           i +=1;
        Enddo;
        rrnCollAR = 1;

        // Grid 7 -- Dividend Plan
        i=1;
        Dow i <= 3;
           rrnDivPln += 1;
           scHdDivPln = strHeads7(i);
           scVaDivPln = strARcod7(i);
           scDivPlnCH = strSelCr7(i);

           Write sflDivPln;
           i +=1;
        Enddo;
        rrnDivPln = 1;
       EndSr;

       // Subrotine to move Database fields into Screen fields.
       BegSr MoveDbSc;

        scfyr = 999;
        setgt keyWDF1L pf101;
        Readpe (scCo#:scFnd) pf101;
        scfyr = 0;
        If Not %Eof(wdf1l);
           scfyr   = F1fyr + 1;
           scStrDt = DtMDYY(F1stpo) + 1;
           scEndDt = DtMDYY(F1enpo) + 1;
           scDps   = F1dps;
           scMfee  = F1mfee;
           scEmrt  = F1emrt;
           scDadj  = F1dadj;
           scCadj  = F1cadj;
           scNetd  = scStrDt;
           scScdt  = DtMDYY(F1scdt);
           scManp  = F1manp;

          // Flags
           scUpar = F1upar;
           scFaud = F1faud;
           scSpla = F1spla;
           scRlxc = F1rlxc;
           scClms = F1clms;
           scExdv = F1exdv;
           scPmfl = F1pmfl;
           scYrad = F1yrad;
           scCxad = F1cxad;
           scApfd = F1apfd;
           scBrfl = F1brfl;
           scBvfl = F1bvfl;
           scAced = F1aced;
           scNetr = F1netr;
           //*** Begin Add *** 01/07/22 ************************************
           If F1netSR = ' ';
              scNetSR = 'N';
           Else;
              scNetSR = F1netSR;
           EndIf;
           //*** End   Add *** 01/07/22 ************************************
           scAafl = F1aafl;
           scDpym = F1dpym;
           scApfl = F1apfl;
           scBlfl = F1blfl;
           scNy07 = F1ny07;
           scIdbd = F1idbd;
           //*** Begin Add *** 05/14/24 ************************************
           scSRAs = F1SRAs;
           //*** End   Add *** 05/14/24 ************************************

         // Set A/R Codes fields
           strARcod1(1)  = F1srbb;
           strARcod1(2)  = F1nbpt;
           strARcod1(3)  = F1nbtr;
           strARcod1(4)  = F1dscd;
           strARcod1(5)  = F1mfpt;
           strARcod1(6)  = F1sftx;
           strARcod1(7)  = F1prtr;
           strARcod1(8)  = F1nbdt;
           strARcod1(9)  = F1cdet;
           strARcod1(10) = F11stt;
           strARcod1(11) = F12ndt;
           strARcod1(12) = F13rdt;
           strARcod1(13) = F14tht;
           strARcod1(14) = F15tht;
           strARcod1(15) = F16tht;
           strARcod1(16) = F17tht;
           strARcod1(17) = F1txtx;
           strARcod1(18) = F1txpy;

           strARcod2(1)  = F1dbtr;
           strARcod2(2)  = F1tftr;
           strARcod2(3)  = F1dptr;
           strARcod2(4)  = F1tqtr;
           strARcod2(5)  = F1pptr;
           strARcod2(6)  = F1tptr;
           strARcod2(7)  = F1dtxb;
           strARcod2(8)  = F1dtxp;

           strARcod3(1)  = F1mftr;
           strARcod3(2)  = F1satr;
           strARcod3(3)  = F1Txtr;
           strARcod3(4)  = F1fctr;
           strARcod3(5)  = F11atr;
           strARcod3(6)  = F12atr;
           strARcod3(7)  = F13atr;
           strARcod3(8)  = F14atr;
           strARcod3(9)  = F15atr;
           strARcod3(10) = F16atr;
           strARcod3(11) = F17atr;

           strARcod4(1)  = F1axdd;
           strARcod4(2)  = F1axec;
           strARcod4(3)  = F1axgf;
           strARcod4(4)  = F1axlf;
           strARcod4(5)  = F1axmf;
           strARcod4(6)  = F1axdp;
           strARcod4(7)  = F1axsf;
           strARcod4(8)  = F1axtx;
           strARcod4(9)  = F1axtd;
           strARcod4(10) = F1ax1t;
           strARcod4(11) = F1ax2t;
           strARcod4(12) = F1ax3t;
           strARcod4(13) = F1ax4t;
           strARcod4(14) = F1ax5t;
           strARcod4(15) = F1ax6t;
           strARcod4(16) = F1ax7t;
           strARcod4(17) = F1axpt;

           strARcod5(1)  = F1shtr;
           strARcod5(2)  = F1sftr;
           strARcod5(3)  = F1sitr;
           strARcod5(4)  = F1sqtr;
           strARcod5(5)  = F1sptr;
           strARcod5(6)  = F1srtr;
           strARcod5(7)  = F1spof;
           strARcod5(8)  = F1srpo;

           strARcod6(1)  = F1colb;
           strARcod6(2)  = F1colp;
           strARcod6(3)  = F1colt;

           strARcod7(1)  = F1dvbl;
           strARcod7(2)  = F1dvpd;
           strARcod7(3)  = F1dvxr;

        EndIf;
       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        F1co#   = scCo#;                 // Company Number
        F1fnd   = scFnd;                 // Group Number
        F1fyr   = scfyr;                 // Policy Year
        F1stpo  = dtCYMD(scStrDt);       // Policy Start date
        F1enpo  = dtCYMD(scEndDt);       // Policy End date
        F1dps   = scDps;                 // Deposit Style
        F1mfee  = scMfee;                // Membership Fee Amount
        F1emrt  = scEmrt;                // Employer Specific Retention
        F1dadj  = scDadj;                // Threshold Amount for Debit Adjustment
        F1cadj  = scCadj;                // Threshold Amount for Credit Adjustment
        F1netd  = dtCYMD(scNetd);        // Starting with Policies Effective
        F1scdt  = dtCYMD(scScdt);        // Stop Collateral Date
        F1manp  = scManp;                // % of Manual Premium

        //F1stat = 'A';
        F1user = q1user;
        F1pgm  = q1pgm;
        F1time = Timeto6(SystemTime());
        F1dt   = Dateto7(SystemDate());
        F1iUsr = q1user;
        F1iPgm = q1pgm;
      //F1iTm  = Timeto6(SystemTime());
       // Move Job number temporarily in initial time
        F1iTm  = q1jobn;
        F1iDt  = Dateto7(SystemDate());

       // Flags
        F1upar  = scUpar;
        F1faud  = scFaud;
        F1spla  = scSpla;
        F1rlxc  = scRlxc;
        F1clms  = scClms;
        F1exdv  = scExdv;
        F1pmfl  = scPmfl;
        F1yrad  = scYrad;
        F1cxad  = scCxad;
        F1apfd  = scApfd;
        F1brfl  = scBrfl;
        F1bvfl  = scBvfl;
        F1aced  = scAced;
        F1netr  = scNetr;
        //*** Begin Add *** 01/07/22 **************************************
        If scNetSR = ' ';
           F1netSR = 'N';
        Else;
           If scNetR  = 'Y';
              F1netSr = 'N';
           Else;
              F1netSR = scNetSR;
           EndIf;
        EndIf;
        //*** End   Add *** 01/07/22 **************************************
        F1aafl  = scAafl;
        F1dpym  = scDpym;
        F1apfl  = scApfl;
        F1blfl  = scBlfl;
        F1ny07  = scNy07;
        F1idbd  = scIdbd;
        //*** Begin Add *** 05/14/24 ************************************
        F1SRAs  = scSRAs;
        //*** End   Add *** 05/14/24 ************************************

       // Set A/R Codes fields
        F1srbb  =  strARcod1(1);
        F1nbpt  =  strARcod1(2);
        F1nbtr  =  strARcod1(3);
        F1dscd  =  strARcod1(4);
        F1mfpt  =  strARcod1(5);
        F1sftx  =  strARcod1(6);
        F1prtr  =  strARcod1(7);
        F1nbdt  =  strARcod1(8);
        F1cdet  =  strARcod1(9);
        F11stt  =  strARcod1(10);
        F12ndt  =  strARcod1(11);
        F13rdt  =  strARcod1(12);
        F14tht  =  strARcod1(13);
        F15tht  =  strARcod1(14);
        F16tht  =  strARcod1(15);
        F17tht  =  strARcod1(16);
        F1txtx  =  strARcod1(17);
        F1txpy  =  strARcod1(18);

        F1dbtr  =  strARcod2(1);
        F1tftr  =  strARcod2(2);
        F1dptr  =  strARcod2(3);
        F1tqtr  =  strARcod2(4);
        F1pptr  =  strARcod2(5);
        F1tptr  =  strARcod2(6);
        F1dtxb  =  strARcod2(7);
        F1dtxp  =  strARcod2(8);

        F1mftr  =  strARcod3(1);
        F1satr  =  strARcod3(2);
        F1Txtr  =  strARcod3(3);
        F1fctr  =  strARcod3(4);
        F11atr  =  strARcod3(5);
        F12atr  =  strARcod3(6);
        F13atr  =  strARcod3(7);
        F14atr  =  strARcod3(8);
        F15atr  =  strARcod3(9);
        F16atr  =  strARcod3(10);
        F17atr  =  strARcod3(11);

        F1axdd  =  strARcod4(1);
        F1axec  =  strARcod4(2);
        F1axgf  =  strARcod4(3);
        F1axlf  =  strARcod4(4);
        F1axmf  =  strARcod4(5);
        F1axdp  =  strARcod4(6);
        F1axsf  =  strARcod4(7);
        F1axtx  =  strARcod4(8);
        F1axtd  =  strARcod4(9);
        F1ax1t  =  strARcod4(10);
        F1ax2t  =  strARcod4(11);
        F1ax3t  =  strARcod4(12);
        F1ax4t  =  strARcod4(13);
        F1ax5t  =  strARcod4(14);
        F1ax6t  =  strARcod4(15);
        F1ax7t  =  strARcod4(16);
        F1axpt  =  strARcod4(17);

        F1shtr  =  strARcod5(1);
        F1sftr  =  strARcod5(2);
        F1sitr  =  strARcod5(3);
        F1sqtr  =  strARcod5(4);
        F1sptr  =  strARcod5(5);
        F1srtr  =  strARcod5(6);
        F1spof  =  strARcod5(7);
        F1srpo  =  strARcod5(8);

        F1colb  =  strARcod6(1);
        F1colp  =  strARcod6(2);
        F1colt  =  strARcod6(3);

        F1dvbl  =  strARcod7(1);
        F1dvpd  =  strARcod7(2);
        F1dvxr  =  strARcod7(3);

       EndSr;

       // Read A/R Codes subfile
       Begsr ReadARsfl;

        // Grid 1 -- Specific A/R Transaction Codes
        i = 1;
        ReadC sflSpecAR;
        Dow not %eof;
           strARcod1(i) = scVaSpecAR;
           i += 1;
           ReadC sflSpecAR;
        Enddo;

        // Grid 2 -- Deposit to Premium Transfer
        i = 1;
        ReadC sflDepPrm;
        Dow not %eof;
           strARcod2(i) = scVaDepPrm;
           i += 1;
           ReadC sflDepPrm;
        Enddo;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i = 1;
        ReadC sflFlatCx;
        Dow not %eof;
           strARcod3(i) = scVaFlatCx;
           i += 1;
           ReadC sflFlatCx;
        Enddo;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i = 1;
        ReadC sflCrAuBl;
        Dow not %eof;
           strARcod4(i) = scVaCrAuBl;
           i += 1;
           ReadC sflCrAuBl;
        Enddo;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i = 1;
        ReadC sflDepoSR;
        Dow not %eof;
           strARcod5(i) = scVaDepoSR;
           i += 1;
           ReadC sflDepoSR;
        Enddo;

        // Grid 6 -- Collateral A/R Codes
        i = 1;
        ReadC sflCollAR;
        Dow not %eof;
           strARcod6(i) = scVaCollAR;
           i += 1;
           ReadC sflCollAR;
        Enddo;

        // Grid 7 -- Dividend Plan
        i = 1;
        ReadC sflDivPln;
        Dow not %eof;
           strARcod7(i) = scVaDivPln;
           i += 1;
           ReadC sflDivPln;
        Enddo;
       EndSr;


       // Writing Policy Years/Group Details subroutine
       BegSr WrtGrpDtl;
        Clear pf100;

        // Read A/R Codes subfiles
        Exsr ReadARsfl;

        Exsr MoveScDb;
        Write pf100;
       EndSr;

      /end-free
     P ProcessAddYear  E

       //===================================================================
       // ProcOther - Process other Options
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
      /free

       ReadC sflGrpCfg;

       Dow not %eof;

          Select;
          // Processing for Copy Group Link
          When scGrpCpy = *On;
             scGrpCpy = *Off;
             @@fnd = scGrp;
             CopyGroup(@@fnd:@@kc:@@kl);

             btnRefresh = *On;

          Endsl;

          ReadC sflGrpCfg;
       Enddo;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProcOther2 - Process other Options for Policy Years Grid
       //===================================================================
     P ProcOther2      B
     D ProcOther2      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear RcdGrpDtl;
       reload = *on;
       isexit = *off;
       ReadC sflPolYr;

       Dow not %eof;

          // Processing for Display Years Link
          If scPolYrDsp = *On;
             scPolYrDsp = *Off;
             mode = 'D';
             scGrpHdg = 'Display Group Detail Information';

             // Loading initial values
             InzGrpDtlScVal();

             // Load Edit Group Detail Screen
             Chain(n) scPolRRN WDF1P;
             Exsr MoveDbSc;

             rdFyr = *on;                             // Read only policy year
             inquiry_0 = *on;                         // Read all fields
             visDtlSav  = *on;                        // Hide Save Button
             visDtlSvAd = *on;                        // Hide Save/Add next Button

             // Load A/R Codes subfiles
             Exsr LoadARsfl;

             Exfmt RcdGrpDtl;
          EndIf;


          // Processing for Edit Years Link
          If scPolYrEdt = *On;
             scPolYrEdt = *Off;
             mode = 'C';
             scGrpHdg = 'Edit Group Detail Information';

             // Loading initial values
             InzGrpDtlScVal();

             Chain scPolRRN WDF1P;

             // Load Edit Group Detail Screen
             If reload = *on;
                Exsr MoveDbSc;
                reload = *off;
             Endif;

             rdFyr = *on;                             // Read only policy year
             visDtlSvAd = *on;                        // Hide Save/Add next Button

             // Load A/R Codes subfiles
             Exsr LoadARsfl;

             Dow isexit = *off;
                Exfmt RcdGrpDtl;

                Select;
                // Savd Button
                When BtnDtlSave = *on;
                   BtnDtlSave = *off;
                   If ValidateGrpDtl(mode);
                      reload = *on;

                      // Read A/R Codes subfiles
                      Exsr ReadARsfl;
                      Exsr MoveScDb;
                      Update pf100;
                      isexit = *on;
                   EndIf;

                // Cacel Button
                When BtnDtlCan = *on;
                     BtnDtlCan = *off;
                      isexit = *on;
                EndSl;
             EndDo;

             Unlock WDF1P;
          Endif;

          ReadC sflPolYr;
       Enddo;



       // Load A/R Codes subfile
       Begsr LoadARsfl;

        // Clear the subfile
        rrnSpecAR = 0;
        dspSpecAR = *off;
        clrSpecAR = *on;
        rrnDepPrm = 0;
        dspDepPrm = *off;
        clrDepPrm = *on;
        rrnFlatCx = 0;
        dspFlatCx = *off;
        clrFlatCx = *on;
        rrnCrAuBl = 0;
        dspCrAuBl = *off;
        clrCrAuBl = *on;
        rrnDepoSR = 0;
        dspDepoSR = *off;
        clrDepoSR = *on;
        rrnCollAR = 0;
        dspCollAR = *off;
        clrCollAR = *on;
        rrnDivPln = 0;
        dspDivPln = *off;
        clrDivPln = *on;

        Write rcdGrpDtl;

        clrSpecAR = *off;
        dspSpecAR = *on;
        clrDepPrm = *off;
        dspDepPrm = *on;
        clrFlatCx = *off;
        dspFlatCx = *on;
        clrCrAuBl = *off;
        dspCrAuBl = *on;
        clrDepoSR = *off;
        dspDepoSR = *on;
        clrCollAR = *off;
        dspCollAR = *on;
        clrDivPln = *off;
        dspDivPln = *on;

        If dspMode;
           inquiry_0 = *on;
        Endif;

        // Populate the subfile

        // Grid 1 -- Specific A/R Transaction Codes
        i=1;
        Dow i <= 18;
           rrnSpecAR += 1;
           scHdSpecAR = strHeads1(i);
           scVaSpecAR = strARcod1(i);
           scSpecARCH = strSelCr1(i);

           Write sflSpecAR;
           i +=1;
        Enddo;
        rrnSpecAR = 1;

        // Grid 2 -- Deposit to Premium Transfer
        i=1;
        Dow i <= 8;
           rrnDepPrm += 1;
           scHdDepPrm = strHeads2(i);
           scVaDepPrm = strARcod2(i);
           scDepPrmCH = strSelCr2(i);

           Write sflDepPrm;
           i +=1;
        Enddo;
        rrnDepPrm = 1;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i=1;
        Dow i <= 11;
           rrnFlatCx += 1;
           scHdFlatCx = strHeads3(i);
           scVaFlatCx = strARcod3(i);
           scFlatCxCH = strSelCr3(i);

           Write sflFlatCx;
           i +=1;
        Enddo;
        rrnFlatCx = 1;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i=1;
        Dow i <= 17;
           rrnCrAuBl += 1;
           scHdCrAuBl = strHeads4(i);
           scVaCrAuBl = strARcod4(i);
           scCrAuBlCH = strSelCr4(i);

           Write sflCrAuBl;
           i +=1;
        Enddo;
        rrnCrAuBl = 1;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i=1;
        Dow i <= 8;
           rrnDepoSR += 1;
           scHdDepoSR = strHeads5(i);
           scVaDepoSR = strARcod5(i);
           scDepoSRCH = strSelCr5(i);

           Write sflDepoSR;
           i +=1;
        Enddo;
        rrnDepoSR = 1;

        // Grid 6 -- Collateral A/R Codes
        i=1;
        Dow i <= 3;
           rrnCollAR += 1;
           scHdCollAR = strHeads6(i);
           scVaCollAR = strARcod6(i);
           scCollARCH = strSelCr6(i);

           Write sflCollAR;
           i +=1;
        Enddo;
        rrnCollAR = 1;

        // Grid 7 -- Dividend Plan
        i=1;
        Dow i <= 3;
           rrnDivPln += 1;
           scHdDivPln = strHeads7(i);
           scVaDivPln = strARcod7(i);
           scDivPlnCH = strSelCr7(i);

           Write sflDivPln;
           i +=1;
        Enddo;
        rrnDivPln = 1;
       EndSr;


       // Subrotine to move Database fields into Screen fields.
       BegSr MoveDbSc;

          scFyr   = F1Fyr;
          scStrDt = DtMDYY(F1stpo);
          scEndDt = DtMDYY(F1enpo);
          scDps   = F1dps;
          scMfee  = F1mfee;
          scEmrt  = F1emrt;
          scDadj  = F1dadj;
          scCadj  = F1cadj;
          scNetd  = DtMDYY(F1Netd);
          scScdt  = DtMDYY(F1scdt);
          scManp  = F1manp;

         // Flags
          scUpar = F1upar;
          scFaud = F1faud;
          scSpla = F1spla;
          scRlxc = F1rlxc;
          scClms = F1clms;
          scExdv = F1exdv;
          scPmfl = F1pmfl;
          scYrad = F1yrad;
          scCxad = F1cxad;
          scApfd = F1apfd;
          scBrfl = F1brfl;
          scBvfl = F1bvfl;
          scAced = F1aced;
          scNetr = F1netr;
          //*** Begin Add *** 01/07/22 ************************************
          If F1netSR = ' ';
             scNetSR = 'N';
          Else;
             scNetSR = F1netSR;
          EndIf;
          //*** End   Add *** 01/07/22 ************************************
          scAafl = F1aafl;
          scDpym = F1dpym;
          scApfl = F1apfl;
          scBlfl = F1blfl;
          scNy07 = F1ny07;
          scIdbd = F1idbd;
          //*** Begin Add *** 05/14/24 ************************************
          scSRAs = F1SRAs;
          //*** End   Add *** 05/14/24 ************************************

         // Set A/R Codes fields
           strARcod1(1)  = F1srbb;
           strARcod1(2)  = F1nbpt;
           strARcod1(3)  = F1nbtr;
           strARcod1(4)  = F1dscd;
           strARcod1(5)  = F1mfpt;
           strARcod1(6)  = F1sftx;
           strARcod1(7)  = F1prtr;
           strARcod1(8)  = F1nbdt;
           strARcod1(9)  = F1cdet;
           strARcod1(10) = F11stt;
           strARcod1(11) = F12ndt;
           strARcod1(12) = F13rdt;
           strARcod1(13) = F14tht;
           strARcod1(14) = F15tht;
           strARcod1(15) = F16tht;
           strARcod1(16) = F17tht;
           strARcod1(17) = F1txtx;
           strARcod1(18) = F1txpy;

           strARcod2(1)  = F1dbtr;
           strARcod2(2)  = F1tftr;
           strARcod2(3)  = F1dptr;
           strARcod2(4)  = F1tqtr;
           strARcod2(5)  = F1pptr;
           strARcod2(6)  = F1tptr;
           strARcod2(7)  = F1dtxb;
           strARcod2(8)  = F1dtxp;

           strARcod3(1)  = F1mftr;
           strARcod3(2)  = F1satr;
           strARcod3(3)  = F1Txtr;
           strARcod3(4)  = F1fctr;
           strARcod3(5)  = F11atr;
           strARcod3(6)  = F12atr;
           strARcod3(7)  = F13atr;
           strARcod3(8)  = F14atr;
           strARcod3(9)  = F15atr;
           strARcod3(10) = F16atr;
           strARcod3(11) = F17atr;

           strARcod4(1)  = F1axdd;
           strARcod4(2)  = F1axec;
           strARcod4(3)  = F1axgf;
           strARcod4(4)  = F1axlf;
           strARcod4(5)  = F1axmf;
           strARcod4(6)  = F1axdp;
           strARcod4(7)  = F1axsf;
           strARcod4(8)  = F1axtx;
           strARcod4(9)  = F1axtd;
           strARcod4(10) = F1ax1t;
           strARcod4(11) = F1ax2t;
           strARcod4(12) = F1ax3t;
           strARcod4(13) = F1ax4t;
           strARcod4(14) = F1ax5t;
           strARcod4(15) = F1ax6t;
           strARcod4(16) = F1ax7t;
           strARcod4(17) = F1axpt;

           strARcod5(1)  = F1shtr;
           strARcod5(2)  = F1sftr;
           strARcod5(3)  = F1sitr;
           strARcod5(4)  = F1sqtr;
           strARcod5(5)  = F1sptr;
           strARcod5(6)  = F1srtr;
           strARcod5(7)  = F1spof;
           strARcod5(8)  = F1srpo;

           strARcod6(1)  = F1colb;
           strARcod6(2)  = F1colp;
           strARcod6(3)  = F1colt;

           strARcod7(1)  = F1dvbl;
           strARcod7(2)  = F1dvpd;
           strARcod7(3)  = F1dvxr;

       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        F1fyr   = scfyr;                 // Policy Year
        F1stpo  = dtCYMD(scStrDt);       // Policy Start date
        F1enpo  = dtCYMD(scEndDt);       // Policy End date
        F1dps   = scDps;                 // Deposit Style
        F1mfee  = scMfee;                // Membership Fee Amount
        F1emrt  = scEmrt;                // Employer Specific Retention
        F1dadj  = scDadj;                // Threshold Amount for Debit Adjustment
        F1cadj  = scCadj;                // Threshold Amount for Credit Adjustment
        F1netd  = dtCYMD(scNetd);        // Starting with Policies Effective
        F1scdt  = dtCYMD(scScdt);        // Stop Collateral Date
        F1manp  = scManp;                // % of Manual Premium

        F1user = q1user;
        F1pgm  = q1pgm;
        F1time = Timeto6(SystemTime());
        F1dt   = Dateto7(SystemDate());

       // Flags
        F1upar  = scUpar;
        F1faud  = scFaud;
        F1spla  = scSpla;
        F1rlxc  = scRlxc;
        F1clms  = scClms;
        F1exdv  = scExdv;
        F1pmfl  = scPmfl;
        F1yrad  = scYrad;
        F1cxad  = scCxad;
        F1apfd  = scApfd;
        F1brfl  = scBrfl;
        F1bvfl  = scBvfl;
        F1aced  = scAced;
        F1netr  = scNetr;
        //*** Begin Add *** 01/07/22 **************************************
        If scNetSR = ' ';
           F1netSR = 'N';
        Else;
           If scNetR  = 'Y';
              F1netSr = 'N';
           Else;
              F1netSR = scNetSR;
           EndIf;
        EndIf;
        //*** End   Add *** 01/07/22 **************************************
        F1aafl  = scAafl;
        F1dpym  = scDpym;
        F1apfl  = scApfl;
        F1blfl  = scBlfl;
        F1ny07  = scNy07;
        F1idbd  = scIdbd;
        //*** Begin Add *** 05/14/24 ************************************
        F1SRAs  = scSRAs;
        //*** End   Add *** 05/14/24 ************************************

       // Set A/R Codes fields
        F1srbb  =  strARcod1(1);
        F1nbpt  =  strARcod1(2);
        F1nbtr  =  strARcod1(3);
        F1dscd  =  strARcod1(4);
        F1mfpt  =  strARcod1(5);
        F1sftx  =  strARcod1(6);
        F1prtr  =  strARcod1(7);
        F1nbdt  =  strARcod1(8);
        F1cdet  =  strARcod1(9);
        F11stt  =  strARcod1(10);
        F12ndt  =  strARcod1(11);
        F13rdt  =  strARcod1(12);
        F14tht  =  strARcod1(13);
        F15tht  =  strARcod1(14);
        F16tht  =  strARcod1(15);
        F17tht  =  strARcod1(16);
        F1txtx  =  strARcod1(17);
        F1txpy  =  strARcod1(18);

        F1dbtr  =  strARcod2(1);
        F1tftr  =  strARcod2(2);
        F1dptr  =  strARcod2(3);
        F1tqtr  =  strARcod2(4);
        F1pptr  =  strARcod2(5);
        F1tptr  =  strARcod2(6);
        F1dtxb  =  strARcod2(7);
        F1dtxp  =  strARcod2(8);

        F1mftr  =  strARcod3(1);
        F1satr  =  strARcod3(2);
        F1Txtr  =  strARcod3(3);
        F1fctr  =  strARcod3(4);
        F11atr  =  strARcod3(5);
        F12atr  =  strARcod3(6);
        F13atr  =  strARcod3(7);
        F14atr  =  strARcod3(8);
        F15atr  =  strARcod3(9);
        F16atr  =  strARcod3(10);
        F17atr  =  strARcod3(11);

        F1axdd  =  strARcod4(1);
        F1axec  =  strARcod4(2);
        F1axgf  =  strARcod4(3);
        F1axlf  =  strARcod4(4);
        F1axmf  =  strARcod4(5);
        F1axdp  =  strARcod4(6);
        F1axsf  =  strARcod4(7);
        F1axtx  =  strARcod4(8);
        F1axtd  =  strARcod4(9);
        F1ax1t  =  strARcod4(10);
        F1ax2t  =  strARcod4(11);
        F1ax3t  =  strARcod4(12);
        F1ax4t  =  strARcod4(13);
        F1ax5t  =  strARcod4(14);
        F1ax6t  =  strARcod4(15);
        F1ax7t  =  strARcod4(16);
        F1axpt  =  strARcod4(17);

        F1shtr  =  strARcod5(1);
        F1sftr  =  strARcod5(2);
        F1sitr  =  strARcod5(3);
        F1sqtr  =  strARcod5(4);
        F1sptr  =  strARcod5(5);
        F1srtr  =  strARcod5(6);
        F1spof  =  strARcod5(7);
        F1srpo  =  strARcod5(8);

        F1colb  =  strARcod6(1);
        F1colp  =  strARcod6(2);
        F1colt  =  strARcod6(3);

        F1dvbl  =  strARcod7(1);
        F1dvpd  =  strARcod7(2);
        F1dvxr  =  strARcod7(3);

       EndSr;

       // Read A/R Codes subfile
       Begsr ReadARsfl;

        // Grid 1 -- Specific A/R Transaction Codes
        i = 1;
        ReadC sflSpecAR;
        Dow not %eof;
           strARcod1(i) = scVaSpecAR;
           i += 1;
           ReadC sflSpecAR;
        Enddo;

        // Grid 2 -- Deposit to Premium Transfer
        i = 1;
        ReadC sflDepPrm;
        Dow not %eof;
           strARcod2(i) = scVaDepPrm;
           i += 1;
           ReadC sflDepPrm;
        Enddo;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i = 1;
        ReadC sflFlatCx;
        Dow not %eof;
           strARcod3(i) = scVaFlatCx;
           i += 1;
           ReadC sflFlatCx;
        Enddo;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i = 1;
        ReadC sflCrAuBl;
        Dow not %eof;
           strARcod4(i) = scVaCrAuBl;
           i += 1;
           ReadC sflCrAuBl;
        Enddo;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i = 1;
        ReadC sflDepoSR;
        Dow not %eof;
           strARcod5(i) = scVaDepoSR;
           i += 1;
           ReadC sflDepoSR;
        Enddo;

        // Grid 6 -- Collateral A/R Codes
        i = 1;
        ReadC sflCollAR;
        Dow not %eof;
           strARcod6(i) = scVaCollAR;
           i += 1;
           ReadC sflCollAR;
        Enddo;

        // Grid 7 -- Dividend Plan
        i = 1;
        ReadC sflDivPln;
        Dow not %eof;
           strARcod7(i) = scVaDivPln;
           i += 1;
           ReadC sflDivPln;
        Enddo;
       EndSr;

      /end-free
     P ProcOther2      E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       // Set Screen Heading
       scHeading = 'Group Configuration';

       // Set Tab Names
       TbNmRcdPnl = 'Group Configuration';

       aMenHeight = 120;

       apFootHght = aMenHeight + 125;

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);

       //*** Beg Add ***** 09/18/23 *****************************
       // Retrieve Client Suffix
       RtvCltSuffix(suffix);
       //*** End Add ***** 09/18/23 *****************************

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

       intCo# = GetUserInfo(q1user);
       Chain intCo# pco00;
       If COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;

      // Get User Default Group
       UsrDftFnd = 0;
       Chain q1user pup01;
       If %found(smupl);
          UsrDftFnd = UPfnd;
       EndIf;

      // Check for Multi Group Restriction
       $$co#  = intCo#;
       $$usr  = q1user;
       $$appl = 'W';
       $$rtcd = *off;
       CheckGrpRst($$co#:$$usr:$$appl:$$rtcd);
       MultiGrpRst = $$rtcd;

      // Check Portal Module
        isPortal = *off;
        isPortal = IsModEnabled('Portal Module');

       // Get Flags value from Workers Comp control file
       // Determine if client is Admin only or Claims only
          visbAdmin = *off;
          visbClaim = *off;
          Chain 'WC' SWC01;
          If %found();
             If WCfl08 = 'A';                          // Admin Only
                visbAdmin = *on;
             Endif;
             If WCfl08 = 'C';                          // Claims only
                visbClaim = *on;
             Endif;
          Endif;

       processMe = *on;

       Return;


      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the Group addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateGroup   b
     d ValidateGroup   pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erFnd  = *off;
        msFnd  = *blanks;
        erName = *off;
        msName = *blanks;
        erShnm = *off;
        msShnm = *blanks;
        erMad1 = *off;
        msMad1 = *blanks;
        erMcty = *off;
        msMcty = *blanks;
        erMst  = *off;
        msMst  = *blanks;
        erMzip = *off;
        msMzip = *blanks;
        erPad1 = *off;
        msPad1 = *blanks;
        erPcty = *off;
        msPcty = *blanks;
        erPst  = *off;
        msPst  = *blanks;
        erPzip = *off;
        msPzip = *blanks;
        erPortDft = *off;
        msPortDft = *blanks;
        erAsOfP = *off;
        msAsOfP = *blanks;
        erEfDta = *off;
        msEfDta = *blanks;
        erExDta = *off;
        msExDta = *blanks;
        erMxex = *off;
        msMxex = *blanks;
        erNacs = *off;
        msNacs = *blanks;
        erNaen = *off;
        msNaen = *blanks;
        erNaqn = *off;
        msNaqn = *blanks;
        erMetr = *off;
        msMetr = *blanks;
        erMep1 = *off;
        msMep1 = *blanks;
        erExtr = *off;
        msExtr = *blanks;
        erExp1 = *off;
        msExp1 = *blanks;
        erCotr = *off;
        msCotr = *blanks;
        erCop1 = *off;
        msCop1 = *blanks;
        erLetr = *off;
        msLetr = *blanks;
        erLep1 = *off;
        msLep1 = *blanks;
        erDfrc = *off;
        msDfrc = *blanks;
        erDfra = *off;
        msDfra = *blanks;
        erDfxc = *off;
        msDfxc = *blanks;
        erDfxa = *off;
        msDfxa = *blanks;
        erPeoe = *off;
        msPeoe = *blanks;
        erPeoa = *off;
        msPeoa = *blanks;
        //*** Beg Add ***  05/08/24  *** WC2965 *************************
        erAsofg = *off;
        msAsofg = *blanks;
        //*** End Add ***  05/08/24  *** WC2965 *************************
        erChk1 = *off;
        msChk1 = *blanks;
        erChk2 = *off;
        msChk2 = *blanks;
        erSior = *off;
        msSior = *blanks;
        erSiau = *off;
        msSiau = *blanks;
        erEdif = *off;
        msEdif = *blanks;
        erMidy = *off;
        msMidy = *blanks;
        erWidy = *off;
        msWidy = *blanks;
        erAnam = *off;
        msAnam = *blanks;
        erAad1 = *off;
        msAad1 = *blanks;
        erActy = *off;
        msActy = *blanks;
        erAst  = *off;
        msAst  = *blanks;
        erAzip = *off;
        msAzip = *blanks;

        //*** Beg Add ***  04/29/24  *** WC2965 *************************
        erEmail = *off;
        msEmail = *blanks;
        //*** End Add ***  04/29/24  *** WC2965 *************************
        //*** Beg Add ***** 05/08/25 ************************************
         erACphn# = *off;
         msACphn# = *blanks;
         erACEmlAd = *off;
         msACEmlAd = *blanks;
        //*** End Add ***** 05/08/25 ************************************

        clear dsErClmRsv;
        clear dsMsClmRsv;

       // Group number is invalid.
        If scFnd <= *zero;
           erFnd = *on;
           msFnd = 'WCR0502';
           isError = *on;
        Endif;

       // Duplicate Group Number
        If scFnd <> *zero;
           chain (scCo#:scFnd) pfn00;
           If %found(wmfnl);
              erFnd = *on;
              msFnd = 'WC30001';
              isError = *on;
           Endif;
        Endif;

       // Group name cannot be blank.
        If scName = *blanks;
           erName = *on;
           msName = 'WCR0503';
           isError = *on;
        Endif;

       // Short name cannot be blank.
        If scShnm = *blanks;
           erShnm = *on;
           msShnm = 'WCR0503';
           isError = *on;
        Endif;

       // Enter complete info for Mailing address, if any of the field is entered
        If scMad1 <> *blanks or scMad2 <> *blanks or
         scMcty <> *blanks or scMst <> *blanks or scMzip <> 0;
           If scMad1 = *Blanks;
              erMad1 = *on;
              msMad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scMcty = *blanks;
              erMcty = *on;
              msMcty = 'WCR0503';
              isError = *on;
           Endif;
           If scMst  = *blanks;
              erMst  = *on;
              msMst  = 'WCR0503';
              isError = *on;
           Endif;
           If scMzip = *zeros;
              erMzip = *on;
              msMzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;

       // Enter complete info for Physical address, if any of the field is entered
        If scPad1 <> *blanks or scPad2 <> *blanks or
         scPcty <> *blanks or scPst <> *blanks or scPzip <> 0;
           If scPad1 = *Blanks;
              erPad1 = *on;
              msPad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scPcty = *blanks;
              erPcty = *on;
              msPcty = 'WCR0503';
              isError = *on;
           Endif;
           If scPst  = *blanks;
              erPst  = *on;
              msPst  = 'WCR0503';
              isError = *on;
           Endif;
           If scPzip = *zeros;
              erPzip = *on;
              msPzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;

       // Check for portal default flag set for another group
        If scPortDft = 'Y';
           Reset decFnd;
           Exec sql
           select FNfnd into :decFnd
           from WMFNP
            where FNco# = :scCo#
             and FNfnd <> :scFnd
             and FNportDft = 'Y';
           If decFnd <> 0;
              erPortDft = *on;
              msPortDft = 'Group ' + %editc(decFnd:'Z') +
               ' is already defined as the default group.';
              isError = *on;
           Endif;
        Endif;

       // Validation Check for Portal As of Date, if entered
        If isPortal = *on and isCmpDft = *off;
           If scAsOfP <> 0;
              erAsOfP = ValidDate(scAsOfP:charMsg);
              If erAsOfP = *on;
                 msAsOfP = charMsg;
                 isError = *on;
              Endif;
           Endif;
        Endif;

       // Validation Check for Effective Date
        erEfDta = ValidDate(scEfDta:charMsg);
        If erEfDta = *on;
           msEfDta = charMsg;
           isError = *on;
        Endif;

       // Validation Check for Expiration Date
        If scExDta <> 0;
           erExDta = ValidDate(scExDta:charMsg);
           If erExDta = *on;
              msExDta = charMsg;
              isError = *on;
           Endif;
        Endif;

       // Dates must be in Ascending order
        If scEfDta <> 0 and scExDta <> 0;
           If erExDta = *off and erEfDta = *off;
              erExDta = AscendDates(scEfDta:scExDta:charMsg);
              If erExDta = *on;
                 erEfDta = *on;
                 msExDta = charMsg;
                 msEfDta = charMsg;
                 isError = *on;
              Endif;
           Endif;
        Endif;

       // Max extended year policy (in months) must be > 12
        If scMxex <> 0 and scMxex < 12;
           erMxex = *on;
           msMxex = 'WCR2376';
           isError = *on;
        Endif;

       // Next Available Case Number must be greater than 0
        If scNacs <= *zeros;
           erNacs = *on;
           msNacs = 'WCR0502';
           isError = *on;
        Endif;

       // Next Available Employer Number must be greater than 0
        If scNaen <= *zeros;
           erNaen = *on;
           msNaen = 'WCR0502';
           isError = *on;
        Endif;

       // Next Available Quote Number must be greater than 0
        If scNaqn <= *zeros;
           erNaqn = *on;
           msNaqn = 'WCR0502';
           isError = *on;
        Endif;


       // Validate Net Deuctible Transaction Code and PV1
       // Medical
        If (scMetr <> *blanks and scMep1 = *blanks) or
         (scMetr = *blanks and scMep1 <> *blanks);
           erMetr = *on;
           msMetr = 'WCR0544';
           erMep1 = *on;
           msMep1 = 'WCR0544';
           isError = *on;
        Endif;

       // Expense
        If (scExtr <> *blanks and scExp1 = *blanks) or
         (scExtr = *blanks and scExp1 <> *blanks);
           erExtr = *on;
           msExtr = 'WCR0544';
           erExp1 = *on;
           msExp1 = 'WCR0544';
           isError = *on;
        Endif;

       // Compensation
        If (scCotr <> *blanks and scCop1 = *blanks) or
         (scCotr = *blanks and scCop1 <> *blanks);
           erCotr = *on;
           msCotr = 'WCR0544';
           erCop1 = *on;
           msCop1 = 'WCR0544';
           isError = *on;
        Endif;

       // Legal
        If (scLetr <> *blanks and scLep1 = *blanks) or
         (scLetr = *blanks and scLep1 <> *blanks);
           erLetr = *on;
           msLetr = 'WCR0544';
           erLep1 = *on;
           msLep1 = 'WCR0544';
           isError = *on;
        Endif;


       // Validate Default Medical Reserve Code and Amount
        If (scDfrc <> *blanks and scDfra = *zeros) or
         (scDfrc = *blanks and scDfra <> *zeros);
           erDfrc = *on;
           msDfrc = 'WCR0544';
           erDfra = *on;
           msDfra = 'WCR0544';
           isError = *on;
        Endif;

       // Validate Default Expense Reserve Code and Amount
        If (scDfxc <> *blanks and scDfxa = *zeros) or
         (scDfxc = *blanks and scDfxa <> *zeros);
           erDfxc = *on;
           msDfxc = 'WCR0544';
           erDfxa = *on;
           msDfxa = 'WCR0544';
           isError = *on;
        Endif;


       // Validate Checkbox - Charge Expense Constant to Emp #1 for PEO Group
        If scPeoe <> *blanks and scPeof = *Blanks;
           erPeoe = *on;
           msPeoe = 'WC31539';
           isError = *on;
        Endif;

       // Validate Checkbox - Calculate Stock Discount Across Entire PEO Group
        If scPeoa <> *blanks and scPeof = *Blanks;
           erPeoa = *on;
           msPeoa = 'WC31539';
           isError = *on;
        Endif;

        //*** Beg Add ***  05/08/24  *** WC2965 *************************
       // Validate Checkbox - ASO Group
        If scAsof <> *blanks and scPeof = *Blanks;
           erAsofg = *on;
           msAsofg = 'WC31539';
           isError = *on;
        Endif;
        //*** End Add ***  05/08/24  *** WC2965 *************************

       // Test the Check number range
        If scChk1 > scChk2;
           erChk1 = *on;
           msChk1 = 'WCR0559';
           erChk2 = *on;
           msChk2 = 'WCR0559';
           isError = *on;
        Endif;

        //***  Beg Add  ***  04/29/24  *** WC2965 ***********************
       // Validate Default Email Address Entered.
        If scEmail <> *blanks;
           erEmail = Check_Email(scEmail:msEmail);
           If erEmail = *On;
              isError = *on;
           EndIf;
        Endif;
        //***  End Add  ***  04/29/24  *** WC2965 ***********************

       // Validate Self-insured Organization Type Code
        If scSior <> *blanks and scSfin <> 'Y';
           erSior = *on;
           msSior = 'WC31348';
           isError = *on;
        Endif;

       // Validate Self-insured Authorization Type Code
        If scSiau <> *blanks and scSfin <> 'Y';
           erSiau = *on;
           msSiau = 'WC31348';
           isError = *on;
        Endif;

       // Validate If EDI Format Identifier is required
        If visbEdif = *on and scEdif = *blanks;
           erEdif = *on;
           msEdif = 'WC31531';
           isError = *on;
        Endif;

       // Due Date for Monthly Interim Payments must be from 1 to 28
        If scMidy < 0 or scMidy > 28;
           erMidy = *on;
           msMidy = 'WC30125';
           isError = *on;
        Endif;

       // Due Date for Weekly Interim Payments must be from 1 to 28
        If scWidy < 0 or scWidy > 28;
           erWidy = *on;
           msWidy = 'WC30125';
           isError = *on;
        Endif;

        //*** Beg Add ***** 05/08/25 ******************************
       // Alternate Company Email Address
        If scACEmlAd <> *Blanks;
           erACEmlAd = Check_Email(scACEmlAd:msACEmlAd);
           If erACEmlAd = *On;
              isError = *on;
           Endif;
        EndIf;

       // Alternate Company Phone #
        If %subst(%editc(scACPhn#:'X'):1:3) <> '000' and
            %subst(%editc(scACPhn#:'X'):4:7) = '0000000';
           erACphn# = *on;
           msACphn# = 'Phone Number required.';
           isError = *on;
        Endif;

        If %subst(%editc(scACPhn#:'X'):1:3) = '000' and
            %subst(%editc(scACPhn#:'X'):4:7) <> '0000000';
           erACphn# = *on;
           msACphn# = 'Area Code required.';
           isError = *on;
        Endif;
        //*** End Add ***** 05/08/25 ******************************

       // Enter complete info for Alternate Name and address,
       // if any of the field is entered
        If scAnam <> *blanks or scAad1 <> *blanks or scAad2 <> *blanks or
         scActy <> *blanks or scAst <> *blanks or scAzip <> 0;
           If scAnam = *Blanks;
              erAnam = *on;
              msAnam = 'WCR0503';
              isError = *on;
           Endif;
           If scAad1 = *Blanks;
              erAad1 = *on;
              msAad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scActy = *blanks;
              erActy = *on;
              msActy = 'WCR0503';
              isError = *on;
           Endif;
           If scAst  = *blanks;
              erAst  = *on;
              msAst  = 'WCR0503';
              isError = *on;
           Endif;
           If scAzip = *zeros;
              erAzip = *on;
              msAzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;


       // Validate WDF2P file fields - Auxillary file info
        erFein = *off;
        msFein = *blanks;
        erTcc  = *off;
        msTcc  = *blanks;
        erCtyp = *off;
        mgCtyp = *blanks;
        erCtyp2 = *off;
        mgCtyp2 = *blanks;
        //*** Beg Del ***** 04/26/23 *****************************
        //*** erLbnm = *off;
        //*** msLbnm = *blanks;
        //*** End Del ***** 04/26/23 *****************************
        erLad1 = *off;
        msLad1 = *blanks;
        erLcty = *off;
        msLcty = *blanks;
        erLst  = *off;
        msLst  = *blanks;
        erLzip = *off;
        msLzip = *blanks;
        erBlog = *off;
        msBlog = *blanks;
        erCeov = *off;
        mgCeov = *blanks;
        erSpsg = *off;
        mgSpsg = *blanks;
        erFold = *off;
        msFold = *blanks;
        erFold2 = *off;
        msFold2 = *blanks;
        erDcid = *off;
        msDcid = *blanks;
        erSlogo = *off;
        mgSlogo = *blanks;
        erSpath = *off;
        mgSpath = *blanks;
        erFysm = *off;
        mgFysm = *blanks;
        erJobq = *off;
        msJobq = *blanks;
        //***  Begin Add ***** 09/18/23 *****************************
        // Variables for Replacement Data to pass in Error Message Description
        rdJobQ = *blanks;
        rdSbs  = *blanks;
        //***  End   Add ***** 09/18/23 *****************************
        erSbs  = *off;
        msSbs  = *blanks;
        erThrl = *off;
        msThrl = *blanks;
        erThrh = *off;
        msThrh = *blanks;

       // Federal Tax Id must be greater than 0
        If scFein <= *zeros;
           erFein = *on;
           msFein = 'WCR0502';
           isError = *on;
        Endif;

       // IRS Transmittal Code cannot be blank.
        If scTcc  = *blanks;
           erTcc  = *on;
           msTcc  = 'WCR0503';
           isError = *on;
        Endif;

       // Validate Type of Payment
        If scClck = 'Y' and scCtyp = *Blanks;
           erCtyp = *on;
           mgCtyp = 'Type of Payment required.';
           If clientID = '114';
              erCtyp2 = *on;
              mgCtyp2 = 'Type of Payment required.';
           Endif;
           isError = *on;
        Endif;

       // Enter complete info for Lock Box Name and address,
       // if any of the field is entered
        //*** Beg Chg ***** 04/26/23 *****************************
        //*** If scLbnm <> *blanks or scLad1 <> *blanks or scLad2 <> *blanks or
        //***  scLcty <> *blanks or scLst <> *blanks or scLzip <> 0;
        //***    If scLbnm = *Blanks;
        //***       erLbnm = *on;
        //***       msLbnm = 'WCR0503';
        //***       isError = *on;
        //***    Endif;
        If scLad1 <> *blanks or scLad2 <> *blanks or
         scLcty <> *blanks or scLst <> *blanks or scLzip <> 0;
        //*** End Chg ***** 04/26/23 *****************************
           If scLad1 = *Blanks;
              erLad1 = *on;
              msLad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scLcty = *blanks;
              erLcty = *on;
              msLcty = 'WCR0503';
              isError = *on;
           Endif;
           If scLst  = *blanks;
              erLst  = *on;
              msLst  = 'WCR0503';
              isError = *on;
           Endif;
           If scLzip = *zeros;
              erLzip = *on;
              msLzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;

       // Billing Logo must be valid page segment object if entered
        If scBlog <> *blanks;
           @@objname = scBlog;
           @@objtype = '*PAGSEG';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erBlog = *on;
              msBlog = 'WC30292';
              isError = *on;
           Endif;
        Endif;

       // Certification of Insurance Overlay must exist if entered
        If scCeov <> *blanks;
           @@objname = scCeov;
           @@objtype = '*OVL   ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erCeov = *on;
              mgCeov = 'Overlay object not found on system.';
              isError = *on;
           Endif;
        Endif;

       // Certification of Insurance Page segment must exist if entered
        If scSpsg <> *blanks;
           @@objname = scSpsg;
           @@objtype = '*PAGSEG';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erSpsg = *on;
              mgSpsg = 'Page segment not found on system.';
              isError = *on;
           Endif;
        Endif;

       // Folder must be valid if entered and user must have authority to folder
        If scFold <> *blanks;
           @@folder = scFold;
           @@rtcd = ' ';
           ValidFlr(@@folder:@@rtcd);
           If @@rtcd = '1';
              erFold = *on;
              msFold = 'WCR0826';
              isError = *on;
           Endif;
           If @@rtcd = '2';
              erFold2 = *on;
              msFold2 = 'SCM0517';
              isError = *on;
           Endif;
        Endif;

       // Document must be valid if entered
        If scDcid <> *blanks;
           @@docid  = scDcid;
           @@folder = scFold;
           @@rtcd = ' ';
           ValidDoc(@@docid:@@folder:@@rtcd);
           If @@rtcd = '1';
              erDcid = *on;
              msDcid = 'WCR0825';
              isError = *on;
           Endif;
        Endif;

       // Validate Output Logo File Name and Path
        If scSlogo <> *blanks;
           If %scan('.':scSLogo) = 0;
              erSlogo = *on;
              mgSlogo = 'Logo file name must have a file name extension.';
              isError = *on;
           Endif;
          // Image path must be entered.
           If scSpath = *blanks;
              erSpath = *on;
              mgSpath = 'Logo image file path must be entered.';
              isError = *on;
           Endif;
          // Path must begin and end with a '/' character.
           If scSpath <> *blanks;
              If %subst(scSpath:1:1) <> '/' OR
               %subst(scSpath:%len(%trim(scSpath)):1) <> '/';
                 erSpath = *on;
                 mgSpath = 'Logo image file path must begin and end ' +
                  'with a ''/''.';
                 isError = *on;
              Endif;
           Endif;
        Endif;

       // Validate CEU Fiscal Year Start Month
        If scFysm < 0 or scFysm > 12;
           erFysm = *on;
           mgFysm = 'Value must be 1 thru 12, if populated.';
           isError = *on;
        Endif;

       // Threshold values must be in ascending order, if entered.
        If scThRl > scThRh;
           erThrl = *on;
           msThrl = 'WCR0750';
           erThrh = *on;
           msThrh = 'WCR0750';
           isError = *on;
        Endif;

       // Check for valid JobQ name
        If scJobq <> *blanks;
           @@objname = scJobq;
           @@err = '0';
           CheckObjName(@@objname:@@err);
           If @@err = '1';
              erJobq = *on;
              msJobq = 'WCR3384';
              isError = *on;
           Endif;
        Endif;

       // Subsystem/JobQ are Mutually Inclusive
        If (scJobq <> *blanks and scSbs = *blanks)
         OR (scJobq = *blanks and scSbs <> *blanks);
           erJobq = *on;
           msJobq = 'WCR3381';
           erSbs  = *on;
           msSbs  = 'WCR3381';
           isError = *on;
        Endif;

       // Check existence of Subsystem entered
        If scSbs  <> *blanks;
           @@objname = scSbs ;
           @@objtype = '*SBSD  ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erSbs  = *on;
              msSbs  = 'WCR3380';
              isError = *on;
           Endif;
        Endif;

        //***  Begin Add ***** 09/18/23 *****************************
       // Retrieve Object Library
        ##lib = *blanks;
        If scSbs <> *blanks and erSbs = *off;
           ##obj = scSbs;
           ##type = '*SBSD  ';
           RtvObjLib(##obj:##type:##lib);
        Endif;
        //***  End   Add ***** 09/18/23 *****************************

       // Check whether JobQ already assigned to any other subsystem
        //***  Begin Chg ***** 09/18/23 *****************************
        //*** If scJobQ <> *blanks and scSbs <> *blanks;
        If scJobQ <> *blanks
         and erJobq = *off and erSbs = *off;
        //***  End   Chg ***** 09/18/23 *****************************
           @@objname = scJobQ;
           @@objtype = '*JOBQ  ';
           @@err = '0';
           //***  Begin Chg ***** 09/18/23 *****************************
           // CheckObj(@@objname:@@objtype:@@err);
           @@lib = ##lib;
           CheckObj2(@@lib:@@objname:@@objtype:@@err);
           //***  End   Chg ***** 09/18/23 *****************************
           If @@err = '0';
              @@jobq = scJobQ;
              //***  Begin Chg ***** 09/18/23 *****************************
              // @@jobqlib = 'QGPL';
              @@jobqlib = ##lib;
              //***  End   Chg ***** 09/18/23 *****************************
              @@jobqsts = ' ';
              @@sbs = ' ';
              @@nojobs = 0;
              RtvJobQA(@@jobq:@@jobqlib:@@jobqsts:@@sbs:@@nojobs);
              // If JobQ already assigned (returned subsystem different from screen)
              If scSbs <> @@Sbs;
                 //***  Begin Add ***** 09/18/23 *****************************
                 rdJobQ = @@Sbs;
                 //***  End   Add ***** 09/18/23 *****************************
                 erJobQ = *on;
                 msJobQ = 'WCR3385';
                 isError = *on;
              Endif;
           Endif;
        Endif;

        //***  Begin Add ***** 09/18/23 *****************************
        // Call Program to Create JobQ
        If isError = *off;
           // Do not create JobQ at Data Center Clients
           If suffix = '   ';
              outCo = %editc(scCo#:'X');
              outFnd# = %editc(scFnd:'X');
              outJobQ = F2jobq;
              outName = scJobq;
              outSbs  = F2sbs;
              outSbs1 = scSbs;
              outMsg  = ' ';
              outRtcd = ' ';
              crtJobQ(outCo:outFnd#:outJobQ:outName:outSbs:outSbs1:outMsg:
               outRtcd);
              If outRtcd <> ' ';
                 rdJobQ  = outMsg;
                 erJobQ  = *On;
                 msJobQ  = 'WCR3382';
                 isError = *On;
              Endif;
           Endif;
        Endif;

        //***  End   Add ***** 09/18/23 *****************************


       // Validate WMGCP file fields - Group Carrier Information
        erDcar = *off;
        msDcar = *blanks;
        erFrcn = *off;
        msFrcn = *blanks;
        erTocn = *off;
        msTocn = *blanks;

       // Default Carrier Code cannot be blank.
        If scDcar = *blanks;
           erDcar = *on;
           msDcar = 'WCR0503';
           isError = *on;
        Endif;

       // Valid Carrier Number Range must be in ascending order, if entered
        If scFrcn > scTocn;
           erFrcn = *on;
           msFrcn = 'WCR0750';
           erTocn = *on;
           msTocn = 'WCR0750';
           isError = *on;
        Endif;

      // Validate Claim Reserve Code fields
       // Check for Duplicate Claim Reserve Code
       For i = 1 to 9;
          For j = 2 to 10;
             If i<>j and dsScClmRsv(i)<>*blanks and dsScClmRsv(i)=dsScClmRsv(j);
                dsErClmRsv(i) = *on;
                dsErClmRsv(j) = *on;
                dsMsClmRsv(i) = 'WC30122';
                dsMsClmRsv(j) = 'WC30122';
                isError = *on;
             Endif;
          Endfor;
       Endfor;


       // If No Error, for saving in the database file
      // Re-arrange Claim Reserve Codes to Eliminate Blank code
       If isError = *off;
          For i = 1 to 9;
             For j = i+1 to 10;
                If dsScClmRsv(i) = *blanks and dsScClmRsv(j) <> *blanks;
                   dsScClmRsv(i) = dsScClmRsv(j);
                   dsScClmRsv(j) = *blanks;
                Endif;
             Endfor;
          Endfor;
       Endif;

















      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateGroup   e

      //===================================================================
      // Validate the Group Details addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateGrpDtl  b
     d ValidateGrpDtl  pi              n
     d strmode                        1
     d isError2        s               n   inz
      /Free

        isError2 = *off;
       // Initialize all the messageId and error indicators.
        erFyr  = *off;
        msFyr  = *blanks;
        erStrDt = *off;
        msStrDt = *blanks;
        erEndDt = *off;
        msEndDt = *blanks;
        erDps  = *off;
        msDps  = *blanks;
        erNetD = *off;
        msNetD = *blanks;
        erScdt = *off;
        msScdt = *blanks;

        If strmode <> 'C';
          // Policy Year must be > 0.
           If scFyr <= *zero;
              erFyr = *on;
              msFyr = 'WCR0502';
              isError2 = *on;
           Endif;
          // Duplicate Policy Year
           If scFyr <> *zero;
              chain keyWDF1L pf101;
              If %found(wdf1l);
                 erFyr = *on;
                 msFyr = 'WC30004';
                 isError2 = *on;
              Endif;
           Endif;
        Endif;

       // Validation Check for Policy Year Start Date
        erStrDt = ValidDate(scStrDt:charMsg);
        If erStrDt = *on;
           msStrDt = charMsg;
           isError2 = *on;
        Endif;
       // Validation Check for Policy Year End Date
        If scEndDt <> 0;
           erEndDt = ValidDate(scEndDt:charMsg);
           If erEndDt = *on;
              msEndDt = charMsg;
              isError2 = *on;
           Endif;
        Endif;
       // Dates must be in Ascending order
        If scStrDt <> 0 and scEndDt <> 0;
           If erEndDt = *off and erStrDt = *off;
              erEndDt = AscendDates(scStrDt:scEndDt:charMsg);
              If erEndDt = *on;
                 erStrDt = *on;
                 msEndDt = charMsg;
                 msStrDt = charMsg;
                 isError2 = *on;
              Endif;
           Endif;
        Endif;
       // Policy Year Start Date Must Greater than or Equal Group Effective Date
        If scStrDt <> 0 and erStrDt = *off;
           If dtCYMD(scStrDt) < dtCYMD(scEfDta);
              erStrDt = *on;
              msStrDt = 'WC31479';
              isError2 = *on;
           Endif;
        Endif;
       // Policy Year Date Range Must not Overlap with other policy years
        If scStrDt <> 0 and scEndDt <> 0;
           If erEndDt = *off and erStrDt = *off;
              Setll (scCo#:scFnd) pf101;
              Reade (scCo#:scFnd) pf101;
              Dow Not %Eof(wdf1l);
                 If scFyr <> F1fyr and
                  dtCYMD(scStrDt) < F1enpo and
                  dtCYMD(scEndDt) > F1stpo;
                    erEndDt = *on;
                    erStrDt = *on;
                    msEndDt = 'WC31414';
                    msStrDt = 'WC31414';
                    isError2 = *on;
                 Endif;
                 Reade (scCo#:scFnd) pf101;
              Enddo;
           Endif;
        Endif;

       // Validate Deposite Style.
        If scDps = *Blanks;
           erDps = *on;
           msDps = 'WC30341';
           isError2 = *on;
        Endif;

       // Validation Check for Starting with Policies Effective Date
        If scNetD <> 0;
           erNetD = ValidDate(scNetD:charMsg);
           If erNetD = *on;
              msNetD = charMsg;
              isError2 = *on;
           Endif;
        Endif;
       // Starting with Policies Effective Date must be within Policy Year Dates
        If scNetD <> 0 and erNetD = *off;
           If dtCYMD(scNetD) < dtCYMD(scStrDt) OR
            dtCYMD(scNetD) > dtCYMD(scEndDt);
              erNetD = *on;
              msNetD = 'WCR1145';
              isError2 = *on;
           Endif;
        Endif;

       // Validation Check for Stop Collateral Effective Date
        If scScDt <> 0;
           erScDt = ValidDate(scScDt:charMsg);
           If erScDt = *on;
              msScDt = charMsg;
              isError2 = *on;
           Endif;
        Endif;


      // Return *on indicator when No error exists.
        If isError2 = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateGrpDtl  e

      // utility procedure definitions
      /COPY SCOPYCOMPI

