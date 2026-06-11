     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTRFCHKREQ- Work with Return Fund Checks Request
     h dftname(WTRFCHKREQ)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
     h bnddir('WPIAGENT')
      //*********************************************************************
      //  Created   : 10/16/23                                         *
      //  Programmer: C:342                                            *
      //  Project   : WC2676 - Redesign of Work with Return Fund Checks*
      //              Request.                                         *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      // REVISIONS:                                                    *
      //                                                               *
      // 04/30/24 C:342 Project WC2676 Support:115521                  *
      //                1.Monitor program statement when premchk data  *
      //                  area is blank.                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fwtrfchkreqcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRFChkR:scSflRRNR)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Return Premium Check Request File
     fwar4p     uf a e             disk    rename(pr401:pr40p)
     f                                     infds(dbfds)
     fwar4l12   uf   e           k disk    rename(pr401:pr412)
     fwar4l2    if   e           k disk    rename(pr401 : pr4l2) prefix(l2)
      * MICR Bank Information File (by Fund)
     fwmbil7    if   e           k disk    rename(pbi01 : pbi07)
      * Bank Account Master file
     fwsckl     if   e           k disk    rename(pck01 : pck00)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Employer Master file
     fwmeml     if   e           k disk    rename(pem01:pem00)
     f                                     infds(dbfds1)
      * Employer Detail file
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds2)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)

      * Agency Billing Detail File
     fwdabl     if   e           k disk    rename(pab01:pab00)
     f                                     infds(dbfds4)
      * Agent Master File
     fwmagl     if   e           k disk    rename(pag01:pag00)
      * Agent Assignment File
     fwmaal1    if   e           k disk    rename(paa01:paa01)
      * Fund master File
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      * Premium Finance file
     fwmpfl     if   e           k disk    rename(ppf01:ppf00)
      * User Parameters File
     fwmupl     if   e           k disk    rename(pup01:pup00)

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0

      // work variables
     d saveDT          s                   like(R4date)
     d saveTM          s                   like(R4time)
     d svPTFL          s                   like(R4ptfl)
     d svPfin          s                   like(R4pfin)
     d svACFL          s                   like(R4acfl)
     d svAgt#          s                   like(R4agt#)
     d svPTN2          s                   like(R4ptn2)
     d svPCty          s                   like(R4pCty)
     d svPSt           s                   like(R4pst)
     d svpZip          s                   like(R4pzip)
     d SavChoices      s                   like(SelChoices)
     d SavValues       s                   like(SelValues)
     d inInclude       s             10    inz
     d flgRcdUpdated   s              1    inz
     d intCo#          s              3  0
     d @@mode          s              1
     d indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d agencyBill      s               n   inz(*off)
     d isnotIndpdt     s               n   inz(*off)
     d charMsg         s            100
     d isDelResp       s               n   inz
     d svsflrrn        s              5  0 inz
     d intZip          s              9  0 inz(*zeros)

     d                 ds
     d  Acct                   1    120
     d                                     dim(6)
     d   wdAcct1               1     20
     d   wdAcct2              21     40
     d   wdAcct3              41     60
     d   wdAcct4              61     80
     d   wdAcct5              81    100
     d   wdAcct6             101    120

     d lo              c                   'abcdefghijklmnopqrs-
     d                                     tuvwxyz'
     d up              c                   'ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ'

     d xxagt#          s                   like(ABagt#)
     d x5stdt          s                   like(ABstdt)

      // Work variable for Print Checks
     d strmsg1         s            200    inz
     d strmsg2         s            200    inz
     d strmsg3         s            200    inz
     d strmessage      s            256    inz
     d svfnd           S                   like(R4fnd)
     d svfyr           S                   like(R4fyr)
     d svbtyp          S                   like(R4btyp)
     d isUseScript     s              1n   inz
     d processType     c                   const('Return Premium Checks')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d xxProcChkPrt    s               n   inz
     d bad             s               n
     d premchk         s             80    dtaara
     d premchkds       ds
     d  premchkdt              1      8  0 inz

     d svLda           ds          1024    DtaAra(*lda)
     d xxDart                         1    overlay(svlda:251)
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

      // Return Fund Check Print - Dataarea creation
     d ##kc            s              1
     d ##kl            s              1

      // For Current Account WA002R.
     d  ##Co#          s              3  0
     d  ##Fnd          s              3  0
     d  ##Emp#         s              9  0
     d  ##Div          s              5  0
     d  ##Fyr          s              3  0
     d  ##Agent#       s              5  0
     d  ##AgentNm      s             60
     d  ##Prod#        s              5  0
     d  ##ProdNm       s             60
     d  ##Area         s              3  0
     d  ##Phone        s              7  0
     d  ##Plan         s              1
     d  ##Date         s              8  0

      // For Current Account W6502C.
     d  @co#           s              3
     d  @ffnd          s              3
     d  @tfnd          s              3
     d  @prdt          s              8
     d  @btyp          s              1

      // For WA023R1 and WA023R.
     d #1mode          s              3
     d #1co#           s              3
     d #1fnd           s              3
     d #1fyr           s              3
     d #1case          s              7
     d #1appl          s              2
     d #1data          s             75
     d #1cktp          s             10
     d #1rtcd          s              1

     d #1prdt          s              8
     d #1acct          s             20
     d #1call          s              1
     d #1hdcy          s              1
     d #1chkq          s             10
     d #1ftyp          s             10
     d #1efdt          s              7
     d #1cpyq          s             10
     d ##rtcd          s              1
     d ##cktp          s             10

      // For W0503C21
     d inco#           s              3
     d inPrtRpt        s              1
     d inOutQ          s             10
     d inBtypPrtR      s              1

      * WA023R1 call - API for determining Bank Code
     d BankCode        pr                  Extpgm('WA023R1')
     d #1mode                         3
     d #1co#                          3
     d #1fnd                          3
     d #1fyr                          3
     d #1case                         7
     d #1appl                         2
     d #1data                        75
     d #1cktp                        10
     d #1rtcd                         1

      * chkSchedule - Check Schedule
     d chkSchedule     pr              n

      * DtaCrtRFCP - Return Fund Check Print - Dataarea creation
     d DtaCrtRFCP      pr                  Extpgm('W6501C')
     d ##kc                           1
     d ##kl                           1

      * WA002R Call - API to gather Agent and Producer information
     d GetAgnPrInfo    pr                  Extpgm('WA002R')
     d  ##Co#                         3  0
     d  ##Fnd                         3  0
     d  ##Emp#                        9  0
     d  ##Div                         5  0
     d  ##Fyr                         3  0
     d  ##Agent#                      5  0
     d  ##AgentNm                    60
     d  ##Prod#                       5  0
     d  ##ProdNm                     60
     d  ##Area                        3  0
     d  ##Phone                       7  0
     d  ##Plan                        1    Const
     d  ##Date                        8  0

      * WA023R call - API - WMBIP: Obtain Account #
     d GetBankAcct     pr                  Extpgm('WA023R')
     d #1co#                          3
     d #1fnd                          3
     d #1appl                         2
     d #1prdt                         8
     d #1acct                        20
     d #1call                         1
     d #1hdcy                         1
     d #1chkq                        10
     d #1ftyp                        10
     d ##rtcd                         1
     d #1efdt                         7
     d #1cpyq                        10
     d ##cktp                        10

      // Load Return Fund Check Requests
     d LoadRFCheckReq  pr
     d inIncludeRFCR                 10    const

      * Main Processing
     d Main            pr

      * Process Return Fund Check Print (Write Checks Button)
     d ProcRFCPrt      pr

      * Process Return Fund Check Report (Print Report Button)
     d ProcRFCRpt      pr

      * Process other Options for Return Funds Check Requests
     d ProcOther       pr

      * Process Performed Action from the action dropdown
     d ProcPerAct      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      * retPremSetup = returns *on if A/R codes not set up in Bank Account Master
     d retPremSetUp    pr                  like(bad)

      * W6502C Call - Submitter - Return Premium Check Print
     d SubRPCPrt       pr                  Extpgm('W6502C')
     d  @co#                          3
     d  @ffnd                         3
     d  @tfnd                         3
     d  @prdt                         8
     d  @btyp                         1

      * W0503C21 call -  Submitter - Checks to be Written Report
     d SubRFCRpt       pr                  Extpgm('W0503C21')
     d inco#                          3
     d inPrtRpt                       1
     d inOutQ                        10
     d inBtypPrtR                     1

     * Validate Return Fund Check Request
     d ValidateRFCReq  pr              n

     * Validate Return Fund Check Print
     d ValidateRFCPrt  pr              n

     * Validate Return Fund Check Report
     d ValidateRFCRpt  pr              n

     *Output Control
     d OutputControl   pr

     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy wPrPolErr
     d/copy sprCompany
     d/copy wprAgent
     d/copy wprBank
     d/copy sprLetter
     d/copy sprXml

     c     UPkey         klist
     c                   kfld                    q1user

     c     keyWdabl      klist
     c                   kfld                    R4co#
     c                   kfld                    x5stdt
     c                   kfld                    xxagt#

     c     keyWmagl      klist
     c                   kfld                    R4co#
     c                   kfld                    R4fein
     c                   kfld                    R4mod

     c     keyWmaal1     klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    svagt#

     c     keyWmagl2     klist
     c                   kfld                    AAco#
     c                   kfld                    AAfein
     c                   kfld                    AAmod

     c     keyWmeml      klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    R4emp#
     c                   kfld                    R4div

     c     keyWdell      klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    R4emp#
     c                   kfld                    R4div
     c                   kfld                    R4fyr

     c     keyWmfnl      klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd

     c     keyWmpfl      klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    svpfin

     c     keyWmpfl2     klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    svpfin
     c                   kfld                    ELpfdt
      *================================================================

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index


      /free
       Exec Sql Set Option Commit = *None;

       ProgramStart(process);

       If process;
          Main();
       Endif;

       *InLr = *On;
      /end-free

       //===================================================================
       // ChkSchedule - Check Schedule
       //===================================================================
     p ChkSchedule     b
     d ChkSchedule     pi              n
     d isError         s               n   inz

      /free
       bad = *on;
       isError = *off;
       If retPremSetup = bad;
          strmessage = *blanks;
          strmessage = 'A/R Codes are not Found For The Following ' +
                       'Bank Account/s. Access The Bank Account Master ' +
                       'to Enter A/R Codes -';
          If wdAcct1 <> *blanks;
             strmessage = %trim(strmessage) + ' ' + %trim(wdAcct1);
          Endif;
          If wdAcct2 <> *blanks;
             strmessage = %trim(strmessage) + ', ' + %trim(wdAcct2);
          Endif;
          If wdAcct3 <> *blanks;
             strmessage = %trim(strmessage) + ', ' + %trim(wdAcct3);
          Endif;
          If wdAcct4 <> *blanks;
             strmessage = %trim(strmessage) + ', ' + %trim(wdAcct4);
          Endif;
          If wdAcct5 <> *blanks;
             strmessage = %trim(strmessage) + ', ' + %trim(wdAcct5);
          Endif;
          If wdAcct6 <> *blanks;
             strmessage = %trim(strmessage) + ', ' + %trim(wdAcct6);
          Endif;
          strmessage = %trim(strmessage) + '.';
          PopOneBtn(strmessage:'Error':'Ok');
          isError = *on;
       Endif;

       // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /end-free
     p ChkSchedule     e

       //===================================================================
       // Load Return Fund Check Requests Grid
       //===================================================================
     p LoadRFCheckReq  b
     d LoadRFCheckReq  pi
     d inIncludeRFCR                 10    const

      /free
       // Clear the subfile
       scSflRRNR = 0;
       scSflDspR = *off;
       scSflClrR = *on;
       Write rcdRFChkR;
       scSflClrR = *off;
       scSflDspR = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll *start war4p;
       Read(n) war4p;
       Dow not %eof(war4p);
          If R4chk# = *zeros;

             scSflRRNR += 1;
             scSflSelR = *off;
 
             If svsflrrn = 0;
                If scSflRRNR = 1;
                   scSflSelR = *on;
                Endif;
             Else;
                If scSflRRNR = svsflrrn;
                   scSflSelR = *on;
                Endif;
             Endif;
 
             scSelc   = R4selc;
             scPayNm  = R4ptnm;

             select;
             when R4ptfl  = 'A';
                scPayTyp  = 'Agent';
             when R4ptfl  = 'E';
                scPayTyp  = 'Employer';
             when R4ptfl  = 'G';
                scPayTyp  = 'Group';
             when R4ptfl  = 'P';
                scPayTyp  = 'Premium Finance Company';
             other;
                scPayTyp  = *blanks;
             endsl;

             scChkAmt = R4amt;

             select;
             when R4btyp = 'A';
                scBType  = 'Account Current';
             when R4btyp = 'D';
                scBType  = 'Deposit';
             when R4btyp = 'I';
                scBType  = 'Independent Cash Distribution';
             when R4btyp = 'P';
                scBType  = 'Premium';
             endsl;

             // Select Checkbox
             If inIncludeRFCR = 'Include';
                scSelect = 'Y';
                scnxtchg = *on;
             Else;
                scSelect = 'N';
                scnxtchg = *off;
             Endif;

             scRRNChkR = dbrrn;

             Write sflRFChkR;
          Endif;
          Read(n) war4p;
       Enddo;

       Clear inInclude;
       scSflRRNR = 1;
       If svSflRRN <> 0;
          scSflRRNR = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadRFCheckReq  e


       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       LoadActionBar();
       BtnCancel = *off;

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load Return Funds Check Request Grid
          LoadRFCheckReq(inInclude);

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             inInclude = *blanks;
             svSflRRN = 0;

          // Save
          When btnSave = *On;
             btnSave = *Off;

          // Include All
          When btnRFCIncl = *on;
             btnRFCIncl = *off;
             inInclude = 'Include';

          // Exclude All
          When btnRFCExcl = *on;
             btnRFCExcl = *off;
             inInclude = ' ';

          // Perform Action
          When btnPerAct = *on;
             btnPerAct = *off;
             ProcPerAct();

          // Process Return Fund Check Print
          When BtnRFCPrt = *On;
             BtnRFCPrt = *Off;
             If ChkSchedule();           // Check schedule
                ProcRFCPrt();            // Process Return Fund Check Print (Write Checks Button)
             Endif;

          // Process Print Report for Return Fund Check
          When BtnRFCRpt = *On;
             BtnRFCRpt = *Off;
             ProcRFCRpt();            // Process Return Fund Check Report (Print Report Button)

          Other;
             ProcOther();             // Process other Options for Return Funds Check Requests

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        indexTab = TabIndex + 1;

        // Initialize Screen fields
        scSelAct = '--- Select List Action ---';
        Select;
        When indexTab = 1;
           pnlHeight = 540;
           APnlHeight = pnlHeight + 60;
           Write rcdComPnl;
           Write rcdPanel;
           Exfmt rcdRFChkR;
           Read rcdComPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // OutputControl - Output Control Processing
       //===================================================================
     p OutputControl   b
     d OutputControl   pi

      /Free

       If isUseScript;

          xxProcType = processType;
          xxMass     = 'Y';

          In svLda;

          Clear svLDA;

          xxDart = 'N';
          xxRMCD = ' ';

          Out svLda;

          xxSubmit = OutputWindow(xxProcType:xxMass);

       Else;
          xxSubmit = *On;
       Endif;

      /End-Free

     p OutputControl   e

       //===================================================================
       // ProcOther - Process other option for Return Fund Check Requests
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /Free

       Clear RcdRFCReq;
       isexit = *off;
       ReadC sflRFChkR;
       Dow not %eof;
          Select;

          // Process the Edit option for Return Fund Check Requests
          When scRFCREdt = *on;
             scRFCREdt = *off;
             svsflrrn = scSflrrnR;  //save rrn value
             Chain(n) scRRNChkR pr40p;
             SetVisRFCReq();
             Exsr Mvdbsc;
             scRFCRqHdg = 'Edit Return Fund Check Request Information';
             Dow isexit = *off;
                Exfmt RcdRFCReq;

                Select;
                When BtnSavRFCR = *on;
                   BtnSavRFCR = *off;
                   // Check if WAR4P updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('WAR4P':dbrrn:'R4date':'R4time':
                    'R4user':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRNChkR pr40p;
                      Exsr Mvdbsc;
                   Endif;
                   If flgRcdUpdated = '0'; // No error
                      // Validate the Return Fund Check Request
                      If ValidateRFCReq();
                         Chain scRRNChkR pr40p;
                         Exsr MVscdb;
                         update pr40p;
                         isExit = *on;
                      Endif;
                   Endif;
                When BtnCanRFCR = *on;
                   BtnCanRFCR = *off;
                   isExit = *on;
                EndSl;
             Enddo;


          // Process Delete option for Return Fund Check Requests
          When scRFCRDlt = *on;
             scRFCRDlt = *off;
             svsflrrn = scSflrrnR;  //save rrn value
             Chain(n) scRRNChkR pr40p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             SetVisRFCReq();
             Exsr MvDbSc;
             scRFCRqHdg = 'Delete Return Fund Check Request';
             Dow isexit = *off;
                Write RcdRFCReq;

                isDelresp = PopTwoBtn('Do you want to delete this Return Fund  +
                Check Request?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Chain scRRNChkR pr40p;
                   Delete pr40p;
                   PopOneBtn('Return Fund Check Request Deleted!':
                      'Delete':'Ok');
                   isexit = *on;
                   svsflrrn = *zeros;
                When isDelresp = *Off;
                   svsflrrn = scSflrrnR;  //save rrn value
                   isexit = *on;
                EndSl;

             Enddo;

          Endsl;

          Unlock War4p;

          ReadC sflRFChkR;
       Enddo;

       // Move data base fields to screen fields
       Begsr MVdbsc;
        svacfl = R4acfl;
        svAgt# = R4agt#;
        svpfin = R4pfin;
        svPTFL = R4ptfl;

        Select;
        When R4btyp = 'A';
           scBTyp = 'Account Current';
        When R4btyp = 'D';
           scBTyp = 'Deposit';
        When R4btyp = 'I';
           scBTyp = 'Independent Cash Distribution';
        When R4btyp = 'P';
           scBTyp = 'Premium';
        Other;
           scBTyp = *blanks;
        endsl;

        scCust = R4cust;
        scFEIN = R4fein;
        scMod  = R4mod;

        // Agent Name
        Chain keyWmagl pag00;
        If %found(wmagl);
           scAgtN = AGname;
        Endif;

        scFND  = R4fnd;

        // Fund Name
        Chain keyWmfnl pfn00;
        If %found(wmfnl);
           scFNDN = FNname;
        Endif;

        scEMP# = R4emp#;
        scDIV  = R4div;
        scFYR  = R4fyr;
        // Employer name
        If R4emp# <> *zeros;
           Chain keyWmeml pem00;
           If %found(wmeml);
              scEmpNm = EMname;
           Endif;
        Endif;
        scAmt  = R4amt;
        scCHK# = R4chk#;
        scStDt = dt6MDYY(R4stdt);

        // Premium Finance Company Name
        If R4pfin <> *blanks;
           chain keywdell pel00;
           If %found(wdell);
              // Get Premium finance compnay name.
              scPFName = R4pfin;
              setgt keyWmpfl2 wmpfl;
              readpe keyWmpfl wmpfl;
              If not %eof(wmpfl);
                 scPFName = %trim(R4pfin) + ' - '  + %trim(PFname);
              Endif;
           Endif;
        Endif;

        scARCd = R4arcd;
        scPTFL = R4ptfl;
        scPTNm = R4ptnm;
        scPAD1 = R4pad1;
        scPAD2 = R4pad2;
        Monitor;
           intZip = %int(R4pzip);
        On-error;
        Endmon;
        scPAD3 = %Trim(R4pcty) + ', ' + R4pst + ' ' +
         %Subst(%Editc(intZip:'X'):1:5);
        If %Subst(%Editc(intZip:'X'):6:4) <> *Blanks;
           scPAD3 = %Trim(scPAD3) + '-' +
            %Subst(%Editc(intZip:'X'):6:4);
        Endif;
        If scPAD2 = *Blanks;
           scPAD2 = scPAD3;
           scPAD3 = *Blanks;
        Endif;
        If R4pzip = *zeros;
           scPAD1 = *Blanks;
           scPAD2 = *Blanks;
           scPAD3 = *Blanks;
        Endif;

        scCom1 = R4com1;
        scCom2 = R4com2;
        scCom3 = R4com3;
        saveTM = R4time;
        saveDT = R4date;
       Endsr;

       // Move screen fields to date base file.
       Begsr MVscdb;
        R4ptfl = scPTfl;
        R4pfin = svPFin;
        R4ptnm = scPTNm;
        R4ptn2 = svptn2;

        R4com1 = sccom1;
        R4com2 = sccom2;
        R4com3 = sccom3;
        R4stdt = dt5CYMD(scSTDT);
        R4cust = scCust;
        R4agt# = svAgt#;
        R4pad1 = scpad1;
        R4pad2 = scpad2;
        R4pcty = svPCty;
        R4pst  = svpST;
        R4pzip = svpZip;
        R4pgm  = q1pgm;
        R4user = q1user;
        R4date = Dateto7(SystemDate());
        R4time = Timeto6(SystemTime());
       Endsr;

      /End-Free
     p ProcOther       e

       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi

      /Free

       ReadC sflRFChkR;
       Dow not %eof;
          If scSelect = 'Y';

             Select;
             When scSelAct = 'Change Check Release Status';
                Chain scRRNChkR pr40p;
                If R4selc = 'Y';
                   R4selc = 'N';
                Else;
                   R4selc = 'Y';
                Endif;
                R4pgm  = q1pgm;
                R4user = q1user;
                R4date = Dateto7(SystemDate());
                R4time = Timeto6(SystemTime());
                update pr40p;
             Endsl;
          Endif;

          ReadC sflRFChkR;
       Enddo;

      /End-Free
     p ProcPerAct      e

       //===================================================================
       // ProcRFCPrt - Process Return Fund Check Print (Write Checks Button)
       //===================================================================
     p ProcRFCPrt      b
     d ProcRFCPrt      pi
     d  isexit         s               n   Inz(*off)
     d  btnEnter       s              1n   inz
     d  process        s              1n   inz(*on)

      /Free

       clear rcdRFCPrt;
       SelChoices = SavChoices;
       SelValues  = SavValues;
       isexit = *off;
       Exsr SetInfo;
       Dow isexit = *off;
          //scProcDt = DateTo8(systemDate());
          Exfmt rcdRFCPrt;
          Select;
          // Process
          When BtnSubmit = *on;
             BtnSubmit = *off;
             If ValidateRFCPrt();
                Exsr Mvscdb;
                isexit = *on;
             Endif;
          When BtnChkPCan = *on;
             BtnChkPCan = *off;
             isexit = *on;
          Endsl;
       Enddo;

       Begsr SetInfo;
        isUseScript = UseScriptura(processType);

        Chain q1user pup00;
        If not %found(wmupl);
           UPco# = *zeros;
        Endif;
        In *lock PremChk;
        //*** Beg Chg *** 04/30/24 *******************************************
        //*** PremChkDt = %int(%Subst(PremChk:1:8));
        Monitor;
           PremChkDt = %int(%Subst(PremChk:1:8));
        On-Error;
           PremChkDt = 0;
        EndMon;
        //*** End Chg *** 04/30/24 *******************************************
        Out PremChk;

        If PremChkDt <> *zeros;
           xxProcChkPrt = PopTwoBtn('Previous Return Fund Check Write +
              Job for this Process Date Ended Abnormally. Press +
              Enter to Continue Processing for this Date. If not +
              Press Cancel, Change the Date and Press +
              Enter':'Return Fund Process - Information Window':'Enter'
              :'Cancel');
           scProcDt = PremChkDt;
           Select;
           When xxProcChkPrt = *On;
           When xxProcChkPrt = *Off;
              In *lock PremChk;
              PremChk = *zeros;
              PremChkDt = *zeros;
              Out PremChk;
              scProcDt = DateTo8(systemDate());
           Endsl;
        Else;
           scProcDt = DateTo8(systemDate());
        Endif;
       Endsr;

       Begsr Mvscdb;
       //Start Reading WAR4L12 in co#/fnd/fyr/Batch type order
        Read pr412;
        Dow not %eof(war4l12) and process = *on;
           If R4selc = 'Y';
              If svfnd <> R4fnd or svfyr <> R4fyr
                     or svbtyp <> R4btyp;
                 If R4btyp = 'A' or R4btyp = 'D'
                    or R4btyp = 'P';
                    #1Appl = 'RP';
                 Elseif R4btyp = 'I';
                    #1Appl = 'IC';
                 Endif;

                 #1co# = %Editc(R4co#:'X');
                 #1fnd = %Editc(R4fnd:'X');
                 #1fyr = %Editc(R4fyr:'X');

                 #1mode  = '2';
                 BankCode(#1mode:#1co#: #1fnd: #1fyr: #1case: #1appl:
                         #1data:#1cktp: #1rtcd);
                 If #1rtcd  <> '1';
                    #1call = 'C';
                    ##rtcd = *blank;
                    #1prdt = %Editc(scProcDt:'X');
                    ##cktp = #1cktp;
                    GetBankAcct(#1co#:#1fnd:#1appl:#1prdt:#1acct:#1call:#1hdcy:
                                #1chkq:#1ftyp: ##rtcd:#1efdt:#1cpyq:##cktp);
                 Endif;

                 strmsg1 = 'With a Check Type of ' + ##cktp;
                 strmsg2 = 'Return Fund Bank Information +
                    is Not Set Up for Group ' + #1fnd;
                 strmsg3 = strmsg2 + strmsg1;

                 If ##cktp <> *blank;
                    If (#1rtcd <> *blank or ##rtcd <> *blank);
                       btnEnter = PoptwoBtn(strmsg3 :'Return Fund Process':
                          'Enter':'Cancel');
                       If btnEnter = *off;
                          process = *off;
                       Endif;
                    Endif;
                 Else;
                    If (#1rtcd <> *blank or ##rtcd <> *blank);
                       btnEnter = PoptwoBtn(strmsg2 :'Return Fund Process':
                          'Enter':'Cancel');
                       If btnEnter = *off;
                          process = *off;
                       Endif;
                    Endif;
                 Endif;
              Endif;                                    // svfnd <> R4fnd

              If (#1rtcd = *blank and ##rtcd = *blank);
                 Select;
                 When scSrt1 = 'EN';
                    R4sort = %Editc(R4fnd:'X') + %Editc(R4emp#:'X')
                             + %Editc(R4div:'X') + %Editc(R4fyr:'X');

                 When scSrt1 = 'CU' and scSrt2 = 'EN';
                    R4sort = %trim(R4cust) + %Editc(R4fnd:'X') +
                             %Editc(R4emp#:'X') + %Editc(R4div:'X') +
                             %Editc(R4fyr:'X');

                 When scSrt1 = 'CU' and scSrt2 = 'AN';
                    R4sort = %trim(R4cust) + %Editc(R4agt#:'X');

                 When scSrt1 = 'AN' and scSrt2 = 'EN';
                    R4sort = %Editc(R4agt#:'X') + %Editc(R4emp#:'X') +
                             %Editc(R4div:'X') + %Editc(R4fyr:'X');

                 When scSrt1 = 'AN' and scSrt2 = 'CU';
                    R4sort = R4sort = %Editc(R4agt#:'X') + R4cust;
                 Endsl;

                 R4pgm  = q1pgm;
                 R4user = q1user;
                 R4date = Dateto7(SystemDate());
                 R4time = Timeto6(SystemTime());
                 update pr412;
                 svfnd = R4fnd;
                 svfyr = R4fyr;
                 svbtyp = R4btyp;
              Endif;
           Endif;

           Read pr412;
        Enddo;

        OutputControl();
        If xxSubmit = *On;
           @co#  = %EditC(intCo#:'X');
           @ffnd = %EditC(scfGrp:'X');
           @tfnd = %EditC(sctGrp:'X');
           @prdt = %EditC(scProcdt:'X');
           @Btyp = scBtypPrt;
           SubRPCPrt(@co#:@ffnd:@tfnd:@prdt:@Btyp);
        Endif;
       Endsr;

      /End-Free
     p ProcRFCPrt      e

       //===================================================================
       // ProcRFCRpt - Process Return Fund Check Report (Print Report Button)
       //===================================================================
     p ProcRFCRpt      b
     d ProcRFCRpt      pi
     d  isexit         s               n   Inz(*off)

      /Free

       clear rcdRFCRpt;
       SelChoices = SavChoices;
       SelValues  = SavValues;
       isexit = *off;
       Dow isexit = *off;
          Exfmt rcdRFCRpt;
          Select;
          // Process
          When BtnPRSub = *on;
             BtnPRSub = *off;
             If ValidateRFCRpt();
                Exsr Chk_Prompt;
                isexit = *on;
             Endif;
          When BtnPRCan = *on;
             BtnPRCan = *off;
             isexit = *on;
          Endsl;
       Enddo;

       Begsr Chk_Prompt;
        inco# = %char(upco#);
        inPrtRpt = scPrtRpt;
        inoutq = UPoutq;
        inBtypPrtR = scBtypPrtR;
        SubRFCRpt(inco#:inPrtRpt:inoutq:inBtypPrtR);
       Endsr;

      /End-Free
     p ProcRFCRpt      e

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
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;

       // Retrieve user parameter
       Chain q1user pup00;

       // Get Flags value from Workers Comp control file (SWCPF)
       Chain 'WC' SWC01;

       // Check if Return Fund Module is Installed or Not
       If WCfl25 <> 'Y';
          PopOneBtn('The Return Fund Module has not been Installed. Please -
           contact Sapiens for more Information.':'Return Fund Process');

          processMe = *off;
          Return;
       Endif;

       // Set Choices for Batch type
       If WCfl36 = 'Y';
          SavChoices = ',Account Current,Deposit,+
                        Independent Cash Distribution,Premium';
          SavValues  = ',A,D,I,P';
       Else;
          SavChoices = ',Account Current,Premium';
          SavValues  = ',A,P';
       Endif;

        // Choices for Action Dropdown for Return fund check
       If @@mode = 'D';
          choSelAct = ' ';
       Else;
          ChoSelAct = '--- Select List Action ---';
          ChoSelAct = %trim(ChoSelAct) + ',Change Check Release Status';
       Endif;
       scSelAct = '--- Select List Action ---';

       processMe = *on;

       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // retPremSetup = returns *on if A/R codes not set up in Bank Account Master
       //===================================================================
     p retPremSetup    b
     d retPremSetup    pi                  like(bad)

      /free
     d setUpFlag       s                   like(bad)
     d svfnd           s                   like(L2R4FND) inz(0)
     d svappl          s                   like(BIAPPL)  inz('RP')
     d sysDate7        s                   like(BIefdt)
     d ctr             s              1  0 inz(0)
     d i               s              3  0 inz(0)
     d ckRDCodes       s               n   inz(*off)
     d ckRPCodes       s               n   inz(*off)

     c     l2ky          klist
     c                   kfld                    intco#
     c                   kfld                    svfnd

     c     biky          klist
     c                   kfld                    intco#
     c                   kfld                    svappl
     c                   kfld                    svfnd


     c     ckky          klist
     c                   kfld                    intco#
     c                   kfld                    BIACCT

        sysDate7  = Dateto7(SystemDate());
        setUpFlag = *off;
        setll intco# pr4l2;
        reade intco# pr4l2;
        dow not %eof(war4l2);
           If l2R4selc = 'Y';
              If l2R4btyp = 'P';
                 ckRPCodes = *on;
              Endif;
              If l2R4btyp = 'D';
                 ckRDCodes = *on;
              Endif;
              If svfnd <> l2R4fnd;
                 svfnd = l2R4fnd;
                 Setll (intco#:svappl:svfnd:sysDate7) pbi07;
                 Reade (intco#:svappl:svfnd) pbi07;
                 If %eof(wmbil7);
                    Iter;
                 Endif;
                 chain ckky pck00;
                 If ckRPCodes = *on;
                    If CKrpcd = *blanks or CKrpvs = *blanks;
                       setUpFlag = *on;
                       ctr = ctr + 1;
                       If ctr = 7;
                          ctr = 6;
                       Endif;
                       i = %LookUp(CKacct:acct:1);
                       If i = 0;
                          acct(ctr) = CKacct;
                       Endif;
                    Endif;
                 Endif;
                 If WCFL36 = 'Y' and ckRDCodes = *on;
                    If CKrdcd = *blanks or CKrdvs = *blanks;
                       setUpFlag = *on;
                       ctr = ctr + 1;
                       If ctr = 7;
                          ctr = 6;
                       Endif;
                       i = %LookUp(CKacct:acct:1);
                       If i = 0;
                          acct(ctr) = CKACCT;
                       Endif;
                    Endif;
                 Endif;
                 ckRDCodes = *off;
                 ckRPCodes = *off;
              Else;
                 setgt l2ky pr4l2;
              Endif;
           Endif;
           reade intco# pr4l2;
        Enddo;
        return setUpFlag;

      /end-free
     p retPremSetup    e

      //===================================================================
      // Set Visibility for Return Fund Check Requests
      //===================================================================
     p SetVisRFCReq    b
     d SetVisRFCReq    pi
      /Free

       // Initialize all the messageId and error indicators.

        If R4acfl = 'Y';
           agencyBill = *on;
           visAgtInfo = *on;
           visPFiPTyp = *off;
        Else;
           agencyBill = *off;
           visAgtInfo = *off;
           visPFiPTyp = *on;
        Endif;

        Chain (R4Co#) pco00;

        If R4btyp <> 'I';
           isnotIndpdt = *on;
        Else;
           isnotIndpdt = *off;
        Endif;

        If R4btyp <> 'A';
           visCustID = *on;
        Else;
           visCustID = *off;
        Endif;

        If agencyBill = *on and isnotIndpdt = *on;
           visARStMt = *on;
           scFocus  = *on;
           lay2Top = 230;
        Else;
           visARStMt = *off;
           scFocus  = *off;
        Endif;

      /End-Free
     p SetVisRFCReq    e

      //===================================================================
      // Validate the Return Fund Check Print
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateRFCPrt  b
     d ValidateRFCPrt  pi              n
     d isError         s               n   inz
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erFGrp = *off;
        msFGrp = *blanks;
        erTGrp = *off;
        msTGrp = *blanks;
        erProcDt = *off;
        msProcDt = *blanks;
        erBtypPrt = *off;
        msBtypPrt = *blanks;
        msSRT1    = *off;
        erSRT1    = *blanks;
        msgSRT2    = *blanks;
        erSRT2    = *off;

        // Validate Group Range
        If scFgrp > scTgrp;
           erFGrp  = *on;
           msFGrp  = 'WCR0702';
           erTGrp  = *on;
           msTGrp  = 'WCR0702';
           isError = *on;
        Endif;

        // Validate Process date
        If scProcDt <> *zeros;
           erProcDt = ValidDate(scProcDt:charMsg);
           If erProcDt = *on;
              msProcDt = charMsg;
              isError = *on;
           Endif;
        Endif;

        // Process date can be today's date or last process date
        If scProcDt <> DateTo8(systemDate()) and
                  (PremChkDt <> 0 and scProcDt <> PremChkDt);
           isError = *on;
           erProcDt = *on;
           msProcDt = 'WCR3213';
        Endif;

        // Check batch type
        If WCfl36 <> 'Y' AND scBtypPrt = 'I';
           erBtypPrt = *on;
           msBtypPrt = 'FCR0730';
           isError = *on;
        Endif;

        // Check primary sort
        If scSrt1 = *blanks;
           erSRT1 = *on;
           msSRT1 = 'WCR0503';
           isError = *on;
        Endif;

        //Check for the proper Secondary Sort value
        If scSrt1 = 'AN' and scSrt2 =  *blank;
           erSRT2 = *on;
           msgSRT2 = 'Secondary Sort Required';
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateRFCPrt  e

      //===================================================================
      // Validate the Return Fund Check Request
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateRFCReq  b
     d ValidateRFCReq  pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erCust = *off;
        msCust = *blanks;
        erPTFL = *off;
        msPTFL = *blanks;
        erSTDT = *off;
        msSTDT = *blanks;

        // Check entered customer ID
        If scCust <> *blanks;
           chain keyWmeml wmeml;
           If %found(wmeml);
              If %xlate(lo:up:scCust) <> %xlate(lo:up:EMcust);
                 erCust = *on;
                 msCust = 'WCR3221';
                 isError = *on;
              Endif;
           Endif;
        Endif;

        // If not account current then Paid to Type should not be blank
        If svacfl <> 'Y' and scPTFL = *blanks;
           erPTFL = *on;
           msPTFL = 'WCR0503';
           isError= *on;
        Endif;

        // validate if Paid to Type is changed
        If scptfl <> svptfl;
           Select;
           When scptfl = 'P';
              chain keyWdell pel00;
              If %found(wdell);
                 chain keyWmpfl ppf00;
                 If %found(wmpfl);
                    svpfin = elpfin;
                    scptnm = PFname;
                    svptn2 = *blanks;
                    scpad1 = PFadr1;
                    scpad2 = PFadr2;
                    svpcty = PFcty;
                    svpst  = PFst;
                    svpZip = %char(PFzip);
                 Else;
                    erPTFL = *on;
                    msPTFL = 'WC30281';
                    isError = *on;
                 Endif;
              Endif;
           When scptfl = 'E';
              chain keyWmeml pem00;
              If %found(wmeml);
                 clear scpfname;
                 clear svpfin;
                 If COfedrc = 'Y' and EMfdnm <> *Blank;
                    scPtNm = EMfdnm;
                    svPtN2 = EMfdn2;
                 Else;
                    scptnm = EMname;
                    svptn2 = EMnam2;
                 Endif;
                 If EMmpfl = 'M';
                    scpad1 = EMmad1;
                    scpad2 = EMmad2;
                    svpcty = EMmcty;
                    svpst  = EMmst;
                    svpZip = %char(EMmzip);
                 Else;
                    scpad1 = EMpad1;
                    scpad2 = EMpad2;
                    svpcty = EMpcty;
                    svpst  = EMpst;
                    svpZip = %char(EMpzip);
                 Endif;
              Endif;
              svptfl = scptfl;
           When scptfl = 'A';
              If svagt# = *zero and agencyBill = *Off;
                 ##Agent#  = 0;
                 ##AgentNm = ' ';
                 ##Prod#   = 0;
                 ##ProdNm  = ' ';
                 ##Area    = 0;
                 ##Phone   = 0;
                 ##Plan    = '*';
                 ##Date    = 0;
                 GetAgnPrInfo(R4co#:R4fnd:R4emp#:R4div:R4fyr:##Agent#:##AgentNm:
                  ##Prod#:##ProdNm:##Area:##Phone:##Plan:##Date);

                 If ##plan = 'A';
                    erPTFL = *on;
                    msPTFL = 'WCR0530';
                    isError = *on;
                 Else;
                    svagt# = ##agent#;
                 Endif;
              Endif;
              If svagt# > *zero  and isError = *off;
                 chain keyWmaal1 paa01;
                 If %found(wmaal1);
                    chain keyWmagl2 pag00;
                    If %found(wmagl);
                       scptnm = AGname;
                       svptn2 = *blanks;
                       scpad1 = AGmad1;
                       scpad2 = *Blanks;
                       svpcty = AGmcty;
                       svpst  = AGmst;
                       svpZip = %char(AGmzip);
                       svptfl = scptfl;
                    Else;
                       erPTFL = *on;
                       msPTFL = 'WCR0530';
                       isError = *on;
                    Endif;
                 Else;
                    erPTFL = *on;
                    msPTFL = 'WCR0530';
                    isError = *on;
                 Endif;
              Else;
                 erPTFL = *on;
                 msPTFL = 'WCR0530';
                 isError = *on;
              Endif;
           When scptfl = 'G';
              chain keyWmfnl pfn00;
              If %found(wmfnl);
                 scptnm = FNname;
                 svptn2 = *blanks;
                 scpad1 = FNmad1;
                 scpad2 = FNmad2;
                 svpcty = FNmcty;
                 svpst  = FNmst;
                 svpZip = %char(FNmzip);
                 svptfl = scptfl;
              Endif;
           Endsl;
        Endif;

        // Check entered statement Month/Year
        If svacfl = 'Y' and R4btyp = 'A';
           scstdt = dt6Check(scstdt);
           If scstdt = *zero;
              erSTDT  = *on;
              msSTDT  = 'Invalid date entered.';
              isError = *on;
           Endif;
           xxagt# = GetAgent#(R4co#:R4fein:R4mod);
           x5stdt = dt5CYMD(scstdt);
           chain KeyWdabl pab00;
           If not %found(wdabl);
              msSTDT ='Statement Month/Year not found for agent.';
              erSTDT  = *on;
              isError = *on;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateRFCReq  e

      //===================================================================
      // Validate the Return Fund Check Report
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateRFCRpt  b
     d ValidateRFCRpt  pi              n
     d isError         s               n   inz
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erPrtRpt = *off;
        msPrtRpt = *blanks;

        // check print report if blank
        If scPrtRpt = *blanks;
           erPrtRpt = *on;
           msPrtRpt = 'WCR0503';
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateRFCRpt  e


      // utility procedure definitions
      /COPY SCOPYCOMPI

