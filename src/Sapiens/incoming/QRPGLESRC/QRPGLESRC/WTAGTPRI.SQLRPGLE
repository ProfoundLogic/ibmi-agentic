     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTAGTPRI- Agency Primary Information
     h dftname(WTAGTPRI)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 10/20/21                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2395 - Redesign of Agency Configuration program*
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  04/21/24 C:340 Project: WC2964                               *
      //                 Recompile over DSPF changes in WTAGTCFG       *
      //                                                               *
      //  01/08/24 C:342 Project: WC2676                               *
      //                 Recompile over DSPF changes in WTAGTCFG       *
      //                                                               *
      //  10/12/23 C:300 Project: WC2395, Support: 113191              *
      //                 When user don't have view authority for Agency*
      //                 Primary Info screen, just below "Contact" label
      //                 field headings are being written over one another
      //                                                               *
      //                 DSPF: Changes in Onload property of rcdAgtAdd *
      //                                                               *
      //  08/07/23 C:300 Support: 113225                               *
      //                 Allow entry of data in Upper/Lower case in all*
      //                 'Comment' or 'Notes' type fields              *
      //                                                               *
      // 06/02/23 C:342 Support:108934                                 *
      //                Increased height of panel on Connect Tab       *
      //                                                               *
      //  02/15/23 C:300 Project: WC2395                               *
      //                 Merge WTAGTPRI DSPF into WTAGTCFG DSPF        *
      //                 (as per review comments by Wes Winner)        *
      //                                                               *
      //  01/16/23 c:62 Project WC2838                                 *
      //                Add Ftp Directory                              *
      //                                                               *
      //  10/03/22 c:62 Project WC2838                                 *
      //                Add Email Information                          *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTAGTCFG  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtPnl)
      * Agent Master file
     fwmagp     if   e             disk    rename(pag01:pag0p)
     f                                     infds(dbfds)
     fwmagl     uf   e           k disk    rename(pag01:pag00)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0


      // work variables
     d saveDT          s                   like(AGdt)
     d saveTM          s                   like(AGtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d xxRrn           s              9  0
     D indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d xxst            s              2    inz

     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz

      // Proto-types

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d*** Begin Add *** 10/03/22 *******************************************
     d* Retrieve Secured Data
     d StRtvDta        pr                  extPgm('STRTVDTA')
     d  inCompany#                    3  0
     d  inType                        1
     d  inTypeKey                    50
     d  inNumber1                     1
     d  outData1                    256
     d  inNumber2                     1
     d  outData2                    256

     d* Write Secured Data
     d StWrtDta        pr                  extPgm('STWRTDTA')
     d  inCompany#                    3  0
     d  inType                        1
     d  inTypeKey                    50
     d  inNumber1                     1
     d  inData1                     256
     d  inNumber2                     1
     d  inData2                     256

     d  inCompany#     s              3  0
     d  inType         s              1    Inz('A')
     d  inTypeKey      s             50
     d  inNumber1      s              1    Inz('1')
     d  inData1        s            256
     d  inNumber2      s              1    Inz('2')
     d  inData2        s            256
     d  outData1       s            256
     d  outData2       s            256
     d  hldPassword    s                   Like(SCpassword) Inz
     d*** End   Add *** 10/03/22 *******************************************
     d*** Begin Add *** 01/16/23 *******************************************
     d  dirLength      s              2S 0 Inz
     d*** End   Add *** 01/16/23 *******************************************

      * Initialize the Screen fields for Agency Configuration
     d InzAgtCfgScVal  pr

      * Main Processing
     d Main            pr

      // Load Agency Info
     d LoadAgtInfo     pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate Agency Addition info. procedure.
     d ValidateAgt     pr              n


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
     d/copy sPrGenErr
     d/copy witmpltd
     d/copy sCopyGrpPr

      //
      //   Key List for the Documents Table File
      //

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index

      /free
       Exec Sql Set Option Commit = *None;

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pag0p;
       inCo# = AGco#;
       scCo# = AGco#;

       If (@@next = *Blanks or @@next = 'WTAGTPRI');
          GetClientID(clientID);

          ProgramStart(process);

          If process;

             Main();

          Endif;
       Endif;


       *InLr = *On;
      /end-free

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

       //===================================================================
       // InzAgtCfgScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzAgtCfgScVal  b
     d InzAgtCfgScVal  pi
     d
      /Free
       scCo# = intCo#;

       // Disable Federal Tax ID and Modifier
       AgtAddEdt = *On;

      // Set visibilty for Client 114 fields (E/O Coverage, E/O Coverage Expiration Date,
      // Exclude from Export, Secretary Name,Marketing Contact, Marketing Region
      // and Background Check Completion Date
        visbC114 = *off;
        If clientID = '114';
           visbC114 = *on;
        Endif;

      // Set selection criteria for County Code
        xxst = AGpst;                    // use Physical Address State
        If xxst = *Blanks;
           xxst = AGmst;                 // use Mailing Address State
        Endif;
        scCncdCH = 'CNco# = ' + %EditC(scCo#:'X') + ' and ' +
         'CNstat = ' + '''A''' + ' and CNkey like ' + '''' + xxst + '%'+ '''';


       // Need to remove following later //
        // visbC114 = *on;




      /End-Free
     p InzAgtCfgScVal  e

       //===================================================================
       // Load Agency Information
       //===================================================================
     p LoadAgtInfo     b
     d LoadAgtInfo     pi

      /free
        //*** Begin Add *** 10/03/22 ************************************
        scSize     = 0;
        scCount    = 0;
        scSite     = ' ';
        scUserName = ' ';
        scPassword = ' ';
        //*** End   Add *** 10/03/22 ************************************
        //*** Begin Add *** 01/16/23 **************************************
        scFtpDir   = ' ';
        //*** End   Add *** 01/16/23 **************************************
        Chain xxRrn pag0p;
        If %found(wmagp);
           scCo#  = AGco#;                  // Company Number
           scFein = AGfein;                 // Federal Tax ID
           scMod  = AGmod;                  // Modifier
           scName = AGname;                 // Name
           scFdnm = AGfdnm;                 // Federal Name
           scShnm = AGshnm;                 // Short Name
           scMad1 = AGmad1;                 // Mailing Addres 1
           scMcty = AGmcty;                 // Mailing City
           scMst  = AGmst;                  // Mailing State
           scMzip = AGmzip;                 // Mailing Zipcode
           scPad1 = AGpad1;                 // Physical Addres 1
           scPcty = AGpcty;                 // Physical City
           scPst  = AGpst;                  // Physical State
           scPzip = AGpzip;                 // Physical Zipcode

           If AGphn1 <> *zeros;
              scPhone1 = %int(%editc(AGare1:'X') + %editc(AGphn1:'X'));
           EndIf;

           If AGphn2 <> *zeros;
              scPhone2 = %int(%editc(AGare2:'X') + %editc(AGphn2:'X'));
           EndIf;

           If AGfax# <> *zeros;
              scFax    = %int(%editc(AGfaxa:'X') + %editc(AGfax#:'X'));
           EndIf;

           scIadr  = AGiadr;                // Internet Address
           scEmal  = AGemal;                // E-Mail Address
           scHcon  = AGhcon;                // Contact
           If visbC114 = *off;
              scSecy  = AGsecy;             // Secretary Name
              scCont  = AGcont;             // Marketing Contact
              scMkrg  = AGmkrg;             // Marketing Region
           EndIf;
           If visbC114 = *on;
              scCovr  = AGsecy;             // E/O Coverage
              scEoEx = dtMDYY(%dec(AGcont:8:0));    // E/O Coverage Expiration Date
           EndIf;
           scEexp  = AGeexp;                // Exclude from Export
           scComm  = AGcomm;                // Comments 1
           scCom2  = AGcom2;                // Comments 2
           scSvcr  = AGsvcr;                // Agency Services Representative
           scMkrp  = AGmkrp;                // Marketing Representative
           scW9se = dtMDYY(AGw9se);         // W9 Sent Date
           scW9rc = dtMDYY(AGw9rc);         // W9 Received Date
           scBkch = dtMDYY(AGbkch);         // Background Check Completion Date
           scTpc2  = AGtpc2;                // Agent Type Code
           scGrcd  = AGgrcd;                // Agent Group Code
           scCncd = AGpst + AGcnty;         // County Code
           IF AGpst = *Blanks;
              scCncd = AGmst + AGcnty;
           EndIf;
           scArea  = AGarea;                // Area Code
           sc109M  = AG109m;                // 1099 Mailed to Modifier
           scCckm  = AGcckm;                // Checks Mailed to Modifier
           scAcdu  = AGacdu;                // Account Current, #Days to Due Date
           sc1099  = AG1099;                // 1099 Flag
           scSsid  = AGss#;                 // Is 1099 ID Number SS#
           scBkup  = AGbkup;                // Backup Withholding
           scSpa   = AGspa;                 // Special Agent Processing

           // Dates and Corresponding Staff Codes
           scDd01  = AGdd01;
           scDd02  = AGdd02;
           scDd03  = AGdd03;
           scDd04  = AGdd04;
           scDd05  = AGdd05;
           scDd06  = AGdd06;
           scStf1  = AGstf1;
           scStf2  = AGstf2;
           scStf3  = AGstf3;
           scStf4  = AGstf4;
           scStf5  = AGstf5;
           scStf6  = AGstf6;
           //*** Begin Add *** 10/03/22 ************************************
           scSize    = AGETsize;
           scCount   = AGETcnt;
           scSite    = AGEftp;
           //*** Begin Add *** 01/16/23 ************************************
           scFtpDir  = AGEftpD;
           //*** End   Add *** 01/16/23 ************************************

           // Let's get the user id and password
           inTypeKey  = %EditC(AGfein:'X') + %EditC(AGmod:'X');
           outData1   = ' ';
           outData2   = ' ';
           StRtvDta(AGco#:inType:inTypeKey:inNumber1:outData1:inNumber2:
                    outData2);
           scUserName = outData1;
           scPassword = outData2;
           hldPassword = scPassword;
           If scPassword <> ' ';
              scPassword  = '**********';
           EndIf;
           //*** End   Add *** 10/03/22 ************************************

           saveDT  = AGdt;
           saveTM  = AGtime;
        EndIf;


      /end-free
     p LoadAgtInfo     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

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
             btnRefresh = *Off;

          // Enter key or Save button
          When btnSave = *on;
             btnSave = *off;
             If @@mode <> 'D';
                // Check if WMAGP updated since screen displayed (Agent Master)
                flgRcdUpdated = RcdUpdatedErr('WMAGP':dbrrn:'AGdt':'AGtime':
                 'AGuser':saveDT:saveTM);
                if flgRcdUpdated = '0'; // No error

                   If ValidateAgt();
                      Exsr UpdAgt;
                   Else;
                      error = *on;
                   EndIf;

                Else;
                   error = *on;
                EndIf;

                // Show Data Saved Sucessfully Message
                If error = *off;
                   PopDataSaved();
                EndIf;
             Endif;

          // For "Change FEIN" option in More Action of Agency Toolbar
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:' ':txtWinData);
             btnShowWin = *off;


          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When arrTabNames(TabIndex+1) = 'Agency Information';

          //*** Begin Chg *** 10/03/22 ************************************
          //*** pnlHeight = 585;
          //*** Begin Chg *** 01/16/23 ************************************
          //*** pnlHeight = 600;
          pnlHeight = 630;
          //*** End   Chg *** 01/16/23 ************************************
          //*** End   Chg *** 10/03/22 ************************************
          APnlHeight = pnlHeight + 60;

          If error = *off;
             Clear rcdAgtAdd;

             // Loading initial values
             InzAgtCfgScVal();

             // Load Agency Info
              LoadAgtInfo();
          EndIf;

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdAgtPnl;
          Write rcdPanel;
          Exfmt rcdAgtAdd;
          Read rcdAgtPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Connect';
          //*** Beg Chg *** 06/02/23 ******************************************
          //*** pnlHeight = 750;
          pnlHeight = 880;
          //*** End Chg *** 06/02/23 ******************************************
          APnlHeight = pnlHeight + 60;

          Reset dsWITMPLTparms;
          dsWITMPLTparms.co# = inCo#;
          dsWITMPLTparms.fein = scFein;
          dsWITMPLTparms.mod = scMod;

          Write rcdAgtPnl;
          Write rcdPanel;
          TemplateAssociations(dsWITMPLTparms:'Agency':@@Mode);
          Read rcdAgtPnl;
          Read rcdPanel;

       EndSl;

       Endsr;


       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        AGname  = scName;                // Name
        AGfdnm  = scFdnm;                // Federal Name
        AGshnm  = scShnm;                // Short Name
        AGmad1  = scMad1;                // Mailing Addres 1
        AGmcty  = scMcty;                // Mailing City
        AGmst   = scMst;                 // Mailing State
        AGmzip  = scMzip;                // Mailing Zipcode
        AGpad1  = scPad1;                // Physical Addres 1
        AGpcty  = scPcty;                // Physical City
        AGpst   = scPst;                 // Physical State
        AGpzip  = scPzip;                // Physical Zipcode

        If scPhone1 > *zeros;
           AGare1  = %dec(%subst(%editc(scPhone1:'X'):1:3):3:0); // Phone 1 Area Code
           AGphn1  = %dec(%subst(%editc(scPhone1:'X'):4):7:0);   // Phone 1 Number
        EndIf;
        If scPhone1 = *zeros;
           AGare1  = *zeros;
           AGphn1  = *zeros;
        EndIf;
        If scPhone2 > *zeros;
           AGare2  = %dec(%subst(%editc(scPhone2:'X'):1:3):3:0); // Phone 2 Area Code
           AGphn2  = %dec(%subst(%editc(scPhone2:'X'):4):7:0);   // Phone 2 Number
        EndIf;
        If scPhone2 = *zeros;
           AGare2  = *zeros;
           AGphn2  = *zeros;
        EndIf;
        If scFax > *zeros;
           AGfaxa  = %dec(%subst(%editc(scFax:'X'):1:3):3:0); // Fax Area Code
           AGfax#  = %dec(%subst(%editc(scFax:'X'):4):7:0);   // Fax Number
        EndIf;
        If scFax = *zeros;
           AGfaxa  = *zeros;
           AGFax#  = *zeros;
        EndIf;

        AGiadr  = scIadr;                // Internet Address
        AGemal  = scEmal;                // E-Mail Address
        AGhcon  = scHcon;                // Contact
        If visbC114 = *off;
           AGsecy  = scSecy;             // Secretary Name
           AGcont  = scCont;             // Marketing Contact
           AGmkrg  = scMkrg;             // Marketing Region
        EndIf;
        If visbC114 = *on;
           AGsecy  = scCovr;             // E/O Coverage
           AGcont  = %EditC(dtCYMD(scEoEx):'X');     // E/O Coverage Expiration Date
        EndIf;
        AGeexp  = scEexp;                // Exclude from Export
        AGcomm  = scComm;                // Comments 1
        AGcom2  = scCom2;                // Comments 2
        AGsvcr  = scSvcr;                // Agency Services Representative
        AGmkrp  = scMkrp;                // Marketing Representative
        AGw9se  = dtCYMD(scW9se);        // W9 Sent Date
        AGw9rc  = dtCYMD(scW9rc);        // W9 Received Date
        AGbkch  = dtCYMD(scBkch);        // Background Check Completion Date
        AGtpc2  = scTpc2;                // Agent Type Code
        AGgrcd  = scGrcd;                // Agent Group Code
        AGcnty  = %subst(scCncd:3:2);    // County Code
        AGarea  = scArea;                // Area Code
        AG109m  = sc109M;                // 1099 Mailed to Modifier
        AGcckm  = scCckm;                // Checks Mailed to Modifier
        AGacdu  = scAcdu;                // Account Current, #Days to Due Date
        AG1099  = sc1099;                // 1099 Flag
        AGss#   = scSsid;                // Is 1099 ID Number SS#
        AGbkup  = scBkup;                // Backup Withholding
        AGspa   = scSpa;                 // Special Agent Processing

        // Dates and Corresponding Staff Codes
        AGdd01  = scDd01;
        AGdd02  = scDd02;
        AGdd03  = scDd03;
        AGdd04  = scDd04;
        AGdd05  = scDd05;
        AGdd06  = scDd06;
        AGstf1  = scStf1;
        AGstf2  = scStf2;
        AGstf3  = scStf3;
        AGstf4  = scStf4;
        AGstf5  = scStf5;
        AGstf6  = scStf6;

        //*** Begin Add *** 10/03/22 ************************************
        AGETsize  = scSize;
        AGETcnt   = scCount;
        AGEftp    = scSite;
        //*** Begin Add *** 01/16/23 ************************************
        AGEftpD   = scFtpDir;
        //*** End   Add *** 01/16/23 ************************************

        // Let's get the user id and password
        inTypeKey  = %EditC(AGfein:'X') + %EditC(AGmod:'X');
        inData1    = scUserName;

        If scPassword = '**********';
           If hldPassword <> ' ';
              inData2 = hldPassword;
           EndIf;
        Else;
           inData2 = scPassword;
        EndIf;

        StWrtDta(AGco#:inType:inTypeKey:inNumber1:inData1:inNumber2:
                 inData2);
        //*** End   Add *** 10/03/22 ************************************

        AGstat  = 'A';                   // Status
        AGuser  = q1user;                // User
        AGpgm   = q1pgm;                 // Program
        AGtime  = Timeto6(SystemTime()); // Time
        AGdt    = Dateto7(SystemDate()); // Date Mintained
        AGiusr  = q1user;                // Initial User
        AGipgm  = q1pgm;                 // Initial Program
        AGitm   = Timeto6(SystemTime()); // Initial Time
        AGidt   = Dateto7(SystemDate()); // Initial Date

        saveDT  = AGdt;
        saveTM  = AGtime;

       EndSr;

       // Update Agency Info subroutine
       BegSr UpdAgt;
        Chain (inCo#:scFein:scMod) pag00;
        Exsr MoveScDb;
        Update pag00;
       EndSr;

      /end-free
     p Main            e

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

       apShowMenu = *on;

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // Set Menu options
       scMenuChoi = 'Primary Agency Information';
       scMenuValu = 'WTAGTPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Group Assignments';
       scMenuValu = %trim(scMenuValu) + ',WTAGTASN';

       scMenuChoi = %trim(scMenuChoi) + ',Producers';
       scMenuValu = %trim(scMenuValu) + ',WTAGTPROD';

       scMenuChoi = %trim(scMenuChoi) + ',Contacts';
       scMenuValu = %trim(scMenuValu) + ',WTAGTCON';

       scMenuChoi = %trim(scMenuChoi) + ',Service Request';
       scMenuValu = %trim(scMenuValu) + ',WTAGTSRVRT';

       scMenuChoi = %trim(scMenuChoi) + ',Return Funds';
       scMenuValu = %trim(scMenuValu) + ',WTAGTRTFND';

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
     // Update Action Panel Info (Rrn, Agency Name)
       scActRrn = xxRrn;
       scCompName = %trim(AGname);
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

       scCo# = intCo#;

       processMe = *on;

       // Set Screen Heading
       scHeading = 'Primary Agency Information';

       strTabNames += AddToList('Agency Information':arrTabNames:n1);
       If IsModEnabled('Portal Module');
          strTabNames += AddToList('Connect':arrTabNames:n1);
       EndIf;
       TbNmRcdPnl = strTabNames;
       //*** Begin Add *** 10/03/22 ************************************
       scSize     = 0;
       scCount    = 0;
       scSite     = ' ';
       scUserName = ' ';
       scPassword = ' ';
       //*** End   Add *** 10/03/22 ************************************
       //*** Begin Add *** 01/16/23 ***************************************
       scFtpDir   = ' ';
       //*** End   Add *** 01/16/23 ***************************************

       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the Agency addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAgt     b
     d ValidateAgt     pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erName = *off;
        msName = *blanks;
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
        erEoEx = *off;
        msEoEx = *blanks;
        erW9se = *off;
        msW9se = *blanks;
        erW9rc = *off;
        msW9rc = *blanks;
        erBkch = *off;
        msBkch = *blanks;
        erEmal = *off;
        mgEmal = *blanks;
        er109M = *off;
        ms109M = *blanks;
        erCckm = *off;
        msCckm = *blanks;
        erDd01 = *off;
        msDd01 = *blanks;
        erDd02 = *off;
        msDd02 = *blanks;
        erDd03 = *off;
        msDd03 = *blanks;
        erDd04 = *off;
        msDd04 = *blanks;
        erDd05 = *off;
        msDd05 = *blanks;
        erDd06 = *off;
        msDd06 = *blanks;
        erStf1 = *off;
        msStf1 = *blanks;
        erStf2 = *off;
        msStf2 = *blanks;
        erStf3 = *off;
        msStf3 = *blanks;
        erStf4 = *off;
        msStf4 = *blanks;
        erStf5 = *off;
        msStf5 = *blanks;
        erStf6 = *off;
        msStf6 = *blanks;
        //*** Begin Add *** 10/03/22 ***************************************
        erSize     = *off;
        mgSize     = *blanks;
        erCount    = *off;
        mgCount    = *blanks;
        erSite     = *off;
        mgSite     = *blanks;
        erUserName = *off;
        mgUserName = *blanks;
        erPassword = *off;
        mgPassword = *blanks;
        //*** End   Add *** 10/03/22 ***************************************
        //*** Begin Add *** 01/16/23 ***************************************
        erFtpDir   = *off;
        mgFtpDir   = *blanks;
        //*** End   Add *** 01/16/23 ***************************************

       // Agency name cannot be blank.
        If scName = *blanks;
           erName = *on;
           msName = 'WCR0503';
           isError = *on;
        Endif;

       // Enter complete info for Mailing address
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

       // Enter complete info for Physical address, if any of the field is entered
        If scPad1 <> *blanks or
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

       // Validation Check for E/O Coverage Expiration Date
        If scEoEx <> 0;
           erEoEx = ValidDate(scEoEx:charMsg);
           If erEoEx = *on;
              msEoEx = charMsg;
              isError = *on;
           Endif;
        Endif;

       // Validation Check for W9 Sent Date
        If scW9se <> 0;
           erW9se = ValidDate(scW9se:charMsg);
           If erW9se = *on;
              msW9se = charMsg;
              isError = *on;
           Endif;
        Endif;

       // Validation Check for W9 Received Date
        If scW9rc <> 0;
           erW9rc = ValidDate(scW9rc:charMsg);
           If erW9rc = *on;
              msW9rc = charMsg;
              isError = *on;
           Endif;
        Endif;

       // Validation Check for Email Address
        If scEmal <> *Blanks;
           erEmal = Check_Email(scEmal:mgEmal);
           If erEmal = *on;
              isError = *on;
           Endif;
        Endif;

       // Validation Check for 1099 Mailed to Modifier
       // It must be Valid or equal to current Mod
        If sc1099 = 'Y' and sc109M <> 0 and sc109M <> scMod;
           Chain(n) (scCo#:scFein:sc109M) pag00;
           If %found(wmagl) and AGstat = 'A';
           Else;
              er109M = *on;
              ms109M = 'WC30091';
              isError = *on;
           Endif;
        Endif;

       // Validation Check for 1099 Mailed to Modifier
       // It must be Valid or equal to current Mod
        If scCckm <> 0 and scCckm <> scMod;
           Chain(n) (scCo#:scFein:scCckm) pag00;
           If %found(wmagl) and AGstat = 'A';
           Else;
              erCckm = *on;
              msCckm = 'WC30091';
              isError = *on;
           Endif;
        Endif;

       // Validation Check for all 6 Dates and Corresponding Staff Codes
       // The indicated information is mutually inclusive.
        If scDd01 <> 0;
           erDd01 = ValidDate(scDd01:charMsg);
           If erDd01 = *on;
              msDd01 = charMsg;
              isError = *on;
           Endif;
        Endif;
        If (erDd01 = *off and scDd01 <> 0 and scStf1 = *Blanks) or
         (scDd01 = 0 and scStf1 <> *Blanks);
           erDd01 = *on;
           msDd01 = 'WCR0544';
           erStf1 = *on;
           msStf1 = 'WCR0544';
           isError = *on;
        Endif;

        If scDd02 <> 0;
           erDd02 = ValidDate(scDd02:charMsg);
           If erDd02 = *on;
              msDd02 = charMsg;
              isError = *on;
           Endif;
        Endif;
        If (erDd02 = *off and scDd02 <> 0 and scStf2 = *Blanks) or
         (scDd02 = 0 and scStf2 <> *Blanks);
           erDd02 = *on;
           msDd02 = 'WCR0544';
           erStf2 = *on;
           msStf2 = 'WCR0544';
           isError = *on;
        Endif;

        If scDd03 <> 0;
           erDd03 = ValidDate(scDd03:charMsg);
           If erDd03 = *on;
              msDd03 = charMsg;
              isError = *on;
           Endif;
        Endif;
        If (erDd03 = *off and scDd03 <> 0 and scStf3 = *Blanks) or
         (scDd03 = 0 and scStf3 <> *Blanks);
           erDd03 = *on;
           msDd03 = 'WCR0544';
           erStf3 = *on;
           msStf3 = 'WCR0544';
           isError = *on;
        Endif;

        If scDd04 <> 0;
           erDd04 = ValidDate(scDd04:charMsg);
           If erDd04 = *on;
              msDd04 = charMsg;
              isError = *on;
           Endif;
        Endif;
        If (erDd04 = *off and scDd04 <> 0 and scStf4 = *Blanks) or
         (scDd04 = 0 and scStf4 <> *Blanks);
           erDd04 = *on;
           msDd04 = 'WCR0544';
           erStf4 = *on;
           msStf4 = 'WCR0544';
           isError = *on;
        Endif;

        If scDd05 <> 0;
           erDd05 = ValidDate(scDd05:charMsg);
           If erDd05 = *on;
              msDd05 = charMsg;
              isError = *on;
           Endif;
        Endif;
        If (erDd05 = *off and scDd05 <> 0 and scStf5 = *Blanks) or
         (scDd05 = 0 and scStf5 <> *Blanks);
           erDd05 = *on;
           msDd05 = 'WCR0544';
           erStf5 = *on;
           msStf5 = 'WCR0544';
           isError = *on;
        Endif;

        If scDd06 <> 0;
           erDd06 = ValidDate(scDd06:charMsg);
           If erDd06 = *on;
              msDd06 = charMsg;
              isError = *on;
           Endif;
        Endif;
        If (erDd06 = *off and scDd06 <> 0 and scStf6 = *Blanks) or
         (scDd06 = 0 and scStf6 <> *Blanks);
           erDd06 = *on;
           msDd06 = 'WCR0544';
           erStf6 = *on;
           msStf6 = 'WCR0544';
           isError = *on;
        Endif;

        //*** Begin Add *** 10/03/22 ************************************
        // Size must be greater than or equal to zero
        If scSize < 0;
           erSize = *On;
           mgSize = 'Size must be greater than or equal to zero.';
           isError = *on;
        EndIf;

        // Count must be greater than or equal to zero.
        If scCount < 0;
           erCount = *On;
           mgCount = 'Count must be greater than or equal to zero.';
           isError = *on;
        EndIf;

        // User Name and Password must be entered if Site entered
        If scSite <> ' ';
           If scUserName = ' ';
              erUserName = *On;
              mgUserName = 'User name must be entered.';
              isError = *on;
           EndIf;
           If scPassword = ' ';
              erPassword = *On;
              mgPassword = 'Password must be entered.';
              isError = *on;
           EndIf;
        // User Name and Password can't be entered if Site not entered
        Else;
           If scUserName <> ' ';
              erUserName = *On;
              mgUserName = 'User name can''t be entered.';
              isError = *on;
           EndIf;
           If scPassword <> ' ';
              erPassword = *On;
              mgPassword = 'Password can''t be entered.';
              isError = *on;
           EndIf;
        EndIf;
        //*** End   Add *** 10/03/22 ************************************
        //*** Begin Add *** 01/16/23 ************************************
        // Directory must start with a / and end with a /
        dirLength = %Len(%Trim(scFtpDir));
        If scFtpDir <> ' ';
           If (%Subst(scFtpDir:1:1) <> '/' Or
               %Subst(scFtpDir:dirLength:1) <> '/');
              erFtpDir = *On;
              mgFtpDir = 'Directory must begin with a / and end with a /.';
              isError  = *On;
           EndIf;
        EndIf;
        //*** End   Add *** 01/16/23 ************************************







      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateAgt     e

      // utility procedure definitions
      /COPY SCOPYCOMPI

