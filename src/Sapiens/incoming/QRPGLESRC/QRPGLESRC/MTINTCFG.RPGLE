     Hcopyright('(c) 2021 Sapiens. All rights reserved.')
     H**********************************************************************
     H DFTNAME(MTINTCFG)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('STBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Created   : 04/13/17                                              *
      *  Programmer: C:180                                                 *
      *  Project   : MC325                                                 *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  06/22/22 C:079 Support: 102638                                    *
      *                 Load Action Panel Items using service pgm          *
      *                                                                    *
      *  05/09/22 C:079 Project: 129WWV                                    *
      *                 Application Configuration Security                 *
      *                                                                    *
      *  05/27/21 C:318 Support:95270                                      *
      *                 Changes for inquiry mode to make fields disable    *
      *                 and links invisible                                *
      *                                                                    *
      *  03/04/19  C:33  Project SY390                                     *
      *                  Soft-code 'Portal'                                *
      *                                                                    *
      *  08/23/2018 c:62 Project SY390
      *                  Add portal menu option
      *                                                                    *
      *  07/25/18 C:180 Project: 146MIC                                    *
      *                 Add Reopened status field.                         *
      *                                                                    *
      *  05/14/18 C:291 Project: 150WCR                                    *
      *                 Add Policy Configuration option to the menu.       *
      *                                                                    *
      * -------------------------------------------------------------------*
      *  NOTE: Please add comments to the top of Revisions.                *
      * -------------------------------------------------------------------*
      *====================================================================*

     fMTINTCFG  cf   e             workstn extfile('MTINTCFG')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     Fsmiep     uf a e             DISK    infds(MieDs1)
     f*** Beg Add *** 05/09/22 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/09/22 *************************************************

      * system data structure
     d psds          esds                  extname(szq1p)

     d MieDs1          ds
     d  rrnMiep              397    400b 0
     d*** Beg Add *** 05/09/22 *************************************************
     d namesArr        s            100    DIM(20)
     d*** End Add *** 05/09/22 *************************************************

      * Prototypes

      * Entry of Program.
     d MTintCFGpgm     pr                  Extpgm('MTINTCFG')
     d                                9
     d                                1
     d                               10
     d                                5
     d MTintCFGPGM     pi
     d  @@rrn                         9
     d  @@mode                        1
     d  @@next                       10
     d  @@TabInd                      5

      * main Processing
     d Main            pr

      // Program Start
     d ProgramStart    pr
     d processMe                       n

     d Rtvsysval       pr                  ExtPgm('SA042R')
     d                                1

      * Copy books
     d/copy sPrDate
     d/copy SPrUsrPrms
     d/copy sPrModule
     d/copy sPrCompany
     d/copy sPrGenPop
     d*** Beg Add *** 05/09/22 ************************************************
     d/copy sPrSecure
     d*** End Add *** 05/09/22 ************************************************
     d*** Beg Add *** 06/22/22 ************************************************
     d/copy sPrActPnl
     d*** End Add *** 06/22/22 ************************************************

      * Stand Alone
     d PopMsg          s            256
     d PopTitle        s             50
     d intco#          S              3P 0
     d mode            s              1
     d Msgf            s             10    inz('FSMESG')
     d MsgLib          s             10    inz('*LIBL')
     d*** Beg Add *** 05/09/22 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
       dcl-s  index         zoned(3:0);
     d*** End Add *** 05/09/22 *************************************************
     d*** Beg Add *** 06/22/22 ************************************************
     d strMenuChoi     s            500    Varying
     d strMenuValu     s            100    Varying
     d*** End Add *** 06/22/22 ************************************************
      * Indicators
     d isValid         s               n   inz(*off)
     d ExitNow         s               n   inz(*off)
     d process         s               n   inz(*off)

      /free
       // Start program processing
       ProgramStart(process);
       //*** Beg Add *** 05/09/22 *********************************************
       LoadTabInfo();
       //*** End Add *** 05/09/22 *********************************************

       // Check for Managed Care Module
       If Process;
          // Main processing
          //*** Beg Chg *** 05/09/22 *********************************************
          //***Main();
          If index <> 0;
             Main();
          Else;
             scCompName = getCompNam(intCo#);
             isNoAccess = *on;
             TabNames = 'Security';
             TabHeight = 560;
             APnlHeight = TabHeight + 60;
             apShowMenu = *on;
             Write rcdSysTbl;
             Exfmt rcdPanel;
             Read rcdSysTbl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
          //*** End Chg *** 05/09/22 *********************************************
       EndIf;

       Unlock SMIEP;
       *InLr = *On;

      /end-free
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     D Main            PI

      /free

       ExitNow = *Off;

       // Load screen values
       Exsr LoadScreenValues;

       // Process until exit button is pressed.
       Dow ExitNow = *Off;

          // Process Import/Export screen.
          Exsr ExfmtScreens;

          Select;
          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh = *Off;

          When btnSave = *on;
             btnSave = *off;
             isValid = *off;

             If mode <> 'D';
                exsr Validate;
                If isValid = *on;
                   exsr WriteToFile;
                Endif;
             Endif;
          //*** Beg Add *** 05/09/22 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'MTINTCFG';
             inPgmName     = 'Managed Care Configuration';
             inTabName     =  namesArr(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/09/22 ******************************************
          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 0;
       When namesArr(TabIndex + 1) = 'Import/Export Settings';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 560;
          APnlHeight = TabHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt RcdMCimp;
          Read RcdSysTbl;
          Read rcdPanel;
       When TabIndex = 1;
       EndSl;

       Endsr;

       //===================================================================
       // Validate - Validate Require Reserve Worksheet Screen
       //===================================================================
       Begsr Validate;
       erFTYP = *off;
       IsValid = *on;

       If ShowPvrExp = *on and SCftyp = *blanks;
          MsgIdFTYP  = 'MCR0503';
          erFTYP = *on;
          isValid = *off;
       Endif;

       If SCcsts <> 'O';
          sccldays = *zero;
       Endif;

       Endsr;

       //===================================================================
       // WriteToFile - Write/Update Import-Export Process File
       //===================================================================
       Begsr WriteToFile;
       Setll 1 SMIEP;
       Read SMIEP;
       If ShowClmExp = *on;
          IEcchg = SCcchg;
          IEcdny = SCcdny;
          IEccvfl= SCccvfl;
          //IEcgrp = SCcgrp;
          IEcrcd = SCcrcd;
          IEcdth = SCcdth;
          IEcsts = SCcsts;
          IEcldays = SCcldays;
          IErsts = SCrsts;
       Endif;

       If ShowPvrExp = *on;
          IEftyp = SCftyp;
       Endif;

       If ShowPayExp = *on;
          IEinczro = SCinczro;
       Endif;

       If ShowBilImp = *on;
          IEudup = SCudup;
       Endif;

       If not %eof(SMIEP);
          update PIE01;
       Else;
          Write PIE01;
       Endif;

       Exfmt RcdSave;

       Endsr;
       //===================================================================
       // LoadScreenValues - Load screen values
       //===================================================================
       Begsr LoadScreenValues;

       erCSTS = *off;

       DsblClmExp = *on;
       DsblPvrExp = *on;
       DsblPayExp = *on;
       DsblBilImp = *on;

       // Check for Active Managed Care Modules
       ShowClmExp = IsModEnabled('Claims Export');
       ShowPvrExp = IsModEnabled('Provider Export');
       ShowPayExp = IsModEnabled('Paid Date Export');
       ShowBilImp = IsModEnabled('Bill Upload');

       Clear Pie01;
       Read(n) SMIEP;
       If not %eof(SMIEP);

          If ShowClmExp = *on;
             SCcchg = IEcchg;
             SCcdny = IEcdny;
             SCccvfl = IEccvfl;
             SCcrcd = IEcrcd;
             SCcdth = IEcdth;
             SCcsts = IEcsts;
             SCcldays = IEcldays;
             DsblClmExp = *off;
             SCrsts = IErsts;
          Endif;

          If ShowPvrExp = *on;
             SCftyp = IEftyp;
             DsblPvrExp = *off;
          Endif;

          If ShowPayExp = *on;
             SCinczro = IEinczro;
             DsblPayExp = *off;
          Endif;

          If ShowBilImp = *on;
             SCudup = IEudup;
             DsblBilImp = *off;
          Endif;

       Else;
             SCcchg = *Blank;
             SCcdny = *Blank;
             SCccvfl = *Blank;
             //SCcgrp = *Blank;
             SCcrcd = *Blank;
             SCcdth = *Blank;
             SCcsts = *Blank;
             SCcldays = *zeros;
             SCftyp = *Blank;
             SCinczro = *Blank;
             SCudup = *Blank;
             SCrsts = *Blank;

             If ShowClmExp = *on;
                DsblClmExp = *off;
             Endif;

             If ShowPvrExp = *on;
                DsblPvrExp = *off;
             Endif;

             If ShowPayExp = *on;
                DsblPayExp = *off;
             Endif;

             If ShowBilImp = *on;
                DsblBilImp = *off;
             Endif;
       Endif;

       If DsblClmExp = *on and DsblPvrExp = *on and
          DsblPayExp = *on and DsblBilImp = *on;
          PopMsg  = 'No Import/Export modules activated. ' +
                    'Contact your software vendor for assistance.';

          PopTitle   = 'Managed Care Configuration' ;

          PopOneBtn(PopMsg:PopTitle);
          @@Next = 'WTAPPCFG';
          ExitNow = *on;
       Endif;
       If mode = 'D';
          DsblClmExp = *on;
          DsblPvrExp = *on;
          DsblPayExp = *on;
          DsblBilImp = *on;
       Endif;
       Endsr;
      /end-free
     P Main            E
       //*** Beg Add *** 05/09/22 *********************************************
       //===================================================================
       // Load Tab Information
       //===================================================================
     P LoadTabInfo     B
     D LoadTabInfo     PI
       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       // Determine if user is in the App Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Import/Export Settings tab
       Chain (intCo#:'MTINTCFG':'Managed Care Configuration':
         'Import/Export Settings':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          TabNames = 'Import/Export Settings';
          index = 1;
          namesArr(index) = 'Import/Export Settings';
       Endif;

     P LoadTabInfo     E
       //*** End Add *** 05/09/22 **********************************************
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /Free
       // Get Company no.
       intCo# = GetUserInfo(q1user);

       // Get Company name
       scCompName = getCompNam(intCo#);

       // Program Mode.
        mode = @@mode;
       //*** Beg Add *** 05/09/22 **********************************************
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          Endsl;
       Endif;
       //*** End Add *** 05/09/22 **********************************************

       //Action Panel
       //aMenHeight = 75;
       aMenHeight = 120;
       apFootHght = aMenHeight + 125;
       ApShowMenu = *on;

       // Check for Managed Care Module
       If IsModEnabled('Managed Care');
          Processme = *on;

          //Applicable options

          // Tab Index Mode.
          TabIndex = 0;

          // Panel Heading
          scPnlHdg = 'Managed Care Configuration';

       Else;   // Managed Care not installed
        //Applicable options

          PopMsg     = 'The Managed Care module is not installed for use ' +
                       'at this time. For more information on this module, '  +
                       'please contact your StoneRiver representative.';

          PopTitle   = 'Managed Care Configuration' ;

          PopOneBtn(PopMsg:PopTitle);
          Processme = *off;
          @@Next = 'WTAPPCFG';

       EndIf;

       //Check for applicable options
       //*** Beg Chg *** 06/22/22 ********************************************
       //***scMenuChoi = 'Application Configuration,Claims Configuration';
       //***scMenuValu = 'WTAPPCFG,WTCLMCFG';

       //***If isModEnabled('Managed Care');
       //***   scMenuChoi = %trim(scMenuChoi) + ',Managed Care Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',MTINTCFG';
       //***Endif;

       //***If isModEnabled('PEO Module');
       //***   scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',WTPOLCFG';
       //***Endif;
       // Add Portal Configuration
       //***If isModEnabled('Portal Module');
       //***   scMenuChoi = %trim(scMenuChoi) + ',' +
       //***    GetModuleProperty('Portal Module':'Name') + ' Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',WICONFIG';
       //***Endif;
       GetActPnlItems(strMenuChoi:strMenuvalu);
       scMenuChoi = strMenuChoi;
       scMenuValu = strMenuValu;
       //*** End Chg *** 06/22/22 ********************************************
       Return;
      /end-free
     p ProgramStart    e
