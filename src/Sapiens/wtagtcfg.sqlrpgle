     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTAGTCFG- Agency Configuration (Profound)
     h dftname(WTAGTCFG)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 08/23/21                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2395 - Redesign of Agency Configuration program*
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  11/28/25 C:333 Support: 120136 (UBIC)                        *
      //                 Agent Activity Report Error - Select Agency   *
      //                 FEIN/ Mod.                                    *
      //                 - This is due to change performed under 117251*
      //                  passing scRrn1 to @@rrn.                     *
      //                 - Changes done as follows:                    *
      //                  The value passed to @@rrn is now either scRRN*
      //                  or scRRN1, determined based on checking the  *
      //                  call stack for WTRPTAGACT or WTRPTAGLOS.     *
      //                                                               *
      //  08/11/25 C:300 Support: 119485                               *
      //                 Issues which could not caught in QA - WC2395  *
      //                 1) Default County Code from ZipCode/County Code
      //                 tables, if left blank during ADD Agency Master*
      //                 2) Initialize Phone# & Carrier# in Agency Grid*
      //                                                               *
      //  02/04/25 C:291 Ticket: 117773                                *
      //                 Polupate iDarts link for Agent Assignment     *
      //                 records (WMAAL) loaded in the Agency grid.    *
      //                                                               *
      //  12/11/24 C:079 Ticket: 146/117773                            *
      //                 Use correct program ID in iDARTS URL          *
      //                                                               *
      //  08/05/24 C:291 Support: 117251                               *
      //                 Add another field scRRN1 on sflAgtCfg grid.   *
      //                 Loaded scRRN1 from wmaal.                     *
      //                 Pass scRRN1 back to calling program When this *
      //                 program is called to get Agency so correct    *
      //                 Agent Number is returned.                     *
      //                                                               *
      //  07/30/24 C:333 Support: 117088 (UBIC)                        *
      //                 QA Testing - New Agent Loss Ratio (PPR) Report*
      //                 Not Working Correctly.                        *
      //                                                               *
      //  04/21/24 C:340 Project: WC2964                               *
      //                 Add Carriers Column to display all Carriers   *
      //                 assigned to Agency.                           *
      //                                                               *
      //  01/08/24 C:342 Project: WC2676                               *
      //                 1.Get Agent FEIN/Mod when called from WTRFBTCH*
      //                 2.DSPF changes:                               *
      //                   1.Javascript changes on Onload property     *
      //                     for rcdAgtCfg with date 01/08/24          *
      //                   2.Javascript changes on OnrowClick property *
      //                     for sflAgtCfg with date 01/08/24          *
      //                   3.Added Cancel button, Simple Container,    *
      //                     Panel, Select button on rcdAgtCfg record  *
      //                     format.                                   *
      //                 3.Call this rogram with "S" mode i.e.         *
      //                   "Search" mode by WTRFBTCH/W0040R            *
      //                                                               *
      //  10/12/23 C:300 Project: WC2395, Support: 113191              *
      //                 When user don't have view authority for Agency*
      //                 Primary Info screen, just below "Contact" label
      //                 field headings are being written over one another
      //                                                               *
      //                 DSPF: Changes in Onload property of rcdAgtAdd *
      //                                                               *
      //  08/21/23 C:300 Project: WC2395, Support: 113191              *
      //                 DSPF: Correct Export to Excel File Name       *
      //                                                               *
      //  08/11/23 C:300 Project: WC2395, Support: 113088              *
      //                 Add coloumn for Agency# in the grid and       *
      //                 Do not show duplicate records for any Agency# *
      //                                                               *
      //  08/07/23 C:300 Project: WC2395, Support: 113225              *
      //                 Allow entry of data in Upper/Lower case in all*
      //                 'Comment' or 'Notes' type fields              *
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
     f                                     Sfile(sflAgtCfg:scSflRRN)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      * Agent Master file
     fwmagl1    if a e           k disk    rename(pag01:pag01)
     f                                     infds(dbfds)
     fwmagl     if   e           k disk    rename(pag01:pag00)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Agent Assignment File
     fwmaal     if   e           k disk    rename(paa01:paa00)
      *** Beg Add ***** 07/30/24 ****************************************
     f                                     infds(dbfds1)
      *** End Add ***** 07/30/24 ****************************************
      *** Beg Add *** 04/21/24 *** WC2964 *******************************
     fWdF2l     IF   E           K Disk
      *** Beg Add *** 04/21/24 *** WC2964 *******************************
      *** Beg Add *** 08/11/25 ******************************************
      * Zipcode by County File
     fwmczl     IF   E           K Disk    rename(pcz01:pcz00)
      *** Beg Add *** 08/11/25 ******************************************

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
      *** Beg Add ***** 07/30/24 ****************************************
     d dbfds1          ds
     d  dbrrn1               397    400b 0
      *** End Add ***** 07/30/24 ****************************************


      // work variables
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d*@@co            s              3  0
     d*@@fein          s              9  0
     d*@@mod           s              3  0
     d @@kl            s              1
     d @@lc            s              1
     d @@mode          s              1
     d @@rrn2          s              9  0
     D indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d xxst            s              2    inz
     d strDartsKey     s             40    inz
     d strUrlRRN       s              9    inz
     d strUrlTabName   s             50    inz
     d*
     d svnewagt        s              9  0 inz
     d svnewrrn        s              5  0 inz
     d t2              s              3  0 inz(*zeros)
     d t3              s              3  0 inz(*zeros)
     d* Array to store Agency numbers, which have been loaded into grid
     d ArrAgency       s              5  0 dim(999)
      *** Beg Add *** 04/21/24 *** WC2964 ******************
     d sqlStmtStr      s               a   len(32000) varying
     d wrkCarrier      s              5  0
      *** End Add *** 04/21/24 *** WC2964 ******************
      *** Beg Add *** 11/28/25 *****************************
     d qqfound         s              1
      *** End Add *** 11/28/25 *****************************

      // Proto-types

      // Change FEIN
     d ChangeFEIN      pr                  extpgm('W4020R')
     d*ChangeFEIN      pr                  extpgm('WTAGTFEIN')
     d @@kl                           1
     d @@lc                           1
     d @@mode                         1
     d @@rrn2                         9  0

      *** Beg Add *** 11/28/25 *****************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  qqPgmName                    10    const
     d  qqFound                       1
      *** End Add *** 11/28/25 *****************************

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

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
     d  dirLength      s              2S 0 Inz

      * Initialize the Screen fields for Agency Configuration
     d InzAgtCfgScVal  pr

      // Load Agency Grid info
     d LoadAgtGrid     pr

      * Main Processing
     d Main            pr

      * Process Agency Addition
     d ProcessAdd      pr

      * Process other Options
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

 ‚    * Validate Agency Addition info. procedure.
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

      //
      //   Key List for the Documents Table File
      //
     c     keyWMAAL      klist
     c                   kfld                    AGco#
     c                   kfld                    AGfein
     c                   kfld                    AGmod

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index

      /free
       Exec Sql Set Option Commit = *None;

       GetClientID(clientID);

       ProgramStart(process);

       //***  Beg Add  **** 01/08/24 *******************************
       If @@next <> 'GetAgency';
       //***  End Add  **** 01/08/24 *******************************
       If process;

          Main();

       Endif;

       //***** Beg Add ******* 01/08/24 **************************
       Endif;

       // Load Agency Data in subfile
       If @@next = 'GetAgency';
          // Panel Heading
          scPnlHdg = 'Select Agency FEIN/Mod';
          visGetAgt = *on;                       // Show cancel button/Hide Detail box
          visbpnl = *on;                         // Show Panel/Hide Agent Hdg/
          topAgtPnl = 100;                       // Top for Panel
          HgtAgtPnl = 215;                       // Height for Panel
          visbC114 = *on;                        // Hide Secretary Name/Marketing Contact/Region
          // Load Agency Grid Info
          LoadAgtGrid();
          Exfmt rcdAgtCfg;
          ReadC sflAgtCfg;
          Dow not %eof;
             If btnAgtSel = *on;
                btnAgtSel = *off;
                @@next   = %char(scAgtFEIN);
                @@TabInd = %char(scAgtMod);
                //*** Beg Chg ***** 08/05/24 *********************************
                //***@@rrn    = %char(scRRN);
                //*** Beg Chg ***** 11/28/25 *********************************
                //***@@rrn    = %Char(scRRN1);
                // Checking Call Stack for WTRPTAGACT or WTRPTAGLOS.

                ChkStack('WTRPTAGACT':qqFound);
                If qqFound = '1';
                   @@rrn    = %char(scRRN);
                Endif;

                ChkStack('WTRPTAGLOS':qqFound);
                If qqFound = '1';
                   @@rrn    = %char(scRRN1);
                Endif;
                //*** End Chg ***** 11/28/25 *********************************
                //*** End Chg ***** 08/05/24 *********************************
             Endif;
             ReadC sflAgtCfg;
          Enddo;
          If BtnACan = *on;
             BtnACan = *off;
             @@TabInd = 'None';
          Endif;

       Endif;
       //***** End Add ******* 01/08/24 **************************

       *InLr = *On;
      /end-free

       //===================================================================
       // InzAgtCfgScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzAgtCfgScVal  b
     d InzAgtCfgScVal  pi
     d
      /Free
       scCo# = intCo#;

 ‚     // Set visibilty for Client 114 fields (E/O Coverage, E/O Coverage Expiration Date,
 ‚     // Exclude from Export, Secretary Name,Marketing Contact, Marketing Region
 ‚     // and Background Check Completion Date
        visbC114 = *off;
        If clientID = '114';
           visbC114 = *on;
        Endif;

 ‚     // Set visibilty for Add Agency Window Panel and Save/Cancel Buttons
       visAgtCfg = *On;

 ‚     // Set selection criteria for County Code
        xxst = AGpst;                    // use Physical Address State
        If xxst = *Blanks;
           xxst = AGmst;                 // use Mailing Address State
        Endif;
        scCncdCH = 'CNco# = ' + %EditC(scCo#:'X') + ' and ' +
         'CNstat = ' + '''A''' + ' and CNkey like ' + '''' + xxst + '%'+ '''';


 ‚      // Need to remove following later //
        // visbC114 = *on;




      /End-Free
     p InzAgtCfgScVal  e

       //===================================================================
       // Load Agency Grid info
       //===================================================================
     p LoadAgtGrid     b
     d LoadAgtGrid     pi

      /free
         // Clear the subfile
         scSflRRN = 0;
         scSflDsp = *off;
         scSflClr = *on;
         Write rcdAgtCfg;
         scSflClr = *off;
         scSflDsp = *on;
         scRowSel = 1;

         //***** Beg Add ******* 01/08/24 **************************
         scInMode = @@Mode;
         //***** End Add ******* 01/08/24 **************************
         // Populate the subfile
         Setll *loval wmagl1;
         Read wmagl1;
         Dow not %eof(wmagl1);
            scSflRRN += 1;
            scSflSel = *off;
            If scSflRRN = 1;
               scSflSel = *on;
            Endif;
 ‚         // Select newly added Agency at the top of the Grid
            If svnewagt <> 0 and svnewagt = AGfein;
               svnewagt = 0;
               svnewrrn = scSflRRN;
               scSflSel = *on;
            Endif;

            scAgtNm = AGname;
            scAgtFEIN = AGfein;
            scAgtMod = AGmod;
            //***** Beg Add ******* 08/11/25 **************************
            scAgtPhn = ' ';
            scAgtCarr = ' ';
            //***** End Add ******* 08/11/25 **************************
            If AGare1 <> *zeros;
               scAgtPhn = '(' + %Subst(%EditC(AGare1:'X'):1:3) + ')';
            EndIf;
            If AGphn1 <> *zeros;
               scAgtPhn = %trim(scAgtPhn)+ '-' +%Subst(%EditC(AGphn1:'X'):1:3) +
                '-' + %Subst(%EditC(AGphn1:'X'):4:4);
            EndIf;
            //*** Beg Del ***** 07/30/24 *******************************
            //***scRRN  = dbrrn;
            //*** End Del ***** 07/30/24 *******************************
            //*** Beg Add ***** 08/05/24 *******************************
            scRRN  = dbrrn;
            //*** End Add ***** 08/05/24 *******************************

            // Populate Edit Agency Link
            Clear scEdtURL;
            strUrlRRN = %Editc(scRRN:'X');
            strUrlTabName = AGname;
            scEdtURL = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'C':
             'WTAGTPRI');

            // Populate Inquiry Agency Link
            Clear scInqryURL;
            strUrlRRN = %Editc(scRRN:'X');
            strUrlTabName = AGname;
            scInqryURL = CreateURL('STDRVPGM':strUrlTabName:strUrlRRN:'D':
             'WTAGTPRI');

            // Populate iDARTS link
            Clear scDartURL;
            strUrlTabName = 'iDARTS' + ' - ' + %Trim(AGname);
            strDartsKey = %editc(AGfein:'X') + %editc(AGmod:'X');
            //***scDartURL  = strUrlTabName +
            //*** Beg Chg *** 12/11/24 ****************************************
            //*** CreateGenUrl('/iDartsWeb/profound.do':
            //*** 'user=' + %Trim(q1User):
            //*** '&keyProgram=WTAGTCFG':
            //*** '&key=' + %trim(strDartsKey):
            //*** '&action=listing':
            //*** '&mode=remarks');
            scDartURL  = strUrlTabName +
             CreateGenUrl('/iDartsWeb/profound.do':
             'user=' + %Trim(q1User):
             '&keyProgram=W0040R':
             '&key=' + %trim(strDartsKey):
             '&action=listing':
             '&mode=remarks');
            //*** End Chg *** 12/11/24 ****************************************

            scAgt# = 0;
            clear t2;
            clear t3;
            clear ArrAgency;
            // Get Agency Number from Agent Assignment File (wmaal)
            Setll keyWMAAL paa00;
            Reade keyWMAAL paa00;
            If not %eof(WMAAL);
               scAgt# = AAagt#;
               // Load Agency Number into Array
               t2 = t2 + 1;
               ArrAgency(t2) = AAagt#;
               //***** Beg Add ******* 08/11/25 **************************
               // Moving it here inside IF condition
               ExSr BuildCarriers;
               //***** End Add ******* 08/11/25 **************************
            Endif;
            //*** Beg Add *** 04/21/24 *** WC2964 *******************************
            //***** Beg Del ******* 08/11/25 **************************
            // Moving it above inside IF condition
            //*** ExSr BuildCarriers;
            //***** End Del ******* 08/11/25 **************************
            //*** End Add *** 04/21/24 *** WC2964 *******************************
            //*** Beg Add ***** 07/30/24 ****************************************
            //*** Beg Chg ***** 08/05/24 ****************************************
            //***scRRN  = dbrrn1;
            scRRN1 = dbrrn1;
            //*** End Chg ***** 08/05/24 ****************************************
            //*** End Add ***** 07/30/24 ****************************************
            Write sflAgtCfg;

            // Load into grid, if found more distinct Agency # records in wmaal file
            //*** Beg Add *** 04/21/24 *** WC2964 *******************************
               scAgtCarr = ' ';
            //*** End Add *** 04/21/24 *** WC2964 *******************************
            Reade keyWMAAL paa00;
            Dow not %eof(WMAAL);
               t3 = 0;
               t3 = %lookup(AAagt#:ArrAgency);
               If t3 = 0;
                  t2 = t2 + 1;
                  ArrAgency(t2) = AAagt#;

                  scAgt# = AAagt#;
                  scSflRRN += 1;
                  scSflSel = *off;
            //*** Beg Add *** 04/21/24 *** WC2964 *******************************
                  ExSr BuildCarriers;
            //*** End Add *** 04/21/24 *** WC2964 *******************************
                  //*** Beg Add ***** 08/05/24 ****************************************
                  scRRN1 = dbrrn1;
                  //*** End Add ***** 08/05/24 ****************************************
                  //*** Beg Add ***** 02/04/25 ****************************************
                  // Populate iDARTS link
                  Clear scDartURL;
                  strUrlTabName = 'iDARTS' + ' - ' + %Trim(AAname);
                  strDartsKey = %editc(AAfein:'X') + %editc(AAmod:'X');
                  scDartURL  = strUrlTabName +
                   CreateGenUrl('/iDartsWeb/profound.do':
                   'user=' + %Trim(q1User):
                   '&keyProgram=W0040R':
                   '&key=' + %trim(strDartsKey):
                   '&action=listing':
                   '&mode=remarks');
                  //*** End Add ***** 02/04/25 ****************************************
                  Write sflAgtCfg;
               Endif;
               Reade keyWMAAL paa00;
            Enddo;

            Read wmagl1;
         Enddo;

         scSflRRN = 1;
 ‚      // Select newly added Agency at the top of the Grid
         If svnewrrn <> 0;
            scSflRRN = svnewrrn;
            scRowSel = svnewrrn;
            svnewrrn = 0;
         Endif;

        //*** Beg Add *** 04/21/24 *** WC2964 *******************************
         BegSr BuildCarriers;

               scAgtCarr = ' ';
               sqlStmtStr = 'Select Distinct F2Car# From WDF2L'
                          + ' Where Exists (Select 1 From WMAAL'
                          + ' Where F2CO# = AACO# and F2FND = AAFND'
                          + ' and AACO# = ' + %Char(AACO#)
                          + ' and AAFEIN = ' + %Char(AAFEIN)
                          + ' and AAAGT# = ' + %Char(AAAGT#) +')';

               Exec SQL Prepare sqlCarrier From :sqlStmtStr;
               Exec SQL Declare cursorCarrier Cursor For sqlCarrier;
               Exec SQL Open cursorCarrier;

               Dou Not IsValidFetch(sqlStt);
                   Exec SQL Fetch next From cursorCarrier into :wrkCarrier;
                   If IsValidFetch(sqlStt) and wrkCarrier <> 0;
                      If scAgtCarr <> ' ';
                         scAgtCarr = %Trim(scAgtCarr) + ',' + %Char(wrkCarrier);
                      Else;
                         scAgtCarr = %Char(wrkCarrier);
                      EndIf;
                   Endif;
               EndDo;

               Exec SQL Close cursorCarrier;
         EndSr;
        //*** End Add *** 04/21/24 *** WC2964 *******************************

      /end-free
     p LoadAgtGrid     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       LoadActionBar();

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load Agency Grid Info
          LoadAgtGrid();

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Add Agency
          When BtnAgtAdd = *On;
             BtnAgtAdd = *Off;
             ProcessAdd();

          Other;
             ProcOther();             // Change FEIN Link

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When indexTab = 1;
          pnlHeight = 630;
          APnlHeight = pnlHeight + 60;

          // Loading initial values
          InzAgtCfgScVal();

          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdAgtCfg;
          Read rcdSysTbl;
          Read rcdPanel;

       EndSl;

       Endsr;


      /end-free
     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of Agency
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d*
     d  isexit         s               n   Inz(*off)
     d  @@st           s              2    Inz
     d  @@zip5         s              5  0 Inz

      /free

       Clear RcdAgtAdd;
       isexit = *off;
       BtnAgtCan = *off;

       // Loading initial values
       InzAgtCfgScVal();

       Dow BtnAgtCan = *off and isexit = *off;

          Exfmt RcdAgtAdd;

          Select;
          // Save Button
          When BtnSaveAgt = *on;
             BtnSaveAgt = *off;
             If ValidateAgt();
                Exsr WrtAgt;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnAgtCan = *on;
               BtnAgtCan = *off;
               isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        AGco#   = scCo#;                 // Company Number
        AGfein  = scFein;                // Federal Tax ID
        AGmod   = scMod;                 // Modifier
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
        //***** Beg Add ******* 08/11/25 **************************
        // If user left County Code Blank, then default it from WMCZP
        If AGcnty = ' ';
           // Use Physical ST & Zip If present, else use Mailing ST & ZIP
           If AGpzip <> 0;
              @@st = AGpst;
              @@zip5 = AGpzip / 10000;
           Else;
              @@st = AGmst;
              @@zip5 = AGmzip / 10000;
           Endif;

           Chain (AGco#:@@st:@@zip5) pcz00;
           If %found(wmczl);
              AGcnty = CZcnty;
           Endif;
        Endif;
        //***** End Add ******* 08/11/25 **************************
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
        AGETsize  = scSize;
        AGETcnt   = scCount;
        AGEftp    = scSite;

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
        AGEftpD   = scFtpDir;

        AGstat  = 'A';                   // Status
        AGuser  = q1user;                // User
        AGpgm   = q1pgm;                 // Program
        AGtime  = Timeto6(SystemTime()); // Time
        AGdt    = Dateto7(SystemDate()); // Date Mintained
        AGiusr  = q1user;                // Initial User
        AGipgm  = q1pgm;                 // Initial Program
        AGitm   = Timeto6(SystemTime()); // Initial Time
        AGidt   = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new Agency subroutine
       BegSr WrtAgt;
        Clear pag01;
        Exsr MoveScDb;
        Write pag01;

 ‚     // Select newly added Agency at the top of the Grid
          svnewagt = scfein;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcOther - Process other Options
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
      /free

       ReadC sflAgtCfg;

       Dow not %eof;

          Select;
          // Processing for Change FEIN Link
          When scChgFEIN = *On;
             scChgFEIN = *Off;
             @@kl = *off;
             @@lc = *off;
             @@mode = ' ';
             @@rrn2 = scRRN;
             ChangeFEIN(@@kl:@@lc:@@mode:@@rrn2);

             btnRefresh = *On;

          Endsl;

          ReadC sflAgtCfg;
       Enddo;

      /end-free
     P ProcOther       E

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
          //***  Beg Add  **** 01/08/24 ****************************
          When @@mode = 'S';                 // "S" stands for Search Mode
             dspMode = *on;
          //***  End Add  **** 01/08/24 ****************************
          Endsl;
       Endif;

       // Set Screen Heading
       scHeading = 'Agency Configuration';

       // Set Tab Names
       TbNmRcdPnl = 'Agency Configuration';

       aMenHeight = 120;

       apFootHght = aMenHeight + 125;

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

       scSize     = 0;
       scCount    = 0;
       scSite     = ' ';
       scUserName = ' ';
       scPassword = ' ';
       scFtpDir   = ' ';

       processMe = *on;

       Return;

      /end-free
     p ProgramStart    e

 ‚     //===================================================================
 ‚     // Validate the Agency addition information.
 ‚     // Returns *on  ----> No errors
 ‚     // Returns *off ----> Any errors
  ‚    //===================================================================
     p ValidateAgt     b
     d ValidateAgt     pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
 ‚      // Initialize all the messageId and error indicators.
        erName = *off;
        msName = *blanks;
        erFein = *off;
        msFein = *blanks;
        erMod  = *off;
        msMod  = *blanks;
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
        erFtpDir   = *off;
        mgFtpDir   = *blanks;

 ‚      // Agency name cannot be blank.
        If scName = *blanks;
           erName = *on;
           msName = 'WCR0503';
           isError = *on;
        Endif;

 ‚      // Federal Tax ID cnnot be Zero.
        If scFein = *zeros;
           erFein = *on;
           msFein = 'WCR0502';
           isError = *on;
        Endif;

 ‚      // Validation check for Duplicate record
        If scFein <> *zeros;
           Chain (scCo#:scFein:scMod) pag00;
           If %found(wmagl);
              erFein = *on;
              msFein = 'WC30005';
              erMod  = *on;
              msMod  = 'WC30005';
              isError = *on;
           Endif;
        Endif;

 ‚      // Enter complete info for Mailing address
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

 ‚      // Enter complete info for Physical address, if any of the field is entered
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

 ‚      // Validation Check for E/O Coverage Expiration Date
        If scEoEx <> 0;
           erEoEx = ValidDate(scEoEx:charMsg);
           If erEoEx = *on;
              msEoEx = charMsg;
              isError = *on;
           Endif;
        Endif;

 ‚      // Validation Check for W9 Sent Date
        If scW9se <> 0;
           erW9se = ValidDate(scW9se:charMsg);
           If erW9se = *on;
              msW9se = charMsg;
              isError = *on;
           Endif;
        Endif;

 ‚      // Validation Check for W9 Received Date
        If scW9rc <> 0;
           erW9rc = ValidDate(scW9rc:charMsg);
           If erW9rc = *on;
              msW9rc = charMsg;
              isError = *on;
           Endif;
        Endif;

 ‚      // Validation Check for Email Address
        If scEmal <> *Blanks;
           erEmal = Check_Email(scEmal:mgEmal);
           If erEmal = *on;
              isError = *on;
           Endif;
        Endif;

 ‚      // Validation Check for 1099 Mailed to Modifier
 ‚      // It must be Valid or equal to current Mod
        If sc1099 = 'Y' and sc109M <> 0 and sc109M <> scMod;
           Chain (scCo#:scFein:sc109M) pag00;
           If %found(wmagl) and AGstat = 'A';
           Else;
              er109M = *on;
              ms109M = 'WC30091';
              isError = *on;
           Endif;
        Endif;

 ‚      // Validation Check for 1099 Mailed to Modifier
 ‚      // It must be Valid or equal to current Mod
        If scCckm <> 0 and scCckm <> scMod;
           Chain (scCo#:scFein:scCckm) pag00;
           If %found(wmagl) and AGstat = 'A';
           Else;
              erCckm = *on;
              msCckm = 'WC30091';
              isError = *on;
           Endif;
        Endif;

 ‚      // Validation Check for all 6 Dates and Corresponding Staff Codes
 ‚      // The indicated information is mutually inclusive.
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







 ‚     // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateAgt     e

      // utility procedure definitions
      /COPY SCOPYCOMPI

