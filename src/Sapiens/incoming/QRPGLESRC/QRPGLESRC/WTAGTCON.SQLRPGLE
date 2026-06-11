     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTAGTCON - Agency Contacts
     h dftname(WTAGTCON)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 08/23/21                                         *
      //  Programmer: C:318                                            *
      //  Project   : WC2395- Redesign of Agency Configuration program *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      // 05/26/25 C:333 Project: WC3009 (Recompile)                    *
      //                Clms, Policy, Agncy contacts: Maint Info       *
      //                - DSPF changes.                                *
      //                                                               *
      // 09/24/24 C:342 Ticket: 117552                                 *
      //                DSPF Changes                                   *
      //                1.Increase the width of Producer, Distribution *
      //                  type, Sub Categories and Process Output      *
      //                  Control multi select box on rcdAddAgCo       *
      //                                                               *
      //  08/30/24 C:342 WC2826/117552                                 *
      //                DSPF changes:                                  *
      //                1. Changed choice option field for Distribution*
      //                   type to Show Distribution code - Description*
      //                                                               *
      //  04/11/24 C:340 Project: 129WXP                               *
      //                 Add Distribution Types before Status on Grid  *
      //                                                               *
      //  03/22/24 C:333 Project: WC2826                               *
      //                 Claims Contacts.                              *
      //                 DSPF - Contact type added to Agent Grid.      *
      //                      - Contact Type dropdown in Agent Add/Edit*
      //                        screen.                                *
      //                                                               *
      //  02/07/24 C:300 Project: WC2395                               *
      //                 During internal testing found that multi-select
      //                 boxes on Detail box, not loading correct values
      //                 DSPF: Changes in onload property in calling of*
      //                 fillSelectBoxes()                             *
      //                                                               *
      //  11/17/23 C:300 Support: 113191 (WC2395)                      *
      //                 DSPF: Change Export to Excel File Name to     *
      //                 Agency Contact                                *
      //                                                               *
      //  09/18/23 C:300 Project: WC2395, Support: 113880              *
      //                 Allow Updating Contacts in Inquiry Mode, if user
      //                 has been authorized via User Parameter Maintenance
      //                                                               *
      //  08/28/23 C:300 Project: WC2395, Support: 113191              *
      //                 Action Hyperlinks like Activate/Inactivate    *
      //                 should not be included as last column of excel*
      //                 spreadsheet export                            *
      //                 DSPF: Removed hyperlink scActAgCo and created *
      //                 2 new hyperlinks for Activate/Inactivate      *
      *                                                                *
      *                                                                *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fwtagtcon  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflAgtCon:rrnAgtCon)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Agent Master file
     fwmagp     if   e             disk    rename(pag01:pag0p)
      * Agent Contacts File
     fwma5p     uf a e             disk    rename(pac01:pac01)
     f                                     infds(pffds)
     fwma5l     if   e           k disk    rename(pac01:pac00)
     f                                     infds(dbfds)
     fwma5pcrt  uf a e           k disk    rename(pac01:pac0p)
     f                                     prefix(PR_)
      *** Beg Add ***** 09/18/23 *****************************
      * User Parameter File
     fsmupl     if   e           k disk
      *** End Add ***** 09/18/23 *****************************
      *** Beg Add ***** 03/22/24 *****************************
      * Conatct Type File
     fstcnl     if   e           k disk
      *** End Add ***** 03/22/24 *****************************
      *** Beg Add ***** 04/11/24 *** 129WXP ******************
     fSTDsl     if   e           k disk
      *** End Add ***** 04/11/24 *** 129WXP ******************


     d psds          esds                  extname(szq1p)
     d pffds           ds
     d  pfrrn                397    400b 0

     d dbfds           ds
     d  dbrrn                397    400b 0

      // work variables
     d saveDT          s                   like(AGdt)
     d saveTM          s                   like(AGtime)
     d flgRcdUpdated   s              1    inz
     d intCo#          s              3  0
     d xxRrn           s              9  0 inz
     d indexTab        s              5  0
     d svRrnAgCo       s              9  0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d xxDelResp       s               n   inz
     d maxseq#         s             15  0 inz
     d infein          s              9  0 inz
     d inco#           s              3  0 inz
     d inmod           s              5  0 inz
     d*
      //

      // Proto-types

      // Load Agency Contacts Grid
     d LoadAgCoGrid    pr

      * Main Processing
     d Main            pr

      * Process Add Agency Contacts information
     d ProcAddAgCo     pr

      * Process other Options for Agency Contacts
     d ProcOthrAgCo    pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate Agency Contacts info procedure.
     d ValidateAgCo    pr              n
     d strmode                        1


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sprCompany
     d/copy wPrPolErr
     d/copy sPrString
     d/copy sCopyGrpPr

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
       inFein = AGfein;
       inMod = AGmod;

       If (@@next = *Blanks or @@next = 'WTAGTCON');

          ProgramStart(process);

          If process;

             Main();

          Endif;
       Endif;


       *InLr = *On;
      /end-free

       //===================================================================
       // Load Expense Constant Grid
       //===================================================================
     p LoadAgCoGrid    b
     d LoadAgCoGrid    pi

      /free

        rrnAgtCon = 0;
        dspAgtCon = *off;
        clrAgtCon = *on;
        Write rcdAgtCont;
        clrAgtCon = *off;
        dspAgtCon = *on;
        scRowSel = 1;


        // Populate the subfile
        Setll (inCo#:inFein:inMod) wma5l;
        Reade (inCo#:inFein:inMod) wma5l;
        Dow not %eof(wma5l);
           rrnAgtCon += 1;
           scAgCoSel = *off;
           // return cursor and selection to the edited/added record.
           If svRrnAgCo = *zeros;
              If rrnAgtCon = 1;
                 scAgCoSel = *on;          //returning selection highlight
              Endif;
           Else;
              If rrnAgtCon = svRrnAgCo;
                 scAgCoSel= *on;
              Endif;
           Endif;

           scLsn  = A5lsn;
           scFsn = A5fsn;
           scAreaPhn   = A5area * 10000000 + A5phn#;
           scCellPhn#  = A5cara * 10000000 + A5cphn;
           //*** Beg Add ***** 04/11/24 *** 129WXP ******************
           // Populate the Distribution Types
           Setll (inco#:inFein:inMod:A5seq#) pac0p;
           Reade(n) (inco#:inFein:inMod:A5seq#) pac0p;
           scConCrt = ' ';
           Dow not %eof(wma5pcrt);
              If PR_A5critID = 'Distribution';
                 Chain (inco#:PR_A5critVal) STDsl;
                    If %Found;
                       If scConCrt <> ' ';
                          scConCrt = %Trim(scConCrt) + ', ' + DsDesc;
                       Else;
                          scConCrt = DsDesc;
                       Endif;
                    Endif;
              Endif;
              Reade(n) (inco#:inFein:inMod:A5seq#) pac0p;
           Enddo;

           //*** End Add ***** 04/11/24 *** 129WXP ******************
           scAgCoSts = *Blanks;

           //*** Begin Chg ***** 08/28/23 *****************************
           // scActAgCo = *Blanks;
           // If A5stat = 'A';
           //    scAgCoSts = 'Active';
           //    scActAgCo = 'Inactivate';
           // Endif;
           // If A5stat = 'I';
           //    scAgCoSts = 'Inactive';
           //    scActAgCo = 'Activate';
           // Endif;

           visActSts = *off;                // New Hyperlink for Activate
           visIActSts = *off;               // New Hyperlink for Inactivate
           If A5stat = 'A';
              scAgCoSts = 'Active';
              visIActSts = *on;
           Endif;
           If A5stat = 'I';
              scAgCoSts = 'Inactive';
              visActSts = *on;
           Endif;
           //*** Beg Add ***** 03/22/24 *********************************
           Chain (A5co#:A5type) pcn01;
           If %found();
              scAConType = %trim(CNtype)+ ' - ' + %trim(CNdesc);
           Else;
              scAConType = ' ';
           Endif;
           //*** End Add ***** 03/22/24 *********************************

           //*** Beg Chg ***** 09/18/23 *****************************
           // If dspMode;
           If dspMode and UPcontUpd <> 'Y';
           //*** End Chg ***** 09/18/23 *****************************
              visActSts = *off;
              visIActSts = *off;
           Endif;
           //*** End Chg ***** 08/28/23 *****************************

           scRrnAgCo = dbrrn;

           Write sflAgtCon;
           Reade (inCo#:inFein:inMod) wma5l;
        Enddo;

        rrnAgtCon = 1;
        If svRrnAgCo <> 0;
           rrnAgtCon = svRrnAgCo;
           scRowSel = svRrnAgCo;
           svRrnAgCo = 0;
        Endif;

      /end-free
     p LoadAgCoGrid    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
      /free

       BtnCancel = *Off;

       // Process until exit button is pressed.
       DoW BtnCancel = *Off;

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Add Agency Contact info screen
          When BtnAgCoAdd = *On;
             BtnAgCoAdd = *Off;
             ProcAddAgCo();

          // For "Change FEIN" option in More Action of Agency Toolbar
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:' ':txtWinData);
             btnShowWin = *off;


          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;
             ProcOthrAgCo();          //other option for Agency Contact info

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;

       When indexTab = 1;
          pnlHeight = 610;
          APnlHeight = pnlHeight + 60;

          Clear rcdAgtCont;
          If dspMode;
             inquiry = *on;
          Endif;

          //*** Beg Add ***** 09/18/23 *****************************
          If UPcontUpd = 'Y';
             inquiry = *off;
          Endif;
          //*** End Add ***** 09/18/23 *****************************

          // Load Agency Contacts grid
          LoadAgCoGrid();

          Write rcdAgtPnl;
          Write rcdPanel;
          Exfmt rcdAgtCont;
          Read rcdAgtPnl;
          Read rcdPanel;

       EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcAddAgCo  - process addition of Agency Contacts
       //===================================================================
     P ProcAddAgCo     B
     D ProcAddAgCo     PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddAgCo;
       mode = 'A';
       isexit = *off;
       BtnAgCoCan = *off;

       Exsr MovDftVal;

       Dow BtnAgCoCan = *off and isexit = *off;

          scAgCoHdg = 'Add Agency Contacts Information';

          Exfmt rcdAddAgCo;

          Select;

          // Save Button
          When BtnAgCoSav = *on;
             BtnAgCoSav = *off;
             If ValidateAgCo(mode);
                Exsr WrtAgCo;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnAgCoCan = *on;
             BtnAgCoCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Writing Agency Contacts details subroutine
       BegSr WrtAgCo;
        Clear pac01;
        Exsr MoveScDb;
        Write pac01;
       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        A5co#   = inCo#;                 // Company Number
        A5fein  = inFein;                // Agency Fein
        A5mod   = inMod;               // Division
        A5titl  = scTitlE;               // Courtesy Title
        A5fsn   = scFsnE;               // First Name
        A5lsn   = scLsnE;               // Last Name
        A5mid   = scMidE;               // M/I
        //*** Beg Chg ***** 03/22/24 *********************************
        //***A5acty  = scActyE;              // Agency Contact Type
        A5type  = scActyE;              // Contact Type
        //*** End Chg ***** 03/22/24 *********************************
        A5posn  = scPosnE;               // Position
        A5adr1  = scAdr1E;               // Address1
        A5adr2  = scAdr2E;              // Address2
        A5cty   = scCtyE;               // City
        A5st    = scStEAC;                // State
        A5zip   = scZipE;               // Zip
        If scAreaPhnE> *zeros;
           A5area  = %dec(%subst(%editc(scAreaPhnE:'X'):1:3):3:0); // Phone  Are
           A5phn#  = %dec(%subst(%editc(scAreaPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        A5ext#  = scExt#;              //  Extention
        If scCellPhnE> *zeros;
           A5cara  = %dec(%subst(%editc(scCellPhnE:'X'):1:3):3:0); // Phone  Are
           A5cphn  = %dec(%subst(%editc(scCellPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scFaxNmbrE> *zeros;
           A5fara  = %dec(%subst(%editc(scFaxNmbrE:'X'):1:3):3:0); // Phone  Are
           A5fphn  = %dec(%subst(%editc(scFaxNmbrE:'X'):4):7:0);   // Phone  Num
        EndIf;
        A5pphf  = scPphfE;              //  Primary Phone
        A5emal  = scEmalE;              // Email
        A5stat  = scAgCoStE;           // Status
        Exec SQL Select Max(A5seq#) Into :maxseq#  From Wma5l
         Where A5co# = :inco# And A5fein = :infein And A5mod = :inmod;
        A5seq# = maxseq# + 1;

        If scProd <> ' ';
           Dow scProd <> ' ';
              PR_A5critID  = 'Producer';
              PR_A5critVal = ParseString(scProd:',':1:'1');
              Exsr Write_CritRec;
           Enddo;
        Endif;
        Dow scDisTyp <> *blank;
           PR_A5critID  = 'Distribution';
           PR_A5critVal = ParseString(scDisTyp:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scSubCat <> *blank;
           PR_A5critID  = 'Sub Category';
           PR_A5critVal = ParseString(scSubCat:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scOutCnt <> *blank;
           PR_A5critID  = 'Process Output Control';
           PR_A5critVal = ParseString(scOutCnt:',':1:'1');
           Exsr Write_CritRec;
        Enddo;

        A5user = q1user;
        A5pgm  = q1pgm;
        A5time = Timeto6(SystemTime());
        A5dt   = Dateto7(SystemDate());
        A5itim = Timeto6(SystemTime());
        A5idt  = Dateto7(SystemDate());
        A5iusr = q1user;
        A5ipgm = q1pgm;
       EndSr;

       Begsr Write_CritRec;
        PR_A5co#   = inco#;
        PR_A5fein  = infein;
        PR_A5mod   = inmod;
        PR_A5seq#  = A5seq#;
        PR_A5stat  = A5stat;
        PR_A5user  = q1user;
        PR_A5time  = Timeto6(SystemTime());
        PR_A5dt    = Dateto7(SystemDate());
        PR_A5iusr  = q1user;
        PR_A5ipgm  = q1pgm;
        PR_A5itim  = Timeto6(SystemTime());
        PR_A5idt   = Dateto7(SystemDate());
        Write pac0p;
        Unlock wma5pcrt;
       Endsr;

       BegSr MovDftVal;
        scAgCoStE = 'A';
        Clear selProd;
        Clear A5seq#;

        selProd = 'APco# = ' + %char(inco#) + ' AND APFein = '
         + %char(inFein) + ' AND APMod  = ' + %char(inMod);
        scProd = ' ';
        Setll (inco#:inFein:inMod:A5seq#) pac0p;
        Reade(n) (inco#:inFein:inMod:A5seq#) pac0p;
        Dow not %eof(wma5pcrt);
        If PR_A5critID = 'Producer';
           If scProd = ' ';
              scProd = PR_A5critVal;
           Else;
              scProd = %trim(scProd) + ',' + %trim(PR_A5critVal);
           Endif;
        Endif;
        Reade(n) (inco#:inFein:inMod:A5seq#) pac0p;
        Enddo;
       EndSr;

      /end-free
     P ProcAddAgCo     E

       //===================================================================
       // ProcOthrAgCo - Process other Options for Agency Contacts
       //===================================================================
     P ProcOthrAgCo    B
     D ProcOthrAgCo    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddAgCo;
       isexit = *off;
       ReadC sflAgtCon;

       Dow not %eof;

          Select;
          When scAgCoEdt = *On; // Processing for Edit Agency Contacts Info

             scAgCoEdt = *Off;
             mode = 'C';
             svRrnAgCo = rrnAgtCon;   //Save Rrn value  for edited record

             Chain(n) scRrnAgCo WMA5P;

             Exsr MovDbSc;

             scAgCoHdg = 'Edit Agency Contacts Information';

             Dow isexit = *off;
                Exfmt rcdAddAgCo;

                Select;
                // Save Button
                When BtnAgCoSav = *on;
                   BtnAgCoSav = *off;
                   // Check if WMA5P updated since screen displayed (Agency Contact Info)
                   flgRcdUpdated=RcdUpdatedErr('WMA5P':pfrrn:'A5dt':'A5time':
                    'A5user':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRrnAgCo WMA5P;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateAgCo(mode);
                         Chain scRrnAgCo WMA5P;
                         Exsr MoveScDb;
                         Update pac01;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnAgCoCan = *on;
                   BtnAgCoCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WMA5P;

          When scAgCoDlt= *On; // Processing for Delete Agency Contacts Link

             scAgCoDlt = *Off;

             Chain(e) scRrnAgCo WMA5P;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again lat+
                er.':'Record Lock');
                Return;
             Endif;

             Chain scRrnAgCo WMA5P;
             Exsr MovDbSc;

             scAgCoHdg = 'Delete Agency Contacts Information';

             Dow isexit = *off;

                Write rcdAddAgCo;
                xxDelresp = PopTwoBtn('Do you want to delete this Agency +
                            Contact?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;

                   Setll (A5co#:A5fein:A5mod:A5seq#) pac0p;
                   Reade (A5co#:A5fein:A5mod:A5seq#) pac0p;
                   Dow not %eof(wma5pcrt);
                      Delete wma5pcrt;
                      Reade (A5co#:A5fein:A5mod:A5seq#) pac0p;
                   Enddo;
                   Unlock wma5pcrt;

                   Delete pac01;
                   PopOneBtn('Agency Contacts Deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svRrnAgCo = rrnAgtCon;   //Save Rrn value
                   isexit = *on;
                EndSl;
             EndDo;
          Endsl;

          ReadC sflAgtCon;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scTitlE = A5titl;
        scFsnE = A5fsn;
        scLsnE = A5lsn;
        scMidE = A5mid;
        scPosnE = A5posn;
        scAdr1E = A5adr1;
        scAdr2E = A5adr2;
        scCtyE =  A5cty;
        scStEAC = A5st;
        scZipE = A5zip;
        //*** Beg Chg ***** 03/22/24 *********************************
        //***scActyE = A5acty;
        scActyE = A5type;
        //*** End Chg ***** 03/22/24 *********************************
        scAgCoStE =A5stat;
        scAreaPhnE  =   A5area * 10000000 + A5phn#;
        scExt# = A5ext#;
        scCellPhnE  =   A5cara * 10000000 + A5cphn;
        scFaxNmbrE  =   A5fara * 10000000 + A5fphn;
        scPphfE = A5pphf;
        scEmalE = A5emal;

        Clear selProd;
        selProd = 'APco# = ' + %char(inco#) + ' AND APFein = '
         + %char(inFein) + ' AND APMod  = ' + %char(inMod);
        scProd = ' ';
        Clear scDisTyp;
        Clear scSubCat;
        Clear scOutCnt;
        Setll (inco#:inFein:inMod:A5seq#) pac0p;
        Reade(n) (inco#:inFein:inMod:A5seq#) pac0p;
        Dow not %eof(wma5pcrt);
           Select;
           When PR_A5critID = 'Producer';
              scProd = %trim(scProd) + ',' + %trim(PR_A5critVal);
           When PR_A5critID = 'Distribution';
              scDisTyp = %trim(scDisTyp) + ',' + %trim(PR_A5critVal);
           When PR_A5critID = 'Sub Category';
              scSubCat = %trim(scSubCat) + ',' + %trim(PR_A5critVal);
           When PR_A5critID = 'Process Output Control';
              scOutCnt = %trim(scOutCnt) + ',' + %trim(PR_A5critVal);
           Endsl;
        Reade(n) (inco#:inFein:inMod:A5seq#) pac0p;
        Enddo;

        If scProd <> *blank;
           scProd = %subst(scProd:2);
        Endif;
        If scDisTyp <> *blank;
           scDisTyp = %subst(scDisTyp:2);
        Endif;
        If scSubCat <> *blank;
           scSubCat = %subst(scSubCat:2);
        Endif;
        If scOutCnt <> *blank;
           scOutCnt = %subst(scOutCnt:2);
        Endif;

        saveDT = A5dt;
        saveTM = A5time;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        A5titl  = scTitlE;               // Courtesy Title
        A5fsn   = scFsnE;               // First Name
        A5lsn   = scLsnE;               // Last Name
        A5mid   = scMidE;               // M/I
        //*** Beg Chg ***** 03/22/24 *********************************
        //***A5acty  = scActyE;              // Agency Contact Type
        A5type  = scActyE;              // Contact Type
        //*** End Chg ***** 03/22/24 *********************************
        A5posn  = scPosnE;               // Position
        A5adr1  = scAdr1E;               // Address1
        A5adr2  = scAdr2E;              // Address2
        A5cty   = scCtyE;               // City
        A5st    = scStEAC;                // State
        A5zip   = scZipE;               // Zip
        If scAreaPhnE> *zeros;
           A5area  = %dec(%subst(%editc(scAreaPhnE:'X'):1:3):3:0); // Phone  Are
           A5phn#  = %dec(%subst(%editc(scAreaPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scCellPhnE> *zeros;
           A5cara  = %dec(%subst(%editc(scCellPhnE:'X'):1:3):3:0); // Phone  Are
           A5cphn  = %dec(%subst(%editc(scCellPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scFaxNmbrE> *zeros;
           A5fara  = %dec(%subst(%editc(scFaxNmbrE:'X'):1:3):3:0); // Phone  Are
           A5fphn  = %dec(%subst(%editc(scFaxNmbrE:'X'):4):7:0);   // Phone  Num
        EndIf;
        A5pphf  = scPphfE;              //  Primary Phone
        A5emal  = scEmalE;              // Email
        A5stat  = scAgCoStE;           // Status

        Setll (inco#:infein:inmod:A5seq#) pac0p;
        Reade (inco#:infein:inmod:A5seq#) pac0p;
        Dow not %eof(wma5pcrt);
           Delete wma5pcrt;
           Reade (inco#:infein:inmod:A5seq#) pac0p;
        Enddo;
        Unlock wma5pcrt;
        If scProd <> ' ';
           Dow scProd <> ' ';
              PR_A5critID  = 'Producer';
              PR_A5critVal = ParseString(scProd:',':1:'1');
              Exsr Write_CritRec;
           Enddo;
        Endif;
        Dow scDisTyp <> *blank;
           PR_A5critID  = 'Distribution';
           PR_A5critVal = ParseString(scDisTyp:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scSubCat <> *blank;
           PR_A5critID  = 'Sub Category';
           PR_A5critVal = ParseString(scSubCat:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        Dow scOutCnt <> *blank;
           PR_A5critID  = 'Process Output Control';
           PR_A5critVal = ParseString(scOutCnt:',':1:'1');
           Exsr Write_CritRec;
        Enddo;
        A5user = q1user;
        A5pgm  = q1pgm;
        A5time = Timeto6(SystemTime());
        A5dt   = Dateto7(SystemDate());

        saveDT = A5dt;
        saveTM = A5time;
       EndSr;

       Begsr Write_CritRec;
        PR_A5co#   = inco#;
        PR_A5fein  = infein;
        PR_A5mod   = inmod;
        PR_A5seq#  = A5seq#;
        PR_A5stat  = A5stat;
        PR_A5user  = q1user;
        PR_A5time  = Timeto6(SystemTime());
        PR_A5dt    = Dateto7(SystemDate());
        Write pac0p;
        Unlock wma5pcrt;
       Endsr;

      /end-free
     P ProcOthrAgCo    E

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
       //*** Beg Add ***** 09/18/23 *****************************
      // Get User Parameter Info
       Chain q1user pup01;
       //*** End Add ***** 09/18/23 *****************************
       // Update Action Panel Info (Rrn, Agency Name)
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

       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate Agency Contact info
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAgCo    b
     d ValidateAgCo    pi              n
     d strmode                        1
     d isErrAgCoEdt    s               n   inz
      /Free

       isErrAgCoEdt = *off;
      // Intialize all the messageId and error indicators.
       erLsnE = *off;
       msLsnE = *blanks;
       erTitlE = *off;
       msTitlE = *blanks;
       erPphfE = *off;
       msPphfE = *blanks;
       erMidE = *off;
       msMidE = *blanks;
       erActyE = *off;
       msActyE = *blanks;
       erZipE  = *off;
       msZipE  = *blanks;
       erStEAC = *off;
       msStEAC = *blanks;
       erCtyE  = *off;
       msCtyE  = *blanks;
       erAdr1E  = *off;
       msAdr1E  = *blanks;

       //Last Name is required entry
       If scLsnE = *Blanks;
          erLsnE = *on;
          msLsnE = 'WCR0503';
          isErrAgCoEdt = *on;
       Endif;
       If scFsnE = *Blanks and scTitlE = *Blanks;
          erFsnE = *on;
          msFsnE = 'WC31407';
          erTitlE = *on;
          msTitlE = 'WC31407';
          isErrAgCoEdt = *on;
       Endif;
       If scAreaPhnE <> *Zeros and scCellPhnE <> *Zeros and scPphfE = *Blanks;
          erPphfE = *on;
          msPphfE = 'WC31410';
          isErrAgCoEdt = *on;
       Endif;
       If strmode= 'A';
          Chain (inCo#:inFein:inMod:scLsnE:scFsnE:scMidE) pac00;
          //*** Beg Chg ***** 03/22/24 *******************************
          //***If %found(wma5l) and scActyE= A5acty and dbrrn <> scRrnAgCo;
          If %found(wma5l) and scActyE= A5type and dbrrn <> scRrnAgCo;
          //*** End Chg ***** 03/22/24 *******************************
             erLsnE = *on;
             msLsnE = 'WC30108';
             erFsnE = *on;
             msFsnE = 'WC30108';
             erMidE = *on;
             msMidE = 'WC30108';
             erActyE = *on;
             msActyE = 'WC30108';
             isErrAgCoEdt = *on;
          Endif;
       Endif;

       If strmode= 'C';
          setll (inCo#:inFein:inMod:scLsnE:scFsnE:scMidE) pac00;
          Reade (inCo#:inFein:inMod:scLsnE:scFsnE:scMidE) pac00;
          Dow Not %Eof(wma5l);
             //*** Beg Chg ***** 03/22/24 ****************************
             //***If scActyE= A5acty and dbrrn <> scRrnAgCo;
             If scActyE= A5type and dbrrn <> scRrnAgCo;
             //*** End Chg ***** 03/22/24 ****************************
                erLsnE = *on;
                msLsnE = 'WC30108';
                erFsnE = *on;
                msFsnE = 'WC30108';
                erMidE = *on;
                msMidE = 'WC30108';
                erActyE = *on;
                msActyE = 'WC30108';
                isErrAgCoEdt = *on;
                leave;
             Endif;
          Reade (inCo#:inFein:inMod:scLsnE:scFsnE:scMidE) pac00;
          Enddo;
       Endif;
      // Enter complete info for Physical address, if any of the field is entered
       If scAdr1E <> *blanks or scAdr2E <> *blanks or
        scCtyE <> *blanks or scStEAC <> *blanks or scZipE <> 0;
          If scAdr1E = *Blanks;
             erAdr1E = *on;
             msAdr1E = 'WCR0503';
             isErrAgCoEdt = *on;
          Endif;
          If scCtyE = *blanks;
             erCtyE = *on;
             msCtyE = 'WCR0503';
             isErrAgCoEdt = *on;
          Endif;
          If scStEAC = *blanks;
             erStEAC = *on;
             msStEAC = 'WCR0503';
             isErrAgCoEdt = *on;
          Endif;
          If scZipE = *zeros;
             erZipE = *on;
             msZipE = 'WCR0556';
             isErrAgCoEdt = *on;
          Endif;
       Endif;
      // Return *on indicator when No error exists.
       If isErrAgCoEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateAgCo    e

      // utility procedure definitions
      /COPY SCOPYCOMPI

