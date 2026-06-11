     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title WTSSNDFT - SSN/FEIN Report Default Masking
     h dftname(WTSSNDFT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/22/24                                         *
      //  Programmer: C:333                                            *
      //  Project   : WC2941 - Redesign of Work with SSN/FEIN Default  *
      //                       Table Information                       *
      //                                                               *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWtSsnDft  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflSsnDft:scSflRRN)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     f*  Physical file definition (for refreshing subfile record)
     fwtsfp     uf   e             disk
     f                                     infds(dbfds)
     f
     f*  Primary logical file definition (for displaying items)
     fwtsfl     if   e           k disk
     f                                     infds(dbfds1)
     f                                     rename(psf01:psf01l)

     d dbfds           ds
     d  dbrrn                397    400b 0

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d psds          esds                  extname(szq1p)

      // work variables
     d intCo#          s              3P 0
     d svsflrrn        s              5  0 inz
     d isError         s               n   inz
     d flgRcdUpdated   s              1    inz

      * Load SSN/FEIN Default Criteria
     d LoadSsnDft      pr

      * Main Processing
     d Main            pr

      * Process Edit option for Report Information
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Validate Report Edits
     d ValidateSsnCri  pr              n
      *
     d/copy sPrDate
     d/copy sPrCompany
     d/copy sPrGenPop
     d/copy wPrUsrPrms
     d/copy sPrString

      /Free

       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // LoadSsnDft - Load SSN/FEIN Report Information
       //===================================================================
     p LoadSsnDft      b
     d LoadSsnDft      pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdSsnDft;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll intCo# Wtsfl;
       Reade intCo# Wtsfl;
       Dow not %eof(Wtsfl);

          scSflRRN += 1;
          scSflSel = *off;
 
          If svsflrrn = 0;
             If scSflRRN = 1;
                scSflSel = *on;
             Endif;
          Else;
             If scSflRRN = svsflrrn;
                scSflSel = *on;
             Endif;
          Endif;
 
          scRpId   = SFrpid;
          scDesc   = SFdesc;

          Select;
           When SFflag = 'Y';
              scFlag = 'Yes';
           When SFflag = '4';
              scFlag = 'Show Last Four Digits';
           When SFflag = 'N';
              scFlag = 'No';
          Endsl;

          scTy01 = SFty01;
          scTy02 = SFty02;
          scTy03 = SFty03;
          scTy04 = SFty04;
          scTy05 = SFty05;
          scTy06 = SFty06;
          scScType = scTy01+' '+scTy02+' '+scTy03+' '+scTy04+' '+scTy05 +
                     ' '+scTy06;
          scScLevel = SFlvl;

          scRRN = dbrrn1;

          Write sflSsnDft;

          Reade intCo# Wtsfl;

       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadSsnDft      e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       BtnCancel = *off;

       // Process until exit button is pressed.
       DoU BtnCancel = *on;

          // Load SSN/FEIN default criteria
          LoadSsnDft();

          // Display Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;
          // Refresh
          When btnRefresh = *on;
             btnRefresh = *off;

          // Save
          When btnSave = *on;
             btnSave = *off;

          Other;
             ProcOther();             // Process edit link

          Endsl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When TabIndex = 0;
           PnlHeight = 400;
           APnlHeight = PnlHeight + 60;

           Write rcdComPnl;
           Write rcdPanel;

           Exfmt rcdSsnDft;

           Read rcdComPnl;
           Read rcdPanel;
        Endsl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProcOther - Process other Options for Report Information
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdSsnEdit;
       isexit = *off;

       ReadC sflSsnDft;
       Dow not %eof;

          Select;
          // Edit Process
          When scSsnEdt  = *on;
             scSsnEdt = *off;

             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRN psf01;
             Exsr Movdbsc;

             Dow isexit = *off;
                Exfmt rcdSsnEdit;

                Select;
                When BtnSaveCri = *on;
                   BtnSaveCri = *off;
                   // Check if WTSFL updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('WTSFL':dbrrn:'SFdt':'SFtime':
                    'SFuser':SFdt:SFtime:'1');
                   If flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN psf01;
                      Exsr Movdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate the Report Information
                      If ValidateSsnCri();
                         Chain scRRN psf01;
                         Exsr Movscdb;
                         Update psf01;
                         isexit = *on;
                      EndIf;
                   EndIf;
                When BtnCanCri = *on;
                   BtnCanCri = *off;
                   isExit = *on;
                Endsl;
             Enddo;
          Endsl;
          ReadC sflSsnDft;
       Enddo;

       //===================================================================
       // MovDbsc - Subrotine to move database fields into Edit screen fields
       //===================================================================
       BegSr MovDbsc;

        scEdRpId = SFrpid;
        scEdDesc = SFdesc;
        scEdFlag = SFflag;
        scEdTy01 = SFty01;
        scEdTy02 = SFty02;
        scEdTy03 = SFty03;
        scEdTy04 = SFty04;
        scEdTy05 = SFty05;
        scEdTy06 = SFty06;
        scEdScLvl = SFlvl;

       Endsr;

       //===================================================================
       // Movscdb - Subrotine to move screen field to database file
       //===================================================================
       BegSr Movscdb;

        SFflag = scEdFlag;
        SFty01 = Upper(scEdTy01);
        SFty02 = Upper(scEdTy02);
        SFty03 = Upper(scEdTy03);
        SFty04 = Upper(scEdTy04);
        SFty05 = Upper(scEdTy05);
        SFty06 = Upper(scEdTy06);
        SFlvl = scEdScLvl;
        SFuser  = Q1user;
        SFpgm   = Q1pgm;
        SFtime  = Timeto6(SystemTime());
        SFdt    = Dateto7(SystemDate());

       Endsr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // ValidateSsnCri - Validate Report Information Edits
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateSsnCri  b
     d ValidateSsnCri  pi              n
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erSecurity = *off;
        msSecurity = *blanks;

        // Check if Security type is blank
        If scEdTy01 = *blanks and scEdTy02 = *blanks and scEdTy03 = *blanks
          and scEdTy04 = *blanks and scEdTy05 = *blanks and scEdTy06 = *blanks
          and scEdScLvl = *zeros;
           erSecurity = *on;
           msSecurity = 'WC32431';
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateSsnCri  e

