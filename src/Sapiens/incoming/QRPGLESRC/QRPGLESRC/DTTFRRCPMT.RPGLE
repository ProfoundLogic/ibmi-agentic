     HCopyright('(c) 2020 Sapiens International. All Rights Reserved.')
     h**********************************************************************
     h/title DTTFRRCPMT - iDarts Tranfer by Remark Code - Prompt
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This program will prompt for remarks to be
     h*                transfered.                                           Images
     h*
     h*  Called from: Menu
     h*  Calls to:    DTTFRRCSBM - Darts Remark Transfer - Submitter
     h*
     h**********************************************************************
     h*  Created on 10/01/2018 by C:180 for Project: 129WVT
     h**********************************************************************
     h*  Revisions:
      *                                                               *
     h*
     h**********************************************************************
     fDTTFRRCPMTcf   e             workstn extfile('DTTFRRCPMT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fsmcol     if   e           k disk    rename(pco01:pco00)
     fsmupl     if   e           k disk    rename(pup01:pup00)
     fWTGSP     Uf   e           k disk

     d**********************************************************************
     d* System Information
     d**********************************************************************
     d psds          esds                  extname(szq1p)

     d**********************************************************************
     d* Prototypes
     d**********************************************************************

     d* Clear Error Indicators
     d ClearErrors     pr

     d* Error Check
     d ErrorCheck      pr              n

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d clientID                       3

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr

     d* Group Select
     d GroupSelect     pr                  extpgm('W7944C')
     d ##ProgramID                   10    const


     d/copy sPrCompany
     d/copy sPrDate
     d/copy wPrUsrPrms

     d**********************************************************************
     d* Work Fields
     d**********************************************************************
     d intCo#          s              3  0
     d clientID        s              3    inz('000')
     d kkSbmPgm        s             10    inz('DTTFRRCSBM')


      /free

       // Main Procesing
       Main();

       *inlr = *on;

      /end-free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     D isError         s               n   Inz(*Off)

      /free

       // Start Of Program
       ProgramStart();

       // Process until exit button is pressed.
       Dou  BtnCancel = *On Or (BtnSubmit = *On And isError = *Off);

          // Process Payment Value 1 code Screens
          Write rcdComPnl;
          Write RcdDrtRmk1;
          ExFmt RcdDrtRmk2;
          Read rcdComPnl;
          Read RcdDrtRmk1;

          // Clear Error Indicators
          ClearErrors();

          If BtnCancel = *Off;
             Select;
             When BtnSubmit = *On;
                isError = errorCheck();
             EndSl;
          EndIf;

          // Submit the Job
          If BtnCancel = *Off And BtnSubmit = *On And isError = *Off;
             chain kkSbmPgm PGS01;
             If %found(WTGSP);
                GSprds = scFromRmk + scToRmk;
                Update PGS01;
                GroupSelect('DTTFRRCSBM');
             Endif;
          EndIf;

       Enddo;

      /end-free
     p Main            e
       //==============================================================
       // UseGroupSel - Check to see if we will be using the group sel
       //==============================================================
     p UseGroupSel     b
     d UseGroupSel     pi              n

      /free
       If Q1pgm = 'DTTFRRCPMT';
          return *On;
       Else;
          return *Off;
       EndIf;
      /end-free

     p UseGroupSel     e
       //===================================================================
       // ClearErrors - Clears Error Indicators
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi

      /free

       Clear msFromRmk;
       Clear erFromRmk;
       Clear msToRmk;
       Clear erToRmk;

      /end-free

     p ClearErrors     e
       //===================================================================
       // ErrorCheck - Error Check
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     D isError         s               n   Inz(*Off)

      /free

       // From and To remarks cannot be the same.
       If scFromRmk = scToRmk;
          msFromRmk  = 'FROM and TO remarks cannot be the same.';
          erFromRmk   = *On;
          isError    = *On;
       EndIf;

       Return isError;

      /end-free

     p ErrorCheck      e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

     d suffix          s              3    Inz(' ')

      /free

       // Get the client id
       GetClientID(clientID);

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);

       // Set Action Panel
       APnlHeight = 670;

      /end-free
     p ProgramStart    e
