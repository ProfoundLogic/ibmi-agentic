     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title SC996R - Darts Image/Notes Transfer - Prompt
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Prompt for a date range and a diary
     h*                type and this will be used to Extract Darts Notes and Images
     h*
     h*  Called from: Menu
     h*  Calls to:    SC996C - Darts Image/Notes Extract - Submitter
     h*
     h**********************************************************************
     h*  Created on 10/01/2018 by C:180 for Project: 129WVD
     h**********************************************************************
     h*  Revisions:
      *                                                               *
     h*
     h**********************************************************************
     fsc996f    cf   e             workstn extfile('SC996F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fsmcol     if   e           k disk    rename(pco01:pco00)
     fsmupl     if   e           k disk    rename(pup01:pup00)

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

     d* Check to see if group select will be used
     d UseGroupSel     pr              n

     d* Group Select
     d GroupSelect     pr                  extpgm('W7944C')
     d ##ProgramID                   10    const

     d* Check to see if submitted
     d ChkSubmitted    pr                  ExtPgm('SC996D')
     D  callIn                        1
     D  modeIn                        1

     d* Submitter
     d Submitter       pr                  ExtPgm('SC996C')
     d                                7    Const
     d                                7    Const
     d                               10    Const
     d                                 n   Const
     d                                1    Const

     d/copy sPrCompany
     d/copy sPrDate
     d/copy wPrUsrPrms

     d**********************************************************************
     d* Work Fields
     d**********************************************************************
     d intCo#          s              3  0
     d clientID        s              3    inz('000')
     d userOutQ        s             10    inz(' ')
     d fromDate        s               d   DatFmt(*usa)
     d toDate          s               d   DatFmt(*usa)
     d callIn          s             40    Inz('C')
     d returnCodeOut   s              1
     d isGrpSel        s               n   Inz(*Off)

     d**********************************************************************
     d* Data Structures
     d**********************************************************************

     d**********************************************************************
     d* Constants
     d**********************************************************************
     d docExists       c                   0                                    Exists
     d docAccess       c                   4                                    Access
     d streamFile      c                   '*STMF'
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

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

     D error           s               n   Inz(*Off)

      /free

       // Start Of Program
       ProgramStart();

       // Process until exit button is pressed.
       Dou  ExitNow = *On Or (Submit = *On And error = *Off);

          // Process Payment Value 1 code Screens
          Write rcdComPnl;
          Write rcdDrtExt;
          ExFmt rcdDrtExt0;
          Read rcdComPnl;
          Read rcdDrtExt;

          // Clear Error Indicators
          ClearErrors();

          If ExitNow = *Off;
             Select;
             When Submit = *On;
                error = errorCheck();
             EndSl;
          EndIf;

          // Submit the Job
          If ExitNow = *Off And Submit = *On And Error = *Off;
             isGrpSel = UseGroupSel();

             // Call Group Select Process
             If isGrpSel = *On;
                GroupSelect('SC996D');

                //Check if group select submitted the job
                returnCodeOut = ' ';
                ChkSubmitted(callIn:returnCodeOut);
             Else;
                returnCodeOut = ' ';
             EndIf;

             // Submit the job
             If returnCodeOut = 'S';
                Submitter(%EditC(dtCYMD(scfromDt):'X'):
                          %EditC(dtCYMD(scToDate):'X'):
                          scDryType:isGrpSel:scRTmode);
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
       If scDryType = 'W0070R' or scDryType = 'W0050R';
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

       Clear mgFromDt;
       Clear erFromDt;
       Clear mgToDate;
       Clear erToDate;
       Clear mgDryType;
       Clear erDryType;
       Clear mgRTmode;
       Clear erRTmode;

      /end-free

     p ClearErrors     e
       //===================================================================
       // ErrorCheck - Error Check
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     D error           s               n   Inz(*Off)
     d validDates      s               n   Inz(*On)

      /free

       // From Date Must Be Valid
       If dtCheck(scFromDt)=0;
          mgFromDt   = 'Invalid date entered.';
          erFromDt   = *On;
          error      = *On;
          validDates = *Off;
       Else;
         scFromDt = dtCheck(scFromDt);
         fromDate = cvtToDate(scFromDt);
       EndIf;

       // To Date Must Be Valid
       If dtCheck(scToDate)=0;
          mgToDate   = 'Invalid date entered.';
          erToDate   = *On;
          error      = *On;
          validDates = *Off;
       Else;
         scToDate = dtCheck(scToDate);
         toDate   = cvtToDate(scToDate);
       EndIf;

       // Dates must be in ascending order
       If validDates = *On And toDate < fromDate;
          mgFromDt = 'Dates must be in ascending order.';
          erFromDt = *On;
          mgToDate = 'Dates must be in ascending order.';
          erToDate = *On;
          error    = *On;
       EndIf;

       // Diary Type must be selected
       If scDryType = ' ';
          mgDryType = 'Diary Type must be selected.';
          erDryType = *On;
          error     = *On;
       EndIf;

       // Mode must be selected
       If scRtmode    = ' ';
          mgRtmode = 'Mode must be selected.';
          erRtmode  = *On;
          error     = *On;
       EndIf;

       Return error;

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

       // Get User Output Queue
       userOutQ = GetUsrOutQ(q1User);

      /end-free
     p ProgramStart    e
