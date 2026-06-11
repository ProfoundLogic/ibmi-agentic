     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title SC990R - Darts Image/Notes Extract - Prompt
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
     h*  Calls to:    SC990C - Darts Image/Notes Extract - Submitter
     h*
     h**********************************************************************
     h*  Created on 06/11/2017 by C:062 for Project: 81WLL
     h**********************************************************************
     h*  Revisions:
      *                                                               *
     h*
     h**********************************************************************
     fsc990f    cf   e             workstn extfile('SC990F')
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

     d* Submitter
     d SC990C          pr                  ExtPgm('SC990C')
     d fromDate                       8    Const
     D toDate                         8    Const
     d fromDate7                      7    Const
     D toDate7                        7    Const
     D diaryType                     10    Const
     D time                           6    Const

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

       Return error;

      /end-free

     p ErrorCheck      e
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
             // Submit the job
             SC990C(%EditC(scFromDt:'X'):%EditC(scToDate:'X'):
                    %EditC(dtCYMD(scfromDt):'X'):%EditC(dtCYMD(scToDate):'X'):
                    scDryType:%EditC(TimeTo6(systemTime()):'X'));
          EndIf;

       Enddo;

      /end-free
     p Main            e
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
