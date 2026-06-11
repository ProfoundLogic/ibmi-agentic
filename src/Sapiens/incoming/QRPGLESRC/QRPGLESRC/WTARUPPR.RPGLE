     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WTARUPPR - A/R Batch Upload Prompt
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Prompt for a document location that
     h*                will then be used to import the A/R Batch Information
     h*
     h*  Called from: Menu
     h*  Calls to:    WTARUPSBM
     h*
     h**********************************************************************
     h*  Created on 05/10/2017 by C:062 for Project: 129WUR
     h**********************************************************************
     h*  Revisions:
      *                                                               *
      *  05/19/23 C:333 Support: 110252                               *
      *                 A/R batch upload screen Lock error Handle     *
      *                                                               *
     h*
     h**********************************************************************
     fwtaruppr  cf   e             workstn extfile('WTARUPPR')
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
     D* Check for Document Authorities
     D access          pr            10I 0 ExtProc('access')
     D   path                          *   Value Options(*string)
     D   aMode                       10I 0 Value

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

     d* Retrieve Document Stats
     d stat            PR            10I 0 ExtProc('stat')
     d   path                          *   value options(*string)
     d   buf                           *   value

     d* Update Path Data Area
     d UpdateDtaAra    pr

     d* Override
     d WtArUpOvr       pr                  ExtPgm('WTARUPOVR')
     d pathIn                       256
     D queueIn                       10
     D errordesc                     10

     d/copy SPrCompany
     d/copy wPrUsrPrms
     d/copy sPrGenPop

     d**********************************************************************
     d* Work Fields
     d**********************************************************************
     d intCo#          s              3  0
     d clientID        s              3    inz('000')
     d userOutQ        s             10    inz(' ')
     d errorDesc       s             10    inz(' ')
     d objectType      s             12

      //***** Beg Chg *** 05/19/23 *****************************************
     d*wtArUpld       uds                  dtaara(WTARUPLD)
     d wtArUpld        ds                  dtaara(WTARUPLD)
      //***** End Chg *** 05/19/23 *****************************************
     d storedPath              1    255

     d**********************************************************************
     d* Data Structures
     d**********************************************************************
     d p_statds        S               *
     d statds          DS                  BASED(p_statds)
     d  st_mode                      10U 0
     d  st_ino                       10U 0
     d  st_nlink                      5U 0
     d  st_pad                        2A
     d  st_uid                       10U 0
     d  st_gid                       10U 0
     d  st_size                      10I 0
     d  st_atime                     10I 0
     d  st_mtime                     10I 0
     d  st_ctime                     10I 0
     d  st_dev                       10U 0
     d  st_blksize                   10U 0
     d  st_allocsize                 10U 0
     d  st_objtype                   10A
     d  st_codepage                   5U 0
     d  st_reserved1                 62A
     d  st_ino_gen_id                10U 0

     D myStat          S                   like(statds)
     d pathOut         s            256

     d**********************************************************************
     d* Constants
     d**********************************************************************
     d docExists       c                   0                                    Exists
     d docAccess       c                   4                                    Access
     d streamFile      c                   '*STMF'
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      //***** Beg Add *** 05/19/23 *****************************************
     d @@title         s             50a
     d @@message       s            256a
     d @@btn           s               n
      //***** End Add *** 05/19/23 *****************************************
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

       Clear mgImpPath;
       Clear erImpPath;

      /end-free

     p ClearErrors     e
       //===================================================================
       // ErrorCheck - Error Check
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     D error           s               n   Inz(*Off)

      /free

       // Path is Required
       If scImpPath = *Blanks;
          mgImpPath = 'This field can not be blank.';
          erImpPath = *On;
          error     = *On;
       Else;
          // Must end in .TXT
          If %Scan('.TXT':%Xlate(lo:hi:scImpPath)) = 0;
             mgImpPath = 'The document must end in .TXT.';
             erImpPath = *On;
             error     = *On;
          Else;
             // Document must exist.
             If Stat(%Trim(scImpPath):%Addr(myStat)) < 0;
                mgImpPath = 'Invalid Document Path.';
                erImpPath = *On;
                error     = *On;
             Else;
                p_statds = %Addr(myStat);
                objectType = st_ObjType;
                If objectType <> streamFile;
                   mgImpPath = 'Path doesn''t include document.';
                   erImpPath = *On;
                   error     = *On;
                Else;
                   If Access(%Trim(scImpPath):docAccess) < 0;
                      mgImpPath = 'Not Authorized to Access this document.';
                      erImpPath = *On;
                      error     = *On;
                   EndIf;
                EndIf;
             EndIf;
          EndIf;
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

       //***** Beg Add *** 05/19/23 *******************************
       if @@btn <> *on;
       //***** End Add *** 05/19/23 *******************************
          // Process until exit button is pressed.
          Dou  ExitNow = *On Or (Submit = *On And error = *Off);

             // Process Payment Value 1 code Screens
             Write rcdComPnl;
             Write rcdArUpld;
             ExFmt rcdArUpld0;
             Read rcdComPnl;
             Read rcdArUpld;

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
                // Update the Data area holding document path
                UpdateDtaAra();
                // Submit the job
                pathOut = scImpPath + '*';
                WtArUpOvr(pathOut:userOutQ:errorDesc);
                // Check for Errors in the Download
                Select;
                When errorDesc = 'FILE LOCK';
                   PopOneBtn('Import file could not be allocated':'Error');
                   Leave;
                When errorDesc = 'BAD DATA';
                   PopOneBtn('Data upload unsuccessful. Check error -
                    report':'Error');
                   Leave;
                Other;
                   PopOneBtn('Data upload successful':'Info');
                   Leave;
                Endsl;
             EndIf;

          Enddo;

       //***** Beg Add *** 05/19/23 *******************************
       Endif;
       //***** End Add *** 05/19/23 *******************************
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

       // Get Stored Path
       //***** Beg Chg *** 05/19/23 *******************************
       //***In *Lock WtArUpld;
       Monitor;
          In *Lock WtArUpld;
       On-Error 431;
             @@title = 'Lock Error';
             @@message = 'Process currently in use!! Please try again later.';
             @@btn = PopOneBtn(@@message:@@title);
       EndMon;
       //***** End Chg *** 05/19/23 *******************************
       scImpPath = storedPath;

      /end-free
     p ProgramStart    e
       //===================================================================
       // UpdateDtaAra - Update Stored Path Data Area
       //===================================================================
     p UpdateDtaAra    b
     d UpdateDtaAra    pi

      /free

       storedPath = scImpPath;
       Out WtArUpld;

      /end-free

     p UpdateDtaAra    e
