     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title W8156R - Gallagher-Basset Claim Update
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program prompts for claim update information
     h*                and allows the submission of the update process
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 9/20/2016 by C:062 for Project: 114WYV
     h**********************************************************************
     h*  Revisions:
     h*
     h**********************************************************************
     fw8156f    cf   e             workstn extfile('W8156F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     InfDs(fids01)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fwagbip    uf a e             disk    rename(pgbi01:pgbi0p)

     D fids01        e ds                  ExtName(szq2p)
     d psds          esds                  extname(szq1p)
      * constant
     d intFlgDim       c                   10

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d mode            s              1
     d intCo#          s              3  0
     d clientID        s              3
     d docPath         s            201
     d savePath        s            111

     DChkIfsObj        pr                  extPgm('CHKIFSOBJ')
     D docName                      256    const
     D found                           n

     d* Error Check
     d ErrorCheck      pr              n

     d* Get Client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d clientID                       3

     d* Load Screen
     d LoadScreen      pr

     d* Program Start
     d ProgramStart    pr

     d* Submitter
     d Submitter       pr                  extpgm('W8156C')
     d mode                           1
     d docPath                      201
     d savePath                     111

     d* Update WAGBIP
     d UpdateWAGBIP    pr

     d/copy SPrCompany
     d/copy wPrUsrPrms
    
     c* Entry of Program.
     c     *entry        plist
     c                   parm                    mode

      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       DoU cancelNow = *On or submitNow = *On;

          // Load Screen
          LoadScreen();

          // Process Screen
          Select;
          When cancelNow = *On;
             Leave;
          When submitNow = *On;
             If ErrorCheck() = *Off;
                UpdateWAGBIP();
                Submitter(mode:docPath:savePath);
                Leave;
             Else;
                submitNow = *Off;
             EndIf;
          EndSl;

       EndDo;

       *inlr = *on;
       return;
      /end-free
       //===================================================================
       // ErrorCheck - Error Check
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     d docName         s            256
     d error           s               n   Inz(*Off)
     d found           s               n   Inz(*Off)
      /free

       // Folder must be entered
       If scFolder = ' ';
          mgFolder = 'Folder must be entered';
          erFolder = *On;
          error = *On;
       EndIf;

       // Document must be entered
       If scDocument = ' ';
          mgDocument = 'Document must be entered';
          erDocument = *On;
          error = *On;
       EndIf;

       // folder must be valid
       If scFolder <> ' ';
          If %Subst(%Trim(scFolder):1:1) = '/';
             docName = scfolder;
          Else;
             docName = '/' + %Trim(scfolder);
          EndIf;

          found = *Off;

          ChkIfsObj(docName:found);
          If Not found;
             mgFolder = 'Folder Doesn''t Exist';
             erFolder = *On;
             error = *On;
          Else;
             savePath = ' ';
             savePath = %Trim(docName) + '/SAVE';
             %Subst(savePath:111:1) = '*';
          EndIf;
       EndIf;

       // Document path must be valid
       If scFolder <> ' ' And scDocument <> ' ';
          If %Subst(%Trim(scFolder):1:1) = '/';
             docName = %Trim(scFolder) + '/' + %Trim(scDocument);
          Else;
             docName = '/' + %Trim(scfolder) + '/' + %Trim(scDocument);
          EndIf;

          found = *Off;

          ChkIfsObj(docName:found);
          If Not found;
             mgDocument = 'Document Doesn''t Exist';
             erDocument = *On;
             error = *On;
          Else;
             docPath = ' ';
             docPath = %Subst(docName:1:200) + '*';
          EndIf;
       EndIf;

       Return error;

      /end-free
     p ErrorCheck      e
       //===================================================================
       // LoadScreen - Load the Screen
       //===================================================================
     p LoadScreen      b
     d LoadScreen      pi

      /free

       Write rcdComPnl;
       Write rcdGBUpdat;
       Exfmt rcdGBUpdPn;
       Read rcdComPnl;
       Read rcdGBUpdat;

      /end-free

     p LoadScreen      e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       Chain 1 pgbi0p;
       If %Found(wagbip);
          scFolder   = GBIfolder;
          scDocument = GBIdocID;
       Else;
          scFolder   = ' ';
          scDocument = ' ';
       EndIf;

       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;
       tabHeight  = 560;

       // Get the client id
       GetClientID(clientID);

      /end-free
     p ProgramStart    e
       //===================================================================
       // UpdateWAGBIP - Update WAGBIP
       //===================================================================
     p UpdateWAGBIP    b
     d UpdateWAGBIP    pi

      /free

       Chain 1 pgbi0p;
       GBIfolder = scFolder;
       GBIdocID  = scDocument;
       If %Found(WAGBIP);
          Update pgbi0p;
       Else;
          Write pgbi0p;
       EndIf;

      /end-free
     p UpdateWAGBIP    e
