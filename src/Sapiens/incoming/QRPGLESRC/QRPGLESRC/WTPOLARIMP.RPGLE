     hCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title WTPOLARIMP - A/R Import (Manual Load)
     H Bnddir('WTBNDDIR')
      *===============================================================
      * Menu Call in Profound UI
      * /profoundui/auth/start?pgm=PDSOBJ/STLAUNCH&p1=WTPOLARIMPR
      *  &l1=10&p2=LIV&l2=3&p3=%28%29*End&l3=6
      * Added for 150WEU to test A/R Import manually
     H*===============================================================
     H* REVISIONS
     H*
     H*===============================================================
     fwtpolarimpcf   e             workstn extfile('WTPOLARIMP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fsdacl     if   e           k disk    rename(pac01:pac00)
      //===================================================================
      // Prototypes
      //===================================================================
     *Entry
     d ARIMP           pr                  Extpgm('WTPOLARIMP')
     d ARIMP           pi

     d ChkDlo          pr                  extPgm('CHKIFSOBJ')
     d  docName                     256    const
     d  error                          n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d SubmitDrvCL     pr

     d Submitter       pr                  ExtPgm('WXARIMPC2')
     d  folder                      256A
     d  fileName                    256A
     d  return                       10A

     d ClearError      pr
     d ExfmtScreens    pr
     d Initialize      pr
     d MainProcess     pr
     d ProgramStart    pr
     d SetCriteria     pr
     d Validate        pr              n
     d/copy sCopyPr01d
     d/copy sPrCompany
     d/copy wPrUsrprms
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrLetter
     d/copy sPrXML
      //===================================================================
      // Fields
      //===================================================================
     d fullPathName    s            256
     d isUseScript     s              1n   inz
     d processType     c                   const('PEO Payroll Import')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d strReturn       s                   Like(Upoutq)
     d strConfirmMsg   s             80
      //===================================================================
      // Data Structures
      //===================================================================
     d pathNameDta     ds                  dtaara('ARIMPORT')
     d  lastPathName           1    256

     d svLDA           ds          1024    dtaara(*lda)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      //===================================================================
      // Main
      //===================================================================

      /Free
       ProgramStart();
       MainProcess();
       *InLr = *on;
      /End-Free

      //===================================================================
      // ClearError - Clear Error/Message fields
      //===================================================================
     p ClearError      b
     d ClearError      pi

      /Free
      /End-Free

     p ClearError      e
      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     p ExfmtScreens    b
     d ExfmtScreens    pi

      /Free
       Select;
       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = TabHeight + 60;
          Write rcdComPnl;
          Write RcdTabPayI;
          If scPathName = *blanks;
             In pathNameDta;
             scPathName = lastPathName;
          Endif;
          Exfmt rcdPayImp;
          Read rcdComPnl;
          Read rcdTabPayI;
       EndSl;
      /End-Free

     p ExfmtScreens    e
      //===================================================================
      // Initialize - Initialization procedure
      //===================================================================
     p Initialize      b
     d Initialize      pi

      /Free
      /End-Free

     p Initialize      e
      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi

      /Free
      // Process until exit button is pressed.
       Dou ExitNow = *On ;
          ExfmtScreens();
          Select;
          When BtnSubmit = *On;
             BtnSubmit = *Off;
             // If isUseScript;
             //    xxSubmit = *on;
             //    xxProcType = processType;
             //    In *Lock *Dtaara;
             //    Clear svLDA;
             //    xxDart = 'N';
             //    xxRMCD = ' ';
             //    Out *Dtaara;
             //    xxSubmit = OutputWindow(xxProcType:xxMass);
             //    If xxSubmit = *off;
             //       Iter;
             //    Endif;
             // Endif;
           // Clear Errors
             ClearError();
            // Validate and Submit Report Program
             If Validate();
                SubmitDrvCl();
                PopOneBtn('Job WXARIMPC submitted. Look for ' +
                'the upload report named WXARIMPO in your ' +
                'printer output.':'Info');
                Leave;
             EndIf;
          When BtnCancel = *On;
             BtnCancel = *Off;
             Leave;
          When BtnRefresh= *On;
             BtnRefresh= *Off;
             Initialize();
             ClearError();
          EndSl;
       EndDo;
      /End-Free

     p MainProcess     e
      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
        Initialize();
        scCompName = GetCompNam(userCo#(q1user));
        Chain Q1user pup00;
        strReturn = 'ONDEMAND  ';
       // isUseScript = UseScriptura(processType);
      /End-Free

     p ProgramStart    e
      //===================================================================
      // SubmitDrvCL - Calls W8361C
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi
     d intCharPos      s              3  0
     d outFolder       s            256
     d outFileName     s            256
     d x               s              3  0 inz(1)
     d y               s              3  0

      /Free
       Dou y = 0;
          y = %Scan('/':fullPathName:x);
          If y <> 0;
             intCharPos = y;
          Endif;
          x = y + 1;
       Enddo;
       outFolder = %Subst(fullPathName:1:intCharPos);
       outFileName = %Subst(fullPathName:intCharPos + 1);
       %Subst(outFolder:256:1) = '$';
       %Subst(outFileName:256:1) = '$';
       Submitter(outFolder:outFileName:strReturn);
      /End-Free

     P SubmitDrvCL     e
      //===================================================================
      // Validate - Validate screen fields
      //===================================================================
     p Validate        b
     d Validate        pi              n
     d docExists       s               n
     d Lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     d root            s             10
     d rootPlus        s             20
     d sFldPos         s              2  0
     d suffix          s              3
     d Up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     d strHotFolder    s            256
     d x               s              3  0 inz(1)
     d y               s              3  0
     d intCharPos      s              3  0

      /Free
       // Must start with a '/'
       If %subst(scPathName:1:1) <> '/';
          errPathNam = *On;
          msgPathNam = 'The import file path name must start with a ''/''.';
          Return *Off;
       Endif;

       fullPathName = %Xlate(lo:up:scPathName);
       // Must be a .TXT file
       If %scan('.TXT':fullPathName) = 0;
          errPathNam = *On;
          msgPathNam = 'The file extension must be .TXT.';
          Return *Off;
       Endif;

       // The folder can't be the same as the 'hot' folder used by AUTOJOB
       Chain ('AR_Import_') pac00;
       Select;
       When Not %Found;
       When ACArImpFld = *Blanks;
       Other;
          Dou y = 0;
             y = %Scan('/':ACArImpFld:x);
             If y <> 0;
                intCharPos = y;
             Endif;
             x = y + 1;
          Enddo;
          strHotFolder = %Subst(fullPathName:1:intCharPos);
       EndSl;

       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathName:docExists);
       If Not(docExists);
          errPathNam = *On;
          msgPathNam = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       Return *On;
      /End-Free

     P Validate        e
