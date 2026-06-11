     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W8385R - FROI import prompt
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* REVISIONS
     H*
     H* 02/22/21 Support: 91730   Add screen to show FROI errors.
     H* 03/30/20 Project: 129WVX  FROI import

     H*===============================================================
     fW8385F    cf   e             workstn extfile('W8385F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflErrors:SflRrnErr)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fWXERP3    if   e             disk    usropn
      //===================================================================
      // Prototypes
      //===================================================================
     *Entry
     d W8385R          pr                  Extpgm('W8385R')
     d  eDebug                        1
     d W8385R          pi
     d  eDebug                        1

     dChkDlo           pr                  extPgm('CHKIFSOBJ')
     d docName                      256    const
     d error                           n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d SubmitDrvCL     pr

     d Submitter       pr                  ExtPgm('W8385C')
     d  outQueue                     10A   Const
     d  fullPathName                258A   Const

     d ClearError      pr
     d ExfmtScreens    pr
     d Initialize      pr
     d MainProcess     pr
     d ProgramStart    pr
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
     d fullPathName    s            258
     d isUseScript     s              1n   inz
     d processType     c                   const('FROI Import')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d strOutq         s                   Like(Upoutq)
     d strConfirmMsg   s             80
      //===================================================================
      // Data Structures
      //===================================================================
     d pathNameDta     ds                  dtaara('FROIIMPPTH')
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
       errPathNam = *Off;
       msgPathNam = *Blanks;
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
          Write RcdTabFroi;
          If scPathName = *blanks;
             In(e) pathNameDta;
             If %error;
               // Do nothing
             Endif;
             scPathName = lastPathName;
          Endif;
          Exfmt RcdFroiImp;
          Read rcdComPnl;
          Read RcdTabFroi;
       EndSl;
      /End-Free

     p ExfmtScreens    e
      //===================================================================
      // Initialize - Initialization procedure
      //===================================================================
     p Initialize      b
     d Initialize      pi

      /Free
       visErr = *Off;
       //Clear subfile
       sflRrnErr = 0;
       clrSflErr = *On;
       dspSflErr = *Off;
       dspCtlErr = *Off;
       write RcdFroiImp;
       clrSflErr = *Off;
       dspCtlErr = *On;
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
             If isUseScript;
                xxSubmit = *on;
                xxProcType = processType;
                In *Lock *Dtaara;
                Clear svLDA;
                xxDart = 'N';
                xxRMCD = ' ';
                Out *Dtaara;
                xxSubmit = OutputWindow(xxProcType:xxMass);
                If xxSubmit = *off;
                   Iter;
                Endif;
             Endif;
            // Clear Errors
             ClearError();
            // Validate and Submit Report Program
             If Validate();
                strConFirmMsg = 'You are about to run the FROI import';
                If PopTwoBtn(strConfirmMsg:'Confirmation':
                 'Cancel':'Confirm') = *Off;
                   SubmitDrvCl();
                   Select;
                   When strOutq = 'FILE LOCK';
                      PopOneBtn('Import file could not be allocated':'Error');
                      Leave;
                   When strOutq = 'BAD DATA';
                      LoadFroiErrors();
                      If visErr = *Off;
                         PopOneBtn('Data upload unsuccessful.':'Error');
                         Leave;
                      Endif;
                   Other;
                      PopOneBtn('Data upload successful':'Info');
                      Leave;
                   Endsl;
                EndIf;
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
        If %Found(wmupl);
           strOutq = UPoutq;
        EndIf;
       isUseScript = UseScriptura(processType);
      /End-Free

     p ProgramStart    e
      //===================================================================
      // SubmitDrvCL - Submit W7280C Premium Due Submit Program
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi

      /Free
       Submitter(strOutq:fullPathName);
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

      /Free
       // Must start with a '/'
       If %subst(scPathName:1:1) <> '/';
          errPathNam = *On;
          msgPathNam = 'The import file path name must start with a ''/''.';
          Return *Off;
       Endif;

       // Build full path name
       RtvDocRoot(root);
       If root = *blanks;
          root = 'QDLS';
       Endif;
       RtvSuffix(suffix);
       If suffix <> *blanks;
          rootPlus = '/' + root + '/PDS/' + suffix;
       Else;
          rootPlus = '/' + root;
       Endif;
       // fullPathName = %Trim(rootPlus) + %Trim(scPathName);
       fullPathName = %Xlate(lo:up:scPathName);

       // Must be a .TXT file
       If %scan('.TXT':fullPathName) = 0;
          errPathNam = *On;
          msgPathNam = 'The file extension must be .TXT.';
          Return *Off;
       Endif;

       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathName:docExists);
       If Not(docExists);
          errPathNam = *On;
          msgPathNam = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       // Load the starting position of the directory path after
       //  the root part and send onto W8396C in case it is needed.
       sFldPos = %Len(%TrimR(rootPlus)) + 1;
       %Subst(fullPathName:257:2) = %EditC(sFldPos:'X');
       Return *On;
      /End-Free

     P Validate        e
      //===================================================================
      // Load Froi errors
      //===================================================================
     p LoadFroierrors  b
     d LoadFroierrors  pi
     d intRrn          s              9  0
       If not %open(WXERP3);
          open WXERP3;
       Endif;
       intRrn = *Zeros;
       sflRrnErr = *zero;
       visErr = *Off;
       Read WXERP3;
       Dow not %Eof(WXERP3);
          scRrn = ERrrn;
          scClaimant = %trim(ERlsn) + ', ' + %trim(ERfsn);
          If ERmid <> *blank;
             scClaimant = %trim(scClaimant) + ' ' + ERmid + '.';
          Endif;
          scMsg = ERerr;
          scAcdt = %subst(ERacdt:1:2) + '/' + %subst(ERacdt:3:2) + '/' +
                   %subst(ERacdt:5:4);
          sflRrnErr =sflRrnErr + 1;
          Write sflErrors;
          Read WXERP3;
       Enddo;

       If SflRrnErr > *Zero;
          visErr = *On;
          dspSflErr = *On;
       Endif;

       If %open(WXERP3);
          close WXERP3;
       Endif;

     p LoadFroierrors  e
