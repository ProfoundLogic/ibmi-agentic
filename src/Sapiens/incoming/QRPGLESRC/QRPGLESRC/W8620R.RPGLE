     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE Automate Rate Set Data Upload Process
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W8620R - Payroll Rate Upload
     H Bnddir('WTBNDDIR')
    H*********************************************************************
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:  W8620C                                                *
    H*                                                                   *
    H*  CREATED: 06/30/23      C:291     PROJECT: WC2915                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
     H* 06/27/23 C:291 Project: WC2915                                    *
     H*                Convert to CompSuite and rewrite.                  *
     H*                                                                   *
    H*********************************************************************
     fW8620F    cf   e             workstn extfile('W8620F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      //===================================================================
      // Prototypes
      //===================================================================
     *Entry
     d W8620R          pr                  Extpgm('W8620R')
     d W8620R          pi

     dChkDlo           pr                  extPgm('CHKIFSOBJ')
     d docName                      256    const
     d error                           n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d SubmitDrvCL     pr

     d Submitter       pr                  ExtPgm('W8621C')
     d  fullPathNamP                258A   Const
     d  fullPathNamE                258A   Const
     d  rtnCode                      15A   Const

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
     d fullPathNamP    s            258A
     d fullPathNamE    s            258A
     d rtnCode         s             15A
     d strMessage      s            256a   inz
     d strOutq         s                   Like(Upoutq)
     d strConfirmMsg   s             80
     d root            s             10
     d rootPlus        s             30
     d suffix          s              3
     d Lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     d Up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      //===================================================================
      // Data Structures
      //===================================================================
     d pathNameDta     ds                  dtaara('PAYRIMPATH')
     d  lastPathNameP          1    256
     d  lastPathNameE        257    512

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
       errPathNmP = *Off;
       msgPathNmP = *Blanks;
       errPathNmE = *Off;
       msgPathNmE = *Blanks;
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
          Write RcdTabPyrl;
          If scPathName = *blanks;
          Endif;
          Exfmt RcdPayUpld;
          Read rcdComPnl;
          Read RcdTabPyrl;
       EndSl;
      /End-Free

     p ExfmtScreens    e
      //===================================================================
      // Initialize - Initialization procedure
      //===================================================================
     p Initialize      b
     d Initialize      pi

      /Free
       Clear scPathNamP;
       Clear scPathNamE;
       In(e) pathNameDta;
       If %error;
         // Do nothing
       Endif;
       If lastPathNameP = *Blank and lastPathNameE = *Blank;
          // Build full path name
          RtvDocRoot(root);
          RtvSuffix(suffix);
          If root = *blanks;
             root = 'QDLS';
          Endif;
          rootPlus = '/' + %trim(root) + '/PDS/';
          If suffix <> *blanks;
             rootPlus = %trim(rootPlus) + suffix + '/';
          Endif;
          If lastPathNameP = *Blank;
             lastPathNameP = %trim(rootPlus) + 'Payroll/PCWMPRP.CSV';
          Endif;
          If lastPathNameE = *Blank;
             lastPathNameE = %trim(rootPlus) + 'Payroll/PCWMPXP.CSV';
          Endif;
       Endif;
       If lastPathNameP <> *Blank;
          scPathNamP = lastPathNameP;
       Endif;
       If lastPathNameE <> *Blank;
          scPathNamE = lastPathNameE;
       Endif;
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
            // Clear Errors
             ClearError();
            // Validate and Submit Report Program
             If Validate();
                strConFirmMsg = 'You are uploading payroll Rates.';
                If PopTwoBtn(strConfirmMsg:'Confirmation':
                 'Cancel':'Confirm') = *Off;
                   SubmitDrvCl();
                   Select;
                   When rtnCode = 'WXPRP_LOCK';
                      strMessage = 'Payroll Class Code Rates file could ' +
                      'not be allocated.';
                      PopOneBtn(strMessage:'Error');
                   When rtnCode = 'WXPRP_NOREC';
                      strMessage = 'An error occured while uploading Payroll '+
                       'Class Code Rates file.';
                      PopOneBtn(strMessage:'Error');
                      Leave;
                   When rtnCode = 'WXPXP1_LOCK';
                      strMessage = 'Payroll Class Code Extension Description ' +
                      'file could not be allocated.';
                      PopOneBtn(strMessage:'Error');
                   When rtnCode = 'WXPXP1_NOREC';
                      strMessage = 'An error occured while uploading Payroll ' +
                       'Class Code Extension Description file.';
                      PopOneBtn(strMessage:'Error');
                      Leave;
                   When rtnCode = 'WXPRP_ERR' or rtnCode = 'WXPXP1_ERR';
                      strMessage = 'Data upload unsuccessful. Check error ' +
                       'report PAYRTUPLD.';
                      PopOneBtn(strMessage:'Error');
                      Leave;
                   When rtnCode = 'UPLD_SUCCESS';
                      PopOneBtn('Payroll Rate Upload job completed ' +
                       'Successfully.'
                       :'Information');
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
        scTitle = 'Payroll Rate Upload';
        scCompName = GetCompNam(userCo#(q1user));
        Chain Q1user pup00;
        If %Found(wmupl);
           strOutq = UPoutq;
        EndIf;
      /End-Free

     p ProgramStart    e
      //===================================================================
      // SubmitDrvCL - Submit W7280C Premium Due Submit Program
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi

      /Free
       Submitter(fullPathNamP:fullPathNamE:rtnCode);
      /End-Free

     P SubmitDrvCL     e
      //===================================================================
      // Validate - Validate screen fields
      //===================================================================
     p Validate        b
     d Validate        pi              n
     d docExists       s               n
     d sFldPos         s              2  0

      /Free

       // Must start with a '/'
       If %subst(scPathNamP:1:1) <> '/';
          errPathNmP = *On;
          msgPathNmP = 'The import file path name must start with a ''/''.';
          Return *Off;
       Endif;
       // Must be a .CSV file
       fullPathNamP = %Xlate(lo:up:scPathNamP);
       If %scan('.CSV':fullPathNamP) = 0;
          errPathNmP = *On;
          msgPathNmP = 'The file extension must be .CSV.';
          Return *Off;
       Endif;
       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathNamP:docExists);
       If Not(docExists);
          errPathNmP = *On;
          msgPathNmP = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       If scPathNamE <> *Blank;
          // Must start with a '/'
          If %subst(scPathNamE:1:1) <> '/';
             errPathNmE = *On;
             msgPathNmE = 'The import file path name must start with a ''/''.';
             Return *Off;
          Endif;
          // Must be a .CSV file
          fullPathNamE = %Xlate(lo:up:scPathNamE);
          If %scan('.CSV':fullPathNamE) = 0;
             errPathNmE = *On;
             msgPathNmE = 'The file extension must be .CSV.';
             Return *Off;
          Endif;
          // Make sure doc exists (also checks authority);
          ChkDlo(fullPathNamE:docExists);
          If Not(docExists);
             errPathNmE = *On;
             msgPathNmE = 'Document does not exist or authority is restricted.';
             Return *Off;
          Endif;
          %Subst(fullPathNamE:258:1) = '*';
       Else;
          fullPathNamE = *Blank;
          %Subst(fullPathNamE:258:1) = '*';
       Endif;

       // Load special character in last position of the pathname to avoid invalid char error
       %Subst(fullPathNamP:258:1) = '*';
       Return *On;
      /End-Free

     P Validate        e
