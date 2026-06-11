     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title WTVPAYIMP - VPay 1099 Payment Import Prompt
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* REVISIONS
     H*
     H*===============================================================
     fWTVPAYIMP cf   e             workstn extfile('WTVPAYIMP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmupl     if   e           k disk    rename(pup01:pup00)

     *Entry
     d WTVPAYImport    pr                  Extpgm('WTVPAYIMP')
     d WTVPAYImport    pi

     dChkDlo           pr                  extPgm('CHKIFSOBJ')
     d docName                      256    const
     d error                           n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d/copy sPrGenPop

     d fullPathName    s            258
     d rtnCode         s             10
     d strOutq         s                   Like(Upoutq)
     d strConfirmMsg   s             80

     d pathNameDta     ds                  dtaara('WTVPAYIMP')
     d  lastPathName           1    256

     d/copy scopypr01d
     d/copy sPrCompany
     d/copy wPrUsrprms

     d SubmitDrvCl     pr

     d Submitter       pr                  ExtPgm('WTVPAYIMC1')
     d  fullPathName                258A   Const
     d  rtnCode                      10A   Const

     * Clear Errors
     d ClearError      pr

     * Display screen
     d ExfmtScreens    pr

     * Initialize
     d Initialize      pr

     * Main Process
     d MainProcess     pr

     * Program Start
     d ProgramStart    pr

     * Validations
     d Validate        pr              n

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
          Write RcdTabVPIm;
          If scPathName = *blanks;
             In(e) pathNameDta;
             If %error;
               // Do nothing
             Endif;
             scPathName = lastPathName;
          Endif;
          Exfmt RcdVPayImp;
          Read rcdComPnl;
          Read RcdTabVPIm;
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

       ClearError();

      // Process until exit button is pressed.
       Dou ExitNow = *On ;
          ExfmtScreens();
          Select;
          When BtnSubmit = *On;
             BtnSubmit = *Off;
             ClearError();
             If Validate();
                strConFirmMsg = 'You are uploading VPay Recon File.';
                If PopTwoBtn(strConfirmMsg:'Confirmation':
                 'Cancel':'Confirm') = *Off;
                   SubmitDrvCl();
                   Select;
                   When strOutq = 'FILE LOCK';
                      PopOneBtn('Import file could not be allocated':'Error');
                      Leave;
                   When strOutq = 'BAD DATA';
                      PopOneBtn('Data upload unsuccessful. Check error -
                       report':'Error');
                      Leave;
                   Other;
                      PopOneBtn('Job VPayImport Submitted successfully':'Info');
                      Leave;
                   Endsl;
                Endif;
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
        scTitle = 'VPay Recon File Upload';
        scCompName = GetCompNam(userCo#(q1user));
        Chain Q1user pup00;
        If %Found(wmupl);
           strOutq = UPoutq;
        EndIf;
      /End-Free

     p ProgramStart    e
      //===================================================================
      // SubmitDrvCL - Submit job
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi

      /Free
       Submitter(fullPathName:rtnCode);

      /End-Free

     P SubmitDrvCL     e
      //===================================================================
      // Validate - Validate screen fields
      //===================================================================
     p Validate        b
     d Validate        pi              n
     d docExists       s               n
     d lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     d root            s             10
     d rootPlus        s             20
     d sFldPos         s              2  0
     d suffix          s              3
     d up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

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

       // Must be a .csv file
       If %scan('.CSV':fullPathName) = 0;
          errPathNam = *On;
          msgPathNam = 'The file extension must be .csv.';
          Return *Off;
       Endif;

       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathName:docExists);
       If Not(docExists);
          errPathNam = *On;
          msgPathNam = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       %Subst(fullPathName:257:2) = '**';

       Return *On;
      /End-Free

     P Validate        e
