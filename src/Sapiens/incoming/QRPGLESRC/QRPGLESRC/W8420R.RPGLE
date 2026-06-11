     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W8420R - Audit Payroll Upload
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* Revisions
     H*
     H* 02/17/25 C:340 Project: WC3001
     H*                Remove Group Filed and Its related all Validations.
     H*
     H* 02/08/17 C:136 Project: WC2479
     H*                Convert to CompSuite
     H*                Add directory path field
     H*===============================================================
     fW8420F    cf   e             workstn extfile('W8420F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmupl     if   e           k disk    rename(pup01:pup00)

     *Entry
     d W8420R          pr                  Extpgm('W8420R')
     d W8420R          pi

     dChkDlo           pr                  extPgm('CHKIFSOBJ')
     d docName                      256    const
     d error                           n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d fullPathName    s            258
     d isUseScript     s              1n   inz
     d processType     c                   const('Audit Payroll Import')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d strOutq         s                   Like(Upoutq)
     d strConfirmMsg   s             80
     d scGrpDsr        s              3  0

     d pathNameDta     ds                  dtaara('AUDPAYPATH')
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

     d/copy sCopyPr01d
     d/copy sPrCompany
     d/copy wPrUsrprms
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrLetter
     d/copy sPrXML

     d SubmitDrvCl     pr

     d Driver          pr                  ExtPgm('W8420C')
     d  outFnd                        3a   Const
     d  outQueue                     10a   Const
     d  fullPathName                258a   Const
     d  outAllowNewCl                 1a   Const
     d  outZeroMisPay                 1a   Const

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

     * Set Criteria
     d SetCriteria     pr

     * Validations
     d Validate        pr              n

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
      *** Beg Del *** 02/17/25 *** WC3001 *************************************
       // erGrpNbr   = *Off;
       // msGrpNbr   = *Blanks;
      *** End Del *** 02/17/25 *** WC3001 *************************************
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
          Write RcdComPnl;
          Write AudPayPnl;
          If scPathName = *blanks;
             In(e) pathNameDta;
             If %error;
               // Do nothing
             Endif;
             scPathName = lastPathName;
          Endif;
          Exfmt AudPayRcd;
          Read RcdComPnl;
          Read AudPayPnl;
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
       SetCriteria();
      // Process until exit button is pressed.
       Dou ExitNow = *On ;
          ExfmtScreens();
          Select;
          When BtnSubmit = *On;
             BtnSubmit = *Off;
            // Clear Errors
             ClearError();
             If Validate();
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
                   PopOneBtn('Data upload successful':'Info');
                   Leave;
                Endsl;
             EndIf;
          When BtnCancel = *On;
             BtnCancel = *Off;
             Leave;
          When BtnRefresh= *On;
             BtnRefresh= *Off;
             Initialize();
             ClearError();
             SetCriteria();
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
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
       scGrpDsr = 0;
      *** End Add *** 02/17/25 *** WC3001 *************************************
      /End-Free

     p ProgramStart    e
      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free
      *** Beg Del *** 02/17/25 *** WC3001 *************************************
       // GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
      *** End Del *** 02/17/25 *** WC3001 *************************************
      /End-Free

     p SetCriteria     e
      //===================================================================
      // SubmitDrvCL - Submit job
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi

      /Free
       Driver(%Editc(scGrpDsr:'X'):strOutq:fullPathName:
        scZeroClas:scAddClass);
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
       // Validate that Group was selected
      *** Beg Del *** 02/17/25 *** WC3001 *************************************
        // If scGrpDsr  = *zeros;
        //    erGrpNbr =  *On;
        //    msGrpNbr = 'WCR0503';
        //    Return *Off;
        // Endif;
      *** End Del *** 02/17/25 *** WC3001 *************************************

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
       //  the root part and send onto W8393C in case it is needed.
       sFldPos = %Len(%TrimR(rootPlus)) + 1;
       %Subst(fullPathName:257:2) = %EditC(sFldPos:'X');

       Return *On;
      /End-Free

     P Validate        e
