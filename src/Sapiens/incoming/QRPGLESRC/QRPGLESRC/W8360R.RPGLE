     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W8395R - PEO Payroll Import
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* REVISIONS
     H*
     H* 04/25/23 C:344 Support: PHW109020
     H*                Default the last pathname to keep only the directory
     H*                path and force the user to always enter the filename.
     H*
     H* 02/15/23 C:344 Support: PHW107030
     H*                Allow user to enter a partial vendor# or
     H*                description in the vendor field. Revised W8360F
     H*                Display File by changing the Vendor field so
     H*                that the 'contains match' property is set to
     H*                true in order to include records that contain the
     H*                vendor search text.
     H*
     H* 07/17/17 C:136 Project: 150WAN
     H*                Rewrite S/R Payroll Import
     H*===============================================================
     fW8360F    cf   e             workstn extfile('W8360F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
      //===================================================================
      // Prototypes
      //===================================================================
     *Entry
     d W8360R          pr                  Extpgm('W8360R')
     d W8360R          pi

     d ChkDlo          pr                  extPgm('CHKIFSOBJ')
     d  docName                     256    const
     d  error                          n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d SubmitDrvCL     pr

     d Submitter       pr                  ExtPgm('W8361C')
     d  groupNumber                   3A   Const
     d  vendorNumber                  5A   Const
     d  outQueue                     10A   Const
     d  fullPathName                258A   Const

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
     d fullPathName    s            258
     d isUseScript     s              1n   inz
     d processType     c                   const('PEO Payroll Import')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d strOutq         s                   Like(Upoutq)
     d strConfirmMsg   s             80
      //===================================================================
      // Data Structures
      //===================================================================
     d pathNameDta     ds                  dtaara('SRPAYIMPTH')
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
       erGrpNbr   = *Off;
       msGrpNbr   = *Blanks;
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
          Write RcdTabPayI;
          If scPathName = *blanks;
             In(e) pathNameDta;
             If %error;
               // Do nothing
             Endif;
             //*** Beg Chg *** 04/25/23 ***********************************
             //*** scPathName = lastPathName;
             // Modify Last Import File Pathname by removing filename.
             scPathName = ModPathName(lastPathName);
             //*** End Chg *** 04/25/23 ***********************************
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
       SetCriteria();
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
                strConFirmMsg = 'You are uploading payrolls to group ' +
                 %Editc(scGrpDsr:'X') + '.';
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
                   When strOutq = 'PREV FILE';
                      PopOneBtn('Data previously loaded for report date. ' +
                       'Import cancelled.':'Error');
                      Leave;
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
      /End-Free

     p ProgramStart    e
      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free
       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
       vdrWhere = 'VPCO# = ' + %EditC(UPCo#:'X');
      /End-Free

     p SetCriteria     e
      //===================================================================
      // SubmitDrvCL - Calls W8361C
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi

      /Free
       Submitter(%Editc(scGrpDsr:'X'):%Editc(scVendor:'X'):
        strOutq:fullPathName);
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
      // Validate that Group was selected
       If scGrpDsr  = *zeros;
          erGrpNbr =  *On;
          msGrpNbr = 'WCR0503';
          Return *Off;
       Endif;

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
       //**** Beg Add *** 04/25/23 ***********************************
       //*************************************************************
       // Procedure:
       // Modify Last Import File Pathname by removing filename.
       //*************************************************************
       Dcl-proc ModPathName;
       Dcl-Pi *N char(256);
          inPath char(256);
       End-Pi;

       Dcl-S i int(5) inz;
       Dcl-S outPath like(lastPathName) inz;

         // Remove the filename portion of the pathname so that only
         // the directory remains.
         For i  = 256 downto 1;
            If %subst(lastPathName:i :1) = '/';
               outPath = %subst(inPath:1:i );
               leave;
            Endif;
         Endfor;
         Return outPath;
       End-proc ModPathName;
       //**** End Add *** 04/25/23 ***********************************

