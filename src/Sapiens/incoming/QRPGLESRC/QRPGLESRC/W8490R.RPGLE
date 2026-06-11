     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W8490R - Territorial Differential Import
     H Bnddir('WTBNDDIR')
     H*===============================================================
     h*  Created on 8/21/2019 by C:291 for Project: 150WEA
     H*===============================================================
     H* REVISIONS
     H*
     H*===============================================================
     fW8490F    cf   e             workstn extfile('W8490F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fsmazl     if   e           k disk
      //===================================================================
      // Prototypes
      //===================================================================
     *Entry
     d W8490R          pr                  Extpgm('W8490R')
     d  eDebug                        1
     d W8490R          pi
     d  eDebug                        1

     dChkDlo           pr                  extPgm('CHKIFSOBJ')
     d docName                      256    const
     d error                           n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d SubmitDrvCL     pr

     d Submitter       pr                  ExtPgm('W8491C')
     d  groupNumber                   3A   Const
     d  carrier#                      5A   Const
     d  outQueue                     10A   Const
     d  fullPathName                258A   Const
     d  debug                         1A   Const

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
     d processType     c                   const('PEO Payroll Import')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d strOutq         s                   Like(Upoutq)
     d strConfirmMsg   s             80
     d hldGroup#       s                   like(UPFND) inz(0)
     d Group#          s                   like(UPFND) inz(0)
     d moreRecords     s              7p 0 inz
     d strCarrier      s              5a
     d strGroup        s              5a
     d flgUpld         s              1
      //===================================================================
      // Data Structures
      //===================================================================
     d pathNameDta     ds                  dtaara('TDFIMPPATH')
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
       erPathNam = *Off;
       msPathNam = *Blanks;
       erCarrier = *Off;
       msCarrier = *Blanks;
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
          Write RcdTabTDFI;
          If scPathName = *blanks;
             In(e) pathNameDta;
             If %error;
               // Do nothing
             Endif;
             scPathName = lastPathName;
          Endif;
          Exfmt RcdTDFImp;
          Read rcdComPnl;
          Read RcdTabTDFI;
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
            // Validate and Submit Report Program
             If Validate();
                If scCarrier = 0;
                   strCarrier = '*All';
                Else;
                   strCarrier = %char(scCarrier);
                Endif;
                If scGroup = 0;
                   strGroup = '*All';
                Else;
                   strGroup = %char(scGroup);
                Endif;
                strConFirmMsg = 'You are uploading Territorial Differential ' +
                 'to Carrier ' + %trim(strCarrier) + ' and Group ' +
                 %trim(StrGroup) + '.';
                If PopTwoBtn(strConfirmMsg:'Confirmation':
                 'Cancel':'Confirm') = *Off;
                   SubmitDrvCl();
                   Select;
                   When strOutq = 'FILE LOCK';
                      PopOneBtn('Import file could not be allocated':'Error');
                      Leave;
                   When strOutq = 'BAD DATA';
                      PopOneBtn('Data upload unsuccessful. Check error -
                       report.':'Error');
                      Leave;
                   Other;
                      If flgUpld = 'Y';      //Upload successful, No Errors
                         PopOneBtn('Data upload successful.':'Info');
                         Leave;
                      Endif;
                      If flgUpld = 'W';      //Upload successful, with some Errors
                         PopOneBtn('Data uploaded. Some errors Found. Check -
                          error report.':'Info');
                         Leave;
                      Endif;
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
      /End-Free

     p ProgramStart    e
      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free
       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       //Select Statement
       carrSelect = 'DIGITS(F2CAR#)';

       If grpFrom = 'WMFNP';
          carrFrom='WMfnp join WDf2p on FNco#=F2co# and FNfnd=F2fnd';
          carrWhere = grpWhere;
       Else;
          carrFrom='WDF2P join SMAZP A on F2co#=AZco# and F2fnd=AZfnd '  +
                    %trim(%subst(grpFrom:50)) +
                    ' join WMFNP on F2co#=FNco# and F2fnd=FNfnd';
          carrWhere = %trim(grpWhere) + ' and F2car# <> 0';
       Endif;

      /End-Free

     p SetCriteria     e
      //====================================================================
      // SubmitDrvCL - Submit W8491C Territorial Differential Submit Program
      //====================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi

      /Free
       Submitter(%Editc(scGroup:'X'):%editc(scCarrier:'X'):
        strOutq:fullPathName:flgUpld);
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
      // Validate if any Group is selected

       // Must start with a '/'
       If %subst(scPathName:1:1) <> '/';
          erPathNam = *On;
          msPathNam = 'The import file path name must start with a ''/''.';
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
          erPathNam = *On;
          mSPathNam = 'The file extension must be .TXT.';
          Return *Off;
       Endif;

       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathName:docExists);
       If Not(docExists);
          erPathNam = *On;
          msPathNam = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       // Load the starting position of the directory path after
       //  the root part and send onto W8396C in case it is needed.
       sFldPos = %Len(%TrimR(rootPlus)) + 1;
       %Subst(fullPathName:257:2) = %EditC(sFldPos:'X');
       Return *On;
      /End-Free

     P Validate        e
