     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W8392R - PEO Policy Import
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* REVISIONS
     H*
     H* 04/29/24 C:340 Project: WC2965
     H*                Recompile the program for the file change.
     H*
     H* 10/05/23 C:333 Project: 150WFZ  (re-compile)
     H*                PEO Import Enhancement
     H*
     H* 11/15/21 C:136 Project: 150WFM
     H*                Add quote processing
     H*
     H* 07/12/21 C:180 Project: 150WFJ
     H*                Allow for employer link code update.
     H*
     H* 02/05/20 C:318 Support: 86313
     H*                Added return statement for the validation
     H*                to trigger error
     H*
     H* 02/08/17 C:136 Project: WC2479
     H*                Convert to CompSuite
     H*                Add directory path field
     H*===============================================================
     fW8392F    cf   e             workstn extfile('W8392F')
     f                                     handler('PROFOUNDUI(HANDLER)')
      *** Begin Add *** 07/12/21 *****************************************
     f                                     sfile(SflPPIerr:rrnPPIerr)
      *** End   Add *** 07/12/21 *****************************************
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmupl     if   e           k disk    rename(pup01:pup00)

     *Entry
     d W8392R          pr                  Extpgm('W8392R')
     d  eRptTyp                       1                                         'A' for Add Member
     d W8392R          pi
     d  eRptTyp                       1

     dChkDlo           pr                  extPgm('CHKIFSOBJ')
     d docName                      256    const
     d error                           n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d/copy sPrGenPop
     d/copy sPrLetter
     d/copy sPrXML
       //*** Begin Add *** 07/12/21 ***********************************
     d/copy sPrRunCmd
     d/copy sPrMessage
     d/copy sPrSQL
       //*** End   Add *** 07/12/21 ***********************************
     d fullPathName    s            258
     d isUseScript     s              1n   inz
     d processType     c                   const('PEO Policy Import')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d strOutq         s                   Like(Upoutq)
     d strConfirmMsg   s             80
       //*** Begin Add *** 07/12/21 ********************************************
       Dcl-S strRunCmd char(500) inz;
       Dcl-S PopMsg    char(256) inz;
       Dcl-S PopTitle  char(50)  inz;
       Dcl-S intResult zoned(3:0);
       Dcl-S msg2ndLevel char(4096) inz;
       Dcl-S pattern char(25) inz;
       Dcl-S strRecNotCopy char(7) inz;
       Dcl-S strDigit char(1) inz;
       Dcl-S isValidFormat ind;
       Dcl-S isValidLink ind;
       Dcl-Ds dsMsgInfo likeds(dsRtnMsgInfo);
       Dcl-Ds dsInMsgInfo likeds(dsRtvMsgInfo);
       //*** End   Add *** 07/12/21 ********************************************
       //*** Beg Add ***** 11/15/21 **************
       Dcl-S strEnv Char(4) Inz('*LIV');
       //*** End Add ***** 11/15/21 **************

     d pathNameDta     ds                  dtaara('POLIMPPATH')
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

     d/copy scopypr01d
     d/copy sPrCompany
     d/copy wPrUsrprms

     d SubmitDrvCl     pr

     d Submitter       pr                  ExtPgm('W8392C')
     d  groupNumber                   3A   Const
     d  outQueue                     10A   Const
     d  fullPathName                258A   Const
     d  rptType                       1A   Const

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

       //*** beg add ***** 11/15/21 **************
       Dcl-Pr RtvEnv ExtPgm('RTVENV');
        rtnEnv Char(4);
       End-Pr;
       //*** end add ***** 11/15/21 **************

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
        erGrpNbr   = *Off;
        msGrpNbr   = *Blanks;
        errPathNam = *Off;
        msgPathNam = *Blanks;

        //*** Begin Add *** 07/12/21 ****************************************
         // Clear PEO Policy Import Error Subfile
        rrnPPIerr  = 0;
        clrPPIerr  = *On;
        dspPPIeSfl = *Off;
        dspPPIeCtl = *Off;
        Write RcdPolImp;
        clrPPIerr = *Off;
        isValidLink = *On;
        isValidFormat = *On;
        dspPPIeCtl = *On;

        //Deallocate WXPMP
        Monitor;
           strRunCmd = 'DLCOBJ OBJ((WXPMP *FILE *EXCL))';
           RunCmd(strRunCmd:500);
        On-Error;
        EndMon;
        //*** End   Add *** 07/12/21 ****************************************
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
          Write RcdTabPolI;
          If scPathName = *blanks;
             In(e) pathNameDta;
             If %error;
               // Do nothing
             Endif;
             scPathName = lastPathName;
          Endif;
          Exfmt RcdPolImp;
          Read rcdComPnl;
          Read RcdTabPolI;
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
       //*** Begin add *** 07/15/21 *******************************************************
       ClearError();
       //*** End   add *** 07/15/21 *******************************************************

      // Process until exit button is pressed.
       Dou ExitNow = *On ;
          ExfmtScreens();
          Select;
          When BtnSubmit = *On;
             BtnSubmit = *Off;
             ClearError();
             If Validate();
                strConFirmMsg = 'You are uploading policies to group ' +
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
                   Other;
                      PopOneBtn('Data upload successful':'Info');
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
        //*** beg add ***** 11/15/21 ************
        RtvEnv(strEnv);
        If strEnv = '*QTE';
           scTitle = 'Quote PEO Policy Import';
        Else;
           scTitle = 'Live PEO Policy Import';
        Endif;
        //*** end add ***** 11/15/21 ************
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
      /End-Free

     p SetCriteria     e
      //===================================================================
      // SubmitDrvCL - Submit job
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi

      /Free
       Submitter(%Editc(scGrpDsr:'X'):
        strOutq:fullPathName:eRptTyp);
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
      // Validate if any Group is selected
       If scGrpDsr = *zeros;
          erGrpNbr =  *On;
          msGrpNbr = 'WCR0503';
          Return *Off;
       Endif;

      // Selected Group must be flagged as a PEO
       If scGrpDsr <> *zeros;
          Chain (Upco#:scGrpDsr) pfn00;
          If FNpeof = *blank;
             erGrpNbr = *On;
             msGrpNbr = 'WC31538';
      //***** Beg Add *** 02/05/20 ************************
             Return *Off;
      //***** End Add *** 02/05/20 ************************
          Endif;
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

       //*** Begin Add *** 07/12/21 ***************************************
       //Allocate file
       Monitor;
          strRunCmd = 'ALCOBJ OBJ((WXPMP *FILE *EXCL *FIRST))';
          //strRunCmd = 'ALCOBJ OBJ((WXPMP *FILE *EXCL *FIRST)) SCOPE(*LCKSPC)';
          RunCmd(strRunCmd:500);
       On-Error;
          PopOneBtn('Another import is in process.':'Error');
          return *Off;
       EndMon;

        ValEmpLinkcode();
        If isValidFormat = *Off or isValidLink = *Off;
           return *Off;
        Endif;
       //*** End   Add *** 07/12/21 ***************************************
       // Load the starting position of the directory path after
       //  the root part and send onto W8393C in case it is needed.
       sFldPos = %Len(%TrimR(rootPlus)) + 1;
       %Subst(fullPathName:257:2) = %EditC(sFldPos:'X');

       Return *On;
      /End-Free

     P Validate        e
       //*** Begin Add *** 07/12/21 **************************************
       //*----------------------------------------------------------------
       //* Validate Employer Link Code                                   *
       //*----------------------------------------------------------------
       Dcl-Proc ValEmpLinkCode;
        Dcl-Pi *N;
        End-Pi;
        Dcl-S intRow packed(9:0) inz;
        Dcl-S isValidEmp ind inz;
        Dcl-Ds WXPMPds extname('WXPMP')  Qualified Inz End-Ds;

        strRunCmd = 'CPYFRMIMPF FROMSTMF(';
        strRunCmd = %Trim(strRunCmd) + '''' + %Trim(scPathName) + '''' + ')';
        strRunCmd = %Trim(strRunCmd) + ' TOFILE(*LIBL/WXPMP)';
        strRunCmd = %Trim(strRunCmd) + ' MBROPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RCDDLM(*CRLF)';
        strRunCmd = %Trim(strRunCmd) + ' STRDLM(*NONE)';
        strRunCmd = %Trim(strRunCmd) + ' STRESCCHR(*NONE)';
        strRunCmd = %Trim(strRunCmd) + ' RMVBLANK(*NONE)';
        strRunCmd = %Trim(strRunCmd) + ' FLDDLM(*TAB)';
        strRunCmd = %Trim(strRunCmd) + ' FROMRCD(2)';
        strRunCmd = %Trim(strRunCmd) + ' RPLNULLVAL(*FLDDFT)';
        Monitor;
           RunCmd(strRunCmd:500);
           Exsr ChkLnkCode;
        On-Error;
         PopMsg = *blanks;
         isValidFormat = *Off;
           // Get the message indicating how many records were not uploaded
           // If more than 2 inform the user the data was not formatted properly
           dsInMsgInfo = dsRtvMsgInfo;
           dsInMsgInfo.type = '*LAST';
           dsMsgInfo = RtvMsg('*':dsInMsgInfo);
           dsInMsgInfo.type = '*PRV';
           Dow dsMsgInfo.MsgID <> *blanks;
              If dsMsgInfo.MsgID = 'CPC2959';
                 Clear msg2ndLevel;
                 Exec Sql
                 select MESSAGE_SECOND_LEVEL_TEXT into :msg2ndLevel
                     from table(qsys2.joblog_info('*')) A
                     where ORDINAL_POSITION = :dsmsginfo.pos;
                 pattern = 'records were not copied';
                 intResult = %scan(%trim(pattern):%trim(msg2ndLevel));
                 If intResult > 0;
                    intResult = intResult - 2;
                    Clear strRecNotCopy;
                    strDigit = %Subst(msg2ndLevel:intResult:1);
                    Dow strDigit >= '0' and strDigit <= '9';
                       strRecNotCopy = strDigit + %Trim(strRecNotCopy);
                       intResult = intResult - 1;
                       strDigit = %Subst(msg2ndLevel:intResult:1);
                    Enddo;
                    If %Dec(%Trim(strRecNotCopy):7:0) > 2;
                       PopTitle = 'Improper Format Message';
                       PopMsg = 'The data was not formatted properly for the +
                                 Upload';
                       PopOneBtn(PopMsg:PopTitle);
                       isValidformat = *off;
                       Leave;
                    Endif;
                 Endif;
                 Leave;
              Endif;
              dsInMsgInfo.pos  = dsMsgInfo.pos;
              dsMsgInfo = RtvMsg('*':dsInMsgInfo);
           Enddo;
           If PopMsg = *blanks;
              PopOneBtn('Error copying file':'Error');
           Endif;
        Endmon;
       //*----------------------------------------------------------------
       //* Check for Employer/ Link Code                                  *
       //*---------------------------------------------------------------
       Begsr ChkLnkCode;
       //Read through wxpmp and turn off isValidLink if errors

       //Prepare Sql
       Exec Sql
        Declare C1 cursor for
        Select rrn(a), a.*,
         case
          when EMemp# is null then '0'
          else '1'
         end as validEmp
         From Wxpmp a
         left join WMEMP
          on EMfnd=:scGrpDsr and EMemp#=a.PMemp# and EMdiv=a.PMdiv
         Where a.PMemp# <>  ' ' and a.PMrect = 'D';

       Exec Sql Open C1;

       //If SqlCod >= *Zero and SqlCod <> 100; Const review use isValidFetch
       Dou not isValidFetch(SqlStt);
          Exec Sql Fetch Next From C1 into :intRow, :WXPMPds, :isValidEmp;
          Select;
          When not isValidFetch(sqlstt);
             Iter;
          Endsl;
          Exsr ChkLnkCodeErrors;
       Enddo;

       If rrnPPIerr > *Zero;
          visPPIerr = *On;
          dspPPIeSfl = *On;
          dspPPIeCtl = *On;
          isValidLink = *Off;
       Else;
          visPPIerr = *Off;
       Endif;

       Exec Sql Close C1;
       Endsr;
       //*----------------------------------------------------------------
       //* Check PEO Policy Import Errors                           *
       //*----------------------------------------------------------------
        Begsr ChkLnkCodeErrors;
          If not isValidEmp;
             scErrDesc = 'Employer ' + %Trim(WXPMPds.PMemp#) + ' ' +
              'Division ' + %Trim(WXPMPds.PMdiv) + ' not found.';
             Exsr LoadPPIerrGrid;
          Endif;
          If WXPMPds.PMpeol = *blank;
             scErrDesc = 'Link code required for update. ' +
            'Employer ' + %Trim(WXPMPds.PMemp#) + ' ' +
              'Division ' + %Trim(WXPMPds.PMdiv) +
              ' exists in import.';
             Exsr LoadPPIerrGrid;
          Endif;
        Endsr;
       //*----------------------------------------------------------------
       //* Load PPO Policy Import Error grid                             *
       //*----------------------------------------------------------------
        Begsr LoadPPIerrGrid;
        scType = WXPMPds.PMrect;
        scRow = intRow;
        scLnkCode = WXPMPds.PMpeol;
        rrnPPIerr = rrnPPIerr + 1;
        Write SflPPIerr;
        Endsr;
       End-Proc;
       //*** End   Add *** 07/12/21 **************************************
