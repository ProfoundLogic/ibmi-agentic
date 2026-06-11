      * (c) 2021 Sapiens. All Rights Reserved.                             *
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR':'YAJL')
      *==========================================================
     h/title WITMPLT - Template Associations
      *==========================================================
      * Description: Create/maintain template associations
      *==========================================================
      * Revisions:
      //
      // 11/17/23 C:300 Support: 113191 (WC2395)
      //                Change Column heading from 'CoNNect User Type' to
      //                'Connect User Type'
      //                DSPF: Change Export to Excel File Name as follows:
      //                1) Templates
      //                2) Class Code Detail
      //                3) Template Questions
      //
      // 07/28/23 C:300 Support: 113102 (WC2395)
      //                Apply Persist state property in SflTmplate
      //                and SflClCdDtl grids
      //
      // 06/02/23 C:342 Support: 108934
      //                Display file changes only.
      //                1) Added Questions subfile.
      //                1) Load Class code subfile.
      //
      // 01/09/23 C:079 Support: 95874
      //                Require Template Association Type and Name
      //
      // 11/28/22 C:300 Support: 101201 (WC2196)
      //                Right Align Effective and Exprartion dates in Grid
      //
      // 10/26/21 C:300 Project: WC2395
      //                Added Connect Tab in new Agency Maintenance screen
      //
      // 05/17/21 C:300 Project: WC2196
      //                Check If record has been updated by another user
      //                for EDIT Mode
      //
      // 03/08/21  C:298 Project: WC2196
      //                 Add new procedure for Connect Tab in Statistics
      //                 and Rating screen.
      //                 Added a visibility indicator for RcdPanel
      //                 and fields alignment changes in display file.
      *
      *==========================================================
     fwitmplt   cf   e             workstn extfile('WITMPLT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflTmplate:rrnTemplat)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwitap     uf a e             disk    rename(pta01:pta0p)

      ***** Beg Add ***** 05/17/21 ***************************
     d saveDT          s                   like(TAdt)
     d saveTM          s                   like(TAtime)
     d flgRcdUpdated   s              1    inz
      ***** Beg Add ***** 05/17/21 ***************************
     dintCo#           s              3  0
     disEofWITAP       s              1n   inz(*off)
     disUserTypeActive...
     d                 s              1n   inz(*on)
     disStub0          s              1n   inz(*off)
     dmodeDsp          s              1n   inz(*off)
     dsaveTabIndex     s                   like(tabIndex)
     dstrBrand         s             80    inz
     dstrCall          s             25    inz('Stub')
     dstrMode          s              1    inz('D')
     DstrPortalModuleName...
     D                 s            256    inz varying
     dstrSql           s           5000    inz varying
     dstrSqlWhere      s           5000    inz varying

     ddsWITAP        e ds                  extname(WITAP) qualified inz
     ddsWITAP0         ds                                 qualified inz
     d rrn                            9  0
     d co#                            3  0
     d name                          40
     d userType                      25
     d type                          10
     d efDt                           7  0
     d exDt                           7  0
     ddsWITAP1         ds                  likeds(dsWITAP0) inz
     ddsWITMP        e ds                  extname(WITMP) qualified inz

     d psds          esds                  extname(szq1p)

      /copy WITMPLTD
      /copy sPrDate
      /copy sPrCompany
      /copy sPrGenPop
      /copy sPrModule
      /copy sPrRtvBrnd
      /copy sPrRunCmd
      /copy sPrString
      /copy wPrUsrPrms

      ***** Beg Add ***** 03/08/21 ***************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1
     d Callpgm         s              1
     d Callpgm2        s              1
      ***** End Add ***** 03/08/21 ***************************
      ***** Beg Add ***** 10/26/21 ***************************
     d Callpgm3        s              1
     d Callpgm4        s              1
      ***** End Add ***** 10/26/21 ***************************

      *entry
     DTemplateAssociations...
     d                 pi
     D inParms                             like(dsWITMPLTparms)
     d inCall                        25    const options(*nopass)
     d inMode                         1    const options(*nopass)
      /Free
       Exec SQL set option commit = *NONE;

       dsWITMPLTparms = inParms;
       If %parms >= 2;
          strCall = inCall;
       Endif;
       If %parms >= 3;
          strMode = inMode;
       Endif;

       // start program processing
       ProgramStart();

       // main processing
       If Process();
          Main();
       Endif;

       *InLr = *On;
       Return;
      /end-free

      ***** Beg Add ***** 03/08/21 ***************************
     PProc_Tmplt       b                   export
     dProc_Tmplt       pi
     D inParms                             like(dsWITMPLTparms)
     d inCall                        25    const options(*nopass)
     d inMode                         1    const options(*nopass)
      /Free
       Exec SQL set option commit = *NONE;

       dsWITMPLTparms = inParms ;
       If %parms >= 2;
          strCall = inCall ;
       Endif;
       If %parms >= 3;
          strMode = inMode ;
       Endif;

       // start program processing
       ProgramStart();

       // main processing
       If Process();
          Main();
       Endif;

       *InLr = *On;
       Return;
      /End-free
     PProc_Tmplt       e
      ***** End Add ***** 03/08/21 ***************************
      //===================================================================
     PAddEditTemplate  b
     DAddEditTemplate  pi
     D inMode                        10    const
     D inRrn                          9  0 const options(*nopass)

     DdecRrn           s              9  0 inz
     DisValid          s              1n
      /Free
       If %parms >= 2;
          decRrn = inRrn;
       Endif;
       Select;
       When inMode = 'Delete';
          Exsr TemplateDelete;
          Return;
       When inMode = 'Edit';
          isNoAddTmp = *on;
          scLeftCx = 925;
          Exsr TemplateAddEdit;
          Return;
       When inMode = 'Add';
          isNoAddTmp = *off;
          scLeftCx = 805;
          Exsr TemplateAddEdit;
          Return;
       Endsl;
       scStubHdg = %trim(scPnlHdg0) + ' - ' + inMode;
       Exfmt rcdStub;
       Return;
       //===================================================================
       Begsr TemplateAddEdit;
        disUserTyp = isNoAddTmp;
        tipUserTyp = *blanks;
        Select;
        When not isUserTypeActive;
           disUserTyp = *on;
           tipUserTyp = %trim(hgUserType) +
            ' associations not yet available.';
        When strCall = 'Agency'
         or  strCall = 'Agency/Group';
           disUserTyp = *on;
           tipUserTyp = %trim(hgUserType) +
            ' associations not available at the agency level(s).';
        Endsl;
        erEfDtEdt = *off;
        erExDtEdt = *off;
        //*** Beg Add *** 01/09/23 *********************************************
        erType    = *off;
        erTmpName = *off;
        //*** End Add *** 01/09/23 *********************************************
        scTmpCrit = 'TMCO# = ' + %char(dsWITMPLTparms.co#);
        Exsr TemplateLoad;
        Dou btnCancel = *on;
           btnCancel = *off;
           btnSave = *off;
           btnSaveNxt = *off;
           Exfmt rcdEdtTemp;
           Select;
           When btnCancel = *on;
              Leave;
           When btnSave = *on;
              Exsr TemplateSave;
              If isValid;
                 Leave;
              Endif;
           When btnSaveNxt = *on;
              Exsr TemplateSave;
              If isValid;
                 Exsr TemplateLoad;
              Endif;
           Endsl;
        Enddo;
       Endsr;
       //===================================================================
       Begsr TemplateDelete;
        If PopTwoBtn('Please confirm deletion of template. ('+%trim(scTmpName)+
         ')':'Deletion Confirmation':'Delete':'Cancel') = *on;
           Exec SQL
            delete from WITAP
             where rrn(WITAP) = :decRrn;
        Endif;
       Endsr;
       //===================================================================
       Begsr TemplateLoad;
        Reset dsWITAP;
        Exec SQL
         select *
          into :dsWITAP
          from WITAP
           where rrn(WITAP) = :decRrn;
        scStat = 'A';
        If dsWITAP.TAstat <> *blank;
           scStat = dsWITAP.TAstat;
        Endif;
        scUserType = dsWITAP.TAuserType;
        scType    = dsWITAP.TAtype;
        scTmpName = dsWITAP.TAname;
        scEfDtEdt = DtMDYY(dsWITAP.TAefDt);
        scExDtEdt = DtMDYY(dsWITAP.TAexDt);
        //*** Beg Add ***** 05/17/21 ***************************
        saveDT = dsWITAP.TAdt;
        saveTM = dsWITAP.TAtime;
        //*** End Add ***** 05/17/21 ***************************
       Endsr;
       //===================================================================
       Begsr TemplateSave;
        Exsr TemplateValidate;
        If isValid;
           Clear pta0p;
           Chain decRrn pta0p;
           TAco#  = dsWITMPLTparms.co#;
           TAfnd  = dsWITMPLTparms.fnd;
           TAst   = dsWITMPLTparms.state;
           TAfein = dsWITMPLTparms.fein;
           TAmod  = dsWITMPLTparms.mod;
           TAuserType = scUserType;
           TAtype = scType;
           TAname = scTmpName;
           TAefDt = DtCYMD(scEfDtEdt);
           TAexDt = DtCYMD(scExDtEdt);
           TAstat = scStat;
           TAuser = q1User;
           TApgm  = q1Pgm;
           TAdt   = DateTo7(SystemDate());
           TAtime = TimeTo6(SystemTime());
           //*** Beg Add ***** 05/17/21 ***************************
           saveDT = TAdt;
           saveTM = TAtime;
           //*** End Add ***** 05/17/21 ***************************
           If %found;
              Update pta0p;
           Else;
              TAiUsr = TAuser;
              TAiPgm = TApgm;
              TAiDt  = TAdt;
              TAiTim = TAtime;
              Write pta0p;
           Endif;
        Endif;
       Endsr;
       //===================================================================
       Begsr TemplateValidate;
        isValid = *on;
        //*** Beg Add *** 01/09/23 ********************************************
        erType = *off;
        erTmpName = *off;
        // Type cannot be blank
        If scType = *blanks;
           erType = *on;
           isValid = *off;
           mgType = 'This field cannot be blank';
        Endif;

        // Template name cannot be blank';
        If scTmpName = *blanks;
           erTmpName = *on;
           isValid    = *off;
           mgTmpName = 'This field cannot be blank';
        Endif;
        //*** End Add *** 01/09/23 ********************************************
        erEfDtEdt = *off;
        erExDtEdt = *off;
        scEfDtEdt = DtCheck(scEfDtEdt);
        If scEfDtEdt = *zero;
           msEfDtEdt = 'WCR0500';
           erEfDtEdt = *on;
           isValid = *off;
        Endif;
        If scExDtEdt <> *zero;
           scExDtEdt = DtCheck(scExDtEdt);
           Select;
           When scExDtEdt = *zero;
              msExDtEdt = 'WCR0500';
              erExDtEdt = *on;
              isValid = *off;
           When DtCYMD(scExDtEdt) < DtCYMD(scEfDtEdt);
              msEfDtEdt = 'WCR0529';
              erEfDtEdt = *on;
              msExDtEdt = 'WCR0529';
              erExDtEdt = *on;
              isValid = *off;
           Endsl;
        Endif;
        // Check for overlapping dates
        strSql = 'select rrn(WITAP), TAco#, TAname, TAuserType, '           +
         'TAtype, TAefDt, TAexDt'                                           +
         ' from WITAP ' + strSqlWhere +
         '  and TATYPE= ''' + scType + '''' +
         ' order by TACO#, TATYPE, TANAME';
        Exec SQL close cursorAssoc1;
        Exec SQL prepare association1 from :strSql;
        Exec SQL declare cursorAssoc1 cursor for association1;
        Exec SQL open cursorAssoc1;
        Reset isEofWITAP;
        Dow not isEofWITAP and isValid;
           Exec SQL fetch next from cursorAssoc1 into :dsWITAP1;
           Select;
           When sqlStt <> '00000';
              isEofWITAP = *on;
              Iter;
           When dsWITAP1.rrn = decRrn;
              Iter;
           When RangeOverlap(dsWITAP1.efDt:dsWITAP1.exDt:scEfDtEdt:scExDtEdt);
              msEfDtEdt = 'WCR0907';
              erEfDtEdt = *on;
              msExDtEdt = 'WCR0907';
              erExDtEdt = *on;
              isValid = *off;
              Iter;
           Endsl;
        Enddo;
        Exec SQL close cursorAssoc1;

        //*** Beg Add ***** 05/17/21 ***************************
        If inMode = 'Edit' and isValid = *on;
           // Check if WITAP updated since screen displayed (Templates)
           flgRcdUpdated = RcdUpdatedErr('WITAP':decRrn:'TAdt':'TAtime':
            'TAuser':saveDT:saveTM:'1');
           if flgRcdUpdated = '2'; // Error with Refresh Button
              isValid = *off;
              Exsr TemplateLoad;
           EndIf;
           if flgRcdUpdated = '1'; // Error
              isValid = *off;
           EndIf;
        EndIf;
        //*** End Add ***** 05/17/21 ***************************
       Endsr;
      /End-free
     PAddEditTemplate  e

      //===================================================================
      // Main - Main processing
      //===================================================================
     PMain             b
     DMain             pi

     DisError          s              1n   inz
     DisExit           s              1n   inz

      /free
       Reset isExit;
       Reset isError;
       ExSr ClrSubFiles;

       // Process until exit button is pressed.
       DoU isExit = *on;

          ExSr ClrSubFiles;
          Exsr LoadTabs;
          Exsr ExfmtScreens;

          Reset isError;

          // Perform actions on screen.
          Select;
          When btnAddTmp = *on;
             btnAddTmp = *off;
             AddEditTemplate('Add');
          When isEdtTmp = *on;
             isEdtTmp = *off;
             AddEditTemplate('Edit':scTmpRrn);
          When isDltTmp = *on;
             isDltTmp = *off;
             AddEditTemplate('Delete':scTmpRrn);
          // do nothing - catch tab click event
          When tabClick = *on;
             If tabIndex <> saveTabIndex;
             Endif;
             tabClick = *off;
          When btnRefresh = *on;
             btnRefresh = *off;
          //***** Beg Add ***** 03/08/21 ***************************
          Other;
             isExit = *on;
          //***** End Add ***** 03/08/21 ***************************
          Endsl;

       Enddo;
       //===================================================================
       // Clear Subfiles
       //===================================================================
       BegSr ClrSubFiles;

        rrnTemplat = 0;
        clrTemplat = *on;
        dspTemplat = *off;
        Write RcdTempAsc;
        clrTemplat = *off;
        dspTemplat = *on;

       Endsr;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       aMenHeight = 120;

       Select;
       When TabIndex = 0;
          btnCancel = *off;
          TabHeight = 770;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          //***** Beg Chg ***** 03/08/21 ***************************
          //If isWindow = *off;
          If isWindow = *off and callpgm <> *on;
          //***** End Chg ***** 03/08/21 ***************************
             Write rcdSystbl;
             Write rcdPanel;
          Endif;
          Select;
          When isStub0;
             Exfmt RcdStub;
             isExit = *on;
          Other;
             Exfmt RcdTempAsc;
             isExit = btnCancel;
          Endsl;

          //***** Beg Chg ***** 03/08/21 ***************************
          //If isWindow = *off;
          If isWindow = *off and callpgm <> *on;
          //***** End Chg ***** 03/08/21 ***************************
             Read rcdSysTbl;
             Read rcdPanel;
          Endif;
          Dou %eof;
             Readc sflTmplate;
             Select;
             When %eof;
                Iter;
             When isDltTmp = *on;
                Leave;
             When isEdtTmp = *on;
                Leave;
             Endsl;
          Enddo;
       Endsl;

       Endsr;

       //===================================================================
       // Load the grid for Tab 0;
       //===================================================================
       Begsr LoadGrid0;
        Select;
        When strCall = 'Company';
           Clear dsWITMPLTparms.fnd;
           Clear dsWITMPLTparms.fein;
           Clear dsWITMPLTparms.mod;
           Clear dsWITMPLTparms.state;
        When strCall = 'Group';
           Clear dsWITMPLTparms.fein;
           Clear dsWITMPLTparms.mod;
           Clear dsWITMPLTparms.state;
        When strCall = 'Group/State';
           Clear dsWITMPLTparms.fein;
           Clear dsWITMPLTparms.mod;
        When strCall = 'Agency';
           Clear dsWITMPLTparms.fnd;
           Clear dsWITMPLTparms.state;
        When strCall = 'Agency/Group';
           Clear dsWITMPLTparms.state;
        Endsl;
        strSqlWhere = 'where TACO# = ' + %char(dsWITMPLTparms.co#) +
         '  and TAFND = ' + %char(dsWITMPLTparms.fnd) +
         '  and TAFEIN = ' + %char(dsWITMPLTparms.fein) +
         '  and TAMOD = ' + %char(dsWITMPLTparms.mod) +
         '  and TAST = ''' + dsWITMPLTparms.state + '''';
        strSql = 'select rrn(WITAP), TAco#, TAname, TAuserType, TAtype, ' +
         ' TAefDt, TAexDt' +
         ' from WITAP ' + strSqlWhere +
         ' order by TACO#, TATYPE, TANAME';
        scTmpKey = 'TCname = '' ''';
        Exec SQL close cursorAssoc0;
        Exec SQL prepare association0 from :strSql;
        Exec SQL declare cursorAssoc0 cursor for association0;
        Exec SQL open cursorAssoc0;
        Reset isEofWITAP;
        Dou isEofWITAP;
           Exec SQL fetch next from cursorAssoc0 into :dsWITAP0;
           Select;
           When sqlStt <> '00000';
              isEofWITAP = *on;
              Iter;
           Endsl;
           isEdtTmp = *off;
           isDltTmp = *off;
           scTmpRrn = dsWITAP0.rrn;
           Reset dsWITMP;
           Exec SQL
            select *
             into :dsWITMP
             from WITMP
             where TMCO# = :dsWITAP0.co#
              and TMNAME = :dsWITAP0.name
             limit 1;
           scCo# = dsWITAP0.co#;
           If dsWITAP0.userType = *blanks;
              dsWITAP0.userType = '*ALL';
           Endif;
           scUserType = dsWITAP0.userType;
           scType = dsWITAP0.type;
           scTmpName = dsWITAP0.name;
           scEfDt = CvtToDate(dsWITAP0.efDt);
           scExDt = CvtToDate(dsWITAP0.exDt);
           scMinPrmG = dsWITMP.TMminPrm;
           scMaxPrmG = dsWITMP.TMmaxPrm;
           scMinYrs  = dsWITMP.TMminYrs;
           scMaxLRa  = dsWITMP.TMmaxLRa;
           scMaxCLR  = dsWITMP.TMmaxCLR;
           scMaxModG = dsWITMP.TMmaxMod;
           scMinScore = dsWITMP.TMminScore;
           scMaxScore = dsWITMP.TMmaxScore;
           scReqConG = 'No';
           If dsWITMP.TMreqCon = 'Y';
              scReqConG = 'Yes';
           EndIf;
           scReqHisG = 'No';
           If dsWITMP.TMreqLHS = 'Y';
              scReqHisG = 'Yes';
           EndIf;
           scIeClCd = 'Include';
           If dsWITMP.TMieClCd = 'E';
              scIeClCd = 'Exclude';
           EndIf;
           rrnTemplat += 1;
           Write sflTmplate;
           If rrnTemplat = 1;
              scTmpKey = 'TCname = ''' + %trim(scTmpName) + '''';
           Endif;
        Enddo;
        Exec SQL close cursorAssoc0;
       Endsr;

       //===================================================================
       // Load Screen(s)
       //===================================================================
       BegSr LoadTabs;

        saveTabIndex = tabIndex;
        scPnlHdg0 = %trim(strCall) + ' Templates';

        Select;
        When TabIndex = 0;
           scStubHdg = strCall;
           Exsr LoadGrid0;
        When TabIndex = 1;
        EndSl;

       Endsr;

      /end-free
     PMain             E

       //===================================================================
       // Process - Determine if the program should run
       //===================================================================
     pProcess          b
     dProcess          pi             1n
      /free
       If not isModEnabled('Portal Module');
          PopOneBtn(strPortalModuleName +
           ' Module is not enabled. Please contact ' +
           %trim(strBrand) + ' for assistance.':
           strPortalModuleName + ' Module');
          Return *off;
       Endif;
       Return *on;
      /end-free
     pProcess          e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     pProgramStart     b
     dProgramStart     pi

      /free
      //***** Beg Chg ***** 03/08/21 ***************************
       //isWindow = *on;
       callpgm = *off;
       ChkStack('WTGRPSTRAT':CallPgm);
       callpgm2 = *off;
       ChkStack('WTGRPPOLCG':CallPgm2);
       //***** Beg Add ***** 10/26/21 ***************************
       callpgm3 = *off;
       ChkStack('WTAGTPRI':CallPgm3);         // Agency Primary Information
       callpgm4 = *off;
       ChkStack('WTAGTASN':CallPgm4);         // Agency Group Asignment
       If callpgm3 = *on or callpgm4 = *on;
          callpgm = *on;
       Endif;
       //***** End Add ***** 10/26/21 ***************************
       If callpgm = *on or callpgm2 = *on;
          callpgm = *on;
       Endif;
       If callpgm = *on;
          isGrpSt = *off;
          isWindow = *off;
       Else;
          isGrpSt = *on;
          isWindow = *on;
       Endif;
       //***** End Chg ***** 03/08/21 ***************************
       Select;
       When strMode = 'C'; // Change Mode
       When strMode = 'D'; // Display Mode
          modeDsp = *on;
       EndSl;
       isModeDsp = modeDsp;

       // Tab Index Mode.
       TabIndex = 0;

       // Get Company no.
       intCo# = dsWITMPLTparms.co#;
       // Get Company name
       scCompName = getCompNam(intCo#);

       RtvBrnd('C':strBrand);

       // Headings
       isKeyHdg0 = *off;
       scPnlHdg0 = %trim(strCall) + ' Templates';
       Select;
       When strCall = 'Company';
          isKeyHdg0 = *on;
          scKeyHdg0 = 'Company: ' + %trim(%editc(intCo#:'Z')) + ' - '
           + scCompName;
       When strCall = 'Group';
       When strCall = 'Group/State';
       When strCall = 'Agency';
       When strCall = 'Agency/Group';
       Other;
          scPnlHdg = 'Stub';
          isStub0 = *on;
       Endsl;
       scEditHdg0 = 'Add/Edit ' + scPnlHdg0;

       strPortalModuleName = %trim(GetModuleProperty('Portal Module':'Name'));

       hgUserType = strPortalModuleName + ' User Type';
       //***** Beg Chg ***** 11/17/23 ***************************
       //*** hgGrid = %trim(hgUserType) +
       //***  ',Association Type,Template Name,Effective Date,Expiration Date,';
       hgGrid = 'Connect User Type+
        ,Association Type,Template Name,Effective Date,Expiration Date,';
       //***** End Chg ***** 11/17/23 ***************************

       Return;

      /end-free
     pProgramStart     e
