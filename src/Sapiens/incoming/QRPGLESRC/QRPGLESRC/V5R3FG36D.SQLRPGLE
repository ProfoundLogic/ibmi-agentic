     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WTCLMICD - ICD-10 Codes
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Process ICD-10 Codes.
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created on 4/20/2015 by C:279 for Project: WC2043
     h**********************************************************************
     h*  Revisions:
     H*                                                               *
     H* 06/11/15 C:306  Project WC2043 (Priss 26)                     *
     H*                - To add the filter criteria functionality     *
     H*                  to filter the ICD 10 code.                   *
     H*                                                               *
     h**********************************************************************

     fwtclmicd  cf   e             workstn extfile('WTCLMICD')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflICDCde:rrnICDCde)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')

     fwmicd10p  uf a e             disk    rename(pic01:pic0p)
     f                                     InfDs(dbfds1)
     fwmicd10l  if   e           k disk    rename(pic01:pic00)
     f                                     InfDs(dbfds2) prefix(a1)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0

      * work variables
     d charPrcMode     s              1    inz
     d flgRcdUpdated   s              1
     d inMode          s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d isValid         s               n
     d SVdt            s              7  0
     d SVtime          s              6  0
      *** beg chg ***** 06/11/15 *************************
     D isFilter        s              1n   inz
     D chartmpcod      s             50    inz
     D chartmpdsc      s             50    inz
      *** end chg ***** 06/11/15 *************************

      * prototypes
     d WrkICD10Cd      pr                  extpgm('WTCLMICD')
     d  ##Mode                        1
     d  ##TabIndex                    5

     d WrkICD10Cd      pi
     d  @@Mode                        1
     d  @@TabIndex                    5

      * clear subfiles
     d ClrSubFiles     pr

      * Edit/Add ICD-10 code
     d EditAddICD      pr

      *** beg chg ***** 06/11/15 *************************
      **** Load ICD-10 code grid
     d**** LoadICDCode     pr
     D*
      * Load ICD-10 code grid using searching criteria
     D SrchICDCode     pr
      *** end chg ***** 06/11/15 *************************

      * Main Processing
     d Main            pr

      * Move database to screen fields
     d MoveDbsc        pr

      * Process ICD-10 code
     d ProcICD         pr

      * Program Start
     d ProgramStart    pr

      * Process other options
     d ProcOther       pr

      * Validate
     d Validate        pr

     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany
     d/copy sPrGenPop
     d/copy wPrGroup
     d/copy wPrUsrPrms
    
      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inlr = *on;
       return;
      /end-free
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Program Mode.
       If %parms() >= 1;
          inMode = @@Mode;
       Endif;

       tabClick = *Off;
       dspMode = *Off;
       If inMode = 'D';
          dspMode = *On;
       Endif;

       // Tab Index Mode.
       If %parms() >= 2;
          inTabIndex = %dec(@@TabIndex:5:0);
       Else;
          inTabIndex = 0;
       Endif;

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1User);

       // Get company name
       scCompName = GetCompNam(intCo#);
       aMenHeight = 50;
       apFootHght = aMenHeight + 125;
       apShowMenu = *off;

       // *** beg chg ***** 06/11/15 *************************
         InSrcVis = *On;
         scSrchType = 'C';
       // *** end chg ***** 06/11/15 *************************
      /end-free
     p ProgramStart    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *on ;

       // *** beg chg ***** 06/11/15 *************************
       // // Clear Sub-files
       // ClrSubfiles();

       // // Load ICD-10 code grid
       // LoadICDCode();

       // Process ICD-10 code Screens
          Exsr ExfmtScreens;
       // *** end chg ***** 06/11/15 *************************

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
       // *** beg chg ***** 06/11/15 *************************
              If InSrcVis = *Off;
                ClrSubfiles();
                SrchICDCode();
               Else;
                 scIcdCode  = chartmpcod;
                 scICDDsc   = chartmpdsc;
              EndIf;
       // *** end chg ***** 06/11/15 *************************

          // Add ICD-10 code
          When scICDAdd = *on;
             charPrcMode = 'A';
             EditAddICD();
       // *** beg chg ***** 06/11/15 *************************

          // Seach ICD-10 code or ICD-10 description
          When BtnSearch = *On;
          // Clear Sub-files
               BtnSearch  = *Off;
          //      Clear Sub-files
               ClrSubfiles();
               SrchICDCode();

          // Seach ICD-10 code Filter again
          When ScICDfiltr = *On;
               ScICDfiltr = *off;
               ClrSubfiles();
               InSrcVis  = *On;

          // Cancel Search Screen
          When BtnCanl = *On;
               ExitNow = *On;
               Leave;
       // *** end chg ***** 06/11/15 *************************

          Other;
             ProcOther();
          Endsl;
       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 760;
          ApnlHeight = TabHeight + 60;
          Write rcdSysTbl;
          Write rcdTabICD;
        // ****  06/11/2015 Change Begins  ************
        If InSrcVis = *On Or Isfilter = *On;
          Exfmt RcdSrchIcd;
         Else;
          Exfmt rcdICDCode;
        EndIf;
        // ****  06/11/2015 Change Ends    ************
          Read rcdSysTbl;
          Read rcdTabICD;
       EndSl;

       Endsr;

      /end-free
     p Main            e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /free

       //****** 06/11/2015 Change Begins ********//
       InSrcVis = *Off;
       Isfilter = *Off;
       //****** 06/11/2015 Change Ends  ********//
       rrnICDCde  = 0;
       clrICDCde  = *on;
       dspICDCde  = *off;
       Write RcdICDCode;

      /end-free
     p ClrSubfiles     e
       //===================================================================
       // Load ICD-10 code grid
       //===================================================================
      *** beg chg ***** 06/11/15 *************************
     p*** LoadICDCode     b
     d*** LoadICDCode     pi

      // /free

       // clear the subfile.
       // clrICDCde = *off;
       // dspICDCde = *on;
       // clear dbRrn1;

       // Setll 1 pic0p;
       // Read(n) pic0p;
       // Dow not %eof(WMICD10P);

          // Code
        //  scICD10Cde = ICcode;

          // Description
       //   scICDDesc =%trim(ICdsc1);

       //  Diagnostic/Operative
       //   scICDDO = ' ';
       //   If IClost = 'D';
       //      scICDDO = 'Diagnostic';
       //   ElseIf IClost = 'O';
       //    scICDDO = 'Operative';
       // Endif;

          // Actual value
       // scICDActVl = ICfCode;

          // Length of Stay Group
       // scICDLOSGp = %trim(IClGrp);

          // Status
       // If ICstat = 'A';
       //    scStatus = 'Active';
       // Else;
       //    scStatus = 'Inactive';
       // Endif;

       // scICDRRN# = dbRrn1;

       // rrnICDCde = rrnICDCde + 1;
       // If rrnICDCde = 1;
       //    scICDRwSel = 'A';
       // Else;
       //    clear scICDRwSel;
       // Endif;

       // Write sflICDCde;

       // Read(n) pic0p;
       // Enddo;

       // rrnICDCde  = 1;

      //  /end-free
     p**** LoadICDCode     e
     P
     p SrchICDCode     b
     D SrchICDCode     pi
     D
     D strSqlCursor    s           5000    Inz
     DstrSqlWhere      s            600    Inz
     DstrICDCode       s                   like(scIcdCode)
     DstrICDDesc       s                   like(scICDDesc)
     Dstr9999Msg       s            256
     D
     DFetchdata        DS
     DDataICcode                      7
     DDataICdsc1                     50
     DDataIClost                      1
     DDataICfCode                     8
     DDataIClGrp                     10
     DDataICstat                      1
     DDataRrn                         9s 0

      /free

       // clear the subfile.
       clrICDCde = *Off;
       dspICDCde = *On;
       clear DataRrn;

       strICDCode  =  scIcdCode;
       strICDDesc  =  scICDDsc;
       chartmpcod  =  scIcdCode;
       chartmpdsc  =  scICDDsc;

       strSqlCursor = 'Select ICcode,ICdsc1,IClost,ICfCode,IClGrp,ICstat, ' +
                      'RRN(WMICD10P) From WMICD10P';

       If scSrchType = 'C';                  // Contains             // scSrchType If

         Select;

           When strICDCode <> ' ' And strICDDesc <> ' ';
             strSqlWhere = ' where upper(ICcode) Like Upper(''%' +
                          %trim(strICDCode) + '%'')' + ' And ' +
                          'Upper(ICDSC1) Like Upper(''%' +
                          %trim(strICDDesc) + '%'')';


           When strICDCode <> ' ' And strICDDesc =  ' ';
             strSqlWhere = ' where upper(ICcode) like Upper(''%' +
                          %trim(strICDCode) + '%'')';

           When strICDCode = ' ' And strICDDesc <> ' ';
             strSqlWhere = ' where upper(ICDSC1) Like upper(''%' +
                           %trim(strICDDesc) + '%'')';

           When strICDCode = ' ' And strICDDesc =  ' ';
              strSqlWhere = *blanks;

         EndSl;

       Elseif scSrchType = 'B';                // Begins With

        Select;

          When strICDCode <> ' ' And strICDDesc <> ' ';
            strSqlWhere = ' where upper(ICcode) Like Upper('+ '''' +
                          %trim(strICDCode) + '%'')' + ' And ' +
                          'Upper(ICDSC1) Like Upper(''%' +
                          %trim(strICDDesc) + '%'')';


          When strICDCode <> ' ' And strICDDesc =  ' ';
            strSqlWhere = ' where upper(ICcode) like Upper('+ '''' +
                          %trim(strICDCode) + '%'')';

          When strICDCode = ' ' And strICDDesc <> ' ';
            strSqlWhere = ' where upper(ICDSC1) Like upper(''%' +
                           %trim(strICDDesc) + '%'')';

          When strICDCode = ' ' And strICDDesc =  ' ';
            strSqlWhere = *blanks;

        EndSl;

       EndIf;                                                        // scSrchType EndIf

       strSqlCursor = %trim(strSqlCursor) + ' ' + %trim(strSqlWhere);  // Final SQL stmt

       Exec SQL Prepare sql1 From :strSqlCursor;
       Exec SQL Declare cursor1 Cursor For sql1;
       Exec SQL Open cursor1;
       Exec SQL Fetch cursor1 into: Fetchdata;

       Dow SQLCODE <> 0100;

          // Code
          scICD10Cde = DataICcode;

          // Description
          scICDDesc =%trim(DataICdsc1);

          // Diagnostic/Operative
          scICDDO = ' ';

          If DataIClost = 'D';
             scICDDO = 'Diagnostic';
           ElseIf DataIClost = 'O';
             scICDDO = 'Operative';
          Endif;

          // Actual value
          scICDActVl = DataICfCode;

          // Length of Stay Group
          scICDLOSGp = %trim(DataIClGrp);

          // Status
          If dataICstat = 'A';
             scStatus = 'Active';
           Else;
             scStatus = 'Inactive';
          Endif;


          rrnICDCde = rrnICDCde + 1;
          scICDRRN# = DataRrn;

          If rrnICDCde = 1;
             scICDRwSel = 'A';
           Else;
             clear scICDRwSel;
          Endif;

          If rrnICDCde > 9999;
            str9999msg = 'Your search returned too many records. ' +
             'Click on Results to see first 9999 records Or, click Filter ' +
             'to narrow down your search results';
            isFilter = PopTwoBtn(str9999msg:'Search':'Filter':'Results');
            Leave;
          Endif;

          Write sflICDCde;       // Writing the Subfile with ICD codes.


       Exec SQL Fetch next from cursor1 into: Fetchdata;

       Enddo;

       Exec SQL Close cursor1;

       rrnICDCde  = 1;

       If Isfilter = '1';               // Filter again
          InSrcVis = *On;
       EndIf;

      /end-free
     p SrchICDCode     e
      *** end chg ***** 06/11/15 *************************
       //==================================================================
       // EditAddICD - Process ICD-10 code add/edit
       //==================================================================
     p EditAddICD      b
     d EditAddICD      pi

      /free

       clear rcdICDAdd;
       clear pic0p;
       BtnICDSave = *off;
       BtnCancel  = *off;
       isValid    = *off;

       // processing for Edit mode
       If charPrcMode = 'C';
          dbAddSts = *off;
          dbAddICDCd = *on;

          Chain(n) scICDRRN# pic0p;
          MoveDbSc();
       Endif;

       // processing for Add mode
       If charPrcMode = 'A';
          scAddSts = 'A';
          dbAddSts = *on;
          scAddDOGrp = ' ';
          dbAddICDCd = *off;
       Endif;

       Dou BtnCancel = *on or (BtnICDSave = *on and isValid = *off);
          Exfmt rcdICDAdd;
          isValid = *off;
          erAddICDCd = *off;
          erAddActVl = *off;
          erAddDesc  = *off;
          erAddDOGrp = *off;

          Select;
          When BtnICDSave = *on;
             Validate();
             If isValid = *off;
                ProcICD();
             Endif;
          When BtnCancel = *on;
             BtnCancel = *off;
             Leave;
          Endsl;
       Enddo;
       unlock WMICD10P;

      /end-free
     p EditAddICD      e
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

      /free

       // Edit Payment value 1 codes
       Readc sflICDCde;

       Dow not %eof;
          If scICDEdt = *on;
             scICDEdt = *off;
             charPrcMode = 'C';
             EditAddICD();
          Endif;

          Readc sflICDCde;
       Enddo;

      /end-free
     p ProcOther       e
       //===================================================================
       // Validate - Validate add/edit screen
       //===================================================================
     p Validate        b
     d Validate        pi

      /free

       isValid  = *off;
       erAddICDCd = *off;
       erAddActVl = *off;
       erAddDesc  = *off;
       erAddDOGrp = *off;

       If charPrcMode = 'C';
          svDt   = ICdt;
          svTime = ICtime;
          // Check if updated since screen displayed
          flgRcdUpdated = RcdUpdatedErr('WMICD10P':scICDRRN#:'ICdt':'ICtime':
           'ICuser':svDt:svTime);
          Select;
          When flgRcdUpdated = '0'; // No error
          When flgRcdUpdated = '1'; // Error
             isValid = *on;
             Return;
          When flgRcdUpdated = '2'; // Error with refresh
          Endsl;
       Endif;

       // ICD-10 Code is required entry
       If scAddICDCd = *blanks;
          msAddICDCd = 'WCR0503';
          erAddICDCd = *on;
          isValid  = *on;
       Endif;

       // Actual Value is required entry
       If scAddActVl = *blanks;
          msAddActVl = 'WCR0503';
          erAddActVl = *on;
          isValid = *on;
       Endif;

       // Description is required entry
       If scAddDesc = *blanks;
          msAddDesc = 'WCR0503';
          erAddDesc = *on;
          isValid = *on;
       Endif;

       // Verify if duplicate record
       If scAddICDCd <> ' ';
          Chain (scAddICDCd) pic00;
          If %found(WMICD10L) and (charPrcMode = 'A' or (charPrcMode = 'C' and
           dbRrn2 <> scICDRrn#));
             msAddICDCd = 'WC30661';
             erAddICDCd = *on;
             isValid = *on;
          Endif;
       Endif;

      /end-free
     P Validate        E
       //===================================================================
       // ProcICD - Process ICD-10 code
       //===================================================================
     p ProcICD         b
     d ProcICD         pi

      /free

       Chain scICDRRN# pic0p;

       ICcode = Upper(scAddICDCd);
       ICdsc1 = scAddDesc;
       ICdsc2 = scAddDesc2;
       IClost = scAddDOGrp;
       IClGrp = scAddLOSGr;
       ICstat = scAddSts;
       ICfCode = Upper(scAddActVl);

       If charPrcMode = 'A';
          ICspecific = ' ';
          ICiusr = q1user;
          ICipgm = q1pgm;
          ICidt  = Dateto7(SystemDate());
          ICitim = Timeto6(SystemTime());
          Write pic0p;
       Else;
          ICuser = q1user;
          ICpgm  = q1pgm;
          ICdt   = Dateto7(SystemDate());
          ICtime = Timeto6(SystemTime());
          Update pic0p;
       Endif;
       Unlock WMICD10P;

      /end-free
     p ProcICD         e
       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     p MoveDbSc        b
     d MoveDbSc        pi

      /free

       scAddICDCd = ICcode;
       scAddDesc  = ICdsc1;
       scAddDesc2 = ICdsc2;
       scAddDOGrp = IClost;
       scAddLOSGr = IClGrp;
       scAddSts   = ICstat;
       scAddActVl = ICfCode;

      /end-free
     p MoveDbSc        e
