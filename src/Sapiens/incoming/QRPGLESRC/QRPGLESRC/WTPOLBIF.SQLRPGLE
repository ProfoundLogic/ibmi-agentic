     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTPOLBIF - BIF Failure Select
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Allow user to process failed
     h*                BIFs.
     h*
     h*  Called from: Menu
     h*  Calls to:    None
     h*
     h**********************************************************************
     h*  Created on 06/27/2023 by C:062 for Project: WC2879
     h**********************************************************************
     h*  Revisions:                                                        *
     h*                                                                    *
     h* 09/03/24 C:291 Project: WC2968                                     *
     h*                Added IVC Failure column on sflBIFFail.             *
     h*                Changed HdgBIFFailure to HdgIVCFailure and it's     *
     h*                value to "IVC Failure Listing"                      *
     h*                PnlBIFFilter: Changed value to "IVC Select Filter". *
     h*                rcdBifFlt: Added BIF Failure Type on Filter screen. *
     h*                rcdBifDsp: Added BIF Failure Type on Display screen.*
     h*                UnBind visibility field VSRRE and VSNCCI from       *
     h*                Resubmit status and File Data Label on Filter screen*
     h*                                                                    *
     h**********************************************************************
     fwtpolbif  cf   e             workstn extfile('WTPOLBIF')
     f                                     Sfile(sflBifFail:rrnBifFail)
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Include(rcdComPnl)

     d psds          esds                  extname(szq1p)

      * constant
     d intFlgDim       c                   10
     d low             c                   'abcdefghijklmnopqrstuvwxyz'
     d up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d/Copy siWebSvcPr
     d/Copy sPrAtrium
     d/copy SPrCompany
     d/copy sPrDate
     d/copy sPrGenPop
     d/Copy sPrSql
     d/copy sPrString
     d/copy wPrUsrPrms

     d ##CltId         s              3
     d @@call          s              1
     d chrResub        s              1
     d clientID        s              3    inz('000')
     d decTrDt7        s              7  0
     d intCo#          s              3  0
     d strSql          s          32000

     d decBifRrn       s              9  0
     d decBifTrDt      s              7s 0
     d decBifTrTm      s              6s 0
     d chrBifResub     s              1
     d chrStatus       s              1
     d chrBifMsg       s            100
     d clbBifData      s                   Sqltype(clob:64000)
      //*** Beg Add ***** 09/03/24 **********************************
     d chrBifFlTyp     s             50
      //*** End Add ***** 09/03/24 **********************************

     C     *Entry        Plist
     C                   Parm                    @@call

      /free

       // Main Procesing
       Main();

       *inlr = *on;

      /end-free

       //===================================================================
       // ClearErrors - Clear Error Indicators
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi

      /free


      /end-free

     p ClearErrors     e
       //===================================================================
       // ClearFilter - Clears Filter
       //===================================================================
     p ClearFilter     b
     d ClearFilter     pi

      /free

       ssTrDt  = 0;
       ssTrTm  = 0;
       ssMsg   = ' ';
       ssData  = ' ';
       ssResub = ' ';
       //*** Beg Add ***** 09/03/24 **********************************
       ssBIFFlTyp = ' ';
       //*** End Add ***** 09/03/24 **********************************
       noFilter = *On;

      /end-free

     p ClearFilter     e

       //===================================================================
       // DisplayBif - Displays the Failed BIF Information
       //===================================================================
     p DisplayBif      b
     d DisplayBif      pi

      /free

       // Reset Indicators
       dspBif=*Off;

       // Read subfile
       ReadC sflBifFail;

       Select;
       When dspBif = *On;
          //*** Beg Chg ***** 09/03/24 *********************************
          //***Exec Sql Select BImsg, BIdata Into :scReqMsg, :scReqData
          //***         from wibifp
          //***         Where rrn(wibifp) = :scBifRrn;
          Exec Sql Select BImsg, BIdata, BItype
                   Into   :scReqMsg, :scReqData, :scReqFlTyp
                   from   wibifp
                   Where  rrn(wibifp) = :scBifRrn;
          //*** End Chg ***** 09/03/24 *********************************
          ExFmt RcdBifDsp;
       EndSl;

      /end-free

     p DisplayBif      e

       //===================================================================
       // FilterBIFs - Filter BIF Listing
       //===================================================================
     p FilterBIFs      b
     d FilterBIFs      pi

     d error           s               n
     d hldTrDt         s                   like(ssTrDt)
     d hldTrTm         s                   like(ssTrTm)
     d hldMsg          s                   like(ssMsg)
     d hldData         s                   like(ssData)
     d hldResub        s                   like(ssResub)
      //*** Beg Add ***** 09/03/24 ******************************
     d hldBIFFlTyp     s                   like(ssBIFFlTyp)
      //*** End Add ***** 09/03/24 ******************************

      /free

       // Hold Original Values
       hldTrTm  = ssTrTm;
       hldTrTm  = ssTrTm;
       hldMsg   = ssMsg;
       hldData  = ssData;
       hldResub = ssResub;
      //*** Beg Add ***** 09/03/24 ******************************
       hldBIFFlTyp = ssBIFFlTyp;
      //*** End Add ***** 09/03/24 ******************************

       DoU cnlFilter = *On Or (appFilter = *On And error = *Off);

          ExFmt RcdBifFlt;

          // reset error fields
          erTrDt   = *Off;
          mgTrDt   = ' ';
          error    = *Off;

          // Validate filter
          // Date must be valid
          If ssTrDt <> 0;
             If dtCheck(ssTrDt) = 0;
                erTrDt = *On;
                mgTrDt = 'Invalid Date';
                error  = *On;
             Else;
                ssTrDt = dtCheck(ssTrDt);
             EndIf;
          EndIf;

          Select;
          When cnlFilter = *On;
             // Restore Original Values
             ssTrTm  = hldTrTm;
             ssTrTm  = hldTrTm;
             ssMsg   = hldMsg;
             ssData  = hldData;
             ssResub = hldResub;
             //*** Beg Add ***** 09/03/24 ******************************
             ssBIFFlTyp = hldBIFFlTyp;
             //*** End Add ***** 09/03/24 ******************************
          When error = *On;
          Other;
             Select;
             When ssTrDt <> 0;
                noFilter = *Off;
             When ssTrTm <> 0;
                noFilter = *Off;
             When ssMsg <> ' ';
                noFilter = *Off;
             When ssData <> ' ';
                noFilter = *Off;
             When ssResub <> ' ';
                noFilter = *Off;
             //*** Beg Add ***** 09/03/24 ***********************
             When ssBIFFlTyp <> ' ';
                noFilter = *Off;
             //*** End Add ***** 09/03/24 ***********************
             Other;
                noFilter = *On;
             EndSl;
          EndSl;

       EndDo;

      /end-free

     p FilterBIFs      e
       //===================================================================
       // LoadBifGrid - Load the BIF Select Grid
       //===================================================================
     p LoadBifGrid     b
     d LoadBifGrid     pi

     d subsetted       s               n

      /free

       rrnBifFail = 0;
       clrBifFail = *on;
       dspBifFail = *off;
       Write rcdBifFail;
       dspBifFail = *on;
       clrBifFail = *off;
       rrnBifFail = 0;

       ExSr BuildSql;

       Exec Sql Prepare sqlWIBIFP From :strSql;
       Exec Sql Declare cursorWIBIFP Cursor For sqlWIBIFP;
       Exec Sql Open cursorWIBIFP;

       Dou Not IsValidFetch(sqlStt);
          //*** Beg Chg ***** 09/03/24 ***************************************
          //***Exec SQL fetch next from cursorWIBIFP into :decBifRrn, :decBifTrDt,
          //***  :decBifTrTm,:chrBifMsg, :chrBifResub, :chrStatus, :clbBifData;
          Exec SQL fetch next from cursorWIBIFP into :decBifRrn, :decBifTrDt,
            :decBifTrTm,:chrBifMsg, :chrBifResub, :chrStatus, :clbBifData,
            :chrBifFlTyp;
          //*** End Chg ***** 09/03/24 ***************************************
          If IsValidFetch(sqlStt);
             scIEfl     = chrStatus;
             //*** Begin Chg *** 08/28/25 **********************************
             //*** scTrDt     = dtMDYY(decBifTrDt);
             scTrDt     = CvtToDate(decBifTrDt);
             //*** End   Chg *** 08/28/25 **********************************
             scTrTm     = decBifTrTm;
             scMsg      = chrBifMsg;
             toolMsg    = chrBifMsg;
             scData     = %Subst(clbBifData_data:1:500);
             toolData   = %Subst(clbBifData_data:1:31000);
             //*** Beg Add ***** 09/03/24 **********************************
             scBIFFlTyp = chrBifFlTyp;
             toolFlTyp  = chrBifFlTyp;
             //*** End Add ***** 09/03/24 **********************************

             clbBifData_data = ' ';

             Select;
             When chrBifResub = 'E';
                scResub    = 'Unavailable';
                toolBif    = 'You can''t select this item for submission,' +
                             ' it is unavailable.';
             When chrBifResub = 'Y';
                scResub    = 'Submitted';
                toolBif    = 'You can''t select this item for submission,' +
                             ' because it has already been submitted.';
             Other;
                scResub    = ' ';
                toolBif    = 'This item can be resubitted.';
             EndSl;

             cantSelect = *Off;
             If chrBifResub <> ' ';
                cantSelect = *On;
             EndIf;

             scBifRrn   = decBifRrn;
             rrnBifFail = rrnBifFail + 1;
             write sflBifFail;
          Else;
             Leave;
          Endif;
       Enddo;

       Exec SQL close cursorWIBIFP;

       //===========================================processre===============
       // Build SQL
       //===================================================================
       BegSr BuildSQL;

          subsetted  = *Off;
          strSql     = 'Select rrn(WIBIFP), BItrDt, BItrTm, BImsg, ' +
                       'BIresub, BIstat, BIdata ' +
                       //*** Beg Add ***** 09/03/24 ***********************
                       ', BItype ' +
                       //*** End Add ***** 09/03/24 ***********************
                       'from wibifp';

          // Apply filters
          //Transaction date
          If ssTrDt <> 0;
             decTrDt7 = dtCYMD(ssTrDt);
             If subsetted;
                strSql = %Trim(strSql) + ' And BItrDt = ' +
                         %EditC(decTrDt7:'X');
             Else;
                strSql = %Trim(strSql) + ' Where BItrDt = ' +
                         %EditC(decTrDt7:'X');
                subsetted  = *On;
             EndIf;
          EndIf;

          //Transaction Time
          If ssTrTm <> 0;
             If subsetted;
                strSql = %Trim(strSql) + ' And BItrTm = ' +
                         %EditC(ssTrTm:'X');
             Else;
                strSql = %Trim(strSql) + ' Where BItrTm = ' +
                         %EditC(ssTrTm:'X');
                subsetted  = *On;
             EndIf;
          EndIf;

          // Message(contains)
          If ssMsg <> ' ';
             If subsetted;
                strSql = %Trim(strSql) + ' And Upper(BImsg) like ''%' +
                         %Trim(Upper(ssMsg)) + '%''';
             Else;
                strSql = %Trim(strSql) + ' Where Upper(BImsg) like ''%' +
                         %Trim(Upper(ssMsg)) + '%''';
                subsetted  = *On;
             EndIf;
          EndIf;

          // Request Data
          If ssData <> ' ';
             If subsetted;
                strSql = %Trim(strSql) + ' And Upper(BIdata) like ''%' +
                         %Trim(Upper(ssData)) + '%''';
             Else;
                strSql = %Trim(strSql) + ' Where Upper(BIdata) like ''%' +
                         %Trim(Upper(ssData)) + '%''';
                subsetted  = *On;
             EndIf;
          EndIf;

          // Resubmission
          If ssResub <> ' ';

             If ssResub = 'N';
                chrResub = ' ';
             Else;
                chrResub = ssResub;
             EndIf;

             If subsetted;
                strSql = %Trim(strSql) + ' And Upper(BIresub) = ''' +
                         chrResub + '''';
             Else;
                strSql = %Trim(strSql) + ' Where Upper(BIresub) = ''' +
                         chrResub + '''';
                subsetted  = *On;
             EndIf;
          EndIf;

          //*** Beg Add ***** 09/03/24 **********************************
          // BIF Failure Type
          If ssBIFFlTyp <> ' ';
             If subsetted;
                strSql = %Trim(strSql) + ' And Upper(BIType) = ''' +
                         %Trim(Upper(ssBIFFlTyp)) + '''';
             Else;
                strSql = %Trim(strSql) + ' Where Upper(BIType) = ''' +
                         %Trim(Upper(ssBIFFlTyp)) + '''';
                subsetted  = *On;
             EndIf;
          EndIf;
          //*** End Add ***** 09/03/24 **********************************
          //*** Begin Add *** 08/28/25 **********************************
          strSql = %Trim(strSql) + ' Order by BItrDt desc, BItrTm desc';
          //*** End   Add *** 08/28/25 **********************************

       EndSr;

      /end-free

     p LoadBifGrid     e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     D error           s               n   Inz(*Off)

      /free

       // Start Of Program
       ProgramStart();

       // Process until exit button is pressed.
       Dou  scExit = *On Or *inKC = *On;

          // Load Group Select Grid
          LoadBifGrid();

          // Process Payment Value 1 code Screens
          aPnlHeight = 625;
          Write rcdComPnl;
          Write rcdBifPnl;
          ExFmt rcdBifFail;
          Read  rcdComPnl;
          Read  rcdBifPnl;

          // Clear Error Indicators
          ClearErrors();

          Select;
          When scIncAll = *On;  // Change all items to Included for resub
             UpdWibifpSts('I');
          When scExclAll = *On; // Change all items to Excluded for resub
             UpdWibifpSts('E');
          When scSubset = *On;  // Engage subsets
             FilterBIFs();
          When scClrFltr = *On; // Clear the filter
             ClearFilter();
          When scSubmit = *On;  // Resubmit BIFs
             ProcessReSubmit();
          When scExit = *On;    // Used when exit is used
          Other;
             ProcessBif();      // deals with status change and displaying
          EndSl;
       Enddo;

       ProgramEnd();

      /end-free

     p Main            e

       //===================================================================
       // ProcessBif - Execute Bif processes like display and change status
       //===================================================================
     p ProcessBif      b
     d ProcessBif      pi

      /free

       // Reset Indicators
       scIEFLc = *Off;
       dspBif  =*Off;

       // Read subfile
       ReadC sflBifFail;

       Select;
       When scIEFLc = *On;
          Exec Sql Update wibifp
                   set BIstat = :scIEFL
                   Where rrn(wibifp) = :scBifRrn;
       When dspBif = *On;
          //*** Beg Chg ***** 09/03/24 *********************************
          //***Exec Sql Select BImsg, BIdata Into :scReqMsg, :scReqData
          //***         from wibifp
          //***         Where rrn(wibifp) = :scBifRrn;
          Exec Sql Select BImsg, BIdata, BItype
                   Into   :scReqMsg, :scReqData, :scReqFlTyp
                   from   wibifp
                   Where  rrn(wibifp) = :scBifRrn;
          //*** End Chg ***** 09/03/24 *********************************
          ExFmt RcdBifDsp;
       EndSl;

      /end-free

     p ProcessBif      e

       //===================================================================
       // ProcessResubmit - Process Submissions
       //===================================================================
     p ProcessReSubmit...
     p                 b
     d ProcessReSubmit...
     d                 pi

      /free

       // Read selected records

       strSql     = 'Select rrn(WIBIFP), BIdata ' +
                    'from wibifp ' +
                    'Where BIstat = ''I''';

       Exec Sql Prepare sqlWIBIFPUpd2 From :strSql;
       Exec Sql Declare cursorWIBIFPUpd2 Cursor For sqlWIBIFPUpd2;
       Exec Sql Open cursorWIBIFPUpd2;

       Dou Not IsValidFetch(sqlStt);
          Exec SQL fetch next from cursorWIBIFPUpd2 into :decBifRrn,
                                                         :clbBifData;
          If IsValidFetch(sqlStt);
             // Load the bif
             LoadSIWEBSVCDataQ(clbBifData);
             // Update Bif to indicate it is submitted
             Exec Sql Update wibifp
                      Set BIresub = 'Y', BIstat = 'E'
                      Where rrn(wibifp) = :decBifRrn;
          Else;
             Leave;
          Endif;
       Enddo;

       Exec SQL close cursorWIBIFPUpd2;

      /end-free

     p ProcessResubmit...
     p                 e

       //===================================================================
       // ProgramEnd - Final Processing
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi

      /free

       Exec Sql Update WIBIFP Set BIstat = 'E' Where BIstat = 'I';

      /end-free

     p ProgramEnd      e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);

       // Get the client id
       GetClientID(##CltId);

       // Activate Filter if Used
       noFilter = *Off;
       vsSubset = *On;
       SSresub  = 'N';

      /end-free
     p ProgramStart    e

       //===================================================================
       // UpdWibifpSts - Change the submit status
       //===================================================================
     p UpdWibifpSts    b
     d UpdWibifpSts    pi
     d   bifStatusIn                  1a   Const

     d subsetted       s               n

      /free

       ExSr BuildSql;

       Exec Sql Prepare sqlWIBIFPUpd From :strSql;
       Exec Sql Declare cursorWIBIFPUpd Cursor For sqlWIBIFPUpd;
       Exec Sql Open cursorWIBIFPUpd;

       Dou Not IsValidFetch(sqlStt);
          Exec SQL fetch next from cursorWIBIFPUpd into :decBifRrn;         ,
          If IsValidFetch(sqlStt);
             Exec Sql Update wibifp
                      Set BIstat = :bifStatusIn
                      Where rrn(wibifp) = :decBifRrn;
          Else;
             Leave;
          Endif;
       Enddo;

       Exec SQL close cursorWIBIFPUpd;

       //===================================================================
       // Build SQL
       //===================================================================
       BegSr BuildSQL;

          subsetted  = *Off;
          strSql     = 'Select rrn(WIBIFP) ' +
                       'from wibifp ' +
                       'where biresub = '' ''';

          // Apply filters
          //Transaction date
          If ssTrDt <> 0;
             decTrDt7 = dtCYMD(ssTrDt);
             If subsetted;
                strSql = %Trim(strSql) + ' And BItrDt = ' +
                         %EditC(decTrDt7:'X');
             Else;
                strSql = %Trim(strSql) + ' Where BItrDt = ' +
                         %EditC(decTrDt7:'X');
                subsetted  = *On;
             EndIf;
          EndIf;

          //Transaction Time
          If ssTrTm <> 0;
             If subsetted;
                strSql = %Trim(strSql) + ' And BItrTm = ' +
                         %EditC(ssTrTm:'X');
             Else;
                strSql = %Trim(strSql) + ' Where BItrTm = ' +
                         %EditC(ssTrTm:'X');
                subsetted  = *On;
             EndIf;
          EndIf;

          // Message(contains)
          If ssMsg <> ' ';
             If subsetted;
                strSql = %Trim(strSql) + ' And Upper(BImsg) like ''%' +
                         %Trim(Upper(ssMsg)) + '%''';
             Else;
                strSql = %Trim(strSql) + ' Where Upper(BImsg) like ''%' +
                         %Trim(Upper(ssMsg)) + '%''';
                subsetted  = *On;
             EndIf;
          EndIf;

          // Request Data
          If ssData <> ' ';
             If subsetted;
                strSql = %Trim(strSql) + ' And Upper(BIdata) like ''%' +
                         %Trim(Upper(ssData)) + '%''';
             Else;
                strSql = %Trim(strSql) + ' Where Upper(BIdata) like ''%' +
                         %Trim(Upper(ssData)) + '%''';
                subsetted  = *On;
             EndIf;
          EndIf;

          // Resubmission
          If ssResub <> ' ';
             If ssResub = 'N';
                chrResub = ' ';
             Else;
                chrResub = ssResub;
             EndIf;

             If subsetted;
                strSql = %Trim(strSql) + ' And Upper(BIresub) = ''' +
                         chrResub + '''';
             Else;
                strSql = %Trim(strSql) + ' Where Upper(BIresub) = ''' +
                         chrResub + '''';
                subsetted  = *On;
             EndIf;
          EndIf;

       EndSr;

      /end-free

     p UpdWibifpSts    e
