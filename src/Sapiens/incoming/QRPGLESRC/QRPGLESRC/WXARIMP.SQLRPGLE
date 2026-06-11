     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     hOption(*nodebugio:*srcstmt)
     hDftactgrp(*no) Actgrp(*CALLER)
     hBnddir('WTBNDDIR':'QC2LE')
      //======================================================
      // Called from WXARIMPC.
      // Processes import file:
      //  Writes Upload Report WXARIMPO
      //  Writes Exception Text file if any transactions
      //   with errors exist.
      //  Sends email if email address exists in WTPOLCFG
      //   with upload report and exception file if applicable.
      //======================================================
      // Revisions:
      //
      // 11/07/23 C:291 Support: 114006
      //                Change ProcessDetail to read WXARIMPF
      //                sequentially instead of SQL.
      //
      // 07/12/23 C:342 Support: 112988
      //                update program to iterate the seconds value
      //                in the timestamp within each batch to ensure
      //                unique key values.
      //
      // 07/12/22 C:136 Support: 102914
      //                Prevent A/R Batch records with zero
      //                employer # from being written.
      //
      // Created for 150WEU on 05/18/20 in V20R2
      //======================================================
      /Free
       Dcl-F WDEAP Usage(*Output) Rename(pea01:pea0p);
       Dcl-F WXARIMPERR Usage(*Input:*Output) Prefix(e_);
       Dcl-F WXARIMPO Printer OFlInd(InOf) UsrOpn;
       Dcl-F WXARIMPF Usage(*Input);
       // *Entry
       Dcl-Pr ARImport ExtPgm('WXARIMP');
        inFolder Char(256);
        inFileName Char(256);
        outReturn Char(10);
       End-Pr;
       Dcl-Pi ARImport;
        inFolder Char(256);
        InFileName Char(256);
        outReturn Char(10);
       End-Pi;
       Dcl-S aryDupeIDs Char(32) Dim(99999);
       Dcl-S aryDataElements VarChar(40) Dim(20);
       Dcl-S aryTransIDs Char(32) Dim(99999);
       Dcl-S chrRecType Char(1);
       Dcl-S hasFileError Ind;
       Dcl-S hasRecError Ind;
       Dcl-S hasUploadedAr Ind;
       Dcl-S indRecError Ind;
       Dcl-S intBatch# Like(EAbat#);
       Dcl-S intCharPos Zoned(3:0);
       Dcl-S intCo# Zoned(3:0);
       Dcl-S int#ofHRecs Zoned(3:0);
       Dcl-S int#ofTRecs Zoned(3:0);
       Dcl-S intRecCount Zoned(9:0);
       Dcl-S intT#OfRecs Zoned(9:0);
       Dcl-S intTrailerCount Zoned(9:0);
       Dcl-S isCalledFromAutoJob Ind;
       Dcl-S strDataRec Char(300);
       Dcl-S strBatchSysCode Char(10);
       Dcl-S svE_Data Like(e_Data);
       //***  Beg Add  **** 07/12/23 *******************************
       Dcl-S intTime Like(EAtime);
       //***  End Add  **** 07/12/23 *******************************
       Dcl-Ds dtl Qualified Inz;
        transID Char(32);
        group Zoned(3:0);
        empLinkCode Char(15);
        rateState Char(2);
        carrPolicy# Char(25);
        employer Zoned(9:0);
        division Zoned(5:0);
        fyr Zoned(3:0);
        postAmt Zoned(9:2);
        postDate Zoned(7:0);
        arCode Char(2);
        arCodeDesc Char(30);
        check# Zoned(7:0);
        checkAmt Zoned(9:2);
        arCategory Char(1);
        arType Char(1);
        invoice# Zoned(10:0);
        reportingDiv Zoned(5:0);
       End-Ds;
       Dcl-Ds SZQ1P ExtName('SZQ1P') Psds;
       End-Ds;
       //***  Beg Add  **** 11/08/23 *******************************
       Dcl-Ds DSDataRec Likerec(ParImp:*Input) Inz;
       //***  BEndAdd  **** 11/08/23 *******************************
       Dcl-Pr StringTokenizer Pointer ExtProc('strtok');
        string Pointer Value Options(*String);
        delim Pointer Value Options(*String);
       End-Pr;
       Dcl-Pr EmailDocuments ExtPgm('WXARIMPC1');
        emailFrom Char(60);
        emailTo   Char(60);
        emailSubject Char(200);
        emailBody Char(500);
        reportDoc Char(256);
        exceptionDoc Char(256);
       End-Pr;
      /Copy SprSql
       //========================================
       // Main
       //========================================
       Exec Sql Set Option Commit = *None;
       If outReturn = 'COPYFAILED';
          EmailReport();
          *Inlr = *On;
          Return;
       Endif;
       PgmStart();
       EditFileStructure();
       ProcessDetail();
       CreateErrorFile();
       EmailReport();
       *Inlr = *On;
       Return;
       //========================================
       Dcl-Proc CreateErrorFile;
        Dcl-S strRptFolder Char(256);
        Open WXARIMPO;
        O_Title = 'A/R Import Report';
        O_InPath = %Trim(inFolder) + inFileName;
        intCharPos = %Scan('.':InFileName);
        intCharPos -= 1;
       // AUTOJOB - the input folder is changed to the
       //  output folder by WXARIFS
       // ONDEMAND, output goes back to the same folder
       //  the text file was dropped into.
        strRptFolder = inFolder;
        O_OutPath = %Trim(strRptFolder) +
         %Subst(InFileName:1:intCharPos) +
         '_Upload_Report.PDF';
        If hasFileError = *On;
           outReturn = 'FILE ERROR';
           Write RptHeader;
           Write FileHdr;
           Setll 1 WXARIMPERR;
           Dou %Eof(WXARIMPERR);
              Read parImpErr;
              If %Eof;
                 Leave;
              Endif;
              If e_Data <> *Blank;
                 Iter;
              Endif;
              O_FileErr = e_ErrMsg;
              Write FileMsg;
              If *InOf = *On;
                 *InOf = *Off;
                 Write RptHeader;
                 Write FileHdr;
              Endif;
           Enddo;
        Endif;
       // Write WXARIMPO to spool file. It will be converted
       //  to PDF by the EmailReport procedure.
        If hasRecError = *On;
           Write RptHeader;
           Setll 1 WXARIMPERR;
           Dou %Eof(WXARIMPERR);
              Read parImpErr;
              If %Eof;
                 Leave;
              Endif;
              If e_Data = *Blank;
                 Iter;
              Endif;
              If e_Data <> O_transDta;
                 O_TransID = ParseDataRecord(e_Data:2);
                 O_transDta = e_Data;
                 Write TransHdr;
              EndIf;
              If *InOf = *On;
                 *InOf = *Off;
                 Write RptHeader;
                 Write TransHdr;
              Endif;
              O_TransErr = e_ErrMsg;
              Write TransMsg;
              If *InOf = *On;
                 *InOf = *Off;
                 Write RptHeader;
                 Write TransHdr;
              Endif;
           Enddo;
        Endif;
       // Return batch number for ONDEMAND pop up window.
        If hasUploadedAr;
           %Subst(outReturn:1:9) = %Editc(EAbat#:'X');
           Select;
           When %Subst(outReturn:10:1) = 'E';
              O_FileErr = 'Successful upload with errors.';
           Other;
              O_FileErr = 'Successful upload with no errors.';
           EndSl;
           Write RptHeader;
           Write FileHdr;
           Write FileMsg;
           Write UploadTot;
        Endif;
        Close WXARIMPO;
       End-Proc;
       //========================================
       Dcl-Proc EditDetail;
       //========================================
        Dcl-Pi *N Ind;
        End-Pi;
        Dcl-S ctr Zoned(3:0);
        Dcl-S flgAudit Char(1);
        Dcl-S flgSelfRpting Char(1);
        Dcl-S flgSummary Char(1);
        Dcl-S indErr Ind;
        Dcl-S intCount Zoned(3:0);
        Dcl-S intCymd Zoned(7:0);
        Dcl-S intIsoDate Zoned(8:0);
        Dcl-S indPolicyErr Ind;
        Dcl-S intStRptDiv Zoned(5:0);
        If hasFileError = *On;
            Return *Off;
        EndIf;
        If %Subst(strDataRec:1:1) <> 'D';
           WriteError('Record format is incorrect.');
           Return *On;
        Endif;
        For ctr = 2 to %Elem(aryDataElements);
           Clear intCount;
           Select;
           When ctr = 2;
              dtl.transID = %Trim(aryDataElements(ctr));
              If %LookUp(dtl.transId:aryDupeIds) <> 0;
                 indErr = *On;
                 WriteError('The import file has a duplicate transaction ' +
                  'ID for "' + dtl.transID + '".');
              Endif;
              Exec SQL
               Select Count(*)
               Into :intCount
               From WDEAP
               Where EAtransID = :dtl.transID;
              If intCount <> 0;
                 indErr = *On;
                 WriteError('A transaction ID already exists in WDEAP for "' +
                  %Trim(dtl.transID) + '".');
              Endif;
           When ctr = 3;
              Monitor;
                 dtl.group = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 indPolicyErr = *On;
                 WriteError('Group must be numeric.');
              EndMon;
           When ctr = 4;
              dtl.empLinkCode = aryDataElements(ctr);
           When ctr = 5;
              dtl.rateState = aryDataElements(ctr);
           When ctr = 6;
              dtl.carrPolicy# = aryDataElements(ctr);
           When ctr = 7;
              If aryDataElements(ctr) = *Blanks;
                 aryDataElements(ctr) = *Zeros;
              Endif;
              Monitor;
                 dtl.employer = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 indPolicyErr = *On;
                 WriteError('Employer must be numeric.');
              EndMon;
           When ctr = 8;
              If aryDataElements(ctr) = *Blanks;
                 aryDataElements(ctr) = *Zeros;
              Endif;
              Monitor;
                 dtl.division = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 indPolicyErr = *On;
                 WriteError('Division must be numeric.');
              EndMon;
           When ctr = 9;
              Monitor;
                 dtl.fyr = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 indPolicyErr = *On;
                 WriteError('Policy year must be numeric.');
              EndMon;
           When ctr = 10;
              Monitor;
                 dtl.postAmt = %Dec(aryDataElements(ctr):9:2);
              On-Error;
                 indErr = *On;
                 WriteError('Post amount must be numeric.');
              EndMon;
           When ctr = 11;
              Monitor;
                 intIsoDate = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 WriteError('Post date must be numeric.');
              EndMon;
              Monitor;
                 dtl.postDate =
                  %Dec(%Char(%Date(intIsoDate:*iso):*cymd0):7:0);
              On-Error;
                 indErr = *On;
                 WriteError('Post date is invalid date. ' +
                 'The YYYYMMDD format is expected.');
              EndMon;
           When ctr = 12;
              Clear intCount;
              dtl.arCode = aryDataElements(ctr);
              Exec Sql
               Select Count(*)
               Into :intCount
               From WTTTP
               Where TTstat <> 'I'
                And TTkey = :dtl.arCode;
              If intCount <> 1;
                 indErr = *On;
                 WriteError('The A/R code is invalid.');
              Else;
                 Exec Sql
                  Select TTctyp, TTdesc, TTtype
                  Into :dtl.arCategory, :dtl.arCodeDesc, :dtl.arType
                  From WTTTP
                  Where TTkey = :dtl.arCode;
              EndIf;
           When ctr = 13;
              If aryDataElements(ctr) <> *Blanks;
                 dtl.arCodeDesc = aryDataElements(ctr);
              Endif;
           When ctr = 14;
              If aryDataElements(ctr) = *Blanks;
                 aryDataElements(ctr) = *Zeros;
              Endif;
              Monitor;
                 dtl.check# = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 WriteError('Check number must be numeric.');
              EndMon;
           When ctr = 15;
              If aryDataElements(ctr) = *Blanks;
                 aryDataElements(ctr) = *Zeros;
              Endif;
              Monitor;
                 dtl.checkAmt = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 WriteError('Check amount must be numeric.');
              EndMon;
           When ctr = 16;
              If aryDataElements(ctr) = *Blanks;
                 aryDataElements(ctr) = *Zeros;
              Endif;
              Monitor;
                 dtl.invoice# = %Int(aryDataElements(ctr));
              On-Error;
                 indErr = *On;
                 WriteError('Invoice number must be numeric.');
              EndMon;
           EndSl;
        EndFor;
        Select;
        When indPolicyErr = *On;
       //*** beg add ***** 07/12/22 *************
        When dtl.employer = 0
         And dtl.carrPolicy# = *Blanks
         And dtl.empLinkCode = *Blanks;
           indErr = *On;
           WriteError('An employer #, carrier policy #, or ' +
            'link code must be populated.');
       //*** end add ***** 07/12/22 *************
        When dtl.employer <> 0;
           Clear intCount;
           Exec Sql
            Select Count(*)
            Into :intCount
            From WDELP
            Where ELfnd = :dtl.group
             And ELemp# = :dtl.employer
             And ELdiv = :dtl.division
             And ELfyr = :dtl.fyr;
           Select;
           When intCount <> 1;
              indErr = *On;
              WriteError('A unique policy division year doesn''t exist ' +
               'for group '                    +
               %Trim(%Editc(dtl.group:'3'))    + ' / ' +
               'employer '                     +
               %Trim(%Editc(dtl.employer:'3')) + ' / ' +
               'division '                     +
               %Trim(%Editc(dtl.division:'3')) + ' / ' +
               'policy year '                  +
               %Trim(%Editc(dtl.fyr:'3'))      + '.');
           When dtl.arCategory = 'R';
           Other;
              Exsr EditPolicyForAR;
           Endsl;
        When dtl.carrPolicy# <> *Blanks;
           Clear intCount;
           Exec Sql
            Select Count(*)
            Into :intCount
            From WMRPP
            Where RPfnd = :dtl.group
             And RPcap# = :dtl.carrPolicy#
             And RPdiv = :dtl.division
             And RPfyr = :dtl.fyr;
           Select;
           When intCount <> 1;
              indErr = *On;
              WriteError('A unique policy division year doesn''t exist ' +
               'for group '                    +
               %Trim(%Editc(dtl.group:'3'))    + ' / ' +
               'carrier policy '               +
                %Trim(dtl.carrPolicy#)         + ' / ' +
               'division '                     +
               %Trim(%Editc(dtl.division:'3')) + ' / ' +
               'policy year '                  +
               %Trim(%Editc(dtl.fyr:'3'))      + '.');
           When dtl.arCategory = 'R';
           Other;
              Exec Sql
               Select RPemp#
               Into :dtl.employer
               From WMRPP
               Where RPfnd = :dtl.group
                And RPcap# = :dtl.carrPolicy#
                And RPdiv = :dtl.division
                And RPfyr = :dtl.fyr;
              Exsr EditPolicyForAR;
           Endsl;
        When dtl.empLinkCode <> *Blanks;
           Clear intCount;
           Exec Sql
            Select Count(*)
            Into :intCount
            From WMEMP
            Join WDELP
             On EMfnd = ELfnd
             And EMemp# = ELemp#
             And EMdiv = ELdiv
            Join WMRSP
             On ELrtcd = RSrtcd
            Where EMfnd = :dtl.group
             And EMlink = :dtl.empLinkCode
             And RSjrst = :dtl.rateState
             And ELfyr  = :dtl.fyr;
           Select;
           When intCount <> 1;
              indErr = *On;
              WriteError('A unique policy division year doesn''t exist ' +
               'for group '                 +
               %Trim(%Editc(dtl.group:'3')) + ' / ' +
               'employer link code '        +
               %Trim(dtl.empLinkCode)       + ' / ' +
               'rate state '                +
               dtl.rateState                + ' / ' +
               'policy year '               +
               %Trim(%Editc(dtl.fyr:'3'))   + '.');
           When dtl.arCategory = 'R';
           Other;
              Exec Sql
               Select EMemp#, EMdiv
               Into :dtl.employer, :dtl.division
               From WMEMP Join WDELP
                On EMfnd = ELfnd
                And EMemp# = ELemp#
                And EMdiv = ELdiv
               Join WMRSP
                On ELrtcd = RSrtcd
               Where EMfnd = :dtl.group
                And EMlink = :dtl.empLinkCode
                And RSjrst = :dtl.rateState
                And ELfyr = :dtl.fyr;
              Exsr EditPolicyForAR;
           Endsl;
        EndSl;
        Return indErr;
        //----------------------------------------
        Begsr EditPolicyForAR;
        //----------------------------------------
         Exec Sql
          Select ELarsm, ELfaf, BSsefl
          Into :flgSummary, :flgAudit, :flgSelfRpting
          From WDELP Join WTBSP
           On ELco# = BSco# And ELbscd = BSkey
          Where ELfnd = :dtl.group
           And ELemp# = :dtl.employer
           And ELdiv = :dtl.division
           And ELfyr = :dtl.fyr;
         Select;
         When Not IsValidFetch(sqlStt);
            indErr = *On;
            WriteError('Policy '             +
             %Trim(%Editc(dtl.group:'3'))    + '/' +
             %Trim(%Editc(dtl.employer:'3')) + '/' +
             %Trim(%Editc(dtl.division:'3')) + '/' +
             %Trim(%Editc(dtl.fyr:'3'))      +
             ' doesn''t have policy detail.');
         When dtl.arCategory = 'C'  //Claims Replinishment
          And dtl.arCategory = 'X'  //Cancellation Fees
          And dtl.arCategory = 'F'  //General Fees
          And dtl.arCategory = 'O'  //Other
          And dtl.arCategory = 'R'; //Deductibles
         When flgAudit = 'A' And
          (dtl.arType = 'A' Or
           dtl.arType = 'B' Or
           dtl.arType = 'D' Or
           dtl.artype = 'F');
            indErr = *On;
            WriteError('This transaction cannot be posted to' +
             ' an audited policy.');
            LeaveSr;
         When flgSelfRpting <> 'S' And
          (dtl.arType = 'A' Or
           dtl.arType = 'B' Or
           dtl.arType = 'D' Or
           dtl.artype = 'F');
            indErr = *On;
            WriteError('This transaction cannot be posted to' +
             ' a policy that is not self-reporting.');
            LeaveSr;
         When flgSummary = *Blank;
            dtl.reportingDiv = dtl.division;
            LeaveSr;
         EndSl;
         Exec Sql
          Select EMdiv
          Into :intStRptDiv
          From WMEMP
          Where EMfnd = :dtl.group
           And EMemp# = :dtl.employer
           And EMstrp = 'Y';
         Select;
         When Not IsValidFetch(sqlStt);
            indErr = *On;
            WriteError('Policy '             +
             %Trim(%Editc(dtl.group:'3'))    + '/' +
             %Trim(%Editc(dtl.employer:'3')) + '/' +
             %Trim(%Editc(dtl.division:'3')) + '/' +
             %Trim(%Editc(dtl.fyr:'3'))      +
             ' doesn''t have a state reporting division' +
             ' and is flagged for summarized A/R.');
         When dtl.arCategory = 'R';  // Deductibles aren't summarized.
            dtl.reportingDiv = dtl.division;
         When dtl.arCategory <> 'R';
            dtl.reportingDiv = dtl.division;
            dtl.division = intStRptDiv;
         Endsl;
        Endsr;
       End-Proc;
       //----------------------------------------
       Dcl-Proc EditFileStructure;
       //----------------------------------------
        If hasFileError = *On;
           Return;
        EndIf;
        Exec Sql
         Select Count(*)
          Into :intRecCount
          From wxarimpf;
        If intRecCount = 0;
           hasFileError = *On;
           WriteError('File is empty.');
           Return;
        EndIf;
        Exec Sql
         Select Count(*)
          Into :int#ofHRecs
          From wxarimpf
          Where Substr(DATA,1,1) = 'H';
        Select;
        When int#ofHRecs = 0;
           hasFileError = *On;
           WriteError('No header record in file.');
           Return;
        When int#ofHRecs <> 1;
           hasFileError = *On;
           WriteError('More than one header record in file.');
           Return;
        Endsl;
        Exec Sql
         Select Substr(DATA,1,1)
         Into :chrRecType
         From wxarimpf
         Where RRN(wxarimpf) =
          (Select Min(RRN(wxarimpf)) From wxarimpf);
        If chrRecType <> 'H';
           hasFileError = *On;
           WriteError('Header record must be the first record in the file.');
           Return;
        Endif;
        Exec Sql
         Select Count(*)
          Into :int#ofTRecs
          From wxarimpf
          Where Substr(DATA,1,1) = 'T';
        Select;
        When int#ofTRecs = 0;
           hasFileError = *On;
           WriteError('No header record in file.');
           Return;
        When int#ofTRecs <> 1;
           hasFileError = *On;
           WriteError('More than one trailer record in file.');
           Return;
        Endsl;
        Exec Sql
         Select Substr(DATA,1,1)
         Into :chrRecType
         From wxarimpf
         Where RRN(wxarimpf) =
          (Select Max(RRN(wxarimpf)) From wxarimpf);
        If chrRecType <> 'T';
           hasFileError = *On;
           WriteError('Trailer record must be the last record in the file.');
           Return;
        Endif;
        Exec Sql
         Select *
          Into :strDataRec
          From wxarimpf
          Where Substr(DATA,1,1) = 'T';
        Monitor;
           intTrailerCount = %Int(ParseDataRecord(strDataRec:2));
        On-Error;
           hasFileError = *On;
           WriteError('Trailer record count not numeric.');
           Return;
        EndMon;
        If intTrailerCount <> intRecCount;
           hasFileError = *On;
           WriteError('Trailer record does not match actual record count.');
           Return;
        Endif;
        FindDupeTransIDs();
       End-Proc;
       //========================================
       Dcl-Proc EmailReport;
       //========================================
        Dcl-S strEmailBody Char(500);
        Dcl-S strEmailFrom Char(60) Inz('*NONE');
        Dcl-S strEmailSubj Char(200);
        Dcl-S strEmailTo Char(60);
        Dcl-S strExceptDoc Char(256);
        Dcl-S strReportDoc Char(256);
       // Get email to address
        Exec Sql
         Select ACarImpEml
         Into :strEmailTo
         From SDACP
         Where ACapId = 'AR_Import_';
        If strEmailTo = *Blanks;
           Return;
        EndIf;
       // Email user that copy failed in WXARIMPC
        If outReturn = 'COPYFAILED';
           strEmailSubj = 'A/R Import Failed';
           strEmailBody = %Trim(O_inPath) + ' could not be copied ' +
            'into the CompSuite database. Possible reasons are: (1) lack of ' +
            'user authority (2) files are locked by another import job ' +
            '(3) the import file is empty or incorrectly formatted.';
           EmailDocuments(strEmailFrom:strEmailTo:strEmailSubj:
            strEmailBody:strReportDoc:strExceptDoc);
           Return;
        Endif;
       // Email user upload results
        Select;
        When hasFileError = *On;
           strEmailSubj = 'A/R Import Failed';
           strEmailBody = %Trim(O_inPath) + ' can not be processed. ' +
            'See attached document for further detail.';
        When hasRecError = *On And hasUploadedAr = *Off;
           strEmailSubj = 'A/R Import Failed';
           strEmailBody = %Trim(O_inPath) + ' can not be processed. ' +
            'See attached document for further detail.';
        When hasRecError = *On And hasUploadedAr = *On;
           strEmailSubj = 'A/R Import Uploaded With Errors';
           strEmailBody = 'Some of the records in ' +
            %Trim(O_inPath) + ' were successfully processed. ' +
            'See attached documents for further detail.';
        When hasUploadedAr = *On;
           strEmailSubj = 'A/R Import Uploaded Without Error';
           strEmailBody = 'All of the records in ' +
            %Trim(O_inPath) + ' were successfully processed. ' +
            'See attached documents for further detail.';
        EndSl;
        strReportDoc = O_OutPath;
        If hasRecError = *On;
           intCharPos = %Scan('_Upload_Report.PDF':O_OutPath);
           intCharPos -= 1;
           strExceptDoc = %Subst(O_OutPath:1:intCharPos) +
            '_Exceptions.TXT';
        Endif;
        EmailDocuments(strEmailFrom:strEmailTo:strEmailSubj:
         strEmailBody:strReportDoc:strExceptDoc);
       End-Proc;
       //========================================
       Dcl-Proc FindDupeTransIDs;
       //========================================
        Dcl-S ix Zoned(5:0) Inz(1);
        Dcl-S iy Zoned(5:0) Inz(1);
        Dcl-S strTransId Like(dtl.transID);
        Exec Sql
         Declare e2 Cursor For
         Select *
         From WXARIMPF
         Where Substr(Data,1,1) = 'D';
        Exec Sql
         Open e2;
        Dou Not IsValidFetch(SqlStt);
           Exec Sql
            Fetch Next
            From e2
            Into :strDataRec;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           strTransId = %Trim(ParseDataRecord(strDataRec:2));
           If %LookUp(strTransId:aryTransIds) = 0;
              aryTransIds(ix) = strTransId;
              ix += 1;
           Else;
              aryDupeIds(iy) = strTransId;
              iy += 1;
           Endif;
        Enddo;
        Exec Sql
         Close e2;
       End-Proc;
       //========================================
       Dcl-Proc GetBatch#;
       //========================================
        Dcl-Pi *N Zoned(9:0);
        End-Pi;
        Select;
        When intBatch# = 0;
           Exec Sql
            Select Max(EAbat#)
            Into :intBatch#
            From WDEAP;
           intBatch# += 1;
        EndSl;
        Return intBatch#;
       End-Proc;
       //========================================
       Dcl-Proc ParseDataRecord;
       //========================================
        Dcl-Pi *N VarChar(40);
         strData Char(300);
         intIdx Zoned(3:0) Const;
        End-Pi;
        Dcl-S counter Zoned(3:0);
        Dcl-S ptrToDataElement Pointer;
        Clear aryDataElements;
        strData = %ScanRpl('||':'| |':strData);
        ptrToDataElement = StringTokenizer(strData:'|');
        Dow ptrToDataElement <> *Null;
           counter += 1;
           aryDataElements(counter) = %Trim(%Str(ptrToDataElement));
           ptrToDataElement = StringTokenizer(*Null:'|');
        Enddo;
        If intIdx <> 0;
           Return aryDataElements(intIdx);
        Else;
           Return *Blank;
        EndIf;
       End-Proc;
       //----------------------------------------
       Dcl-Proc PgmStart;
       //----------------------------------------
        Exec Sql
         Select SYco#, SYkey
         Into :intCo#, :strBatchSysCode
         From WTSYP
         Join SMCOP
          On SYco# = COco#
         Where SYstat <> 'I'
          And SYtype = 'Y'
         Order by SYkey
         Limit 1;
        If intCo# = 0 Or strBatchSysCode = *Blank;
           hasFileError = *On;
           WriteError('Company number or batch system code ' +
            'could not be located. Contact vendor.');
        Endif;
        // Get out folder when called from PDSFAX subsystem
        If outReturn = 'AUTOJOB';
           isCalledFromAutoJob = *On;
        Endif;
       End-Proc;
       //----------------------------------------
       Dcl-Proc ProcessDetail;
       //----------------------------------------
        If hasFileError = *On;
           Return;
        EndIf;
        //***  Beg Del  **** 11/08/23 *******************************
        //Exec Sql
        //  Declare c1 Cursor For
        //  Select *
        //  From wxarimpf;
        // Exec Sql
        //  Open c1;
        //***  Beg Add  **** 07/12/23 *******************************
        //*** intTime = %Dec(%Time():*hms);
        //***  End Add  **** 07/12/23 *******************************
        // Dou Not IsValidFetch(sqlStt);
        //    Exec Sql
        //     Fetch Next
        //     From c1
        //     Into :strDataRec;
        //  Select;
        //    When sqlStt = '02000';
        //       Leave;
        //    When Not IsValidFetch(sqlStt);
        //       hasFileError = *On;
        //       WriteError('SQL in ProcessDetail failed. Contact vendor.');
        //       Leave;
        //***  End Del  **** 11/08/23 *******************************
          //***  Beg Add  **** 11/08/23 *******************************
          intTime = %Dec(%Time():*hms);
          Setll 1 WXARIMPF;
          Dou %Eof(WXARIMPF);
           Read parImp DSDataRec;
           strDataRec = DSDataRec.Data;
           Select;
           When %Eof;
              Leave;
          //***  End Add  **** 11/08/23 *******************************
           When strDataRec = *Blanks;
              Iter;
           When %Subst(strDataRec:1:1) = 'H';
              Iter;
           When %Subst(strDataRec:1:1) = 'T';
              Iter;
           Endsl;
           ParseDataRecord(strDataRec:0);
           indRecError = EditDetail();
           If indRecError = *Off;
              WriteWdeap();
           Else;
              hasRecError = *On;
           Endif;
        Enddo;
        //***  Beg Del  **** 11/08/23 *******************************
        // Exec Sql
        //  Close c1;
        //***  End Del  **** 11/08/23 *******************************
        If intBatch# <> 0;
           Exec Sql
            Update WDEAP
            Set EAbata =
             (Select Sum(EAamt)
               From WDEAP
               Where EAbat# = :intBatch#)
            Where EAbat# = :intBatch#;
        EndIf;
        End-Proc;
        //========================================
        Dcl-Proc WriteError;
        //========================================
        Dcl-Pi *N;
         inMessage Char(150) Const;
        End-Pi;
        e_Data = strDataRec;
        e_ErrMsg = inMessage;
        Write pArImpErr;
        End-Proc;
        //========================================
        Dcl-Proc WriteWdeap;
        //========================================
        If hasFileError;
           Return;
        Endif;
        EAco#  = intCo#;
        EAtransID = dtl.transID;
        EAsycd = strBatchSysCode;
        EAbat# = GetBatch#;
        EAfnd  = dtl.group;
        EAemp# = dtl.employer;
        EAdiv  = dtl.division;
        EAfyr  = dtl.fyr;
        EAamt  = dtl.postAmt;
        EApodt = dtl.postDate;
        EAarcd = dtl.arCode;
        EAdesc = dtl.arCodeDesc;
        EActyp = dtl.arCategory;
        EAchk# = dtl.check#;
        EAckam = dtl.checkAmt;
        EAinv# = dtl.invoice#;
        EArdiv = dtl.reportingDiv;
        EAtfap = 'N';
        EAtffl = 'N';
        EAidt  = %Dec(%Date():*cymd);
        EAdt   = EAidt;
        //***  Beg Chg  **** 07/12/23 *******************************
        //***  EAtime = %Dec(%Time():*hms);
        EAtime = intTime;
        intTime = %Dec(%Time(intTime:*Hms) + %Seconds(1):*Hms);
        //***  End Chg  **** 07/12/23 *******************************
        EAiusr = Q1user;
        EAuser = Q1user;
        EAipgm = Q1pgm;
        EApgm  = Q1pgm;
        Write pea0p;
        FEOD WDEAP;
        O_RecCount += 1;
        O_TotAmt += dtl.postAmt;
        hasUploadedAr = *On;
       End-Proc;
      /End-Free
