     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H nomain
     *========================================================================
     *
     * WPIEPAY - Procedures used for various e-pay processes
     *
      *=====================================================================
     * Created on 08/18/2020 by c:62 for Project WC2749
     *========================================================================
     * Revisions:
     *
     * 04/19/21 C:079 Support: 90/95537
     *                Add B2CKTP to GetEobPay, GetPayerID, GetWcEPay
     *
     *========================================================================
     f*  physical file definition (for maintaining record)
     fwcvpayf   o    e             disk    rename(pvp01:pvp0p) usropn
     fwcvpayout o    e             disk    rename(pvp01:pvp0out)
     fwcvpayhst o    e             disk    rename(pvh01:pvh0p)
     fwmevml2   if   e           k disk    rename(pevm01:pevm02)
     fwdb2l1    if   e           k disk

    d*---------------------------------------------------------------------
    d* ProtoTypes
    d*---------------------------------------------------------------------
     d ExecClCmd       pr                  ExtPgm('QCMDEXC')
     d                              512a   Const
     d                               15p 5 Const
     D/copy sprdate
     D/copy sprCompany
     D/copy wprGroup
     D/copy wprTable
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     D psds          esds                  ExtName(szq1p)

      //----------------------------------------------------------------------
      // ClrEPayFile - Clear E-Pay File
       //
       // Returns indicator if an error occurred
       // InFile - the file that is being cleared
       // OutError - The error message if it fails to clear file
      //----------------------------------------------------------------------
     P ClrEPayFile     B                   export
     D ClrEPayFile     PI              n
     D inFile                        10    Const
     D outError                       7

     D errorFound      s               n   Inz(*Off)
     D command         s            512    Inz(' ')
      /free

       command = 'CLRPFM File(' + %Trim(inFile) + ')';
       CallP(e) ExecClCmd(command:%Len(%Trim(command)));
       If %Error;
          outError   = Q1exTp + Q1exNm;
          errorFound = *On;
       EndIf;

       Return errorFound;

      /end-free

     P ClrEPayFile     E

      //----------------------------------------------------------------------
      // CrtEPayDocument - Create the E-Pay Document
       //
       // Returns indicator if an error occurred
       // InFile - The file that is being used to create document
       // InPath - The path and document name of the resolved document
       // OutError - The error message if it fails to clear file
      //----------------------------------------------------------------------
     P CrtEPayDocument...
     P                 B                   export
     D CrtEPayDocument...
     D                 PI              n
     D inFile                        10    Const
     D inPath                       256    Const
     D outError                       7

     D command         s            512    Inz(' ')
     D errorFound      s               n   Inz(*Off)

      /free

       command = 'CrtCsvF File(' + %Trim(inFile) +
                 ') ToStmf(''' + %Trim(inPath) +  ''') ' +
                 'DfCCSID(*FILE) RplChr(*NONE) FldDlm(''^'')';

       CallP(e) ExecClCmd(command:%len(%Trim(command)));
       If %Error;
          outError   = Q1exTp + Q1exNm;
          errorFound = *On;
       EndIf;

       Return errorFound;

      /end-free
     P CrtEPayDocument...
     P                 E


      //----------------------------------------------------------------------
      // CrtVPayClaimRecord - Create the V-Pay Claim Record
       //
       // inPaymentID - Payment ID to lnk to payment
       // inClaim# - The claim number
       // inMemberName - Member Name
       // inPolicyNumber - Policy Number
       // inAccidentDate - Accident Date
       // inAdjusterName - Adjuster Name
       // inPaymentPrd - Payment Period
       // inWeeksDays - Weeks/Days
       // inPV1Desc - Payment Value 1 Description
       // inRsvDesc - Reserve Type Description
       // inMemo1 - Memo 1
       // inMemo2 - Memo 2
       // inMemo3 - Memo 3
       // inMemo4 - Memo 4
      //----------------------------------------------------------------------
     P CrtVPayClaimRecord...
     P                 B                   export
     D CrtVPayClaimRecord...
     D                 PI
     D inPaymentID                  100    Const
     D inClaim#                      20    Const
     D inMemberName                  50    Const
     D inPolicyNumber               100    Const
     D inAccidentDate                10    Const
     D inAdjusterName                50    Const
     D inPaymentPrd                  25    Const
     D inWeeksDays                    6    Const
     D inPV1Desc                     40    Const
     D inRsvDesc                     40    Const
     D inMemo1                       40    Const
     D inMemo2                       40    Const
     D inMemo3                       40    Const
     D inMemo4                       40    Const

      /free

       Open  wcvpayf;
       Clear pvp0p;

       VPdata = 'VR003'               + '|' +         // Record Type
                %Trim(inPaymentID)    + '|' +         // Payment ID
                %Trim(inClaim#)       + '|' +         // Claim #
                %Trim(inMemberName)   + '|' +         // Member Name
                %Trim(inPolicyNumber) + '|' +         // Policy Number
                %Trim(inAccidentDate) + '|' +         // Accident Date
                %Trim(inAdjusterName) + '|' +         // Adjuster Name
                %Trim(inPaymentPrd)   + '|' +         // Payment Period
                %Trim(inWeeksDays)    + '|' +         // Weeks/Days
                %Trim(inPV1Desc)      + '|' +         // PV1 Description
                %Trim(inRsvDesc)      + '|' +         // Reserve Description
                %Trim(inMemo1)        + '|' +         // Memo 1
                %Trim(inMemo2)        + '|' +         // Memo 2
                %Trim(inMemo3)        + '|' +         // Memo 3
                %Trim(inMemo4);                       // Memo 4

       Write pvp0p;
       feod  wcvpayf;
       Close wcvpayf;

       return;

      /end-free
     P CrtVPayClaimRecord...
     P                 E

      //----------------------------------------------------------------------
      // CrtVPayHeaderRecord - Create the V-Pay Header Record
      //----------------------------------------------------------------------
     P CrtVPayHeaderRecord...
     P                 B                   export
     D CrtVPayHeaderRecord...
     D                 PI

     D origin          s                   Like(EVMorigin)
     D tpaCode         s                   Like(EVMtpaCode)
     D version         s                   Like(EVMvers)

      /free

       origin  = GetEpayOrigin('VPAY':DateTo7(systemDate()));
       tpaCode = GetEPayTPACode('VPAY':DateTo7(systemDate()));
       version = GetEpayVersion('VPAY':DateTo7(systemDate()));

       Clear pvp0out;

       VPdata = 'HR000'                 + '|' +        // Record Type
                %Trim(origin)           + '|' +        // Origin
                %Trim(tpaCode)          + '|' +        // TPA
                '1'                     + '|' +        // Record #
                %Char(%timestamp())     + '|' +        // Date of File TS
                %Trim(version)          + '|' +        // Version
                ' '                     + '|' +        // Description
                ' ';                                   // Note Or Message

       Write pvp0out;
       feod wcvpayout;

       return;

      /end-free
     P CrtVPayHeaderRecord...
     P                 E

      //----------------------------------------------------------------------
      // CrtVPayPayeeRecord - Create the V-Pay Payee Record
       //
       // inPaymentID - Payment ID
       // inPayeeType - What type of Payee: B=Business C=Claimant
       // inPayeeName - Payee Name
       // inPayeeAdr1 - Payee Address 1
       // inPayeeAdr2 - Payee Address 2
       // inPayeeAdr3 - Payee Address 3
       // inPayeeCity - Payee City
       // inPayeeState - Payee State
       // inPayeeCntry - Payee Country
       // inPayeeZip - Payee Zip code
       // inPayeeFein - Payee Fein/SS#
      //----------------------------------------------------------------------
     P CrtVPayPayeeRecord...
     P                 B                   export
     D CrtVPayPayeeRecord...
     D                 PI
     D inPaymentID                  100    Const
     D inPayeeType                    1    Const
     D inPayeeName                  100    Const
     D inPayeeAdr1                   40    Const
     D inPayeeAdr2                   40    Const
     D inPayeeAdr3                   40    Const
     D inPayeeCity                   50    Const
     D inPayeeState                   3    Const
     D inPayeeCntry                   3    Const
     D inPayeeZip                    15    Const
     D inPayeeFein                    9    Const

      /free

       Open  wcvpayf;
       Clear pvp0p;

       VPdata = 'VR002'              + '|' +         // Record Type
                %Trim(inPaymentID)   + '|' +         // Payment ID
                %Trim(inPayeeType)   + '|' +         // Payment Type
                %Trim(inPayeeName)   + '|' +         // Payee Name
                %Trim(inPayeeAdr1)   + '|' +         // Payee Address 1
                %Trim(inPayeeAdr2)   + '|' +         // Payee Address 2
                %Trim(inPayeeAdr3)   + '|' +         // Payee Address 3
                %Trim(inPayeeCity)   + '|' +         // Payee City
                %Trim(inPayeeState)  + '|' +         // Payee State
                %Trim(inPayeeCntry)  + '|' +         // Payee Country
                %Trim(inPayeeZip)    + '|' +         // Payee Zip Code
                %Trim(inPayeeFein);                  // Payee Fein

       Write pvp0p;
       feod  wcvpayf;
       Close wcvpayf;

       return;

      /end-free
     P CrtVPayPayeeRecord...
     P                 E

      //----------------------------------------------------------------------
      // CrtVPayPaymentRecord - Create the V-Pay Payment Record
       //
       // inPaymentID - Payment ID used to link to payment record
       // inClientRefID - Client Reference ID
       // inPayerID - Payer ID
       // inPayerTime - Payer TimeStamp
       // inPaymentAmt - Payment Amount
       // inPaymentCurr - Payment Currency
       // inRemitMethod - Remit Method
       // inJurState - Jurisdicition State
      //----------------------------------------------------------------------
     P CrtVPayPaymentRecord...
     P                 B                   export
     D CrtVPayPaymentRecord...
     D                 PI
     D inPaymentID                  100    Const
     D inClientRefID                 16    Const
     D inPayerID                     50    Const
     D inPayerTime                   26    Const
     D inPaymentAmt                  18    Const
     D inPaymentCurr                  3    Const
     D inRemitMethod                 10    Const
     D inJurState                     3    Const

      /free

       Open  wcvpayf;
       Clear pvp0p;

       VPdata = 'VR001'                + '|' +       // Record Type
                %Trim(inPaymentID)     + '|' +       // Payment ID
                %Trim(inClientRefID)   + '|' +       // Client Reference ID
                %Trim(inPayerID)       + '|' +       // Payer ID
                %Trim(inPayerTime)     + '|' +       // Payer Time Stamp
                %Trim(inPaymentAmt)    + '|' +       // Payment Amount
                %Trim(inPaymentCurr)   + '|' +       // Payment Currency
                %Trim(inRemitMethod)   + '|' +       // Remit Method
                %Trim(inJurState);                   // Jurisdiction Body

       Write pvp0p;
       feod  wcvpayf;
       Close wcvpayf;

       return;

      /end-free
     P CrtVPayPaymentRecord...
     P                 E

      //----------------------------------------------------------------------
      // CrtVPayTrailerRecord - Create the V-Pay Trailer Record
       //
       // inRecordType - Record Type associated with this trailer
      //----------------------------------------------------------------------
     P CrtVPayTrailerRecord...
     P                 B                   export
     D CrtVPayTrailerRecord...
     D                 PI
     D inRecordType                   5    Const

     D recordSeq#      s              9  0 Inz
     D recordCount     s              9  0 Inz
      /free

       Select;
       When inRecordType = 'HR000';
          recordSeq# = 1;
          recordCount = GetHR000Count();
       When inRecordType = 'VR001';
          recordSeq# = 2;
          recordCount = GetVR001Count();
       When inRecordType = 'VR002';
          recordSeq# = 3;
          recordCount = GetVR002Count();
       When inRecordType = 'VR003';
          recordSeq# = 4;
          recordCount = GetVR003Count();
       When inRecordType = 'TR999';
          recordSeq# = 5;
          recordCount = GetTotalCount() + 1;
       EndSl;

       Clear pvp0out;

       VPdata = 'TR999'                   + '|' +
                inRecordType              + '|' +  // Reference Record Type
                %EditC(recordSeq#:'X')    + '|' +  // Trailer Record #
                %EditC(recordCount:'X')   + '|' +  // Reference Record Count
                ' ';                               // MD5 Hash

       Write pvp0out;
       feod wcvpayout;

       Return;

      /end-free
     P CrtVPayTrailerRecord...
     P                 E

      //----------------------------------------------------------------------
      // GetEPayFormat - Get the Vendor Format
       //
       // inVendor - E-Pay Vendor Code
       // inEffDate - E-Pay Effective Date
      //----------------------------------------------------------------------
     P GetEpayFormat   B                   Export
     D GetEpayFormat   PI             4A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const

      /free
       SetGT  (inVendor:inEffDate) pevm02;
       ReadPE (inVendor)           pevm02;
       If Not %Eof(wmevml2);
          Return EVMfmt;
       Else;
          Return ' ';
       Endif;
      /end-free

     P GetEPayFormat   E

      //----------------------------------------------------------------------
      // GetEPayInFolderForFiles - Get the EPay Inbound folder for files
       //
       // inVendor - E-Pay Vendor Code
       // inPaymentType - E-Pay Payment Type
       // inEffDate - E-Pay Effective Date
      //----------------------------------------------------------------------
     p GetEPayInFolderForFiles...
     P                 B                   Export
     D GetEPayInFolderForFiles...
     D                 PI            70A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const

      /free
       SetGT  (inVendor:inEffDate) pevm02;
       ReadPE (inVendor)           pevm02;
       If Not %Eof(wmevml2);
          Return EVMiFldFil;
       Else;
          Return ' ';
       Endif;
      /end-free

     p GetEPayInFolderForFiles...
     P                 E

      //----------------------------------------------------------------------
      // GetEPayOrigin - Get the Origin
       //
       // inVendor - E-Pay Vendor Code
       // inPaymentType - E-Pay Payment Type
       // inEffDate - E-Pay Effective Date
      //----------------------------------------------------------------------
     P GetEpayOrigin   B                   Export
     D GetEpayOrigin   PI            50A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const

      /free
       SetGT  (inVendor:inEffDate) pevm02;
       ReadPE (inVendor)           pevm02;
       If Not %Eof(wmevml2);
          Return EVMorigin;
       Else;
          Return ' ';
       Endif;
      /end-free

     P GetEPayOrigin   E

      //----------------------------------------------------------------------
      // GetEPayOutFolderForDocuments - Get the EPay Outbound folder for documents
       //
       // inVendor - E-Pay Vendor Code
       // inPaymentType - E-Pay Payment Type
       // inEffDate - E-Pay Effective Date
      //----------------------------------------------------------------------
     p GetEPayOutFolderForDocuments...
     P                 B                   Export
     D GetEPayOutFolderForDocuments...
     D                 PI            70A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const

      /free
       SetGT  (inVendor:inEffDate) pevm02;
       ReadPE (inVendor)           pevm02;
       If Not %Eof(wmevml2);
          Return EVMoFldAtt;
       Else;
          Return ' ';
       Endif;
      /end-free

     p GetEPayOutFolderForDocuments...
     P                 E

      //----------------------------------------------------------------------
      // GetEPayOutFolderForFiles - Get the EPay Outbound folder for files
       //
       // inVendor - E-Pay Vendor Code
       // inPaymentType - E-Pay Payment Type
       // inEffDate - E-Pay Effective Date
      //----------------------------------------------------------------------
     p GetEPayOutFolderForFiles...
     P                 B                   Export
     D GetEPayOutFolderForFiles...
     D                 PI            70A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const

      /free
       SetGT  (inVendor:inEffDate) pevm02;
       ReadPE (inVendor)           pevm02;
       If Not %Eof(wmevml2);
          Return EVMoFldFil;
       Else;
          Return ' ';
       Endif;
      /end-free

     p GetEPayOutFolderForFiles...
     P                 E

      //----------------------------------------------------------------------
      // GetEPayTPACode - Get the TPA Code
       //
       // inVendor - E-Pay Vendor Code
       // inPaymentType - E-Pay Payment Type
       // inEffDate - E-Pay Effective Date
      //----------------------------------------------------------------------
     P GetEPayTPACode  B                   Export
     D GetEPayTPACode  PI            10A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const

      /free
       SetGT  (inVendor:inEffDate) pevm02;
       ReadPE (inVendor)           pevm02;
       If Not %Eof(wmevml2);
          Return EVMtpaCode;
       Else;
          Return ' ';
       Endif;
      /end-free

     P GetEPayTPACode  E

      //----------------------------------------------------------------------
      // GetEPayVersion - Get the Version
       //
       // inVendor - E-Pay Vendor Code
       // inPaymentType - E-Pay Payment Type
       // inEffDate - E-Pay Effective Date
      //----------------------------------------------------------------------
     P GetEpayVersion  B                   Export
     D GetEpayVersion  PI             4A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const

      /free
       SetGT  (inVendor:inEffDate) pevm02;
       ReadPE (inVendor)           pevm02;
       If Not %Eof(wmevml2);
          Return EVMvers;
       Else;
          Return ' ';
       Endif;
      /end-free

     P GetEPayVersion  E

      //----------------------------------------------------------------------
      // GetHR000Count - Get the HR000(Header) Count
      //----------------------------------------------------------------------
     P GetHR000Count   B
     D GetHR000Count   PI             9  0

     D count           s              9  0 Inz(0)
      /free

       Exec SQl Select count(*) Into :count
                From WCVPayOut
                Where Substr(VPdata,1,5) = 'HR000';

       Return count;

      /end-free
     P GetHR000Count   E

      //----------------------------------------------------------------------
      // GetVR001Count - Get the VR001(Payment) Count
      //----------------------------------------------------------------------
     P GetVR001Count   B
     D GetVR001Count   PI             9  0

     D count           s              9  0 Inz(0)
      /free

       Exec SQl Select count(*) Into :count
                From WCVPayOut
                Where Substr(VPdata,1,5) = 'VR001';

       Return count;

      /end-free
     P GetVR001Count   E

      //----------------------------------------------------------------------
      // GetVR002Count - Get the HD001(Payee) Count
      //----------------------------------------------------------------------
     P GetVR002Count   B
     D GetVR002Count   PI             9  0

     D count           s              9  0 Inz(0)
      /free

       Exec SQl Select count(*) Into :count
                From WCVPayOut
                Where Substr(VPdata,1,5) = 'VR002';

       Return count;

      /end-free
     P GetVR002Count   E

      //----------------------------------------------------------------------
      // GetVR003Count - Get the VR003(Claim) Count
      //----------------------------------------------------------------------
     P GetVR003Count   B
     D GetVR003Count   PI             9  0

     D count           s              9  0 Inz(0)
      /free

       Exec SQl Select count(*) Into :count
                From WCVPayOut
                Where Substr(VPdata,1,5) = 'VR003';

       Return count;

      /end-free
     P GetVR003Count   E

      //----------------------------------------------------------------------
      // GetTR999Count - Get the TR999(Trailer) Count
      //----------------------------------------------------------------------
     P GetTR999Count   B
     D GetTR999Count   PI             9  0

     D count           s              9  0 Inz(0)
      /free

       Exec SQl Select count(*) Into :count
                From WCVPayOut
                Where Substr(VPdata,1,5) = 'TR999';

       Return count;

      /end-free
     P GetTR999Count   E

      //----------------------------------------------------------------------
      // GetTotalCount - Get the Total Count
      //----------------------------------------------------------------------
     P GetTotalCount   B
     D GetTotalCount   PI             9  0

     D count           s              9  0 Inz(0)
      /free

       Exec SQl Select count(*) Into :count
                From WCVPayOut;

       Return count;

      /end-free
     P GetTotalCount   E

      //----------------------------------------------------------------------
      // WriteVPayHist - Writes to Vpay History File
       //
       // inDocName - Document Name
      //----------------------------------------------------------------------
     P WriteVPayHist   B                   export
     D WriteVPayHist   PI
     D inDocName                     30    Const

      /free

       Clear pvh0p;

       VHdocName  = inDocName;
       VHextDate  = dateTo7(systemDate());
       VHextTime  = timeTo6(systemTime());
       VHhd001Cnt = GetHR000Count();
       VHvr001Cnt = GetVR001Count();
       VHvr002Cnt = GetVR002Count();
       VHvr003Cnt = GetVR003Count();
       VHtr999Cnt = GetTR999Count();
       VHtotalCnt = GetTotalCount();
       VHstat     = 'A';
       VHuser     = Q1user;
       VHpgm      = Q1pgm;
       VHdt       = dateTo7(systemDate());
       VHtime     = timeTo6(systemTime());

       Write pvh0p;

      /end-free
     P WriteVpayHist   E
      //----------------------------------------------------------------------
      // GetEobPay - Return E-Pay Vendor for EOB
      //----------------------------------------------------------------------
     P GetEobEPay      B                   Export
     D GetEobEPay      PI             5a
     D inCo#                          3  0 Const
     D inAppl                         2    Const
     D inFnd                          3  0 Const
     D inAcct                        20    Const
     D inEfDt                         7  0 Const
     D*** Beg Add *** 04/19/21 *********************************************
     D inCkTp                        10    Const
     D*** End Add *** 04/19/21 *********************************************

     D strEPayVend     s              5a   Inz(*blanks)

      /free

       //*** Beg Chg *** 04/19/21 ******************************************
       //***SetGT  (inCo#:inAppl:inFnd:inAcct:inEfDt) pb201;
       //***ReadPE (inCo#:inAppl:inFnd:inAcct) pb201;
       SetGT  (inCo#:inAppl:inFnd:inAcct:inCkTp:inEfDt) pb201;
       ReadPE (inCo#:inAppl:inFnd:inAcct:inCkTp) pb201;
       //*** End Chg *** 04/19/21 ******************************************
       If not %Eof(WDB2L1) And B2ePClmMed = 'Y';
          strEPayVend = B2ePvend;
       EndIf;
       Return strEPayVend;

      /end-free
     P GetEobEPay      E
      //----------------------------------------------------------------------
      // GetPayerID - Return E-Pay Payer ID
      //----------------------------------------------------------------------
     P GetPayerID      B                   Export
     D GetPayerID      PI            50a
     D inCo#                          3  0 Const
     D inAppl                         2    Const
     D inFnd                          3  0 Const
     D inAcct                        20    Const
     D inEfDt                         7  0 Const
     D*** Beg Add *** 04/19/21 *********************************************
     D inCkTp                        10    Const
     D*** End Add *** 04/19/21 *********************************************

     D strPayerID      s             50a   Inz(*blanks)

      /free

       //*** Beg Chg *** 04/19/21 ******************************************
       //***SetGT  (inCo#:inAppl:inFnd:inAcct:inEfDt) pb201;
       //***ReadPE (inCo#:inAppl:inFnd:inAcct) pb201;
       SetGT  (inCo#:inAppl:inFnd:inAcct:inCkTp:inEfDt) pb201;
       ReadPE (inCo#:inAppl:inFnd:inAcct:inCkTp) pb201;
       //*** End Chg *** 04/19/21 ******************************************
       If not %Eof(WDB2L1);
          If B2vpFAcTbl = ' ';
             strPayerID = B2vpFAcFF;
          Else;
             Select;
             When B2vpFacTbl = 'CNUM';
                strPayerID = %EditC(inCo#:'X');
             When B2vpFacTbl = 'CNAM';
                strPayerID = GetCompNam(inCo#);
             When B2vpFacTbl = 'GNUM';
                strPayerID = %EditC(inFnd:'X');
             When B2vpFacTbl = 'GNAM';
                strPayerID = GetGrpName(inFnd);
             Endsl;
          EndIf;
       EndIf;

       Return strPayerID;

      /end-free
     P GetPayerID      E
      //----------------------------------------------------------------------
      // GetWcPay - Return E-Pay Vendor for Work Comp
      //----------------------------------------------------------------------
     P GetWcEPay       B                   Export
     D GetWcEPay       PI             5a
     D inCo#                          3  0 Const
     D inAppl                         2    Const
     D inFnd                          3  0 Const
     D inAcct                        20    Const
     D inEfDt                         7  0 Const
     D inPtyp                         1    Const
     D*** Beg Add *** 04/19/21 *********************************************
     D inCkTp                        10    Const
     D*** End Add *** 04/19/21 *********************************************

     D strEPayVend     s              5a   Inz(*blanks)

      /free

       //*** Beg Chg *** 04/19/21 ******************************************
       //***SetGT  (inCo#:inAppl:inFnd:inAcct:inEfDt) pb201;
       //***ReadPE (inCo#:inAppl:inFnd:inAcct) pb201;
       SetGT  (inCo#:inAppl:inFnd:inAcct:inCkTp:inEfDt) pb201;
       ReadPE (inCo#:inAppl:inFnd:inAcct:inCkTp) pb201;
       //*** End Chg *** 04/19/21 ******************************************
       If not %Eof(WDB2L1);
          Select;
          When inPtyp = 'C' and B2ePClmInd = 'Y';
             strEPayVend = B2ePvend;
          When inPtyp = 'M' and B2ePClmMed = 'Y';
             strEPayVend = B2ePvend;
          When inPtyp = 'E' and B2ePClmExp = 'Y';
             strEPayVend = B2ePvend;
          When inPtyp = 'L' and B2ePClmLeg = 'Y';
             strEPayVend = B2ePvend;
          Endsl;
       EndIf;
       Return strEPayVend;

      /end-free
     P GetWcEPay       E
