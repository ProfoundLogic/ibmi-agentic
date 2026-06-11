    D*========================================================================
    d* WPREPAY - ProtoTypes
     D*=====================================================================
    D* Created on 08/18/2020 by c:62 for Project WC2749
    D*========================================================================
    D* Revisions:
     D*
    D* 04/19/21 C:079 Support: 90/95537
    D*                Add B2CKTP to GetEobPay, GetPayerID, GetWcEPay
    D*
    D*========================================================================
    D*-------------------------------------------------------------------  ---
    D* ClrEPayFile - Clear E-Pay File
    D*-------------------------------------------------------------------  ---
     D ClrEPayFile     PR              n
     D inFile                        10    Const
     D outError                       7
    D*-------------------------------------------------------------------  ---
    D* CrtEPayDocument - Create the E-Pay Document
    D*-------------------------------------------------------------------  ---
     D CrtEPayDocument...
     D                 PR              n
     D inFile                        10    Const
     D inPath                       256    Const
     D outError                       7    Const
    D*-------------------------------------------------------------------  ---
    D* CrtVPayClaimRecord - Create the V-Pay Claim Record
    D*-------------------------------------------------------------------  ---
     D CrtVPayClaimRecord...
     D                 PR
     D inPaymentID                  100    Const
     D inClaim#                      20    Const
     D inMemberName                  50    Const
     D inPoliyNumber                100    Const
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
    D*-------------------------------------------------------------------  ---
    D* CrtVPayHeaderRecord - Create the V-Pay Header Record
    D*-------------------------------------------------------------------  ---
     D CrtVPayHeaderRecord...
     D                 PR
    D*-------------------------------------------------------------------  ---
    D* CrtVPayPayeeRecord - Create the V-Pay Payee Record
    D*-------------------------------------------------------------------  ---
     D CrtVPayPayeeRecord...
     D                 PR
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
    D*-------------------------------------------------------------------  ---
    D* CrtVPayPaymentRecord - Create the V-Pay Payment Record
    D*-------------------------------------------------------------------  ---
     D CrtVPayPaymentRecord...
     D                 PR
     D inPaymentID                  100    Const
     D inClientRefID                 16    Const
     D inPayerID                     50    Const
     D inPayerTime                   26    Const
     D inPaymentAmt                  18    Const
     D inPaymentCurr                  3    Const
     D inRemitMethod                 10    Const
     D inJurState                     3    Const
    D*-------------------------------------------------------------------  ---
    D* CrtVPayTrailerRecord - Create the V-Pay Trailer Record
    D*-------------------------------------------------------------------  ---
     D CrtVPayTrailerRecord...
     D                 PR
     D inRecordType                   5    Const
      //----------------------------------------------------------------------
      // GetEPayFormat - Get the Vendor Format
      //----------------------------------------------------------------------
     D GetEpayFormat   PR             4A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const
      //----------------------------------------------------------------------
      // GetEPayInFolderForFiles - Get the EPay Inbound folder for files
      //----------------------------------------------------------------------
     D GetEPayInFolderForFiles...
     D                 PR            70A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const
      //----------------------------------------------------------------------
      // GetEPayOrigin - Get the Origin
      //----------------------------------------------------------------------
     D GetEpayOrigin   PR            50A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const
      //----------------------------------------------------------------------
      // GetEPayOutFolderForDocuments - Get the EPay Outbound folder for documents
      //----------------------------------------------------------------------
     D GetEPayOutFolderForDocuments...
     D                 PR            70A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const
      //----------------------------------------------------------------------
      // GetEPayOutFolderForFiles - Get the EPay Outbound folder for files
      //----------------------------------------------------------------------
     D GetEPayOutFolderForFiles...
     D                 PR            70A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const
      //----------------------------------------------------------------------
      // GetEPayTPACode - Get the TPA Code
      //----------------------------------------------------------------------
     D GetEPayTPACode  PR            10A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const
      //----------------------------------------------------------------------
      // GetEPayVersion - Get the Version
      //----------------------------------------------------------------------
     D GetEpayVersion  PR             4A
     D  inVendor                      5A   Const
     D  inEffDate                     7  0 Const
      //----------------------------------------------------------------------
      // WriteVPayHist - Writes to Vpay History File
      //----------------------------------------------------------------------
     D WriteVPayHist   PR
     D inDocName                     30    Const
      //----------------------------------------------------------------------
      // GetEobEPay - Return E-pay Vendor for EOB
      //----------------------------------------------------------------------
     D GetEobEPay      PR             5a
     D inCo#                          3  0 Const
     D inAppl                         2    Const
     D inFnd                          3  0 Const
     D inAcct                        20    Const
     D inEfDt                         7  0 Const
     D*** Beg Add *** 04/19/21 *********************************************
     D inCkTp                        10    Const
     D*** End Add *** 04/19/21 *********************************************
      //----------------------------------------------------------------------
      // GetPayerID - Return E-pay Payer ID
      //----------------------------------------------------------------------
     D GetPayerID      PR            50a
     D inCo#                          3  0 Const
     D inAppl                         2    Const
     D inFnd                          3  0 Const
     D inAcct                        20    Const
     D inEfDt                         7  0 Const
     D*** Beg Add *** 04/19/21 *********************************************
     D inCkTp                        10    Const
     D*** End Add *** 04/19/21 *********************************************
      //----------------------------------------------------------------------
      // GetWcEPay - Return E-pay Vendor for Work Comp
      //----------------------------------------------------------------------
     D GetWcEPay       PR             5a
     D inCo#                          3  0 Const
     D inAppl                         2    Const
     D inFnd                          3  0 Const
     D inAcct                        20    Const
     D inEfDt                         7  0 Const
     D inPtyp                         1    Const
     D*** Beg Add *** 04/19/21 *********************************************
     D inCkTp                        10    Const
     D*** End Add *** 04/19/21 *********************************************
