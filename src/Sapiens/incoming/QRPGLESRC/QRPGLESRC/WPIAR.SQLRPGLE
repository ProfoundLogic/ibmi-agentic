     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H nomain
      *=========================================================================
      * Revisions:                                                             *
      *
      * 10/04/24 C:62  Project: 117721(133)
      *                Make sure to include future credits when audit adj.
      *
      * 06/12/24 C:62  Project: 146WKQ
      *                Add rotine to retrieve A/R Code based upon type and
      *                category.
      *
      * 05/24/24 C:62  Ticket: 115513(129)
      *                Do not exclude payments if no SR Billing exists.
      *
      * 03/01/24 C:65  Ticket: 115167 (UBIC)
      *                Correct the change done under ticket 115025 below.
      *                The change did not check if invoice # was being passed
      *                into the procedure.  If no invoice # then process the
      *                payments how they were being done before the 115025 chg.
      *
      * 02/12/24 C:62  Ticket: 115025(133)
      *                Use refunds if they predate the invoice.
      *
      * 01/09/24 C:62  Ticket: 114824(129)
      *                If there is a balance then check for credits.
      *
      * 12/21/23 C:62  Ticket: 114750(133)
      *                Don't zero out if policy has been audited
      *
      * 12/19/23 C:62  Ticket: 114695(129)
      *                Remove refunds from IVC balance calculation.
      *
      * 08/29/23 C:62  Ticket: 113679(099)
      *                Address category stuff when a category has null value
      *
      * 08/24/23 C:62  Ticket: 113342(140)
      *                Change voids so they don't get negated.
      *
      * 12/01/22 C:62  Project WC2663
      *                Fix balance by including payments.
      *
      * 10/31/22 C:65  Project WC2663
      *                Fix prior change done on 10/10/22 - there is no transID
      *                field in WMEAP and earef# is alpha so inserts were failing.
      *
      * 10/10/22 C:062 Project WC2663
      *                Add new fields to insert of WMEAP
      *
      * 03/22/21 C:062 Project WC2663
      *                -Add ARBalance Procedure
      *                -Add TransferARToLive
      *          C:136 -Rewrote ARBalance, ARbilled and ARPaid (Not Commented)
      *
      * 11/18/20 C:180 Project WC2757
      *                Recompiled only.
      *
      * 09/28/20 C:136 Project: SY407 DE799                                    *
      *                Add GetLast3rdPartySeq#                                 *
      *                Add GetNext3rdPartySeq#                                 *
      *                                                                        *
      * 07/08/20 C:33  Project: SY407                                          *
      *               -Add ARBilled                                            *
      *               -Add ARPaid                                              *
      *                                                                        *
      * 01/22/19 C:33  Project: SY407                                          *
      *               -Add GetARCode                                           *
      *               -Add AccumulateAR                                        *
      *                                                                        *
      * 10/11/13 C:283 Support: WA133 83719                                    *
      *                Remove support 82740 for estimated billing.             *
      *                                                                        *
      * 08/23/13 C:283 Support: WA133 82740                                    *
      *                For record of 'B' type, should return the Transaction   *
      *                Date.                                                   *
      *                                                                        *
      * 01/28/13 C:282 Project: 133WOC                                         *
      *                Add procedures: ARHistBal                               *
      *                                                                        *
      * 12/06/10 C:61  Project: wc1866                                         *
      *                Add Wdelp to files to avoid signature issues.           *
      *                                                                        *
      * 11/19/09 C:33  Project: 25WQI                                          *
      *                Add procedures:                                         *
      *                 BillType                                               *
      *                 PayType                                                *
      *                                                                        *
      * 05/02/07 C:136 Support: WA081 58066                                    *
      *                Correct ValidCode.  It didn't check for the A/R Code    *
      *                status being inactive.                                  *
      *                                                                        *
      * 02/19/04 C:136 Project: WC1033                                         *
      *                Process new A/R type of 'T' (True Void) when calculating*
      *                A/R balance in ARvalue                                  *
      *=========================================================================

     Fwtabl     if   e           k disk    include(ptt01)      infds(tbfds)
     Fwtctl     if   e           k disk    rename(pct01:pct00) infds(ctfds)
     Fwtbsp     if   e             disk    rename(pbs01:pbs0p)
     Fwdelp     if   e             disk
     Fwdf2l     if   e           k disk    Rename(pf201:pf200)
      *** beg add **** 09/28/20 ***************************
     fwdseqp    uf a e           k disk    usropn rename(pseq01:pseq0p)
      *** end add **** 09/28/20 ***************************

     D tbfds           ds
     D tbrrn                 397    400b 0
     D ctfds           ds
     D ctrrn                 397    400b 0

      *** beg add **** 03/22/21 ***************************
     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a
      *** end add **** 03/22/21 ***************************

     D***  Begin Add  ***  01/22/19  **************************************
     D/copy sPrString
     D***  End   Add  ***  01/22/19  **************************************
     D/copy wprAR
     D/copy wpremplr
     D/copy wprusrprms
     D/copy sprDate
     D/copy sprSql

     D psds          esds                  extname(szq1p)



      //***  Begin Add  ***  01/22/19  *************************
      //========================================================
      // Must be first procedure - only for commit
      //========================================================
     PLeaveAsFirst     b
      /Free
       Exec Sql Set Option Commit = *None;
      /End-free
     PLeaveAsFirst     e


    P*====================================================================
    P* Accumulate all A/R for specific purposes
    P*
    P*  AccumulateAR - Returns the to A/R posted for a particular code
     P*   code      - A/R code for posted A/R   (required)
     P*   from date - A/R posted on or after    (required)
     P*   to date   - A/R posted on or before   (required)
    P*   co#      - accepts the company #  (required)
    P*   group    - accepts the group      (required)
    P*   year     - accepts the year       (optional)
    P*   employer - accepts the employer   (optional)
    P*   division - accepts the division   (optional)
    P*====================================================================
    P*
     PAccumulateAR     b                   export
    P*
     DAccumulateAR     pi            15  2
     D inCode                         2    const
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D*
     DoutAmt           s             15  2 inz
     DstrSql           s           5000    inz varying
     DstrSqlWhere      s           1000    inz varying
      /Free
       strSqlWhere = 'where EAco# = ' + %char(inCo#) +
        ' and EAfnd = ' + %char(inFnd) +
        ' and EApoDt >= ' + %char(inDt7From) +
        ' and EApoDt <= ' + %char(inDt7To) +
        ' and EAarCd = ''' + inCode + '''';
       If %parms >= 6 and inFyr <> 0;
          strSqlWhere += ' and EAfyr = ' + %char(inFyr);
       Endif;
       If %parms >= 7 and inEmp# <> 0;
          strSqlWhere += ' and EAemp# = ' + %char(inEmp#);
       Endif;
       If %parms >= 8;
          strSqlWhere += ' and EAdiv = ' + %char(inDiv);
       Endif;
       strSql = 'select sum(EAamt)' +
        ' from WMEAP ' + strSqlWhere;
       Exec SQL close cursorWMEAP1;
       Exec SQL prepare sqlWMEAP1 from :strSql;
       Exec SQL declare cursorWMEAP1 cursor for sqlWMEAP1;
       Exec SQL open cursorWMEAP1;
       Exec SQL fetch next from cursorWMEAP1 into :outAmt;
       Exec SQL close cursorWMEAP1;
       Return outAmt;
      /End-free
     PAccumulateAR     e
      //***  End   Add  ***  01/22/19  *************************


     P*** Begin Add *** 03/22/21 ******************************************
    P*====================================================================
    P* Return the balance for a policy based upon the posting dates passed
    P*
    P*  ARBalance       - Returns the A/R Balance
     P*   from date      - A/R posted on or after    (required)
     P*   to date        - A/R posted on or before   (required)
    P*   co#            - accepts the company #     (required)
    P*   group          - accepts the group         (required)
    P*   year           - accepts the year          (optional)
    P*   employer       - accepts the employer      (optional)
    P*   division       - accepts the division      (optional)
    P*   mode           - accepts the mode          (optional)
    P*   includeBatch   - not transferred(Y/N)      (optional)
    P*   ExcludeDeposit - exclude deposits (Y/N)    (optional)
    P*====================================================================
     pARBalance        b                   export
     dARBalance        pi            15  2
     d inDt7From                      7  0 const
     d inDt7To                        7  0 const
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inFyr                          3  0 const options(*nopass)
     d inEmp#                         9  0 const options(*nopass)
     d inDiv                          5  0 const options(*nopass)
     d inMode                        25    const options(*nopass)
     d inInvoice#                    10  0 const options(*nopass)
     d inIncludeBatch                 1    const options(*nopass)
     d inExcludeDeposit...
     d                                1    const options(*nopass)

     d balanceAmount   s             15  2 Inz(0)
     d chrIncludeBatch...
     d                 s              1    Inz(' ')
     d chrExcludeDeposit...
     d                 s              1    Inz(' ')
     d chrMode         s             25    Inz(' ')
     d chrRtnInvType   s             25    Inz(' ')
     d decBillAmt      s             15  2
     d decCurInvAmt...
     d                 s             15  2
     d*** Begin Add *** 12/01/22 *******************************************
     d decCurPaid      s             15  2
     d*** End   Add *** 12/01/22 *******************************************
     d decPaidAmt      s             15  2
     d decPrInvBilled...
     d                 s             15  2
     d*** Begin Add *** 01/09/24 *******************************************
     d inDt7ToP1       s              7  0
     d decFutCredit    s             15  2
     d*** End   Add *** 01/09/24 *******************************************
     d HasWDEAP_HLD    s               n   Inz(*On)
     d idx             s              1  0 Inz(0)
     d intCount        s              1  0 Inz(0)
     d intDiv          s              3  0 Inz(0)
     d intEmp#         s              9  0 Inz(0)
     d intInv#         s             10  0 Inz(0)
     d intYear         s              3  0 Inz(0)
     d strLib          s             10a   Inz Varying
     d strSql          s           5000    Inz Varying

       Select;
       When %Parms() > 10;  // Exclude Deposits
          chrExcludeDeposit = inExcludeDeposit;
          chrIncludeBatch = inIncludeBatch;
          intInv# = inInvoice#;
          chrMode = inMode;
          intDiv  = inDiv;
          intEmp# = inEmp#;
          intYear = inFyr;
       When %Parms() > 9;   // Include Batch
          chrIncludeBatch = inIncludeBatch;
          intInv# = inInvoice#;
          chrMode = inMode;
          intDiv  = inDiv;
          intEmp# = inEmp#;
          intYear = inFyr;
       When %Parms() > 8;   // Include Invoice#
          intInv# = inInvoice#;
          chrMode = inMode;
          intDiv  = inDiv;
          intEmp# = inEmp#;
          intYear = inFyr;
       When %Parms() > 7;   // Mode
          chrMode = inMode;
          intDiv  = inDiv;
          intEmp# = inEmp#;
          intYear = inFyr;
       When %Parms() > 6;   // Division
          intDiv  = inDiv;
          intEmp# = inEmp#;
          intYear = inFyr;
       When %Parms() > 5;   // Employer #
          intEmp# = inEmp#;
          intYear = inFyr;
       When %Parms() > 4;   // Policy Year
          intYear = inFyr;
       EndSl;

       Select;
       When chrMode = 'Invoice Cloud';
         // Ensure WDEAP_HLD record exists for invoice.
         // Return zero if not.
          Exsr FindWDEAP_HLD;
          If Not(HasWDEAP_HLD);
             Return 0;
          Endif;
         // Find out if a 'total balance' type
         // of calc is needed.
          DeleteUnpaidInvoices(inCo#:inFnd:
           intEmp#:intDiv:intYear:intInv#:chrRtnInvType);
          If chrRtnInvType <> *Blanks;
             chrMode = chrRtnInvType;
          Endif;
         // Get Prior Billed A/R (FrDt <> ToDt)
          decPrInvBilled = ARBilled(inDt7From:inDt7To:
           inCo#:infnd:intYear:intEmp#:intDiv:chrMode:
           intInv#:chrIncludeBatch:chrExcludeDeposit);
         // Get Cash (To Date Passed as Zero to get cash for all post dates)
          decPaidAmt = ARPaid(inDt7From:0:
           inCo#:infnd:intYear:intEmp#:intDiv:chrMode:
           intInv#:chrIncludeBatch:chrExcludeDeposit);
         // Get Current Inv Amount (FrDt = ToDt)
          decCurInvAmt = ARBilled(inDt7To:inDt7To:
           inCo#:infnd:intYear:intEmp#:intDiv:chrMode:
           intInv#:chrIncludeBatch:chrExcludeDeposit);
         //*** Begin Add *** 12/01/22 *********************************
         // Get Current Inv Paid Amount (FrDt = ToDt)
          decCurPaid = ARPaid(inDt7To:inDt7To:
           inCo#:infnd:intYear:intEmp#:intDiv:chrMode:
           intInv#:chrIncludeBatch:chrExcludeDeposit);
         //*** End   Add *** 12/01/22 *********************************
         //*** Begin Add *** 01/09/24 *********************************
         // Get Future Credits.
          inDt7ToP1 = dtCYMD(dtFuture(dtMDYY(inDt7To):1));
          decFutCredit = ARBilled(inDt7ToP1:9999999:
           inCo#:infnd:intYear:intEmp#:intDiv:chrMode:
           0:chrIncludeBatch:chrExcludeDeposit);
         //*** End   Add *** 01/09/24 *********************************
         // Total balance returns total billed - total paid.
         // If not a total balance calc:
         //  Find out if there's cash in excess of prior invoice billed.
         //  Return current invoice amount if not. Subtract the
         //  excess cash amount from current invoice amount if so.
         //  Return Zero if nothing owed on current invoice.
         // Do not return less than zero.
          balanceAmount = decPrInvBilled - decPaidAmt;
          Select;
          When chrMode = 'Invoice Cloud';
             If balanceAmount < 0;
                balanceAmount = decCurInvAmt + balanceAmount;
                //*** Begin Add *** 01/09/24 **************************
                //*** Begin Add *** 01/09/24 **************************
                //*** If balanceAmount > 0 And decFutCredit < 0;
                //***    balanceAmount = balanceAmount + decFutCredit;
                //*** EndIf;
                //*** End   Add *** 01/09/24 **************************
                //*** End   Add *** 01/09/24 **************************
             Else;
                //*** Begin Chg *** 12/01/22 **************************
                //*** balanceAmount = decCurInvAmt;
                balanceAmount = decCurInvAmt - decCurPaid;
                //*** End   Chg *** 12/01/22 **************************
                //*** Begin Del *** 10/04/24 *******************************
                //*** Begin Add *** 01/09/24 **************************
                //*** If balanceAmount > 0 And decFutCredit < 0;
                //***    balanceAmount = balanceAmount + decFutCredit;
                //*** EndIf;
                //*** End   Add *** 01/09/24 **************************
                //*** End   Del *** 10/04/24 *******************************
             Endif;
          Endsl;
          //*** Begin Add *** 10/04/24 *************************************
          If balanceAmount > 0 And decFutCredit < 0;
             balanceAmount = balanceAmount + decFutCredit;
          EndIf;
          //*** End   Add *** 10/04/24 *************************************
          If balanceAmount < 0;
             balanceAmount = 0;
          EndIf;
       Other;
          decBillAmt =
           ARBilled(inDt7From:inDt7To:inCo#:infnd:intYear:intEmp#:
           intDiv:chrMode:intInv#:chrIncludeBatch:chrExcludeDeposit);
          decPaidAmt =
           ARPaid(inDt7From:inDt7To:inCo#:infnd:intYear:intEmp#:
           intDiv:chrMode:intInv#:chrIncludeBatch:chrExcludeDeposit);
          balanceAmount = decBillAmt - decPaidAmt;
       Endsl;

       Return balanceAmount;

       //-----------------------------------------
       Begsr FindWDEAP_HLD;
       //----------------------------------------
        LeaveSr;
        If intInv# = 0;
           LeaveSr;
        EndIf;
        For idx = 1 To 2;
           Select;
           When idx = 1;
              strLib = RtvQuoteLib('*LIV');
           When idx = 2;
              strLib = RtvQuoteLib('*QTE');
           Endsl;
           strSql =
            'Select Count(*) ' +
            'From ' + %Trim(strLib) + '/WDEAP_HLD ' +
            'Where ' +
            ' EAco# = ' +  %Char(inCo#) +
            ' EAfnd = ' +  %Char(inFnd) +
            ' EAemp# = ' + %Char(intEmp#) +
            ' EAdiv = ' +  %Char(intDiv) +
            ' EAfyr = ' +  %Char(intYear) +
            ' EAninv = ' + %Char(intInv#);
           Exec Sql Close cursorHold;
           Exec Sql Prepare sqlHold from :strsql;
           Exec Sql Declare cursorHold Cursor For sqlHold;
           Exec Sql Open cursorHold;
           Exec Sql Fetch Next From sqlHold Into :intCount;
           Exec Sql Close cursorHold;
           If intCount > 0;
              hasWDEAP_HLD = *On;
              Leave;
           Endif;
        EndFor;
       Endsr;
     PARBalance        e
     P*** End   Add *** 03/22/21 ******************************************

      //***  Begin Add  ***  07/08/20  *************************
    P*====================================================================
    P* Accumulate all Billed A/R
    P*
    P*  ARBilled        - Returns the ammount of Billed A/R
     P*   from date      - A/R posted on or after    (required)
     P*   to date        - A/R posted on or before   (required)
    P*   co#            - accepts the company #     (required)
    P*   group          - accepts the group         (required)
    P*   year           - accepts the year          (optional)
    P*   employer       - accepts the employer      (optional)
    P*   division       - accepts the division      (optional)
    P*   mode           - accepts the mode          (optional)
    P*   invoice #      - accepts the invoice #     (optional)
    P*   includeBatch   - not transferred(Y/N)      (optional)
    P*   ExcludeDeposit - exclude deposits (Y/N)    (optional)
    P*====================================================================
     PARBilled         b                   export
     DARBilled         pi            15  2
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D inMode                        25    const options(*nopass)
     d inInvoice#                    10  0 const options(*nopass)
     d inIncludeBatch                 1    const options(*nopass)
     d inExcludeDeposit...
     d                                1    const options(*nopass)

     d chrCategory     s              1    Inz
     d decCtyAmount    s             15  2 Inz
     d decRtnAmount    s             15  2 Inz
     d decSrAmtExcluded...
     d                 s             15  2 Inz
     d decSrBilledFees...
     d                 s             15  2 Inz
     d decSrBilledTaxes...
     d                 s             15  2 Inz
     d decSrBilledPrem...
     d                 s             15  2 Inz
     d flgDltAr        s              1    Inz
     d flgShowCxAr     s              1
     d indExcDeposit   s               n   Inz
     d indIncBatch     s               n   Inz
     d isIncCxAr       s               n   Inz
     d intInvoice      s             10  0 Inz
     d intDt7To        s              7  0 Inz
     d isIncSrBills    s               n
     d isICBalCalc     s               n
     d isTotICBalCalc  s               n
     d isDeductible    s               n   Inz
     d isFinished      s               n   Inz
     d isRetro         s               n   Inz
     d outAmt          s             15  2 Inz
     d strCategories   s            200    Inz Varying
     d strEAstat       s             25    Inz Varying
     d strExcFromIC    s             25    Inz Varying
     d strCo#          s             25    Inz Varying
     d strDiv          s             25    Inz Varying
     d strEAco#        s              5    Inz
     d strEAfnd        s              5    Inz
     d strEAemp#       s              6    Inz
     d strEAdiv        s              5    Inz
     d strEAfyr        s              5    Inz
     d strEAamt        s              5    Inz
     d strEAarcd       s              6    Inz
     d strEApodt       s              6    Inz
     d strEActyp       s              6    Inz
     d strEmp#         s             25    Inz Varying
     d strFnd          s             25    Inz Varying
     d strFromDate     s             25    Inz Varying
     d strFetchType    s             25    Inz Varying
     d strFyr          s             25    Inz Varying
     d strFileName     s             25    Inz Varying
     d strInvEquals    s             50    Inz Varying
     d strInvField     s             25    Inz Varying
     d strInExInv      s             50    Inz Varying
     d strMode         s             25
     d strPrefix       s              2    Inz
     d strResult       s            200    Inz Varying
     d strSql          s           5000    Inz Varying
     d strSqlWhere     s           1000    Inz Varying
     d strToDate       s             25    Inz Varying

       // Include allfuture A/R if inDt7To = 0
       intDt7To = inDt7To;
       If intDt7To = 0;
          intDt7To = 9999999;
       Endif;

       // Build SQL strings for WMEAP/WDEAP
       strPrefix = 'EA';
       Exsr BuildFieldNameStrings;
       Exsr BuildPolicyStrings;

       If %Parms >= 8;
          strMode = inMode;
          Select;
          When strMode = 'Invoice Cloud'
           And inDt7From = inDt7To;
             isICBalCalc = *On;
             isIncSrBills = *On;
          When strMode = 'S/R Rev Credit Adj';
             isICBalCalc = *On;
             isIncSrBills = *Off;
             isTotICBalCalc = *On;
          When strMode = 'Est Rev Credit Adj';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *On;
          When strMode = 'Audit Adj';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *On;
          When strMode = 'Deductible Credit';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *Off;
          When strMode = 'Retro Credit';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *Off;
          When strMode = 'Invoice Cloud';
             isICBalCalc = *On;
          EndSl;
          If isICBalCalc = *On;
             Exsr GetCxArStatus;
          Endif;
       EndIf;

       If %Parms >= 9;
          intInvoice = inInvoice#;
       Endif;

       // Are we including batch?
       If %parms >= 10;
          indIncBatch = inIncludeBatch = 'Y';
       Endif;

       // Deposit is always included in IC balance calc.
       If %parms >= 11;
          indExcDeposit = inExcludeDeposit = 'Y';
       Endif;

       Select;
       When isICBalCalc = *On;
         // Don't get current invoice billed for audit or
         // revised bill credit. The current invoice is
         // calculated when the from date = to date.
         // For a total Ic balance calculation, get total billed
         // up to post date on prior A/R billed call (including
         // current invoice.)
          If inDt7From = inDt7To And isTotICBalCalc = *On;
             Return 0;
          Endif;
         // Exclude flagged codes from IC
          strExcFromIC = ' And TTexIC <> ''Y''';
         // Check if this calc is for a retro or deductible invoice
          Exsr GetInvoiceType;
         // Include or Exclude categories based on invoice type
          Exsr BuildCategoryString;
         // Include or Exclude transations with the
         // current invoice number.
          Exsr GetSrBilled;
          Exsr CalcSrExcludedAmt;
       Other;
         // Include or Exclude categories
          Exsr BuildCategoryString;
       Endsl;

       strInvField = 'EAninv';
       Exsr BuildIncExcInvoiceString;
       Clear strEAstat;
       Exsr BuildWhereString;
       strFileName = 'WMEAP';
       Exsr BuildSelectString;
       Exsr RunSqlFetch;
       outAmt = decRtnAmount;

       If indIncBatch = *On;
          strInvField = 'EAinv#';
          Exsr BuildIncExcInvoiceString;
          strEAstat = ' And EAstat = '' '' ';
          Exsr BuildWhereString;
          strFileName = 'WDEAP';
          Exsr BuildSelectString;
          Exsr RunSqlFetch;
          outAmt += decRtnAmount;
       Endif;

       // Include CX A/R available for IC calc only
       If isIncCxAr = *On;
          strPrefix = 'XA';
          Exsr BuildFieldNameStrings;
          Exsr BuildPolicyStrings;
          strInvField = 'XAninv';
          Exsr BuildIncExcInvoiceString;
          Clear strEAstat;
          Exsr BuildCategoryString;
          Exsr BuildWhereString;
          strFileName = 'WMXAP';
          Exsr BuildSelectString;
         // Run Sql
          Exsr RunSqlFetch;
          outAmt += decRtnAmount;
       Endif;

       Return outAmt - decSrAmtExcluded;
       //--------------------------------------------------
       Begsr BuildCategoryString;
       //--------------------------------------------------
        Select;
        When isRetro = *On;
           strCategories = ' And ' + strEActyp + ' = ''C'' ';
        When isDeductible = *On;
           strCategories = ' And ' + strEActyp + ' = ''R'' ';
        When isICBalCalc = *On;
           strCategories =
            ' And ' + strEActyp + ' Not In (''C'',''R'') ';
        Other;
           If indExcDeposit = *On;
              strCategories =
               ' And ' + strEActyp + ' Not In (''D'') ';
           Endif;
        Endsl;
       Endsr;
       //--------------------------------------------------
       Begsr BuildIncExcInvoiceString;
       //--------------------------------------------------
       // If not calculating a total IC balance,
       // get current invoice only if From Date = To Date or
       // get prior billed  for invoices <> current invoice
       //  when From Date <> To Date
        Select;
        When isTotICBalCalc = *On;
        When isICBalCalc = *On;
           //*** Begin Chg *** 01/09/24 ************************************
           //*** strInExInv = ' And ' + %Trim(strInvField);
           //*** If inDt7From = inDt7To;
           //***    strInExInv += ' = ';
           //*** Else;
           //***    strInExInv += ' <> ';
           //*** Endif;
           //*** strInExInv += %Char(intInvoice);
           strInExInv = ' ';
           If inDt7To <> 9999999;
              strInExInv = ' And ' + %Trim(strInvField);
              If inDt7From = inDt7To;
                 strInExInv += ' = ';
              Else;
                 strInExInv += ' <> ';
              Endif;
              strInExInv += %Char(intInvoice);
           EndIf;
           //*** End   Chg *** 01/09/24 ************************************
        EndSl;
       Endsr;
       //--------------------------------------------------
       Begsr BuildFieldNameStrings;
       //--------------------------------------------------
        strEAco# = strPrefix + 'co#';
        strEAfnd = strPrefix + 'fnd';
        strEAemp# = strPrefix + 'emp#';
        strEAdiv = strPrefix + 'div';
        strEAfyr = strPrefix + 'fyr';
        strEAamt = strPrefix + 'amt';
        strEAarcd = strPrefix + 'arcd';
        strEApodt = strPrefix + 'podt';
        strEActyp = strPrefix + 'ctyp';
       Endsr;
       //--------------------------------------------------
       Begsr BuildInvoiceEqualsString;
       //--------------------------------------------------
        If intInvoice = 0;
           LeaveSr;
        Endif;
        strInvEquals =  ' And ' + %Trim(strInvField) +
         ' = ' +  %Char(intInvoice);
       Endsr;
       //--------------------------------------------------
       Begsr BuildPolicyStrings;
       //--------------------------------------------------
        strCo# = ' ' + strEAco# + ' = ' + %Char(inCo#);
        strFnd = ' And ' + strEAfnd + ' = ' + %Char(inFnd);
        strFromDate = ' And ' + strEApodt + ' >= ' + %Char(inDt7From);
        strToDate = ' And ' + strEApodt + ' <= ' + %Char(intDt7To);
        If %Parms >= 5 And inFyr <> 0;
           strFyr = ' And ' + strEAfyr + ' = ' + %Char(inFyr);
        Endif;
        If %Parms >= 6 And inEmp# <> 0;
           strEmp# = ' And ' + strEAemp# + ' = ' + %Char(inEmp#);
        Endif;
        If %Parms >= 7;
           strDiv = ' And ' + strEAdiv + ' = ' + %Char(inDiv);
        Endif;
        Clear strEAstat;
       Endsr;
       //--------------------------------------------------
       Begsr BuildSelectString;
       //--------------------------------------------------
        Select;
        When strFetchType = 'S/R Billed';
           strSql =
            'Select ' + strEActyp + ', Sum(' + strEAamt + ') ' +
            ' From ' + strFileName + ' a' +
            ' Join WDF1P '   +
            '  On ' + strEAco# + ' = F1co#  ' +
            '  And ' + strEAfnd + ' = F1fnd ' +
            '  And ' + strEAfyr + ' = F1fyr ' +
            ' Join WTTTP ' +
            '  On ' + strEAco# + ' = TTco# ' +
            '  And ' + strEAarcd + ' = TTkey ' +
            strSqlWhere +
            ' Group By EActyp';
        Other;
           strSql =
            'Select Char(Sum(' +
            'Case When TTtype In (''A'',''B'',''D'',''F'',''H'') ' +
            'Then ' + strEAamt + ' End)) ' +
            'From ' + %Trim(strFileName) + ' Join WTTTP ' +
            'On ' + strEAco# + ' = TTco# And ' + strEAarcd + ' = TTkey ' +
            strSqlWhere;
        EndSl;
       Endsr;
       //--------------------------------------------------
       Begsr BuildWhereString;
       //--------------------------------------------------
        Select;
        When strFetchType = 'S/R Billed';
           strSqlWhere =
            ' Where ' +
            %TrimR(strCo#) +
            %TrimR(strFnd) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr) +
            %TrimR(strToDate) +
            %TrimR(strEAstat) +
            %TrimR(strExcFromIC) +
            ' And TTtype Not In (''P'', ''I'',''Q'')' +
            ' And Exists(Select * From ' +
            strFileName + ' b ' + ' Join WDF1P ' +
            ' On b.' + strEAco# + ' = F1co# ' +
            ' And b.' + strEAfnd + ' = F1fnd ' +
            ' And b.' + strEAfyr + ' = F1fyr ' +
            ' Where a.' + strEAco#  + ' = ' +
             'b.' + strEAco# +
            ' And a.' +  strEAfnd  + ' = ' +
             'b.' + strEAfnd +
            ' And a.' + strEAemp#  + ' = ' +
             'b.' + strEAemp# +
            ' And a.' + strEAdiv + ' = ' +
             'b.' + strEAdiv +
            ' And a.' + strEAfyr  + ' = ' +
             'b.' + strEAfyr +
            ' And a.' + strEApodt  + ' = ' +
             'b.' + strEApodt +
            ' And a.' + strInvField  + ' = ' +
             'b.' + strInvField +
            ' And b.' + strEAarcd + ' = F1srbb) ';
        Other;
           strSqlWhere =
            'Where ' +
            %TrimR(strCo#) +
            %TrimR(strFnd) +
            %TrimR(strFromDate) +
            %TrimR(strToDate) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr) +
            %TrimR(strCategories) +
            %TrimR(strExcFromIC) +
            %TrimR(strInExInv) +
            %TrimR(strEAstat);
        EndSl;
       Endsr;
       //--------------------------------------------------
       Begsr CalcSrExcludedAmt;
       //--------------------------------------------------
        If isIncSrBills = *On;
           LeaveSr;
        EndIf;
        decSrAmtExcluded += decSrBilledFees;
        decSrAmtExcluded += decSrBilledTaxes;
        decSrAmtExcluded += decSrBilledPrem;
       Endsr;
       //--------------------------------------------------
       Begsr EvalFetch;
       //--------------------------------------------------
        Select;
        When strFetchType = 'S/R Billed';
           Select;
           When chrCategory = 'F';
              decSrBilledFees += decCtyAmount;
           When chrCategory = 'T';
              decSrBilledTaxes += decCtyAmount;
           When chrCategory = 'P';
              decSrBilledPrem += decCtyAmount;
           Endsl;
        When strResult = 'C';
           isRetro = *On;
           isFinished = *On;
           LeaveSr;
        When strResult = 'R';
           isDeductible = *On;
           isFinished = *On;
           LeaveSr;
        Other;
           Monitor;
              decRtnAmount = %Dec(strResult:15:2);
              isFinished = *On;
              LeaveSr;
           On-Error;
              decRtnAmount = 0;
           Endmon;
        Endsl;
       EndSr;
      //-----------------------------------------
       Begsr GetCxArStatus;
      //-----------------------------------------
        If %Parms < 7;
           LeaveSr;
        Endif;
        Exec Sql
         Select EXdlAr, EXicCxInv
         Into :flgDltAr, :flgShowCxAr
         From WMXAP Join WMEXP
          On XAco# = EXco#
          And XAfnd = EXfnd
         Where XAco# = :inCo#
          And XAfnd = :inFnd
          And XAemp# = :inEmp#
          And XAdiv = :inDiv
          And XAfyr = :inFyr
          And XApodt <= :intDt7To Limit 1;
         If isValidFetch(sqlStt)
          And flgDltAr = 'Y'
          And flgShowCxAr = 'Y';
            isIncCxAr = *On;
         Else;
            isIncCxAr = *Off;
         Endif;
       Endsr;
      //-----------------------------------------
       Begsr GetInvoiceType;
      //-----------------------------------------
        Select;
        When isICBalCalc = *On;
          // Check if invoice is a Retro or Deductible in Live A/R
           strFileName = 'WMEAP';
           strInvField = 'EANINV';
           Exsr BuildInvoiceEqualsString;
           strSql =
            'Select ' + strEActyp + ' ' +
            'From ' +  %TrimR(strFileName) +
            ' Where ' +
            %TrimR(strCo#) +
            %TrimR(strInvEquals) +
            %TrimR(strFnd) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr);
           Exsr RunSqlFetch;
          // Check if invoice is a Retro or Deductible in A/R Batch
           If Not isRetro And Not isDeductible;
              strFileName = 'WDEAP';
              strInvField = 'EAINV#';
              Exsr BuildInvoiceEqualsString;
              strSql =
               'Select EActyp ' +
               'From ' +
               %TrimR(strFileName) +
               ' Where EAstat = '' '' And ' +
               %TrimR(strCo#) +
               %TrimR(strInvEquals) +
               %TrimR(strFnd) +
               %TrimR(strEmp#) +
               %TrimR(strDiv) +
               %TrimR(strFyr);
              Exsr RunSqlFetch;
           Endif;
          // Check if invoice is a Retro or Deductible in Cx A/R
          // if flagged to do so
           If isIncCxAr And Not isRetro And Not isDeductible;
              strPrefix = 'XA';
              Exsr BuildFieldNameStrings;
              Exsr BuildPolicyStrings;
              strFileName = 'WMXAP';
              strInvField = 'XAninv';
              Exsr BuildInvoiceEqualsString;
              strSql =
               'Select XActyp ' +
               'From ' +
               %TrimR(strFileName) +
               ' Where ' +
               %TrimR(strCo#) +
               %TrimR(strInvEquals) +
               %TrimR(strFnd) +
               %TrimR(strEmp#) +
               %TrimR(strDiv) +
               %TrimR(strFyr);
              Exsr RunSqlFetch;
             // Reset strings for WMEAP/WDEAP
              strPrefix = 'EA';
              Exsr BuildFieldNameStrings;
              Exsr BuildPolicyStrings;
           Endif;
        Endsl;
       Endsr;
       //--------------------------------------------------
       Begsr GetSrBilled;
       //--------------------------------------------------
       // Reduce bills by S/R bill amount if excluding
       // S/R bills from the calculation. Include S/R only
       // on audit and revised bill credit on estimating
       // policy. Don't exclude S/R if calculating current
       // invoice balance.
        If isIncSrBills = *On;
           LeaveSr;
        EndIf;
        strFetchType = 'S/R Billed';
       // Sum S/R Billing in live A/R
        strPrefix = 'EA';
        Exsr BuildFieldNameStrings;
        Exsr BuildPolicyStrings;
        strInvField = 'EANINV';
        Exsr BuildInvoiceEqualsString;
        Clear strEAstat;
        strFileName = 'WMEAP';
        Exsr BuildWhereString;
        Exsr BuildSelectString;
        Exsr RunSqlFetch;
       // Add non-transferred S/R billings
       // from A/R Batch
        strEAstat = ' And EAstat = '' '' ';
        strFileName = 'WDEAP';
        strInvField = 'EAINV#';
        Exsr BuildInvoiceEqualsString;
        Exsr BuildWhereString;
        Exsr BuildSelectString;
        Exsr RunSqlFetch;
        Clear strFetchType;
       Endsr;

       //--------------------------------------------------
       Begsr RunSqlFetch;
       //--------------------------------------------------
        isFinished = *Off;
        Clear strResult;
        Clear decRtnAmount;
        Exec Sql Close cursorBFetch;
        Exec Sql Prepare sqlBFetch From :strSQL;
        Exec Sql Declare cursorBFetch Cursor For sqlBFetch;
        Exec Sql Open cursorBFetch;
        Dou Not(isValidFetch(sqlStt));
           Select;
           When strFetchType = 'S/R Billed';
              Exec Sql
               Fetch Next From cursorBFetch
               Into :chrCategory, :decCtyAmount;
           Other;
              Exec Sql
               Fetch Next From cursorBFetch
               Into :strResult;
           EndSl;
           If Not(isValidFetch(sqlStt));
              Leave;
           Endif;
           Exsr EvalFetch;
           If isFinished = *On;
              Leave;
           Endif;
        Enddo;
        Exec Sql Close cursorBFetch;
       Endsr;
     PARBilled         e
    P*====================================================================
    P* Accumulate all Paid A/R
    P*
    P*  ARPaid          - Returns the ammount of Paid A/R
     P*   from date      - A/R posted on or after    (required)
     P*   to date        - A/R posted on or before   (required)
    P*   co#            - accepts the company #     (required)
    P*   group          - accepts the group         (required)
    P*   year           - accepts the year          (optional)
    P*   employer       - accepts the employer      (optional)
    P*   division       - accepts the division      (optional)
    P*   mode           - accepts the mode          (optional)
    P*   invoice#       - accepts the invoice #     (optional)
    P*   includeBatch   - not transferred(Y/N)      (optional)
    P*   ExcludeDeposit - exclude deposits (Y/N)    (optional)
    P*====================================================================
     PARPaid           b                   export
     DARPaid           pi            15  2
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D inMode                        25    const options(*nopass)
     d inInvoice#                    10  0 const options(*nopass)
     d inIncludeBatch                 1    const options(*nopass)
     d inExcludeDeposit...
     d                                1    const options(*nopass)

     d chrCategory     s              1    Inz
     d decCtyAmount    s             15  2 Inz
     d decPaidFees     s             15  2 Inz
     d decPaidTaxes    s             15  2 Inz
     d decPaidPrem     s             15  2 Inz
     d decSrAmtExcluded...
     d                 s             15  2 Inz
     d decSrBilledFees...
     d                 s             15  2 Inz
     d decSrBilledTaxes...
     d                 s             15  2 Inz
     d decSrBilledPrem...
     d                 s             15  2 Inz
     d decTotalPaid    s             15  2 Inz
     d*** Begin Add *** 12/21/23 ****************************************
     d auditFlag       s              1a   Inz
     d*** End   Add *** 12/21/23 ****************************************
     d indExcDeposit   s               n   Inz
     d indIncBatch     s               n   Inz
     d intDt7To        s              7  0 Inz
     d intInvoice      s             10  0 Inz
     d isDeductible    s               n   Inz
     d isFinished      s               n   Inz
     d isICBalCalc     s               n   Inz
     d isIncCxAr       s               n   Inz
     d isIncSrBills    s               n   Inz
     d isRetro         s               n   Inz
     d isTotICBalCalc  s               n   Inz
     d outAmt          s             15  2 Inz
     d strCategories   s            200    Inz Varying
     d strCo#          s             25    Inz Varying
     d strDiv          s             25    Inz Varying
     d strEAamt        s              5    Inz
     d strEAarcd       s              6    Inz
     d strEAco#        s              5    Inz
     d strEActyp       s              6    Inz
     d strEAdiv        s              5    Inz
     d strEAemp#       s              6    Inz
     d strEAfnd        s              5    Inz
     d strEAfyr        s              5    Inz
     d strEApodt       s              6    Inz
     d strEAstat       s             25    Inz Varying
     d strEmp#         s             25    Inz Varying
     d strExcFromIC    s             25    Inz Varying
     d strFileName     s             25    Inz Varying
     d strFetchType    s             25    Inz Varying
     d strFnd          s             25    Inz Varying
     d strFromDate     s             25    Inz Varying
     d strFyr          s             25    Inz Varying
     d strInvEquals    s             50    Inz Varying
     d*** Begin Add *** 12/01/22 *******************************************
     d strInExInv      s             50    Inz Varying
     d*** End   Add *** 12/01/22 *******************************************
     d strInvField     s             25    Inz Varying
     d strMode         s             25    Inz Varying
     d strPrefix       s              2    Inz
     d strResult       s            200    Inz Varying
     d strSql          s           5000    Inz Varying
     d strSqlWhere     s           1000    Inz Varying
     d strSrBillCode   s             25    Inz Varying
     d strToDate       s             25    Inz Varying
     d*** Begin Add *** 02/12/24 ***************************************
     d invoicePostDate...
     d                 s              7  0
     d*** End   Add *** 02/12/24 ***************************************
     d*** Begin Add *** 05/24/24 ***************************************
     d isSRBilled      s               n   Inz(*Off)
     d*** End   Add *** 05/24/24 ***************************************

       // Include all future cash if To Date = 0
       intDt7To = inDt7To;
       If inDt7To = 0;
        intDt7To = 9999999;
       Endif;

       //*** Begin Add *** 02/12/24 ***************************************
       // Get the invoice posting date
       //*** Begin Add *** 03/01/24 ***************************************
       invoicePostdate = 9999999;
       If %Parms >= 9 And inInvoice# <> 0;
       //*** End   Add *** 03/01/24 ***************************************
       Exec Sql
          Select distinct EApoDt  Into :invoicePostDate
          From wmeap
          Where EAco#=:inCo# And EAfnd=:inFnd And EAfyr=:inFyr And
                EAemp#=:inEmp# And EAdiv=:inDiv And EAninv=:inInvoice#;
       //*** Begin Add *** 03/01/24 ***************************************
       Endif;
       //*** End   Add *** 03/01/24 ***************************************
       //*** End   Add *** 02/12/24 ***************************************

       If %Parms >= 8;
          strMode = inMode;
          Select;
          When strMode = 'Invoice Cloud';
             isICBalCalc = *On;
          When strMode = 'S/R Rev Credit Adj';
             isICBalCalc = *On;
             isIncSrBills = *Off;
             isTotICBalCalc = *On;
          When strMode = 'Est Rev Credit Adj';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *On;
          When strMode = 'Audit Adj';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *On;
          When strMode = 'Deductible Credit';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *Off;
          When strMode = 'Retro Credit';
             isICBalCalc = *On;
             isTotICBalCalc = *On;
             isIncSrBills = *Off;
          EndSl;
          // Get all A/R paid transactions for IC calculation
          If isICBalCalc = *On;
             intDt7To = 9999999;
             strToDate = ' And EApoDt <= ' + %Char(intDt7To);
          EndIf;
          If isICBalCalc = *On;
             Exsr GetCxArStatus;
          Endif;
       EndIf;

       If %Parms >= 9;
          intInvoice = inInvoice#;
       Endif;

       // Are we including batch?
       If %Parms >= 10;
          indIncBatch = inIncludeBatch = 'Y';
       Endif;

       // Are we excluding deposits?
       If %Parms >= 11;
          indExcDeposit = inExcludeDeposit = 'Y';
       Endif;

       Select;
       When isICBalCalc = *On;
        // Exclude flagged codes from IC
        strExcFromIC = ' And TTexIC <> ''Y''';
        // Segregate retro and deductible invoices.
        Exsr BuildCategoryString;
        // Include S/R billings for audit invoices
        // and revised bill credits only.
        Exsr GetSRBilled;
        //*** Begin Add *** 05/24/24 ***************************************
        If isSRBilled;
        //*** End   Add *** 05/24/24 ***************************************
           Exsr GetSRCash;
        //*** Begin Add *** 05/24/24 ***************************************
        EndIf;
        //*** End   Add *** 05/24/24 ***************************************
        Exsr CalcSrExcludedAmt;
       Other;
         // Include or Exclude categories
          Exsr BuildCategoryString;
       Endsl;

       strFetchType = 'A/R Balance';
       // Build SQL strings for WMEAP/WDEAP
       strPrefix = 'EA';
       Exsr BuildFieldNameStrings;
       Exsr BuildPolicyStrings;
       Clear strEAstat;
       strInvField = 'EANINV';
       //*** Begin Add *** 12/01/22 ****************************************
       Exsr BuildIncExcInvoiceString;
       //*** End   Add *** 12/01/22 ****************************************
       Exsr BuildInvoiceEqualsString;
       Exsr BuildWhereString;
       strFileName = 'WMEAP';
       Exsr BuildSelectString;
       Exsr RunSqlFetch;
       outAmt = decTotalPaid;

       If indIncBatch = *On;
          strEAstat = ' And EAstat = '' '' ';
          //*** Begin Add *** 12/01/22 *************************************
          strInvField = 'EAinv#';
          Exsr BuildIncExcInvoiceString;
          //*** End   Add *** 12/01/22 *************************************
          Exsr BuildWhereString;
          strFileName = 'WDEAP';
          Exsr BuildSelectString;
          Exsr RunSqlFetch;
          outAmt += decTotalPaid;
       Endif;
       Clear strFetchType;

       // Return cash (minus whatever S/R was billed)
       // to segregate S/R cash for IC balance calc. Exclude
       // S/R Billings in excess of cash from balance calc.
       // Cash is not reduced by S/R bills for retro, deductible,
       // audit, or revised bill credit on an estimating policy.
       outAmt = outAmt - decSrAmtExcluded;
       //*** Begin Chg *** 12/21/23 ****************************************
       //*** If outAmt < 0;
       ExSr GetAuditStatus;
       If outAmt < 0 And
          (auditFlag <> 'A' Or (inDt7From = inDt7To And inDt7To <> 0));
       //*** End   Chg *** 12/21/23 ****************************************
          outAmt = 0;
       EndIf;

       Return outAmt;
       //--------------------------------------------------
       Begsr BuildFieldNameStrings;
        //--------------------------------------------------
        strEAco# = strPrefix + 'co#';
        strEAfnd = strPrefix + 'fnd';
        strEAemp# = strPrefix + 'emp#';
        strEAdiv = strPrefix + 'div';
        strEAfyr = strPrefix + 'fyr';
        strEAamt = strPrefix + 'amt';
        strEAarcd = strPrefix + 'arcd';
        strEApodt = strPrefix + 'podt';
        strEActyp = strPrefix + 'ctyp';
       Endsr;
       //*** Begin Add *** 12/01/22 ***************************************
       //--------------------------------------------------
       Begsr BuildIncExcInvoiceString;
       //--------------------------------------------------
       // If not calculating a total IC balance,
       // get current invoice only if From Date = To Date or
       // get prior billed  for invoices <> current invoice
       //  when From Date <> To Date
        strInExInv = ' ';
        Select;
        When isTotICBalCalc = *On;
        When isICBalCalc = *On;
           If inDt7From = inDt7To And inDt7To <> 0;
              strInExInv = ' And ' + %Trim(strInvField);
              strInExInv += ' = ';
              strInExInv += %Char(intInvoice);
           Endif;
        EndSl;
       Endsr;
       //*** End   Add *** 12/01/22 ***************************************
       //--------------------------------------------------
       Begsr BuildInvoiceEqualsString;
        //--------------------------------------------------
        If intInvoice = 0;
           LeaveSr;
        Endif;
        strInvEquals =  ' And ' + %Trim(strInvField) +
         ' = ' +  %Char(intInvoice);
       Endsr;
       //--------------------------------------------------
       Begsr BuildPolicyStrings;
        //--------------------------------------------------
        strCo# = ' ' + strEAco# + ' = ' + %Char(inCo#);
        strFnd = ' And ' + strEAfnd + ' = ' + %Char(inFnd);
        strFromDate = ' And ' + strEApodt + ' >= ' + %Char(inDt7From);
        strToDate = ' And ' + strEApodt + ' <= ' + %Char(intDt7To);
        strSrBillCode = ' And ' + strEAarcd +  ' = F1srbb ';
        If %Parms >= 5 And inFyr <> 0;
         strFyr = ' And ' + strEAfyr + ' = ' + %Char(inFyr);
        Endif;
        If %Parms >= 6 And inEmp# <> 0;
         strEmp# = ' And ' + strEAemp# + ' = ' + %Char(inEmp#);
        Endif;
        If %Parms >= 7;
           strDiv = ' And ' + strEAdiv + ' = ' + %Char(inDiv);
        Endif;
        Clear strEAstat;
       Endsr;
       //--------------------------------------------------
       Begsr BuildSelectString;
        //--------------------------------------------------
        Select;
        When strFetchType = 'A/R Balance';
        //*** Begin Chg *** 02/12/24 ***************************************
        //***   strSql =
        //***    'Select Char(Sum(' +
        //***     ' Case ' +
        //*** Begin Chg *** 08/24/23 ***************************************
        //***'  When TTtype In (''P'',''I'',''Q'') Then ' + strEAamt +
        //***'  When TTtype In (''N'',''R'',''T'') Then -' + strEAamt +
        //***'  When TTtype In (''P'',''I'',''Q'',''T'') Then ' + strEAamt +
        //***//*** Begin Chg *** 12/19/23 **********************************
        //***//*** '  When TTtype In (''N'',''R'') Then -' + strEAamt +
        //***'  When TTtype In (''N'') Then -' + strEAamt +
        //***//*** End   Chg *** 12/19/23 **********************************
        //*** End   Chg *** 08/24/23 ***************************************
        //*** Begin Add *** 08/29/23 ***************************************
        //***     '  Else 0 ' +
        //*** End   Add *** 08/29/23 ***************************************
        //***     ' End)) ' +
        //***    ' From ' + strFileName +
        //***    ' Join WTTTP ' +
        //***    '  On ' + strEAco# + ' = TTco# ' +
        //***    '  And ' + strEAarcd + ' = TTkey ' +
        //***    strSqlWhere;
           strSql =
            'Select Char(Sum(' +
             ' Case ' +
             '  When TTtype In (''P'',''I'',''Q'',''T'') Then ' + strEAamt +
             '  When TTtype In (''N'', ''R'') Then -' + strEAamt +
             '  Else 0 ' +
             ' End)) ' +
            ' From ' + strFileName +
            ' Join WTTTP ' +
            '  On ' + strEAco# + ' = TTco# ' +
            '  And ' + strEAarcd + ' = TTkey ' +
            strSqlWhere +
            ' And ((TTtype = ''R'' And EApodt <= ' +
            %EditC(invoicePostDate:'X') + ') Or (TTtype <> ''R''))';
        //*** End   Chg *** 02/12/24 ***************************************
        When strFetchType = 'Category';
           strSql =
            'Select ' + strEActyp +
            ' From ' +  strFileName +
            ' Join WTTTP ' +
            '  On ' + strEAco# + ' = TTco# ' +
            '  And ' + strEAarcd + ' = TTkey ' +
            strSqlWhere;
        When strFetchType = 'S/R Billed';
           strSql =
            'Select ' + strEActyp + ', Sum(' + strEAamt + ') ' +
            ' From ' + strFileName + ' a' +
            ' Join WDF1P '   +
            '  On ' + strEAco# + ' = F1co#  ' +
            '  And ' + strEAfnd + ' = F1fnd ' +
            '  And ' + strEAfyr + ' = F1fyr ' +
            ' Join WTTTP ' +
            '  On ' + strEAco# + ' = TTco# ' +
            '  And ' + strEAarcd + ' = TTkey ' +
            strSqlWhere +
            ' Group By EActyp';
        When strFetchType = 'S/R Paid';
           strSql =
            'Select ' + strEActyp  + ', Char(Sum(' +
             ' Case ' +
        //*** Begin Chg *** 08/24/23 ***************************************
        //***'  When TTtype In (''P'',''I'',''Q'') Then ' + strEAamt +
        //***'  When TTtype In (''N'',''R'',''T'') Then -' + strEAamt +
             '  When TTtype In (''P'',''I'',''Q'',''T'') Then ' + strEAamt +
             '  When TTtype In (''N'',''R'') Then -' + strEAamt +
        //*** End   Chg *** 08/24/23 ***************************************
        //*** Begin Add *** 08/29/23 ***************************************
             '  Else 0 ' +
        //*** End   Add *** 08/29/23 ***************************************
             ' End)) ' +
            ' From ' + strFileName +
            ' Join WTTTP ' +
            '  On ' + strEAco# + ' = TTco# ' +
            '  And ' + strEAarcd + ' = TTkey ' +
            strSqlWhere +
            ' Group By EActyp';
        When strFetchType = 'Cx A/R';
           strSql =
            'Select EXdlar Concat EXicCxInv' +
            ' From ' + strFileName +
            ' Join WMEXP ' +
            '  On ' + strEAco# + ' = EXco# ' +
            '  And ' + strEAfnd + ' = EXfnd ' +
            ' Join WTTTP ' +
            '  On ' + strEAco# + ' = TTco# ' +
            '  And ' + strEAarcd + ' = TTkey ' +
            strSqlWhere;
        EndSl;
       Endsr;
       //--------------------------------------------------
       Begsr BuildWhereString;
        //--------------------------------------------------
        Select;
        When strFetchType = 'A/R Balance';
           strSqlWhere =
            ' Where ' +
            %TrimR(strCo#) +
            %TrimR(strFnd) +
            %TrimR(strFromDate) +
            %TrimR(strToDate) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr) +
            %TrimR(strCategories) +
            %TrimR(strExcFromIC) +
            //*** Begin Add *** 12/01/22 ***********************************
            %TrimR(strInExInv) +
            //*** End   Add *** 12/01/22 ***********************************
            %TrimR(strEAstat);
        When strFetchType = 'Category';
           strSqlWhere =
            ' Where ' +
            %TrimR(strCo#) +
            %TrimR(strInvEquals) +
            %TrimR(strFnd) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr) +
            %TrimR(strExcFromIC) +
            %TrimR(strEAstat);
        When strFetchType = 'Cx A/R';
           strSqlWhere =
            ' Where ' +
            %TrimR(strCo#) +
            %TrimR(strFnd) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr) +
            %TrimR(strExcFromIC) +
            %TrimR(strToDate);
        When strFetchType = 'S/R Billed';
           strSqlWhere =
            ' Where ' +
            %TrimR(strCo#) +
            %TrimR(strFnd) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr) +
            %TrimR(strToDate) +
            %TrimR(strEAstat) +
            %TrimR(strExcFromIC) +
            ' And TTtype Not In (''P'', ''I'',''Q'')' +
            ' And Exists(Select * From ' +
            strFileName + ' b ' + ' Join WDF1P ' +
            ' On b.' + strEAco# + ' = F1co# ' +
            ' And b.' + strEAfnd + ' = F1fnd ' +
            ' And b.' + strEAfyr + ' = F1fyr ' +
            ' Where a.' + strEAco#  + ' = ' +
             'b.' + strEAco# +
            ' And a.' +  strEAfnd  + ' = ' +
             'b.' + strEAfnd +
            ' And a.' + strEAemp#  + ' = ' +
             'b.' + strEAemp# +
            ' And a.' + strEAdiv + ' = ' +
             'b.' + strEAdiv +
            ' And a.' + strEAfyr  + ' = ' +
             'b.' + strEAfyr +
            ' And a.' + strEApodt  + ' = ' +
             'b.' + strEApodt +
            ' And a.' + strInvField  + ' = ' +
             'b.' + strInvField +
            ' And b.' + strEAarcd + ' = F1srbb) ';
        When strFetchType = 'S/R Paid';
           strSqlWhere =
            ' Where ' +
            %TrimR(strCo#) +
            %TrimR(strFnd) +
            %TrimR(strEmp#) +
            %TrimR(strDiv) +
            %TrimR(strFyr) +
            %TrimR(strExcFromIC) +
            //*** Begin Add *** 12/01/22 ***********************************
            %TrimR(strInExInv) +
            //*** End   Add *** 12/01/22 ***********************************
            %TrimR(strEAstat);
        EndSl;
       Endsr;
       //--------------------------------------------------
       Begsr CalcSrExcludedAmt;
       //--------------------------------------------------
        If isIncSrBills = *On;
           LeaveSr;
        EndIf;
        If decSrBilledFees < decPaidFees;
           decSrAmtExcluded += decSrBilledFees;
        Else;
           decSrAmtExcluded += decPaidFees;
        Endif;
        If decSrBilledTaxes < decPaidTaxes;
           decSrAmtExcluded += decSrBilledTaxes;
        Else;
           decSrAmtExcluded += decPaidTaxes;
        Endif;
        If decSrBilledPrem < decPaidPrem;
           decSrAmtExcluded += decSrBilledPrem;
        Else;
           decSrAmtExcluded += decPaidPrem;
        Endif;
       Endsr;
       //--------------------------------------------------
       Begsr EvalFetch;
        //--------------------------------------------------
        Select;
        When strFetchType = 'A/R Balance';
           Monitor;
              decTotalPaid = %Dec(strResult:15:2);
              isFinished = *On;
           On-Error;
              decTotalPaid = 0;
           Endmon;
        When strFetchType = 'Category';
           Select;
           When strResult = 'C';
              isRetro = *On;
              isFinished = *On;
           When strResult = 'R';
              isDeductible = *On;
              isFinished = *On;
           EndSl;
        When strFetchType = 'Cx A/R';
           If strResult = 'YY';
              isIncCxAr = *On;
           Else;
              isIncCxAr = *Off;
           Endif;
           isFinished = *On;
        When strFetchType = 'S/R Billed';
           Select;
           When chrCategory = 'F';
              decSrBilledFees += decCtyAmount;
           When chrCategory = 'T';
              decSrBilledTaxes += decCtyAmount;
           When chrCategory = 'P';
              decSrBilledPrem += decCtyAmount;
           Endsl;
        When strFetchType = 'S/R Paid';
           Select;
           //*** Begin Chg *** 05/24/24 ************************************
           //*** When chrCategory = 'F';
           When chrCategory = 'F' Or chrCategory = '1' Or
                chrCategory = '2' Or chrCategory = '3' Or
                chrCategory = '4' Or chrCategory = '5' Or
                chrCategory = '6' Or chrCategory = '7';
           //*** End   Chg *** 05/24/24 ************************************
              decPaidFees += decCtyAmount;
           When chrCategory = 'T';
              decPaidTaxes += decCtyAmount;
           When chrCategory = 'P';
              decPaidPrem += decCtyAmount;
           Endsl;
        EndSl;
       EndSr;
       //--------------------------------------------------
       Begsr BuildCategoryString;
        //--------------------------------------------------
        strFetchType = 'Category';
        // Build SQL strings for WMEAP/WDEAP
        strPrefix = 'EA';
        Exsr BuildFieldNameStrings;
        Exsr BuildPolicyStrings;
        Select;
        When isICBalCalc = *On;
         // Look Retros or Deductibles in Live A/R
           Clear strEAstat;
           strInvField = 'EANINV';
           Exsr BuildInvoiceEqualsString;
           Exsr BuildWhereString;
           strFileName = 'WMEAP';
           Exsr BuildSelectString;
           Exsr RunSqlFetch;
           // Check if invoice is a Retro or Deductible in A/R Batch
           If Not isRetro And Not isDeductible;
              // Look in A/R Batch
              strEAstat = ' And EAstat = '' '' ';
              strInvField = 'EAINV#';
              Exsr BuildInvoiceEqualsString;
              Exsr BuildWhereString;
              strFileName = 'WDEAP';
              Exsr BuildSelectString;
              Exsr RunSqlFetch;
           Endif;
           // Check if invoice is a Retro or Deductible in Cx A/R
           // if flagged to do so
           If isIncCxAr And Not isRetro And Not isDeductible;
              strPrefix = 'XA';
              Exsr BuildFieldNameStrings;
              Exsr BuildPolicyStrings;
              strInvField = 'XANINV';
              Exsr BuildInvoiceEqualsString;
              Clear strEAstat;
              Exsr BuildWhereString;
              strFileName = 'WMXAP';
              Exsr BuildSelectString;
              Exsr RunSqlFetch;
           Endif;
           // Set category variable accordingly
           Select;
           When isRetro = *On;
              strCategories = ' And EActyp = ''C'' ';
           When isDeductible = *On;
              strCategories = ' And EActyp = ''R'' ';
           Other;
              strCategories =
               ' And EActyp Not In (''C'',''R'') ';
           Endsl;
        When indExcDeposit = *On;
           strCategories =
            ' And EActyp Not In (''D'') ';
        Endsl;
        Clear strFetchType;
       Endsr;
       //*** Begin Add *** 12/21/23 ****************************************
       //===================================================================
       Begsr GetAuditStatus; // Retrieves Audit Flag
       //===================================================================

         Exec Sql Select ELfaf Into :auditFlag
                  From wdelp
                  Where ELco# = :inCo# And ELfnd  = :inFnd And
                        ELfyr = :inFyr And ELemp# = :inEmp# And
                        ELdiv = :inDiv;

       Endsr;
       //*** End   Add *** 12/21/23 ****************************************
       //-----------------------------------------
       Begsr GetCxArStatus;
        //-----------------------------------------
        strFetchType = 'Cx A/R';
        If %Parms < 7;
           LeaveSr;
        Endif;
        strPrefix = 'XA';
        Exsr BuildFieldNameStrings;
        Exsr BuildPolicyStrings;
        Clear strEAstat;
        Exsr BuildWhereString;
        strFileName = 'WMXAP';
        Exsr BuildSelectString;
        Exsr RunSqlFetch;
        Clear strFetchType;
       Endsr;
       //--------------------------------------------------
       Begsr GetSrBilled;
       //--------------------------------------------------
       // Reduce cash by S/R bill amount if excluding
       // S/R bills from the calculation. Include S/R only
       // on audit and revised bill credit on estimating
       // policy.
        If isIncSrBills = *On;
           LeaveSr;
        EndIf;
        strFetchType = 'S/R Billed';
       // Sum S/R Billing in live A/R
        strPrefix = 'EA';
        Exsr BuildFieldNameStrings;
        Exsr BuildPolicyStrings;
        strInvField = 'EANINV';
        Exsr BuildInvoiceEqualsString;
        Clear strEAstat;
        strFileName = 'WMEAP';
        Exsr BuildWhereString;
        Exsr BuildSelectString;
        Exsr RunSqlFetch;
       // Add non-transferred S/R billings
       // from A/R Batch
        strEAstat = ' And EAstat = '' '' ';
        strFileName = 'WDEAP';
        strInvField = 'EAINV#';
        Exsr BuildInvoiceEqualsString;
        Exsr BuildWhereString;
        Exsr BuildSelectString;
        Exsr RunSqlFetch;
        Clear strFetchType;
       Endsr;
       //--------------------------------------------------
       Begsr GetSrCash;
        //--------------------------------------------------
        If isIncSrBills = *On;
           LeaveSr;
        EndIf;
        strFetchType = 'S/R Paid';
        strPrefix = 'EA';
        Exsr BuildFieldNameStrings;
        Exsr BuildPolicyStrings;
        strInvField = 'EANINV';
        Exsr BuildInvoiceEqualsString;
        Clear strEAstat;
        Exsr BuildWhereString;
        strFileName = 'WMEAP';
        Exsr BuildSelectString;
        Exsr RunSqlFetch;
        // Add non-transferred S/R billings
        // from A/R Batch
        strEAstat = ' And EAstat = '' '' ';
        strInvField = 'EAINV#';
        Exsr BuildInvoiceEqualsString;
        Exsr BuildWhereString;
        strFileName = 'WDEAP';
        Exsr BuildSelectString;
        Exsr RunSqlFetch;
        Clear strFetchType;
       Endsr;
       //--------------------------------------------------
       Begsr RunSqlFetch;
        //--------------------------------------------------
        isFinished = *Off;
        Clear strResult;
        Clear decTotalPaid;
        Exec Sql
         Close cursorFetch;
        Exec Sql
         Prepare sqlFetch From :strSQL;
        Exec Sql
         Declare cursorFetch Cursor For sqlFetch;
        Exec Sql
         Open cursorFetch;
        //*** Begin Add *** 05/24/24 ************************************
        isSRBilled = *Off;
        //*** End   Add *** 05/24/24 ************************************
        Dou Not(isValidFetch(sqlStt));
           Select;
           When strFetchType = 'S/R Billed'
            Or strFetchType = 'S/R Paid';
              Exec Sql
               Fetch Next From cursorFetch
               Into :chrCategory,
               :decCtyAmount;
               //*** Begin Add *** 05/24/24 *****************************
               If strFetchType = 'S/R Billed' And isValidFetch(sqlStt);
                  isSRBilled = *On;
               EndIf;
               //*** End   Add *** 05/24/24 *****************************
           Other;
              Exec Sql
               Fetch Next
               From cursorFetch
               Into :strResult;
           EndSl;
           If Not(isValidFetch(sqlStt));
              Leave;
           Endif;
           Exsr EvalFetch;
           If isFinished = *On;
              Leave;
           Endif;
        Enddo;
        Exec Sql
         Close cursorFetch;
       Endsr;
     PARPaid           e
      //***  End   Add  ***  07/08/20  *************************
    P*====================================================================
    P* ARValue - Determine if A/R amount is positive or negative
    P*
    P*  ARValue   - Returns the value based on the type
    P*   ARAmount - passed in for validation  (required)
    P*   ARType   - passed in for validation  (required)
    P*====================================================================
    P*
     PARValue          b                    export
    P*
     DARValue          PI             9  2
     D ARAmount                       9  2  const
     D ARType                         1     const

     D amount          s              9  2

     C                   select
     C                   when      ARType = 'P' or ARType = 'Q' or
     C                             ARType = 'I' or ARType = 'T'
     C                   eval      amount = ARAmount * -1
     C                   other
     C                   eval      amount = ARAmount
     C                   endsl
     C*
     C                   return    amount

     PARValue          e





    P*====================================================================
    P* ARCatSrch - Searh A/R categories
    P*
    P*  ARCatSrch - Returns the RRN of WTCTP
    P*   inkc     - returns *on if F3 was pressed from search  (required)
    P*   inkl     - returns *on if F12 was pressed from search (required)
    P*====================================================================
    P*
     PARCatSrch        b                    export
    P*
     DARCatSrch        PI             9  0
     D inkc                           1
     D inkl                           1

     DW2162R           PR                  extpgm('W2162R')
     D ##call                         1    const
     D ##kc                           1
     D ##kl                           1
     D ##rrn                          9  0

     D rrn             s              9  0

     C                   callp     W2162R('2':inkc:inkl:rrn)
     C*
     C                   return    rrn

     PARCatSrch        e





    P*====================================================================
    P* ARCodeSrch - Searh A/R codes
    P*
    P*  ARCodeSrch - Returns the the A/R code
    P*   inkc      - returns *on if F3 was pressed from search  (required)
    P*   inkl      - returns *on if F12 was pressed from search (required)
    P*====================================================================
    P*
     PARCodeSrch       b                    export
    P*
     DARCodeSrch       PI            10
     D inkc                           1
     D inkl                           1

     DW2161R           PR                  extpgm('W2161R')
     D ##call                         1    const
     D ##kc                           1
     D ##kl                           1
     D ##key                         10

     D key             s             10

     C                   callp     W2161R('2':inkc:inkl:key)
     C*
     C                   return    key

     PARCodeSrch       e





    P*====================================================================
    P* BillType - Check for type as a bill type
    P*
    P*  BillType - Returns *on if A/R type is a bill type
    P*   type    - passed in for checking  (required)
    P*====================================================================
     PBillType         b                   export
     DBillType         pi             1n
     D inType                         1    const

     C                   select
     C                   when      inType = 'A'
     C                   return    *on
     C                   when      inType = 'B'
     C                   return    *on
     C                   when      inType = 'D'
     C                   return    *on
     C                   when      inType = 'F'
     C                   return    *on
     C                   when      inType = 'H'
     C                   return    *on
     C                   endsl
     C*
     C                   return    *off

     PBillType         e
       //===================================================
       Dcl-Proc DeleteUnpaidInvoices Export;
       //===================================================
       // Return *On if condition exists that warrants removing
       // unpaid invoices at Invoice Cloud.
       // outInvType describes the condition.
        Dcl-Pi *N Ind;
         inCo# Packed(3:0) Const;
         inFnd Packed(3:0) Const;
         inEmp# Packed(9:0) Const;
         inDiv Packed(5:0) Const;
         inFyr Packed(3:0) Const;
         inInv# Packed(10:0) Const;
         outInvType Char(25);
        End-Pi;
        Dcl-S chrPlanType Char(1);
        Dcl-S decInvTotal Packed(13:2);
        Dcl-S intCount Packed(3:0);
       // Look for audit invoice
        Exec Sql
         Select Count(*)
          Into :intCount
          From WMEAP
          Where EAninv <> 0
           And EAninv = :inInv#
           And EAco# = :inCo#
           And EAfnd = :inFnd
           And EAemp# = :inEmp#
           And EAdiv = :inDiv
           And EAfyr = :inFyr
           And EAarty = 'D';
        If intCount > 0;
         outInvType = 'Audit Adj';
         Return *On;
        Endif;
       // Look for revised bill credit
        Exec Sql
         Select Sum(
          Case
           When EAarty Not In ('P', 'I','Q')
           Then EAamt
          End)
         Into :decInvTotal
         From WMEAP a
         Join WTTTP
          On a.EAco# = TTco#
          And a.EAarcd = TTkey
         Where a.EAninv <> 0
          And a.EAninv = :inInv#
          And a.EAco# = :inCo#
          And a.EAfnd = :inFnd
          And a.EAemp# = :inEmp#
          And a.EAdiv = :inDiv
          And a.EAfyr = :inFyr
          And TTexic <> 'Y'
          And Exists(
           Select *
           From WMEAP b
           Where a.EAninv = b.EAninv
            And a.EAco# = b.EAco#
            And a.EAfnd = b.EAfnd
            And a.EAemp# = b.EAemp#
            And a.EAdiv = b.EAdiv
            And a.EAfyr = b.EAfyr
            And b.EAPgm = 'W8011R');
       // If revised bill credit, find out if S/R or Est
        If decInvTotal < 0;
           Exec Sql
            Select BSsefl
            Into :chrPlanType
            From WDELP
            Join WTBSP
             On ELco# = BSco#
             And ELbscd = BSkey
            Where ELco# = :inCo#
             And ELfnd = :inFnd
             And ELemp# = :inEmp#
             And ELdiv = :inDiv
             And ELfyr = :inFyr;
          // Revised bill credit includes S/R bills if estimating
          // Revised bill credit excludes S/R bills if self reporting.
           Select;
           When chrPlanType = 'S';
              outInvType = 'S/R Rev Credit Adj';
           When chrPlanType = 'E';
              outInvType = 'Est Rev Credit Adj';
           EndSl;
           Return *On;
        Endif;
       // Look for deductible credit adjustment
        Exec Sql
         Select Sum(EAamt)
         Into :decInvTotal
         From WMEAP a
         Join WTTTP
          On a.EAco# = TTco#
          And a.EAarcd = TTkey
         Where a.EAninv <> 0
          And a.EAninv = :inInv#
          And a.EAco# = :inCo#
          And a.EAfnd = :inFnd
          And a.EAemp# = :inEmp#
          And a.EAdiv = :inDiv
          And a.EAfyr = :inFyr
          And TTexic <> 'Y'
          And Exists(
           Select *
           From WMEAP b
           Where a.EAninv = b.EAninv
            And a.EAco#  = b.EAco#
            And a.EAfnd  = b.EAfnd
            And a.EAemp# = b.EAemp#
            And a.EAdiv  = b.EAdiv
            And a.EAfyr  = b.EAfyr
            And b.EActyp = 'R');
        If decInvTotal < 0;
           outInvType = 'Deductible Credit';
           Return *On;
        Endif;
       // Look for retro settlement credit adjustment
        Exec Sql
         Select Sum(EAamt)
         Into :decInvTotal
         From WMEAP a
         Join WTTTP
          On a.EAco# = TTco#
          And a.EAarcd = TTkey
         Where a.EAninv <> 0
          And a.EAninv = :inInv#
          And a.EAco# = :inCo#
          And a.EAfnd = :inFnd
          And a.EAemp# = :inEmp#
          And a.EAdiv = :inDiv
          And a.EAfyr = :inFyr
          And TTexic <> 'Y'
          And Exists(
           Select *
           From WMEAP b
           Where a.EAninv = b.EAninv
            And a.EAco#  = b.EAco#
            And a.EAfnd  = b.EAfnd
            And a.EAemp# = b.EAemp#
            And a.EAdiv  = b.EAdiv
            And a.EAfyr  = b.EAfyr
            And b.EActyp = 'C');
        If decInvTotal < 0;
           outInvType = 'Retro Credit';
           Return *On;
        Endif;
        Return *Off;
       End-Proc;
    P*====================================================================
    P* GetARCat - Get the A/R category for a specific A/R code
    P*
    P*  GetARCat - Returns the A/R category
    P*   code    - passed in for validation  (required)
    P*   TT_rrn  - returns the RRN of WTTTP
    P*   co#     - accepts the company #
    P*====================================================================
    P*
     PGetARCat         b                   export
    P*
     DGetARCat         PI             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const

     D company         s                   like(TTCO#)
     D key             s                   like(TTKEY)

     C     ttky          klist
     C                   kfld                    company
     C                   kfld                    key

     C                   eval      company = UserCo#(q1user)
     C                   eval      key     = code
     C                   eval      TTCTYP  = *blank
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     ttky          chain     ptt01                              60
     C*
     C                   if        %parms >= 2
     C                   eval      TT_rrn = tbrrn
     C                   endif
     C*
     C                   return    TTCTYP

     PGetARCat         e

      //***  Begin Add  ***  01/22/19  *************************
    P*====================================================================
    P* GetARCode - Get the appropriate A/R code
    P*
    P*  GetARCode - Returns the A/R code
     P*   type     - A/R code to be returned   (required)
     P*            -- paymentSR
     P*            -- billSR
    P*   co#      - accepts the company #  (required)
    P*   group    - accepts the group      (required)
    P*   year     - accepts the year       (required)
    P*====================================================================
    P*
     PGetARCode        b                   export
    P*
     DGetARCode        pi             2
     D inType                        25    const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inYear                         3  0 const
     D*
     DoutCode          s              2    inz
     DstrType          s             25    inz
      /Free
       strType = Upper(inType);
       Select;
       When strType = 'PAYMENTSR';
          Exsr PaymentSR;
       When strType = 'BILLSR';
          Exsr BillSR;
       Endsl;
       Return outCode;
       //=======================================================
       Begsr BillSR;
        Exec sql
         select F1srBB into :outCode
          from WDF1P
          where F1co# = :inCo# and F1fnd = :inFnd and F1fyr = :inYear;
       Endsr;
       //=======================================================
       Begsr PaymentSR;
        Exec sql
         select C3spCd into :outCode
          from WTC3P
          where C3co# = :inCo# and C3fnd = :inFnd;
       Endsr;
      /End-free
     PGetARCode        e
      //***  End   Add  ***  01/22/19  *************************
     P*** Begin Add *** 06/12/24 *******************************************
    P*====================================================================
    P* GetDefaultARCode - Get First A/R Code that matches type and category
    P*
    P*  GetDefaultARCode - Returns the Default A/R Code
    P*   type     - used to get A/R code  (required)
    P*   category - used to get A/R code  (required)
    P*   co#      - accepts the company # (optional)
    P*====================================================================
     PGetDefaultARCode...
     P                 b                   export
    P*
     DGetDefaultARCode...
     D                 PI             2
     D inType                         1    const
     D inCategory                     1    const
     D inCompany                      3  0 options(*nopass) const

     D company         s                   like(TTCO#)
     D type            s                   like(TTtype)
     D category        s                   like(TTcTyp)
     D arCode          s              2    Inz

       type     = inType;
       category = inCategory;

       If %Parms() > 2;
          company = inCompany;
       Else;
          company = UserCo#(q1user);
       EndIf;

       Exec Sql Select TTkey
                Into   :arCode
                From wtttp
                Where TTco#=:company And TTtype=:type And TTcTyp=:category
                Limit 1;

       Return arCode;

     PGetDefaultARCode...
     P                 e
     P*** End   Add *** 06/12/24 *******************************************

      //***  Begin Add  ***  01/22/19  *************************


    P*====================================================================
    P* GetARComm - Get the A/R exclude commission flag for an A/R code
    P*
    P*  GetARComm - Returns the A/R exclude commission flag
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     PGetARComm        b                   export
    P*
     DGetARComm        PI             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const

     D company         s                   like(TTCO#)
     D key             s                   like(TTKEY)

     C     ttky          klist
     C                   kfld                    company
     C                   kfld                    key

     C                   eval      company = UserCo#(q1user)
     C                   eval      key     = code
     C                   eval      TTECFL  = *blank
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     ttky          chain     ptt01                              60
     C*
     C                   if        %parms >= 2
     C                   eval      TT_rrn = tbrrn
     C                   endif
     C*
     C                   return    TTECFL

     PGetARComm        e
    P*====================================================================
    P* GetARDate - Get A/R transaction or posing date
    P*
    P*  GetARDate - Returns the date used for any particular transaction
    P*   co#      - accepts the company #        (required)
    P*   group    - accepts the group #          (required)
    P*   year     - accepts the policy year      (required)
    P*   employer - accepts the employer         (required)
    P*   division - accepts the division         (required)
    P*   type     - accepts the transaction type (required)
    P*   postDate - accepts the posting date     (required)
    P*   tranDate - accepts the transaction date (required)
    P*====================================================================
    P*
     PGetARDate        b                   export
    P*
     DGetARDate        PI             7  0
     D co#                            3  0 const
     D group                          3  0 const
     D year                           3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D type                           1    const
     D postDate                       7  0 const
     D tranDate                       7  0 const

     D BS_rrn          s              9  0
     D billSchCd       s             50
     D selfRptEst      s              1
     D date            s              7  0

     C                   eval      selfRptEst = 'E'
     C                   eval      billSchCd = GetBillSch(GetEmpYrRR(year:
     C                                                    employer:division:
     C                                                    group:co#):
     C                                         BS_RRN)
     C     BS_rrn        chain     pbs0p                              60
     C  n60              eval      selfRptEst = BSSEFL
    C*
     C                   if        selfRptEst = 'S'
    C*
    C*  Use the transaction date for Self-Reporting accounts, except if the
    C*   record is a deposit (H) because they may put the deposit on a
    C*   payment plan.  Also, use the posting date if the record is an
    C*   adjustment (A) because this is used to put an audit on a payment
    C*   plan.  ALL other adjustments will show correctly because the
    C*   transaction date always equals the posting date.
    C*
     C                   select
     C                   when      type = 'A'
     C                   eval      date = postDate
     C                   when      postDate > tranDate
     C                              and type <> 'P' and type <> 'I'
     C                              and type <> 'Q' and type <> 'R'
     C                              and type <> 'N'
      *** beg add ***** 08/23/13 **********************************************
      *** beg del ***** 10/11/13 **********************************************
     C*************                 and type <> 'B'
      *** end del ***** 10/11/13 **********************************************
      *** end add ***** 08/23/13 **********************************************
     C                   eval      date = postDate
     C                   other
     C                   eval      date = tranDate
     C                   endsl
    C*
     C                   else
    C*
    C*  Use the posting date for Estimating accounts, except when the
    C*   transaction date is greater than the posting date.  This will
    C*   occur when a Self-Reporting accoung changes to an Estimating
    C*   account and had a bill posted for the prior month as a Self-
    C*   Reporting account.  This can also occur if the user keys a late
    C*   payment with a posting date for a prior date.
    C*
     C                   select
     C                   when      tranDate > postDate
     C                              or type = 'P' or type = 'I'
     C                              or type = 'Q' or type = 'R'
     C                              or type = 'N'
      *** beg add ***** 08/23/13 **********************************************
      *** beg del ***** 10/11/13 **********************************************
     C*****************             or type = 'B'
      *** end del ***** 10/11/13 **********************************************
      *** end add ***** 08/23/13 **********************************************
     C                   eval      date = tranDate
     C                   other
     C                   eval      date = postDate
     C                   endsl
    C*
     C                   endif
     C*
     C                   return    date
     C*
     PGetARDate        e





    P*====================================================================
    P* GetARDesc - Get the A/R description for a specific A/R code
    P*
    P*  GetARDesc - Returns the A/R code descriptio0n
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     PGetARDesc        b                   export
    P*
     DGetARDesc        PI            30
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const

     D company         s                   like(TTCO#)
     D key             s                   like(TTKEY)

     C     ttky          klist
     C                   kfld                    company
     C                   kfld                    key

     C                   eval      company = UserCo#(q1user)
     C                   eval      key     = code
     C                   eval      TTDESC  = *blank
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     ttky          chain     ptt01                              60
     C*
     C                   if        %parms >= 2
     C                   eval      TT_rrn = tbrrn
     C                   endif
     C*
     C                   return    TTDESC

     PGetARDesc        e





    P*====================================================================
    P* GetARType - Get the A/R type for a specific A/R code
    P*
    P*  GetARType - Returns the A/R type
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     PGetARType        b                   export
    P*
     DGetARType        PI             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const

     D company         s                   like(TTCO#)
     D key             s                   like(TTKEY)

     C     ttky          klist
     C                   kfld                    company
     C                   kfld                    key

     C                   eval      company = UserCo#(q1user)
     C                   eval      key     = code
     C                   eval      TTTYPE  = *blank
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     ttky          chain     ptt01                              60
     C*
     C                   if        %parms >= 2
     C                   eval      TT_rrn = tbrrn
     C                   endif
     C*
     C                   return    TTTYPE

     PGetARType        e





    P*====================================================================
    P* GetCatDesc - Get the A/R category description
    P*
    P*  GetCatDesc - Returns the A/R category description
    P*   co#       - accepts the company #     (required)
    P*   category  - passed in for validation  (required)
    P*   CT_rrn    - returns the RRN of WTCTP
    P*====================================================================
    P*
     PGetCatDesc       b                   export
    P*
     DGetCatDesc       PI            30
     D co#                            3  0 const
     D category                       1    const
     D CT_rrn                         9  0       options(*nopass)

     D desc            s             30

     C     ctky          klist
     C                   kfld                    co#
     C                   kfld                    category

     C                   eval      CTDESC  = *blank
     C*
     C     ctky          chain     pct00                              60
     C*
     C                   if        %parms >= 3
     C                   eval      CT_rrn = ctrrn
     C                   endif
     C*
     C                   eval      desc = CTDESC
     C*
     C                   return    desc

     PGetCatDesc       e
    P*====================================================================
    P* PayComm - Detemine if the code will pay commission
    P*
    P*  PayComm - Returns *on if code will pay commission, otherwise *off
    P*   co#    - accepts the company #     (required)
    P*   code   - passed in for validation  (required)
    P*   TT_rrn - returns the RRN of WTTTP
    P*====================================================================
    P*
     PPayComm          b                   export
    P*
     DPayComm          PI             1
     D co#                            3  0 const
     D code                           2    const
     D TT_rrn                         9  0       options(*nopass)

     D pay             s                   like(PayComm)
     D key             s                   like(TTKEY)

     C     ttky          klist
     C                   kfld                    co#
     C                   kfld                    key

     C                   eval      key = code
     C*
     C     ttky          chain     ptt01                              60
     C*
     C                   eval      pay = not *in60 and TTECFL <> 'Y' and
     C                                   (TTTYPE = 'P' or TTTYPE = 'I' or
     C                                    TTTYPE = 'N' or TTTYPE = 'V' or
     C                                    TTTYPE = 'R')
     C*
     C                   if        %parms >= 3
     C                   eval      TT_rrn = tbrrn
     C                   endif
     C*
     C                   return    pay

     PPayComm          e





    P*====================================================================
    P* PayType - Check for type as a payment type
    P*
    P*  PayType - Returns *on if A/R type is a payment type
    P*   type   - passed in for checking  (required)
    P*====================================================================
     PPayType          b                   export
     DPayType          pi             1n
     D inType                         1    const

     C*
     C                   return    not BillType(inType)

     PPayType          e





    P*====================================================================
    P* ValidCat - Valid category
    P*
    P*  ValidCat  - Returns *on if category valid
    P*   categroy - passed in for validation  (required)
    P*   CT_rrn   - returns the RRN of WTCTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     PValidCat         b                   export
    P*
     DValidCat         PI             1
     D category                       1                     const
     D CT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const

     D company         s                   like(CTCO#)

     C     ctky          klist
     C                   kfld                    company
     C                   kfld                    category

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     ctky          chain     pct00                              60
     C*
     C                   if        %parms >= 2
     C                   eval      CT_rrn = ctrrn
     C                   endif
     C*
     C                   return    not *in60

     PValidCat         e





    P*====================================================================
    P* ValidCode - Valid A/R code
    P*
    P*  ValidCode - Returns *on if A/R code valid
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     PValidCode        b                   export
    P*
     DValidCode        PI             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const

     D company         s                   like(TTCO#)
     D key             s                   like(TTKEY)

     C     ttky          klist
     C                   kfld                    company
     C                   kfld                    key

     C                   eval      company = UserCo#(q1user)
     C                   eval      key     = code
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     ttky          chain     ptt01                              60
     C*
     C                   if        %parms >= 2
     C                   eval      TT_rrn = tbrrn
     C                   endif
     C*
     C                   if        TTSTAT = 'I'
     C                   return    *off
     C                   endif
     C*
     C                   return    not *in60

     PValidCode        e





      *** beg add ***** 01/28/13 **********************************************
    P*====================================================================
    P* ARHistBal - A/R Historical Balance
    P*
    P*  ARHistBal - Returns the RRN of WDELP
    P*   inkc     - returns *on if F3 was pressed from search  (required)
    P*   inkl     - returns *on if F12 was pressed from search (required)
    P*====================================================================
    P*
     PARHistBal        b                    export
    
     dARHistBal        PI             9  0
     d inCall                         1
     d inKC                           1
     d inKL                           1
     d inRRN                          9  0

     dW0612R5          PR                  extpgm('W0612R5')
     d ##Call                         1    const
     d ##KC                           1
     d ##KL                           1
     d ##RRN                          9  0

     d ##Call          s              1
     d ##KC            s              1
     d ##KL            s              1
     d ##RRN           s              9  0
     d RRN             s              9  0

      /FREE

       ##Call = inCall;
       ##RRN  = inRRN;

       W0612R5(##Call:inKC:inKL:##RRN);

       // Retrieve the RRN of file WDELP
       RRN = ##RRN;
       return RRN;

      /END-FREE

     PARHistBal        e
    P*====================================================================
      *** end add ***** 01/28/13 ***************************
      //*** beg add ***** 09/28/20 *************************
      //====================================================
      // GetLast3rdPartySeq#
      // Parms: inType - Determines how to parse key
      //        inKey  - Stores info associated with type
      //                 appended by seq#
      //====================================================
       Dcl-Proc GetLast3rdPartySeq# Export;
       Dcl-Pi *N Packed(10:0);
        inKey Char(20) Const;
       End-Pi;
       Open WDSEQP;
       Chain(n) (inKey) pseq0p;
       Close WDSEQP;
       If %Found(WDSEQP);
          Return SElastSeq#;
       Else;
          Return *Zero;
       Endif;
       End-Proc GetLast3rdPartySeq#;
      //====================================================
      // GetNext3rdPartySeq#
      // Parms: inType - Determines how to parse key
      //        inKey  - Stores info associated with type
      //====================================================
       Dcl-Proc GetNext3rdPartySeq# Export;
       Dcl-Pi *N Packed(10:0);
        inKey Char(20) Const;
       End-Pi;
       Open WDSEQP;
       Chain (inKey) pseq0p;
       If %Found;
          SElastSeq# += 1;
          Update pseq0p;
          FEOD WDSEQP;
       Else;
          SEkey = inKey;
          SElastSeq# = 1;
          Write pseq0p;
          FEOD WDSEQP;
       Endif;
       Close WDSEQP;
       Return SElastSeq#;
       End-Proc GetNext3rdPartySeq#;
      //=====================================================
      *** end add ***** 09/28/20 **********************************************
     P*** Begin Add *** 03/22/21 ******************************************
    P*====================================================================
    P* Return the balance for a policy based upon the posting dates passed
    P*
    P*  TransARToLive   - Returns nothing
     P*   fileName       - Incoming file name        (required)
     P*   rrn            - WDEAP_HLD rrn             (required)
    P*====================================================================
     PTransferARToLive...
     P                 b                   export
     DTransferARToLive...
     D                 pi
     D inFile                        10    const
     D inRRN                          9  0 const

     d arType          s              1
     d strEnv          s              4a
     d strHoldLib      s             10a   Varying
     d strLiveLib      s             10a   Varying
     d strSql          s           5000a   Varying
     d sysDate         s              7  0
     d sysTime         s              6  0
     d dsWDEAP       e ds                  ExtName(WDEAP) inz

      /Free

       // Get system date and time
       sysDate = DateTo7(systemDate());
       systime = TimeTo6(systemtime());
       // Get library names for the HOLD
       //  and LIVE libraries. HOLD may be
       //  Quote.
       strLiveLib = RtvQuoteLib('*LIV');
       RtvEnv(strEnv);
       If strEnv = '*QTE';
          strHoldLib = RtvQuoteLib('*QTE');
       Else;
          strHoldLib = strLiveLib;
       Endif;

       // Access A/R to be transferred
       Select;
       When inFile = 'WDEAP';
          strSql = 'Select * From ' +
           %Trim(strLiveLib) + '/WDEAP ' +
           'Where rrn(WDEAP) = ' + %EditC(inRRN:'3');
       When inFile = 'WDEAP_HLD';
          strSql = 'Select * From ' +
           %Trim(strHoldLib) + '/WDEAP_HLD ' +
           'Where rrn(WDEAP_HLD) = ' + %EditC(inRRN:'3');
          EAbat# = 0;
          EApoDt = sysDate;
          EAtrDt = sysDate;
       EndSl;

       Exec Sql Prepare AR1 From :strSql;
       Exec SQL Declare cursorAR1 Cursor For AR1;
       Exec SQL Open cursorAR1;
       Exec SQL Fetch Next From cursorAR1 Into :dsWDEAP;
       If isValidFetch(sqlStt) = *Off;
          Exec SQL Close cursorAR1;
          Return;
       Endif;
       Exec SQL Close cursorAR1;

       // Get the A/R Type
       arType = GetArType(EAarCd);
       //*** begin add *** 10/31/22 ***************************************
       If EAtrdt = 0;
          EAtrDt = sysDate;
       Endif;
       //*** end add ***** 10/31/22 ***************************************

       // Write to WMEAP
       strSql = 'Insert Into ' +
        %Trim(strLiveLib) + '/WMEAP ' +
        'Values(' +
        %Editc(EAco#:'3') + ', ' +
        %Editc(EAfnd:'3') + ', ' +
        %Editc(EAfyr:'3') + ', ' +
        %Editc(EAemp#:'3') + ', ' +
        %Editc(EAdiv:'3') + ', ' +
        '''' + EAarCd + ''', ' +
        '''' + arType + ''', ' +
        '''' + EAdesc + ''', ' +
        %Editc(EAamt:'Q') + ', ' +
        %Editc(EAtrdt:'3') + ', ' +
        %Editc(EApodt:'3') + ', ' +
        ''' '', ' +
        ''' '', ' +
        '''' + Q1user + ''', ' +
        '''' + Q1pgm + ''', ' +
        %Editc(sysTime:'3') + ', ' +
        %Editc(sysDate:'3') + ', ' +
        %Editc(EAbat#:'3') + ', ' +
        %Editc(EAchk#:'3') + ', ' +
        %Editc(EAinv#:'3') + ', ' +
        '''' + EAlate + ''', ' +
        '''' + EAfl01 + ''', ' +
        ''' '', ' +
        '0, ' +
        '0, ' +
        ''' '', ' +
        ''' '', ' +
        '0, ' +
        ''' '', ' +
        '''' + EActyp + ''', ' +
        %Editc(EArdiv:'3') + ', ' +
        %Editc(EAcolEfdt:'3') + ', ' +
        %Editc(EAcolExdt:'3') + ', ' +
        %Editc(EAcolPerc:'3') + ', ' +
        %Editc(EAsrRptDt:'3') + ', ' +
        //*** Begin Chg *** 10/10/22 ***************************************
        //*** %Editc(EAsrManPr:'3') +
        %Editc(EAsrManPr:'3') + ', ' +
        //*** begin del *** 10/31/22 ***************************************
        //*****%Trim(EAtransID) + ', ' +
        //*** end del ***** 10/31/22 ***************************************
        %Editc(EAInvTypID:'3') + ', ' +
        //*** begin chg *** 10/31/22 ***************************************
        //*****%Trim(EAref#) +
        '''' + EARef# + '''' +
        //*** end chg ***** 10/31/22 ***************************************
        //*** End   Chg *** 10/10/22 ***************************************
        ')';
       Exec Sql Execute Immediate :strSql;

       // Delete batch record
       Select;
       When inFile = 'WDEAP';
         // Delete from A/R Batch
          strSql = 'Delete From ' +
           %Trim(strLiveLib) + '/WDEAP ' +
           'Where rrn(WDEAP) = ' + %Editc(inRRN:'3');
          Exec Sql Execute Immediate :strSql;
         // Update A/R Batch total for those records that are left
          strSql = 'Update ' +
           %Trim(strLiveLib) + '/WDEAP ' +
           'Set EAbata = (Select Sum(EAamt) From ' +
           %Trim(strLiveLib) + '/WDEAP ' +
           'Where EAbat# = ' + %Editc(EAbat#:'3') + ') ' +
           'Where EAbat# = ' + %Editc(EAbat#:'3');
          Exec Sql Execute Immediate :strSql;
       When inFile = 'WDEAP_HLD';
          strSql = 'Delete From ' +
           %Trim(strHoldLib) + '/WDEAP_HLD ' +
           'Where rrn(WDEAP_HLD) = ' + %Editc(inRRN:'3');
          Exec Sql Execute Immediate :strSql;
       EndSl;

      /End-free

     PTransferARToLive...
     P                 e
     P*** End   Add *** 03/22/21 ******************************************
