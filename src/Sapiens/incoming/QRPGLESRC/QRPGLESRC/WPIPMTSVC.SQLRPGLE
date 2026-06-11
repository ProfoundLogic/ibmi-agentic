     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     h/title WPIPMTSVC - Service program for Payment Services
     h nomain
     h DFTNAME(WPIPMTSVC)
     h option(*nodebugio: *srcstmt)
     h BndDir('YAJL') DecEdit('0.')
      **********************************************************************
      *  Description:  This will be used to for 3rd Party Payment Processing
      *
      *  Created: 03/11/21  C:33/62  Project: WC2663
      **********************************************************************
      * Revisions:
      *
      * 08/28/25 C:062 Project 153WBE
      *                -Write wipsp record when submitting to IVC
      *
      * 07/18/25 C:062 Project 153WBE
      *                Pass Carrier Policy # for Vital.
      *
      * 10/07/24 C:062 Ticket: 117690(129)
      *                Send with active equals on.
      *
      * 09/27/24 C:062 Ticket: 117721(133)
      *                Remove WIIHP records for all previous entries
      *                                                                                            *
      * 06/24/24 C:079 Project: 146WKR
      *                Collateral Processing - Recomile
      *
      * 01/05/24 C:062 Ticket: 114720(129)
      *                -Only delete invoice if divisions are the same.
      *
      * 10/25/23 C:340 Projecr: WC2922
      *                Add Procedures to Inactivate INV on Contested Audit.
      *
      * 10/25/23 C:062 Projecr: WC2663
      *                Change union to union all.
      *
      * 09/07/23 C:062 Projecr: WC2663
      *                Pass an account# without the company#.
      *
      * 08/28/23 C:291 Projecr: WC2920
      *                Add Procedure ICPaperlessOpt that will return
      *                Paperless start date.
      *
      * 07/07/23 C:062 Project: WC2916
      *                -Add procedure to return if the IVC Sweep has run
      *                 the sweep.
      *                -Add Sweep Tag
      *
      * 04/21/23 C:079 Support: 129/108863
      *                Invoice Cloud - Get BIF Customer Name/Address from
      *                the Mailing Address insted of Physical Address.
      *
      * 03/20/23 C:062 Support: 107691(129)
      *                Access the state reporting division.
      *
      * 11/15/22 C:079 Support: 129/104269
      *                Invoice Cloud - Get BIF Customer Name/Address from
      *                Employer file instead of Company file.
      *
      * 04/11/22 C:340 Project: WC2663
      *                Invoice Cloud - Add Invoice Type ID
      *
      **********************************************************************
     D psds          esds                  extname(SZQ1P)

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz

     d key             s             50a   varying
     d key1            s             50a   varying
     d key2            s             50a   varying
     d key3            s             50a   varying

     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)

     d docNode         s                   Like(yajl_val)
     d list            s                   Like(yajl_val)
     d val             s                   Like(yajl_val)
     d errMsg          s          99999a   Varying Inz(' ')
     d errMsg500       s            500a   Varying Inz(' ')

     d active          s              5
     d company#        s              3  0
     d division        s              5  0
     d employer#       s              9  0
     d group#          s              3  0
     d invoice#        s             10  0
     d orderBy         s            100a
        //***  Begin Chg  ***  04/12/22  ***************
     d*sqlStmtStr      s           1000a
     d*sqlStmtStr1     s            500a
     d*sqlStmtStr2     s            500a
     d sqlStmtStr      s           1000a   varying
     d sqlStmtStr1     s            500a   varying
     d sqlStmtStr2     s            500a   varying
        //***  End   Chg  ***  04/12/22  ***************
     d invoiceCount    s              9  0 Inz
     d firstInvHdr     s               n   Inz(*On)
     d totalPaid       s             11s 2 Inz

     d arrInvoice      s             30a   dim(9999)

     dold              ds                  qualified inz
     d company                        3
     d group                          3
     d employer                       9

     dnew              ds                  qualified inz
     d company                        3
     d group                          3
     d employer                       9
     d blockCreditCards...
     d                                5
     d blockEFT                       5
     d active                         5
     d updateInvoiceNumbers...
     d                                5

     d*** Begin Add *** 07/07/23 *******************************************
     d LodWC2663       ds                  DTAARA(LODWC2663)
     d   sweepID                     40a
     d*** End   Add *** 07/07/23 *******************************************

     d/copy SiWebSvcPR
     d/copy sPrDate
     d/copy sPrModule
     d/copy sPrSql
     d/copy wPrAr
     d/copy wPrEmplr
     d/Include yajl_h
        // *** Beg Add *** 04/11/22 ***************************************
     d/copy wPrBank
        // *** End Add *** 04/11/22 ***************************************

     d* Determine if Quote
     d RetrieveEnv     pr                  ExtPgm('RTVENV')
     d   env                          4

     d*** Begin Add *** 07/18/25 ****************************************
     d* Determine if Quote
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outClientID                  3
     d*** End   Add *** 07/18/25 ****************************************

     D PaymentPgm      pr                  ExtPgm('WIPOLPMT')
     D inRequest                    256
     D inUser                        10
     D inParms                         a   len(8500000) varying
     D outJSON                         a   len(8500000) varying
     D ioRtnCode                     10a

      //*** Beg Chg *** 10/25/23 *** WC2922 **************************
     D DeleteInvoice   pr                  ExtPgm('DELETEINV')
     D inCo#                          3a
     D inFnd                          3a
     D inEmp#                         9a
     D inDiv                          5a
     D inInv#                        10a
      //*** End Chg *** 10/25/23 *** WC2922 **************************

     d jsonIC          s                   Sqltype(clob:64000) Inz
     d chrJson         s          64000a   Varying Inz
     d chrJsonIC       s          64000a   Varying Inz
     d totalInvAmount  s             11s 2
     d env             s              4a

     D inRequest       s            256a
     D inUser          s             10a
     D inParms         s               a   len(8500000) varying
     D outJSON         s               a   len(8500000) varying
     D ioRtnCode       s             10a
     D holdSqlSTT      s                   Like(sqlSTT)

     d account#        s             25a
     d*** Begin Add *** 09/07/23 ******************************************
     d account#2       s             25a
     d*** End   Add *** 09/07/23 ******************************************
        // *** Beg Add *** 04/11/22 ***************************************
     d decToday        S              8S 0
     d chrBnkAcc       S             20A
     d*** Begin Add *** 07/18/25 ****************************************
     d clientID        s              3    Inz(' ')
     d*** End   Add *** 07/18/25 ****************************************
     d decInvTypId     S             15S 0
     d
     d @@co            S              3
     d @@fnd           S              3
     d @@appl          S              2
     d @@srdt          S              8
     d @@call          S              1
     d @@hdcy          S              1
     d @@chkq          S             10
     d @@ftyp          S             10
     d @@efdt          S              7
     d @@cpyq          S             10
     d @@cktp          S             10
     d @@eftc          S              1
     d @@coid          S             20
     d @@rtcd          S              1
        // *** End Add *** 04/11/22 ***************************************
       //===================================================================
       // Never processed. Only used for setting COMMIT
       //===================================================================
     pAAA              b
      /Free
       Exec SQL
        set option commit = *none;
      /End-free
     pAAA              e

     p*** Begin Add *** 07/28/25 *****************************************
     p*===================================================================
     pCreateWIPSP      b
     dCreateWIPSP      pi
     d inCompany#                     3  0
     d inGroup#                       3  0
     d inEmployer#                    9  0
      *
     d decCount        s              5  0 Inz(0)
     d holdStt         s                   Like(sqlStt)
     d sysDate         s              7  0
     d sysTime         s              6  0

      /Free

       holdStt = sqlStt;
       sysDate = DateTo7(systemDate());
       sysTime = TimeTo6(systemTime());

       Exec Sql
          Select count(*)
          Into :decCount
          From wipsp
          Where PSco#=:inCompany# And PSfnd=:inGroup# And
                PSemp#=:inEmployer#;

       If Not IsValidFetch(sqlStt);
          Exec Sql Insert into WIPSP
                   Values(inCo#,inGroup#,inEmp#,'InvoiceCloud','O',
                          0, 0, timeStamp, timeStamp, 'A', Q1user,
                          Q1pgm, sysTime, sysDate, Q1user, Q1pgm,
                          sysTime, sysDate);
       EndIf;

       sqlStt = holdStt;

      /End-free

     pCreateWIPSP      e
     p*** End   Add *** 07/18/25 *****************************************
     p*===================================================================
       //===================================================================
     pExcludeAR        b
     dExcludeAR        pi             1n
     d inCo#                          3  0 Const
     d inArCode                       2    Const
      *
     d excludeIC       s              1    Inz(' ')
     d holdStt         s                   Like(sqlStt)

      /Free

       holdStt = sqlStt;
       Exec Sql Select TTexIC Into :excludeIC
                From WTTTP
                Where TTco#=:inCo# And TTkey=:inArCode;
       sqlStt  = holdStt;

       If excludeIC = 'Y';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-free

     pExcludeAR        e
     p*===================================================================
     pFinalizeInvoicingIC...
     p                 b
     dFinalizeInvoicingIC...
     d                 pi

      /Free

       // Load the IC Json Header
       InvoiceCloudBIFHeader();

      /End-free

     pFinalizeInvoicingIC...
     p                 e

       //===================================================================
     pFinalizeThirdPartyInvoicing...
     p                 b                   export
     dFinalizeThirdPartyInvoicing...
     d                 pi             1n
     d inCo#                          3  0 Const
     d inGroup                        3  0 Const
      *
     d isInvCloud      s               n   Inz
     d groupIncluded   s               n   Inz

      /Free

       // Let's see if Invoice Cloud is Installed
       isInvCloud    = isModEnabled('Invoice Cloud');

       // Let's get the Invoice Cloud Header
       groupIncluded = ICGroupOpt(inCo#:inGroup);

       // Leave if there are no invoices
       If invoiceCount = 0;
          Clear arrInvoice;
          Clear chrJsonIC;
          Clear totalPaid;
          invoiceCount = 0;
          Return *Off;
       EndIf;

       If isInvCloud And groupIncluded;
          FinalizeInvoicingIC();
          LoadSIWEBSVCDataQ(jsonIC);
       EndIf;

       Return *on;
      /End-free

     pFinalizeThirdPartyInvoicing...
     p                 e

     p*** Begin Add *** 07/18/25 *****************************************
     p*===================================================================
     pGetCarrierPolicy#...
     p                 b
     dGetCarrierPolicy#...
     d                 pi            25
     d inCompany#                     3  0
     d inGroup#                       3  0
     d inEmployer#                    9  0
     d inDivision                     5  0
      *
     d carrierPol#     s             25    Inz(' ')
     d holdStt         s                   Like(sqlStt)

      /Free

       holdStt = sqlStt;

       Exec Sql
          Select Max(RPcap#)
          Into :carrierPol#
          From wmrpp
          Where RPco#=:inCompany# And RPfnd=:inGroup# And
                RPemp#=:inEmployer# And RPdiv=:inDivision;

       sqlStt = holdStt;

       Select;
       When %Subst(carrierPol#:12:1) = ' ' Or
            %Subst(carrierPol#:12:1) = '-';
          Return %Subst(carrierPol#:1:11);
       When %Subst(carrierPol#:13:1) = ' ' Or
            %Subst(carrierPol#:13:1) = '-';
          Return %Subst(carrierPol#:1:12);
       Other;
          Return ' ';
       EndSL;

      /End-free

     pGetCarrierPolicy#...
     p                 e
     p*** End   Add *** 07/18/25 *****************************************

       //===================================================================
     pICGroupOpt...
     p                 b                   export
     dICGroupOpt...
     d                 pi             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const

     ddsWMEXP        e ds                  extname(WMEXP) qualified inz
      *
      /Free
       Exec sql
        select * into :dsWMEXP
         from WMEXP
         where EXco# = :inCo# and EXfnd = :inGroup;
       Return dsWMEXP.EXicGrp = 'Y';
      /End-free
     pICGroupOpt...
     p                 e

       //*** Begin Add *** 07/07/23 ****************************************
       //===================================================================
     pICGroupRan...
     p                 b                   export
     dICGroupRan...
     d                 pi             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const

     ddsWMEXP        e ds                  extname(WMEXP) qualified inz
      *
      /Free
       Exec sql
        select * into :dsWMEXP
         from WMEXP
         where EXco# = :inCo# and EXfnd = :inGroup;
       Return dsWMEXP.EXswpRan = 'Y';
      /End-free
     pICGroupRan...
     p                 e
       //*** End   Add *** 07/07/23 ****************************************
       //*** Beg Add ***** 08/28/23 ****************************************
       //===================================================================
     pICPaperlessOpt   b                   export
     dICPaperlessOpt   pi             7  0
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
      *
     ddsWIPSP        e ds                  extname(WIPSP) qualified inz
      *
      /Free
       Exec sql
        Select * into :dsWIPSP
         From WIPSP
         Where PSco# = :inCo# and PSfnd = :inGroup
          and PSemp# = :inEmp#;

        Return dsWIPSP.PSPaper;
      /End-free
     pICPaperlessOpt   e

       //*** End Add ***** 08/28/23 ****************************************

       //===================================================================
     pICPolicyOpt...
     p                 b                   export
     dICPolicyOpt...
     d                 pi             1
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
      *
     ddsWIPSP        e ds                  extname(WIPSP) qualified inz
     ddsWIPSPHIST    e ds                  extname(WIPSPHIST) qualified inz
      *
      /Free
       Exec sql
        select * into :dsWIPSP
         from WIPSP
         where PSco# = :inCo# and PSfnd = :inGroup
          and PSemp# = :inEmp#;
       Exec sql
        select * into :dsWIPSPHIST
         from WIPSPHIST
         where PSco# = :inCo# and PSfnd = :inGroup
          and PSemp# = :inEmp#
          and PSfield = 'PSOPTFLAG'
          order by PStrDt desc, PStrTm desc
          limit 1;
       If dsWIPSP.PSoptFlag = 'I';
          Return 'Y';
       Else;
          Return ' ';
       Endif;
      /End-free
     pICPolicyOpt...
     p                 e

       //===================================================================
     pICPolicyNoOutput...
     p                 b                   export
     dICPolicyNoOutput...
     d                 pi             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
      *
      /Free
       Return ICPolicyOpt(inCo#:inGroup:inEmp#) = 'I';
      /End-free
     pICPolicyNoOutput...
     p                 e

       //===================================================================
     pICQuoteOpt...
     p                 b                   export
     dICQuoteOpt...
     d                 pi             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const

     ddsWMEXP        e ds                  extname(WMEXP) qualified inz

      /Free
       Exec sql
        select * into :dsWMEXP
         from WMEXP
         where EXco# = :inCo# and EXfnd = :inGroup;
       Return dsWMEXP.EXicQte = 'Y';
      /End-free

     pICQuoteOpt...
     p                 e

       //===================================================================
     pInvoiceCloudBIFInvoicingDetail...
     p                 b
     dInvoiceCloudBIFInvoicingDetail...
     d                 pi             1n
     d inJson                     32000    const varying

     d detailWritten   s               n   Inz(*Off)
     d invoiceDetail   s           1000a   dim(999)
     d invoiceHeader   s           1000a   varying inz
     d formatGuid      s             50a
     d billingType     s             30a
     d i               s              5s 0
     d installment#    s              5s 0
     d invoice         s             30a
     d saveDiv         s                   Like(dsWMEMP.EMdiv) Inz(0)
     d firstInvDtl     s               n   Inz(*On)
     d invDtlJsonIC    s          64000a   Inz Varying
     d strBatchSysCd   s              2a
     d totalDtlPaid    s             11s 2
     d totalInvPaid    s             11s 2

     d chrArCode       s              2    Inz
     d chrArDesc       s             30    Inz
     d chrArCat        s              1    Inz

     d EMco#           s                   Like(dsWMEMP.EMco#)
     d EMfnd           s                   Like(dsWMEMP.EMfnd)
     d EMfyr           s                   Like(dsAr.EAfyr)
     d EMemp#          s                   Like(dsWMEMP.EMemp#)
     d EMdiv           s                   Like(dsWMEMP.EMdiv)
     d EAinv#          s                   Like(dsAr.EAinv#)
     d EAtrDt          s                   Like(dsAR.EAtrDt)
     d EApoDt          s                   Like(dsAR.EApoDt)

     d sysDate         s              7  0
     d sysTime         s              6  0

     ddsIn             ds                  qualified inz
     d type                          20
     d invoice                       10
     d company                        3
     d group                          3
     d employer                       9
     d division                       5
     d amount                        12
     d active                         5
     d delete                         5

     d chrTranDate     s             10
     d chrPostDate     s             10
     d creditCount     s              5  0
     d decAmountIC     s             11  2
     d deletedInvoice  s               n   Inz(*Off)
     d foundInvDtl     s               n   Inz(*Off)
     d chrInvoice      s             30

     d dsAR            ds                  qualified inz
     d  EAco#                         3  0
     d  EAfnd                         3  0
     d  EAfyr                         3  0
     d  EAemp#                        9  0
     d  EAdiv                         5  0
     d  EAinv#                       10  0
     d  EAarCd                        2
     d  EAamt                         9  2
     d  EAtrDt                        7  0
     d  EApoDt                        7  0
     d*** Begin Add *** 10/25/23 *******************************************
     d  EArrn                         9  0
     d*** End   Add *** 10/25/23 *******************************************

     d dsWMEMP       e ds                  extname(WMEMP)    qualified inz

      /Free
       ExSr GetParms;

       clear invoiceDetail;
       clear invDtlJsonIC;
       ExSr ProcessAR;
       If detailWritten;
          ExSr ProcessInvoiceHeader;
       EndIf;

       Return *On;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Load date and time and initialize fields
          sysDate        = DateTo7(systemDate());
          sysTime        = TimeTo6(systemTime());
          dsIn.active    = 'true';
          dsIn.delete    = 'false';
          deletedInvoice = *Off;
          decAmountIC    = 0;

          // Load the json tree
          node0 = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                Select;
                When key = 'type';
                   dsIn.type = YAJL_get_string(val);
                   billingType = dsIn.type;
                When key = 'invoice';
                   dsIn.invoice = YAJL_get_string(val);
                When key = 'company';
                   dsIn.company = YAJL_get_string(val);
                When key = 'group';
                   dsIn.group = YAJL_get_string(val);
                When key = 'employer';
                   dsIn.employer = YAJL_get_string(val);
                When key = 'division';
                   dsIn.division = YAJL_get_string(val);
                When key = 'amount';
                   dsIn.amount = YAJL_get_string(val);
                   If dsIn.amount <> ' ';
                      Monitor;
                         decAmountIC = %Dec(%Trim(dsIn.amount):11:2);
                      On-Error;
                         decAmountIC = 0;
                      EndMon;
                   EndIf;
                When key = 'active';
                   dsIn.active = YAJL_get_string(val);
                When key = 'delete';
                   dsIn.delete = YAJL_get_string(val);
                   If dsIn.delete = 'true';
                      deletedInvoice = *On;
                   EndIf;
                Endsl;
             Enddo;
          Endif;

          // Store invoice to make sure duplicates aren't sent
          chrInvoice = dsIn.company + dsIn.group + dsIn.employer +
                       dsIn.division + dsIn.invoice;

          i = %LookUp(chrInvoice:arrInvoice);
          If i = 0;
             i = %LookUp(' ':arrInvoice);
             arrInvoice(i) = chrInvoice;
             SortA arrInvoice;
          Else;
             // Leave this invoice was already processed
             Return *On;
          EndIf;
       Endsr;


       //===================================================================
       // ProcessAR - Process A/R to create invoice detail rcd
       //===================================================================
       BegSr ProcessAR;

          // Initialize the counter and total invoice
          i = 0;
          totalInvPaid = 0;
          totalDtlPaid = 0;
          foundInvDtl  = *Off;
          EMco#        = %Dec(dsIn.company:3:0);
          EMfnd        = %Dec(dsIn.group:3:0);

          // Are we in quote?
          RetrieveEnv(env);

          // Get Batch System Code
          Exec Sql
            Select SYkey
            Into :strBatchSysCd
            From WTSYP
            Where SYstat <> 'I'
             And SYtype = 'Y'
            Order by SYkey
            Limit 1;

          // Get A/R Category and A/R Code(Quote)
          Select;
          When billingType = 'retro';
             chrArCat  = 'C';
             chrArCode = ' ';
             chrArDesc = ' ';
          When billingType = 'deductible';
             chrArCat  = 'R';
             chrArCode = ' ';
             chrArDesc = ' ';
          When billingType = 'billing';
             chrArCat  = ' ';
             chrArCode = ' ';
             chrArDesc = ' ';
          When billingType = 'quote billing';
             // Get the New Business A/R Code
             Exec Sql Select F2NpCd Into :chrArCode From WDF2P
               Where F2co#=:EMco# And F2fnd=:EMfnd;
             // Get A/R Description
             Exec Sql Select TTdesc Into :ChrArDesc From WTTTP
               Where TTco# = :EMco# And TTkey = :chrArCode;
             chrArCat = 'P';
          EndSl;

          // Sort Order
          orderBy = 'Order by EAco#, EAfnd, EAdiv, EAinv#';

          // Build based upon environment
        //***  Begin Chg  ***  04/12/22  ***************
        //If env = '*QTE';
        //   // WDEAP_HLD
        //   sqlStmtStr1 = 'Select EAco#, EAfnd, EAfyr, EAemp#, EAdiv, ' +
        //     'EAinv#, EAarCd, EAamt, EAtrDt, EApoDt ' +
        //     'From WDEAP_HLD Where EAco#=' +
        //     dsIn.company + ' And EAfnd=' + dsIn.group +
        //     ' And EAemp#=' + dsIn.employer + ' And EAinv#=' +
        //     dsIn.invoice + ' And EAarCd <> ''' + chrArCode + '''';
        //Else;
        //   // WDEAP
        //   sqlStmtStr1 = 'Select EAco#, EAfnd, EAfyr, EAemp#, EAdiv, ' +
        //     'EAinv#, EAarCd, EAamt, EAtrDt, EApoDt ' +
        //     'From WDEAP Where EAco#=' +
        //     dsIn.company + ' And EAfnd=' + dsIn.group +
        //     ' And EAemp#=' + dsIn.employer +
        //     ' And EAdiv='  + dsIn.division + ' And EAinv#=' +
        //     dsIn.invoice;
        //EndIf;
          //*** Begin Del *** 10/25/23 *************************************
          //***sqlStmtStr1 = 'Select EAco#, EAfnd, EAfyr, EAemp#, EAdiv, ' +
          //*** 'EAinv#, EAarCd, EAamt, EAtrDt, EApoDt';
          //*** End   Del *** 10/25/23 *************************************
          Select;
          When env = '*QTE';
             //*** Begin Add *** 10/25/23 **********************************
             sqlStmtStr1 = 'Select EAco#, EAfnd, EAfyr, EAemp#, EAdiv, ' +
              'EAinv#, EAarCd, EAamt, EAtrDt, EApoDt, rrn(wdeap_hld) ';
             //*** End   Add *** 10/25/23 **********************************
             // WDEAP_HLD
             sqlStmtStr1 += ' From WDEAP_HLD';
          Other;
             // WDEAP
             //*** Begin Add *** 10/25/23 **********************************
             sqlStmtStr1 = 'Select EAco#, EAfnd, EAfyr, EAemp#, EAdiv, ' +
              'EAinv#, EAarCd, EAamt, EAtrDt, EApoDt, rrn(wdeap) ';
             //*** End   Add *** 10/25/23 **********************************
             sqlStmtStr1 += ' From WDEAP';
          Endsl;
          sqlStmtStr1 += ' join WTTTP on TTco#=EAco# and TTkey=EAARCD';
          sqlStmtStr1 += ' Where EAco#=' + dsIn.company +
           ' And EAfnd=' + dsIn.group +
           ' And EAemp#=' + dsIn.employer +
           ' And EAdiv='  + dsIn.division +
           ' And EAinv#=' + dsIn.invoice +
           ' And EAstat = '' ''';
          sqlStmtStr1 +=
           ' And TTtype in (''A'', ''B'', ''D'', ''F'', ''H'')';
        //***  End   Chg  ***  04/12/22  ***************

          // WMEAP
          //*** Begin Chg *** 10/25/23 *************************************
          //***sqlStmtStr2 = 'Select EAco#, EAfnd, EAfyr, EAemp#, EAdiv, ' +
          //***  'EAninv EAinv#, EAarCd, EAamt, EAtrDt, EApoDt ' +
          //***  'From WMEAP Where EAco#=' +
          //***  dsIn.company + ' And EAfnd=' + dsIn.group +
          //***  ' And EAemp#=' + dsIn.employer +
          //***  ' And EAdiv='  + dsIn.division + ' And EAninv=' +
          //***  dsIn.invoice +
          //***  ' And EAarTy in (''A'', ''B'', ''D'', ''F'', ''H'')';
          sqlStmtStr2 = 'Select EAco#, EAfnd, EAfyr, EAemp#, EAdiv, ' +
            'EAninv EAinv#, EAarCd, EAamt, EAtrDt, EApoDt, rrn(wmeap) ' +
            'From WMEAP Where EAco#=' +
            dsIn.company + ' And EAfnd=' + dsIn.group +
            ' And EAemp#=' + dsIn.employer +
            ' And EAdiv='  + dsIn.division + ' And EAninv=' +
            dsIn.invoice +
            ' And EAarTy in (''A'', ''B'', ''D'', ''F'', ''H'')';
          //*** End   Chg *** 10/25/23 *************************************
          // Build Statement
          //*** Begin Chg *** 10/25/23 *************************************
          //*** sqlStmtStr = %Trim(sqlStmtStr1) + ' Union ' +
          //***              %Trim(sqlStmtStr2) + ' ' + %Trim(orderBy);
          sqlStmtStr = %Trim(sqlStmtStr1) + ' Union All ' +
                       %Trim(sqlStmtStr2) + ' ' + %Trim(orderBy);
          //*** End   Chg *** 10/25/23 *************************************

          Exec Sql Prepare sqlAR From :sqlStmtStr;
          Exec SQL Declare cursorAR Cursor For sqlAR;
          Exec SQL Open cursorAR;

          Dou Not IsValidFetch(sqlStt);
             Exec SQL fetch next from cursorAR into :dsAR;
             If IsValidFetch(sqlStt);
                If Not ExcludeAR(dsAR.EAco#:dsAR.EAarCd);
                   // Maintain totals
                   EMco#         = dsAR.EAco#;
                   EMfnd         = dsAR.EAfnd;
                   EMfyr         = dsAR.EAfyr;
                   EMemp#        = dsAR.EAemp#;
                   EMdiv         = dsAR.EAdiv;
                   EAtrDt        = dsAR.EAtrDt;
                   EApoDt        = dsAR.EApoDt;
                   EAinv#        = dsAR.EAinv#;
                   If decAmountIC = 0;
                      totalDtlPaid += dsAR.EAamt;
                   EndIf;
                   foundInvDtl   = *On;
                   //*** Begin Add *** 08/28/25 ****************************
                   // Create 3rd Party PaymentService file
                   CreateWIPSP(EMco#:EMfnd:EMemp#);
                   //*** End   Add *** 08/28/25 ****************************
                EndIf;
             Else;
               // Process last Invoice
               If foundInvDtl Or deletedInvoice;
                   If decAmountIC <> 0;
                      totalDtlPaid += decAmountIC;
                   EndIf;
                  // Process Invoice Detail
                  ExSr ProcessInvoiceDetail;
               EndIf;
             EndIf;
          EndDo;
          Exec SQL Close cursorAR;

       EndSr;
       //===================================================================
       // ProcessInvoiceDetail - Generate invoice detail rcd
       //===================================================================
       BegSr ProcessInvoiceDetail;

          // Populate work fields if deleted invoice and no A/R
          If deletedInvoice;
             EMco#         = %Dec(dsIn.company:3:0);
             EMfnd         = %Dec(dsIn.group:3:0);
             EMfyr         = 0;
             EMemp#        = %Dec(dsIn.employer:9:0);
             EMdiv         = %Dec(dsIn.division:5:0);
             EAtrDt        = DateTo7(systemDate());
             EApoDt        = DateTo7(systemDate());
             EAinv#        = %Dec(dsIn.invoice:10:0);
             totalDtlPaid  = 1;
          EndIf;

          detailWritten = *Off;
          If totalDtlPaid > 0;

             // Get division name
             holdSqlSTT = sqlSTT;
             Exec Sql Select * Into :dsWMEMP
                      From wmemp
                      Where EMco#= :EMco# And EMfnd= :EMfnd And
                        EMemp# = :EMemp# And EMdiv= :EMdiv;

             // Create invoice detail
             i += 1;
             invoiceDetail(i) = '70~ID|' +
               '71~Division ' + %Trim(%EditC(EMdiv:'3')) + '|' +
               '72~' + %Trim(%ScanRpl('"':'':dsWMEMP.EMname)) + '|' +
               '73~' + %Trim(%EditC(totalDtlPaid:'3'));

             // Create Payment Holder if not a deleted invoice

             // Delete the hold if it already exists.
             Exec Sql Delete From WDEAP_HLD Where EAco#=:EMco# And
                      EAfnd=:EMfnd And EAemp#=:EMemp# And EAdiv=:EMdiv And
                      EAinv#=:EAinv# And EAarCd=:chrArCode;

             // Don't create payment hold record if invoice being deleted
             If not deletedInvoice;
                Exec Sql Insert Into WDEAP_HLD (EAco#, EAfnd, EAfYr, EAemp#,
                EAdiv, EAarCd, EAdesc, EApoDt, EAamt, EAuser, EApgm, EAtime,
                EAdt, EAinv#, EAtrDt, EAiUsr, EAiPgm, EAiDt, EAcTyp, EAsycd)
                Values(:EMco#, :EMfnd, :EMfyr, :EMemp#, :EMdiv, :chrArCode,
                  :chrArDesc, :EApoDt, :totalDtlPaid, :Q1user, :Q1pgm,
                  :sysTime, :sysDate, :EAinv#, :EAtrDt, :Q1user, :Q1pgm,
                  :sysDate, :chrArCat, :strBatchSysCd);
             EndIf;

             sqlSTT = holdSqlSTT;

             detailWritten = *On;
             totalPaid    += totalDtlPaid;
             totalInvPaid += totalDtlPaid;
             totalDtlPaid  = 0;
             saveDiv       = dsAR.EAdiv;
             invoiceCount += 1;
          EndIf;

       EndSr;

       //===================================================================
       // ProcessInvoiceHeader - Generate invoice heading and customer rcd
       //===================================================================
       BegSr ProcessInvoiceHeader;

          // Get the format guide
          formatGuid = GetModuleProperty('Invoice Cloud':'InvoiceGuidBilled');

          // Create Invoice Header
          Reset invoiceHeader;
          inVoiceHeader += '7~IH|';

          // Build full invoice #
          invoice =  dsIn.company + dsIn.group + dsIn.employer +
                     dsin.division + dsIn.invoice;

          // Convert Transaction date and posting date
          chrTranDate = %Char(cvtToDate(dtMDYY(EAtrDt)):*usa);
          chrPostDate = %Char(cvtToDate(dtMDYY(EApoDt)):*usa);

          //Populate installment #
          installment# = 0;
          If foundInvDtl;
             installment# = %Dec(cvtToDate(dtMDYY(EApoDt)):*jul);
          EndIf;

          Select;
          When dsIn.delete = 'true';
             invoiceHeader += '8~D|';
          Other;
             invoiceHeader += '8~R|';
          Endsl;

        // *** Beg Add *** 04/11/22 ***************************************
        // Get the Bank Acount

          @@co   = dsIn.company;
          @@fnd  = dsIn.group;
          @@appl = 'WP';
          @@srDt = %EditC(DateTo8(systemDate()):'X');
          @@call = *Blanks;
          @@hdcy = *Blanks;
          @@chkq = *Blanks;
          @@ftyp = *Blanks;
          @@efdt = *Zeros ;
          @@cpyq = *Zeros ;
          @@cktp = *Blanks;
          @@eftc = 'B' ;
          @@coid = *Blanks;
          @@rtcd = *Blanks;

          chrBnkAcc = GetBankAcc(@@co : @@fnd: @@appl: @@srdt: @@call:
                      @@hdcy: @@chkq: @@ftyp: @@efdt: @@cpyq:
                      @@cktp: @@eftc: @@coid: @@rtcd);

          decInvTypId = 0 ;
          Exec Sql
            Select CkInvTypId Into :decInvTypId
            From   WSCKP
            Where  CKCO# = :dsIn.company and CKACCT = :chrBnkAcc;

          If decInvTypId = 0 ;
             decInvTypId = 164;
          EndIf;

          ExSr AddInvTypID;

          invoiceHeader += '10~' + %Trim(invoice) + '|' +
                           '11~' + chrTranDate + '|' +
                           '12~' + chrPostDate + '|' +
                           '13~' + %Trim(%EditC(totalInvPaid:'3')) + '|' +
                           '20~0|' +
                           '21~' + %Trim(%EditC(totalInvPaid:'3')) + '|' +
                           '38~' + %Trim(billingType) + '|' +
                           '252~'+ %Trim(%EditC(decInvTypId:'3')) + '|';

          //***invoiceHeader += '10~' + %Trim(invoice) + '|' +
          //***                 '11~' + chrTranDate + '|' +
          //***                 '12~' + chrPostDate + '|' +
          //***                 '13~' + %Trim(%EditC(totalInvPaid:'3')) + '|' +
          //***                 '20~0|' +
          //***                 '21~' + %Trim(%EditC(totalInvPaid:'3')) + '|' +
          //***                 '38~' + %Trim(billingType) + '|' +
          //***                 '252~164|';

        // *** End Add *** 04/11/22 ***************************************

          // Send policy expiration date if the invoice is being inactivated
          If dsIn.active = 'false';
             invoiceHeader += '317~' + AlphaUSA(systemDate() - %days(1)) + '|';
          Else;
             invoiceHeader += '317~  |';
          EndIf;

          invoiceHeader += '319~' + %Trim(billingType) + '|' +
                           '345~' + %Trim(%EditC(installment#:'3')) + '|';
          //Select;
          //When dsIn.active = 'true';
          invoiceHeader += '375~1|';
          //Other;
          //   invoiceHeader += '375~0|';
          //Endsl;

          totalInvPaid = 0;

          invDtlJsonIC = ' ';
          i = 1;
          For i=1 to 999;
             If invoiceDetail(i) <> ' ';
                If i=1;
                   invDtlJsonIC = %Trim(invDtlJsonIc) + '"' +
                        %Trim(invoiceDetail(i)) + '"';
                Else;
                   invDtlJsonIC = %Trim(invDtlJsonIc) + ',"' +
                        %Trim(invoiceDetail(i)) + '"';
                EndIf;
             EndIf;
          EndFor;

          If firstInvHdr;
             chrJsonIC += '{' +
                 '"number":"' + %Trim(invoice) + '",' +
                 '"header":"' + %Trim(invoiceHeader) + '",' +
                 '"detail":['  + %Trim(invDtlJsonIC) + ']}';
             firstInvHdr  = *Off;
          Else;
             chrJsonIC += ',{' +
                 '"number":"' + %Trim(invoice) + '",' +
                 '"header":"' + %Trim(invoiceHeader) + '",' +
                 '"detail":['  +%Trim(invDtlJsonIC) + ']}';
          EndIf;

          invDtlJsonIC = ' ';

       EndSr;

       // *** Beg Add *** 04/11/22 ***************************************
       //===================================================================
       // AddInvTypID - Add Invoice Type ID to WDEAP & WMEAP
       //===================================================================
       BegSr AddInvTypID;

          // WDEAP_HLD
          Exec Sql
           Update WDEAP_HLD Set EAInvTypID = :decInvTypId
            Where EAco#  = :dsIn.company  and
             EAfnd  = :dsIn.group    and
             EAemp# = :dsIn.employer and
             EAdiv  = :dsIn.division and
             EAinv# = :dsIn.invoice and
             (:env = '*QTE' or
              EAARCD = '  ' or
              EAARCD in
              (select TTKEY from WTTTP
                where TTco#=EAco# and TTTYPE in ('A','B','D','F','H')));
          // WDEAP
          Exec Sql
           Update WDEAP Set EAInvTypID = :decInvTypId
            Where EAco#  = :dsIn.company  and
             EAfnd  = :dsIn.group    and
             EAemp# = :dsIn.employer and
             EAdiv  = :dsIn.division and
             EAinv# = :dsIn.invoice and
             (EAARCD = '  ' or
              EAARCD in
              (select TTKEY from WTTTP
                where TTco#=EAco# and TTTYPE in ('A','B','D','F','H')));
          // WMEAP
          Exec Sql
           Update WMEAP Set EAInvTypID = :decInvTypId
            Where EAco#  = :dsIn.company  and
             EAfnd  = :dsIn.group    and
             EAemp# = :dsIn.employer and
             EAdiv  = :dsIn.division and
             EAninv = :dsIn.invoice and
             EAarTy in ('A','B','D','F','H');

       EndSr;
       // *** End Add *** 04/11/22 ***************************************

      /End-free

     pInvoiceCloudBIFInvoicingDetail...
     p                 e

       //===================================================================
     pInvoiceCloudBIFHeader...
     p                 b
     dInvoiceCloudBIFHeader...
     d                 pi

     d billerGuide     s             50a   varying
     d fileReference   s             50a   varying
     d creationDate    s               d   DatFmt(*usa)
     d fileHeader      s            256a
     d customerRcd     s           1000a
     d*** Beg Add *** 11/15/22 ****************************************
     d strEmpAd1       s                   Like(dsWMEMP.EMpAd1)
     d strEmpAd2       s                   Like(dsWMEMP.EMpAd2)
     d strEmpCty       s                   Like(dsWMEMP.EMpCty)
     d strEmpSt        s                   Like(dsWMEMP.EMpSt)
     d decEmpZip       s                   like(dsWMEMP.EMpZip)
     d*** End Add *** 11/15/22 ****************************************
     d*** Beg Chg *** 11/15/22 ****************************************
     d*** dsSMCOP       e ds                  extname(SMCOP) qualified inz
     d dsWMEMP       e ds                  extname(WMEMP) qualified inz
     d*** End Chg *** 11/15/22 ****************************************

      /Free

       //*** Begin Add *** 07/07/23 ****************************************
       // Retrieve sweep ID
       Monitor;
          In lodWC2663;
       On-Error;
          sweepID = ' ';
       EndMon;
       //*** End   Add *** 07/07/23 ****************************************

       //*** Beg Chg *** 11/15/22 ****************************************
       //***Exec Sql Select * Into :dsSMCOP from SMCOP Where COco# = :company#;
       // Access Employer Master
       //*** Begin Chg *** 03/20/23 ***************************************
       //*Exec Sql Select * Into :dsWMEMP from WMEMP Where EMco# = :company#
       // and EMfnd = :group# and EMemp# = :employer# and EMdiv = :division;
       Exec Sql Select * Into :dsWMEMP from WMEMP Where EMco# = :company#
        and EMfnd = :group# and EMemp# = :employer# and EMstRp = 'Y';
       //*** End   Chg *** 03/20/23 ***************************************
       //*** End Chg *** 11/15/22 ****************************************

       account# = %EditC(company#:'X') +
                  %EditC(group#:'X') +
                  %EditC(employer#:'X');
       //*** Begin Add *** 09/07/23 **************************************
       //*** Begin Chg *** 07/18/25 **************************************
       //*** account#2 = %EditC(group#:'X') +
       //***             %EditC(employer#:'X');
       // Get the client ID
       GetClientID(clientID);

       Select;
       When clientID = '153';
          account#2 = GetCarrierPolicy#(company#:group#:employer#:
                      dsWMEMP.EMdiv);
          If account#2 = ' ';
             account#2 = %EditC(group#:'X') +
                         %EditC(employer#:'X');
          EndIf;
       Other;
          account#2 = %EditC(group#:'X') +
                      %EditC(employer#:'X');
       EndSl;
       //*** End   Chg *** 07/18/25 **************************************
       //*** End   Add *** 09/07/23 **************************************

       // Get the format guide
       billerGuide = GetModuleProperty('Invoice Cloud':'BillerGuid');

       // Build File Header
       fileHeader   = '1~FH|' +
                      '2~' + %Trim(billerGuide) + '|' +
                      '3~' + %Trim(fileReference) + '|' +
                      '4~' + alphaUSA(systemDate) + '|' +
                      '5~' + %Trim(%EditC(invoiceCount:'3')) + '|' +
                      '6~' + %Trim(%EditC(totalPaid:'3'));

       // Build Customer Record
       //*** Beg Add *** 11/15/22 *****************************************
       //***If dsWMEMP.EMpAd1 <> *blanks;
       If dsWMEMP.EMmAd1 = *blanks and dsWMEMP.EMpAd1 <> *blanks;
       //*** End Chg *** 04/21/23 ****************************************
          strEmpAd1  = dsWMEMP.EMpAd1;
          strEmpAd2  = dsWMEMP.EMpAd2;
          strEmpCty  = dsWMEMP.EMpCty;
          strEmpSt   = dsWMEMP.EMpSt;
          decEmpZip  = dsWMEMP.EMpZip;
       Endif;
       //*** Beg Chg *** 04/21/23 ****************************************
       //***If dsWMEMP.EMpAd1 = *blanks and dsWMEMP.EMmAd1 <> *blanks;
       If dsWMEMP.EMmAd1 <> *blanks;
       //*** End Chg *** 04/21/23 ****************************************
          strEmpAd1  = dsWMEMP.EMmAd1;
          strEmpAd2  = dsWMEMP.EMmAd2;
          strEmpCty  = dsWMEMP.EMmCty;
          strEmpSt   = dsWMEMP.EMmSt;
          decEmpZip  = dsWMEMP.EMmZip;
       Endif;
       //*** End Add *** 11/15/22 *****************************************
       customerRcd =  '58~CR|' +
                      '59~'  + %Trim(account#) + '|' +
                      //*** Beg Chg *** 11/15/22 ******************************
                      //***'60~'  + %Trim(dsSMCOP.COname) + '|' +
                      //***'61~'  + %Trim(dsSMCOP.COadr1) + '|' +
                      //***'62~'  + %Trim(dsSMCOP.COadr2) + '|' +
                      //***'63~'  + %Trim(dsSMCOP.COcty) + '|' +
                      //***'64~'  + %Trim(dsSMCOP.COst) + '|' +
                      //***'65~'  + %EditC(dsSMCOP.COzip:'X') + '|' +
                      '60~'  + %Trim(dsWMEMP.EMname) + '|' +
                      '61~'  + %Trim(strEMpAd1) + '|' +
                      '62~'  + %Trim(strEmpAd2) + '|' +
                      '63~'  + %Trim(strEmpCty) + '|' +
                      '64~'  + %Trim(strEmpSt) + '|' +
                      '65~'  + %EditC(decEmpZip:'X') + '|' +
                      //*** End Chg *** 11/15/22 ******************************
       //*** Begin Chg *** 09/07/23 **************************************
       //**********   '373~' + %Trim(account#);
                      '373~' + %Trim(account#2);
       //*** End   Chg *** 09/07/23 **************************************

       //*** Begin Chg *** 07/07/23 ****************************************
       //*** chrJsonIC = '{"servicer":"Invoice Cloud",' +
       //***   '"transaction":"Biller Invoice File",' +
       //***   '"information":{"fileHeader":"' +  %Trim(fileHeader) + '",' +
       //***   '"customerRecord":"' + %Trim(customerRcd) + '",' +
       //***   '"accountNumber":"' +  %Trim(account#) + '",' +
       //***   '"invoice":[' + %Trim(chrJsonIC) + ']}}';
       //*** Begin Chg *** 07/07/23 ****************************************
       If sweepID = ' ';
          chrJsonIC = '{"servicer":"Invoice Cloud",' +
             '"transaction":"Biller Invoice File",' +
             '"information":{"fileHeader":"' +  %Trim(fileHeader) + '",' +
             '"customerRecord":"' + %Trim(customerRcd) + '",' +
             '"accountNumber":"' +  %Trim(account#) + '",' +
             '"invoice":[' + %Trim(chrJsonIC) + ']}}';
       Else;
          chrJsonIC = '{"servicer":"Invoice Cloud",' +
             '"transaction":"Biller Invoice File",' +
             '"information":{"fileHeader":"' +  %Trim(fileHeader) + '",' +
             '"customerRecord":"' + %Trim(customerRcd) + '",' +
             '"accountNumber":"' +  %Trim(account#) + '",' +
             '"invoice":[' + %Trim(chrJsonIC) + ']},' +
             '"sweepID":"' + %Trim(sweepID) + '"}';
       EndIf;
       //*** End   Chg *** 07/07/23 ****************************************
       //*** End   Chg *** 07/07/23 ****************************************

       jsonIC_Data = chrJsonIC;
       jsonIC_Len  = %Len(%Trim(chrJsonIC));
       Clear arrInvoice;
       Clear chrJsonIC;
       Clear totalPaid;
       invoiceCount = 0;
       firstInvHdr = *On;

      /End-free

     pInvoiceCloudBIFHeader...
     p                 e

       //===================================================================
     pInvoiceCloudCCFICustomerConveyance...
     p                 b
     dInvoiceCloudCCFICustomerConveyance...
     d                 pi             1n

     d conveyanceHeader...
     d                 s           1000a
     d conveyanceRecord...
     d                 s           1000a

     d billerGuide     s             50a   varying
     d fileReference   s             50a   varying
     d creationDate    s               d   DatFmt(*usa)
     d convCount       s              5s 0 Inz(1)
     d oldAccount      s             30a
     d newAccount      s             30a
     d blockPayment    s              1a   Inz('0')
     d blockEFT        s              1a   Inz('0')
     d*** Begin Chg *** 10/07/24 *******************************************
     d*active***       s              1a   Inz('0')
     d active          s              1a   Inz('1')
     d*** End   Chg *** 10/07/24 *******************************************
     d conveyInv#      s              1a   Inz('0')

      /Free

       // Get the format guide
       billerGuide = GetModuleProperty('Invoice Cloud':'BillerGuid');

       oldAccount = old.company + old.group + old.employer;
       newAccount = new.company + new.group + new.employer;

       If new.blockCreditCards = 'true';
          blockPayment = '1';
       EndIf;

       If new.blockEFT = 'true';
          blockEFT = '1';
       EndIf;

       If new.active = 'true';
          active = '1';
       EndIf;

       If new.updateInvoiceNumbers = 'true';
          conveyInv# = '1';
       EndIf;

       creationDate = systemDate();

       conveyanceHeader = '358~CH|' +
                          '359~'    + %Trim(billerGuide) + '|' +
                          '360~'    + %Trim(fileReference) + '|' +
                          '361~'    + AlphaUSA(creationDate) + '|' +
                          '362~'    + %EditC(convCount:'3');

       conveyanceRecord = '363~CC|' +
                          '364~'    + %Trim(oldAccount) + '|' +
                          '365~'    + %Trim(newAccount) + '|' +
                          '366~'    + %Trim(blockPayment) + '|' +
                          '367~'    + %Trim(blockEFT) + '|' +
                          '369~'    + %Trim(active) + '|' +
                          '374~'    + %Trim(conveyInv#);

       chrJsonIC = '{' +
                   '"servicer":"Invoice Cloud",' +
                   '"transaction":"Customer Conveyance File",' +
                   '"information":{' +
                   '"fileHeader":"' + %Trim(conveyanceHeader) + '",' +
                   '"conveyanceRecord":"' + %Trim(conveyanceRecord) + '"' +
                   '}}';

       jsonIC_Data = chrJsonIC;
       jsonIC_Len  = %Len(%Trim(chrJsonIC));
       LoadSIWEBSVCDataQ(jsonIC);

       Return *on;
      /End-free

     pInvoiceCloudCCFICustomerConveyance...
     p                 e

       //===================================================================
     pQuotePaymentRecieved...
     p                 b                   export
     dQuotePaymentRecieved...
     d                 pi             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const

     ddecCount         s             10i 0 inz
      /Free
       sqlStmtStr = 'select count(*) ' +
        'from ' + %trim(RtvQuoteLib('*QTE')) + '/WDEAP_HLD ' +
        'where EAco# = ' + %char(inCo#) +
        ' and EAfnd = ' + %char(inGroup) +
        ' and EAemp# = ' + %char(inEmp#) +
        ' and EAdiv = ' + %char(inDiv) +
        ' and EAeaTp = ''IP''';  // Invoice Paid
       Exec sql close cursorQuotePayment;
       Exec sql prepare sqlQuotePayment from :sqlStmtStr;
       Exec sql declare cursorQuotePayment cursor for sqlQuotePayment;
       Exec sql open cursorQuotePayment;
       Exec SQL fetch next from cursorQuotePayment into :decCount;
       Return decCount > 0;
      /End-free
     pQuotePaymentRecieved...
     p                 e

       //===================================================================
     pProcessAdjustedInvoicing...
     p                 b                   export
     dProcessAdjustedInvoicing...
     d                 pi             1n
     d inJson                     32000    const varying

     ddsIn             ds                  qualified inz
     d company                        3
     d group                          3
     d employer                       9
     d division                       5
     d invoice                       10

     d dsInvoices      ds                  Qualified Dim(99999) Inz
     d   decCompany#                  3s 0
     d   decGroup                     3s 0
     d   decEmployer#                 9s 0
     d   decDivision                  5s 0
     d   decInvoice#                 10s 0

     d chrIncludeBatch...
     d                 s              1    Inz('N')
     d chrExcludeDeposit...
     d                 s              1    Inz('N')

     d decFromDate     s              7  0 Inz
     d decToDate       s              7  0 Inz
     d decARBalance    s             11  2 Inz

     d i               s              5  0
     d chrArCatIncInv  s              1
     d chrArCat        s              1
     d decCo#          s              3  0
     d decFnd          s              3  0
     d decEmp#         s              9  0
     d decDiv          s              5  0
     d decInv#         s             10  0
     d decFyr          s              3  0
     d intCo#          s              3  0
     d intFnd          s              3  0
     d intEmp#         s              9  0
     d intDiv          s              5  0
     d intInv#         s             10  0
     d intFyr          s              3  0
     d invoiceType     s             30

     d postDate        s              7  0
     d sysDate         s              7  0

      /Free

       // Is this taking place in Quote.
       env = ' ';
       RetrieveEnv(env);
       sysDate = DateTo7(systemDate());

       ExSr GetParms;
       If errMsg500 = ' ';
          // Write to invoice cloud
          If IsModEnabled('Invoice Cloud') And ICGroupOpt(intCo#:intFnd);
             ExSr DeleteInvoices;
          EndIf;
       EndIf;

       Return *on;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Load the json tree
          node0 = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                Select;
                When key = 'company';
                   dsIn.company = YAJL_get_string(val);
                When key = 'group';
                   dsIn.group = YAJL_get_string(val);
                When key = 'employer';
                   dsIn.employer = YAJL_get_string(val);
                When key = 'division';
                   dsIn.division = YAJL_get_string(val);
                When key = 'invoice';
                   dsIn.invoice = YAJL_get_string(val);
                Endsl;
             Enddo;
          Endif;

          intCo#  = %Dec(dsIn.company:3:0);
          intFnd  = %Dec(dsIn.group:3:0);
          intEmp# = %Dec(dsIn.employer:9:0);
          intDiv  = %Dec(dsIn.division:5:0);
          intInv# = %Dec(dsIn.invoice:10:0);

          Exec Sql Select EAfyr, EAcTyp Into :intFyr, :chrArCat
                   From WMEAP
                   Where EAco#=:intCo# And EAfnd=:intFnd And
                         EAemp#=:intEmp# And EAdiv=:intDiv And
                         EAninv=:intInv# Limit 1;

          Select;
          When chrArCat = 'R';
             invoiceType = 'deductible';
          When chrArCat = 'C';
             invoiceType = 'retro';
          Other;
             invoiceType = 'billing';
          EndSl;

       Endsr;

       //=================================================================
       // Delete Incompleted Invoices
       //=================================================================
       Begsr DeleteInvoices;

          // Finalize to maek sure previous invcoices processed.
          FinalizeThirdPartyInvoicing(intCo#:intFnd);

          // Get a list of incomplete invoices for this employer
          inRequest = 'ListIncompleteInvoices';
          inUser    = Q1user;
          inParms   = '{' +
                      '"company":"'  + dsIn.company   + '",' +
                      '"group":"'    + dsIn.group     + '",' +
                      '"employer":"' + dsIn.employer  + '"}';
          outJSON   = ' ';
          ioRtnCode = ' ';
          PaymentPgm(inRequest:inUser:inParms:outJson:ioRtnCode);

          // Extract the incomplete invoices

          // Load the incomplete invoice results
          Clear dsInvoices;
          outJson = '{' + %Trim(outJson) + '}';
          node0 = yajl_string_load_tree(outJson : errMsg500);
          If errMsg500 = ' ';
             node1 = YAJL_object_find(node0:'incompleteInvoices');
             i1 = 0;
             Dow YAJL_array_loop(node1:i1:node2);
                i2 = 0;
                Dow YAJL_object_loop(node2:i2:key:val);
                   Select;
                   When key = 'invoice#';
                      dsInvoices(i1) = YAJL_get_string(val);
                   Endsl;
                Enddo;
             EndDo;
          EndIf;

          // Get the posting date of current invoice
          Exec Sql Select Distinct EApoDt Into :decToDate
                   From WMEAP
                   Where EAco#=:intCo# And EAfnd=:intFnd And
                         EAemp#=:intEmp# And EAdiv=:intDiv And
                         EAninv=:intInv#;

          // Free up the space by removing the tree
          yajl_tree_free(node0);

          For i=1 to 99999;

             // Inactivate Incomplete Invoices
             decCo#  = dsInvoices(i).decCompany#;
             decFnd  = dsInvoices(i).decGroup;
             decEmp# = dsInvoices(i).decEmployer#;
             decDiv  = dsInvoices(i).decDivision;
             decInv# = dsInvoices(i).decInvoice#;

             // Leave if there are no invoices
             If decInv# = 0;
                Leave;
             EndIf;

             // Get the policy Year and category
             Exec Sql Select EAfyr, EAcTyp, EApoDt
                      Into :decFyr, :chrArCatIncInv, :postDate
                      From WMEAP
                      Where EAco#=:decCo# And EAfnd=:decFnd And
                            EAemp#=:decEmp# And EAdiv=:decDiv And
                            EAninv=:decInv# Limit 1;

             // Don't delete invoice if it is:
             // 1) Going to be updated
             // 2) Not the same invoice type
             // 3) If it is a different policy year from the final invoice
             // 4) If the post date is greater than the new invoice
             //    post date
             If (decInv# <> intInv#) And
                ((chrArCat = 'R' And chrArCatIncInv = 'R') Or
                 (chrArCat = 'C' And chrArCatIncInv = 'C') Or
                 (chrArCat <> 'R' And chrArCat <> 'C' And
                  chrArCatIncInv <> 'R' And chrArCatIncInv <> 'C')) And
                (intFyr = decFyr) And
                //*** Begin Chg *** 01/05/24 *******************************
                //*** (postDate <= decToDate);
                (postDate <= decToDate) And
                (intDiv = decDiv);
                //*** End   Chg *** 01/05/24 *******************************

                // Inactivate Invoice by calling Third Party Invoici   ng
                chrJson = '{' +
                       '"type":"'     + %Trim(invoiceType)       + '   ",' +
                       '"invoice":"'  + %EditC(decInv#:'X')      + '   ",' +
                       '"company":"'  + %EditC(decCo#:'X')       + '   ",' +
                       '"group":"'    + %EditC(decFnd:'X')       + '   ",' +
                       '"employer":"' + %EditC(decEmp#:'X')      + '   ",' +
                       '"division":"' + %EditC(decDiv:'X')       + '   ",' +
                       '"active":"false",'  +
                       '"delete":"true"'   +
                       '}';
                ThirdPartyInvoicing(chrJson);
             EndIf;
          EndFor;

          // Get the new invoice balance
          decARBalance = ArBalance(decFromDate:decToDate:intCo#:intFnd:
                                   intFyr:intEmp#:intDiv:'Invoice Cloud':
                                   intInv#:chrIncludeBatch:chrExcludeDeposit);

          // Call Third Party invoicing if there is a balance
          If decARBalance > 0;
             chrJson = '{' +
                       '"type":"'     + %Trim(invoiceType)       + '",' +
                       '"invoice":"'  + %EditC(intInv#:'X')      + '",' +
                       '"company":"'  + %EditC(intCo#:'X')       + '",' +
                       '"group":"'    + %EditC(intFnd:'X')       + '",' +
                       '"employer":"' + %EditC(intEmp#:'X')      + '",' +
                       '"division":"' + %EditC(intDiv:'X')       + '",' +
                       '"amount":"'   + %EditC(decARBalance:'3') + '",'  +
                       '"active":"true",'  +
                       '"delete":"false"'   +
                       '}';
             ThirdPartyInvoicing(chrJson);
          EndIf;
          FinalizeThirdPartyInvoicing(intCo#:intFnd);

       Endsr;

      /End-free
     pProcessAdjustedInvoicing...
     p                 e

       //===================================================================
     pThirdPartyInvoicing...
     p                 b                   export
     dThirdPartyInvoicing...
     d                 pi             1n
     d inJson                     32000    const varying

     ddsIn             ds                  qualified inz
     d type                          20
     d invoice                       10
     d company                        3
     d group                          3
     d year                           3
     d employer                       9
     d division                       5
     d amount                        11
     d active                         5
     d delete                         5

      /Free

       ExSr GetParms;
       If errMsg500 = ' ';
          ExSr MakePayment;
       EndIf;

       Return *On;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Initialize non-required fields
          dsIn.active = 'true';
          dsIn.delete = 'false';

          // Load the json tree
          node0 = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                Select;
                When key = 'type';
                   dsIn.type = YAJL_get_string(val);
                When key = 'invoice';
                   dsIn.invoice = YAJL_get_string(val);
                When key = 'company';
                   dsIn.company = YAJL_get_string(val);
                When key = 'group';
                   dsIn.group = YAJL_get_string(val);
                When key = 'year';
                   dsIn.year = YAJL_get_string(val);
                When key = 'employer';
                   dsIn.employer = YAJL_get_string(val);
                When key = 'division';
                   dsIn.division = YAJL_get_string(val);
                When key = 'amount';
                   dsIn.amount = YAJL_get_string(val);
                When key = 'active';
                   dsIn.active = YAJL_get_string(val);
                When key = 'delete';
                   dsIn.delete = YAJL_get_string(val);
                Endsl;
             Enddo;

             invoice#  = %Dec(dsIn.invoice:10:0);
             company#  = %Dec(dsIn.company:3:0);
             group#    = %Dec(dsIn.group:3:0);
             employer# = %Dec(dsIn.employer:9:0);

             If dsIn.division <> ' ';
                division  = %Dec(dsIn.division:5:0);
             EndIf;
          Endif;

       Endsr;

       //=================================================================
       // Make Third Party Payment
       //=================================================================
       Begsr MakePayment;

          // Is this taking place in Quote.
          env = ' ';
          RetrieveEnv(env);

          // Write to invoice cloud
          If IsModEnabled('Invoice Cloud') And ICGroupOpt(company#:group#);
             InvoiceCloudBIFInvoicingDetail(inJson);
          Endif;

       Endsr;

      /End-free
     pThirdPartyInvoicing...
     p                 e

       //===================================================================
     pThirdPartyUpdates...
     p                 b                   export
     dThirdPartyUpdates...
     d                 pi             1n
     d inJson                     32000    const varying

      /Free

       ExSr GetParms;
       If errMsg500 = ' ';
          ExSr SendConveyance;
       EndIf;

       Return *on;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Load the json tree
          node0 = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;
             Dow YAJL_object_loop(node0:i0:key:val);
                Select;
                When key = 'informationOld';
                   ExSr ProcessOldInformation;
                When key = 'informationNew';
                   ExSr ProcessNewInformation;
                Endsl;
             Enddo;
          Endif;

       Endsr;

       //=================================================================
       // Process New Information
       //=================================================================
       Begsr ProcessNewInformation;

          // Load the json tree
          node2 = YAJL_object_find(node0:key);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key:val);
              Select;
              When key = 'company';
                 new.company  = YAJL_get_string(val);
              When key = 'group';
                 new.group = YAJL_get_string(val);
              When key = 'employer';
                 new.employer = YAJL_get_string(val);
              When key = 'blockCreditCards';
                 new.blockCreditCards = YAJL_get_string(val);
              When key = 'blockEFT';
                 new.blockEFT = YAJL_get_string(val);
              When key = 'active';
                 new.active = YAJL_get_string(val);
              When key = 'updateInvoiceNumbers';
                 new.updateInvoiceNumbers = YAJL_get_string(val);
              Endsl;
          Enddo;


       Endsr;

       //=================================================================
       // Process old Information
       //=================================================================
       Begsr ProcessOldInformation;

          // Load the json tree
          node1 = YAJL_object_find(node0:key);
          Reset i1;
          Dow YAJL_object_loop(node1:i1:key:val);
              Select;
              When key = 'company';
                 old.company  = YAJL_get_string(val);
              When key = 'group';
                 old.group = YAJL_get_string(val);
              When key = 'employer';
                 old.employer = YAJL_get_string(val);
              Endsl;
          Enddo;

          // Load key fields
          company# = %Dec(old.company:3:0);
          group#   = %Dec(old.group:3:0);

       Endsr;
       //=================================================================
       // Send Converyance
       //=================================================================
       Begsr SendConveyance;

          // Write to invoice cloud
          If IsModEnabled('Invoice Cloud') And
             ICGroupOpt(company#:group#);
             InvoiceCloudCCFICustomerConveyance();
          Endif;

       Endsr;
      /End-free
     pThirdPartyUpdates...
     p                 e

      //*** Beg Chg *** 10/25/23 *** WC2922 **************************
      //===================================================================
     pRemoveInvoices...
     p                 b                   export
     dRemoveInvoices...
     d                 pi             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFYR                          3  0 const
     d inTyp                          1    const
      *
     ddsWMEAP        E DS                  ExtName(WMEAP) Qualified Inz
     dInvToRmv         S             10  0
     dInvAmt           S             13  2
     d sysDate         S              7  0
     d sysTime         S              6  0
     D ChrinCo#        S              3a
     D ChrinFnd        S              3a
     D ChrinEmp#       S              9a
     D ChrinDiv        S              5a
     D ChrinInv#       S             10a
      *
      /Free
       sysDate = DateTo7(systemDate());
       sysTime = TimeTo6(systemTime());

       Exec sql
        Declare RemoveInvCSR Cursor For
          Select *
          From WMEAL10
          Where EACO# = :inCo# and EAFND = :inFnd and EAEMP# = :inEmp# and
                EADIV = :inDiv and EAFYR = :inFYR;

       Exec SQL Open RemoveInvCSR;

       Exec SQL Fetch RemoveInvCSR into :dsWMEAP;

       // Loop as long as there is a Invoices to Dleete from IVC
       Dow SQLCOD = 0;
           // Check for Inv on WDEAP_HLD and If Found Call DELETEINV to Remove
           InvToRmv = 0;
           Exec SQL
              Select EAINV#, EAAMT into :InvToRmv, :InvAmt
              From WDEAP_HLD
              Where EACO# = :inCo# and EAFND = :inFnd and EAEMP# = :inEmp# and
                    EADIV = :inDiv and EAFYR = :inFYR and
                    EAINV# = :dsWMEAP.EANINV;

              If InvToRmv <> 0;
                 ChrinCo#  = %EditC(inCo#:'X')   ;
                 ChrinFnd  = %EditC(inFnd:'X')   ;
                 ChrinEmp# = %EditC(inEmp#:'X')  ;
                 ChrinDiv  = %EditC(inDiv:'X')   ;
                 ChrinInv# = %EditC(InvToRmv:'X');

                 DeleteInvoice(ChrinCo# : ChrinFnd : ChrinEmp# : ChrinDiv :
                               ChrinInv#);

                 Exec SQL
                    Insert into WIIHP(IHCO#,IHFND,IHEMP#,IHDIV,IHFYR,IHTYPE,
                    IHINV#,IHPODT,IHAMT,IHUSER,IHPGM,IHTIME,IHDT,
                    IHIUSR,IHIPGM,IHITIM,IHIDT)
                    Values(:dsWMEAP.EACO#,:dsWMEAP.EAFnd,:dsWMEAP.EAEmp#,
                    :dsWMEAP.EADiv,:dsWMEAP.EAFyr,:inTyp,:InvToRmv,
                    :dsWMEAP.EAPoDT,:InvAmt,:Q1user,:Q1pgm,:sysTime,:sysDate,
                    :Q1user,:Q1pgm,:sysTime,:sysDate);
              EndIf;
          Exec SQL Fetch RemoveInvCSR into :dsWMEAP;
       EndDo;

       Exec SQL Close RemoveInvCSR;

       Return *On;
      /End-free
     pRemoveInvoices...
     p                 e
      //===================================================================
     pResendInvoices...
     p                 b                   export
     dResendInvoices...
     d                 pi             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFYR                          3  0 const
     d inTyp                          1    const
      *
     ddsWIIHP        E DS                  ExtName(WIIHP) Qualified Inz
     D inJson          S          32000A   Inz Varying
      *
      /Free
       Exec sql
        Declare ResendInvCSR Cursor For
          Select *
          From WIIHP
          Where IHCO# = :inCo# and IHFND = :inFnd and IHEMP# = :inEmp# and
                IHDIV = :inDiv and IHFYR = :inFYR and IHTYPE = :inTyp;

       Exec SQL Open ResendInvCSR;

       Exec SQL Fetch ResendInvCSR into :dsWIIHP;;

       // Loop as long as there is a Invoice to Resend to IVC
       Dow SQLCOD = 0;
           // If Invoice Found to be Resend, Call ProcessAdjustedInvoicing
           inJson = '{"type":"billing",' +
                    '"invoice":"'  + %EditC(dsWIIHP.IHInv#:'X') + '",' +
                    '"company":"'  + %EditC(inco#:'X')  + '",' +
                    '"group":"'    + %EditC(inFnd:'X')  + '",' +
                    '"employer":"' + %EditC(inEmp#:'X') + '",' +
                    '"division":"' + %EditC(inDiv:'X')  + '",' +
                    '"active":"true"}';
           ProcessAdjustedInvoicing(inJson);

           // Now Delete the Invoice from Holding File
           Exec SQL
              Delete From WIIHP
              Where IHCO# = :inCo# and IHFND = :inFnd and IHEMP# = :inEmp# and
                    IHDIV = :inDiv and IHFYR = :inFYR and IHTYPE = :inTyp and
                    IHINV# = :dsWIIHP.IHInv#;

          Exec SQL Fetch ResendInvCSR into :dsWIIHP;
       EndDo;

       Exec SQL Close ResendInvCSR;

       Return *On;
      /End-free
     pResendInvoices...
     p                 e

      //===================================================================
     pReplaceHeldInvoices...
     p                 b                   export
     dReplaceHeldInvoices...
     d                 pi             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFYR                          3  0 const
     d inInv#                        10  0 const
     d inTyp                          1    const
     d*** Begin Add *** 09/27/24 ******************************************
     d inMode                         1    const options(*nopass)
     d*** End   Add *** 09/27/24 ******************************************
      *
     d InvToRplc       S              9  0 Inz(0)
     d InvPoDt         S              7  0
     d chrIncludeBatch...
     d                 S              1    Inz('N')
     d chrExcludeDeposit...
     d                 S              1    Inz('N')
     d decFromDate     S              7  0 Inz
     d decToDate       S              7  0 Inz
     d decInvAmt       S             15  2 Inz
     d sysDate         s              7  0
     d sysTime         s              6  0
     d*** Begin Add *** 09/27/24 ******************************************
     d chrMode         s              1    Inz
     d*** Begin Add *** 09/27/24 ******************************************
     d
      *
      /Free
       sysDate = DateTo7(systemDate());
       sysTime = TimeTo6(systemTime());

       //*** Begin Add *** 09/27/24 **************************************
       chrMode = ' ';
       If %Parms() > 7;
          chrMode = inMode;
       EndIf;
       //*** End   Add *** 09/27/24 **************************************

       // Now Delete the Invoice from Holding File for Information Passed
       Exec SQL
          Delete From WIIHP
          Where IHCO# = :inCo# and IHFND = :inFnd and IHEMP# = :inEmp# and
                IHDIV = :inDiv and IHFYR = :inFYR and IHTYPE = :inTyp and
                IHINV# = :inInv#;

       //*** Begin Add *** 09/27/24 **************************************
       If chrMode = 'D';
          Return *On;
       EndIf;
       //*** End   Add *** 09/27/24 **************************************

       Exec SQL
          Select EAPODT into :decToDate
          From WMEAP
          Where EACO# = :inCo# and EAFND = :inFnd and EAEMP# = :inEmp# and
                EADIV = :inDiv and EAFYR = :inFYR and EANINV = :inInv#;

       decInvAmt = ArBalance(decFromDate:decToDate:inCo#:inFnd:
                             inFyr:inEmp#:inDiv:'Invoice Cloud':
                             inInv#:chrIncludeBatch:chrExcludeDeposit);

       Exec SQL
          Insert into WIIHP(IHCO#,IHFND,IHEMP#,IHDIV,IHFYR,IHTYPE,
          IHINV#,IHPODT,IHAMT,IHUSER,IHPGM,IHTIME,IHDT,
          IHIUSR,IHIPGM,IHITIM,IHIDT)
          Values(:inCO#,:inFnd,:inEmp#,:inDiv,:inFyr,:inTyp,:inInv#,
          :decToDate,:decInvAmt,:Q1user,:Q1pgm,:sysTime,:sysDate,
          :Q1user,:Q1pgm,:sysTime,:sysDate);

       Return *On;
      /End-free
     pReplaceHeldInvoices...
     p                 e

      //*** End Chg *** 10/25/23 *** WC2922 **************************
