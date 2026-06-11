     H*Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     c*====================================================================
     c* Description:  Copybook to build premium line items and payroll data.
     c*
     c* Project: SY406   Date: 08/16/18
     c* Pgmr:    C:65
     c*
     c*====================================================================
     c* Revisions:
     H*
     H* 11/11/24 C:333 Project: WC2985
     H*                Apply Deductible to Discounted Premium.
     H*
     H* 04/18/24 C:291 Project: WC2961
     H*                Add Schedule DB/CR when applied at Manual Premium.
     c*
     H* 09/23/22 C:65  Project: Phw#103712 (Inhouse)
     H*                Apply payroll rate changes laid out in WIRATE/WIRATECMP.
     c*
     c* 03/15/22  C:33  Support: 100464
     c*                 Show leading zeros on class codes
     c*====================================================================
      /Free
       //=================================================================
       // Clear amounts for policy summary.
       //=================================================================
       Begsr ClearSummary;

        //* Summary fields
        Clear xtman;
        Clear xtincr;
        Clear xtsubj;
        Clear xtmod;
        Clear xtstan;
        Clear xtdisc;
        Clear xtnorm;
        Clear xtexp;
        Clear xtterr;
        Clear xtprem;
        Clear xtfee;
        Clear xttotal;

       Endsr;

       //=================================================================
       // Accumulate amounts for policy summary.
       //=================================================================
       Begsr SumPremium;

        //* Summary totals
        xtman  = xtman + ELMAPR;
        xtincr = xtincr + ELEIN1 + ELEIN2;
        xtsubj = xtsubj + ELESBJ;
        xtmod  = xtmod  + ELEMPM;
        xtstan = xtstan + ELSTPR;
        xtdisc = xtdisc + ELEDC3;
        xtnorm = xtnorm + ELENMP;
        xtexp  = xtexp  + ELPRTD;
        xtterr = xtterr + ELETER + ELEDTR;
        xtprem = xtprem + ELANPR;
        xtfee  = xtfee  + EL1AAM + EL2AAM + EL3AAM + EL4AAM + EL5AAM +
         EL6AAM + EL7AAM + ELTXDU + ELMFAM + ELSFAM;
        xttotal = xtprem + xtfee;

       Endsr;

       //=================================================================
       // Build policy summary.
       //=================================================================
       Begsr BuildSummary;

        //* Add premium summary information as new array.
        json = %trim(json) + ', "premiumSummary" : [';

        json = %trim(json) + ' { ';
        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Manual ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(xtman:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Increased Limits');
        BuildGiantJSON(json :'heading' : 'Increased Limits');
        BuildGiantJSON(json :'amount' :%editc(xtincr:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Subject ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(xtsubj:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Modified ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(xtmod:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Standard ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(xtstan:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Stock Discount');
        BuildGiantJSON(json :'heading' : 'Discount');
        BuildGiantJSON(json :'amount' :%editc((-1 * xtdisc):'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Normal ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(xtnorm:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Expense Constant');
        BuildGiantJSON(json :'heading' : 'Expense Constant');
        BuildGiantJSON(json :'amount' :%editc(xtexp:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Terrorism Charges');
        BuildGiantJSON(json :'heading' : 'Terrorism Charges');
        BuildGiantJSON(json :'amount' :%editc(xtterr:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Annual ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(xtprem:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Fees and Taxes');
        BuildGiantJSON(json :'heading' : 'Fees and Taxes');
        BuildGiantJSON(json :'amount' :%editc(xtfee:'P'));
        json = %trim(json) + ' }, { ';

        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Total');
        BuildGiantJSON(json :'amount' :%editc(xttotal:'P'));

        json = %trim(json) + ' } ]';

       Endsr;

       //=================================================================
       // Build beginning of data for a quote/policy.
       //=================================================================
       Begsr BuildHeader;

        If commaPol <> ' ';
           json = %trim(json) + commaPol;
        Else;
           commaPol = ',';
        Endif;

        json = %trim(json) + ' { ';
        BuildGiantJSON(json :'company' :%editc(ELCO#:'X'));
        BuildGiantJSON(json :'group' :%editc(ELFND:'X'));
        BuildGiantJSON(json :'year' :%editc(ELFYR:'X'));
        BuildGiantJSON(json :'employer' :%editc(ELEMP#:'X'));
        BuildGiantJSON(json :'premium' :%editc(xxPremium:'P'));

        If jsonData = ' ';
           jsonData = '""';
        EndIf;
        json = %trim(json) + ', "validation" : ' + %trim(jsonData);

        //* Add premium summary information as new array.
        Exsr BuildSummary;

        json = %trim(json) + ', "divisionInfo" : [ ';
        commaDiv = ' ';

       Endsr;

       //=================================================================
       // Build record results
       //=================================================================
       Begsr BuildRecord;

        If commaDiv <> ' ';
           json = %trim(json) + commaDiv;
        Else;
           commaDiv = ',';
        Endif;

        json = %trim(json) + ' { ';

        BuildGiantJSON(json :'division' :%editc(ELDIV:'X'));

       //* Report payroll records unless indicated.
        If payFlag = 'Y';

           Clear comma1;
           json = %trim(json) + ', "payrollRcds" : [ ';
           Chain(n) (ELCO#:ELFND:ELEMP#:ELDIV:ELFYR) pep0e;
           If %found;
              json = %trim(json) + ' { ';

           //* Load up payroll charges and report payrolls.
              Setll (ELCO#:ELFND:ELEMP#:ELDIV:ELFYR) pep0e;
              Reade(n) (ELCO#:ELFND:ELEMP#:ELDIV:ELFYR) pep0e;
              Dow not %eof;
                 If EPStat <> 'I';
                    xxEFDT = SrEffd(ELCO#:ELFND:ELFYR:ELEMP#:ELDIV:EPEFDT:
                     'P':EPCLAS);
                    Setgt (ELCO#:EPCLAS:EPEXT:RSJRST:xxEFDT) ppx01;
                    Readpe(n) (ELCO#:EPCLAS:EPEXT:RSJRST) ppx01;
                    Setgt (ELCO#:ELRTCD:EPCLAS:xxEFDT) ppr01;
                    Readpe(n) (ELCO#:ELRTCD:EPCLAS) ppr01;
                    If not %eof;
                       Select;
                          When PRoccd = 'Y';
                             xtoccd = xtoccd + EPadld;
                          When PRnrat = 'Y';
                             xtnrat = xtnrat + EPadld;
                          When PXcode <> *Zero and EPext <> *Blanks;
                          Other;
                             xtadld = xtadld + EPadld;
                       Endsl;
                       If comma1 <> ' ';
                          json = %trim(json) + ' } , { ';
                       Else;
                          comma1 = ',';
                       Endif;
                     //***  Begin Chg  ***  03/15/22  ***************
                     //BuildGiantJSON(json :'class' :%editc(EPCLAS:'P'));
                       BuildGiantJSON(json :'class'
                        :%subst(%editc(EPCLAS:'X'):2));
                     //***  End   Chg  ***  03/15/22  ***************
                       BuildGiantJSON(json :'spec' : %trim(EPSPEC));
                       BuildGiantJSON(json :'ext' : %trim(EPEXT));
                       BuildGiantJSON(json :'effective' :%editc(DtMDYY(
                        EPEFDT):'X'));
                       BuildGiantJSON(json :'description' : %trim(PRDESC));
                    //*** begin chg ***  09/23/22  **************************************
                    //*****   BuildGiantJSON(json :'rate' : %editc(PRRT:'P'));
                       If calcflag = 'Y' or (ELSTAT = 'Y' and PRARFL = 'Y');
                          BuildGiantJSON(json :'rate' : %editc(EPARAT:'P'));
                       Else;
                       //* Get Territory Differential Factor
                          GetTDF(ELCo#:ELFnd:ELEmp#:ELDiv:ELFYr:xxTDF);
                          If xxTDF = 0;
                             xxTDF = 1;
                          Endif;
                          qqCo#  = %editc(ELCo#:'X');
                          qqFnd  = %editc(ELFnd:'X');
                          qqClas = %editc(EPClas:'X');
                          qqEfdt = %editc(xxEfdt:'X');
                          qqLCM  = *blanks;
                       //* Get Loss Cost Multiplier
                          SRLCM(qqCo#:qqFnd:ELRtcd:qqClas:qqEfdt:qqLCM);
                          If qqLCM <> *blanks;
                       //*** begin chg ***  11/04/22  *****************************
                       //****xxLCM = %dec(qqLCM:5:4);
                             xxLCM = %dec(%int(qqLCM)/10000:5:4);
                       //*** end chg *****  11/04/22  *****************************
                          Else;
                             xxLCM = 1;
                          Endif;
                       //* Get Rate Deviation
                          xxRDEV = 1 - (RRRDev/100);
                       //* Build Payroll Rate
                          Eval(h) xxARAT = PRRt * xxTDF * xxLCM * xxRDev;
                       //* Output the Rate
                          BuildGiantJSON(json :'rate' : %editc(xxARAT:'P'));
                       Endif;
                    //*** end chg *****  09/23/22  **************************************
                       BuildGiantJSON(json :'perCap' : %trim(PRPCF));
                       BuildGiantJSON(json :'exposure' :%editc(EPEAPR:'P'));
                       BuildGiantJSON(json :'premium' :%editc(EPEMPR:'P'));
                       BuildGiantJSON(json :'addLoad' :%editc(EPADLD:'P'));
                    Endif;
                 Endif;
                 Reade(n) (ELCO#:ELFND:ELEMP#:ELDIV:ELFYR) pep0e;
              Enddo;

              json = %trim(json) + ' } ';
           Endif;
           json = %trim(json) + ' ] ';
        Endif;

       //* Report premium line items.
        json = %trim(json) + ', "premiumItems" : [ ';
        json = %trim(json) + ' { ';
        If ELEMNP = 'Y';
           ELMAPR = ELMAPR - ELEIN1 - ELEIN2 - ELEWAV;
        Endif;
        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Manual ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(ELMAPR:'P'));
        json = %trim(json) + ' }, { ';

        //* Deviation #1 at Manual.
        If RRDV1L = 'M';
           Exsr BuildDeviation1;
        Endif;

        //* Deviation #2 at Manual.
        If RRDV2L = 'M';
           Exsr BuildDeviation2;
        Endif;

        //***Beg Add ***** 04/18/24 ************************************
        //* Schedule Db/Cr applied at Manual
        If RRdcap = 'M' and ELDBCR <> 0;
           Exsr BuildSchedDBCR;
        Endif;
        //***End Add ***** 04/18/24 ************************************

        //* Waiver at Manual.
        If ELWVFL = 'M' and ELEWAV <> 0;
           Exsr BuildWaiver;
        Endif;

        //* Increased Limits 1.
        If ELILCD <> ' ';
           Exsr BuildIL1;
        Endif;

        //* Increased Limits 2.
        If ELINL2 <> ' ';
           Exsr BuildIL2;
        Endif;

        //* Voluntary Labor Charge.
        If ELVLCA <> 0 and ELEMNP <> 'Y';
           Exsr BuildVLC;
        Endif;

        If RRRDPR = 'M' and xxLarge <> 'Y';
        //* Small Deductible #1 at Manual.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and ELDDCT <> 0);
              Exsr BuildSmallDed1;
           Endif;

        //* Small Deductible #2 at Manual.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and ELDD2P <> 0);
              Exsr BuildSmallDed2;
           Endif;
        Endif;

        If xxLarge = 'Y' and LDPLV1 = 'M';
        //* Large Deductible #1 at Manual.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and LDPCT1 <> 0);
              Exsr BuildLargeDed1;
           Endif;
        Endif;

        If xxLarge = 'Y' and LDPLV2 = 'M';
        //* Large Deductible #2 at Manual.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and LDPCT2 <> 0);
              Exsr BuildLargeDed2;
           Endif;
        Endif;

        If ELEMNP = 'Y' and ELWVFL <> 'M';
           ELESBJ = ELESBJ - ELEWAV;
        Endif;
        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Subject ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(ELESBJ:'P'));
        json = %trim(json) + ' }, { ';

        //* Managed care credit.
        If ELEDCM <> 0 or (ELEMNP= 'Y' and ELASPM <> 0);
           Exsr BuildMCCredit;
        Endif;

        //* Safety credit.
        If ELEDCS <> 0 or (ELEMNP= 'Y' and ELASPS <> 0);
           Exsr BuildSafetyCredit;
        Endif;

        //* Deviation #1 at Subject.
        If RRDV1L = 'J';
           Exsr BuildDeviation1;
        Endif;

        //* Deviation #2 at Subject.
        If RRDV2L = 'J';
           Exsr BuildDeviation2;
        Endif;

        //* Waiver at Subject.
        If ELWVFL = 'J' and ELEWAV <> 0;
           Exsr BuildWaiver;
        Endif;

        //* Additional load at Subject.
        If RRASFL = 'J' and XTADLD <> 0 and ELEMNP <> 'Y';
           Exsr BuildAddLoad;
        Endif;

        If xxLarge = 'Y' and LDPLV1 = 'J';
        //* Large Deductible #1 at Subject.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and LDPCT1 <> 0);
              Exsr BuildLargeDed1;
           Endif;
        Endif;

        If xxLarge = 'Y' and LDPLV2 = 'J';
        //* Large Deductible #2 at Subject.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and LDPCT2 <> 0);
              Exsr BuildLargeDed2;
           Endif;
        Endif;

        //* Experience Modifiers
        If ELEXM1 <> 0 or ELEXM2 <> 0 or ELEXM3 <> 0;
           Exsr BuildExpMods;
        Endif;

        //* Contractors credit.
        If ELEDC1 <> 0 or (ELEMNP= 'Y' and ELASP1 <> 0);
           Exsr BuildContCredit;
        Endif;

        //* Schedule DB/CR.
        If ELDBCR <> 0 and RRDCAP = ' ';
           Exsr BuildSchedDBCR;
        Endif;

        //* Deviation #1 at Modified.
        If RRDV1L = 'E';
           Exsr BuildDeviation1;
        Endif;

        //* Deviation #2 at Modified.
        If RRDV2L = 'E';
           Exsr BuildDeviation2;
        Endif;

        //* Schedule DB/CR.
        If ELDBCR <> 0 and RRDCAP = 'Y';
           Exsr BuildSchedDBCR;
        Endif;

        //* Waiver at Modified.
        If ELWVFL = 'E' and ELEWAV <> 0;
           Exsr BuildWaiver;
        Endif;

        //* Additional load at Modified.
        If RRASFL = 'E' and XTADLD <> 0 and ELEMNP <> 'Y';
           Exsr BuildAddLoad;
        Endif;

        If xxLarge = 'Y' and LDPLV1 = 'E';
        //* Large Deductible #1 at Modified.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and LDPCT1 <> 0);
              Exsr BuildLargeDed1;
           Endif;
        Endif;

        If xxLarge = 'Y' and LDPLV2 = 'E';
        //* Large Deductible #2 at Modified.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and LDPCT2 <> 0);
              Exsr BuildLargeDed2;
           Endif;
        Endif;

        If RRRDPR = 'S' and xxLarge <> 'Y';
        //* Small Deductible #1 at Standard.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and ELDDCT <> 0);
              Exsr BuildSmallDed1;
           Endif;

        //* Small Deductible #2 at Standard.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and ELDD2P <> 0);
              Exsr BuildSmallDed2;
           Endif;
        Endif;

        //* Loss Constant.
        If ELLOSS <> 0 and ELEMNP <> 'Y';
           Exsr BuildLossConst;
        Endif;

        //* Occupational Disesase.
        If xtOCCD <> 0 and ELEMNP <> 'Y';
           Exsr BuildOccpDisease;
        Endif;

        //* Non-Ratable Catastrophe.
        If xtNRat <> 0 and ELEMNP <> 'Y';
           Exsr BuildNonRatable;
        Endif;

        If ELEMNP = 'Y' and ELWVFL <> 'M' and ELWVFL <> 'J' and ELWVFL <> 'E';
           ELSTPR = ELSTPR - ELEWAV;
        Endif;
        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Standard ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc(ELSTPR:'P'));
        json = %trim(json) + ' }, { ';

        //* Deviation #1 at Standard.
        If RRDV1L = 'S';
           Exsr BuildDeviation1;
        Endif;

        //* Deviation #2 at Standard.
        If RRDV2L = 'S';
           Exsr BuildDeviation2;
        Endif;

        //* Waiver at Standard.
        If ELWVFL = 'S' and ELEWAV <> 0;
           Exsr BuildWaiver;
        Endif;

        //* Additional load at Standard.
        If RRASFL = 'S' and XTADLD <> 0 and ELEMNP <> 'Y';
           Exsr BuildAddLoad;
        Endif;

        If xxLarge = 'Y' and LDPLV1 = 'S';
        //* Large Deductible #1 at Standard.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and LDPCT1 <> 0);
              Exsr BuildLargeDed1;
           Endif;
        Endif;

        If xxLarge = 'Y' and LDPLV2 = 'S';
        //* Large Deductible #2 at Standard.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and LDPCT2 <> 0);
              Exsr BuildLargeDed2;
           Endif;
        Endif;

        //* Stock/Premium Discount.
        If ELEDC3 <> 0 or (ELEMNP = 'Y' and (ELASP3 <> 0 or ELDISC <> ' '));
           Exsr BuildStockDisc;
        Endif;

        //* Deviation #1 at Discounted.
        If RRDV1L = 'D';
           Exsr BuildDeviation1;
        Endif;

        //* Deviation #2 at Discounted.
        If RRDV2L = 'D';
           Exsr BuildDeviation2;
        Endif;

        //* Waiver at Discounted.
        If ELWVFL = 'D' and ELEWAV <> 0;
           Exsr BuildWaiver;
        Endif;

        //* Additional load at Discounted.
        If RRASFL = 'D' and XTADLD <> 0 and ELEMNP <> 'Y';
           Exsr BuildAddLoad;
        Endif;

        If xxLarge = 'Y' and LDPLV1 = 'D';
        //* Large Deductible #1 at Discounted.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and LDPCT1 <> 0);
              Exsr BuildLargeDed1;
           Endif;
        Endif;

        If xxLarge = 'Y' and LDPLV2 = 'D';
        //* Large Deductible #2 at Discounted.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and LDPCT2 <> 0);
              Exsr BuildLargeDed2;
           Endif;
        Endif;

        //*** Beg Add ***** 11/11/24 ************************************
        If RRRDPR = 'D' and xxLarge <> 'Y';
        //* Small Deductible #1 at Discounted.
           If ELEDCT <> 0 or (ELEMNP = 'Y' and ELDDCT <> 0);
              Exsr BuildSmallDed1;
           Endif;

        //* Small Deductible #2 at Discounted.
           If ELEDD2 <> 0 or (ELEMNP = 'Y' and ELDD2P <> 0);
              Exsr BuildSmallDed2;
           Endif;
        Endif;
        //*** End Add ***** 11/11/24 ************************************

        //* ARAP Factors
        If ELEMNP <> 'Y' and (ELRPE1 <> 0 or ELRPE2 <> 0 or ELRPE3 <> 0);
           Exsr BuildARAP;
        Endif;

        BuildGiantJSON(json :'type' : 'Total');
        If ELEMNP = 'Y';
           BuildGiantJSON(json :'heading' : 'Minimum ' + %trim(premWord));
        Else;
           BuildGiantJSON(json :'heading' : 'Normal ' + %trim(premWord));
        Endif;
        BuildGiantJSON(json :'amount' :%editc(ELENMP:'P'));
        json = %trim(json) + ' }, { ';

        //* Facultative Reinsurance Charge
        If ELEARC <> 0;
           Exsr BuildFaculRein;
        Endif;

        //* Terrorism Risk Act Charge
        If ELETER <> 0;
           Exsr BuildTerrorCharge;
        Endif;

        //* Domestive Terrorism Charge
        If ELEDTR <> 0;
           Exsr BuildDomTerror;
        Endif;

        //* Harbor Exposure Fee
        If ELHXMN <> 0;
           Exsr BuildHarborExp;
        Endif;

        //* Expense Constant
        If ELPRTF <> 0 and (ELEMNP <> 'Y' or EXBEXP = 'Y');
           Exsr BuildExpenseConst;
        Endif;

        //* Policy Fee
        If ELEPF <> 0;
           Exsr BuildPolicyFee;
        Endif;

        //* Estimated annual premium
        BuildGiantJSON(json :'type' : 'Total');
        BuildGiantJSON(json :'heading' : 'Estimated Annual ' + %trim(premWord));
        BuildGiantJSON(json :'amount' :%editc((ELENMP + ELPRTF + ELETER +
         ELEDTR + ELHXMN + ELEARC + ELEPF):'P'));
        json = %trim(json) + ' } ';

        //* Prorated premium
        If ELPROR <> 1;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : 'Total');
           BuildGiantJSON(json :'factor' :%editc(ELPROR:'P'));
           BuildGiantJSON(json :'heading' : 'Prorated ' + %trim(premWord));
           BuildGiantJSON(json :'amount' :%editc(ELANPR:'P'));
           json = %trim(json) + ' } ';
        Endif;

        //* Fees and taxes

        //* State Tax
        If ELTXDU <> 0 or RRTXPC <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : 'State Tax');
           BuildGiantJSON(json :'percent' :%editc(RRTXPC:'P'));
           If RRTXHD <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RRTXHD));
           Else;
              BuildGiantJSON(json :'heading' : 'State Tax');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(ELTXDU:'P'));
           json = %trim(json) + ' } ';
        Endif;

        //* Membership Fee
        If ELMFAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : 'Membership Fee');
           BuildGiantJSON(json :'heading' : 'Membership Fee');
           BuildGiantJSON(json :'amount' :%editc(ELMFAM:'P'));
           json = %trim(json) + ' } ';
        Endif;

        //* Safety Fee
        If ELMFAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : 'Safety Fee');
           If RRSFHD <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RRSFHD));
           Else;
              BuildGiantJSON(json :'heading' : 'Safety Fee');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(ELSFAM:'P'));
           If ELSFPC <> 0;
              BuildGiantJSON(json :'percent' :%editc(ELSFPC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Assessment #1
        If EL1AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : '1st Assessment');
           If RR1stH <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RR1stH));
           Else;
              BuildGiantJSON(json :'heading' : '1st Assessment');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(EL1AAM:'P'));
           If RR1stP <> 0;
              BuildGiantJSON(json :'percent' :%editc(RR1stP:'P'));
           Endif;
           If RR1stC <> 0;
              BuildGiantJSON(json :'code' :%editc(RR1stC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Assessment #2
        If EL2AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : '2nd Assessment');
           If RR2ndH <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RR2ndH));
           Else;
              BuildGiantJSON(json :'heading' : '2nd Assessment');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(EL2AAM:'P'));
           If RR2ndP <> 0;
              BuildGiantJSON(json :'percent' :%editc(RR2ndP:'P'));
           Endif;
           If RR2ndC <> 0;
              BuildGiantJSON(json :'code' :%editc(RR2ndC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Assessment #3
        If EL3AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : '3rd Assessment');
           If RR3rdH <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RR3rdH));
           Else;
              BuildGiantJSON(json :'heading' : '3rd Assessment');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(EL3AAM:'P'));
           If RR3rdP <> 0;
              BuildGiantJSON(json :'percent' :%editc(RR3rdP:'P'));
           Endif;
           If RR3rdC <> 0;
              BuildGiantJSON(json :'code' :%editc(RR3rdC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Assessment #4
        If EL4AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : '4th Assessment');
           If RR4thH <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RR4thH));
           Else;
              BuildGiantJSON(json :'heading' : '4th Assessment');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(EL4AAM:'P'));
           If RR4thP <> 0;
              BuildGiantJSON(json :'percent' :%editc(RR4thP:'P'));
           Endif;
           If RR4thC <> 0;
              BuildGiantJSON(json :'code' :%editc(RR4thC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Assessment #5
        If EL5AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : '5th Assessment');
           If RR5thH <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RR5thH));
           Else;
              BuildGiantJSON(json :'heading' : '5th Assessment');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(EL5AAM:'P'));
           If RR5thP <> 0;
              BuildGiantJSON(json :'percent' :%editc(RR5thP:'P'));
           Endif;
           If RR5thC <> 0;
              BuildGiantJSON(json :'code' :%editc(RR5thC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Assessment #6
        If EL6AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : '6th Assessment');
           If RR6thH <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RR6thH));
           Else;
              BuildGiantJSON(json :'heading' : '6th Assessment');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(EL6AAM:'P'));
           If RR6thP <> 0;
              BuildGiantJSON(json :'percent' :%editc(RR6thP:'P'));
           Endif;
           If RR6thC <> 0;
              BuildGiantJSON(json :'code' :%editc(RR6thC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Assessment #7
        If EL7AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : '7th Assessment');
           If RR7thH <> ' ';
              BuildGiantJSON(json :'heading' : %trim(RR7thH));
           Else;
              BuildGiantJSON(json :'heading' : '7th Assessment');
           Endif;
           BuildGiantJSON(json :'amount' :%editc(EL7AAM:'P'));
           If RR7thP <> 0;
              BuildGiantJSON(json :'percent' :%editc(RR7thP:'P'));
           Endif;
           If RR7thC <> 0;
              BuildGiantJSON(json :'code' :%editc(RR7thC:'P'));
           Endif;
           json = %trim(json) + ' } ';
        Endif;

        //* Total
        If ELTXDU <> 0 or ELMFAM <> 0 or ELSFAM <> 0 or EL1AAM <> 0 or
         EL2AAM <> 0 or EL3AAM <> 0 or EL4AAM <> 0 or EL5AAM <> 0 or
         EL6AAM <> 0 or EL7AAM <> 0;
           json = %trim(json) + ', { ';
           BuildGiantJSON(json :'type' : 'Total');
           BuildGiantJSON(json :'heading' : 'Total');
           BuildGiantJSON(json :'amount' :%editc((ELANPR + ELTXDU + ELMFAM +
            ELSFAM + EL1AAM + EL2AAM + EL3AAM + EL4AAM + EL5AAM + EL6AAM +
            EL7AAM):'P'));
           json = %trim(json) + ' } ';
        Endif;

        json = %trim(json) + ' ] ';

        json = %trim(json) + ' } ';

       Endsr;

       //=================================================================
       // Export waiver info
       //=================================================================
       Begsr BuildWaiver;
          BuildGiantJSON(json :'type' : 'Waiver Surcharge');
          If RRWVHD = ' ';
             BuildGiantJSON(json :'heading' : 'Waiver Surcharge');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRWVHD));
          Endif;
          BuildGiantJSON(json :'amount' :%editc(ELEWAV:'P'));
          If RRWVAC <> 0;
             BuildGiantJSON(json :'code' :%editc(RRWVAC:'P'));
          Endif;
          If ELWVPC <> 0;
             BuildGiantJSON(json :'percent' :%editc(ELWVPC:'P'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export voluntary labor charge.
       //=================================================================
       Begsr BuildVLC;
          BuildGiantJSON(json :'type' : 'Voluntary Labor Charge');
          If RRVLCH = ' ';
             BuildGiantJSON(json :'heading' : 'Voluntary Labor Charge');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRVLCH));
          Endif;
          BuildGiantJSON(json :'amount' :%editc(ELVLCA:'P'));
          If RRVLCC <> 0;
             BuildGiantJSON(json :'code' :%editc(RRVLCC:'P'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export additional load charge.
       //=================================================================
       Begsr BuildAddLoad;
          BuildGiantJSON(json :'type' : 'Additional Load');
          BuildGiantJSON(json :'heading' : 'Aircraft Surcharge');
          BuildGiantJSON(json :'amount' :%editc(xtadld:'P'));
          If RRDCCD <> 0;
             BuildGiantJSON(json :'code' :%editc(RRDCCD:'P'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export loss constant charge.
       //=================================================================
       Begsr BuildLossConst;
          BuildGiantJSON(json :'type' : 'Loss Constant');
          BuildGiantJSON(json :'heading' : 'Loss Constant');
          BuildGiantJSON(json :'amount' :%editc(ELLOSS:'P'));
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export occupational disease charge.
       //=================================================================
       Begsr BuildOccpDisease;
          BuildGiantJSON(json :'type' : 'Occupational Disease');
          BuildGiantJSON(json :'heading' : 'Occupational Disease Coverage');
          BuildGiantJSON(json :'amount' :%editc(xtoccd:'P'));
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export non-ratable catastrophe loading charge.
       //=================================================================
       Begsr BuildNonRatable;
          BuildGiantJSON(json :'type' : 'Non-Ratable Catastrophe');
          BuildGiantJSON(json :'heading' : 'Non-Ratable Catastrophe Loading');
          BuildGiantJSON(json :'amount' :%editc(xtnrat:'P'));
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export facultative reinsurance charge.
       //=================================================================
       Begsr BuildFaculRein;
          BuildGiantJSON(json :'type' : 'Facultative Reinsurance');
          BuildGiantJSON(json :'heading' : 'Facultative Reinsurance Charge');
          BuildGiantJSON(json :'amount' :%editc(ELEARC:'P'));
          If RRARCC <> 0;
             BuildGiantJSON(json :'code' :%editc(RRARCC:'P'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export terrorism risk act charge.
       //=================================================================
       Begsr BuildTerrorCharge;
          BuildGiantJSON(json :'type' : 'Foreign Terrorism');
          BuildGiantJSON(json :'heading' : 'Foreign Terrorism Charge');
          BuildGiantJSON(json :'amount' :%editc(ELETER:'P'));
          If RRTERC <> 0;
             BuildGiantJSON(json :'code' :%editc(RRTERC:'P'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export domestic terrorism charge.
       //=================================================================
       Begsr BuildDomTerror;
          BuildGiantJSON(json :'type' : 'Domestic Terrorism');
          BuildGiantJSON(json :'heading' : 'Domestic Terrorism Charge');
          BuildGiantJSON(json :'amount' :%editc(ELEDTR:'P'));
          If RRDTRC <> 0;
             BuildGiantJSON(json :'code' :%editc(RRDTRC:'P'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export harbor exposure fee.
       //=================================================================
       Begsr BuildHarborExp;
          BuildGiantJSON(json :'type' : 'Harbor Exposure');
          BuildGiantJSON(json :'heading' : 'Harbor Exposure Fee');
          BuildGiantJSON(json :'amount' :%editc(ELHXMN:'P'));
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export expense constant charge.
       //=================================================================
       Begsr BuildExpenseConst;
          BuildGiantJSON(json :'type' : 'Expense Constant');
          BuildGiantJSON(json :'heading' : 'Expense Constant');
          BuildGiantJSON(json :'amount' :%editc(ELPRTF:'P'));
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export policy fee.
       //=================================================================
       Begsr BuildPolicyFee;
          BuildGiantJSON(json :'type' : 'Policy Fee');
          BuildGiantJSON(json :'heading' : 'Policy Fee');
          BuildGiantJSON(json :'amount' :%editc(ELEPF:'P'));
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export deviation 1
       //=================================================================
       Begsr BuildDeviation1;

          If ELEDC2 <> 0 or (ELEMNP = 'Y' and ELASP2 <> 0);
             Exsr BuildDev1;
          Endif;

          If ELD1E2 <> 0 or (ELEMNP = 'Y' and ELD1S2 <> 0);
             Exsr BuildDev1Split;
          Endif;

       Endsr;

       //=================================================================
       // Export deviation 2
       //=================================================================
       Begsr BuildDeviation2;

          If ELEDV2 <> 0 or (ELEMNP = 'Y' and ELDEV2 <> 0);
             Exsr BuildDev2;
          Endif;

          If ELD2E2 <> 0 or (ELEMNP = 'Y' and ELD2S2 <> 0);
             Exsr BuildDev2Split;
          Endif;

       Endsr;

       //=================================================================
       // Export deviation #1 info
       //=================================================================
       Begsr BuildDev1;
          BuildGiantJSON(json :'type' : 'Deviation #1');
          If RRDV1H = ' ';
             BuildGiantJSON(json :'heading' : 'Deviation #1');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRDV1H));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDC2):'P'));
          BuildGiantJSON(json :'code' :%editc(RRDV1C:'P'));
          BuildGiantJSON(json :'percent' :%editc(ELASP2:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELDVEF):
           'X'));
          If ELDVEX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELDVEX):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export deviation #1 split info
       //=================================================================
       Begsr BuildDev1Split;
          BuildGiantJSON(json :'type' : 'Deviation #1 Split');
          If RRDV1H = ' ';
             BuildGiantJSON(json :'heading' : 'Deviation #1');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRDV1H));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELD1E2):'P'));
          BuildGiantJSON(json :'code' :%editc(RRDV1C:'P'));
          BuildGiantJSON(json :'percent' :%editc(ELD1S2:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELD1D2):
           'X'));
          If ELD1X2 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELD1X2):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export deviation #2 info
       //=================================================================
       Begsr BuildDev2;
          BuildGiantJSON(json :'type' : 'Deviation #2');
          If RRDV2H = ' ';
             BuildGiantJSON(json :'heading' : 'Deviation #2');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRDV2H));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDV2):'P'));
          BuildGiantJSON(json :'code' :%editc(RRDV2C:'P'));
          BuildGiantJSON(json :'percent' :%editc(ELDEV2:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELD2EF):
           'X'));
          If ELD2EX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELD2EX):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export deviation #2 split info
       //=================================================================
       Begsr BuildDev2Split;
          BuildGiantJSON(json :'type' : 'Deviation #2 Split');
          If RRDV2H = ' ';
             BuildGiantJSON(json :'heading' : 'Deviation #2');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRDV2H));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELD2E2):'P'));
          BuildGiantJSON(json :'code' :%editc(RRDV2C:'P'));
          BuildGiantJSON(json :'percent' :%editc(ELD2S2:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELD2D2):
           'X'));
          If ELD2X2 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELD2X2):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export increased limits #1 info
       //=================================================================
       Begsr BuildIL1;
          BuildGiantJSON(json :'type' : 'Increased Limits');
          xxKey = ELILCD;
          Setgt (ELCO#:ELFND:RSJRST:xxKey:ELPFDT) plm01;
          Readpe (ELCO#:ELFND:RSJRST:xxKey) plm01;

          BuildGiantJSON(json :'heading' : 'Increased Limits');
          BuildGiantJSON(json :'amount' :%editc(ELEIN1:'P'));
          If LMCLAS <> 0;
             BuildGiantJSON(json :'code' :%editc(LMCLAS:'P'));
          Endif;
          BuildGiantJSON(json :'percent' :%editc(LMPCT:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELILEF):
              'X'));
          Select;
          When ELILEX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELILEX):
              'X'));
          When ELI2EF <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELI2EF):
              'X'));
          Other;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endsl;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export increased limits #2 info
       //=================================================================
       Begsr BuildIL2;
          BuildGiantJSON(json :'type' : 'Increased Limits 2');
          xxKey = ELINL2;
          Setgt (ELCO#:ELFND:RSJRST:xxKey:ELPFDT) plm01;
          Readpe (ELCO#:ELFND:RSJRST:xxKey) plm01;

          BuildGiantJSON(json :'heading' : '2nd Increased Limits');
          BuildGiantJSON(json :'amount' :%editc(ELEIN2:'P'));
          If LMCLAS <> 0;
             BuildGiantJSON(json :'code' :%editc(LMCLAS:'P'));
          Endif;
          BuildGiantJSON(json :'percent' :%editc(LMPCT:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELI2EF):
           'X'));
          Select;
          When ELI2EX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELI2EX):
              'X'));
          Other;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endsl;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export small deductible #1 info
       //=================================================================
       Begsr BuildSmallDed1;
          BuildGiantJSON(json :'type' : 'Deductible Discount');
          Setgt (ELCO#:ELFND:RSJRST:ELDCDE:xxEFDT) pdd02;
          Readpe (ELCO#:ELFND:RSJRST:ELDCDE) pdd02;

          BuildGiantJSON(json :'heading' : 'Deductible Discount');
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDCT):'P'));
          If DDCLAS <> 0;
             BuildGiantJSON(json :'code' :%editc(DDCLAS:'P'));
          Endif;
          BuildGiantJSON(json :'percent' :%editc(ELDDCT:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELDCEF):
           'X'));
          Select;
          When ELDCEX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELDCEX):
              'X'));
          When EL2DEF <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(EL2DEF):
              'X'));
          Other;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endsl;
          json = %trim(json) + ' }, { ';
       Endsr;
       //=================================================================
       // Export small deductible #2 info
       //=================================================================
       Begsr BuildSmallDed2;
          BuildGiantJSON(json :'type' : 'Deductible Discount 2');
          Setgt (ELCO#:ELFND:RSJRST:ELDCD2:xxEFDT) pdd02;
          Readpe (ELCO#:ELFND:RSJRST:ELDCD2) pdd02;

          BuildGiantJSON(json :'heading' : '2nd Deductible Discount');
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDD2):'P'));
          If DDCLAS <> 0;
             BuildGiantJSON(json :'code' :%editc(DDCLAS:'P'));
          Endif;
          BuildGiantJSON(json :'percent' :%editc(ELDD2P:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(EL2DEF):
           'X'));
          Select;
          When EL2DEX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(EL2DEX):
              'X'));
          Other;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endsl;
          json = %trim(json) + ' }, { ';
       Endsr;
       //=================================================================
       // Export large deductible #1 info
       //=================================================================
       Begsr BuildLargeDed1;

          BuildGiantJSON(json :'type' : 'Large Deductible Discount');
          BuildGiantJSON(json :'heading' : 'Large Deductible Discount');
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDCT):'P'));
          If LDCLS1 <> 0;
             BuildGiantJSON(json :'code' :%editc(LDCLS1:'P'));
          Endif;
          BuildGiantJSON(json :'percent' :%editc(LDPCT1:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(LDEFD1):
           'X'));
          Select;
          When LDEXP1 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(LDEXP1):
              'X'));
          When LDEFD2 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(LDEFD2):
              'X'));
          Other;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endsl;
          json = %trim(json) + ' }, { ';
       Endsr;
       //=================================================================
       // Export large deductible #2 info
       //=================================================================
       Begsr BuildLargeDed2;

          BuildGiantJSON(json :'type' : 'Large Deductible Discount 2');
          BuildGiantJSON(json :'heading' : '2nd Large Deductible Discount');
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDD2):'P'));
          If LDCLS2 <> 0;
             BuildGiantJSON(json :'code' :%editc(LDCLS2:'P'));
          Endif;
          BuildGiantJSON(json :'percent' :%editc(LDPCT2:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(LDEFD2):
           'X'));
          Select;
          When LDEXP2 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(LDEXP2):
              'X'));
          Other;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endsl;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export managed care credit info
       //=================================================================
       Begsr BuildMCCredit;
          BuildGiantJSON(json :'type' : 'Subject Premium Credit 1A');
          If RR1AHD = ' ';
             BuildGiantJSON(json :'heading' : 'Managed Care Credit');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RR1AHD));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDCM):'P'));
          BuildGiantJSON(json :'code' :%editc(RR1ACD:'P'));
          BuildGiantJSON(json :'percent' :%editc(ELASPM:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELMCEF):
           'X'));
          If ELMCEX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELMCEX):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export safety credit info
       //=================================================================
       Begsr BuildSafetyCredit;
          BuildGiantJSON(json :'type' : 'Subject Premium Credit 1B');
          If RR1BHD = ' ';
             BuildGiantJSON(json :'heading' : 'Safety Credit');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RR1BHD));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDCS):'P'));
          BuildGiantJSON(json :'code' :%editc(RR1BCD:'P'));
          BuildGiantJSON(json :'percent' :%editc(ELASPS:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELSCEF):
           'X'));
          If ELSCEX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELSCEX):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export contractors credit info
       //=================================================================
       Begsr BuildContCredit;
          BuildGiantJSON(json :'type' : 'Contractors Credit');
          If RRCCHD = ' ';
             BuildGiantJSON(json :'heading' : 'Contractors Credit');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRCCHD));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDC1):'P'));
          BuildGiantJSON(json :'code' :%editc(RRCCCD:'P'));
          BuildGiantJSON(json :'percent' :%editc(ELASP1:'P'));
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELCREF):
           'X'));
          If ELCREX <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELCREX):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export stock discount info
       //=================================================================
       Begsr BuildStockDisc;
          BuildGiantJSON(json :'type' : 'Stock Discount');
          If RRDSHD = ' ';
             BuildGiantJSON(json :'heading' : 'Stock Discount');
          Else;
             BuildGiantJSON(json :'heading' : %trim(RRDSHD));
          Endif;
          BuildGiantJSON(json :'amount' :%editc((-1 * ELEDC3):'P'));
          BuildGiantJSON(json :'code' :%editc(63:'P'));
          If ELASP3 <> 0;
             BuildGiantJSON(json :'percent' :%editc(ELASP3:'P'));
          Else;
             BuildGiantJSON(json :'percent' :%editc(ELDISP:'P'));
          Endif;
          json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export schedule db/cr info
       //=================================================================
       Begsr BuildSchedDBCR;
          BuildGiantJSON(json :'type' : 'Schedule DB/CR');
         Select;
          When ELDBCR > 1;
             If RRDBHD = ' ';
                BuildGiantJSON(json :'heading' : 'Schedule Debit');
             Else;
                BuildGiantJSON(json :'heading' : %trim(RRDBHD));
             Endif;
             BuildGiantJSON(json :'code' :%editc(RRDBCD:'P'));
          When ELDBCR < 1;
             If RRCRHD = ' ';
                BuildGiantJSON(json :'heading' : 'Schedule Credit');
             Else;
                BuildGiantJSON(json :'heading' : %trim(RRCRHD));
             Endif;
             BuildGiantJSON(json :'code' :%editc(RRCRCD:'P'));
          Other;
             BuildGiantJSON(json :'heading' : 'Schedule DB/CR');
         Endsl;
         BuildGiantJSON(json :'amount' :%editc(ELEDC:'P'));
         BuildGiantJSON(json :'factor' :%editc(ELDBCR:'P'));
         BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELSDEF):
          'X'));
         If ELSDEX <> 0;
            BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELSDEX):
             'X'));
         Else;
            BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
             'X'));
         Endif;
         json = %trim(json) + ' }, { ';
       Endsr;

       //=================================================================
       // Export Experience Modifiers
       //=================================================================
       Begsr BuildExpMods;

       //* First modifier
        If ELEXM1 <> 0;
          BuildGiantJSON(json :'type' : 'Experience Modifier');
          BuildGiantJSON(json :'type2' : 'Total');
          If ELEXM2 <> 0;
             If ELPROM = 'M';
                BuildGiantJSON(json :'heading' : 'Merit Rating #1');
             Else;
                BuildGiantJSON(json :'heading' : 'Experience Modification #1');
             Endif;
          Else;
             If ELPROM = 'M';
                BuildGiantJSON(json :'heading' : 'Merit Rating');
             Else;
                BuildGiantJSON(json :'heading' : 'Experience Modification');
             Endif;
          Endif;
          BuildGiantJSON(json :'amount' :%editc(ELEXP1:'P'));
          BuildGiantJSON(json :'factor' :%editc(ELEXM1:'P'));
          If ELEMD1 <> 0;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELEMD1):
              'X'));
          Else;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELPFDT):
              'X'));
          Endif;
          If ELEMD2 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELEMD2):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
        Endif;

       //* Second modifier
        If ELEXM2 <> 0;
          BuildGiantJSON(json :'type' : 'Experience Modifier');
          BuildGiantJSON(json :'type2' : 'Total');
          If ELPROM = 'M';
             BuildGiantJSON(json :'heading' : 'Merit Rating #2');
          Else;
             BuildGiantJSON(json :'heading' : 'Experience Modification #2');
          Endif;
          BuildGiantJSON(json :'amount' :%editc(ELEXP2:'P'));
          BuildGiantJSON(json :'factor' :%editc(ELEXM2:'P'));
          If ELEMD2 <> 0;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELEMD2):
              'X'));
          Endif;
          If ELEMD3 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELEMD3):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
        Endif;

       //* Third modifier
        If ELEXM3 <> 0;
          BuildGiantJSON(json :'type' : 'Experience Modifier');
          BuildGiantJSON(json :'type2' : 'Total');
          If ELPROM = 'M';
             BuildGiantJSON(json :'heading' : 'Merit Rating #3');
          Else;
             BuildGiantJSON(json :'heading' : 'Experience Modification #3');
          Endif;
          BuildGiantJSON(json :'amount' :%editc(ELEXP3:'P'));
          BuildGiantJSON(json :'factor' :%editc(ELEXM3:'P'));
          If ELEMD3 <> 0;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELEMD3):
              'X'));
          Endif;
          BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
           'X'));
          json = %trim(json) + ' }, { ';
        Endif;

       Endsr;
       //=================================================================
       // Export ARAP factors charge
       //=================================================================
       Begsr BuildARAP;

       //* First factor
        If ELARP1 <> 0;
          If ELARP2 <> 0;
             BuildGiantJSON(json :'heading' : 'ARAP Charge #1');
          Else;
             BuildGiantJSON(json :'heading' : 'ARAP Charge');
          Endif;
          BuildGiantJSON(json :'type' : 'ARAP');
          BuildGiantJSON(json :'amount' :%editc(ELRPE1:'P'));
          BuildGiantJSON(json :'factor' :%editc(ELARP1:'P'));
          If ELEMD1 <> 0;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELEMD1):
              'X'));
          Else;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELPFDT):
              'X'));
          Endif;
          If ELEMD2 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELEMD2):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
        Endif;

       //* Second factor
        If ELARP2 <> 0;
          BuildGiantJSON(json :'type' : 'ARAP');
          BuildGiantJSON(json :'heading' : 'ARAP Charge #2');
          BuildGiantJSON(json :'amount' :%editc(ELRPE2:'P'));
          BuildGiantJSON(json :'factor' :%editc(ELARP2:'P'));
          If ELEMD2 <> 0;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELEMD2):
              'X'));
          Endif;
          If ELEMD3 <> 0;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELEMD3):
              'X'));
          Else;
             BuildGiantJSON(json :'expiration' :%editc(DtMDYY(ELPTDT):
              'X'));
          Endif;
          json = %trim(json) + ' }, { ';
        Endif;

       //* Third factor
        If ELARP3 <> 0;
          BuildGiantJSON(json :'type' : 'ARAP');
          BuildGiantJSON(json :'heading' : 'ARAP Charge #3');
          BuildGiantJSON(json :'amount' :%editc(ELRPE3:'P'));
          BuildGiantJSON(json :'factor' :%editc(ELARP3:'P'));
          If ELEMD3 <> 0;
             BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELEMD3):
              'X'));
          Endif;
          BuildGiantJSON(json :'effective' :%editc(DtMDYY(ELPTDT):
           'X'));
          json = %trim(json) + ' }, { ';
        Endif;

       Endsr;

      /End-Free
