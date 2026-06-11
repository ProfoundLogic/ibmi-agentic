     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H*================================================================
     H/TITLE LODHUBPRC - Load request information to SIHUBPRC
     H*================================================================
     H* Products                       Sub-Products
     H* ========                       ============
     H* Quote
     H* FROI
     H* S/R Online
     H* Consumer                       Policy,Claims,Billing
     H* Reports                        Policy,Claims
     H* iDARTS                         Policy,Claims,Agent
     H*================================================================
      * Revisions:
      *
      * 06/12/23 C:062 Project: WC2879
      *                Add ProcessFailedBIFRequest
      *
      * 12/08/21 C:136 Project: CN028
      *                Add ReportAccidentYearLossRecap
      *
      * 11/03/21 C:291 Project: CN020
      *                Add ReportLossSummary
      *
      * 10/18/21 C:33  Project: CN026
      *                -iDARTS:Agent
      *
      * 07/07/21 C:062 Project WC2781
      *                -DeleteClaim
      *                -GetClaim
      *                -SaveClaim
      *
      * 03/22/21 C:062 Project WC2663
      *                -GetImageAR
      *                -PutNotificationsAR
      *                -SaveOptOutInAR
      *                -GetCurrentBalanceAR
      *
      * 09/02/20 C:053 Project SY424
      *                Add requests for payments via invoice cloud
      *
      *================================================================
     D isDevMachine    s               n
     D isClearFile     s               n   inz('1')
     D sysDt7          s              7  0
     D sysTime6        s              6  0
     D psds          esds                  extname(szq1p)

     DCheckObj         pr                  extpgm('SRCHKOBJ')
     D                               10    const
     D                                7    const
     D                                1
     DChkDevMachine    pr                  ExtPgm('SACHKPDS')
     D dev                            1n

     dCopyDft          pr                  extPgm('CPYIFSC')
     d inObjFrom                   1024    const
     d inObjTo                     1024    const
     d outRtCd                        2

     D/copy sPrDate
     D/copy sPrFiles
     D/copy sPrRunCmd
     D/copy sPrSql

      /Free
       Exsr PgmStart;

       //***  Begin Add  ***  10/18/21  **************
       AddRequest('SIDARTS':'*ROLE ONLY 3':'iDARTS':'Agent');
       //***  End   Add  ***  10/18/21  **************
       AddRequest('SIDARTS':'*ROLE ONLY 1':'iDARTS':'Claim');
       AddRequest('SIDARTS':'*ROLE ONLY 2':'iDARTS':'Policy');
       //***  Begin Chg  ***  10/18/21  **************
       //AddRequest('SIDARTS':'GetRemark':'Consumer');
       //AddRequest('SIDARTS':'ListRemarks':'Consumer');
       AddRequest('SIDARTS':'GetRemark':'iDARTS');
       AddRequest('SIDARTS':'ListRemarks':'iDARTS');
       //***  End   Chg  ***  10/18/21  **************

       AddRequest('SISSO':'GetUserInfoSSO');
       AddRequest('SISSO':'SaveUserInfoSSO');

       AddRequest('SIUSERS':'GetiSeriesUser');
       AddRequest('SIUSERS':'ListiSeriesUsers');

       AddRequest('WIAGENCY':'ListAgencies');
       AddRequest('WIAGENCY':'ListCommissions':'Consumer':'Commissions');
       //AddRequest('WIAGENCY':'ListCommissionPayments');

       AddRequest('WIALERTS':'GetSRAlerts':'S/R Online');

       AddRequest('WIAPPCOMM':'ListAppComments':'Quote');
       AddRequest('WIAPPCOMM':'SaveAppComments':'Quote');

       AddRequest('WIBILLAR':'ListBilling':'Quote');
       AddRequest('WIBILLAR':'ListBillingDetail':'Consumer':'Billing');
       AddRequest('WIBILLAR':'ListBillingDetail':'Quote');

       AddRequest('WICARRAT':'GetCarrierRating':'Quote');

       AddRequest('WICLAIMS':'GetClaimSummary':'Consumer':'Claims');
       AddRequest('WICLAIMS':'ListClaimPayments':'Consumer'
        :'Claims Transactions');
       AddRequest('WICLAIMS':'ListClaims':'Consumer':'Claims');
       AddRequest('WICLAIMS':'DeleteClaim':'Consumer':'Claims');
       AddRequest('WICLAIMS':'GetClaim':'Consumer':'Claims');
       AddRequest('WICLAIMS':'SaveClaim':'Consumer':'Claims');

       AddRequest('WIFROI':'GetFROI':'FROI');
       AddRequest('WIFROI':'SaveFROI':'FROI');
       AddRequest('WIFROI':'SubmitFROI':'FROI');
       AddRequest('WIFROI':'PrintFROI':'FROI');
       AddRequest('WIFROI':'ListFROI':'FROI');
       AddRequest('WIFROI':'DeleteFROI':'FROI');
       AddRequest('WIFROI':'ListFROIHistory':'FROI');

       AddRequest('WIGROUP':'ListCarriers');
       AddRequest('WIGROUP':'ListGroups');

       AddRequest('WIPOLICY':'CheckFEIN':'Quote');
       AddRequest('WIPOLICY':'GetApplication':'Quote');
       AddRequest('WIPOLICY':'GetDivisionDetail':'Consumer':'Policy');
       AddRequest('WIPOLICY':'GetPolicyDetail':'Consumer':'Policy');
       AddRequest('WIPOLICY':'ListApplications':'Quote');
       AddRequest('WIPOLICY':'ListEmployers':'Consumer':'Policy');
       AddRequest('WIPOLICY':'ListEmployers':'S/R Online');
       AddRequest('WIPOLICY':'ListLocations':'Consumer':'Policy');
       AddRequest('WIPOLICY':'ListLocations':'FROI');
       AddRequest('WIPOLICY':'ListPolicies':'Consumer':'Policy');
       AddRequest('WIPOLICY':'ListPolicyDivisions':'Consumer':'Policy');
       AddRequest('WIPOLICY':'ListPolicyDivisions':'FROI');
       AddRequest('WIPOLICY':'ListPolicyYears':'Consumer':'Policy');
       AddRequest('WIPOLICY':'ListQuoteStatusHistory':'Quote');
       AddRequest('WIPOLICY':'PrintAcordForm':'Quote');
       AddRequest('WIPOLICY':'SaveApplicationData':'Quote');

      //*** beg add *** 09/02/20 *************************
       AddRequest('WIPOLPMT':'ListIncompleteInvoices');
       AddRequest('WIPOLPMT':'ProcessPayment':'Consumer':'Billing');
       AddRequest('WIPOLPMT':'ProcessThirdPartyPayment');
      //*** end add *** 09/02/20 *************************
      //*** Begin Add *** 06/12/23 *****************************************
       AddRequest('WIPOLPMT':'ProcessFailedBIFRequest');
      //*** End   Add *** 06/12/23 *****************************************

       AddRequest('WIPROD':'ListProducers');

       AddRequest('WIQUEST':'ListQuestions':'Quote');

       AddRequest('WIQTEBILL':'PrintQuote':'Quote');

       AddRequest('WIRATECMP':'GetRateComparison':'Quote');

       AddRequest('WIRATE':'RateApplication':'Quote');

       AddRequest('WIREPORTS':'ReportBookOfBusiness':'Reports':'Policy');
       //*** Beg Add *** 11/03/21 *************************
       AddRequest('WIREPORTS':'ReportLossSummary':'Reports':'Claims');
       //*** End Add *** 11/03/21 *************************
       //*** Beg Add *** 12/08/21 *************************
       AddRequest('WIREPORTS':'ReportAccidentYearLossRecap':'Reports':'Claims');
       //*** End Add *** 12/08/21 *************************

       AddRequest('WISBMQTE':'ChangeStatus':'Quote');
       AddRequest('WISBMQTE':'SubmitQuote':'Quote');

       AddRequest('WISRPAY':'CalculateSRBalance':'S/R Online');
       AddRequest('WISRPAY':'CreateSRBatches':'S/R Online');
      //*** beg del *** 09/02/20 *************************
      //*** AddRequest('WISRPAY':'ListIncompleteSRPayments':'S/R Online');
      //*** end del *** 09/02/20 *************************
       AddRequest('WISRPAY':'ListSRPayrollClasses':'S/R Online');
       AddRequest('WISRPAY':'ListSRPayrollPeriods':'S/R Online');
       AddRequest('WISRPAY':'PrintSRWorksheet':'S/R Online');
      //*** beg del *** 09/02/20 *************************
      //*** AddRequest('WISRPAY':'ProcessThirdPartyPayment':'S/R Online');
      //*** end del *** 09/02/20 *************************
       AddRequest('WISRPAY':'ProcessSRPayments':'S/R Online');
       AddRequest('WISRPAY':'SaveSRPayrolls':'S/R Online');

       AddRequest('WITABLE':'ListBillingPlans');
       AddRequest('WITABLE':'ListExposureCodes');
       AddRequest('WITABLE':'ListQuoteStatuses');
       AddRequest('WITABLE':'ListStates');
       AddRequest('WITABLE':'ListTableValues');

       AddRequest('WICLMRPTS':'ReportClaimSize':'Reports':'Claims');
       AddRequest('WICLMRPTS':'ReportLossExperience':'Reports':'Claims');
       AddRequest('WICLMRPTS':'ReportLossExperienceAsOf':'Reports':'Claims');
       AddRequest('WICLMRPTS':'ReportTop20Claims':'Reports':'Claims');

       AddRequest('WIAR':'GetImageAR':'Consumer':'Billing');
       AddRequest('WIAR':'PutNotificationsAR':'Consumer':'Billing');
       AddRequest('WIAR':'SaveOptOutInAR':'Consumer':'Billing');
       AddRequest('WIAR':'GetCurrentBalanceAR':'Consumer':'Billing');

       Exsr AddFileMembers;

       *InLr = *On;

       //========================================================
       Begsr AddFileMembers;
        AddFileMember('WAQTP'    :*blank     :'HISTORY');
        AddFileMember('WAQTP'    :*blank     :'STATUS');
        AddFileMember('WASRP'    :*blank     :'HOLD');
        AddFileMember('WDEAP'    :'WDEAL'    :'HOLD'   :'WDEAL_HLD');
        AddFileMember('WDEAP'    :'WDEAL4'   :'HOLD'   :'WDEAL4_HLD');
        AddFileMember('WDEAP'    :'WDEAL30'  :'HOLD'   :'WDEAL30HLD');
        AddFileMember('WHFRP'    :*blank     :'HISTORY');
        AddFileMember('WHFRP'    :*blank     :'HOLD');

        CreateAliasSQL('WAQTP_HST':'WAQTP':'HISTORY':*on);
        CreateAliasSQL('WAQTP_STS':'WAQTP':'STATUS':*on);
        CreateAliasSQL('WASRP_HLD':'WASRP':'HOLD':*on);
        CreateAliasSQL('WDEAP_HLD':'WDEAP':'HOLD':*on);
        CreateAliasSQL('WHFRP_HST':'WHFRP':'HISTORY':*on);
        CreateAliasSQL('WHFRP_HLD':'WHFRP':'HOLD':*on);
       Endsr;

       //========================================================
       Begsr PgmStart;
        ChkDevMachine(isDevMachine);
        Exec SQL
         set option commit = *none;
        sysDt7 = DateTo7(SystemDate);
        sysTime6 = TimeTo6(SystemTime);

        If isClearFile;
           Exec SQL
            delete from SIHUBPRC;
      //*** beg add *** 09/02/20 *************************
           Exec SQL
            delete from SIHUBPRCA;
      //*** end add *** 09/02/20 *************************
        Endif;
       Endsr;
      /End-free

     P*====================================================================
     PAddFileMember    b
     DAddFileMember    pi
     D inPF                          10    const varying
     D inLF                          10    const varying
     D inPhysicalMbr                256    const varying
     D inLogicalMbr                 256    const varying options(*nopass)

     DoutError         s              1
     DstrCmd           s            456    varying

      /Free
       Callp CheckObj(inPF:'*FILE':outError);
       If inLF <> *blank;
          Callp CheckObj(inLF:'*FILE':outError);
       Endif;
       Select;
       When outError = '1';
          Return;
       Endsl;

       strCmd = 'ADDPFM FILE(' + inPF + ') MBR(' + inPhysicalMbr + ')';
       Monitor;
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
       If inLF <> *blank;
          strCmd = 'ADDLFM FILE(' + inLF + ') MBR(' + inLogicalMbr + ') ' +
           'DTAMBRS((' + inPF + ' (' + inPhysicalMbr + ')))';
          Monitor;
             RunCmd(strCmd:%len(strCmd));
          On-error;
          Endmon;
       Endif;

      /End-free
     PAddFileMember    e

     P*====================================================================
     PAddRequest       b
     DAddRequest       pi
     D inProgram                     10    const
     D inRequest                    256    const
     D inProduct                     25    const options(*nopass)
     D inSubProduct                  25    const options(*nopass)

     DdsSIHUBPRC     e ds                  extname(SIHUBPRC)
     D outError        s              1
     D strProduct      s             25    inz
     D strSubProduct   s             25    inz

      /Free
       Callp CheckObj(inProgram:'*PGM':outError);
       Select;
       When outError = '1';
          Return;
       Endsl;
       If %parms >= 3;
          strProduct = inProduct;
       Endif;
       If %parms >= 4;
          strSubProduct = inSubProduct;
       Endif;
       If strProduct = *blanks;
          Reset strSubProduct;
       Endif;
       Exec SQL
        select * into :dsSIHUBPRC
         from SIHUBPRC
         where SIREQUEST = :inRequest;
       If IsValidFetch(sqlstt);
          Return;
       Endif;

       Exec SQL
        insert into SIHUBPRC
         values(:inRequest, :inProgram, 'A',
         :q1Pgm, :q1User, :sysDt7, :sysTime6,
         :q1Pgm, :q1User, :sysDt7, :sysTime6
         );

       Exsr AddAssociations;

       AddTestDir(inRequest);

       //=============================
       Begsr AddAssociations;
        If strProduct = *blanks;
           Leavesr;
        Endif;
        Exec SQL
         insert into SIHUBPRCA
          values(:inRequest, :strProduct, :strSubProduct, 'A',
          :q1Pgm, :q1User, :sysDt7, :sysTime6,
          :q1Pgm, :q1User, :sysDt7, :sysTime6
          );
       Endsr;
      /End-free
     PAddRequest       e

     P*====================================================================
     PAddTestDir       b
     DAddTestDir       pi
     D inRequest                    256    const

     DoutRtCd          s              2a   inz
     DstrCmd           s            456
     DstrPathDft       s            256a
     DstrPathRequest   s            256a

      /Free
       Select;
       When not isDevMachine;
          Return;
       Endsl;

       strCmd = 'CRTDIR DIR(''/www/portal/' + %trim(inRequest) + ''')';
       Monitor;
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
       strPathDft = '/www/portal/request.txt';
       strPathRequest = '/www/portal/' + %trim(inRequest) + '/request.txt';
       CopyDft(strPathDft:strPathRequest:outRtCd);

      /End-free
     PAddTestDir       e
