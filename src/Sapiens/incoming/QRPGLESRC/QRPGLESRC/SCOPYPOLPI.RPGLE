      * copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
      * StoneRiver is a trademark of StoneRiver, Inc.')
    *===============================================================*
     * SCPYWACTPI Procedure Interfaces for Action pannel of all Policy pgms.
     *========================================================================
    *===============================================================*
    *  Created   : 03/07/12                                         *
    *  Programmer: C:190                                            *
    *  Project   : WC2126                                           *
    *===============================================================*
     *========================================================================
     * REVISIONS:
     *
     * 10/09/15 C:298 Support: 28149
     *                Added Email link for the agency and producer
      *                on Policy action Panel
     *
     * 04/29/15 C:65  Project: SY251
     *                Added hidden field for Scriptura Module flag.
     *
     * 02/12/15 C:282 Project: WC2335
     *                Added the handling to hide the policy menu option from
     *                action panel for Endorsement mdoule.
     *
     * 11/08/13 C:295 Support: 83334
     *                Using GetCxdate to fetch the original cancellation date to
     *                action panel.
     *
     * 10/08/13 C:279 Support: 83531
     *                Use Policy year to get Billing status.
     *
     * 09/20/13 C:191 Redmine: 2826
     *                Use scActMode to create URL instead of hardcoded
     *                'D' value
     *
     * 06/12/13 C:65  PROJECT: REDMINE #2961
     *                Move the bill status procedure call so it uses the
     *                proper RRN# when the action panel is called from
     *                within a revised bill.
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *========================================================================
      *========================================================================
      * Procedure Prototypes
      *========================================================================

      *------------------------------------------------------------------------
       // Load Action Bar - Load the action bar fields.
      *------------------------------------------------------------------------
     P LoadActionBar   B
     D LoadActionBar   PI
      ****** Beg Add ******** 02/12/15 **************************************
     D inPolPln                       1n   options(*nopass)
      ****** End Add ******** 02/12/15 **************************************


     D ##RRN2          s              9
     D ##Mode          s              1    Inz('D')
      /free

       //****** Beg Add ******** 02/12/15 **************************************
       indPolDis = *off;
       If %parms > 0;
          If inPolPln  = *on;
             indPolDis = *on;
          Endif;
       Endif;
       //****** End Add ******** 02/12/15 **************************************

       scCo#  = ELco#;
       scFnd  = ELfnd;
       scEmp# = ELemp#;
       scDiv  = ELdiv;
       scFyr  = ELfyr;

       scActRRN = xxRRN;
       //*** begin add ***  04/29/15  *******************************************
       GetScriptura(scScriptur);
       //*** end addd ****  04/29/15  *******************************************

       //  DBA Name
       xxNmType= 'DBA';
       xxEmpDba = GetEmpFullName(scEmp#:scDiv:x#RRN:scFnd:scCo#:xxNmType);
       scEmpDbaNm = xxEmpDba;
       //scEmpDbaN2 =  %subst(xxEmpDba:51);
       tlEmpDbaNm = xxEmpDba;

       //  Federal Name
       xxNmType= 'FED';
       xxEmpFed = GetEmpFullName(scEmp#:scDiv:x#RRN:scFnd:scCo#:xxNmType);
       scEmpFedNm = xxEmpFed;
       //scEmpFedN2 =  %subst(xxEmpFed:51);
       tlEmpFedNm = xxEmpFed;

       Clear scHdgFedNm;
       If scEmpFedNm <> *blanks;
          scHdgFedNm = 'Federal Name';
       Endif;

       Clear scHdgPol#;
       Clear scFile#;
       Clear scPolicy#;
       Clear scFile#;

       policy# = GetCarrPol(scCO#:scFND:scEMP#:scDIV:scFYR);
       If  policy# <> *blanks;
          scHdgPol#  = 'Carrier Policy #';
          scPolicy#  = policy#;
          scHdgFile# = 'File Number';
          scFile# = %editc(scFnd : 'X') + %editc(scEmp# : 'X')
           + %editc(scFyr : 'X');
       Else;
          scHdgPol#  = 'Policy Number';
          scPolicy# = %editc(scFnd : 'X') + %editc(scEmp# : 'X')
           + %editc(scFyr : 'X');
       Endif;
       If scenv <> 'Q';
          If GetRnwlQte(scCO#:scFND:scEMP#:scDIV)='Y';
             rnwlQuoteP = *On;
             inenv = 'W';
             actCo# = scCo#;
             actFnd =scFnd;
             actEmp# =scEmp#;
             actDiv  =scDiv;
             WTQLRRN(actCO#:actFND:actEMP#:actDIV:inEnv:outRRn);
             strRRN =  %Editc(outRRN:'X');
             empName = GetEmpName(scEmp#:scDiv:outRRN:scfnd:scCo#);
          //*** Beg Chg ****** 09/20/13 *******************************************
          //*****  scRnwlQte = CreateQteURL('STDRVPGM':empName:strRRN:'D':
          //*****    'WTPOLSUM':'00000':'Q');
             scRnwlQte = CreateQteURL('STDRVPGM':empName:strRRN:scActMode:
              'WTPOLSUM':'00000':'Q');
          //*** End Chg ****** 09/20/13 *******************************************
          Else;
             scRnwlQte = ' ';
             rnwlQuoteP = *Off;
          Endif;
       Endif;

       //*
       If scenv = 'Q';
          scPolicy# = %editc(scFnd : 'X') + %editc(scEmp# : 'X')
           + %editc(scFyr : 'X');
          If GetRnwlQte(scCO#:scFND:scEMP#:scDIV)='Y';
             rnwlQuote = *On;
             inenv = 'Q';
             actCo# = scCo#;
             actFnd =scFnd;
             actEmp# =scEmp#;
             actDiv  =scDiv;
             WTQLRRN(actCO#:actFND:actEMP#:actDIV:inEnv:outRRn);
              strRRN =  %Editc(outRRN:'X');
             empName = GetEmpName(scEmp#:scDiv:outRRN:scfnd:scCo#);
          //*** Beg Chg ****** 09/20/13 *******************************************
          //******** scPdnURL = CreateQteURL('STDRVPGM':empName:strRRN:'D':
          //********    'WTPOLSUM');
             scPdnURL = CreateQteURL('STDRVPGM':empName:strRRN:scActMode:
              'WTPOLSUM');
          //*** End Chg ****** 09/20/13 *******************************************
          Else;
             scPdnURL = ' ';
          rnwlQuote = *Off;
          Endif;
       Endif;


       scDIV# = %editc(ELdiv : 'X');

       incDate7 = GetEmIncDt(scCO#:scFND:scEMP#:scDIV);
       scIncpDt =  CvtToDate(incDate7);

       Clear scPolSts;
       Clear scHdgCx;
       Clear scCxDt;

       //*** Beg chg ***  11/08/13  *******************************************
       //*** cxDate7 = GetCxDate1(scCO#:scFND:scEMP#:scDIV:scFYR:xxcfdt:xxredt);
       GetCxDate1(scCO#:scFND:scEMP#:scDIV:scFYR:xxcfdt:xxredt);
       cxDate7 = GetCxDate(scCO#:scFND:scEMP#:scDIV);
       //*** End chg ***  11/08/13  *******************************************

       If xxredt = 0;
          If cxDate7 <> 0;
             scCxDt =  CvtToDate(cxDate7);
             scHdgCx = 'Cancellation Date';
          Endif;
          If xxcfdt <> 0;
             scCxDt =  CvtToDate(cxDate7);
             scHdgCx = 'Confirmed Cancel Date';
          Endif;
       Endif;

       If scenv = 'Q'and EMrqfl = 'Y';
          rnwlQuote = *On;
       Endif;

       //* Poicy/Quote Status.
       scPolSts = GetQuoteStsDesc(scco#:scfnd:scEmp#:scDiv:scFYR:scENV);

       //*** begin del ***  06/12/13  *******************************************
       //*****Clear scBillSts;
       //*****xxRRn1 = xxRrn;
       //*****xxBillSts = GetBillsts(xxRRn1);
       //*****scBillSts = xxBillSts;
       //*** end del *****  06/12/13  *******************************************

       //* Active policy link stuff here.
       If scHdgCx <> ' ';
          Clear scHdActive;
          Clear scActive#;
          activeUnd = *Off;
          GetLatXEmp#(scCO#:scFND:scEMP#:scDIV:fCo#:fGroup:
           fEmp#:fDiv:ffYr);
          If femp# <> 0;
             scHdActive = 'Policy Active Under';
             activeUnd = *On;
             policy1# = GetCarrPol(fCO#:fGroup:fEMP#:fDIV:fFYR);
             If  policy1# <> *blanks;
                 SCActive#  = policy1#;
             Else;
                 SCActive# = %editc(scFnd : 'X') + %editc(scEmp# : 'X')
                  + %editc(scFyr : 'X');
             Endif;

             intRRN1=  GetEmpYrRR(0:fEmp#:fDiv:fGroup:fCo#:' ');
             strRRN =  %Editc(intRRN1:'X');
             empName = GetEmpName(fEmp#:fDiv:intRRN1:fGroup:fCo#);
             scEmURL = CreateURL('STDRVPGM':empName:strRRN:'D':'WTPOLSUM');
          Endif;
       Endif;

       //* Cross Reference
       If GetPrvEmp#(scCO#:scFND:scEMP#:scDIV) <> ' ' or
             GetFutEmp#(scCO#:scFND:scEMP#:scDIV) <>  ' ';
          crossRef = *On;
       Else;
          crossRef = *Off;
       Endif;

       //******* beg chg ****** 10/08/13 *****************************
       // intRRN1=  GetEmpYrRR(0:scEmp#:scDiv:scFnd:scCo#:' ');
       intRRN1=  GetEmpYrRR(scFyr:scEmp#:scDiv:scFnd:scCo#:' ');
       //******* end chg ****** 10/08/13 *****************************
       //*** begin add ***  06/12/13  *******************************************
       Clear scBillSts;
       xxRRn1 = intRRN1;
       xxBillSts = GetBillsts(xxRRn1);
       scBillSts = xxBillSts;
       //*** end add *****  06/12/13  *******************************************
       strRRN =  %Editc(intRRN1:'X');
       empName = GetEmpName(scEmp#:scDiv:intRRN1:scFnd:scCo#);
       AgencyUrl = CreateURL('STDRVPGM':empName:strRRN:'D':'WTPOLSUM':
        '00001');

       Clear xxAgt#;
       Clear xxFEIN;
       Clear AgentName;
       Clear ProdName;

       Clear scHdg1;
       Clear scHdg2;
       Clear scHdg3;
       Clear scHdg1Name;
       Clear scHdg2Name;
       Clear scHdg3Name;
       Clear scSubHdg1;
       Clear scSubHdg2;
       Clear scSubHdg3;
       Clear scSubHdg1N;
       Clear scSubHdg2N;
       Clear scSubHdg3N;
       Clear scAgt1Tool;
       Clear scAgt2Tool;
       Clear scAgt3Tool;
       Clear scPrd1Tool;
       Clear scPrd2Tool;
       Clear scPrd3Tool;

       scHdg1 = 'Agency # Name';
       scSubHdg1 = 'Producer # Name';

       //* Commission (or just regular) Agent Info
       sysDt7 = DateTo7(SystemDate);
       sysDt8 = DateTo8(SystemDate);
       xxAgt# = GetEmpAgt(scCO#:scFND:scEMP#:
                 scDIV:elFYR:sysDt7:agentName:AHrrn);
       saveAHrrn = AHrrn;
       If xxAgt# <> 0;
          scHdg1Name = %editc(xxAgt# : 'X') + ' - ' + agentName;
          scAgt1Tool = %editc(xxAgt# : 'X') + ' - ' + agentName;
          Clear ##kc#;
          Clear ##kl#;
           ##call = '1';
           ##MODE1 = '*';
           ##QTE  = scEnv;
           ##RRN  = %editc(saveAHrrn:'X');
           ##view = '1';
           scHdNm1Url=CreateURL('W0040RW':scAgt1Tool:##kc#:##kl#:##Call:##RRN:
            ##view:##QTE:##MODE1);

       //
       // Agent Information
       //
          GetAgtInfo1(AHrrn:xaco#:xafnd:xaagt#:xaagrc:xaprrc:xagprod);

          //* Chain AHrrn PAH0P;
          xxFEIN = GetAgtFein(xaco#:xafnd:xaAgt#:agtMod);
          ProdName = GetProdNm(xxFEIN:agtMod:xagprod:APrrn:xaco#);
          scSubHdg1N   = %editc(xagprod : 'X') + ' - ' + ProdName;
          scPrd1Tool   = %editc(xagprod : 'X') + ' - ' + ProdName;

          agentName = GetAgtName(xxAgt#:AGrrn:AArrn:xafnd:xaco#);
          Clear ##kc#;
          Clear ##kl#;
          ##call = '1';
          ##RRN  = %editc(APrrn:'X');
          ##RRN2 = %editc(AGrrn:'X');
          SubHd1NUrl=CreateURL('W0042R02W':scPrd1Tool:##kc#:##kl#:##Mode:##RRN:
           ##RRN2);

       Endif;
       //***** Beg Add ***** 10/09/15 *************************************
       // Agent Email Address
       GetAgtinfo(ELco#:ELfnd:ELemp#:ELdiv:scAgt1Tool:scAgtEm:scAgtPhn);

       If scAgtEm = *Blanks;
          scAgtEmTT= 'No E-Mail Available';
       Else;
          scAgtEmTT=scAgtEm;
       EndIf;
       // Producer Email Address
       GetProdInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:0:*blanks:0:
                   *blanks:0:0:*blanks:0:0:0:0:scPrdEm);

       If scPrdEm = *Blanks;
          scPrdEmTT = 'No E-Mail Available';
       Else;
          scPrdEmTT = scPrdEm;
       EndIf;
         //* Set Flag ON if no agent is available for the employer
         flgAgtEml = *off;
         If agentName = ' ';
            flgAgtEml = *on;
         Endif;

         //* Set Flag ON if no producer is available for the employer
         flgProEml = *off;
         If ProdName = ' ';
            flgProEml = *on;
         Endif;
       //***** End Add ***** 10/09/15 ********************************************

       Clear xxFEIN;
       Clear AgentName;
       Clear ProdName;

       //* Agent of Record Info
       If xxAgt# <> 0 and xaAgrc <> 0;

          scHdg2   = 'Agency of Record # Name';
          scSubHdg2 = 'Producer # Name';

          agentName = GetAgtName(xaagrc:AGrrn:AArrn:xafnd:xaco#);
          scHdg2Name = %editc(xAagrc : 'X') + ' - ' + agentName;
          scAgt2Tool = %editc(xAagrc : 'X') + ' - ' + agentName;
          Clear ##kc#;
          Clear ##kl#;
           ##call = '1';
           ##MODE1 = '*';
           ##QTE  = scEnv;
           ##view = '1';
           ##RRN  = %editc(saveAHrrn:'X');
           scHdNm2Url=CreateURL('W0040RW':scAgt2Tool:##kc#:##kl#:##Call:##RRN:
            ##view:##QTE:##MODE1);

          xxFEIN = GetAgtFein(xaCO#:xaFND:xaAgrc:agtMod);
          ProdName = GetProdNm(xxFEIN:agtMod:xaprrc:APrrn:xaCO#);
          scSubHdg2N = %editc(xaprrc : 'X') + ' - ' + ProdName;
          scPrd2Tool = %editc(xaprrc : 'X') + ' - ' + ProdName;
          Clear ##kc#;
          Clear ##kl#;
          ##call = '1';
          ##RRN  = %editc(APrrn:'X');
          ##RRN2 = %editc(AGrrn:'X');
          SubHd2NUrl=CreateURL('W0042R02W':scPrd2Tool:##kc#:##kl#:##Mode:##RRN:
           ##RRN2);
       Endif;

       Clear xxAgt#;
       Clear xxFEIN;
       Clear AgentName;
       Clear ProdName;

       //* Servicing Agent Info
       xxAgt# = GetEmpAgt(scco#:scfnd:scemp#:
        scdiv:elfyr:*hival:agentName:AHrrn);
       If xxAgt# <> 0 and AHrrn <> saveAHrrn;

          scHdg1     = 'Commission Agency # Name';
          scSubHdg1  = 'Producer # Name';
          scHdg2     = 'Servicing Agency# Name';
          scSubHdg2  = 'Producer # Name';

          //* Chain AHrrn PAH0P;
          GetAgtInfo1(AHrrn:xaco#:xafnd:xaagt#:x2agrc:x2prrc:xagprod);
          xxFEIN = GetAgtFein(xaCO#:xaFND:xaagt#:agtMod);
          ProdName = GetProdNm(xxFEIN:agtMod:xagprod:APrrn:xaCO#);
          scSubHdg2N = %editc(xagprod : 'X') + ' - ' + ProdName;
          scPrd2Tool = %editc(xagprod : 'X') + ' - ' + ProdName;

          // URL
          agentName = GetAgtName(xxAgt#:AGrrn:AArrn:xafnd:xaco#);
          scHdg2Name = %editc(xxAgt# : 'X') + ' - ' + agentName;
          scAgt2Tool = %editc(xxAgt# : 'X') + ' - ' + agentName;
          Clear ##kc#;
          Clear ##kl#;
          ##call = '1';
          ##RRN  = %editc(APrrn:'X');
          ##RRN2 = %editc(AGrrn:'X');
          SubHd2NUrl=CreateURL('W0042R02W':scPrd2Tool:##kc#:##kl#:##Mode:##RRN:
           ##RRN2);

       //* Agent of Record Info
       If xxAgt# <> 0 and xaAgrc <> 0;

          scHdg3   = 'Agency of Record # Name';
          scSubHdg3 = 'Producer # Name';

          agentName = GetAgtName(xaagrc:AGrrn:AArrn:xafnd:xaco#);
          scHdg3Name = %editc(xAagrc : 'X') + ' - ' + agentName;
          scAgt3Tool = %editc(xAagrc : 'X') + ' - ' + agentName;
          Clear ##kc#;
          Clear ##kl#;
           ##call = '1';
           ##MODE1 = '*';
           ##QTE  = scEnv;
           ##view = '1';
           ##RRN  = %editc(saveAHrrn:'X');
          scHdNm3Url=CreateURL('W0040RW':scAgt3Tool:##kc#:##kl#:##Call:##RRN:
           ##view:##QTE:##MODE1);

          xxFEIN = GetAgtFein(xaCO#:xaFND:xaAgrc:agtMod);
          ProdName = GetProdNm(xxFEIN:agtMod:xaprrc:APrrn:xaCO#);
          scSubHdg3N = %editc(xaprrc : 'X') + ' - ' + ProdName;
          scPrd3Tool = %editc(xaprrc : 'X') + ' - ' + ProdName;
          Clear ##kc#;
          Clear ##kl#;
          ##call = '1';
          ##RRN  = %editc(APrrn:'X');
          ##RRN2 = %editc(AGrrn:'X');
          SubHd3NUrl=CreateURL('W0042R02W':scPrd3Tool:##kc#:##kl#:##Mode:##RRN:
           ##RRN2);
       Endif;
       Endif;

       GetEmIncDt(scCO#:scFND:scEMP#:scDIV:x#Fein);
       scFEIN = x#Fein;

       Clear scGrpName;
       scGrpName = %editc(scFND : 'X') + ' - ' + GetGrpName(scFND:FNrrn:scCO#);

       Clear scHdgStrp;
       Clear scSTRPName;
       Clear scSTRPNam2;
       Clear scSTRPFedH;
       Clear scSTRPFDN;
       Clear scSTRPFDN2;

       stateRptDiv = GetStRptDv(scCO#:scFND:scEMP#:rprrn:strp);
       If strp = 'Y';
          scHdgStrp = 'State Reporting Division';
          STRPName();
       Endif;

       Clear scHdgPEO;
       Clear scPEOName;
       Clear scPEONam2;

       GetGrpName(scFND:gprrn:scCO#:flgPEO);
       If  (flgPEO = 'M' or flgPEO = 'S');
          scHdgPEO = 'PEO Name';
          PEOName();
       Endif;



      /end-free
     P LoadActionBar   E

       //***************************************************************************//
       //===================================================================
       // Load Names for State Reporting Division.
       //===================================================================
     P STRPName        B
     D STRPName        PI
      /free


       stateRptDiv = GetStRptDv(scCO#:scFND:scEMP#:rpRRN:strp);
       If strp = 'Y';
       // DBA Name
          xxNmType= 'DBA';
          xxEmpDba = GetEmpFullName(scEmp#:stateRptDiv:rpRRN:scFnd:scCo#:
           xxNmType);
          scSTRPName =  xxEmpDba;
          scSTRPNam2 =  %subst(xxEmpDba:51);

       // Federal Name
          xxNmType= 'FED';
          xxEmpFed = GetEmpFullName(scEmp#:stateRptDiv:rpRRN:scFnd:scCo#:
           xxNmType);
          scSTRPFdn =  xxEmpFed;
          scSTRPFdn2 = %subst(xxEmpFed:51);

          Clear scSTRPFedH;
             If xxEmpFed <> *blanks;
                scSTRPFedH = 'Federal Name';
             Endif;
       Endif;

      /end-free
     P STRPName        E
       //***************************************************************************//
       //===================================================================
       // Load Names for PEO.
       //===================================================================
     P PEOName         B
     D PEOName         PI
      /free

       SRADD1(scCO#:scFND:scEMP#:scDIV:scFYR:name:fnam:ead1:ead2:
        ecsz:qty:st:zip:fein:eadr);

       scPEOName =  dbanm1 + %subst(dbanm2:1:10);
       scPEONam2 =  %subst(dbanm2:11);

      /end-free
     P PEOName         E

       //***************************************************************************//
       //===================================================================
       // Execute Menu Choice.
       //===================================================================
     P Menu            B
     D Menu            PI
      /free

       Select;
       When scSelOpt = 'PolicySummary';
              //* Policy Summary call
       When scSelOpt = 'LocationMaster';
             //* Location Master call
       When scSelOpt = 'PolicyDetail';
              //* Policy Detail call
       When scSelOpt = 'A/RInquiry';
              //* A/R Inquiry call
       When scSelOpt = 'Contacts';
              //* Contacts call
       When scSelOpt = 'PolicyTracking';
              //* Policy Tracking call
       When scSelOpt = 'CXReinstatements';
              //* CX/Reinstatements call
       When scSelOpt = 'AccountCustom';
              //* Account Custom call
       When scSelOpt = 'ClaimInquiry';
              //* Claim Inquiry call
       EndSl;

      /end-free
     P Menu            E

       //***************************************************************************//
       //===================================================================
       // Process Popup Windows.
       //===================================================================
     P Popup           B
     D Popup           PI
      /free



       If CrossClick = *on;
          PrvEmpXRef(scCO#:scFND:scEMP#:scDIV);
            CrossClick = *off;
       Endif;


       If CmAgtClick = *on;
       //* Commission Agent popup goes here.
          CmAgtClick = *off;
       Endif;

       If CmPrdClick = *on;
       //* Commission Producer popup goes here.
          CmPrdClick = *off;
       Endif;

       If SrAgtClick = *on;
       //* Servicing Agent popup goes here.
          SrAgtClick = *off;
       Endif;

       If SrPrdClick = *on;
       //* Servicing Producer popup goes here.
          SrPrdClick = *off;
       Endif;

      /end-free
     P Popup           E

       //***************************************************************************//
