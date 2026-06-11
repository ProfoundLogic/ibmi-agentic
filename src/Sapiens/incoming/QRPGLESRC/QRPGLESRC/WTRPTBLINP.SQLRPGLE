     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE WTRPTBLINP - Program to print billing invoices via Scriptura
     H*====================================================================
     H*  Description:
     H*
     H*  This program is called to print Billing Invoices.
     H*  It runs over the Employer Payroll Self-Reporting file WASRP when
     H*  Billing Invoice is requested from the Menu or popup and WXSRP when
     H*  Billing Invoice is generated from Payroll Upload. Builds work file
     H*  data for each Bililng Invoice, and then calls the Scriptura
     H*  server.
     H*
     H*  Billing Invoice submitted from:
     H*  - Menu for a group with Date range
     H*  - Popup for an employer with Date range
     H*  - From Payroll Upload
     H*
     H*  Created:  02/13/18  C:291 Project: 150WBV
     H*==============================================================*
      * Revisions:                                                   *
      *                                                              *
      * 09/25/18 C:291 Support: 73075                                *
      *                Appllied requested changes during SUNZ QA     *
      *                testing.                                      *
      *==============================================================*
     fwxbip     o    e             disk    rename(pbi01:pbi02)
     fwxbip1    o    e             disk    rename(pbd01:pbd02)
     fwxbip2    o    e             disk    rename(pbs01:pbs02) prefix(x)
     fwxbi2l    uf a e           k disk    rename(pbs01:pbs02l)
     fwxbi2l1   uf a e           k disk    rename(pbs01:pbs02l1)
     fwxsrl     if   e           k disk    rename(psr01:psr00)
     fwmrsl     if   e           k disk    rename(prs01:prs00)
     fwdvhl1    if   e           k disk
     fwmvpl     if   e           k disk    rename(pvp01:pvp00)
     fxmlprel1  uf   e           k disk
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmrpl     if   e           k disk    rename(prp01:prp00)
     fwmepl7e   if   e           k disk    rename(pep01:pep07e)
     fwdell     if   e           k disk    infds(dbfds)
     fwmofl     if   e           k disk
     fwtabl     if   e           k disk    include(pbs01)
     d*
     d psds          esds                  extname(szq1p)
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x@RRN           s              9  0 inz(0)
     D sysDt7          s              7  0
     D xxDtFrom        s              7  0
     D xxDtTo          s              7  0
     D xxDtFrom5       s              5  0
     D xxDtTo5         s              5  0
     D xxLargeDed      s              1n
     D PEOFlag         S              1n   inz
     D BillingInvoice  S              1n   inz
     D LargeDeduct     S              1n   inz
     D svPolNbr        s                   like(BSPOLNBR)
     D svco#           s                   like(emco#)
     D svfnd           s                   like(emfnd)
     D svdiv           s                   like(emdiv)
     D svemp#          s                   like(ememp#)
     D svfyr           s                   like(elfyr)
     D x1fnd           s                   like(emfnd)
     D x1emp#          s                   like(ememp#)
     D s1Emp#          s                   like(ememp#)
     D s1Fnd           s                   like(emfnd)
     d xmlFile         s                   like(XPXML)
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     D isUseXML        S              1n   inz
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('PN')
     d processType     c                   const('Billing Invoice')
     d xxProcessType   s            100
     D xxPolicy#       s                   like(RPCAP#)
     D xxPolAdr1       s                   like(EMMAD1)
     D xxPolAdr2       s                   like(EMMAD2)
     D xxPolCity       s                   like(EMMCTY)
     D xxPolSt         s                   like(EMMST)
     D xxPolZip        s                   like(EMMZIP)
     D moreRows        s               n   Inz(*on)
     D WriteHeader     s               n   Inz(*on)
     D TotPrmDue       s                   like(BSPRMDUE)
     D RptTotal        s                   like(BSGRDTOT)
     D strPolicy#      s                   like(RPCAP#)
     D intTManPrmR     s                   like(BSMANPREM)
     D intTWagesR      s                   like(BSWAGEAMT)
     D intTManPrmP     s                   like(BSMANPREM)
     D intTWagesP      s                   like(BSWAGEAMT)
     D intRptTotWage   s                   like(BSWAGEAMT)
     D intRptTotMPrm   s                   like(BSMANPREM)
     D svPolNbrI       s                   like(BSPOLNBR)
     D svRptPrd        s                   like(BSRptPrd)

      * Receive wasrp record from SQL SELECT.
     D wasrp_ds      e ds                  extname(wasrp)  INZ

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     d
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const
     d
     DObjExists        pr             1n
     D object                        10    const
     D objType                        7    const
     d
     d GenBillInv      pr              n
     d
     d LargeDed        pr              n
     d
     d RptPrdDt        pr             7  0
     d  rptPrdDtM                     5  0 const
     D
     D/copy sPrModule
     D/copy sPrXML
     D/copy sprDate
     C*
     C* Entry list to program.
     C*
     C     *entry        Plist
     C                   Parm                    QQCo#             3
     C                   Parm                    QQFnd             3
     C                   Parm                    QQEmp#            9
     C                   Parm                    QQDiv             5
     C                   Parm                    QQFyr             3
     C                   Parm                    QQFmDt            8
     C                   Parm                    QQToDt            8
     C                   Parm                    QQCall            1
     C*
     C                   Move      QQCo#         xxCo#             3 0
     C                   Move      QQFnd         xxFnd             3 0
     C                   Move      QQEmp#        xxEmp#            9 0
     C                   Move      QQDiv         xxDiv             5 0
     C                   Move      QQFyr         xxFyr             3 0
     C                   Move      QQFmDt        xxFmDt            8 0
     C                   Move      QQToDt        xxToDt            8 0
      /free

       //Convert to Date 7,0
       xxDtFrom = DtCYMD(xxFmDt);
       xxDtTo   = DtCYMD(xxToDt);

       //Convert to Date 5,0
       xxDtFrom5 = %Int(%Subst(%Editc(xxDtFrom:'X'):1:5));
       xxDtTo5   = %Int(%Subst(%Editc(xxDtTo:'X'):1:5));

       Exsr ClearSvFlds;

       xxProcessType = processType;

       isUseXML = UseScriptura(xxProcessType)
        or Use3rdXml(xxProcessType);

       //* Exit program if Scriptura or XML is not enabled.
       If not isUseXML;
          *inlr = *on;
          Return;
       Endif;

       sysDt7 = Dateto7(SystemDate());
       Clear xmlLink;
       Clear xmlFlowName;

       //* xmlFile - contains the name of the template XML file, used for work file linking.
       //* xmlFlowName - contains the name of the flow XML file, used for S4401C call.
       //* xmlLink - used to link individual records together in a mass run - XML flow file name.
       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';


       //Called from toolbar OR from the menu OR from payroll Import
       Select;
          When QQCall = 'E';          //Called from the Toolbar with Date Range
             Exsr ProcToolbarOpt;
          When QQCall = 'G';          //Called from the menu with Date Range
             Exsr ProcMenuOpt;
          When QQCall = 'U';          //Called from the payroll Upload
             Exsr ProcPayUpload;
       Endsl;

       //Policy level Total
       Exsr PolicyTotal;

       //* Submit request to Scriptura
       Select;
          When xmlFlowName = *Blanks;
          When objExists('PDSDEV':'*LIB');
             Monitor;
             S4401C(xmlFlowName + '$');
             On-error;
             Endmon;
        Other;
            S4401C(xmlFlowName + '$');
       Endsl;

       *inlr = *on;
       return;

       // *=========================================================================
       // *  WriteWXBIP - Write the WXBIP Header Record for each Invoice
       // *=========================================================================
       Begsr WriteWXBIP;

          Exsr GetWMEMP;

          Clear xxPolAdr1;
          Clear xxPolAdr2;
          Clear xxPolCity;
          Clear xxPolSt;
          Clear xxPolZip;
          If EMMAD1 <> '  ';
             xxPolAdr1 = EMMAD1;
             xxPolAdr2 = EMMAD2;
             xxPolCity = EMMCTY;
             xxPolSt   = EMMST;
             xxPolZip  = EMMZIP;
          Else;
             xxPolAdr1 = EMPAD1;
             xxPolAdr2 = EMPAD2;
             xxPolCity = EMPCTY;
             xxPolSt   = EMPST;
             xxPolZip  = EMPZIP;
          Endif;

          //Get Carrier Policy No. - Main Policy No.
          Chain (EMCo#:EMFnd:EMEmp#:EMDiv:ELFyr) prp00;
          If %found;
             xxPolicy# = RPCAP#;
          Else;
             xxPolicy# = %editc(EMFnd:'X') + %editc(EMEmp#:'X') +
              %editc(ELFyr:'X');
          Endif;

          //Write Work file record for Billing Invoice header (Page 1 Info)
          BIXML  = xmlFile;
          BICO#  = EMCO#;
          BIFND  = EMFND;
          BIEMP# = EMEMP#;
          BIDIV  = EMDIV;
          BIINVDAT  = DateTo8(SystemDate());
          BIINSNAME = EMNAME;
          //*** Beg Add ****** 09/25/18 **********************************
          BIFEDNAME = EMFDNM;
          //*** Beg Add ****** 09/25/18 **********************************
          BIINSADR1 = xxPolAdr1;
          BIINSADD2 = xxPolAdr2;
          BIINSCITY = xxPolCity;
          BIINSST   = xxPolst;
          BIINSZIP  = xxPolZip;
          BIDUEDAT = 0;
          //Retrieve Office Master file
          Chain (EMCO#:EMOFF#) POF01;
          If %FOUND(WMOFL);
             BIPOLICY# = xxPolicy#;
             BIABANUM  = OFAABA;
             BIACTNUM  = OFAACC;
             BIABAWIR  = OFWABA;
             BIWIRBNK  = OFBNAM;
             BIWIRADR1 = OFBAD1;
             BIWIRADR2 = OFBAD2;
             BIWIRCITY = OFBCTY;
             BIWIRST   = OFBST;
             BIWIRZIP  = OFBZP;
             BIACCNAM = OFBANM;
             BIACCNBR = OFBANB;
             BIATTNNM = OFBATT;
             BIMAILNAM = OFBNAM;
             BIMAILAD1 = OFBAD1;
             BIMAILDD2 = OFBAD2;
             BIMAILCTY = OFPCTY;
             BIMAILST  = OFPST;
             BIMAILZIP = OFPZP;
             BIRMTTXT = OFREMV;
          Endif;
          BICONAME = EMNAME;
          //*** Beg Chg ****** 09/25/18 **********************************
          //BICARR   = 'SUNZ Insurance';
          BICARR = FNNAME;
          //*** End Chg ****** 09/25/18 **********************************
          BIFROMDT = xxFmDt;
          BITODT   = xxToDt;
          BIPRTDT  = DateTo8(SystemDate());
          Write PBI02;
          FEOD wxbip;

       Endsr;

       // *=========================================================================
       // *  WriteWXBIP1 - Write the WXBIP1 Detail Record
       // *=========================================================================
       Begsr WriteWXBIP1;

          //Get Files
          If srCo# <> svCo# or srFnd <> svFnd or
              srEmp# <> svEmp# or srDiv <> svDiv or srFyr <> svFyr;
             Setll (srCo#:srFnd:srEmp#:srDiv) pem00;
             Reade (srCo#:srFnd:srEmp#:srDiv) pem00;
             Chain (srCo#:srFnd:srEmp#:srDiv:srFyr) pel01;
             Chain (ELCo#:ELRtcd) prs00;
             Chain (srCo#:srFnd:srEmp#:srDiv:srFyr) prp00;
             If %found;
                strPolicy# = RPCAP#;
             Else;
                strPolicy# = %editc(srFnd:'X') + %editc(srEmp#:'X') +
                 %editc(srFyr:'X');
             Endif;
             BillingInvoice = GenBillInv();    //If Generate Bill Inv flag is 'Y' in Vendor table
             LargeDeduct = LargeDed();         //If policy is large deductible
             svCo# = srCo#;
             svFnd = srFnd;
             svEmp# = srEmp#;
             svDiv = srDiv;
             svFyr = srFyr;
          Endif;

          If BillingInvoice = *on and LargeDeduct = *on;

             If WriteHeader = *on;
                x1Fnd  = srFnd;
                x1Emp# = sremp#;
                Exsr CreateXMLrcd;
                Exsr WriteWXBIP;                    //Write Header record for each employer
                WriteHeader = *off;
             Endif;

             //Write WXBIP1 Detail record
             Clear pbd02;
             BDXml  = xmlFile;
             BDco#  = ELco#;
             BDfnd  = ELfnd;
             BDemp# = ELemp#;
             BDdiv  = ELdiv;
             BDfyr  = ELfyr;
             If SRTODT <> 0;
                BDrprdwk = SRTODT;                  //Weekly date 7,0
                BDRPTPRD = SRTODT;                  //Weekly date 7,0
             Else;
                BDrprdmth = SRRPMY;                 //Monthly date 5,0
                BDRPTPRD = RptPrdDt(SRRPMY);        //Converts 5,0 date 7,0
             Endif;
             BDpolnbr = xxPolicy#;                  //Main Policy Number
             BDpolnbrI= strPolicy#;                 //Individual Policy Number
             BDemplnk = EMLINK;
             BDcltname = EMNAME;
             BDstate = RSJRST;
             BDwccode  = SRCLAS;
             BDwageamt = SRAMTP;
             BDmanprem = SRMANPRM;
             //Get WMEPP file for wages
             Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
                    srClas:srSpec:srExt:xxDtTo) pep07e;
             Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
                    srClas:srSpec:srExt) pep07e;
             BDwcrate  = EPARAT;
             Write PBD02;
             FEOD wxbip1;
             Exsr WriteWXBIP2;
          Endif;

       Endsr;

       // *=========================================================================
       // *  WriteWXBIP2 - Write the WXBIP2 Summary Record
       // *=========================================================================
       Begsr WriteWXBIP2;

          //Write or Update Summary by Wages total for Individual policy number and state
          Chain (bdxml:'SW':bdpolnbrI:bdRptPrd:bdstate) WXBI2L1;
          If %Found;
             BSmanprem = BSmanprem + BDmanprem;
             BSwageamt = BSwageamt + BDwageamt;
             Update pbs02l1;
          Else;
             Clear pbs02l1;
             BSxml = xmlfile;
             BStype = 'SW';
             BSco# = BDco#;
             BSfnd = BDfnd;
             BSemp#= BDemp#;
             BSdiv = BDdiv;
             BSfyr = BDfyr;
             BSpolnbr = BDpolnbr;                //Main policy number
             BSpolnbrI = BDpolnbrI;              //Individual policy number
             BSrptprd = BDRptPrd;     // 7,0 date
             BSrptprdm = BDrprdmth;   //This field is to store Monthly date 5,0
             BSstate   = BDstate;
             BSmanprem = BDmanprem;
             BSwageamt = BDwageAmt;
             BSemplnk = BDemplnk;
             Write pbs02l1;
             FEOD wxbi2l1;
          Endif;

          //Write or Update Summary by reporting Period for MAIN policy
          Chain (bdxml:'SR':bdpolnbr:BDRptPrd) WXBI2L;
          If %Found;
             BSwageamt = BSwageamt + BDwageamt;
             BSmanprem = BSmanprem + BDmanprem;
             eval(h) BSamtdue = (BSmanprem * ELmanp)/100;
             Update pbs02l;
          Else;
             Clear pbs02l;
             BSxml = xmlfile;
             BStype = 'SR';
             BSco# = BDco#;
             BSfnd = BDfnd;
             BSemp#= BDemp#;
             BSdiv = BDdiv;
             BSfyr = BDfyr;
             BSpolnbr = BDpolnbr;
             BSrptprd = BDRptPrd;
             BSrptprdm = BDrprdmth;  //This field is to store Monthly date 5,0
             BSmanprem = BDmanprem;
             BSmanppct = ELmanp;
             eval(h) BSamtdue = (BSmanprem * ELmanp) / 100;
             BSwageamt = BDwageamt;
             Write pbs02l;
             FEOD wxbi2l;
          Endif;

       Endsr;

       // *=========================================================================
       // *  PolicyTotal - Calculate Premium Due, Collateral Due and Report Total
       // *=========================================================================
       Begsr PolicyTotal;

          Select;
            When QQCall = 'E';          // Toolbar option
              Chain(n) (xmlFlowName) xp100;
              If %Found(xmlprel1);
                 Exsr WriteWXBIP2T;
              Endif;
            Other;
              Setll (xmlFileBlank:xmlFlowName) xp100;
              Reade(n) (xmlFileBlank:xmlFlowName) xp100;
              Dow Not %Eof(xmlprel1);
                  Exsr WriteWXBIP2T;                //Write totals for each report
              Reade(n) (xmlFileBlank:xmlFlowName) xp100;
              Enddo;
          Endsl;

       Endsr;

       // *=========================================================================
       // *  WriteWXBIP2T - Write WXBIP2 Totals
       // *=========================================================================
       Begsr WriteWXBIP2T;
          Clear TotPrmDue;
          Clear RptTotal;
          //Spin through all the SR records for this policy, calculate Total prm & Rpt Total
          Setll (xpxmlt:'SR') pbs02l;
          Reade(n) (xpxmlt:'SR') pbs02l;
          Dow Not %Eof(WXBI2L);
             TotPrmDue = TotPrmDue + BSAMTDUE;
             RptTotal = RptTotal + BSAMTDUE;
             Reade(n) (xpxmlt:'SR') pbs02l;
          Enddo;
          svPolNbr = BSPOLNBR;
          svco# = BSCO#;
          svfnd = BSFND;
          svemp# = BSEMP#;
          //Write TR record in WXBIP2 file
          If TotPrmDue > 0;
             Clear pbs02l;
             BSxml = XpxmlT;
             BStype = 'TR';
             BSco# = svco#;
             BSfnd = svfnd;
             BSemp#= svemp#;
             BSpolnbr = svPolNbr;
             BSPRMDUE = TotPrmDue;
             BSCOLDUE = 0;
             BSGRDTOT = RptTotal;
             Write pbs02l;
             FEOD wxbi2l;
          Endif;

          //Spin through all the SW records for this policy, calculate Total
          //by Reporting period and Total by Policy
          Clear intTManPrmR;
          Clear intTManPrmP;
          Clear intRptTotMPrm;
          Clear intTWagesR;
          Clear intTWagesP;
          Clear intRptTotWage;
          Setll (xpxmlt:'SW') pbs02l1;
          Reade(n) (xpxmlt:'SW') pbs02l1;
          svco# = BSCO#;
          svfnd = BSFND;
          svemp# = BSEMP#;
          svRptPrd = BSRptPrd;
          svPolNbr = BSPolNbr;
          svPolNbrI = BSPolNbrI;
          Dow Not %Eof(WXBI2L1);
             //Reporting period break
             If BSPolNbrI <> svPolNbrI or BSrptprd <> svRptPRd;
                Exsr WrtSWRPrd;
             Endif;
             //Policy break
             If BSPolNbrI <> svPolNbrI;
                Exsr WrtSWPol;
             Endif;

             intTManPrmR   = intTManPrmR + BSMANPREM;           //Manual Prm Tot by Rpt Prd
             intTWagesR    = intTWagesR  + BSWAGEAMT;           //Wages Total by Rpt Prd
             intTManPrmP   = intTManPrmP + BSMANPREM;           //Manual Prm Tot by Policy
             intTWagesP    = intTWagesP  + BSWAGEAMT;           //Wages Total by Policy
             intRptTotWage = intRptTotWage + BSWAGEAMT;         //Wages SW Report total
             intRptTotMPrm = intRptTotMPrm + BSMANPREM;         //Manual Prm SW Report Total
          Reade(n) (xpxmlt:'SW') pbs02l1;
          Enddo;

          //Write Last Record
          Exsr WrtSWRPrd;
          Exsr WrtSWPol;

          //Write Report Total
          Exsr WrtSWGTot;

       Endsr;

       // *=========================================================================
       // *  WrtSWRPrd - Write Summary of wages total by Reporting period
       // *=========================================================================
       Begsr WrtSWRPrd;
          //Write WR record in WXBIP2 file
          Clear pbs02;
          xBSxml = XpxmlT;
          xBStype = 'WR';
          xBSco# = svco#;
          xBSfnd = svfnd;
          xBSemp#= svemp#;
          xBSpolnbr = svPolNbr;
          xBSpolnbrI= svPolNbrI;
          xBSRptPrd = svRptPrd;
          xBSManPrem = intTManPrmR;
          xBSWageAmt = intTWagesR;
          Write pbs02;
          FEOD wxbip2;
          Clear intTManPrmR;
          Clear intTWagesR;
          svRptPrd = BSRptPrd;
       Endsr;

       // *=========================================================================
       // *  WrtSWPol - Write Summary of wages (SW) total by Policy
       // *=========================================================================
       Begsr WrtSWPol;
          //Write WP record in WXBIP2 file
          Clear pbs02;
          xBSxml = XpxmlT;
          xBStype = 'WP';
          xBSco# = svco#;
          xBSfnd = svfnd;
          xBSemp#= svemp#;
          xBSpolnbr = svPolNbr;
          xBSpolnbrI= svPolNbrI;
          xBSManPrem = intTManPrmP;
          xBSWageAmt = intTWagesP;
          Write pbs02;
          FEOD wxbip2;
          Clear intTManPrmP;
          Clear intTWagesP;
          svco# = BSCO#;
          svfnd = BSFND;
          svemp# = BSEMP#;
          svPolNbr = BSPolNbr;
          svPolNbrI = BSPolNbrI;
       Endsr;

       // *=========================================================================
       // *  WrtSWGTot - Write Summary of Wages Grand Total
       // *=========================================================================
       Begsr WrtSWGTot;
          //Write WT record in WXBIP2 file. Write WXBIP2 record by main policy number
          Clear pbs02;
          xBSxml = XpxmlT;
          xBStype = 'WT';
          xBSco# = svco#;
          xBSfnd = svfnd;
          xBSpolnbr= svPolNbr;                       //Main Policy number
          xBSWAGEAMT = intRptTotWage;
          xBSMANPREM = intRptTotMPrm;
          Write pbs02;
          FEOD wxbip2;
          Clear intRptTotWage;
          Clear intRptTotMPrm;
       Endsr;

       // *=========================================================================
       // *  ProcToolbarOpt - Billing Invoice requested from policy Toolbar
       // *=========================================================================
       Begsr ProcToolbarOpt;

          isMassReq = *off;
          massFlag = 'N';

          Exec SQL
          declare wasrp_c cursor for select * from wasrp
          where srco# = :xxco# and srfnd = :xxfnd and sremp# = :xxemp#
                and ( (srtodt >= :xxDtFrom and srtodt <= :xxDtTo)  or
                     (srrpmy >= :xxDtFrom5 and srrpmy <= :xxDtTo5) )
          order by srco#, srfnd, sremp#, srdiv, srfyr, srtodt, srrpmy
          for Read only;

          Exec SQL open wasrp_c;

          Exec SQL Fetch next from wasrp_c into :wasrp_ds;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             moreRows = *off;
             Exec SQL close wasrp_c;
          EndIf;

          If moreRows = *On;
              WriteHeader = *on;
          EndIf;

          // If row was fetched then process
          DoW moreRows = *On;
             Exsr WriteWXBIP1;

             // Fetch next Record
             Exec SQL Fetch next from wasrp_c into :wasrp_ds;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                moreRows = *Off;
                Exec SQL close wasrp_c;
             EndIf;
          EndDo;

       Endsr;

       // *===============================================================================
       // *  ProcMenuOpt - Billing Invoice requested from the Menu by Grp and Date range
       // *===============================================================================
       Begsr ProcMenuOpt;

          Chain (xxCo#:xxFnd) pfn00;
          isMassReq = *on;
          massFlag = 'Y';
          //* Begin reading the WASRP file.
          Exec SQL
          declare wasrp_c1 cursor for select * from wasrp
          where srco# = :xxco# and srfnd = :xxfnd
                and ( (srtodt >= :xxDtFrom and srtodt <= :xxDtTo)  or
                      (srrpmy >= :xxDtFrom5 and srrpmy <= :xxDtTo5) )
          order by srco#, srfnd, sremp#, srdiv, srfyr, srtodt, srrpmy
          for Read only;

          Exec SQL open wasrp_c1;

          Exec SQL Fetch next from wasrp_c1 into :wasrp_ds;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             moreRows = *off;
             Exec SQL close wasrp_c1;
          EndIf;

          If moreRows = *On;
             WriteHeader = *on;
             s1Emp# = sremp#;
          EndIf;

          // If row was fetched then process
          DoW moreRows = *On;

             //Create Billing Invoice for each employer for Non PEO group
             //For PEO, one Billing Invoice will be created for Employer 1, Div 0
             If sremp# <> s1Emp# and FNpeof = ' ';
                WriteHeader = *on;
                s1Emp# = sremp#;
             Endif;

             Exsr WriteWXBIP1;

             // Fetch next Record
             Exec SQL Fetch next from wasrp_c1 into :wasrp_ds;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                moreRows = *Off;
                Exec SQL close wasrp_c1;
             EndIf;
          EndDo;

       Endsr;

       // *===================================================================
       // *  ProcPayUpload - Billing Invoice requested from Payroll Upload
       // *===================================================================
       Begsr ProcPayUpload;

          isMassReq = *on;
          massFlag = 'Y';

          //* Begin reading the WXSRP file.
          Setll *Loval psr00;
          Read psr00;

          If Not %Eof(wxsrl);
             WriteHeader = *on;
             s1Emp# = sremp#;
             s1Fnd  = srFnd;
          EndIf;

          // If row was fetched then process
          Dow Not %Eof(wxsrl);

             //Create Billing Invoice for each employer for Non PEO group
             //For PEO, one Billing Invoice will be created for Employer 1, Div 0
             If srFnd <> s1fnd OR                      //Create XML rcd if Fnd changes
                FNpeof = ' ' and sremp# <> s1Emp#;    //Create XML rcd if non-PEO and Emp# chgs
                If srFnd <> s1fnd;
                   Chain (xxCo#:SRfnd) pfn00;
                Endif;
                WriteHeader = *on;
                s1Emp# = sremp#;
                s1Fnd  = srFnd;
             Endif;

             If srRepFlg = *Blank;
                Exsr WriteWXBIP1;                   //write detail and summary record
             Endif;

             // Read next Record
             Read psr00;

          EndDo;

       Endsr;

       // *=========================================================================
       // *  GetWMEMP - Get Employer master record
       // *=========================================================================
       Begsr GetWMEMP;

          Chain (srCo#:srFnd:srEmp#:srDiv:srFyr) pel01;
          Chain(n) (xxCo#:x1Fnd) pfn00;
          If FNPEOF <> ' ';
             PEOFlag = *on;
             Setll (xxCo#:x1Fnd:1:0) pem00;        //Get WMEMP for Employer 1, Div 0 for PEO
             Reade (xxCo#:x1Fnd:1:0) pem00;
          Else;
             PEOFlag = *off;
          Endif;

          // Find state reporting division for non PEO group
          If PEOFlag = *off;
             //* Get policy mailing address.
             Setll (xxCo#:x1Fnd:x1Emp#) pem00;
             Reade (xxCo#:x1Fnd:x1Emp#) pem00;
             Dow not %eof;
                //* check state reporting flag
                If EMSTRP = 'Y';
                   Leave;
                Endif;
                Reade (xxCo#:x1Fnd:x1Emp#) pem00;
             Enddo;
          Endif;

       Endsr;

       // *=========================================================================
       // *  CreateXMLrcd - Create the XMLPREP record for the notice
       // *=========================================================================
       Begsr CreateXMLrcd;

          //For PEO it will attach output to Employer 1 Div 0.
          //For non-PEO it will attach to State Reporting division.
          Exsr GetWMEMP;
          //Get dbrrn of state Reporting division or Employer 1, Div 0 for PEO
          Chain (emCo#:emFnd:emEmp#:emDiv:srFyr) pel01;

          //* Request Master record for mass run.
          If isMassReq;
             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'Bill_Inv':'000':'000':massFlag:xmlLink);

             isMassReq = *off;
             //* Flow XML file name is used for the S4401C call.
             xmlFlowName = xmlFile;
             xmlLink = xmlFile;

             Chain (xmlFile) xp100;
             XPCat = 'PS';
             If QQCall = 'U';
                XPDart = 'Y';
                XPrmcd = 'PSPN';
             Endif;
             Update xp100;
          Endif;

          x@RRN = dbRRN;
          //* Request record for individual Billing Invoice
          StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x@RRN:'X'):
           xxPre:string:%editc(srCo#:'X'):%editc(srFnd:'X'):massFlag:xmlLink);

          //* Flow XML file name is used for the S4401C call.
          If xmlFlowName = ' ';
             xmlFlowName = xmlFile;
          Endif;

          //* The template XML file name must be pulled from record for work files for a
          //* single request.  Otherwise it has been returned from the XML Request call.
          If xmlLink = ' ';
             Chain (xmlFile) xp100;
             xmlFile = XPXmlT;
          Else;
             Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          Endif;

          If QQCall = 'U';
             XPDart = 'Y';
             XPrmcd = 'PSPN';
          Endif;

          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPTime = Timeto6(SystemTime());
          XPDt   = sysDt7;

          Update xp100;

       Endsr;

       // *===========================================================
       // *  ClearSvFlds - Clear Save fields
       // *===========================================================
       Begsr ClearSvFlds;
          Clear svCo#;
          Clear svFnd;
          Clear svEmp#;
          Clear svDiv;
          Clear svFyr;
          Clear s1Emp#;
          Clear s1Fnd;
       Endsr;

      /end-free
      //----------------------------------------------------------------
      // GenBillInv - Return *On if Billing Invoice should be generated
      //----------------------------------------------------------------
     p GenBillInv      b
     d GenBillInv      pi              n
     d xxBillInv       s               n

      /Free
         xxBillInv = *off;
         //Check if vendor is setup to generate Billing Invoice
         Setgt (srco#:srfnd:sremp#:srdiv:srfyr:sysDt7) pvh01;
         Readpe (srco#:srfnd:sremp#:srdiv:srfyr) pvh01;
         If Not %Eof(wdvhl1) and VHVDR# <> 0;
            Chain (srCo#:VHVDR#) pvp00;
            If %Found and VPGENPRM = 'Y';
               xxBillInv = *on;
             Endif;
         Endif;
         Return xxBillInv;

      /End-Free

     p GenBillInv      e

      //----------------------------------------------------------------
      // LargeDed - Return *On if Policy is Large Deductible
      //----------------------------------------------------------------
     p LargeDed        b
     d LargeDed        pi              n
     d xxLargeDeduct   s               n

      /Free
         //Check if policy is Large Deductible
         Chain (srCo#:srFnd:srEmp#:srDiv:srFyr) pel01;
         Chain (ELco#:%Editc(ELbscd:'X')) pbs01;
         If %Found and BSLPGM = 'L';
            xxLargeDeduct = *on;
         Else;
            xxLargeDeduct = *off;
         Endif;

         Return xxLargeDeduct;

      /End-Free

     p LargeDed        e

      //----------------------------------------------------------------
      // LargeDed - Return *On if Policy is Large Deductible
      //----------------------------------------------------------------
     p RptPrdDt        b
     d RptPrdDt        pi             7  0
     D rptPrdDtM                      5  0 Const

     d rptPrdDtA       s              7a
     d rptPrdDtN       s              7  0

      /Free
         rptPrdDtA = %editc(rptPrdDtM:'X');
         rptPrdDtA = %replace('01':rptPrdDtA:6);
         rptPrdDtN = %int(rptPrdDtA);
         Return rptPrdDtN;

      /End-Free

     p RptPrdDt        e
     P*====================================================================
     P* Determine if the object exists
     P*====================================================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e

