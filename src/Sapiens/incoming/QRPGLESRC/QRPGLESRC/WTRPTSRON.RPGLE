     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     h*================================================================
     h/title S/R Online Worksheet via Scriptura
     h*================================================================
     H*  Description:                                                 *
     H*    This program will recieve data from CIC and populate       *
     H*    the scriptura file so that the report can be printed       *
     H*    by  scriptura.                                             *
     H*    WXMIP1 - Monthly Interim - Policy                          *
     H*    WXMIP2 - Monthlu Interim - Payrolls/Discounts              *
     H*                                                               *
     H*  Called From: CiCenter                                        *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************2***********************
     H* Create on 06/14/2017 by c:62 for Project CI143G               *
     H*****************************************2***********************
     H* Revisions:                                                    *
     H*                                                               *
     H* 02/04/25 C:300 Project:146WKR, Support:117984 (Prescient)     *
     H*                Recognize collateral from CiCenter to show on  *
     H*                S/R online Worksheet                           *
     H*                                                               *
     H*****************************************************************
     Fwxmip1    if a e             Disk    Rename(pmi01:pmi0p1)
     Fwxmip2    if a e             Disk    Rename(pmi02:pmi0p2)
     Fxmlprel1  uf   e           k Disk
     Fwdf1l     if   e           k Disk    Rename(pf101:pf100)
     Fwdf2l     if   e           k Disk    Rename(pf201:pf200)
     Fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
     Fwmexl     if   e           k Disk    Rename(pex01:pex00)
     Fwmofl     if   e           k Disk    Rename(pof01:pof00)
     Fwmeml     if   e           k Disk    Rename(pem01:pem00)
     Fwdell     if   e           k Disk    Rename(pel01:pel00)
     Fwmaal1    if   e           k Disk
     Fwmagl     if   e           k Disk    Rename(pag01:pag00)
     Fwmrpl     if   e           k Disk    Rename(prp01:prp00)
     Fwdrrl     if   e           k Disk    Rename(prr01:prr00)
     Fwmrsl     if   e           k Disk    Rename(prs01:prs00)
     Fwmpxl1    if   e           k Disk
     Fwmifl     if   e           k Disk    Rename(pif01:pif00)

     d*****************************************************************
     d* Prototype Declaration
     d*****************************************************************
     d* Get Agent Information
     dGetAgtInfo       pr                  ExtPgm('WA002R')
     d company#                            Like(EMco#)  Const
     d group#                              Like(EMfnd)  Const
     d employer#                           Like(EMemp#) Const
     d division                            Like(EMdiv)  Const
     d year                                Like(ELfYr)  Const
     d agent#                         5  0
     d agentName                     60

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     D   outCltID                     3

     d* Get Group#
     d GetGroup#       pr

     d* Fill Payroll Array
     d FillPayArray    pr

     d* Fill Discount Array
     d FillDiscArray   pr

     d* Main Processing
     d Main            pr
     d

     d* Program End
     d ProgramEnd      pr

     d* Program Start
     d ProgramStart    pr

     d* Remove bad characters.
     d RemoveBadChars  pr           120a
     D   stringIn                   120a

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

     d* Data Queue Process
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const

     d* Write to Policy Header - WXMIP1
     d WriteWXMIP1     pr

     d* Write to Payroll/Discount File - WXMIP2
     d WriteWXMIP2     pr
     D   type                         1    const

     d* Entry
     d WTRPTSRON       pr                  Extpgm('WTRPTSRON')
     d                             9999    const
     d                             9999    const
     d                             9999    const
     d                               30
     d                              300
     d WTRPTSRON       pi
     d  inCalcFactors              9999    const
     d  inPayrollInfo              9999    const
     d  inDiscFactors              9999    const
     d  outSuccErrMsg                30
     d  outDocPath                  300

     d*****************************************************************
     d* Work Fields
     d*****************************************************************
     D psds          esds                  ExtName(SZQ1P)

     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)

     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d xmlFlowName     s                   like(M1XML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFileName     s                   like(M1XML)
     d xmlLink         s                   like(XPLink)
     D xxPre           s              2    inz('CP')

     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outFldrPrefix   s              2    inz
     d outPreString    s             10    inz
     d outReturn       s            250    inz
     d outXMLFile      s             61    inz
     d outFlow         s             61    inz
     d outCltID        s              3    inz

     d processType     s            100    inz
     d holdXmlFile     s             60    inz
     d xmlFile         s             60    inz
     d xmlKey          s             60    inz
     d fromDate8       s              8  0
     d toDate8         s              8  0
     d fromDate7       s              7  0
     d toDate7         s              7  0
     d currDate        s              7  0
     d chrCompName     s             40
     d stateKey        s             10
     d massFlag        s              1    inz(' ')
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D flow            s             61    inz(*Blanks)
     d errorsExist     s              1n   inz
     d errorDesc       s            100a   inz
     d fieldDesc       s            200a   inz
     d totalPayroll    s             11s 2 inz
     d company#        s              3  0 inz
     d group#          s              3  0 inz

     D policyHeader    s        9999999
     D payrollArray    s            500    Dim(20000)
     D discountArray   s            200    Dim(50000)

     d/copy sPrLetter
     d/copy sPrModule
     d/copy sPrXML
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrUsrPrms

      /free
       //****************************************************************
       // Main Processing
       //****************************************************************
       Main();

      /End-Free
       //===================================================================
       // GetGroup# - Extract the group #
       //===================================================================
     p GetGroup#       b
     d GetGroup#       pi

     d pos             s              5s 0
     d i               s              5s 0 Inz(1)
     d tempField       s            120a   Inz

      /Free

       // Let's read thru fields
       policyHeader = inCalcFactors;
       pos = %Scan('|':policyHeader);
       If pos <> 0;
          DoU pos = 0;
             // Let's extract the field
             If pos = 1; // Value is null
                tempField = ' ';
             Else;
                tempField = %Subst(policyHeader:1:pos-1);
                tempField = RemoveBadChars(tempField);
             EndIf;

             // Lets Assign to Fields
             If i = 8;  // group#
                group# = %Dec(%Subst(%Trim(tempField):1:3):3:0);
                Leave;
             EndIf;

             // Move to next field
             policyHeader = %Subst(policyHeader:pos+1);

             // Get next field
             pos = %Scan('|':policyHeader);

             // Increment to next field
             i += 1;
          EndDo;
       EndIf;

      /End-Free

     p GetGroup#       e
       //===================================================================
       // FillDiscArray - Populate the Discount Array
       //===================================================================
     p FillDiscArray   b
     d FillDiscArray   pi

     d i               s              5s 0 Inz(1)
     d pos             s              5s 0
     d discountData    s        9999999

      /Free

       discountData = inDiscFactors;

       pos = %Scan('<NEXT>':discountData);
       If pos = 0;
          discountArray(i) = %Subst(discountData:1:200);
       Else;
          DoU pos = 0;
             discountArray(i) = %Subst(discountData:1:pos-2);
             i += 1;
             discountData = %Subst(discountData:pos+7);
             pos = %Scan('<NEXT>':discountData);
             If pos = 0;
                discountArray(i) = %Subst(discountData:1:200);
             EndIf;
          EndDo;
       EndIf;

      /End-Free

     p FillDiscArray   e
       //===================================================================
       // FillPayArray - Populate the Payroll Array
       //===================================================================
     p FillPayArray    b
     d FillPayArray    pi

     d i               s              5s 0 Inz(1)
     d pos             s              5s 0
     d payrollData     s        9999999

      /Free

       payrollData = inPayrollInfo;

       pos = %Scan('<NEXT>':payrollData);
       If pos = 0;
          payrollArray(i) = %Subst(payrollData:1:500);
       Else;
          DoU pos = 0;
             payrollArray(i) = %Subst(payrollData:1:pos-2);
             i += 1;
             payrollData = %Subst(payrollData:pos+7);
             pos = %Scan('<NEXT>':payrollData);
             If pos = 0;
                payrollArray(i) = %Subst(payrollData:1:500);
             EndIf;
          EndDo;
       EndIf;

      /End-Free

     p FillPayArray    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Start Of Program
       ProgramStart();

       Monitor;
          // Populate the Payroll Information
          WriteWXMIP2('P');
       On-Error;
          errorsExist = *On;
          errorDesc   = 'Payroll Information Parameter (Parm 2) has errors.';
       EndMon;

       If errorsExist = *Off;
          Monitor;
             // Populate the Policy Header File
             WriteWXMIP1();
          On-Error;
             errorsExist = *On;
             errorDesc   = 'Calculated Factor Parameter (Parm 1) has errors.';
          EndMon;
       EndIf;

       If errorsExist = *Off;
          Monitor;
             // Populate the Discount Information
             WriteWXMIP2('D');
          On-Error;
             errorsExist = *On;
             errorDesc   = 'Discount Information Parameter (Parm 3)' +
                           ' has errors.';
          EndMon;
       EndIf;

       // End   Of Program
       ProgramEnd();

      /End-Free

     p Main            e
       //===================================================================
       // ProgramEnd - End Program Processing
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi

      /Free

       If xmlFlowName <> ' ' And errorsExist = *Off;
          flow = xmlFlowName + '$';
          S4401C(flow);
       Endif;

       Chain(n) (holdXmlFile) xp100;

       If errorsExist;
          outSuccErrMsg = 'Error';
          outDocPath    = %Trim(errorDesc) + ' ' + fieldDesc;
       Else;
          outSuccErrMsg = 'Success';
          outDocPath    = %Trim(XPdir) + %Trim(XPfile);
       EndIf;

       *inLR = *On;
       Return;

      /End-Free

     p ProgramEnd      e
       //===================================================================
       //
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Get the client id
       GetClientID(outCltId);

       // Get the system date
       currdate = DateTo7(SystemDate());

       // Make sure there are details, if not then end
       If inPayrollInfo = ' ' And inDiscFactors = ' ';
          errorsExist = *On;
          errorDesc   = 'This report cannot print since there isn''t ' +
                        'any detail information.';
          fieldDesc   = ' ';
          ProgramEnd();
       EndIf;

       // Check Scriptura and do setup is so
       ExSr CheckScriptura;

       // Populate Payroll Array
       FillPayArray();
       FillDiscArray();

       //===================================================================
       // CheckScriptura - Check Scriptura
       //===================================================================
       Begsr CheckScriptura;

          processType    = 'Self-Reporting Worksheet';
          outReturn      = processType;
          outFldrPrefix  = *blanks;
          xmlFileName    = *blanks;
          isUseSpoolOut  = UseSpoolOut(processType);
          isUseScriptura = UseScriptura(processType);
          isUse3rdParty  = Use3rdXml(processType);
          isUseXML = isUseScriptura or isUse3rdParty;

          If isUseXML;

             In *Lock *dtaara;

             Clear svLDA;

             Company# = UserCo#(q1User);
             GetGroup#();
             Chain (company#:group#) pex00;
             If %Found and EXmiCd <> ' ';
                xxDart = 'Y';
                xxRmCD = EXmiCd;
             Else;
                xxDart = 'N';
                xxRMCD = ' ';
             Endif;

             Out *dtaara;

             If OutputWindow(processType:' ':'Y':'M') = *Off;

                outPreString = 'Mnthly_Int';

                //* Request record for Self-Reporting Worksheet.
                message = %trim(processType);
                StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':
                xxPre:string:'000':'000':massFlag:xmlLink);

                xmlKey       = xmlFile;
                xmlFileName  = xmlFile;
                xmlFlowName  = xmlFile;

                //* The template XML file name must be pulled from record for work files for a
                //* single request.  Otherwise it has been returned from the XML Request call.
                Chain (xmlFile) xp100;

                XPData = ' ';
                holdXmlfile = xmlFile;
                xmlFile = XPXmlT;

                XPPgm  = Q1pgm;
                XPUser = Q1user;
                XPTime = TimeTo6(SystemTime());
                XPDt   = DateTo7(SystemDate());

                Update xp100;
             EndIf;
          Else;
             errorsExist = *On;
             errorDesc   = 'This report is not set to print in Scriptura.';
             fieldDesc   = ' ';
             ProgramEnd();
          Endif;

       endsr;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // RemoveBadChars - Remove Bad Characters
       //===================================================================
     p RemoveBadChars  b
     d RemoveBadChars  pi           120
     d inData                       120

     d fixedData       s            120
     d i               s              5s 0

      /free

       fixedData = inData;

       For i=1 to 120;
          If %Subst(fixedData:i:1) < x'40';
             If %Subst(fixedData:i:1) <> x'08' And
                   %Subst(fixedData:i:1) <> x'09';
                %Subst(fixedData:i:1) = x'40';
             Endif;
          EndIf;
          If %Subst(fixedData:i:1) > x'F9';
             %Subst(fixedData:i:1) = x'40';
          EndIf;
       EndFor;

       Return fixedData;

      /End-Free

     p RemoveBadchars  e
       //===================================================================
       // WriteWXMIP1 - Write to WXMIP1 - Policy Header
       //===================================================================
     p WriteWXMIP1     b
     d WriteWXMIP1     pi

     d pos             s              5s 0
     d i               s              5s 0 Inz(1)
     d tempField       s            120a   Inz
     d agent#          s              5  0
     d agentName       s             60a

      /Free

       clear pmi0p1;

       // Let's read thru fields
       policyHeader = inCalcFactors;
       pos = %Scan('|':policyHeader);
       If pos <> 0;
          DoU pos = 0;
             // Let's extract the field
             If pos = 1; // Value is null
                tempField = ' ';
             Else;
                tempField = %Subst(policyHeader:1:pos-1);
                tempField = RemoveBadChars(tempField);
             EndIf;

             // Lets Assign to Fields
             Select;
             When i = 1;  // id
                // Not used
             When i = 2;  // submitted by
                // Not used
             When i = 3;  // submit date
                fieldDesc   = 'Submit Date:' + tempField;
                M1PaidDate  = DtCheck(%Dec(%Trim(tempField):8:0));
             When i = 4;  // created by
                // Not used
             When i = 5;  // create date
                // Not used
             When i = 6;  // status
                // Not used
             When i = 7;  // insured
                // Not used
             When i = 8;  // policy #
                fieldDesc   = 'Group:' + tempField;
                M1Fnd     = %Dec(%Subst(tempField:1:3):3:0);
                fieldDesc   = 'Employer#:' + tempField;
                M1Emp#    = %Dec(%Subst(tempField:4:9):9:0);
                fieldDesc   = 'Year:' + tempField;
                M1Fyr     = %Dec(%Subst(tempField:13:3):3:0);
                M1Policy# = %Trim(tempField);
             When i = 9;  // division
                fieldDesc   = 'Division:' + tempField;
                M1Div  = %Dec(%Trim(tempField):5:0);
             When i = 10;  // report from date
                fieldDesc   = 'Report From Date:' + tempField;
                M1RfDate  = DtCheck(%Dec(%Trim(tempField):8:0));
             When i = 11;  // report to date
                fieldDesc   = 'Report To Date:' + tempField;
                M1RtDate  = DtCheck(%Dec(%Trim(tempField):8:0));
             When i = 12;  // report frequency
                // Not used
             When i = 13;  // late flag
                // Not used
             When i = 14;  // increased limit
                fieldDesc   = 'Increased Limit:' + tempField;
                M1IncLmt = %Dec(%Trim(tempField):5:3);
             When i = 15;  // subtotal 1
                fieldDesc   = 'SubTotal 1:' + tempField;
                M1subTot1 = %DecH(%Trim(tempField):9:0);
             When i = 16;  // discount before
                fieldDesc   = 'Discount Before:' + tempField;
                M1BefMod = %Dec(%Trim(tempField):9:6);
             When i = 17;  // subtotal 2
                fieldDesc   = 'SubTotal 2:' + tempField;
                M1subTot2 = %DecH(%Trim(tempField):9:0);
             When i = 18;  // experience mod
                fieldDesc   = 'Experience Mod:' + tempField;
                M1expMod = %Dec(%Trim(tempField):5:4);
             When i = 19;  // subtotal 3
                fieldDesc   = 'SubTotal 3:' + tempField;
                M1subTot3 = %DecH(%Trim(tempField):9:0);
             When i = 20;  // discount after
                fieldDesc   = 'Discount After:' + tempField;
                M1AftMod = %Dec(%Trim(tempField):9:6);
             When i = 21;  // subtotal 4 (Total Premium Due)
                fieldDesc   = 'SubTotal 4:' + tempField;
                M1totPrm = %DecH(%Trim(tempField):9:0);
             When i = 22;  // terrorism amount
                fieldDesc   = 'Terrorism:' + tempField;
                M1terAmt = %Dec(%Trim(tempField):11:2);
             When i = 23;  // waiver
                // Not used
             When i = 24;  // reinsurance
                // Not used
             When i = 25;  // net rate factor
                // Not used
             When i = 26;  // net premium
                // Not used
             When i = 27;  // net waiver
                // Not used
             When i = 28;  // manual premium
                fieldDesc   = 'Manual Premium:' + tempField;
                M1totMan = %DecH(%Trim(tempField):9:0);
             When i = 29;  // previous balance
                fieldDesc   = 'Previous Balance:' + tempField;
                M1PrvBal = %DecH(%Trim(tempField):9:0);
             When i = 30;  // total premium
                // Not used
             When i = 31;  // paid premium
                fieldDesc   = 'Paid Premium:' + tempField;
                M1PremPaid = %DecH(%Trim(tempField):9:0);
             When i = 32;  // premium billed amount
                // Not used
             When i = 33;  // premium tax amount
                fieldDesc   = 'Premium Tax Amount:' + tempField;
                M1taxAmt = %DecH(%Trim(tempField):9:0);
             When i = 34;  // prior premium billed amount
                // Not used
             //***  Begin Add  ***  02/04/25  *****************
             When i = 37;  // Collateral Billed
                fieldDesc   = 'Collateral:' + tempField;
                M1taxAmt = %DecH(%Trim(tempField):9:0);
             //***  End   Add  ***  02/04/25  *****************
             EndSl;

             // Move to next field
             policyHeader = %Subst(policyHeader:pos+1);

             // Get next field
             pos = %Scan('|':policyHeader);

             // Populate the last field
             If pos = 0 and i = 35;  // prior tax amount
                // Not used
             EndIf;

             // Increment to next field
             i += 1;
          EndDo;

          // Populate Other fields
          M1XML     = xmlFile;
          M1Co#     = UserCo#(q1User);
          M1prmWord = 'Premium';
          M1incAmt  = %DecH(M1totMan * M1inclmt/100:11:0);

          Chain (M1co#:M1fnd) pf200;
          M1Logo = F2BLOG;

          Chain (M1co#:M1fnd) pfn00;
          M1FndNam = FNname;

          Chain (M1co#:M1fnd:M1emp#:M1div:M1fYr) pel00;
          M1PfDate  = DtMDYY(ELpfDt);
          M1PtDate  = DtMDYY(ELptDt);
          M1terRte  = ELterr + ELdTrr;

          Chain (M1co#:M1fnd) pex00;
          If EXaHdr <> ' ';
             M1AgtHdg = %trim(EXaHdr) + ':';
          Else;
             M1AgtHdg = 'Agent:';
          Endif;

          GetAgtInfo(M1co#:M1fnd:M1emp#:M1div:M1fYr:agent#:agentName);

          M1Agt# = agent#;
          M1AgtNam = agentName;

          Chain (M1co#:M1fnd:M1agt#) paa01;
          If %found(wmaal1);
             Chain (AACo#:AAFein:AAMod) pag00;
             If %found;
                M1AgtAdr  = AGMad1;
                M1AgtCity = AGMCty;
                M1AgtSt   = AGMSt;
                M1AgtZip  = AGMZip;
                M1AgtArea = AGare1;
                M1AgtPhn  = AGphn1;
             Endif;
          Endif;

          Chain (M1co#:M1fnd:M1emp#:M1div) pem00;
          M1PolFdnm = EMFdnm + EMFdn2;
          M1PolName = EMName + EMFdn2;
          M1PolCont = EMcont;
          M1PolAdr1 = EMmad1;
          M1PolAdr2 = EMmad2;
          M1PolCity = EMmCty;
          M1PolSt   = EMmSt;
          M1PolZip  = EMmZip;

          M1RefHdg  = 'Policy No.:';

          Chain (M1co#:M1fnd:M1emp#:M1div:M1fYr) prp00;
          If RPcap# <> ' ';
             M1CarHdg = 'Carrier Policy#:';
             M1CarPol = RPCap#;
          Endif;

          Chain (EMco#:EMoff#) pof00;
          M1OffNam  = OFCknm;
          M1OffAd1  = OFAad1;
          M1OffAd2  = OFAad2;
          M1OffCity = OFActy;
          M1OffSt   = OFAst;
          M1OffZip  = OFAzip;
          M1DirInq  = OFminm;
          M1OffPhn  = OFAPhn;
          M1OffArea = OFAAra;

          Chain (ELco#:ELrtCd) prs00;
          M1RateSt  = RSjrSt;

          SetGt (ELco#:ELfnd:RSjrSt:ELpfDt) prr00;
          Chain (ELco#:ELfnd:RSjrSt) prr00;
          If RRterH = ' ';
             M1TerHdg  = 'Foreign Terrorism';
          Else;
             M1TerHdg  = RRterH;
          EndIf;

          If RRtxHd = ' ';
             M1TaxHdg  = 'State Tax';
          Else;
             M1TaxHdg  = RRtxHd;
          EndIf;

          M1TaxRte = RRtxPc;

          SetGt (M1co#:M1fnd:M1rateSt:ELpfDt) pif00;
          ReadPE (M1co#:M1fnd:M1rateSt) pif00;
          M1SRFee  = IFisFe;

          Chain (M1co#:M1fnd:M1fYr) pf100;
          If F1netR = 'Y' And ELpfDt >= F1netD;
             M1NetRate = 'Y';
             M1NetVerb = 'Net';
          EndIf;

          If (ELwvPc = 0 And ELeWav <> 0) Or (ELeWav <> 0 And ELwvTp = 'S');
             M1WaivBal = 'Y';
          EndIf;

          M1PrtDate = DtMDYY(DateTo7(SystemDate()));
          M1PayDate = %Int(%Subst(%EditC(DtMDYY(ELpfDt):'X'):1:2) + '01' +
                      %Subst(%EditC(DtMDYY(ELpfDt):'X'):5:4));
          M1ProPerc = 'Y';
          M1payType = 'EFT';

          If ELprom = 'M';
             M1ExpHdg = 'Merit Rating';
             M1Merit = 'Y';
          Else;
             M1ExpHdg = 'Experience Modifier';
          EndIf;

          M1TotDue = M1totPrm + M1terAmt + M1taxAmt + M1prvBal;

          M1pgm  = Q1pgm;
          M1user = Q1user;
          M1time = TimeTo6(SystemTime());
          M1dt   = DateTo7(SystemDate());

          Write pmi0p1;
       EndIf;

      /End-Free

     p WriteWXMIP1     e
       //===================================================================
       // WriteWXMIP2 - Write to WXMIP2 - Payroll/Discounts
       //===================================================================
     p WriteWXMIP2     b
     d WriteWXMIP2     pi
     D   type                         1    Const

     d pos             s              5s 0
     d i               s              5s 0
     d j               s              5s 0
     d tempField       s            120a   Inz

      /Free

       Select;
       When type = 'P';
          ExSr ProcessPayroll;
       When type = 'D';
          ExSr ProcessDiscount;
       EndSl;

       //===================================================================
       // ProcessDiscount - Process Discount Information
       //===================================================================
       Begsr ProcessDiscount;

          // Read discount records
          For j=1 to 50000;
             If discountArray(j) = ' ';
                Leave;
             Else;
                i = 1;
                clear pmi0p2;
                // Let's read thru fields
                pos = %Scan('|':discountArray(j));
                If pos <> 0;
                   DoU pos = 0;
                      // Let's extract the field
                      If pos = 1; // Value is null
                         tempField = ' ';
                      Else;
                         tempField = %Subst(discountArray(j):1:pos-1);
                         tempField = RemoveBadChars(tempField);
                      EndIf;

                      // Let's populate Fields
                      Select;
                      When i = 1;  // id
                         // Not used
                      When i = 2;  // discount name
                         fieldDesc   = 'Discount Name:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2discHdg = %Subst(tempField:1:40);
                      EndSl;

                      // Move to next field
                      discountArray(j) = %Subst(discountArray(j):pos+1);

                      // Get next field
                      pos = %Scan('|':discountArray(j));

                      // Populate the last field
                      If pos = 0 and i = 3; // discount value
                         fieldDesc   = 'Discount Value:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         If %Scan('.':%Trim(tempField)) =  2;
                            M2discFac = %Dec(%Trim(tempField):5:4);
                         Else;
                            M2discPer = %Dec(%Trim(tempField):5:2);
                         EndIf;
                      EndIf;

                      // Increment to next field
                      i += 1;
                   EndDo;

                   // Populate Other fields
                   M2XML     = xmlFile;
                   M2Type    = 'D';
                   M2ProrFac = ELhcn;
                   M2ProrPer = ELddCt;
                   M2Approx  = 'Y';

                   M2pgm  = Q1pgm;
                   M2user = Q1user;
                   M2time = TimeTo6(SystemTime());
                   M2dt   = DateTo7(SystemDate());

                   Write pmi0p2;
                EndIf;
             EndIf;
          EndFor;

       endsr;
       //===================================================================
       // ProcessPayroll - Process Payroll Information
       //===================================================================
       Begsr ProcessPayroll;

          // Read payroll records
          For j=1 to 20000;
             If payrollArray(j) = ' ';
                Leave;
             Else;
                i = 1;
                clear pmi0p2;
                // Let's read thru fields
                pos = %Scan('|':payrollArray(j));
                If pos <> 0;
                   DoU pos = 0;
                      // Let's extract the field
                      If pos = 1; // Value is null
                         tempField = ' ';
                      Else;
                         tempField = %Subst(payrollArray(j):1:pos-1);
                         tempField = RemoveBadChars(tempField);
                      EndIf;

                      // Let's populate Fields
                      Select;
                      When i = 1;  // id
                         // Not used
                      When i = 2;  // class code
                         fieldDesc   = 'Class Code:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2epClas = %Dec(%Trim(tempField):5:0);
                      When i = 3;  // class code description
                         fieldDesc   = 'Class Code Description:' +
                            %Trim(tempField) + ' Line: ' + %EditC(j:'4');
                         M2desc = %Subst(tempField:1:40);
                      When i = 4;  // specialty
                         fieldDesc   = 'Specialty:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2epSpec = %Subst(tempField:1:1);
                      When i = 5;  // extension
                         fieldDesc   = 'Extension:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2epExt = %Subst(tempField:1:2);
                      When i = 6;  // payroll
                         fieldDesc   = 'Payroll:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2payroll = %Dec(%Trim(tempField):11:0);
                         totalPayroll += M2payroll;
                      When i = 7;  // rate
                         fieldDesc   = 'Rate:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2rate = %Dec(%Trim(tempField):7:2);
                      When i = 8;  // premium
                         fieldDesc   = 'Premium:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2premium = %Dec(%Trim(tempField):11:0);
                      When i = 9;  // net rate
                         fieldDesc   = 'Net Rate:' + %Trim(tempField) +
                         ' Line: ' + %EditC(j:'4');
                         M2netRt = %Dec(%Trim(tempField):7:4);
                      When i = 10;  // begin date
                         fieldDesc   = 'Begin Date:' + tempField;
                         M2epEfDt    = DtCheck(%Dec(%Trim(tempField):8:0));
                      EndSl;

                      // Move to next field
                      payrollArray(j) = %Subst(payrollArray(j):pos+1);

                      // Get next field
                      pos = %Scan('|':payrollArray(j));

                      // Populate the last field
                      If pos = 0 and i = 11;  // end date
                         // Not used
                      EndIf;

                      // Increment to next field
                      i += 1;
                   EndDo;

                   // Populate Other fields
                   M2XML = xmlFile;
                   M2Type = 'P';

                   M2TerDiff = 'Territory Differential';  //32

                   M2pgm  = Q1pgm;
                   M2user = Q1user;
                   M2time = TimeTo6(SystemTime());
                   M2dt   = DateTo7(SystemDate());

                   Write pmi0p2;
                EndIf;
             EndIf;
          EndFor;

       endsr;

      /End-Free

     p WriteWXMIP2     e
