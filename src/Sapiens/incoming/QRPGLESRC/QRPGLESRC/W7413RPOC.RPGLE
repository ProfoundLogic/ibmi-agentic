     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE W7413RPOC - Program to print POC notices via Scriptura
     H*====================================================================
     H*  Description:
     H*
     H*  This program is called to print POC (Proof of Coverage) notices.
     H*  It runs over the POC request file written by the policy page,
     H*  builds work file data for each notice, and then calls the Scriptura
     H*  server.  The process makes one call per state in order to accumulate
     H*  all of the notices for a state into one output file.
     H*
     H*  Created:  07/19/17  C:65  Project: WC2492/148WAE/SY371
     H*====================================================================
     H*  Revisions:
     H*
     H*  09/25/20 C:65  Project: Phw#91613 (Amwins)
     H*                 Use proper key fields for chains/reads.
     H*
     H*  04/22/20 C:65  Project: Phw#88742 (Amwins)
     H*                 Wrap the loop for additional locations for a div
     H*                 inside the presence of an emplolyer detail record.
     H*
     H*====================================================================
     FXMLPREL1  UF   E           K DISK
     FWMPOL1    UF   E           K DISK    rename(ppo01:ppo00)
     FWXPO1L    UF A E           K DISK    usropn
     FWXPO2L    UF A E           K DISK    usropn
     FWMFNL     IF   E           K DISK    rename(pfn01:pfn00)
     FWDF2L     IF   E           K DISK    rename(pf201:pf200)
     FWMEML     IF   E           K DISK    rename(pem01:pem00)
     FWMALL     IF   E           K DISK    rename(pal01:pal00)
     FWMRPL     IF   E           K DISK    rename(prp01:prp00)
     FWMEPL     IF   E           K DISK    rename(pep01:pep00)
     FWDELL1    IF   E           K DISK
     F                                     infds(dbfds)
     FWDELL     IF   E           K DISK    rename(pel01:pel00)
     F                                     prefix(e2:2)
     d*
     d psds          esds                  extname(szq1p)
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x@RRN           s              9  0 inz(0)
     D sysDt7          s              7  0
     D xxSvSt          s                   like(EMMST)
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
     D xxPre           s              2    inz('WC')
     d processType     c                   const('Policy Page')
     d pocType         c                   const('Proof of Coverage')
     d xxProcessType   s            100
     d i               s              5  0
     d index           s              5  0
     d count           s              5  0
     D arrFein         s              9    dim(9999)
     D arrFile         s             60    dim(9999)
     D arrCount        s              5  0 dim(9999)
     D arrName         s            120    dim(9999)
     D xxFein          s              9
     D parentFein      s              9
     D xxPolicy#       s                   like(RPCAP#)
     D xxPolAdr1       s                   like(EMMAD1)
     D xxPolAdr2       s                   like(EMMAD2)
     D xxPolCity       s                   like(EMMCTY)
     D xxPolSt         s                   like(EMMST)
     D xxPolZip        s                   like(EMMZIP)
     D xxAdr1          s                   like(EMMAD1)
     D xxAdr2          s                   like(EMMAD2)
     D xxCity          s                   like(EMMCTY)
     D xxSt            s                   like(EMMST)
     D xxZip           s                   like(EMMZIP)
     d xxName          s            120
     d xxPolName       s            120

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
     C*
     C                   Move      QQCo#         xxCo#             3 0
     C                   Move      QQFnd         xxFnd             3 0
     C                   Move      QQEmp#        xxEmp#            9 0
     C                   Move      QQDiv         xxDiv             5 0
     C                   Move      QQFyr         xxFyr             3 0
      /free

       xxProcessType = processType;

       isUseXML = UseScriptura(xxProcessType)
        or Use3rdXml(xxProcessType);

       //* Exit program if Scriptura or XML is not enabled.
       If not isUseXML;
          *inlr = *on;
          Return;
       Endif;

       Chain(n) (xxCo#:xxFnd) pfn00;
       Chain(n) (xxCo#:xxFnd) pf200;

       sysDt7 = Dateto7(SystemDate());
       Clear xmlLink;
       Clear xmlFlowName;

       isMassReq = *on;
       massFlag = 'Y';

       //* Begin reading the POC notice request file.
       Setll (xxCo#:xxFnd:0) ppo00;
       Reade (xxCo#:xxFnd:0) ppo00;

       //* Continue reading until a record for specific employer is found when
       //* running for single policy.
       If xxEmp# <> 0;
          Dow not %eof and POEmp# <> xxEmp# and PODiv <> xxDiv
           and POFyr <> xxFyr;
             Reade (xxCo#:xxFnd:0) ppo00;
          Enddo;
       Endif;

       //* Main loop to produce notices.
       Dow not %eof;

          If xmlFlowName = ' ';
             xxSvSt = POState;
             Open wxpo1l;
             Open wxpo2l;
          Endif;

          //* xmlFile - contains the name of the template XML file, used for work file linking.
          //* xmlFlowName - contains the name of the flow XML file, used for S4401C call.
          //* xmlLink - used to link individual records together in a mass run - XML flow file name.

          xmlFile = ' ';
          message = %trim(processType);
          string = ' ';

          //* Request Master record for mass run.
          If isMassReq;

             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'POC_Notice':'000':'000':massFlag:xmlLink);

             isMassReq = *off;
             //* Flow XML file name is used for the S4401C call.
             xmlFlowName = xmlFile;
             xmlLink = xmlFile;

             Chain (xmlFile) xp100;
             XPCat = 'PS';
             XPType = pocType;
             Update xp100;

          Endif;

          //*** begin chg ***  09/25/20  ************************************
          //*****Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pel01;
          Chain (POCo#:POFnd:POFyr:POEmp#:PODiv) pel01;
          //*** end chg *****  09/25/20  ************************************
          x@RRN = dbRRN;

          //* Store the policy's mailing address.
          Setll (POCo#:POFnd:POEmp#) pem00;
          Reade (POCo#:POFnd:POEmp#) pem00;

          Clear parentFein;
          Clear xxPolAdr1;
          Clear xxPolAdr2;
          Clear xxPolCity;
          Clear xxPolSt;
          Clear xxPolZip;

          Dow not %eof;
             If EMSTRP = 'Y';
                parentFein = %editc(EMFEIN:'X');
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
                Leave;
             Endif;
             Reade (POCo#:POFnd:POEmp#) pem00;
          Enddo;

          Chain (POCo#:POFnd:POEmp#:PODiv:POFyr) prp00;
          If %found;
             xxPolicy# = RPCAP#;
          Else;
             xxPolicy# = %editc(POFnd:'X') + %editc(POEmp#:'X') +
              %editc(POFyr:'X');
          Endif;

          //* Clean up any potential records.
          Setll (POCo#:POFnd:POEmp#:PODiv:POFyr:POState) ppo011;
          Reade (POCo#:POFnd:POEmp#:PODiv:POFyr:POState) ppo011;
          Dow not %eof;
             Delete ppo011;
             Reade (POCo#:POFnd:POEmp#:PODiv:POFyr:POState) ppo011;
          Enddo;

          //* Write data for late notice work file.
          Clear ppo011;
          Clear ppo012;

          Clear arrFein;
          Clear arrFile;
          Clear arrCount;
          Clear arrName;
          index = 1;

          Setll (POCo#:POFnd:POEmp#) pem00;
          Reade (POCo#:POFnd:POEmp#) pem00;

          Dow not %eof;

          //*** begin chg ***  09/25/20  ************************************
          //*****   Chain (xxCo#:xxFnd:xxEmp#:EMDiv:xxFyr) pel00;
             Chain (POCo#:POFnd:POEmp#:EMDiv:POFyr) pel00;
          //*** end chg *****  09/25/20  ************************************

             //*** begin add ***  04/22/20  *********************************
             If %found and (EMCAD1 = 0 or EMCAD1 > ELPFDT);
             //*** end add *****  04/22/20  *********************************

             //* Process employer master record if its target state and active.
             If (EMMST = POState or EMPST = POState) and %found and
              (EMCAD1 = 0 or EMCAD1 > ELPFDT);

                If EMFDNM <> ' ';
                   xxName = EMFdnm + EMFdn2;
                Else;
                   xxName = EMName + EMNam2;
                Endif;

                xxFein = %editc(EMFEIN:'X');

                //* Determine if the FEIN represents a new form.
                Exsr CreateWorkrcd;

                i = %lookup(xxFein:arrFein);
                xmlFile = arrFile(i);
                xxPolName = arrName(i);

                //* Check if division's mailing address needs to print.
                xxAdr1 = EMMAD1;
                xxAdr2 = EMMAD2;
                xxCity = EMMCTY;
                xxSt   = EMMST;
                xxZip  = EMMZIP;
                Exsr CheckAddress;

                //* Check if division's physical address needs to print.
                xxAdr1 = EMPAD1;
                xxAdr2 = EMPAD2;
                xxCity = EMPCTY;
                xxSt   = EMPST;
                xxZip  = EMPZIP;
                Exsr CheckAddress;

                //* Recheck if an address needs to be reported for DBA name.
                If EMFdnm <> ' ' and EMName <> EMFdnm and EMName <> ' ';
                   xxName = EMName + EMNam2;

                   If EMPAD1 <> ' ';
                      xxAdr1 = EMPAD1;
                      xxAdr2 = EMPAD2;
                      xxCity = EMPCTY;
                      xxSt   = EMPST;
                      xxZip  = EMPZIP;
                      Exsr CheckAddress;
                   Else;
                      xxAdr1 = EMMAD1;
                      xxAdr2 = EMMAD2;
                      xxCity = EMMCTY;
                      xxSt   = EMMST;
                      xxZip  = EMMZIP;
                      Exsr CheckAddress;
                   Endif;

                Endif;

             Endif;

             //* Check additional locations for this division.

             Setll (POCo#:POFnd:POEmp#:EMDiv) pal00;
             Reade (POCo#:POFnd:POEmp#:EMDiv) pal00;

             Dow not %eof;

                //* Process additional location record if its target state and active.
                If (ALMST = POState or ALPST = POState) and  ALEFDT < ELPTDT and
                 (ALDLDT = 0 or ALDLDT > ELPFDT);

                   If ALFDNM <> ' ';
                      xxName = ALFdnm + ALFdn2;
                   Else;
                      xxName = ALName + ALNam2;
                   Endif;

                   xxFein = %editc(ALFEIN:'X');

                   //* Determine if the FEIN represents a new form.
                   Exsr CreateWorkrcd;

                   i = %lookup(xxFein:arrFein);
                   xmlFile = arrFile(i);
                   xxPolName = arrName(i);

                   //* Check if location's mailing address needs to print.
                   xxAdr1 = ALMAD1;
                   xxAdr2 = ALMAD2;
                   xxCity = ALMCTY;
                   xxSt   = ALMST;
                   xxZip  = ALMZIP;
                   Exsr CheckAddress;

                   //* Check if location's physical address needs to print.
                   xxAdr1 = ALPAD1;
                   xxAdr2 = ALPAD2;
                   xxCity = ALPCTY;
                   xxSt   = ALPST;
                   xxZip  = ALPZIP;
                   Exsr CheckAddress;

                   //* Recheck if an address needs to be reported for DBA name.
                   If ALFdnm <> ' ' and ALName <> ALFdnm and ALName <> ' ';
                      xxName = ALName + ALNam2;

                      If ALPAD1 <> ' ';
                         xxAdr1 = ALPAD1;
                         xxAdr2 = ALPAD2;
                         xxCity = ALPCTY;
                         xxSt   = ALPST;
                         xxZip  = ALPZIP;
                         Exsr CheckAddress;
                      Else;
                         xxAdr1 = ALMAD1;
                         xxAdr2 = ALMAD2;
                         xxCity = ALMCTY;
                         xxSt   = ALMST;
                         xxZip  = ALMZIP;
                         Exsr CheckAddress;
                      Endif;

                   Endif;

                Endif;

                Reade (POCo#:POFnd:POEmp#:EMDiv) pal00;
             Enddo;

             //*** begin add ***  04/22/20  *********************************
             Endif;
             //*** end add *****  04/22/20  *********************************

             //* Read next division for policy.
             Reade (POCo#:POFnd:POEmp#) pem00;
          Enddo;

          //* Update Notice request record to indicate submitted.
          POSbdt = sysDt7;
          PODt   = sysDt7;
          POPgm  = Q1PGM;
          POUser = Q1USER;
          POTime = Timeto6(SystemTime());

          Update ppo00;

          //* Read next record for notices.
          Reade (xxCo#:xxFnd:0) ppo00;

          //* Continue reading until a record for specific employer is found when
          //* running for single policy.
          If xxEmp# <> 0;
             Dow not %eof and POEmp# <> xxEmp# and PODiv <> xxDiv
              and POFyr <> xxFyr;
                Reade (xxCo#:xxFnd:0) ppo00;
             Enddo;
          Endif;

          //* Submit request to Scriptura for the policy and/or state.
          If %eof or POState <> xxSvSt;

             Close wxpo1l;
             Close wxpo2l;

             Select;
             When xmlFlowName   = *Blanks;
             When objExists('PDSDEV':'*LIB');
                Monitor;
                S4401C(xmlFlowName + '$');
                On-error;
                Endmon;
             Other;
                S4401C(xmlFlowName + '$');
             Endsl;

             Clear xmlLink;
             Clear xmlFlowName;

             If xxEmp# <> 0;
                isMassReq = *off;
                massFlag = ' ';
             Else;
                isMassReq = *on;
                massFlag = 'Y';
             Endif;

          Endif;

       Enddo;

       *inlr = *on;
       return;

       // *=========================================================================
       // *  CreateXMLrcd - Create the XMLPREP record for the notice
       // *=========================================================================
       Begsr CreateXMLrcd;

          //* Request record for individual POC Notice.
          StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x@RRN:'X'):
           xxPre:string:%editc(POCo#:'X'):%editc(POFnd:'X'):massFlag:xmlLink);

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

          XPData = 'Y';
          XPType = pocType;
          XPDcid = PODoc;
          XPPath = POPath;
          XPLang = POLang;

          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPTime = Timeto6(SystemTime());
          XPDt   = sysDt7;

          Update xp100;

       Endsr;

       // *=========================================================================
       // *  CreateWorkrcd - Create the workfile record for the notice
       // *=========================================================================
       Begsr CreateWorkrcd;

          Chain (POCo#:POFnd:POEmp#:PODiv:POFyr:POState:xxFein) ppo011;

          If not %found;

             //* Create the request for notice in Scriptura.
             Exsr CreateXMLrcd;

             arrFein(index) = xxFein;
             arrFile(index) = xmlFile;
             arrCount(index) = 1;
             arrName(index) = xxName;
             index = index + 1;

             Clear ppo011;

             PO1Co# = POCo#;
             PO1Fnd = POFnd;
             PO1Emp# = POEmp#;
             PO1Div = PODiv;
             PO1Fyr = POFyr;
             PO1State = POState;
             PO1XML = xmlFile;
             PO1Fein = xxFein;
             PO1Name = xxName;

             If xxFein <> parentFein;
                PO1FeinP = parentFein;
             Endif;

             PO1MailAd1 = xxPolAdr1;
             PO1MailAd2 = xxPolAdr2;
             PO1MailCty = xxPolCity;
             PO1MailSt  = xxPolSt;
             PO1MailZip = xxPolZip;

             PO1NAICID = %subst(F2NAIC:1:5) + %editc(F2CARG:'X');
             PO1InsZip = FNMZip;
             PO1InsName = FNName;

             PO1TPOB = EMTPOB;
             PO1Pol# = xxPolicy#;
             PO1EffDate = DtMDYY(ELPfdt);
             PO1GovC = ELGovC;

             Clear PO1AnnPay;
             Setll (POCo#:POFnd:POEmp#:EMDiv:POFyr) pep00;
             Reade (POCo#:POFnd:POEmp#:EMDiv:POFyr) pep00;

             Dow not %eof;
                If EPStat <> 'I';
                   PO1AnnPay = PO1AnnPay + EPEAPR;
                Endif;
                Reade (POCo#:POFnd:POEmp#:EMDiv:POFyr) pep00;
             Enddo;

             PO1Dt   = sysDt7;
             PO1Pgm  = Q1PGM;
             PO1User = Q1USER;
             PO1Time = Timeto6(SystemTime());

             Write ppo011;

          Endif;

       Endsr;

       // *=========================================================================
       // *  CheckAddress - Check if address is already being listed for name.
       // *=========================================================================
       Begsr CheckAddress;


          //* Check if address needs to print (is not same as policy name/address combo).
          If xxAdr1 <> ' ' and ((xxAdr1 <> xxPolAdr1 or xxAdr2 <> xxPolAdr2
           or xxCity <> xxPolCity or xxSt <> xxPolSt or xxZip <> xxPolZip)
           or xxName <> xxPolName) and xxSt = POState;

             Chain (xmlFile:xxName:xxAdr1:xxAdr2:xxCity:xxSt:xxZip) ppo012;
             If not %found;
                Clear ppo012;

                PO2XML = xmlFile;
                PO2Key = arrCount(i);
                arrCount(i) = arrCount(i) + 1;

                PO2Name = xxName;
                PO2Adr1 = xxAdr1;
                PO2Adr2 = xxAdr2;
                PO2City = xxCity;
                PO2St   = xxSt;
                PO2Zip  = xxZip;

                PO2Pgm  = Q1PGM;
                PO2User = Q1USER;
                PO2Time = Timeto6(SystemTime());
                PO2Dt   = sysDt7;

                Write ppo012;
             Endif;

          Endif;

       Endsr;

      /end-free
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

