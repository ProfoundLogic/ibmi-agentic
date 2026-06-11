     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7191R - Carrier Policy # Report
     H*****************************************************************
     H*  Description:                                                 *
     H*    This program reads thru the group select file and then     *
     H*    read thru WMRPP by RPIDT and populate two Scriptura files. *
     H*    WXCPSP - Carrier Policy # - Selection Criteria File        *
     H*    WXCPOP - Carrier Policy # - Output File                    *
     H*                                                               *
     H*  Called From: W7291C - Carrier Policy # Report - Override     *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************2***********************
     H* Create on 03/29/2017 by c:62 for Project 148WAG               *
     H*****************************************2***********************
     H* Revisions:                                                    *
     H*
     H* 12/16/21 C:300 Support: 94825
     H*                Carrier Policy Report error
     H*                Pass the document name
     H*                                                               *
     H*****************************************************************
     FSMCOL     if   e           k Disk    Rename(pco01:pco00)
     FSTABL     if   e           k Disk    Rename(pst01:pst00)
     FSXCPOP    if a e             Disk    Rename(pcpo01:pcpo0p)
     FSXCPSP    if a e             Disk    Rename(pcps01:pcps0p)
     FWDELL     if   e           k Disk    Rename(pel01:pel00)
     FWMFNL     if   e           k Disk    Rename(pfn01:pfn00)
     FWMEML     if   e           k Disk    Rename(pem01:pem00)
     FWMRPL     if   e           k Disk    Rename(prp01:prp00)
     FWMRSL     if   e           k Disk    Rename(prs01:prs00)
     FWXGSL     if   e           k Disk    Rename(PGS01:PGS00)
     FXMLPREL1  uf   e           k Disk

     d*****************************************************************
     d* Prototype Declaration
     d*****************************************************************
     d* Entry
     d GetClientID     pr                  extpgm('RTVCLTID')
     D   outCltID                     3

     d* Main Processing
     d Main            pr
     d

     DObjExists        pr             1n
     D object                        10    const
     D objType                        7    const

     d* Program End
     d ProgramEnd      pr

     d* Program Start
     d ProgramStart    pr

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

     d* Write to Selection file - SXCPSP
     d WriteSXCPSP     pr

     d* Write to Output file - SXCPOP
     d WriteSXCPOP     pr

     d* Entry
     d W8191R          pr                  Extpgm('W8191R')
     D   company#                     3
     D   fromDate                     8
     D   toDate                       8
     D   queue                       10
     D   flow                        61
     d W8191R          pi
     D   company#                     3
     D   fromDate                     8
     D   toDate                       8
     D   queue                       10
     D   flow                        61

     d*****************************************************************
     d* Work Fields
     d*****************************************************************
     D psds          esds                  ExtName(SZQ1P)
     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d xmlFlowName     s                   like(CPSXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFileName     s                   like(CPSXML)
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
     d xmlFile         s             60    inz
     d xmlKey          s             60    inz
     d fromDate8       s              8  0
     d toDate8         s              8  0
     d fromDate7       s              7  0
     d toDate7         s              7  0
     d currDate        s              7  0
     d chrCompName     s             40
     d stateKey        s             10
     d massFlag        s              1
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)

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

       *inLR = *On;
      /End-Free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Start Of Program
       ProgramStart();

       // Populate the Selection Criteria File.
       WriteSXCPSP();

       // Populate the Output File.
       WriteSXCPOP();

       // End   Of Program
       ProgramEnd();

      /End-Free

     p Main            e
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
       //===================================================================
       // ProgramEnd - End Program Processing
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi

      /Free

       If xmlFlowName <> ' ';
          flow = xmlFlowName + '$';
       Endif;

      /End-Free

     p ProgramEnd      e
       //===================================================================
       //
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Get Company Name
       outCo#      = %Int(company#);
       chrCompName = GetCompNam(outCo#);

       // Prepare Selection Date Range
       fromDate8 = %Int(fromDate);
       If fromDate8 <> 0;
          fromDate7 = DTcymd(fromDate8);
       Else;
          fromDate7 = 0;
       EndIf;

       toDate8   = %Int(toDate);
       If toDate8 <> 0;
          toDate7 = DTcymd(toDate8);
       Else;
          toDate7 = 9999999;
       EndIf;

       // Get the system date
       currdate = DateTo7(SystemDate());

       // Get the client id
       GetClientID(outCltId);

       processType    = 'Carrier Policy# Report';
       outReturn      = processType;
       outFldrPrefix  = *blanks;
       xmlFileName    = *blanks;
       isUseSpoolOut  = UseSpoolOut(processType);
       isUseScriptura = UseScriptura(processType);
       isUse3rdParty  = Use3rdXml(processType);
       isUseXML = isUseScriptura or isUse3rdParty;

       If isUseXML;
          outPreString = 'CarPolRpt';
          //*** Beg Add *** 12/16/21 *******************************************
          string = 'CarPol#Rpt';
          //*** End Add *** 12/16/21 *******************************************

          //* Request record for individual Estimated Late Notice.
          message = %trim(processType);
          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':
           xxPre:string:'000':'000':massFlag:xmlLink);

          xmlKey       = xmlFile;
          xmlFileName  = xmlFile;
          xmlFlowName  = xmlFile;

          //* The template XML file name must be pulled from record for work files for a
          //* single request.  Otherwise it has been returned from the XML Request call.
          If xmlLink = ' ';
             Chain (xmlFile) xp100;
             xmlFile = XPXmlT;
          Else;
             Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          Endif;

          XPData = ' ';

          XPPgm  = Q1pgm;
          XPUser = Q1user;
          XPTime = TimeTo6(SystemTime());
          XPDt   = DateTo7(SystemDate());

          Update xp100;
       Endif;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // WriteSXCPOP - Write to WXCPOP - Report Output
       //===================================================================
     p WriteSXCPOP     b
     d WriteSXCPOP     pi

      /Free

       // Read Thru Group Select File
       SetLL *loval pgs00;
       Read pgs00;
       DoU %EOF(wxgsl);
          // Only process if the group was selected
          If GSieFL = 'I';
             // Read Thru the Carrier Policy File
             Chain (GSco#:GSfnd) prp00;
             If %Found(wmrpl);
                DoU %Eof(wmrpl);
                   If RPidt >= fromDate7 And RPiDt <= toDate7;
                      Clear pcpo0p;
                      CPOxml   = xmlKey;
                      CPOfnd   = GSfnd;
                      CPOgNam  = GSname;

                      Chain (GSco#:GSfnd) pfn00;
                      If %Found(wmfnl);
                         CPOgShN = FNshNm;
                      EndIf;

                      CPOemp#  = RPemp#;
                      CPOdiv   = RPdiv;
                      CPOfYr   = RPfYr;
                      CPOcap#  = RPcap#;

                      Chain (RPco#:RPfnd:RPemp#:RPdiv) pem00;
                      If %Found(wmeml);
                         CPOeNam = EMname + EMnam2;
                      EndIf;

                      Chain (RPco#:RPfnd:RPemp#:RPdiv:RPfYr) pel00;
                      If %Found(wdell);
                         CPOpFdt = dtMDYY(ELpFdt);
                         CPOpTdt = dtMDYY(ELpTdt);

                         Chain (ELco#:ELrtCd) prs00;
                         If %Found(wmrsl);
                            CPOratSt = RSjrSt;
                            stateKey = RSjrSt;
                            Chain stateKey pst00;
                            If %Found(stabl);
                               CPOrtStNm = STname;
                            EndIf;
                         EndIf;
                      EndIf;

                      CPOiDt   = currDate;

                      Write pcpo0p;

                   EndIf;
                   ReadE (GSco#:GSfnd) prp00;
                EndDo;
             EndIf;
          EndIf;
          Read pgs00;
       EndDo;

      /End-Free

     p WriteSXCPOP     e
       //===================================================================
       // WriteSXCPSP - Write to WXCPOP - Report Selection Criteria
       //===================================================================
     p WriteSXCPSP     b
     d WriteSXCPSP     pi

      /Free

       // Read thru the group select file
       SetLL *loval pgs00;
       Read pgs00;
       DoU %EOF(wxgsl);
          // Only process if the group was selected
          If GSieFL = 'I';
             Clear pcps0p;
             CPSxml   = xmlKey;
             CPSco#   = outCo#;
             CPScoNm  = chrCompName;
             CPSfnd   = GSfnd;
             CPSgNam  = GSname;

             // Get the Groups Short Name
             Chain (GSco#:GSfnd) pfn00;
             If %Found(wmfnl);
                CPSgShN = FNshNm;
             EndIf;

             CPSfrmDt = fromDate8;
             CPStoDt  = toDate8;
             CPSiDt   = currDate;
             Write pcps0p;
          EndIf;
          Read pgs00;
       EndDo;

      /End-Free

     p WriteSXCPSP     e
