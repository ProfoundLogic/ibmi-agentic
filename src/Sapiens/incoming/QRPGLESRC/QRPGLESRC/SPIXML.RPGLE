     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
      **********************************************************************
     h/title SPIXML - Procedures for XML information needed for Scriptura
     h nomain
     h DFTNAME(SPIXML)
     h option(*nodebugio: *srcstmt)
      **********************************************************************
      *  Created: 10/23/15 C:33    Project: SY371
      **********************************************************************
      * Revisions:
      *
      * 05/01/23 C:62  Project: WC2838
      *                -Fix lda body size.
      *                -Move %key% to xxtray position
      *
      * 04/22/21 C:65  Project: WC2278
      *                Add procedures for storing LDA values for Scriptura
      *                in an XMLPREP record with a unique key - GetUniqueKeyXML
      *                and CreateKeyRecordXML.
      *
      * 07/24/17 C:33  Project: WC2405
      *               -Add flags for determining if processing
      *                 messages should be shown (ShowXmlMessages).
      *               -Get secondary output name.
      *
      * 06/09/17 C:136 Support: 47334
      *                Add a template request procedure (RequestTemplate).
      *
      * 08/29/16 C:33  SY371
      *               -Add retrieval of XML template
      *               -Add creation of XML from data
      *
      * 04/15/16 C:136 SY371
      *                Add return parm to call of STLETTER in GetXMLFileName
      *
      **********************************************************************
     Fstspl     if   e           k disk    rename(psp01:psp00)
     Fxmlprel1  if   e           k disk    rename(xp100:xp101)
     F*** begin add ***  04/22/21  *****************************************
     Fxmlprel4  if a e           k disk    rename(xp100:xp104)
     F*** end add *****  04/22/21  *****************************************

     dStLetter         pr                  ExtPgm('STLETTER')
     d outCall                       10    const
     d inXML                         60
     d inReturn                     250
     d outMode                        1    const
     d outRRN                         9    const
     d outPrefix                      2    const
     d outString                     10    const
     d outCo#                         3    const
     d outFnd                         3    const

     D*** begin add ***  04/22/21  **************************************
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxHold                         1    overlay(svlda:189)
     D xxSave                         1    overlay(svlda:190)
     D xxDarts                        1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopyOut                      1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d*** Begin Add *** 05/01/23 ********************************
     d*xxBody***                    350    overlay(svlda:673)
     d xxBody                       200    overlay(svlda:673)
     d*** End   Add *** 05/01/23 ********************************
     d*** Begin Chg *** 05/01/23 ********************************
     d*xxLDATag                      10    overlay(svlda:251)
     d*xxLDAKey                      10    overlay(svlda:261)
     d xxLDATag                      10    overlay(svlda:269)
     d xxLDAKey                      10    overlay(svlda:279)
     d*** End   Chg *** 05/01/23 ********************************

     D PSDS          ESDS                  extname(SZQ1P)
     D*** end add *****  04/22/21  **************************************

     d/copy sPrJobInfo
     d/copy sPrXML

      *** begin add ***  04/22/21  *********************************************
       //===================================================================
       // Write LDA values to file for Scriptura processes.
       //===================================================================
     PCreateKeyRecordXML...
     P                 b                   Export
     dCreateKeyRecordXML...
     d                 pi            10
     d inBody                     16000    options(*nopass)

     dstrKey           s             10

      /Free

       //* Get key value for record and read in LDA values.
       strKey = GetUniqueKeyXML();
       In *Lock *dtaara;
       Out *dtaara;

       Clear xp104;
       //* Set values into record.
       XPPgm    = strKey;
       XPDart   = xxDarts;
       XPRmcd   = xxRmcd;
       XPOutFld = xxCopyOut;
       XPHold   = xxHold;
       XPSave   = xxSave;
       If xx#CopiesA <> ' ';
          XPCopy = %int(xx#CopiesA);
       Endif;
       XPForm   = xxPrinter;
       XPRType  = xxPrint;
       XPTray   = xxTray;
       XPIntr   = xxEmail;
       XPMail   = xxToEmail;
       XPSubj   = xxSubject;
       XPMailAdd = %trim(xxBody);

       If %parms >= 1;
          XPBody = inBody;
       Endif;

       //* Write record for values.
       Write xp104;

       //* Update LDA with key indicator and value.
       In *Lock *dtaara;
       xxLDATag = '%key%     ';
       xxLDAKey = XPPgm;
       Out *dtaara;

       Return XPPgm;

      /End-free

     PCreateKeyRecordXML...
     P                 e
      *** end add *****  04/22/21  *********************************************
       //===================================================================
       // Create the XML (and Scriptura)
       //===================================================================
     PCreateXML        b                   Export
     dCreateXML        pi
     d inXML                         60    const
     d inSubmit                       1n   const options(*nopass)

     doutXML           s             61
     disSubmit         s              1n   inz('1')

      /Free
       If GetJobType = 'BCH';
          isSubmit = *off;
       Endif;

       If %parms >= 2;
          isSubmit = inSubmit;
       Endif;

       outXML = %trim(inXML);
       %subst(outXML:61:1) = '$';

       If isSubmit;
          CreateXMLSbm(outXML);
       Else;
          CreateXMLInt(outXML);
       Endif;

       Return;
      /End-free

     PCreateXML        e

       //===================================================================
       // Get the output processing detail
       //===================================================================
     PGetRecord        b                   Export
     dGetRecord        pi             1n
     d inProcess                    100    const

      /Free
       Chain (inProcess) stspl;
       Return %found;
      /End-free

     PGetRecord        e

       //===================================================================
       // Get the XMLPREP  detail
       //===================================================================
     PGetRecordXML     b                   Export
     dGetRecordXML     pi             1n
     d inXMLFile                     60    const

      /Free
       Chain (inXMLFile) xmlprel1;
       Return %found;
      /End-free

     PGetRecordXML     e
      *** begin add ***  04/22/21  *********************************************
       //===================================================================
       // Determine unique key value based on job # and # of records with it.
       //===================================================================
     PGetUniqueKeyXML  b                   Export
     dGetUniqueKeyXML  pi            10

     ddecCount         s              4s 0
     dstrJob#          s              6a

      /Free
       decCount = 0;
       strJob# = %editc(q1jobn:'X');

       Setll (strjob# + '    ') xp104;
       Read xp104;
       Dow not %eof;
          If %subst(XPPgm:1:6) = strJob#;
             decCount += 1;
          Else;
             Leave;
          Endif;
          Read xp104;
       Enddo;

       Return %editc(q1jobn:'X') + %editc(decCount:'X');

      /End-free

     PGetUniqueKeyXML  e
      *** end add *****  04/22/21  *********************************************

       //===================================================================
       // Get the XML file name to be used with Scriptura
       // http://worsharepoint/tech/common/scriptura/Documents/Technical/Scriptura Phase 2 Quick Inf
       //
       // inMode - 'E' - Policy
       //          'C' - Claim
       //          'M' - Medical Case
       //          'P' - Pre-certification
       //          ' ' - Generic
       // inRRN - if non-Generic mode, assosciated file RRN is required
       // inCo# - if Generic mode, required
       // inFnd - if Generic mode, required
       // inPrefix - used to determine where the output is going to be
       //            stored on the IFS (as it incorporates our system folder
       //            control process for folders
       // inStringPrefix - used for building the document name if one of the
       //                  preset modes is not used.  Up to 10 characters
       //                  can be passed and it will start the document
       //                  name with this value.
       // inReturn - if passed the process type, STLETTER will populate
       //            XMLPREP with various settings from STSPP
       //===================================================================
     PGetXMLFileName   b                   Export
     dGetXMLFileName   pi            60
     d inMode                         1    const
     d inRRN                          9  0 const options(*nopass)
     d inCo#                          3  0 const options(*nopass)
     d inFnd                          3  0 const options(*nopass)
     d inPrefix                       2    const options(*nopass)
     d inStrPrefix                   10    const options(*nopass)
     d inReturn                     250    const options(*nopass)

     d strCo#          s              3    inz(*all'0')
     d strFnd          s              3    inz(*all'0')
     d strPrefix       s              2    inz
     d strReturn       s            250    inz
     d strRRN          s              9    inz(*all'0')
     d strStrPrefix    s             10    inz
     d strXMLName      s             60    inz

      /Free
       If %parms >= 2;
          strRRN = %editc(inRRN:'X');
       Endif;
       If %parms >= 3;
          strCo# = %editc(inCo#:'X');
       Endif;
       If %parms >= 4;
          strFnd = %editc(inFnd:'X');
       Endif;
       If %parms >= 5;
          strPrefix = inPrefix;
       Endif;
       If %parms >= 6;
          strStrPrefix = inStrPrefix;
       Endif;
       If %parms >= 7;
          strReturn = inReturn;
       Endif;

       Callp StLetter('XMLREQUEST':strXMLName:strReturn:inMode:strRRN:
        strPrefix:strStrPrefix:strCo#:strFnd);

       Return strXMLName;

      /End-Free
     p GetXMLFileName  e

       //===================================================================
       // Get the XML output fle name (including directory)
       //===================================================================
     PGetXMLOutputName...
     P                 b                   Export
     dGetXMLOutputName...
     d                 pi           250
     d inXMLFile                     60    const
     d inIncludeDir                   1n   const
     d***  Begin Add  ***  07/24/17  **************************************
     d inNum                          3  0 const options(*nopass)

     d decNum          s              3  0 inz(1)
     d***  End   Add  ***  07/24/17  **************************************
      /Free
       //***  Begin Add  ***  07/24/17  ***********************************
       If %parms >= 3 and inNum <> *zero;
          decNum = inNum;
       Endif;
       //***  End   Add  ***  07/24/17  ***********************************
       Clear XPdir;
       Clear XPfile;
       //***  Begin Add  ***  07/24/17  ***********************************
       Clear XPdir2;
       Clear XPfile2;
       //***  End   Add  ***  07/24/17  ***********************************
       GetRecordXML(%trim(inXMLFile));
       If not inIncludeDir;
          Clear XPdir;
          //***  Begin Add  ***  07/24/17  ********************************
          Clear XPdir2;
          //***  End   Add  ***  07/24/17  ********************************
       Endif;
       //***  Begin Chg  ***  07/24/17  ***********************************
       //Return %trim(XPdir) + %trim(XPfile);
       Select;
       When decNum = 1;
          Return %trim(XPdir) + %trim(XPfile);
       When decNum = 2;
          Return %trim(XPdir2) + %trim(XPfile2);
       Other;
          Return *blanks;
       Endsl;
       //***  End   Chg  ***  07/24/17  ***********************************
      /End-Free
     PGetXMLOutputName...
     P                 e

       //===================================================================
       // Get the XML template name to be used with Scriptura
       // http://worsharepoint/tech/common/scriptura/Documents/Technical/Scriptura Phase 2 Quick Inf
       //===================================================================
     PGetXMLTempName   b                   Export
     dGetXMLTempName   pi            60
     d inXMLFile                     60    const

      /Free
       Clear XPxmlT;
       GetRecordXML(%trim(inXMLFile));
       Return %trim(XPxmlT);
      /End-Free
     PGetXMLTempName   e
      *** beg add ***** 06/09/17 *************************************
       //===================================================================
       // Returns template file name.
       // Creates XML template record to be used with Scriptura
       // http://worsharepoint/tech/common/scriptura/Documents/Technical/Scriptura Phase 2 Quick Inf
       //
       // inMode - 'E' - Policy
       //          'C' - Claim
       //          'M' - Medical Case
       //          'P' - Pre-certification
       //          ' ' - Generic
       // inRRN - if non-Generic mode, assosciated file RRN is required
       // inCo# - if Generic mode, required
       // inFnd - if Generic mode, required
       // inPrefix - used to determine where the output is going to be
       //            stored on the IFS (as it incorporates our system folder
       //            control process for folders
       // inStringPrefix - used for building the document name if one of the
       //                  preset modes is not used.  Up to 10 characters
       //                  can be passed and it will start the document
       //                  name with this value.
       // inReturn - if passed the process type, STLETTER will populate
       //            XMLTEMP with various settings from STSPP
       //===================================================================
     PRequestTemplate  b                   Export
     dRequestTemplate  pi            60
     d inTemplateName                60
     d inProcessType                250    options(*nopass)
     d inMode                         1    const options(*nopass)
     d inRrn                          9  0 const options(*nopass)
     d inPrefix                       2    const options(*nopass)
     d inString                      10    const options(*nopass)
     d inCo#                          3  0 const options(*nopass)
     d inFnd                          3  0 const options(*nopass)

     d strCo#          s              3    inz(*all'0')
     d strFnd          s              3    inz(*all'0')
     d chrMode         s              1    inz
     d strPrefix       s              2    inz
     d strReturn       s            250    inz
     d strRRN          s              9    inz(*all'0')
     d strStrPrefix    s             10    inz
     d strXMLName      s             60    inz

      /Free
       strXmlName = inTemplateName;
       If %parms >= 2;
          strReturn = inProcessType;
       Endif;
       If %parms >= 3;
          chrMode   = inMode;
       Endif;
       If %parms >= 4;
          strRRN = %editc(inRRN:'X');
       Endif;
       If %parms >= 5;
          strPrefix = inPrefix;
       Endif;
       If %parms >= 6;
          strStrPrefix = inString;
       Endif;
       If %parms >= 7;
          strCo# = %editc(inCo#:'X');
       Endif;
       If %parms >= 8;
          strFnd = %editc(inFnd:'X');
       Endif;

       Callp StLetter('TEMPLRQST':strXmlName:strReturn:inMode:strRRN:
        strPrefix:strStrPrefix:strCo#:strFnd);

       Return strXmlName;

      /End-Free
     pRequestTemplate  e
      *** end add ***** 06/09/17 *************************************

      ***  Begin Add  ***  07/24/17  **************************************
       //===================================================================
       // Deterime if the messages should be shown when processing
       //  Parm 1 - XML file name or Process Type
       //  Parm 2 - 'XML' or 'Process' (optional, defaults to 'XML')
       //  Return - *on if the messages should be shown, otherwise *off
       //===================================================================
     PShowXmlMessages  b                   export
     dShowXmlMessages  pi             1n
     d inKey                        100    const
     d inKeyType                     10    const options(*nopass)

     dstrJobType       s              3    inz
     dstrKeyType       s             10    inz('XML')
     dstrType          s            100    inz
      /Free
       strType = inKey;
       If %parms >= 2 and inKeyType <> *blanks;
          strKeyType = inKeyType;
       Endif;
       If strKeyType = 'XML';
          Clear XPtype;
          GetRecordXML(%trim(%subst(inKey:1:60)));
          strType = XPtype;
       Endif;
       Clear SPmsgInt;
       Clear SPmsgBch;
       GetRecord(strType);
       strJobType = GetJobType();
       Select;
       When strJobType = 'BCH';
          Return not(SPmsgBch = 'Y');
       When strJobType = 'INT';
          Return not(SPmsgInt = 'Y');
       Other;
          Return *on;
       Endsl;
      /End-Free
     PShowXmlMessages  e
      ***  End   Add  ***  07/24/17  **************************************

       //===================================================================
       // Determine if 3rd party XML should be used
       //===================================================================
     PUse3rdXML        b                   Export
     dUse3rdXML        pi             1n
     d inProcess                    100    const
     d outFolder                    250    options(*nopass)
     d outSndDataQ                   10    options(*nopass)
     d outRcvDataQ                   10    options(*nopass)

     disUse3rdXML      s              1n   inz('0')
     dstrFolder        s            250    inz
     dstrRcvDataQ      s             10    inz
     dstrSndDataQ      s             10    inz

      /Free
       If GetRecord(inProcess);
          isUse3rdXML = SP3Xml = 'Y';
          strFolder   = SP3XmlF;
          strSndDataQ = SP3DtQS;
          strRcvDataQ = SP3DtQR;
       Endif;

       If %parms >= 2;
          outFolder = strFolder;
       Endif;
       If %parms >= 3;
          outSndDataQ = strSndDataQ;
       Endif;
       If %parms >= 4;
          outRcvDataQ = strRcvDataQ;
       Endif;

       Return isUse3rdXML;

      /End-free

     PUse3rdXML        e

       //===================================================================
       // Determine if Scriptura should be used
       //===================================================================
     PUseScriptura     b                   Export
     dUseScriptura     pi             1n
     d inProcess                    100    const
     d outFolder                    250    options(*nopass)
     d outSndDataQ                   10    options(*nopass)
     d outRcvDataQ                   10    options(*nopass)
     d outArcFolder                 250    options(*nopass)

     disUseScriptura   s              1n   inz('0')
     dstrArcFolder     s            250    inz
     dstrFolder        s            250    inz
     dstrRcvDataQ      s             10    inz
     dstrSndDataQ      s             10    inz

      /Free
       If GetRecord(inProcess);
          isUseScriptura = SPscrO = 'Y';
          strFolder      = SPsXmlF;
          strArcFolder   = SPsXmlA;
          strSndDataQ    = SPsDtQS;
          strRcvDataQ    = SPsDtQR;
       Endif;

       If %parms >= 2;
          outFolder = strFolder;
       Endif;
       If %parms >= 3;
          outSndDataQ = strSndDataQ;
       Endif;
       If %parms >= 4;
          outRcvDataQ = strRcvDataQ;
       Endif;
       If %parms >= 5;
          outArcFolder = strArcFolder;
       Endif;

       Return isUseScriptura;

      /End-free

     PUseScriptura     e

       //===================================================================
       // Determine if spooled output should be used
       //===================================================================
     PUseSpoolOut      b                   Export
     dUseSpoolOut      pi             1n
     d inProcess                    100    const
     d outRetire                      1n   options(*nopass)

     disUseSpoolOut    s              1n   inz('1')
     disRetire         s              1n   inz('0')

      /Free
       If GetRecord(inProcess);
          isUseSpoolOut = SPsplO = 'Y';
          isRetire      = SPsplR = 'Y';
       Endif;

       If %parms >= 2;
          outRetire = isRetire;
       Endif;

       Return isUseSpoolOut;

      /End-free

     PUseSpoolOut      e

