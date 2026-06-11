      *===============================================================*
      * Revisions:                                                    *
      *                                                               *
      * 11/02/18 C:65  Project: Phw#73970 (LUBA)                      *
      *                Data flag must be 'Y' for single endorsement   *
      *                print requests - to create SXDOP record.       *
      *                                                               *
      * 10/16/18 C:180 Support: 72139                                 *
      *                Populate "Jurisdiction State" and              *
      *                "Generate PDF" fields in work file WXWXFMP.    *
      *                                                               *
      * 11/02/17 C:65  Project: SY371                                 *
      *                Check client path for template.                *
      *                                                               *
      *===============================================================*

      /Free
       Begsr Scriptura;
       // Determine if this module get spool file and/or XML output

          isUseSpoolOut = UseSpoolOut('Policy Page');
          isUseXML = UseScriptura('Policy Page') Or
             Use3rdXml('Policy Page');
          If isUseXML;
             isScriptUsed = 'Y';
          Else;
             isScriptUsed = 'N';
          Endif;

       Endsr;


       Begsr WriteWorkFile;

          Monitor;

             If isScriptUsed = 'N' or isUseSpoolOut = *On;
                write pfm01;
             EndIf;

             If isScriptUsed = 'Y';
                Exsr GetXMLFileNm;

                Open WXWXFMP;
                FMxml = inXML;
                FMxmlT = XPxmlT;
                FMFORMF = formName;

                //*** Begin Add *** 10/16/18 ***********************************
                //Jurisdiction state;
                Chain (xxco#:ELrtcd) PRS01;
                If %found(WMRSL);
                   FMjrst = RSjrst;
                Else;
                   FMjrst = *blanks;
                Endif;

                // Generate PDF
                FMgpdf = ENgpdf;
                //*** End   Add *** 10/16/18 ***********************************

                write FMRecord;
                Close WXWXFMP;

                If Not PrintingPolicy;
                   inXML@ = inXML + '$';
                   SendToScriptura(inXML@);
                EndIf;

             Endif;

          On-error;
          Endmon;

       Endsr;

       Begsr GetXMLFileNm;

          If PrintingPolicy;
             in *dtaara;
             outCall = 'TEMPLRQST';
             inXML = ldaXML;
             OutPrefix = '  ';
             OutString = xxForm;
             InReturn ='Policy Page';
             outMode = ' ';
             StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:'000':'000');

             Chain inXML XMLTEML1;
             If %found(XMLTEML1);
                XTdcid = %Trim(xxForm) + '.SCD';
                XPXMLT = XTXMLT;
                If RunningSRText;
                   XTtype = 'Text Only Endorsement';
                Else;
                   XTtype = %SubSt(xxForm:1:8);
                Endif;
             //*** begin add ***  11/02/17   *************************************
                If ENPathC <> ' ';
                   XTPath = ENPathC;
                Endif;
             //*** end add *****  11/02/17   *************************************
                Update XT100;
             EndIf;

          Else;
             outCall = 'XMLREQUEST';
             OutPrefix = '  ';
             OutString = xxForm;
             InReturn ='Policy Page';
             outMode = ' ';
             StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:'000':'000');

             Chain inXML XMLPREL1;
             If %found(XMLPREL1);
             //*** begin chg ***  11/02/18  ***********************************
             //*****   XPdata = *blanks;
                XPdata = 'Y';
                XPCat = 'PS';
                XPCo# = ELCo#;
                XPFnd = ELFnd;
                XPEmp# = ELEmp#;
                XPDiv = ELDiv;
                XPFyr = ELFyr;
             //*** end chg *****  11/02/18  ***********************************
                XPdcid = %Trim(xxForm) + '.SCD';
                If RunningSRText;
                   XPtype = 'Text Only Endorsement';
                Else;
                   XPtype = %SubSt(xxForm:1:8);
                Endif;
             //*** begin add ***  11/02/17   *************************************
                If ENPathC <> ' ';
                   XPPath = ENPathC;
                Endif;
             //*** end add *****  11/02/17   *************************************
                Update XP100;
             Endif;
          EndIf;

       Endsr;

      /End-free
