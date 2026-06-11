       // Revisions
       //
       // 11/02/18 C:65  Project: Phw#73970 (LUBA)
       //                Data flag must be 'Y' for single endorsement
       //                print requests - to create SXDOP record.
       //
       // 02/23/18 C:062 Project:148WAS
       //                - Override Form
       //                - Add Generate from PDF                            ts
       //                - Add Client Document Path
       //
       // 12/01/16 C:136 Project:SY371
       //                Implement Scriptura Phase 2 for Mid-Term Endorsements
       //                Copied SCOPYEN02C and:
       //                - Added ChkPgmInStack as external procedure
       //                - Added processType 250A
       //                - Added pgmInStack    1A
       //
      /Free
       Begsr Scriptura;
       // Determine if this module get spool file and/or XML output
       // Check if this is a Mid-Term Endorsement or not
          ChkPgmInStack('W7051C1':pgmInStack);
          If pgmInStack = *On;
             processType = 'Mid-Term Endorsement';
          Else;
             processType = 'Policy Page';
          Endif;
          isUseSpoolOut = UseSpoolOut(processType);
          isUseXML = UseScriptura(processType) Or
             Use3rdXml(processType);
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

                If processType = 'Mid-Term Endorsement';
                   PrintingPolicy = *on;
                Endif;

                Exsr GetXMLFileNm;

                Open WXWXFMP;
                FMxml = inXML;
                FMxmlT = XPxmlT;
                FMFORMF = formName;
                //*** Begin Add *** 02/23/18 ***********************
                FMformO = ENformO;
                //*** End   Add *** 02/23/18 ***********************
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
             InReturn = processType;
             outMode = ' ';
             StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:'000':'000');

             Chain inXML XMLTEML1;
             If %found(XMLTEML1);
                XTdcid = %Trim(xxForm) + '.SCD';
                XPXMLT = XTXMLT;
             //*** begin add *** 02/23/18 ********************************* ***
                // Generate from PDF file
                If ENGPDF = 'Y';
                   XTdcid = %Trim(xxForm) + '.PDF';
                   XTGPDF = 'Y';
                Endif;
                // Client Document Path
                If ENPathC <> ' ';
                   XTPath = ENPathC;
                Endif;
             //*** end   add *** 02/23/18 ********************************* ******
                If RunningSRText;
                   XTtype = 'Text Only Endorsement';
                Else;
                   XTtype = %SubSt(xxForm:1:8);
                Endif;
                Update XT100;
             EndIf;

          Else;
             outCall = 'XMLREQUEST';
             OutPrefix = '  ';
             OutString = xxForm;
             InReturn = processType;
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
             //*** begin add *** 02/23/18 ********************************* ***
                // Generate from PDF file
                If ENGPDF = 'Y';
                   XPdcid = %Trim(xxForm) + '.PDF';
                   XPGPDF = 'Y';
                Endif;
                // Client Document Path
                If ENPathC <> ' ';
                   XPPath = ENPathC;
                Endif;
             //*** end   add *** 02/23/18 ********************************* ******
                If RunningSRText;
                   XPtype = 'Text Only Endorsement';
                Else;
                   XPtype = %SubSt(xxForm:1:8);
                Endif;
                Update XP100;
             Endif;
          EndIf;

       Endsr;

      /End-free
