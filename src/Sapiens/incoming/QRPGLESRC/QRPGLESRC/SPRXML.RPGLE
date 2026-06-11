     d dsXmlMsg        ds                  dtaara('S4401MSG')
     d  strXmlMsg                   250
       //===================================================================
       // Write LDA values to file for Scriptura processes.
       //===================================================================
     dCreateKeyRecordXML...
     d                 pr            10
     d inBody                     16000    options(*nopass)
       //===================================================================
       // Create the XML (and Scriptura)
       //===================================================================
     dCreateXML        pr
     d inXML                         60    const
     d inSubmit                       1n   const options(*nopass)

       //===================================================================
       // External program call to create the XML (and Scriptura)
       //===================================================================
     dCreateXMLInt     pr                  extpgm('S4401C')
     d inXML                         61    const

       //===================================================================
       // External program call to create the XML (and Scriptura) - Submitter
       //===================================================================
     dCreateXMLSbm     pr                  extpgm('S4400C')
     d inXML                         61    const

       //===================================================================
       // Get the output processing detail
       //===================================================================
     dGetRecord        pr             1n
     d inProcess                    100    const

       //===================================================================
       // Get the XMLPREP  detail
       //===================================================================
     dGetRecordXML     pr             1n
     d inXMLFile                     60    const

       //===================================================================
       // Determine unique key value based on job # and # of records with it.
       //===================================================================
     dGetUniqueKeyXML  pr            10

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
     dGetXMLFileName   pr            60
     d inMode                         1    const
     d inRRN                          9  0 const options(*nopass)
     d inCo#                          3  0 const options(*nopass)
     d inFnd                          3  0 const options(*nopass)
     d inPrefix                       2    const options(*nopass)
     d inStrPrefix                   10    const options(*nopass)
     d inReturn                     250    const options(*nopass)

       //===================================================================
       // Get the XML output fle name (including directory)
       //===================================================================
     dGetXMLOutputName...
     d                 pr           250
     d inXMLFile                     60    const
     d inIncludeDir                   1n   const
     d inNum                          3  0 const options(*nopass)

       //===================================================================
       // Get the XML template name to be used with Scriptura
       // http://worsharepoint/tech/common/scriptura/Documents/Technical/Scriptura Phase 2 Quick Inf
       //===================================================================
     dGetXMLTempName   pr            60
     d inXMLFile                     60    const

       //===================================================================
       // Determine if 3rd party XML should be used
       //===================================================================
     dUse3rdXML        pr             1n
     d inProcess                    100    const
     d outFolder                    250    options(*nopass)
     d outSndDataQ                   10    options(*nopass)
     d outRcvDataQ                   10    options(*nopass)

       //===================================================================
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
     dRequestTemplate  pr            60
     d inXmlFile                     60
     d inProcessType                250    options(*nopass)
     d inMode                         1    const options(*nopass)
     d inRrn                          9  0 const options(*nopass)
     d inPrefix                       2    const options(*nopass)
     d inString                      10    const options(*nopass)
     d inCo#                          3  0 const options(*nopass)
     d inFnd                          3  0 const options(*nopass)

       //===================================================================
       // Deterime if the messages should be shown when processing
       //  Parm 1 - XML file name or Process Type
       //  Parm 2 - 'XML' or 'Process' (optional, defaults to 'XML')
       //  Return - *on if the messages should be shown, otherwise *off
       //===================================================================
     dShowXmlMessages  pr             1n
     d inKey                        100    const
     d inKeyType                     10    const options(*nopass)

       //===================================================================
       // Determine if Scriptura should be used
       //===================================================================
     dUseScriptura     pr             1n
     d inProcess                    100    const
     d outFolder                    250    options(*nopass)
     d outSndDataQ                   10    options(*nopass)
     d outRcvDataQ                   10    options(*nopass)
     d outArcFolder                 250    options(*nopass)

       //===================================================================
       // Determine if spooled output should be used
       //===================================================================
     dUseSpoolOut      pr             1n
     d inProcess                    100    const
     d outRetire                      1n   options(*nopass)

