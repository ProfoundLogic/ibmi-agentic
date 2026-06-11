      *=========================================================
      * Add error messages to array
      *  - Use this routine to store up messages for later processing to JSON string
      *  - Include the ProcessErrorMessagesJson procedure at the appropriate place in your JSON
      *=========================================================
     dAddErrorMessageJson...
     d                 pr
     d inMessage                    500    varying const
     d outRtnCode                    10    options(*nopass)

     P*=====================================================================
     P* BuildJsonAddress - Build JSON string for addresses (returns 1024a)
     P*          Parm 1 = address line 1 (256a varsize)
     P*          Parm 2 = address line 2 (256a varsize)
     P*          Parm 3 = city (100a varsize)
     P*          Parm 4 = state (2a)
     P*          Parm 5 = zip (9a varsize)
     P*          Parm 6 = tag name (256a varsize) (optional)
     P*=====================================================================
     DBuildJsonAddress...
     D                 pr          1024a   opdesc varying
     D inAddress1                   256a   const options(*varsize)
     D inAddress2                   256a   const options(*varsize)
     D inCity                       100a   const options(*varsize)
     D inState                        2a   const
     D inZip                          9a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)

     P*=====================================================================
     P* BuildJsonCode - Build JSON string for codes (returns 600a)
     P*          Parm 1 = code (25a varsize)
     P*          Parm 2 = description (100a varsize)
     P*          Parm 3 = tag name (256a varsize) (optional)
     P*          Parm 4 = other description (100a varsize) (optional)
     P*=====================================================================
     DBuildJsonCode...
     D                 pr           600a   opdesc varying
     D inCode                        25a   const options(*varsize)
     D inDesc                       100a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)
     D inOtherDesc                  100a   const options(*varsize:*nopass)

     P*=====================================================================
     P* BuildJsonName - Build JSON string for names (returns 1024a)
     P*          Parm 1 = first (256a varsize)
     P*          Parm 2 = last  (256a varsize)
     P*          Parm 3 = middle (256a varsize)
     P*          Parm 4 = tag name (256a varsize) (optional)
     P*          Parm 5 = suffix (10a varsize) (optional)
     P*          Parm 6 = prefix (10a varsize) (optional)
     P*=====================================================================
     DBuildJsonName...
     D                 pr          1024a   opdesc varying
     D inFirst                      256a   const options(*varsize)
     D inLast                       256a   const options(*varsize)
     D inMiddle                     256a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)
     D inSuffix                      10a   const options(*varsize:*nopass)
     D inPrefix                      10a   const options(*varsize:*nopass)

     P*=====================================================================
     P* BuildJsonNode - Build JSON key:value pair (returns 64256a)
     P*          Parm 1 = tag name (256a varsize)
     P*          Parm 2 = value (64000a varsize)
     P*          Parm 3 = type (25a varsize) (optional)
     P*=====================================================================
     DBuildJsonNode...
     D                 pr         64256a   opdesc varying
     D inTag                        256a   const options(*varsize)
     D inValue                    64000a   const options(*varsize)
     D inType                        25a   const options(*varsize:*nopass)
     D inExcept                       1n   options(*nopass)   const

     P*=====================================================================
     P* BuildJsonPhone - Build JSON string for phone #s (returns 350a)
     P*          Parm 1 = area code (3a varsize)
     P*          Parm 2 = phone # (10a varsize) (area code can be included here)
     P*          Parm 3 = type (25a varsize) (optional)
     P*          Parm 4 = tag name (256a varsize) (optional)
     P*=====================================================================
     DBuildJsonPhone...
     D                 pr           350a   opdesc varying
     D inAreaCode                     3a   const options(*varsize)
     D inPhone                       10a   const options(*varsize)
     D inType                        25a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)

      //==========================================================
      // Extract address from input json
      //==========================================================
     dExtractJsonAddress...
     d                 pr
     d nodeAddress                         like(yajl_val) const
     D outAddress1                  256a   options(*varsize)
     D outAddress2                  256a   options(*varsize)
     D outCity                      100a   options(*varsize)
     D outState                       2a
     D outZip                         9  0
     d outJson                      350    options(*nopass) varying

      //==========================================================
      // Extract code information from input json
      //==========================================================
     dExtractJsonCode...
     d                 pr
     d nodeCode                            like(yajl_val) const
     d outCode                       25    varying
     d outDesc                      100    varying
     d outOtherDesc                 100    options(*nopass) varying
     d outJson                      600    options(*nopass) varying

      //==========================================================
      // Extract dates from input json
      //==========================================================
     dExtractJsonDates...
     d                 pr
     d nodeDts                             like(yajl_val) const
     d outEffDt                       8  0
     d outExpDt                       8  0
     d outJson                      350    options(*nopass) varying
     d inDateFormat                  10    options(*nopass) const

      //==========================================================
      // Extract name from input json
      //==========================================================
     dExtractJsonName...
     d                 pr
     d nodeName                            like(yajl_val) const
     d outFirst                     256    varying
     d outLast                      256    varying
     d outMiddle                    256    varying
     d outJson                     1024    options(*nopass) varying
     d outSuffix                     10    options(*nopass) varying
     d outPrefix                     10    options(*nopass) varying

      //==========================================================
      // Extract phone # from input json
      //==========================================================
     dExtractJsonPhone...
     d                 pr
     d nodePhone                           like(yajl_val) const
     d outAreaCode                    3  0
     d outPhone                       7  0
     d outType                       25    options(*nopass) varying
     d outJson                      350    options(*nopass) varying

      *=========================================================
      * Process/report error messages
      *  - This procedure will return a JSON string with the error messges
      *     previously added with AddErrorMessageJson
      *  - Include this procedure at the appropriate place in your JSON
      *  -- commaPos ==> *PRE, *POST, *NONE {defaults to *NONE}
      *=========================================================
     dProcessErrorMessagesJson...
     d                 pr              a   len(1000000) varying
     d inCommaPos                    10    options(*nopass) const
