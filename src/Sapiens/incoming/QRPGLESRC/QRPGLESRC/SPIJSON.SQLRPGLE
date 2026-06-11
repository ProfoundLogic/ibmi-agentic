     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H nomain
    H*********************************************************************
     H*  Created:
    H*           08/09/18  C:33  Project: SY390
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*  12/09/21  C:33  Support: 99529                                   *
    H*                  Increase throughput                              *
    H*********************************************************************

     D/copy sPrAtrium
     D/copy sPrJSON
     D/copy sPrDate
     D/copy sPrString
      /Include yajl_h

     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0

     D***  Begin Add  ***  12/09/21 ****************
     DQp0zLprintf      pr            10I 0 extProc('Qp0zLprintf')
     D szOutputStg                     *   value options(*string)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D***  End   Add  ***  12/09/21 ****************

     d maxMsg          c                   999
     d decMsg#         s             10i 0 inz
     d errMsgArray     s            500    dim(maxMsg) varying inz
     d strErrorMsg     s            500a   Varying

     D psds          esds                  extname(szq1p)

      *=========================================================
      * Add error messages for future processing
      *  - Use this routine to store up messages for later processing to JSON string
      *  - Include the ProcessErrorMessagesJson procedure at the appropriate place in your JSON
      *=========================================================
     pAddErrorMessageJson...
     p                 b                   export
     dAddErrorMessageJson...
     d                 pi
     d inMessage                    500    varying const
     d outRtnCode                    10    options(*nopass)

     dstrMsg           s            500    varying
      /Free
       If %parms >= 2;
          outRtnCode = 'Error';
       Endif;
       decMsg# += 1;
       If decMsg# > maxMsg;
          Return;
       Endif;
       strMsg = inMessage;
       If %subst(inMessage:1:1) <> '"';
          strMsg = '"' + strMsg + '"';
       Endif;
       errMsgArray(decMsg#) = strMsg;
      /End-free
     pAddErrorMessageJson...
     p                 e

     P*=====================================================================
     P* BuildJsonAddress - Build JSON string for addresses (returns 1024a)
     P*          Parm 1 = address line 1 (256a varsize)
     P*          Parm 2 = address line 2 (256a varsize)
     P*          Parm 3 = city (100a varsize)
     P*          Parm 4 = state (2a)
     P*          Parm 5 = zip (9a varsize)
     P*          Parm 6 = tag name (256a varsize) (optional)
     P*=====================================================================
     PBuildJsonAddress...
     P                 b                   Export
     DBuildJsonAddress...
     D                 pi          1024a   opdesc varying
     D inAddress1                   256a   const options(*varsize)
     D inAddress2                   256a   const options(*varsize)
     D inCity                       100a   const options(*varsize)
     D inState                        2a   const
     D inZip                          9a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)
     D*
     D strAddress1     s            256    inz varying
     D strAddress2     s            256    inz varying
     D strAddressFull  s           1000    inz varying
     D strCity         s            100    inz varying
     D strJSON         s                   like(BuildJsonAddress) inz
     D strState        s              2    inz
     D strTag          s            256    inz
     D strZip          s              9    inz
      /Free
       CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0 and %subst(inAddress1:1:inLen) <> *blanks;
          strAddress1 = %subst(inAddress1:1:inLen);
          strAddressFull += %trim(strAddress1) + ', ';
       Endif;
       CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0 and %subst(inAddress2:1:inLen) <> *blanks;
          strAddress2 = %subst(inAddress2:1:inLen);
          strAddressFull += %trim(strAddress2) + ', ';
       Endif;
       CEEDOD(3:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0 and %subst(inCity:1:inLen) <> *blanks;
          strCity     = %subst(inCity:1:inLen);
          strAddressFull += %trim(strCity) + ', ';
       Endif;
       CEEDOD(4:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0 and %subst(inState:1:inLen) <> *blanks;
          strState    = %subst(inState:1:inLen);
          strAddressFull += %trim(strState) + ' ';
       Endif;
       CEEDOD(5:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0 and %subst(inZip:1:inLen) <> *blanks
        and %subst(inZip:1:inLen) <> '0';
          strZip      = %subst(inZip:1:inLen);
          strAddressFull += %subst(strZip:1:5);
          If %subst(strZip:6:4) <> '0000';
             strAddressFull += '-' + %subst(strZip:6:4);
          Endif;
       Endif;
       If %parms >= 6;
          CEEDOD(6:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strTag   = %subst(inTag:1:inLen);
          Endif;
       Endif;
       If strAddressFull <> *blanks;
          strAddressFull =
           %scanrpl(', ':'':strAddressFull:%len(strAddressFull)-2);
       Endif;

       strJSON = '{' +
        %trim(BuildJsonNode('address1':%trim(strAddress1))) + ',' +
        %trim(BuildJsonNode('address2':%trim(strAddress2))) + ',' +
        %trim(BuildJsonNode('city':%trim(strCity)))         + ',' +
        %trim(BuildJsonNode('state':%trim(strState)))       + ',' +
        %trim(BuildJsonNode('zip':%trim(strZip)))           + ',' +
        BuildJsonNode('full':strAddressFull) +
        '}';
       If %parms >= 6 and inTag <> *blanks;
          strJSON = BuildJsonNode(%trim(strTag):%trim(strJSON):'address':*Off);
       Endif;
       Return strJSON;
      /End-free
     PBuildJsonAddress...
     P                 e

     P*=====================================================================
     P* BuildJsonCode - Build JSON string for codes (returns 600a)
     P*          Parm 1 = code (25a varsize)
     P*          Parm 2 = description (100a varsize)
     P*          Parm 3 = tag name (256a varsize) (optional)
     P*          Parm 4 = other description (100a varsize) (optional)
     P*=====================================================================
     PBuildJsonCode...
     P                 b                   Export
     DBuildJsonCode...
     D                 pi           600a   opdesc varying
     D inCode                        25a   const options(*varsize)
     D inDesc                       100a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)
     D inOtherDesc                  100a   const options(*varsize:*nopass)
     D*
     D strCode         s             25    inz
     D strDesc         s            100    inz
     D strOtherDesc    s            100    inz
     D strJSON         s            600    inz varying
     D strTag          s            256    inz varying
      /Free
       CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strCode = %subst(inCode:1:inLen);
       Endif;
       CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strDesc = %subst(inDesc:1:inLen);
       Endif;
       If %parms >= 3;
          CEEDOD(3:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strTag = %subst(inTag:1:inLen);
          Endif;
       Endif;
       If %parms >= 4;
          CEEDOD(4:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strOtherDesc = %subst(inOtherDesc:1:inLen);
          Endif;
       Endif;

       strJSON += '{' +
        BuildJsonNode('code':%trim(strCode)) + ',' +
        BuildJsonNode('description':%trim(strDesc)) + ',' +
        BuildJsonNode('display':%trim(strCode)+' - '+%trim(strDesc)) + ',';
       If %parms >= 4;
          strJSON += BuildJsonNode('type':%trim(strOtherDesc)) + ',';
       Endif;
       strJSON = %scanrpl(',':'}':strJSON:%len(strJSON)-1);
       If strTag <> *blanks;
          strJSON = '"' + strTag + '":' + strJSON;
       Endif;
       Return strJSON;
      /End-free
     PBuildJsonCode...
     P                 e

     P*=====================================================================
     P* BuildJsonName - Build JSON string for names (returns 1024a)
     P*          Parm 1 = first (256a varsize)
     P*          Parm 2 = last  (256a varsize)
     P*          Parm 3 = middle (256a varsize)
     P*          Parm 4 = tag name (256a varsize) (optional)
     P*          Parm 5 = suffix (10a varsize) (optional)
     P*          Parm 6 = prefix (10a varsize) (optional)
     P*=====================================================================
     PBuildJsonName...
     P                 b                   Export
     DBuildJsonName...
     D                 pi          1024a   opdesc varying
     D inFirst                      256a   const options(*varsize)
     D inLast                       256a   const options(*varsize)
     D inMiddle                     256a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)
     D inSuffix                      10a   const options(*varsize:*nopass)
     D inPrefix                      10a   const options(*varsize:*nopass)
     D*
     D strFirst        s            256    inz varying
     D strJSON         s                   like(BuildJsonAddress) inz
     D strLast         s            256    inz varying
     D strMiddle       s            256    inz varying
     D strPrefix       s             10    inz varying
     D strSuffix       s             10    inz varying
     D strTag          s            256    inz varying
      /Free
       CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strFirst = %trim(%subst(inFirst:1:inLen));
       Endif;
       CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strLast = %trim(%subst(inLast:1:inLen));
       Endif;
       CEEDOD(3:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strMiddle = %trim(%subst(inMiddle:1:inLen));
       Endif;
       If %parms >= 4;
          CEEDOD(4:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strTag = %subst(inTag:1:inLen);
          Endif;
       Endif;
       If %parms >= 5;
          CEEDOD(5:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strSuffix = %trim(%subst(inSuffix:1:inLen));
          Endif;
       Endif;
       If %parms >= 6;
          CEEDOD(6:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strPrefix = %trim(%subst(inPrefix:1:inLen));
          Endif;
       Endif;
       If strMiddle <> *blanks;
          strMiddle += ' ';
       Endif;

       strJSON = '{' +
        BuildJsonNode('first':strFirst) + ',' +
        BuildJsonNode('last':strLast) + ',' +
        BuildJsonNode('middle':strMiddle) + ',' +
        BuildJsonNode('full':strPrefix + ' ' + strFirst + ' ' + strMiddle +
         strLast + ' ' + strSuffix) + ',';
       If strPrefix <> *blanks;
          strJson += BuildJsonNode('prefix':strPrefix) + ',';
       Endif;
       If strSuffix <> *blanks;
          strJson += BuildJsonNode('suffix':strSuffix) + ',';
       Endif;
       strJson = %scanrpl(',':'':strJson:%len(strJson)-1);
       strJson += '}';
       If %parms >= 4 and inTag <> *blanks;
          strJSON = BuildJsonNode(strTag:strJSON:'name':*Off);
       Endif;
       Return strJSON;
      /End-free
     PBuildJsonName...
     P                 e

     P*=====================================================================
     P* BuildJsonNode - Build JSON key:value pair (returns 64256a)
     P*          Parm 1 = tag name (256a varsize)
     P*          Parm 2 = value (64000a varsize)
     P*          Parm 3 = type (25a varsize) (optional)
     P*=====================================================================
     PBuildJsonNode...
     P                 b                   Export
     DBuildJsonNode...
     D                 pi         64256a   opdesc varying
     D inTag                        256a   const options(*varsize)
     D inValue                    64000a   const options(*varsize)
     D inType                        25a   const options(*varsize:*nopass)
     D inExcept                       1n   options(*nopass)   const
     D*
     D isExcept        s              1n   inz('1')
     D***  Begin Chg  ***  12/09/21 ****************
     D*strValue*       s          64000    inz
     D strValue        s          64000    inz varying
     D***  End   Chg  ***  12/09/21 ****************
     D strNode         s                   like(BuildJsonNode) inz
     D strType         s             25    inz('text')
     D***  Begin Chg  ***  12/09/21 ****************
     D*strTag***       s            256    inz
     D strTag          s            256    inz varying
     D***  End   Chg  ***  12/09/21 ****************
      /Free
       If %parms >= 4;
          isExcept = inExcept;
       Endif;
       CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strTag = %subst(inTag:1:inLen);
       Endif;
       CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strValue   = %subst(inValue:1:inLen);
       Endif;
       If %parms >= 3;
          CEEDOD(3:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strType = %subst(inType:1:inLen);
          Endif;
       Endif;

       Monitor;
       Select;
       When isExcept;
        //***  Begin Chg  ***  12/09/21 ****************
        //strValue = %trim(RmvExceptJSON(%TrimR(strValue)));
          strValue = RmvExceptJSON(strValue);
        //***  End   Chg  ***  12/09/21 ****************
       Endsl;
       On-Error;
       Endmon;

       Select;
       When strType = 'text'; // Add Double Quotes
        //***  Begin Chg  ***  12/09/21 ****************
        //strValue = '"' + %Trim(strValue) + '"';
          strValue = '"' + strValue + '"';
        //***  End   Chg  ***  12/09/21 ****************
       When strType = 'time'; // Add Double Quotes
        //***  Begin Chg  ***  12/09/21 ****************
        //strValue = '"' + %Trim(strValue) + '"';
          strValue = '"' + strValue + '"';
        //***  End   Chg  ***  12/09/21 ****************
       When strType = 'date'; // Convert and Add Double Quotes
          Monitor;
             If strValue <> *blanks and %trim(strValue) <> *zeros;
                strValue = %char(CvtToDate(%dec(strValue:8:0)):*usa0);
             Endif;
          On-error;
           //***  Begin Chg  ***  12/09/21 ****************
           //strValue = %trim(strValue) + ' - invalid date';
             strValue += ' - invalid date';
           //***  End   Chg  ***  12/09/21 ****************
          Endmon;
        //***  Begin Chg  ***  12/09/21 ****************
        //strValue = '"' + %trim(strValue) + '"';
          strValue = '"' + strValue + '"';
        //***  End   Chg  ***  12/09/21 ****************
       Other;
       EndSl;

       //***  Begin Chg  ***  12/09/21 ****************
       //strNode = '"' + %Trim(strTag) + '" : ' + %Trim(strValue);
       strNode = '"' + strTag + '" : ' + strValue;
       //***  End   Chg  ***  12/09/21 ****************

       Return strNode;
      /End-free
     PBuildJsonNode...
     P                 e

     P*=====================================================================
     P* BuildJsonPhone - Build JSON string for phone #s (returns 350a)
     P*          Parm 1 = area code (3a varsize)
     P*          Parm 2 = phone # (10a varsize) (area code can be included here)
     P*          Parm 3 = type (25a varsize) (optional)
     P*          Parm 4 = tag name (256a varsize) (optional)
     P*=====================================================================
     PBuildJsonPhone...
     P                 b                   Export
     DBuildJsonPhone...
     D                 pi           350a   opdesc varying
     D inAreaCode                     3a   const options(*varsize)
     D inPhone                       10a   const options(*varsize)
     D inType                        25a   const options(*varsize)
     D inTag                        256a   const options(*varsize:*nopass)
     D*
     D strPhone        s              7    inz
     D strAreaCode     s              3    inz
     D strJSON         s                   like(BuildJsonAddress) inz
     D strType         s             25    inz
     D strTag          s            256    inz
      /Free
       CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strAreaCode = %subst(inAreaCode:1:inLen);
       Endif;
       CEEDOD(2:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       Select;
       When inLen = 10 and (strAreaCode = '000' or strAreaCode = *blanks);
          strAreaCode = %subst(inPhone:1:3);
          strPhone    = %subst(inPhone:4:7);
       When inLen > 0;
          strPhone   = %subst(inPhone:1:inLen);
       Endsl;
       CEEDOD(3:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       If inLen > 0;
          strType    = %subst(inType:1:inLen);
       Endif;
       If %parms >= 4;
          CEEDOD(4:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          If inLen > 0;
             strTag     = %subst(inTag:1:inLen);
          Endif;
       Endif;

       strJSON = '{' +
        %trim(BuildJsonNode('areacode':%trim(strAreaCode))) + ',' +
        %trim(BuildJsonNode('phone':%trim(strPhone)))       + ',' +
        %trim(BuildJsonNode('type':%trim(strType)))         +
        '}';
       If %parms >= 4 and inTag <> *blanks;
          strJSON = BuildJsonNode(%trim(strTag):%trim(strJSON):'phone':*off);
       Endif;
       Return strJSON;
      /End-free
     PBuildJsonPhone...
     P                 e

      //==========================================================
      // Extract address from input json
      //==========================================================
     pExtractJsonAddress...
     p                 b                   export
     dExtractJsonAddress...
     d                 pi
     d nodeAddress                         like(yajl_val) const
     D outAddress1                  256a   options(*varsize)
     D outAddress2                  256a   options(*varsize)
     D outCity                      100a   options(*varsize)
     D outState                       2a
     D outZip                         9  0
     d outJson                      350    options(*nopass) varying

     di1               s             10i 0 Inz
     Dkey              s             50a   varying
     dstrJson          s          32767    inz
     DstrZip           s              9a   inz
     dval              s                   Like(yajl_val)
      /Free
       Clear outAddress1;
       Clear outAddress2;
       Clear outCity;
       Clear outState;
       Clear outZip;
       Dow YAJL_Object_loop(nodeAddress:i1:key:val);
          BuildJson(strJson:key:YAJL_get_string(val));
          Select;
          When key = 'address1';
             outAddress1 = YAJL_get_string(val);
          When key = 'address2';
             outAddress2 = YAJL_get_string(val);
          When key = 'city';
             outCity = YAJL_get_string(val);
          When key = 'state';
             outState = YAJL_get_string(val);
          When key = 'zip';
             strZip = YAJL_get_string(val);
          Endsl;
       Enddo;
       Monitor;
          outZip = %int(strZip);
          If outZip < 100000;
             outZip = outZip * 10000;
          Endif;
       On-error;
       Endmon;
       If %parms >= 6;
          outJson = %trim(strJson);
       Endif;
      /End-free
     pExtractJsonAddress...
     p                 e

      //==========================================================
      // Extract code information from input json
      //==========================================================
     pExtractJsonCode...
     p                 b                   export
     dExtractJsonCode...
     d                 pi
     d nodeCode                            like(yajl_val) const
     d outCode                       25    varying
     d outDesc                      100    varying
     d outOtherDesc                 100    options(*nopass) varying
     d outJson                      600    options(*nopass) varying

     di1               s             10i 0 Inz
     Dkey              s             50a   varying
     dstrJson          s          32767    inz
     dstrOtherDesc     s            100a   inz varying
     dval              s                   Like(yajl_val)
      /Free
       Clear outCode;
       Clear outDesc;
       Clear outOtherDesc;
       Dow YAJL_Object_loop(nodeCode:i1:key:val);
          BuildJson(strJson:key:YAJL_get_string(val));
          Select;
          When key = 'code';
             outCode = YAJL_get_string(val);
          When key = 'description';
             outDesc = YAJL_get_string(val);
          When key = 'otherDescription';
             strOtherDesc = YAJL_get_string(val);
          Endsl;
       Enddo;
       If %parms >= 3;
          outOtherDesc = %trim(strOtherDesc);
       Endif;
       If %parms >= 4;
          outJson = %trim(strJson);
       Endif;
      /End-free
     pExtractJsonCode...
     p                 e

      //==========================================================
      // Extract dates from input json
      //==========================================================
     pExtractJsonDates...
     p                 b                   export
     dExtractJsonDates...
     d                 pi
     d nodeDts                             like(yajl_val) const
     d outEffDt                       8  0
     d outExpDt                       8  0
     d outJson                      350    options(*nopass) varying
     d inDateFormat                  10    options(*nopass) const

     di1               s             10i 0 Inz
     Dkey              s             50a   varying
     dstrDateFormat    s             10    inz('MMDDYYYY')
     dstrEffDt         s              8    inz('00000000')
     dstrExpDt         s              8    inz('00000000')
     dstrJson          s          32767    inz
     dval              s                   Like(yajl_val)
      /Free
       If %parms >= 4 and inDateFormat <> *blank;
          strDateFormat = inDateFormat;
       Endif;
       Dow YAJL_Object_loop(nodeDts:i1:key:val);
          BuildJson(strJson:key:YAJL_get_string(val));
          Select;
          When key = 'effective';
             strEffDt = YAJL_get_string(val);
          When key = 'expiration';
             strExpDt = YAJL_get_string(val);
          When key = 'cancel';
             strExpDt = YAJL_get_string(val);
          Endsl;
       Enddo;
       Clear outEffDt;
       Clear outExpDt;
       Monitor;
          outEffDt = %int(strEffDt);
       On-error;
       Endmon;
       Monitor;
          outExpDt = %int(strExpDt);
       On-error;
       Endmon;
       Select;
       When strDateFormat = 'CYYMMDD';
          outEffDt = DtCYMD(outEffDt);
          outExpDt = DtCYMD(outExpDt);
       Endsl;
       If %parms >= 3;
          outJson = %trim(strJson);
       Endif;
      /End-free
     pExtractJsonDates...
     p                 e

      //==========================================================
      // Extract name from input json
      //==========================================================
     pExtractJsonName...
     p                 b                   export
     dExtractJsonName...
     d                 pi
     d nodeName                            like(yajl_val) const
     d outFirst                     256    varying
     d outLast                      256    varying
     d outMiddle                    256    varying
     d outJson                     1024    options(*nopass) varying
     d outSuffix                     10    options(*nopass) varying
     d outPrefix                     10    options(*nopass) varying

     di1               s             10i 0 Inz
     Dkey              s             50a   varying
     dstrJson          s          32767    inz
     dstrPrefix        s             10a   inz varying
     dstrSuffix        s             10a   inz varying
     dval              s                   Like(yajl_val)
      /Free
       Dow YAJL_Object_loop(nodeName:i1:key:val);
          BuildJson(strJson:key:YAJL_get_string(val));
          Select;
          When key = 'first';
             outFirst = YAJL_get_string(val);
          When key = 'last';
             outLast = YAJL_get_string(val);
          When key = 'middle';
             outMiddle = YAJL_get_string(val);
          When key = 'prefix';
             strPrefix = YAJL_get_string(val);
          When key = 'suffix';
             strSuffix = YAJL_get_string(val);
          Endsl;
       Enddo;
       If %parms >= 4;
          outJson = strJson;
       Endif;
       If %parms >= 5;
          outSuffix = strSuffix;
       Endif;
       If %parms >= 6;
          outPrefix = strPrefix;
       Endif;
      /End-free
     pExtractJsonName...
     p                 e

      //==========================================================
      // Extract phone from input json
      //==========================================================
     pExtractJsonPhone...
     p                 b                   export
     dExtractJsonPhone...
     d                 pi
     d nodePhone                           like(yajl_val) const
     d outAreaCode                    3  0
     d outPhone                       7  0
     d outType                       25    options(*nopass) varying
     d outJson                      350    options(*nopass) varying

     di1               s             10i 0 Inz
     Dkey              s             50a   varying
     dstrAreaCode      s              3a   inz
     dstrJson          s          32767    inz
     dstrPhone         s              7a   inz
     dval              s                   Like(yajl_val)
      /Free
       Clear outAreaCode;
       Clear outPhone;
       Clear outType;
       Dow YAJL_Object_loop(nodePhone:i1:key:val);
          BuildJson(strJson:key:YAJL_get_string(val));
          Select;
          When key = 'areacode';
             strAreaCode = YAJL_get_string(val);
          When key = 'phone';
             strPhone = YAJL_get_string(val);
          When key = 'type';
             outType = YAJL_get_string(val);
          Endsl;
       Enddo;
       Monitor;
          outAreaCode = %int(strAreaCode);
       On-error;
       Endmon;
       Monitor;
          outPhone = %int(strPhone);
       On-error;
       Endmon;
       If %parms >= 4;
          outJson = %trim(strJson);
       Endif;
      /End-free
     pExtractJsonPhone...
     p                 e

      *=========================================================
      * Process/report error messages
      *  - This procedure will return a JSON string with the error messges
      *     previously added with AddErrorMessageJson
      *  - Include this procedure at the appropriate place in your JSON
      *  -- commaPos ==> *PRE, *POST, *NONE {defaults to *NONE}
      *=========================================================
     pProcessErrorMessagesJson...
     p                 b                   export
     dProcessErrorMessagesJson...
     d                 pi              a   len(1000000) varying
     d inCommaPos                    10    options(*nopass) const

     Di1               s             10i 0
     DoutJson          s               a   len(1000000) varying inz
     DstrCommaPre      s              2    varying inz
     DstrCommaPost     s              2    varying inz
      /Free
       If decMsg# <= 0;
          Return *blanks;
       Endif;
       If %parms >= 1;
          Select;
          When inCommaPos = '*PRE';
             strCommaPre = ', ';
          When inCommaPos = '*POST';
             strCommaPost = ',';
          Endsl;
       Endif;
       outJson += '"errorMsg": [';
       For i1 = 1 to decMsg#;
          outJson += errMsgArray(i1) + ',';
       Endfor;
       outJson = strCommaPre + %scanrpl(',':']':outJson:%len(outJson)-1) +
        strCommaPost;
       Reset errMsgArray;
       Reset decMsg#;
       Return outJson;
      /End-free
     pProcessErrorMessagesJson...
     p                 e
