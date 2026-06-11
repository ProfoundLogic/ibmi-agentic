     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     h BndDir('WTBNDDIR':'YAJL') DecEdit('0.')
     H*====================================================================
     H* Description:
     H*
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 04/21/23 c:62 Ticket: 109080(099)
     H*               Only select inhouse groups
     H*
     H* 02/22/23 c:62 Project CN023
     H*               Add new filters
     H*
     H*====================================================================

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz
     D key             s             50a   varying
     d list            s                   Like(yajl_val)
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d strErrorMsg     s            500a   Varying
     d val             s                   Like(yajl_val)

     d clientID        s              3a
     d intCo#          s              3  0
     d*** Begin Add *** 02/22/23 *******************************************
     d intFein         s              9  0
     d intFnd          s              3  0
     d intMod          s              3  0
     d isBadData       s               n   Inz(*Off)
     d agtCount        s              5  0 Inz
     d grpCount        s              5  0 Inz
     d*** End   Add *** 02/22/23 *******************************************
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     d strAuthority    s             10a
     d strCo#          s              3a
     d strEnv          s              4a
     d strSql          s           5000a    varying
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0

     d*** Begin Chg *** 02/22/23 *******************************************
     d********** dsIn             ds
     ddsIn             ds                  Qualified
     d*** End   Chg *** 02/22/23 *******************************************
     d company                        3
     d*** Begin Add *** 02/22/23 *******************************************
     d fein                           9
     d mod                            3
     d groupName                     60
     d group#                         3
     d*** End   Add *** 02/22/23 *******************************************

     D PSDS          ESDS                  extname(SZQ1P)
     d*** Begin Chg *** 02/22/23 *******************************************
     d**********dsWMFNP        e ds                  extname(WMFNP) inz
     ddsWMFNP        e ds                  extname(WMFNP) inz qualified
     d*** End   Chg *** 02/22/23 *******************************************
     d*** Begin Add *** 04/21/23 *******************************************
     ddsWMFIP        e ds                  extname(WMFIP) inz qualified

     d FNco#           s                   Like(dsWMFNP.FNco#)
     d FNfnd           s                   Like(dsWMFNP.FNfnd)
     d*** End   Add *** 04/21/23 *******************************************

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJson
      /copy sPrSecure
      /copy sPrSql
      /copy wPrUsrPrms
      /include yajl_h
      //*** Begin Add *** 02/22/23 *****************************************
      /copy wPrGroup
      //*** End   Add *** 02/22/23 *****************************************

      *entry
     DRequestPgm       pr                  ExtPgm('SISHELL')
     D                              256
     D                               10
     D                                 a   len(8500000) varying
     D                                 a   len(8500000) varying
     D                               10
     DRequestPgm       pi
     D inRequest                    256
     D inUser                        10
     D inParms                         a   len(8500000) varying
     D outJSON                         a   len(8500000) varying
     D ioRtnCode                     10

      *==========================================================
      * Main
      *==========================================================
      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;
       Select;
       When inRequest = 'ListCarriers';
          ListGroups('carrier');
       When inRequest = 'ListGroups';
          ListGroups();
       Other;
          Exsr BuildStub;
       Endsl;
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';
       Endsr;
      /End-free

      *=========================================================
      * Request: ListGroups
      * Description: Returns an array of group information
      *=========================================================
     pListGroups       b
     dListGroups       pi
     d inHeading                     25    varying const options(*nopass)

     disEofWMFNP       s              1n   inz
     dstrHeading       s             25    varying inz('group')
      /Free
       If %parms >= 1 and inHeading <> *blanks;
          strHeading = inHeading;
       Endif;
       Exsr GetParms;
       //*** Begin Chg *** 02/22/23 ****************************************
       //********** Exsr BuildOutput;
       If ioRtnCode <> 'Error';
          Exsr BuildOutput;
       EndIf;
       //*** End   Chg *** 02/22/23 ****************************************
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       Return;

       //=================================================================
       // Build output
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSql;
        json += ', "' + strHeading + 'List":[';
        Dou isEofWMFNP;
           Exec SQL fetch next from cursorWMFNP into :dsWMFNP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofWMFNP = *on;
              Iter;
           //*** Begin Add *** 04/21/23 ************************************
           Other;
              // See if this is a Connect group
              FNco# = dsWMFNP.FNco#;
              FNfnd = dsWMFNP.FNfnd;
              Exec sql
               select * into :dsWMFIP
                from WMFIP
                where FICO# = :FNco# and FIFND = :FNfnd;
              If IsValidFetch(sqlstt);
                 If dsWMFIP.FIquote <> 'Y' And dsWMFIP.FIpolicy <> 'Y' And
                    dsWMFIP.FIclaim <> 'Y' And dsWMFIP.FIsr <> 'Y' And
                    dsWMFIP.FIfroi  <> 'Y';
                    Iter;
                 EndIf;
              Else;
                 Iter;
              EndIf;
           //*** End   Add *** 04/21/23 ************************************
           Endsl;
           Exsr ProcessRecord;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']';
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListGroups}');
        BuildGiantJSON(json:'heading':strHeading);
       Endsr;

       //=================================================================
       // Build sql required to retrieve data
       //=================================================================
       Begsr BuildSql;
        //*** Begin Chg *** 02/22/23 *************************************
        //***strSql = 'select * from WMFNP where FNco# = ' + strCo# +
        //*** ' order by FNco#, FNfnd';
        If intFein = 0;
           strSql  = 'select * from WMFNP where FNco# = ' + strCo#;
        Else;
           strSql  = 'select a.* from WMFNP a join WMAAP on FNco# = AAco#' +
                     ' And FNfnd = AAfnd where FNco# = ' + strCo#;
        EndIf;

        If intFnd <> 0;
           strSql += ' And FNfnd = ' + dsIn.group#;
        EndIf;

        If dsIn.groupName <> ' ';
           strSql += ' And Upper(FNname) like Upper(''%' +
                     %Trim(dsIn.groupName) +'%'')';
        EndIf;

        If intFein <> 0;
           strSql += ' And AAfein = ' + dsIn.fein + ' And  AAmod = ' +
                                        dsIn.mod;
        EndIf;

        strSql += ' order by FNco#, FNfnd';
        //*** End   Chg *** 02/22/23 *************************************
        Exec SQL close cursorWMFNP;
        Exec SQL prepare sqlWMFNP from :strSql;
        Exec SQL declare cursorWMFNP cursor for sqlWMFNP;
        Exec SQL open cursorWMFNP;
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        // Load the json tree
        node0 = yajl_string_load_tree(inParms : strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        json += ' "requestParms":{';
        // Load the data structure with the parameter values from the tree
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.company = YAJL_get_string(val);
           //*** Begin Add *** 02/22/23 ************************************
           When key = 'agencyInfo';
              yajl_tree_free(node1);
              Reset i1;
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'fein';
                    dsIn.fein = YAJL_get_string(val);
                 When key = 'mod';
                    dsIn.mod = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '} ';
           When key = 'group';
              yajl_tree_free(node1);
              Reset i1;
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'name';
                    dsIn.groupName = YAJL_get_string(val);
                 When key = 'number';
                    dsIn.group#    = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '} ';
           //*** End   Add *** 02/22/23 ************************************
           Endsl;
        Enddo;
        json += '}';
        // Free up the space by removing the tree
        yajl_tree_free(node0);

        //*** Begin Add *** 02/22/23 ************************************
        // Let's Validate the incoming filters
        ExSr ValidateParms;
        //*** End   Add *** 02/22/23 ************************************

       Endsr;

       //=================================================================
       // Process records into json
       //=================================================================
       Begsr ProcessRecord;
        json += '{';
        json += BuildJsonNode('company':strCo#) + ',';
        json += '"' + strHeading + '":{';
         json += BuildJsonNode('code':%editc(dsWMFNP.FNfnd:'X')) + ',';
         json += BuildJsonNode('name':dsWMFNP.FNname) + ',';
         json += BuildJsonNode('shortName':dsWMFNP.FNshNm) + ',';
         json +=
          BuildJsonNode('displayCode':strCo# + %editc(dsWMFNP.FNfnd:'X')) + ',';
         json += '"address":{' +
          %trim(BuildJsonAddress(dsWMFNP.FNmAd1:dsWMFNP.FNmAd2:
           dsWMFNP.FNmCty:dsWMFNP.FNmSt:%char(dsWMFNP.FNmZip):'mailing')) +
           ', ' +
          %trim(BuildJsonAddress(dsWMFNP.FNpAd1:dsWMFNP.FNpAd2:
           dsWMFNP.FNpCty:dsWMFNP.FNpSt:%char(dsWMFNP.FNpZip):'physical')) +
         '},';
         json += '"phone":[' +
          '{'+%trim(BuildJsonPhone(%char(dsWMFNP.FNph1A):%char(dsWMFNP.FNph1):
           '':'phone1')) + '}, ' +
          '{'+%trim(BuildJsonPhone(%char(dsWMFNP.FNarea):%char(dsWMFNP.FNph2):
           '':'phone2')) + '}, ' +
          '{'+%trim(BuildJsonPhone(%char(dsWMFNP.FNfaxA):%char(dsWMFNP.FNfax):
           'fax':'fax')) + '} ' +
         '], ';
         json += '"dates":{' +
          %trim(BuildJsonNode('effective':%char(dsWMFNP.FNefDt):'date'))+', '+
          %trim(BuildJsonNode('expiration':%char(dsWMFNP.FNexDt):'date'))+'},';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Validation
       //=================================================================
       Begsr ValidateParms;

        Monitor;
           intCo# = %int(dsIn.company);
           strCo# = dsIn.company;
        On-error;
           //*** Begin Add *** 02/22/23 ************************************
           intCo# = 0;
           //*** End   Add *** 02/22/23 ************************************
        Endmon;
        //*** Begin Add *** 02/22/23 ************************************
        If intCo# = 0;
           intCo# = UserCo#(inUser);
           strCo# = %Editc(intCo#:'X');
        EndIf;
        //*** End   Add *** 02/22/23 ************************************

        //*** Begin Add *** 02/22/23 ***************************************
        // Group Number Edits
        //======================================================

        // Group # must be a valid numeric
        isBadData = *Off;
        If dsIn.group# <> ' ';
           Monitor;
              intFnd = %Dec(dsIn.group#:3:0);
           On-Error;
              isBadData = *On;
              AddErrorMessageJson('Group must be numeric.':ioRtnCode);
           EndMon;
        Else;
           intFnd = 0;
        EndIf;

        // Group must exist
        If isBadData = *Off And intFnd <> 0;
           Exec  Sql
             Select count(*)
             Into :grpCount
             From wmfnp
             Where FNco# = :intCo# And FNfnd = :intFnd;

           If grpCount = 0;
              AddErrorMessageJson('Group does not exist.':ioRtnCode);
           Else;
              // Is this a Portal Group
              If not UseGroupInPortal(intCo#:intFnd:'Policy');
                 AddErrorMessageJson('Group ' + dsIn.group# +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              EndIf;
           EndIf;
        EndIf;

        // Agency Fein/Mod Edits
        //======================================================

        // Agency Fein must be a valid numeric
        If dsIn.fein <> ' ';
           isBadData = *Off;
           Monitor;
              intFein = %Dec(dsIn.fein:9:0);
           On-Error;
              isBadData = *On;
              AddErrorMessageJson('Agency Fein must be numeric.':
                ioRtnCode);
           EndMon;

           // Agency Mod must be a valid numeric
           If dsIn.mod <> ' ';
              Monitor;
                 intMod = %Dec(dsIn.mod:3:0);
              On-Error;
                 isBadData = *On;
                 AddErrorMessageJson('Agency Modifier must be numeric.':
                  ioRtnCode);
              EndMon;
           Else;
              intMod  = 0;
           EndIf;
        Else;
           intFein = 0;
           intMod  = 0;
        EndIf;

        // Agency must exist
        If isBadData = *Off and intFein <> 0;
           Exec  Sql
             Select count(*)
             Into :agtCount
             From wmagp
             Where AGfein = :intFein And AGmod = :intMod;

           If agtCount = 0;
              AddErrorMessageJson('Agency does not exist.':ioRtnCode);
           EndIf;
        EndIf;
        //*** End   Add *** 02/22/23 ***************************************
       Endsr;
      /End-free
     pListGroups       e
