     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Description:  Returns list of producers.
     H*
     H* Project: SY406   Date: 08/03/18
     H* Pgmr:    C:65
     H*
     H*====================================================================
     H* inParms -
     H*   name:value
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 01/25/24 c:79 Support: 140/114870
     H*               Don't clear keyMod for missign agent number
     H*
     H* 10/27/22 c:62 Ticket 104042
     H*               Don't include inactive producers
     H*
     H*====================================================================
     Fwmapl     if   e           k disk    rename(pap01:pap00) infds(apfds)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fwmagl     if   e           k disk    rename(pag01:pag00)

     D* this size needs to change
     Darray            s               a   len(32767)
     DinJson           s               a   len(2000000) varying
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying
     dcomma            s              1a
     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')

     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')

     D docNode         s                   like(yajl_val)
     D i1              s             10i 0 inz
     D i2              s             10i 0 inz
     D key             s             50a   varying
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D node2           s                   like(yajl_val)
     D val             s                   like(yajl_val)

     d sysDate         s               d   datfmt(*iso)
     d sysDate7        s              7  0
     d sysTime6        s              6  0

     dclientID         s              3a
     dintCo#           s              3  0
     disProcess        s              1n   inz
     dstrCo#           s              3a

     DagencyInfo       ds                  qualified inz
     D number                         5    varying
     D name                         100a   varying

     D result          ds                  Qualified inz
     D  rtnCode                      10a
     D  fein                          9a
     D  mod                           3a
     D  agency                             likeds(agencyInfo)
     D  lastName                     30a
     D  firstName                    15a
     D  status                        1a
     D  co#                           3a
     D  prod#                         5a
     D  includeRequest...
     D                                1n   inz('1')
     D  includeCancelled...
     D                                1a   inz('N')

     ddecAgt#          s              5  0 inz
     ddecCount         s             15  0 inz
     dkeyAgentNumber   s              5  0 inz
     dkeyCo#           s              3s 0
     dkeyIncludeCancelled...
     d                 s              1a
     dkeyFein          s              9s 0
     dkeyFirstName     s             15a
     dkeyLastName      s             30a
     dkeyMod           s              3s 0
     dkeyProd#         s              5s 0
     dkeyStatus        s              1a
     dnoKeyMod         s              1a
     d strAuthority    s             10a

     D PSDS          ESDS                  extname(SZQ1P)
     D apfds           ds
     D aprrn                 397    400b 0

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrString
      /copy wPrAgent
      /Copy wPrUsrPrms
      /include yajl_h

     dGetClientID      pr                  ExtPgm('RTVCLTID')
     d clientID                       3a

      *entry
     DRequestPgm       pr                  ExtPgm('WIPROD')
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
     D ioRtCd                        10

      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;

       Chain (inUser) pup00;

       Clear json;
       Exsr GetParms;
       If errMsg = ' ';
          Select;
          When inRequest = 'GetProducer';
             Exsr BuildOutputGet;
          When inRequest = 'ListProducers';
             Exsr BuildOutputList;
          Endsl;
       Else;
          json = %trim(json) + ' "errorMsg": [ ' + %trim(errMsg) + ' ] ';
          ioRtCd = 'error';
       Endif;
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutputGet;
        json += '"producerInfo":';
        Chain (keyCo#:keyFein:keyMod:keyProd#) pap00;
        If not %found;
           APco# = keyCo#;
           APprod = keyProd#;
           APfein = keyFein;
           APmod = keyMod;
        Endif;
        Exsr BuildRecord;
       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutputList;

        Clear comma;
        json += ' "producerList" : [';

        Select;
        When keyFein <> 0 and noKeyMod = ' ';
           Setll (keyCo#:keyFein:keyMod) pap00;
           Reade (keyCo#:keyFein:keyMod) pap00;
        When keyFein <> 0;
           Setll (keyCo#:keyFein) pap00;
           Reade (keyCo#:keyFein) pap00;
        Other;
           Setll (keyCo#) pap00;
           Reade (keyCo#) pap00;
        Endsl;

        Dow not %eof;

           Clear AGname;
           Chain (APco#:APfein:APmod) pag00;
           Reset decAgt#;
           Exec sql
            select AAagt# into :decAgt#
             from WMAAP
             where AAco# = :APco# and AAfein = :APfein and AAmod = :APmod
              and AAagt# = :keyAgentNumber
             limit 1;

           //* Check Filters not in key list.
           isProcess = *on;
           Select;
           When keyStatus <> ' ' and keyStatus <> APStat;
              isProcess = *off;
           When keyProd# <> 0 and keyProd# <> APProd;
              isProcess = *off;
           When keyLastName <> ' ' and not StringFound(APlsn:keyLastName:*on);
              isProcess = *off;
           When keyFirstName <> ' ' and not StringFound(APfsn:keyFirstName:*on);
              isProcess = *off;
           When keyIncludeCancelled = 'N'
            and APcxDt <> *zero and APcxDt <= sysDate7;
              isProcess = *off;
           When result.agency.name <> *blank and
            not StringFound(AGname:result.agency.name:*on);
              isProcess = *off;
           When result.agency.number <> *blank and decAgt# <> keyAgentNumber;
              isProcess = *off;
           Endsl;
         //If (keyStatus = ' ' or (keyStatus <> ' ' and keyStatus = APStat))
         // and (keyProd# = 0 or (keyProd# <> 0 and keyProd# = APProd))
         // and (keyLastName = ' ' or (%scan(%xlate(lo:up:%trim(keyLastName)):
         // %xlate(lo:up:APLSN)) <> 0))
         // and (keyFirstName = ' ' or (%scan(%xlate(lo:up:%trim(keyFirstName)):
         // %xlate(lo:up:APFSN)) <> 0));

           If isProcess;
              Exsr BuildRecord;
           Endif;

           Select;
           When keyFein <> 0 and noKeyMod = ' ';
              Reade (keyCo#:keyFein:keyMod) pap00;
           When keyFein <> 0;
              Reade (keyCo#:keyFein) pap00;
           Other;
              Reade (keyCo#) pap00;
           Endsl;
        Enddo;

        json = %trim(json) + ' ]';

       Endsr;

       //=================================================================
       // Build record results
       //=================================================================
       Begsr BuildRecord;

        Clear AGname;
        Chain (APco#:APfein:APmod) pag00;

        If comma <> ' ';
           json = %trim(json) + comma;
        Else;
           comma = ',';
        Endif;

        json = %trim(json) + ' { ';
        BuildGiantJSON(json :'company' :%editc(APCO#:'X'));
        BuildGiantJSON(json :'number' :%editc(APPROD:'X'));
        json += ',"name":{';
         BuildGiantJSON(json :'last' :%trim(APLSN));
         BuildGiantJSON(json :'first' :%trim(APFSN));
         BuildGiantJSON(json :'full':FormatName(APlsN:APfsN));
        json += '}';
        BuildGiantJSON(json :'email' :%trim(APemal));
        json += ',"agentInfo":{';
         BuildGiantJSON(json :'fein' :%editc(APFEIN:'X'));
         BuildGiantJSON(json :'modifier' :%editc(APMOD:'X'));
         BuildGiantJSON(json :'name':AGname);
        json += '}';
        json += ' } ';

       Endsr;

       //=================================================================
       // Build a example array
       //=================================================================
       Begsr BuildExampleArray;
        StartJSONarray(array);
        array = %trim(array) + 'arrayValue1,arrayValue2,arrayValue3';
        EndJSONarray(array);
        BuildGiantJSON(json:'myArray':array);
       Endsr;

       //=================================================================
       // Build a example sub-JSON
       //=================================================================
       Begsr BuildExampleSub;
        StartGiantElement(jsonSub);
        BuildGiantJSON(jsonSub:'first':'John');
        BuildGiantJSON(jsonSub:'last':'Smith');
        EndGiantElement(jsonSub);
        StartGiantElement(jsonSub);
        BuildGiantJSON(jsonSub:'first':'Bill');
        BuildGiantJSON(jsonSub:'last':'Jones');
        EndGiantElement(jsonSub);
        BuildGiantJSON(json:'names':jsonSub:'0');
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          Reset result;
          Clear errMsg;
          Clear errMsg500;
          Clear comma;
          //*** Begin Add *** 10/27/22 *************************************
          // If status not passed in then defualt to Active
          If inRequest = 'ListProducers';
             result.status = 'A';
          EndIf;
          //*** End   Add *** 10/27/22 *************************************

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          errMsg = errMsg500;

          // Parse the parameters if no error from YAJL.
          If errMsg = ' ';

             json = %trim(json) + ' "requestParms" : { ';

             // Load the data structure with the parameter values
             // from the tree
             Dow YAJL_object_loop(docNode:i1:key:val);
                BuildGiantJSON(json :key :%trim(YAJL_get_string(val)));
                Select;
                When key = 'rtnCode';
                   result.rtnCode = YAJL_get_string(val);
                When key = 'fein';
                   result.fein = YAJL_get_string(val);
                When key = 'mod';
                   result.mod = YAJL_get_string(val);
                When key = 'lastName';
                   result.lastName = YAJL_get_string(val);
                When key = 'firstName';
                   result.firstName = YAJL_get_string(val);
                When key = 'status';
                   result.status = YAJL_get_string(val);
                When key = 'co#';
                   result.co# = YAJL_get_string(val);
                When key = 'prod#';
                   result.prod# = YAJL_get_string(val);
                When key = 'includeRequest';
                   result.includeRequest = YAJL_get_string(val);
                When key = 'includeCancelled';
                   result.includeCancelled = YAJL_get_string(val);
                When key = 'agency';
                   Exsr GetParmsAgency;
                Endsl;
             Enddo;

             json = %trim(json) + ' }, ';

             // Free up the space by removing the tree
             yajl_tree_free(docNode);
          Else;
             Leavesr;
          EndIf;
          If not result.includeRequest;
             Clear json;
          Endif;

          //* Agency Federal ID;
          If result.fein <> ' ';
             Monitor;
                keyFein = %int(result.fein);
             On-error;
                If comma <> ' ';
                   errMsg = %trim(errMsg) + comma;
                Else;
                   comma = ',';
                Endif;
                errMsg =  %trim(errMsg) + '"Invalid fein received in ' +
                 'incoming parameters: ''' + result.fein + ''' "';
             Endmon;
          Else;
             Clear keyFein;
          Endif;

          //* Agency Mod
          If result.mod <> ' ';
             Monitor;
                keyMod = %int(result.mod);
                noKeyMod = ' ';
             On-error;
                If comma <> ' ';
                   errMsg = %trim(errMsg) + comma;
                Else;
                   comma = ',';
                Endif;
                errMsg =  %trim(errMsg) + '"Invalid mod received in ' +
                 'incoming parameters: ''' + result.mod + ''' "';
             Endmon;
          Else;
             Clear keyMod;
             noKeyMod = 'Y';
          Endif;

          //* Agency Number
          If result.agency.number <> ' ';
             Monitor;
                keyAgentNumber = %int(result.agency.number);
             On-error;
                errMsg += comma + '"Invalid agency number received in ' +
                 'incoming parameters: ''' + result.agency.number + ''' "';
                comma = ',';
             Endmon;
          Else;
             //*** Beg Chg *** 01/25/24 ************************************
             //***Clear keyMod;
             //***noKeyMod = 'Y';
             Clear keyAgentNumber;
             //*** End Chg *** 01/25/24 ************************************
          Endif;

          //* Producer Last Name
          If result.lastName <> ' ';
             keyLastName = %trim(result.lastName);
          Else;
             Clear keyLastName;
          Endif;

          //* Producer First Name
          If result.firstName <> ' ';
             keyFirstName = %trim(result.firstName);
          Else;
             Clear keyFirstName;
          Endif;

          //* Producer Status
          If result.status <> ' ';
             keyStatus = %trim(result.status);
          Else;
             Clear keyStatus;
          Endif;

          //* Company
          If result.co# <> ' ';
             Monitor;
                keyCo# = %int(result.co#);
             On-error;
                If comma <> ' ';
                   errMsg = %trim(errMsg) + comma;
                Else;
                   comma = ',';
                Endif;
                errMsg =  %trim(errMsg) + '"Invalid co# received in ' +
                 'incoming parameters: ''' + result.co# + ''' "';
             Endmon;
          Else;
             keyCo# = UPCo#;
          Endif;

          //* Producer
          If result.prod# <> ' ';
             Monitor;
                keyProd# = %int(result.prod#);
             On-error;
                If comma <> ' ';
                   errMsg = %trim(errMsg) + comma;
                Else;
                   comma = ',';
                Endif;
                errMsg =  %trim(errMsg) + '"Invalid prod# received in ' +
                 'incoming parameters: ''' + result.prod# + ''' "';
             Endmon;
          Else;
             Clear keyProd#;
          Endif;

          keyIncludeCancelled = result.includeCancelled;
          If keyIncludeCancelled = *blanks;
             keyIncludeCancelled = 'N';
          Endif;

       Endsr;

       //=================================================================
       // Break parms out from inParms - Agency
       //=================================================================
       Begsr GetParmsAgency;
        node2 = YAJL_object_find(docNode:key);
        json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
        i2 = 0;
        Dow YAJL_object_loop(node2:i2:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              result.agency.number = YAJL_get_string(val);
           When key = 'name';
              result.agency.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtCd;
        Clear ioRtCd;
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
       Endsr;

      /End-free
