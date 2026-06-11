     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL') DECEDIT('0.')
     H*====================================================================
     H* Create on 08/08/2018 by c:62 for Project SY406
     H*====================================================================
     H* Description: Retrieves Agency Information
     H*
     H*
     H*====================================================================
     H* inParms -
     H*   fein:AGfein
     H*   mod:AGmod
     H*   name:AGname
     H*   status:AGstat
     H*   group:AAfnd
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 11/18/20 C:180 Project WC2757
     H*                Recompiled only.
     H*
     H*
     H*====================================================================
     Fwmupl     if   e           k disk    rename(pup01:pup00)

     D* this size needs to change
     D array           s               a   len(32767)
     d inJson          s               a   len(2000000) varying
     d isIncludeRequest...
     d                 s              1n   inz('1')
     D json            s               a   len(8500000) varying
     D jsont           s               a   len(8500000) varying
     D jsonSub         s               a   len(8500000) varying
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     d intCo#          s              3  0
     D sqlStmtStr      s            200a
     d strAuthority    s             10a
     d strCo#          s              3a
     d strEnv          s              4a
     d strSql          s          15000a   varying inz
     d strSqlWhere     s           5000a   varying inz
     d strSqlWith      s          10000a   varying inz
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     D moreRows        s               n   Inz(*On)
     D agent#          s              5a
     D errMsg          s            500a   Varying Inz(' ')
     d minutes         s              3  0 Inz(2)
     d seconds         s              3  0 Inz(0)
     d filter          s               n   Inz(*Off)
     d group#          s              3  0
     d clientID        s              3a

     D docNode         s                   like(yajl_val)
     D i0              s             10i 0 inz
     D i1              s             10i 0 inz
     D i2              s             10i 0 inz
     D key             s             50a   varying
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D node0           s                   like(yajl_val)
     D node1           s                   like(yajl_val)
     D node2           s                   like(yajl_val)
     D
     D val             s                   like(yajl_val)

     DdsAgtGrpInfo     ds                  qualified dim(999) inz
     D group                          3  0
     D agent#                         5  0

     D PSDS          ESDS                  extname(SZQ1P)

     D dsWMAGP       e ds                  extname(WMAGP) inz

     D result          ds                  Qualified inz
     D  rtnCode                      10a
     D  fein                          9a
     D  mod                           3a
     D  name                         40a
     D  status                       25a   varying
     D  group                         3a
     D  includeRequest...
     D                                1n   inz('1')

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJson
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy wPrAgent
      /copy wPrEmplr
      /copy wPrGroup
      /copy wPrUsrPrms
      /include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('WIAGENCY')
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

     d* Time Delay
     d TimeDelay       pr                  ExtPgm('TIMEDELAY')
     d   minutes                      3  0
     d   seconds                      3  0

     DCreateNode       pr         20000a   varying
     D   tag                        256a   const
     D   value                    20000a   varying const
     D   type                        10    const

     DCrtAddrObj       pr           400a   varying
     D   tag                        256a   varying const
     D   address1                    40a   const
     D   address2                    40a   const
     D   city                        15a   const
     D   state                        2a   const
     D   zipCode                      9s 0 const


     DCrtPhoneObj      pr           400a   varying
     D   tag                        256a   varying const
     D   areaCode                     3s 0 const
     D   phone#                       9s 0 const
     D   type                        15a   const

     DStrArray         pr           260a   varying
     D   tag                        256a   varying

      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;

       // TimeDelay(minutes:seconds);

       Chain (inUser) pup00;

       Clear json;

       Select;
       When inRequest = 'GetAgency';
          Exsr GetAgtLstP;
          Exsr BldRequest;
          Exsr BldAgtInfo;
       When inRequest = 'ListAgencies';
          Exsr GetAgtLstP;
          Exsr BldRequest;
          Exsr BldAgtList;
       When inRequest = 'ListCommissions';
          ListCommissions();
       When inRequest = 'ListCommissionPayments';
          ListCommissionPayments();
       Other;
          Exsr BuildStub;
       EndSl;

       outJSON = json;

       *inlr = *on;

       //=================================================================
       // Build Agency Cursor
       //=================================================================
       Begsr BldAgtCursor;
          Select;
          When Upper(result.status) = 'ACTIVE';
             result.status = 'A';
          When Upper(result.status) = 'INACTIVE';
             result.status = 'I';
          When Upper(result.status) = 'ALL';
             Clear result.status;
          Endsl;

          // Build agency sql statement
          sqlStmtStr = 'Select * from WMAGP a';

          // Add on Filters
          // Start Where
          If result.fein  <> ' ' Or result.mod    <> ' ' Or
             result.name  <> ' ' Or result.status <> ' ' Or
             result.group <> ' ';
             sqlStmtStr = %Trim(sqlStmtStr) + ' Where';
             // Fein Filter
             If result.fein <> ' ';
                sqlStmtStr = %Trim(sqlStmtStr) + ' AGfein=' + result.fein;
                filter = *On;
             EndIf;
             // Modifier Filter
             If result.mod <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And AGmod=' + result.mod;
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' AGmod=' + result.mod;
                   filter = *On;
                EndIf;
             EndIf;
             // Modifier Filter
             If result.name <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And Upper(AGname) like ''%' +
                      %Trim(Upper(result.name)) + '%''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' Upper(AGname) like ''%' +
                       %Trim(Upper(result.name)) + '%''';
                   filter = *On;
                EndIf;
             EndIf;
             // Status Filter
             If result.status <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And AGstat=''' + result.status + '''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' AGstat=''' + result.status + '''';
                   filter = *On;
                EndIf;
             EndIf;
             // Group Filter
             If result.group <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And (Select count(*) from wmaap ' +
                      'where AAfein=a.AGfein And AAmod=a.AGmod And ' +
                      'AAfnd=' + %Trim(result.group) + ') <> 0';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' (Select count(*) from wmaap ' +
                      'where AAfein=a.AGfein And AAmod=a.AGmod And ' +
                      'AAfnd=' + %Trim(result.group) + ') <> 0';
                   filter = *On;
                EndIf;
             EndIf;
          EndIf;

          Exec SQL close agtCursor;

          // Prepare the SQL statement
          Exec SQL prepare agtStmt from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare agtCursor cursor for agtStmt;

          // Open cursor
          Exec SQL open agtCursor;
       Endsr;

       //=================================================================
       // Build Agency Information
       //=================================================================
       Begsr BldAgtInfo;
        json += '"agencyInfo":';
        Exsr BldAgtCursor;
        Reset dsWMAGP;
        Exec SQL fetch next from agtCursor into :dsWMAGP;
        If not IsValidFetch(sqlstt);
           Monitor;
              AGfein = %int(result.fein);
           On-error;
           Endmon;
           Monitor;
              AGmod = %int(result.mod);
           On-error;
           Endmon;
        Endif;
        // Build Record
        Exsr BldAgtRcd;
       Endsr;

       //=================================================================
       // Build Agency Listing
       //=================================================================
       Begsr BldAgtList;

          // Start the Array
          json += '"agencyList" : [';

          Exsr BldAgtCursor;

          // Get first record if there is one
          Exec SQL fetch next from agtCursor into :dsWMAGP;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             moreRows = *off;
             Exec SQL close agtCursor;
          EndIf;

          // If row was fetched then process
          DoU moreRows = *Off;

             // Build Record
             Exsr BldAgtRcd;

             // Fetch next Record
             Exec SQL fetch next from agtCursor into :dsWMAGP;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                moreRows = *Off;
                Exec SQL close agtCursor;
             Else;
                // Prepare for next record
                json = %Trim(json) + ' ,';
             EndIf;
          EndDo;

          // End the Array
          json = %Trim(json) + ' ]';

       Endsr;

       //=================================================================
       // Build the Array Records
       //=================================================================
       Begsr BldAgtRcd;

        Select;
        When not MeetsSecurity;
           Leavesr;
        Endsl;

        If result.group <> ' ';
           Monitor;
              group# = %Dec(%Trim(result.group):3:0);
           On-Error;
              group# = 0;
           EndMon;
           agent# = %EditC(GetAgent#(AGco#:AGfein:AGmod:group#):'X');
        Else;
           agent# = ' ';
        EndIf;

        json = %trim(json) + ' { ' +
         CreateNode('fein':%EditC(AGfein:'X'):'text') + ',' +
         CreateNode('mod':%EditC(AGmod:'X'):'text')   + ',' +
         CreateNode('name':%Trim(AGname):'text')      + ',' +
         '"address":{' +
          BuildJsonAddress(AGmAd1:*blanks:AGmCty:AGmSt:%char(AGmZip)
           :'mailing') +
          ', ' +
          BuildJsonAddress(AGpAd1:' ':AGpCty:AGpSt:%char(AGpZip)
           :'physical') +
         '}, ' +
         '"phone":[{'  +
          BuildJsonPhone(%char(AGare1):%char(AGphn1):'':'phone1') + ',' +
          BuildJsonPhone(%char(AGare2):%char(AGphn2):'':'phone2') + ',' +
          BuildJsonPhone(%char(AGfaxA):%char(AGfax#):'fax':'fax') +
         '}],' +
         BuildJsonNode('email':%trim(AGeMal)) + ',' +
         BuildJsonNode('website':%trim(AGiAdr)) + ',' +
         CreateNode('agt#':%Trim(agent#):'text') +
         ' }';

       Endsr;

       //=================================================================
       // Build Request Json
       //=================================================================
       Begsr BldRequest;
          If not result.includeRequest;
             Leavesr;
          Endif;

          json = '"requestParms" : {' +
                 CreateNode('fein':result.fein:'text') + ',' +
                 CreateNode('mod':result.mod:'text') + ',' +
                 CreateNode('name':%Trim(result.name):'text') + ',' +
                 CreateNode('status':result.status:'text') + ',' +
                 CreateNode('group':result.group:'text') +
                 '},';
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetAgtLstP;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg);
          If errMsg = ' ';
             // Load the data structure with the parameter values
             // from the tree
             Dow YAJL_object_loop(docNode:i1:key:val);
                Select;
                When key = 'rtnCode';
                   result.rtnCode = YAJL_get_string(val);
                When key = 'fein';
                   result.fein = YAJL_get_string(val);
                When key = 'mod';
                   result.mod = YAJL_get_string(val);
                When key = 'name';
                   result.name = YAJL_get_string(val);
                When key = 'status';
                   result.status = YAJL_get_string(val);
                When key = 'group';
                   result.group = YAJL_get_string(val);
                When key = 'includeRequest';
                   result.includeRequest = YAJL_get_string(val);
                Endsl;
             Enddo;
             // Free up the space by removing the tree
             yajl_tree_free(docNode);
          EndIf;

       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
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
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';
       Endsr;

      /End-free
       //===================================================================
       // CreateNode - Create Json Node
       //===================================================================
     p CreateNode      b
     d CreateNode      pi         20000a   varying
     D   tag                        256a   const
     D   value                    20000a   varying const
     D   type                        10    const

     d strNode         s          20000a   varying Inz
     d strValue        s          20000a   varying Inz

      /free

       Select;
       When Type = 'text'; // Add Double Quotes
          strValue = '"' + %Trim(value) + '"';
       Other;
          strValue = value;
       EndSl;

       strNode = '"' + %Trim(tag) + '" : ' + %Trim(strValue);

       Return strNode;

      /end-free
     p CreateNode      e
       //===================================================================
       // CrtAddrObj - Create Json Address Object
       //===================================================================
     p CrtAddrObj      b
     d CrtAddrObj      pi           400a   varying
     D   tag                        256a   varying const
     D   address1                    40a   const
     D   address2                    40a   const
     D   city                        15a   const
     D   state                        2a   const
     D   zipCode                      9s 0 const

     d strAddress      s            400a   varying Inz

      /free

       strAddress = '"' + tag + '" : { "address1" : "' + %Trim(address1) +
          '" , "address2" : "' + %Trim(address2) + '" , "city" : "' +
          %Trim(city) + '" , "state" : "' + %Trim(state) +
          '" , "zipCode" : "' + %EditC(zipCode:'X') + '" }';

       Return strAddress;

      /end-free
     p CrtAddrObj      e
       //===================================================================
       // CrtAddrObj - Create Json Address Object
       //===================================================================
     p CrtPhoneObj     b
     d CrtPhoneObj     pi           400a   varying
     D   tag                        256a   varying const
     D   areaCode                     3s 0 const
     D   phone#                       9s 0 const
     D   type                        15a   const

     d strPhone#       s            400a   varying Inz

      /free

       strPhone# = '"' + tag + '" : { "areaCode" : "' +
          %EditC(areaCode:'X') + ' ", "phone#" : "' +
          %EditC(phone#:'X')   + ' ", "type" : "' +
          %Trim(type) + '" }';

       Return strPhone#;

      /end-free
     p CrtPhoneObj     e

       //=================================================================
       // List Commission Payments
       //=================================================================
     PListCommissionPayments...
     P                 b

     ddecAmount        s             15  2 inz
     ddecCollected     s             15  2 inz
     ddecEarned        s             15  2 inz
     ddecPaid          s             15  2 inz
     dintFein          s              9  0 inz
     dintMod           s              3  0 inz
     dintProd          s              5  0 inz
     disEofWMACP       s              1n   inz
     dstrJsonPayment   s          10000    inz varying
     dstrJsonProd      s          25000    inz varying
     dstrName          s            100    inz varying

     ddsWMACP          ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d agt#                           5  0
     d fein                           9  0
     d mod                            3  0
     d prod                           5  0
     d rate                           5  2
     d postDt7                        7  0
     d transDt7                       7  0
     d coTp                           1
     d coCd                           2
     d desc                          30
     d chk#                           7  0
     d amt                            9  2
     d toCl                          11  2
     ddsWMACPinz       ds                  likeds(dsWMACP) inz
     ddsWMACPsave      ds                  likeds(dsWMACP) inz
     ddsWMAPP        e ds                  extname(WMAPP) inz qualified

     ddsDate           ds                  qualified inz
     d dt8                            8
     d dt7                            7  0
     ddsAgent          ds                  qualified inz
     d number                         5
     d fein                           9
     d mod                            3
     d name                          40    varying
     ddsDateRange      ds                  qualified inz
     d from                                likeds(dsDate)
     d to                                  likeds(dsDate)
     ddsProducer       ds                  qualified inz
     d number                         5
     d name                          40    varying
     ddsIn             ds                  qualified inz
     d group                          3
     d agent                               likeds(dsAgent)
     d producer                            likeds(dsProducer)
     d asOfDate                            likeds(dsDate)
     d statementDate                       likeds(dsDateRange)
      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based on parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListCommissionPayments}');
        AddErrorMessageJson(%trim(inRequest) + ' has been depricated. ' +
         'Please use ListCommissions.':ioRtCd);
        Leavesr;
        //Exsr BuildStub;
        //Leavesr;
        Exsr BuildSql;
        //json += ', "sql":"' + strSql + '"';
        json += ', "results":{"dates":{' +
         BuildJsonNode('asOf':dsIn.asOfDate.dt8:'date') + ',' +
         '"statement":{' +
          BuildJsonNode('from':dsIn.statementDate.from.dt8:'date') + ',' +
          BuildJsonNode('to':dsIn.statementDate.to.dt8:'date') + '}' +
         '}';
        strJsonProd += ', "producerList":[';
        json += ', "paymentsList":[';
        Reset dsWMACP;
        Dou isEofWMACP;
           Exec sql fetch next from cursorPaymentsList into :dsWMACP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofWMACP = *on;
              If dsWMACPsave <> dsWMACPinz;
                 //Exsr BuildOutputProd;
              Endif;
              Iter;
           When dsWMACPsave <> dsWMACPinz and
            dsWMACPsave.prod <> *zero and
            (dsWMACP.prod <> dsWMACPsave.prod
              or dsWMACP.rate <> dsWMACPsave.rate);
              //Exsr BuildOutputProd;
           Endsl;
           dsWMACPsave = dsWMACP;
           Select;
           When dsWMACP.coTp <> 'P' and dsWMACP <> 'V';
              decCollected += dsWMACP.toCl;
              decAmount += dsWMACP.amt;
           When dsWMACP.coTp = 'V';
              dsWMACP.amt = -dsWMACP.amt;
           Endsl;
           Select;
           When dsWMACP.coTp = 'P' or dsWMACP.coTp = 'V';
              //Exsr BuildOutputPayment;
           Endsl;
        Enddo;
        strJsonProd = %scanrpl(',':'':strJsonProd:%len(strJsonProd)-1);
        strJsonProd += ']';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']' + strJsonProd + '}';
        Exec sql close cursorPaymentsList;
       Endsr;

       //=================================================================
       // Build sql based on incoming parms - Payments
       //=================================================================
       Begsr BuildSql;
        strSql = 'select ACCO#, ACFND, ACAGT#, ACFEIN, ACMOD, ACPROD, ' +
         'ACRATE, ACPODT, ACTRDT, ACCOTP, ACCOCD, ACDESC, ACCHCK, ACAMT, ' +
         'ACTOCL ' +
         ' from WMACP ' +
         ' join WMAAP on ACCO#=AACO# and ACFND=AAFND and ACAGT#=AAAGT# ' +
         ' join WMAGP on AACO#=AGCO# and AAFEIN=AGFEIN and AAMOD=AGMOD ' +
         ' left join WMAPP on AACO#=APCO# and AAFEIN=APFEIN and AAMOD=APMOD ' +
         '  and ACPROD=APPROD ' +
         ' where ACCO# = ' + strCo# +
         '  and ACPODT between ' + %char(dsIn.statementDate.from.dt7) +
         '    and ' + %char(dsIn.statementDate.to.dt7);
        If dsIn.group <> *blank;
           strSql += ' and ACfnd = ' + dsIn.group;
        Endif;
        If dsIn.agent.number <> *blank;
           strSql += ' and ACagt# = ' + dsIn.agent.number;
        Endif;
        If dsIn.agent.fein <> *blank;
           strSql += ' and ' +
            ' (digits(ACfnd)||digits(ACagt#) in (select agt# from assign) ' +
            '   or ACfein = ' + dsIn.agent.fein;
           strSqlWith = 'with assign as ' +
            '(select digits(AAFND) || digits(AAAGT#) agt#, AAFEIN, AAMOD ' +
            '  from WMAAP ' +
            '   where AAfein = ' + dsIn.agent.fein;
           If dsIn.agent.mod <> *blank;
              strSqlWith += ' and AAmod = ' + dsIn.agent.mod;
              strSql += ' and ACmod = ' + dsIn.agent.mod + ')';
           Endif;
           strSqlWith += ') ';
        Endif;
        If dsIn.agent.name <> *blank;
           strSql += ' and upper(AGname) like ' +
            '''%' + %trim(Upper(dsIn.agent.name)) + '%'' ';
        Endif;
        // Filter out dsIn.producer.name
        If dsIn.producer.number <> *blank;
           strSql += ' and (ACprod = ' + dsIn.producer.number +
            ' or ACprod = 0)';
        Endif;
        If dsIn.producer.name <> *blank;
           strSql += ' and (upper(APfsN) || upper(APlsN) like ' +
            '''%' + %trim(Upper(dsIn.producer.name)) + '%'' ' +
            ' or ACprod = 0)';
        Endif;
        strSql = strSqlWith + strSql +
         ' order by ACCO#, ACFND, ACAGT#, AAFEIN, AAMOD, ACPROD, ACRATE, ' +
         '  ACPODT desc';
        Exsr BuildSqlCursor;
       Endsr;

       //=================================================================
       // Build sql cursor
       //=================================================================
       Begsr BuildSqlCursor;
        Exec sql close cursorPaymentsList;
        Exec sql prepare sqlPaymentsList from :strSql;
        Exec sql declare cursorPaymentsList cursor for sqlPaymentsList;
        If sqlstt = '42601';
           AddErrorMessageJson('RPG created an invalid SQL statement.':ioRtCd);
        Endif;
        Exec sql open cursorPaymentsList;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListCommissionPayments}');
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson : errMsg);
        If errMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtCd);
           Leavesr;
        Endif;
        json += ' "requestParms":{';
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'dates';
              Exsr GetParmsDates;
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'agent';
              Exsr GetParmsAgent;
           When key = 'producer';
              Exsr GetParmsProducer;
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '}';
        // Free up the space by removing the tree
        yajl_tree_free(node0);

        Exsr Validate;
       Endsr;

       //=================================================================
       // Break parms out from inParms - Agent
       //=================================================================
       Begsr GetParmsAgent;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.agent.number = YAJL_get_string(val);
           When key = 'fein';
              dsIn.agent.fein = YAJL_get_string(val);
           When key = 'mod';
              dsIn.agent.mod = YAJL_get_string(val);
           When key = 'name';
              dsIn.agent.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Dates
       //=================================================================
       Begsr GetParmsDates;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'asOf';
              dsIn.asOfDate.dt8 = YAJL_get_string(val);
              Monitor;
                 dsIn.asOfDate.dt7 =
                  DtCYMD(%int(dsIn.asOfDate.dt8));
              On-error;
              Endmon;
           When key = 'statement';
              json = %scanrpl('""':'{':json:%len(json)-1);
              node2 = YAJL_object_find(node1:key);
              Reset i2;
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'from';
                    dsIn.statementDate.from.dt8 = YAJL_get_string(val);
                    Monitor;
                       dsIn.statementDate.from.dt7 =
                        DtCYMD(%int(dsIn.statementDate.from.dt8));
                    On-error;
                    Endmon;
                 When key = 'to';
                    dsIn.statementDate.to.dt8 = YAJL_get_string(val);
                    Monitor;
                       dsIn.statementDate.to.dt7 =
                        DtCYMD(%int(dsIn.statementDate.to.dt8));
                    On-error;
                    Endmon;
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Producer
       //=================================================================
       Begsr GetParmsProducer;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.producer.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.producer.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Validate incoming parms
       //=================================================================
       Begsr Validate;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        If dsIn.asOfDate.dt8 <> *blanks
         and dsIn.asOfDate.dt7 = 0;
           AddErrorMessageJson('Invalid ''as of'' date.'
            :ioRtCd);
        Endif;
        If dsIn.statementDate.from.dt8 <> *blanks
         and dsIn.statementDate.from.dt7 = 0;
           AddErrorMessageJson('Invalid statement ''from'' date.'
            :ioRtCd);
        Endif;
        If dsIn.statementDate.to.dt8 <> *blanks
         and dsIn.statementDate.to.dt7 = 0;
           AddErrorMessageJson('Invalid statement ''to'' date.'
            :ioRtCd);
        Endif;
        If dsIn.asOfDate.dt7 = *zeros;
           dsIn.asOfDate.dt7 = DateTo7(%date);
           dsIn.asOfDate.dt8 = %char(DtMDYY(dsIn.asOfDate.dt7));
        Endif;
        Select;
        When dsIn.statementDate.to.dt7 = *zeros;
           dsIn.statementDate.to.dt8 = dsIn.asOfDate.dt8;
           dsIn.statementDate.to.dt7 = dsIn.asOfDate.dt7;
        Other;
        Endsl;
       Endsr;
      /End-free
     PListCommissionPayments...
     P                 e

       //=================================================================
       // List Commissions
       //=================================================================
     PListCommissions  b

     ddecAmount        s             15  2 inz
     ddecCollected     s             15  2 inz
     ddecEarned        s             15  2 inz
     ddecPaid          s             15  2 inz
     dintFein          s              9  0 inz
     dintMod           s              3  0 inz
     dintProd          s              5  0 inz
     disEofWMACP       s              1n   inz
     disIncDetail      s              1n   inz
     disIncZeroAmt     s              1n   inz
     dstrJsonAgent     s         200000    inz varying
     dstrJsonDetail    s         100000    inz varying
     dstrJsonProd      s         150000    inz varying
     dstrName          s            100    inz varying

     ddsWMACPold       ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d agt#                           5  0
     d fein                           9  0
     d mod                            3  0
     d coTp                           1
     d amt                           13  2
     ddsWMACP          ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d agt#                           5  0
     d fein                           9  0
     d mod                            3  0
     d prod                           5  0
     d rate                           5  2
     d postDt7                        7  0
     d transDt7                       7  0
     d coTp                           1
     d coCd                           2
     d desc                          30
     d chk#                           7  0
     d amt                            9  2
     d toCl                          11  2
     d emp#                           9  0
     d div                            5  0
     d fyr                            3  0
     ddsWMACPinz       ds                  likeds(dsWMACP) inz
     ddsWMACPsave      ds                  likeds(dsWMACP) inz
     ddsWMAPP        e ds                  extname(WMAPP) inz qualified

     ddsDate           ds                  qualified inz
     d dt8                            8
     d dt7                            7  0
     ddsAgent          ds                  qualified inz
     d number                         5
     d fein                           9
     d mod                            3
     d name                          40    varying
     ddsDateRange      ds                  qualified inz
     d from                                likeds(dsDate)
     d to                                  likeds(dsDate)
     ddsProducer       ds                  qualified inz
     d number                         5
     d name                          40    varying
     dmaxTrans         c                   100
     ddsTransaction    ds                  qualified inz
     d code                           2    dim(maxTrans)
     d type                           1    dim(maxTrans)
     ddsIn             ds                  qualified inz
     d group                          3
     d agent                               likeds(dsAgent)
     d producer                            likeds(dsProducer)
     d asOfDate                            likeds(dsDate)
     d statementDate                       likeds(dsDateRange)
     d transaction                         likeds(dsTransaction)
     d detailList                    25    varying inz('*NONZERO')
      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based on parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSql;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        json += ', "results":{"dates":{' +
         BuildJsonNode('asOf':dsIn.asOfDate.dt8:'date') + ',' +
         '"statement":{' +
          BuildJsonNode('from':dsIn.statementDate.from.dt8:'date') + ',' +
          BuildJsonNode('to':dsIn.statementDate.to.dt8:'date') + '}' +
         '}';
        json += ', "commissionsList":[';
        Reset dsWMACP;
        Dou isEofWMACP;
           Exec sql fetch next from cursorCommissionList into :dsWMACP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofWMACP = *on;
              If dsWMACPsave <> dsWMACPinz;
                 Exsr BuildOutputAgent;
              Endif;
              Iter;
           When dsWMACPsave <> dsWMACPinz and
            (dsWMACP.fein <> dsWMACPsave.fein
             or dsWMACP.mod <> dsWMACPsave.mod);
              Exsr BuildOutputAgent;
           When dsWMACPsave <> dsWMACPinz and
            dsWMACPsave.prod <> *zero and
            (dsWMACP.prod <> dsWMACPsave.prod
              or dsWMACP.rate <> dsWMACPsave.rate);
              Exsr BuildOutputProd;
           Endsl;
           dsWMACPsave = dsWMACP;
           If dsWMACP.coTp = 'V';
              dsWMACP.amt = -dsWMACP.amt;
           Endif;
           Select;
           When dsWMACP.coTp = 'P' or dsWMACP.coTp = 'V';
              decPaid += dsWMACP.amt;
              dsWMACP.amt = -dsWMACP.amt;
           Other;
              decCollected += dsWMACP.toCl;
              decAmount += dsWMACP.amt;
              decEarned += dsWMACP.amt;
           Endsl;
           Exsr BuildOutputDetail;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']}';
        Exec sql close cursorCommissionList;
       Endsr;

       //=================================================================
       // Build list
       //=================================================================
       Begsr BuildOutputAgent;
        Exsr BuildOutputProd;
        intFein = dsWMACPsave.fein;
        intMod = dsWMACPsave.mod;
        Reset strName;
        Exec sql
         select AGname into :strName
          from WMAGP
          where AGco# = :intCo# and AGfein = :intFein and AGmod = :intMod;
        If strName = *blanks;
           strName = '** Not Found **';
        Endif;
        json += '{';
        json += '"agent":{' +
         BuildJsonNode('fein':%char(dsWMACPsave.fein)) + ',' +
         BuildJsonNode('mod':%char(dsWMACPsave.mod)) + ',' +
         BuildJsonNode('name':strName) + '},';
        json += '"detailSummary":{' +
         BuildJsonNode('earned':%editc(decEarned:'3')) + ',' +
         BuildJsonNode('paid':%editc(decPaid:'3')) + ',' +
         BuildJsonNode('outstanding':%editc(decEarned-decPaid:'3')) + '},';
        If strJsonProd <> *blank;
           strJsonProd = %scanrpl(',':'':strJsonProd:%len(strJsonProd)-1);
        Endif;
        json += '"producerList": [' + strJsonProd + '],';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
        Reset strJsonProd;
        Reset decEarned;
        Reset decPaid;
       Endsr;

       //=================================================================
       // Build list - Payments
       //=================================================================
       Begsr BuildOutputDetail;
        Select;
        When not isIncDetail;
           Leavesr;
        When dsWMACP.amt = 0 and not isIncZeroAmt;
           Leavesr;
        Endsl;
        strJsonDetail += '{';
        strJsonDetail += '"dates":{' +
         BuildJsonNode('posted':%char(dsWMACP.postDt7):'date') + ',' +
         BuildJsonNode('transaction':%char(dsWMACP.transDt7):'date') + '},';
        strJsonDetail += BuildJsonNode('checkNumber':%char(dsWMACP.chk#)) + ',';
        strJsonDetail += '"policyInformation":{' +
         BuildJsonNode('number'
          :GetEmpPol#(
           GetEmpYrRR(dsWMACP.fyr:dsWMACP.emp#:dsWMACP.div:dsWMACP.fnd
            :dsWMACP.co#))) + ',' +
         BuildJsonNode('group':%char(dsWMACP.fnd)) + ',' +
         BuildJsonNode('employer':%char(dsWMACP.emp#)) + ',' +
         BuildJsonNode('division':%char(dsWMACP.div)) + ',' +
         BuildJsonNode('year':%char(dsWMACP.fyr)) + '},';
        strJsonDetail += '"transaction":{' +
         BuildJsonNode('code':dsWMACP.coCd) + ',' +
         BuildJsonNode('type':dsWMACP.coTp) + ',' +
         BuildJsonNode('description':dsWMACP.desc) + '},';
        strJsonDetail += BuildJsonNode('amount':%editc(dsWMACP.amt:'J')) + ',';
        strJsonDetail = %scanrpl(',':'':strJsonDetail:%len(strJsonDetail)-1);
        strJsonDetail += '},';
       Endsr;

       //=================================================================
       // Build list - Producers
       //=================================================================
       Begsr BuildOutputProd;
        intFein = dsWMACPsave.fein;
        intMod = dsWMACPsave.mod;
        intProd = dsWMACPsave.prod;
        Reset dsWMAPP;
        Exec sql
         select * into :dsWMAPP
          from WMAPP
          where APco# = :intCo# and APfein = :intFein and APmod = :intMod
           and APprod = :intProd;
        strJsonProd += '{';
        strJsonProd += BuildJsonNode('number':%char(dsWMACPsave.prod)) + ',';
        Select;
        When dsWMAPP.APfsn = *blanks and dsWMAPP.APlsn = *blanks;
           strJsonProd += BuildJsonNode('name':'** Not Found **') + ',';
        Other;
           strJsonProd += '"name":{' +
            BuildJsonNode('first':dsWMAPP.APfsn) + ',' +
            BuildJsonNode('last':dsWMAPP.APlsn) + ',' +
            BuildJsonNode('full':FormatName(dsWMAPP.APlsn:dsWMAPP.APfsn)) +
            '},';
        Endsl;
        strJsonProd += '"detailSummary":{' +
         BuildJsonNode('collected':%editc(decCollected:'3')) + ',' +
         BuildJsonNode('rate':%editc(dsWMACPsave.rate:'3')) + ',' +
         BuildJsonNode('amount':%editc(decAmount:'3')) + '},';
        If strJsonDetail <> *blank;
           strJsonDetail = %scanrpl(',':'':strJsonDetail:%len(strJsonDetail)-1);
        Endif;
        strJsonProd += '"detailList": [' + strJsonDetail + '],';
        strJsonProd = %scanrpl(',':'':strJsonProd:%len(strJsonProd)-1);
        strJsonProd += '},';
        Reset strJsonDetail;
        Reset decAmount;
        Reset decCollected;
       Endsr;

       //=================================================================
       // Build sql based on incoming parms
       //=================================================================
       Begsr BuildSql;
        strSql = 'select ACCO#, ACFND, ACAGT#, AAFEIN, AAMOD, ACPROD, ' +
         'ACRATE, ACPODT, ACTRDT, ACCOTP, ACCOCD, ACDESC, ACCHCK, ACAMT, ' +
         'ACTOCL, ACEMP#, ACDIV, ACFYR ' +
         ' from WMACP ' +
         ' join WMAAP on ACCO#=AACO# and ACFND=AAFND and ACAGT#=AAAGT# ' +
         ' join WMAGP on AACO#=AGCO# and AAFEIN=AGFEIN and AAMOD=AGMOD ' +
         ' left join WMAPP on AACO#=APCO# and AAFEIN=APFEIN and AAMOD=APMOD ' +
         '  and ACPROD=APPROD ' +
         ' where ACCO# = ' + strCo# +
         '  and ACPODT between ' + %char(dsIn.statementDate.from.dt7) +
         '    and ' + %char(dsIn.statementDate.to.dt7);
        If dsIn.group <> *blank;
           strSql += ' and ACfnd = ' + dsIn.group;
        Endif;
        If dsIn.agent.number <> *blank;
           strSql += ' and ACagt# = ' + dsIn.agent.number;
        Endif;
        If dsIn.agent.fein <> *blank;
           strSql += ' and ' +
            ' (digits(ACfnd)||digits(ACagt#) in (select agt# from assign) ';
           strSqlWith = 'with assign as ' +
            '(select digits(AAFND) || digits(AAAGT#) agt#, AAFEIN, AAMOD ' +
            '  from WMAAP ' +
            '   where AAfein = ' + dsIn.agent.fein;
           If dsIn.agent.mod <> *blank;
              strSqlWith += ' and AAmod = ' + dsIn.agent.mod;
              strSql += ' and ACmod = ' + dsIn.agent.mod ;
           Endif;
           strSqlWith += ') ';
           strSql += ') ';
        Endif;
        If dsIn.agent.name <> *blank;
           strSql += ' and upper(AGname) like ' +
            '''%' + %trim(Upper(dsIn.agent.name)) + '%'' ';
        Endif;
        // Filter out dsIn.producer.name
        If dsIn.producer.number <> *blank;
           strSql += ' and (ACprod = ' + dsIn.producer.number +
            ' or ACprod = 0)';
        Endif;
        If dsIn.producer.name <> *blank;
           strSql += ' and (upper(APfsN) || upper(APlsN) like ' +
            '''%' + %trim(Upper(dsIn.producer.name)) + '%'' ' +
            ' or ACprod = 0)';
        Endif;
        If dsIn.transaction.code(1) <> *blank;
           strSql += ' and ACcoCd in (';
           For i2 = 1 to maxTrans;
              If dsIn.transaction.code(i2) = *blanks;
                 Iter;
              Endif;
              strSql += '''' + dsIn.transaction.code(i2) + ''',';
           Endfor;
           strSql = %scanrpl(',':')':strSql:%len(strSql)-1);
        Endif;
        If dsIn.transaction.type(1) <> *blank;
           strSql += ' and ACcoTp in (';
           For i2 = 1 to maxTrans;
              If dsIn.transaction.type(i2) = *blanks;
                 Iter;
              Endif;
              strSql += '''' + dsIn.transaction.type(i2) + ''',';
           Endfor;
           strSql = %scanrpl(',':')':strSql:%len(strSql)-1);
        Endif;
        strSql = strSqlWith + strSql +
         ' order by ACCO#, ACFND, ACAGT#, AAFEIN, AAMOD, ACPROD, ACRATE, ' +
         '  ACPODT desc';
        Exsr BuildSqlCursor;
       Endsr;

       //=================================================================
       // Build sql cursor
       //=================================================================
       Begsr BuildSqlCursor;
        Exec sql close cursorCommissionList;
        Exec sql prepare sqlCommissionList from :strSql;
        Exec sql declare cursorCommissionList cursor for sqlCommissionList;
        If sqlstt = '42601';
           AddErrorMessageJson('RPG created an invalid SQL statement.':ioRtCd);
        Endif;
        Exec sql open cursorCommissionList;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListCommissions}');
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson : errMsg);
        If errMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtCd);
           Leavesr;
        Endif;
        json += ' "requestParms":{';
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'dates';
              Exsr GetParmsDates;
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'agent';
              Exsr GetParmsAgent;
           When key = 'producer';
              Exsr GetParmsProducer;
           When key = 'transaction';
              Exsr GetParmsTransaction;
           When key = 'detailList';
              dsIn.detailList = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '}';
        // Free up the space by removing the tree
        yajl_tree_free(node0);

        Exsr Validate;
       Endsr;

       //=================================================================
       // Break parms out from inParms - Agent
       //=================================================================
       Begsr GetParmsAgent;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.agent.number = YAJL_get_string(val);
           When key = 'fein';
              dsIn.agent.fein = YAJL_get_string(val);
           When key = 'mod';
              dsIn.agent.mod = YAJL_get_string(val);
           When key = 'name';
              dsIn.agent.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Dates
       //=================================================================
       Begsr GetParmsDates;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'asOf';
              dsIn.asOfDate.dt8 = YAJL_get_string(val);
              Monitor;
                 dsIn.asOfDate.dt7 =
                  DtCYMD(%int(dsIn.asOfDate.dt8));
              On-error;
              Endmon;
           When key = 'statement';
              json = %scanrpl('""':'{':json:%len(json)-1);
              node2 = YAJL_object_find(node1:key);
              Reset i2;
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'from';
                    dsIn.statementDate.from.dt8 = YAJL_get_string(val);
                    Monitor;
                       dsIn.statementDate.from.dt7 =
                        DtCYMD(%int(dsIn.statementDate.from.dt8));
                    On-error;
                    Endmon;
                 When key = 'to';
                    dsIn.statementDate.to.dt8 = YAJL_get_string(val);
                    Monitor;
                       dsIn.statementDate.to.dt7 =
                        DtCYMD(%int(dsIn.statementDate.to.dt8));
                    On-error;
                    Endmon;
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Producer
       //=================================================================
       Begsr GetParmsProducer;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.producer.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.producer.name = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Transaction
       //=================================================================
       Begsr GetParmsTransaction;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'code';
              json = %scanrpl('""':'[':json:%len(json)-1);
              node2 = YAJL_object_find(node1:key);
              Reset i2;
              Dow YAJL_array_loop(node2:i2:val);
                 json += '"' + YAJL_get_string(val) + '",';
                 If i2 < maxTrans;
                    dsIn.transaction.code(i2) = YAJL_get_string(val);
                 Endif;
              Enddo;
              If i2 > maxTrans + 1;
                 AddErrorMessageJson('Too many transactions codes requested. ' +
                  'Only the first ' + %char(maxTrans) + ' included.');
                 ioRtCd = 'Info Only';
              Endif;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ']';
           When key = 'type';
              json = %scanrpl('""':'[':json:%len(json)-1);
              node2 = YAJL_object_find(node1:key);
              Reset i2;
              Dow YAJL_array_loop(node2:i2:val) and i2 < maxTrans;
                 json += '"' + YAJL_get_string(val) + '",';
                 If i2 < maxTrans;
                    dsIn.transaction.type(i2) = YAJL_get_string(val);
                 Endif;
              Enddo;
              If i2 > maxTrans + 1;
                 AddErrorMessageJson('Too many transactions codes requested. ' +
                  'Only the first ' + %char(maxTrans) + ' included.');
                 ioRtCd = 'Info Only';
              Endif;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ']';
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Validate incoming parms
       //=================================================================
       Begsr Validate;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        If dsIn.asOfDate.dt8 <> *blanks
         and dsIn.asOfDate.dt7 = 0;
           AddErrorMessageJson('Invalid ''as of'' date.'
            :ioRtCd);
        Endif;
        If dsIn.statementDate.from.dt8 <> *blanks
         and dsIn.statementDate.from.dt7 = 0;
           AddErrorMessageJson('Invalid statement ''from'' date.'
            :ioRtCd);
        Endif;
        If dsIn.statementDate.to.dt8 <> *blanks
         and dsIn.statementDate.to.dt7 = 0;
           AddErrorMessageJson('Invalid statement ''to'' date.'
            :ioRtCd);
        Endif;
        If dsIn.asOfDate.dt7 = *zeros;
           dsIn.asOfDate.dt7 = DateTo7(%date);
           dsIn.asOfDate.dt8 = %char(DtMDYY(dsIn.asOfDate.dt7));
        Endif;
        Select;
        When dsIn.statementDate.to.dt7 = *zeros;
           dsIn.statementDate.to.dt8 = dsIn.asOfDate.dt8;
           dsIn.statementDate.to.dt7 = dsIn.asOfDate.dt7;
        Other;
        Endsl;
        If dsIn.detailList = *blanks;
           Reset dsIn.detailList;
        Endif;
        isIncDetail = Upper(dsIn.detailList) <> '*NONE';
        isIncZeroAmt = Upper(dsIn.detailList) <> '*NONZERO';
       Endsr;
      /End-free
     pListCommissions  e

       //=================================================================
       // Check security
       //=================================================================
     PMeetsSecurity    b
     DMeetsSecurity    pi             1n

     DisMeets          s              1n   inz('1')
     Dg1               s              3  0
      /Free
       Exsr GetAgtGrpInfo;

       // Do not show if agecny is not in any allowed groups
       For g1 = 1 to 999;
          Select;
          When GroupAuth(AGco#:dsAgtGrpInfo(g1).group:inUser) = *on;
             Leave;
          Endsl;
       Endfor;
       Return isMeets;

       //=================================================================
       Begsr GetAgtGrpInfo;
        Reset dsAgtGrpInfo;
        Exec sql
         declare aa cursor for
          select AAfnd, AAagt#
           from WMAAP
           where AAco# = :AGco# and AAfein = :AGfein and AAmod = :AGmod
           order by AAFND;
        Exec sql open aa;
        Exec sql
         fetch aa for 999 rows
          into :dsAgtGrpInfo;
        Exec sql close aa;
       Endsr;
      /End-free
     PMeetsSecurity    e
