     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DftActGrp(*No) ActGrp('CONNECT')
     h Option(*NoDebugIO: *SrcStmt)
     h BndDir('STBNDDIR':'YAJL') DecEdit('0.')
     h/Title SISSO
     h* Object description intentially left blank
      *==========================================================
      * Description: Stores procedures to process various
      *              portal requests for policy JSON returns
      *==========================================================
      * Revisions:
      *
      *==========================================================
     FSSSOP     uf a e             disk    rename(pso01:pso0p) usropn

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying

     d clientID        s              3a
     d decRRN          s             10i 0 inz
     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz
     d intCo#          s              3  0 inz
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     d isError         s              1n
     d key             s             50a   varying
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d strAuthority    s             10a
     d strCo#          s              3a
     d strEnv          s              4a
     d strErrorMsg     s            500a   Varying
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     d val             s                   Like(yajl_val)

     dconSqlFileMismatch...
     d                 c                   '42802'

     ddsSSSOP        e ds                  extname(SSSOP) qualified inz
     ddsSSSOPdef       ds                  likerec(pso0p:*all) inz

     D PSDS          ESDS                  extname(SZQ1P)

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
      /copy sPrUsrPrms
      /Include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('SISSO')
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
       When inRequest = 'GetUserInfoSSO';
          GetUserInfoSSO();
       When inRequest = 'SaveUserInfoSSO';
          SaveUserInfoSSO();
       Other;
          Exsr BuildStub;
       EndSl;
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(outJSON:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        //intCo# = UserCo#(inUser);
        //strCo# = %Editc(intCo#:'X');
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
      * Get 3rd party SSO user information
      *=========================================================
     pGetUserInfoSSO...
     p                 b

     ddsIn             ds                  qualified Inz
     d thirdParty                    60a   varying
     d ssoUser                      128a   varying
      /Free

       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build output json
       //=================================================================
       Begsr BuildOutput;
        //Exsr BuildStub;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        dsSSSOP.SOparty = dsIn.thirdParty;
        dsSSSOP.SOssoUser = dsIn.ssoUser;
        Exec sql
         select * into :dsSSSOP from SSSOP
          where SOparty = :dsSSSOP.SOparty
           and SOssoUser = :dsSSSOP.SOssoUser;
        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('Record not found for requested information.'
            :ioRtnCode);
           Leavesr;
        Endif;
        json += ',"ssoInfo":{';
        json += BuildJsonNode('name':dsSSSOP.SOuserName) + ',';
        json += BuildJsonNode('password':dsSSSOP.SOuserInfo) + ',';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '}';
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetUserInfoSSO}'));
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        node0 = yajl_string_load_tree(inParms:strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        Reset i0;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'thirdParty';
              dsIn.thirdParty = YAJL_get_string(val);
           When key = 'ssoUser';
              dsIn.ssoUser = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}';

        // Free up the space by removing the tree
        yajl_tree_free(node0);

        Exsr Validate;
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr Validate;
        If dsIn.thirdParty = *blank;
           AddErrorMessageJson('Third Party required.'
            :ioRtnCode);
        Endif;
        If dsIn.ssoUser = *blank;
           AddErrorMessageJson('SSO user required.'
            :ioRtnCode);
        Endif;
       Endsr;
     pGetUserInfoSSO...
     p                 e

      *=========================================================
      * Save 3rd party SSO user information
      *=========================================================
     pSaveUserInfoSSO...
     p                 b

     ddsIn             ds                  qualified Inz
     d thirdParty                    60a   varying
     d ssoUser                      128a   varying
     d ssoName                      128a   varying
     d ssoPassword                  128a   varying

     disUpdate         s              1n
     dstrResults       s            100a   varying
      /Free

       Exsr GetParms;
       Exsr Process;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build output json
       //=================================================================
       Begsr BuildOutput;
        //Exsr BuildStub;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        strResults = 'Save successful.';
        Select;
        When isUpdate;
           strResults += ' (Updated)';
        Other;
           strResults += ' (Added)';
        Endsl;
        ioRtnCode = 'Success';
        json += ',' + BuildJsonNode('results':strResults);
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {SaveUserInfoSSO}'));
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        node0 = yajl_string_load_tree(inParms:strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        Reset i0;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'thirdParty';
              dsIn.thirdParty = YAJL_get_string(val);
           When key = 'ssoUser';
              dsIn.ssoUser = YAJL_get_string(val);
           When key = 'ssoInfo';
              json = %scanrpl('""':'{':json:%len(json)-2);
              node1 = YAJL_object_find(node0:key);
              Reset i1;
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'name';
                    dsIn.ssoName = YAJL_get_string(val);
                 When key = 'password';
                    dsIn.ssoPassword = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = json + '}';
           Endsl;
        Enddo;
        json = json + '}';

        // Free up the space by removing the tree
        yajl_tree_free(node0);

        Exsr Validate;
       Endsr;

       //=================================================================
       // Process the information
       //=================================================================
       Begsr Process;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        dsSSSOP.SOparty = dsIn.thirdParty;
        dsSSSOP.SOssoUser = dsIn.ssoUser;
        Exec sql
         select * into :dsSSSOP from SSSOP
          where SOparty = :dsSSSOP.SOparty
           and SOssoUser = :dsSSSOP.SOssoUser;
        isUpdate = isValidFetch(sqlstt);
        dsSSSOP.SOuserName = dsIn.ssoName;
        dsSSSOP.SOuserInfo = dsIn.ssoPassword;
        dsSSSOP.SOuser = q1User;
        dsSSSOP.SOpgm  = q1Pgm;
        dsSSSOP.SOtime = sysTime6;
        dsSSSOP.SOdt   = sysDate7;
        If isUpdate;
           Exec sql
            update SSSOP
             set row = :dsSSSOP
             where SOparty = :dsSSSOP.SOparty
              and SOssoUser = :dsSSSOP.SOssoUser;
           If sqlstt = conSqlFileMismatch;
              Exsr zFileMismatch;
           Endif;
        Else;
           dsSSSOP.SOiUsr = dsSSSOP.SOuser;
           dsSSSOP.SOiPgm = dsSSSOP.SOpgm;
           dsSSSOP.SOiTim = dsSSSOP.SOtime;
           dsSSSOP.SOiDt  = dsSSSOP.SOdt;
           Exec sql
            insert into SSSOP
             values(:dsSSSOP);
           If sqlstt = conSqlFileMismatch;
              Exsr zFileMismatch;
           Endif;
        Endif;
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr Validate;
        If dsIn.thirdParty = *blank;
           AddErrorMessageJson('Third Party required.'
            :ioRtnCode);
        Endif;
        If dsIn.ssoUser = *blank;
           AddErrorMessageJson('SSO user required.'
            :ioRtnCode);
        Endif;
        If dsIn.ssoName = *blank;
           AddErrorMessageJson('SSO name required.'
            :ioRtnCode);
        Endif;
        If dsIn.ssoPassword = *blank;
           AddErrorMessageJson('SSO password required.'
            :ioRtnCode);
        Endif;
       Endsr;

       //========================================================
       // Process insert/update if a mismatch exists with data structur
       //========================================================
       Begsr zFileMismatch;
        If not %open(SSSOP);
           Open SSSOP;
        Endif;
        Exec SQL
         Select rrn(SSSOP) Into :decRRN From SSSOP
          where SOparty = :dsSSSOP.SOparty
           and SOssoUser = :dsSSSOP.SOssoUser;
        dsSSSOPdef = dsSSSOP;
        Select;
        When decRRN = *zero;
           Write pso0p dsSSSOPdef;
        Other;
           Chain decRRN pso0p;
           If %found;
              Update pso0p dsSSSOPdef;
           Endif;
        Endsl;
        If %open(SSSOP);
           Close SSSOP;
        Endif;
       Endsr;
     pSaveUserInfoSSO...
     p                 e
