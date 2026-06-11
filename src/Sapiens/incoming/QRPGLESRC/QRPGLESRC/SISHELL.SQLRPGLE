     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('STBNDDIR')
     H*====================================================================
     H* Description:
     H*
     H*
     H*====================================================================
     H* inParms -
     H*   name:value
     H*
     H*====================================================================
     H* Revisions:
     H*
     H*
     H*====================================================================

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying

     d* PgmStart variables
     d clientID        s              3a
     d intCo#          s              3  0
     d intUserCo#      s              3  0
     d strAuthority    s             10a
     d strCo#          s              3a
     d strEnv          s              4a
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     d isEnvQuote      s              1n
     d isEnvLive       s              1n

     D PSDS          ESDS                  extname(SZQ1P)

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrSecure
      *copy sPrUsrPrms
      *copy wPrUsrPrms

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

      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;
       Exsr GetParms;
       Exsr BuildOutput;
       Select;
       When inRequest = '';
          // RequestProcedure();
       Endsl;
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        Exsr BuildStub;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
        //Exsr BuildExampleArray;
        //Exsr BuildExampleSub;
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
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        //intCo# = UserCo#(inUser);
        intUserCo# = intCo#;
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
