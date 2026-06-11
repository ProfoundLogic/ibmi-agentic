     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H bnddir('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Revisions:
     H*
     H* 01/27/22  C:33  Support: 100695
     H*                -Return error messaging in JSON
     H*                -Use branded S/R Online name in message
     H*====================================================================

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying
     dclientID         s              3a
     dintCo#           s              3  0
     d strAuthority    s             10a
     dstrCo#           s              3a
     dstrSql           s           5000    varying inz

     dconSRBranding    c                   'Pay as You Go'

     d maxSize         c                   999
     d decMsg#         s             10i 0
     d errMsgArray     s            500    dim(maxSize) varying inz
     d strErrorMsg     s            500a   Varying

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d inJson          s               a   Len(2000000) varying
     d key             s             50a   varying
     d list            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d val             s                   Like(yajl_val)

     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0

     D PSDS          ESDS                  extname(SZQ1P)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJson
     D***  Begin Add  ***  01/27/22  *************
      /copy sPrModule
     D***  End   Add  ***  01/27/22  *************
      /copy sPrSecure
      /copy sPrSql
      /copy wPrUsrPrms
      /include yajl_h

     dGetClientID      pr                  ExtPgm('RTVCLTID')
     d clientID                       3a

     dGetObjLib        pr                   extpgm('RTVOBJLIB')
     d outObject                     10    const
     d outType                        7    const
     d inLibrary                     10

     dSROnline         pr                  extpgm('SRONLI')
     d outCo#                         3    const
     d outGroup                       3    const
     d outEmp#                        9    const
     d outDiv                         5    const
     d outYear                        3    const
     d outEffDt                       8    const
     d outMode                        1    const
     d inError                        1
     d ioData                       100

     dReceiveDtaQ      pr                  extpgm('QRCVDTAQ')
     d outDtaQ                       10    const
     d outLib                        10    const
     d inLength                       5p 0
     d inData                       300
     d outWait                        5p 0 const
     d outKeuOrder                    2    const
     d outKeyLength                   3p 0 const
     d outKey                        25    const
     d outSenderLen                   3p 0 const
     d outSenderInfo                 10    const

      *entry
     DRequestPgm       pr                  ExtPgm('WIALERTS')
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
       Select;
       When inRequest = 'GetSRAlerts';
          GetSRAlerts();
       Other;
          Exsr BuildStub;
       EndSl;
       *Inlr = *On;

       //--------------------------------------------------------
       // Build stub results
       //--------------------------------------------------------
       Begsr BuildStub;
        BuildGiantJSON(outJSON:'stub':%trim(inRequest) + ' (' +
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
       Endsr;
      /End-free

      //=============================================================
      // Get S/R Payroll Alerts
      //=============================================================
     PGetSRAlerts      b
     DGetSRAlerts      pr

     dconDataLen       c                   300

     dd1               s             10i 0 inz
     dd2               s             10i 0 inz
     ddecLength        s              5p 0
     ddecTest          s             10i 0 inz
     disPop            s              1n   inz
     dstrData          s            100    inz
     dstrError         s              1    inz('Y')
     dstrDtaQLib       s             10    inz
     dstrDtaQName      s             10    inz('SRODATQ')
     dstrPolicy        s            100    varying inz('Policy ')
     daryDiv           s              5    dim(999)

     ddsDataId         ds            25    qualified inz
     d fein                           9
     d time                           6
     d user                          10

     ddsDateRange      ds            16    qualified inz
     d from                           8
     d to                             8

     ddsEmpInfo        ds                  qualified inz
     d co#                            3
     d group                          3
     d emp#                           9
     d div                            5
     d year                           3

     ddsInfo4          ds           300    qualified inz
     d id                                  likeds(dsDataId)
     d emp#                           9
     d div                            5
     d year                           3
     d frequency                      1
     d spilt                          1
     d dateRange                           likeds(dsDateRange)
     d reported                       1
     d group                          3
     ddsInfo4Save      ds                  likeds(dsInfo4)

     ddsIn             ds                  qualified inz
     d fein                           9
     d emp#                           9
     d div                            5    dim(999)
     d divInt                         5  0 dim(999) inz(-99999)

      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        //***  Begin Add  ***  01/27/22  ******************
        Exsr DataQGet;
        //***  End   Add  ***  01/27/22  ******************
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //***  Begin Del  ***  01/27/22  ******************
        //Exsr DataQGet;
        //***  End   Del  ***  01/27/22  ******************
        Exsr DataQPush;
        Exsr DataQPop;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetSRAlerts}'));
       Endsr;

       //=================================================================
       // Get data queue information
       //=================================================================
       Begsr DataQGet;
        GetObjLib(strDtaQName:'*DTAQ':strDtaQLib);
        If strDtaQLib = *blanks;
         //***  Begin Chg  ***  01/27/22  ********************
         //AddErrorMessageJson('SR Online not configured correctly. ' +
         // 'Data queue doesn''t exist.':ioRtnCode);
         //Return;
           AddErrorMessageJson(GetModuleProperty('S/R Online':'Name') +
            ' not configured correctly. Data queue doesn''t exist.'
            :ioRtnCode);
           Leavesr;
         //***  End   Chg  ***  01/27/22  ********************
        EndIf;
        dsDataId.fein = dsIn.fein;
        dsDataId.time = %editc(Timeto6(SystemTime()):'X');
        dsDataId.user = q1User;
        strSql = 'select digits(EMco#), digits(EMfnd), digits(EMemp#), ' +
         'digits(EMdiv), digits(ELfyr) ' +
         'from WMEMP join WDELP on ' +
         ' (ELco#=EMco# and ELfnd=EMfnd and ELemp#=EMemp# and ELdiv=EMdiv) ' +
         'where EMfein = ' + dsIn.fein + ' ' +
         'order by EMco#, EMfnd, EMemp#, ELfyr, EMdiv';
        strSql = 'select distinct digits(EMco#), digits(EMfnd), ' +
         'digits(EMemp#), ''     '', ''   '' ' +
         'from WMEMP ' +
         'where EMfein = ' + dsIn.fein;
        If dsIn.emp# <> *blanks;
           strSql += ' and EMemp# = ' + dsIn.emp#;
        Endif;
        Exec Sql close cursorEmp;
        Exec Sql prepare sqlEmp from :strSql;
        Exec Sql declare cursorEmp cursor for sqlEmp;
        Exec Sql open cursorEmp;
       Endsr;

       //=================================================================
       // Read from the data queue
       //=================================================================
       Begsr DataQPop;
        json += ', "alertList":[';
        Select;
        When not isPop;
           json += ']';
           Leavesr;
        Endsl;
        Reset d1;
        Reset aryDiv;
        Dou decLength < conDataLen;
           decLength = conDataLen;
           Reset dsInfo4;
           ReceiveDtaQ(strDtaQName:strDtaQLib:decLength:dsInfo4:5:'EQ':25:
            dsDataId:0:' ');
           Select;
           When decLength < conDataLen;
              Iter;
           When dsInfo4.reported = 'Y';
              Iter;
           Endsl;
           Exsr ProcessPolicies;
        Enddo;
        Exsr ProcessDivArray;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']';
       Endsr;

       //=================================================================
       // Load the data queue
       //=================================================================
       Begsr DataQPush;
        Dou not IsValidFetch(sqlstt);
           Exec SQL fetch next from cursorEmp into :dsEmpInfo;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           Endsl;
           Reset strPolicy;
           Reset strError;
           strData = dsDataId;
           SROnline(dsEmpInfo.co#:dsEmpInfo.group:dsEmpInfo.emp#:dsEmpInfo.div:
            dsEmpInfo.year:'00000000':'4':strError:strData);
           strPolicy += dsEmpInfo.group + '-' + dsEmpInfo.emp#;
           If dsEmpInfo.div <> *blanks;
              strPolicy += '-' + dsEmpInfo.div;
           Endif;
           If dsEmpInfo.year <> *blanks;
              strPolicy += '-' + dsEmpInfo.year;
           Endif;
           Select;
           When strData <> '000000';
              isPop = *on;
           When strError = '1';
              AddErrorMessageJson(strPolicy +
               ' is set for ' + conSRBranding + ' but has no valid ACH.');
              Iter;
           When strError = '2';
              AddErrorMessageJson(strPolicy +
               ' is set for ' + conSRBranding + ' but is not flagged for' +
               ' online reporting.');
              Iter;
           When strError = '3';
              AddErrorMessageJson(strPolicy +
               ' is not set for ' + conSRBranding + '.');
              Iter;
           Endsl;
        Enddo;
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;

        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'insuredFein';
              dsIn.fein = YAJL_get_string(val);
           When key = 'emp#';
              dsIn.emp# = YAJL_get_string(val);
           When key = 'division';
              json = %scanrpl('""':'[':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              i1 = 0;
              Dow YAJL_array_loop(node1:i1:val);
                 dsIn.div(i1) = YAJL_get_string(val);
                 json += '"' + %trim(dsIn.div(i1)) + '",';
                 Monitor;
                    dsIn.divInt(i1) = %int(dsIn.div(i1));
                 On-error;
                 Endmon;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ']';
           Endsl;
        Enddo;
        json = json + '}';

        Exsr Validate;

       Endsr;

       //=================================================================
       // Process division array
       //=================================================================
       Begsr ProcessDivArray;
        If d1 < 1;
           Leavesr;
        Endif;
        json += '"Self Reported Payroll for Policy ' + dsInfo4Save.group + '-' +
         dsInfo4Save.emp# + '-' + dsInfo4Save.year + ' and Division(s)';
        For d2 = 1 to d1;
           json += ' ' + aryDiv(d2) + ',';
        Endfor;
        json = %scanrpl(',':' is due for the current billing period.", ':
         json:%len(json)-1);
        Reset d1;
        Reset aryDiv;
       Endsr;

       //=================================================================
       // Process policies
       //=================================================================
       Begsr ProcessPolicies;
        If (dsInfo4.group <> dsInfo4Save.group
         or dsInfo4.emp# <> dsInfo4Save.emp#
         or dsInfo4.year <> dsInfo4Save.year);
           Exsr ProcessDivArray;
           dsInfo4Save = dsInfo4;
        Endif;
        Select;
        //When dsIn.div  = *blank or %int(dsIn.div) = %int(dsInfo4.div);
        When dsIn.div(1) = *blank or %lookup(%int(dsInfo4.div):dsIn.divInt) > 0;
           d1 += 1;
           aryDiv(d1) = dsInfo4.div;
        Endsl;
       Endsr;

       //=================================================================
       // Validate parameters
       //=================================================================
       Begsr Validate;
        Monitor;
           decTest = %int(dsIn.fein);
        On-error;
           AddErrorMessageJson('insuredFein required.':ioRtnCode);
        Endmon;
       Endsr;
      /End-free
     PGetSRAlerts      e
