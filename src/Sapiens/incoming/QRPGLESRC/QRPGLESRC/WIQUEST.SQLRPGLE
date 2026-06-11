     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Description:
     H*  Web-services for Rating Questions
     H*
     H*====================================================================
     H* Revisions:
     H*
    H*  12/09/21  C:33  Support: 99529                                   *
    H*                  ListQuestions update %trim                       *
     H*====================================================================

     Darray            s               a   len(32767)
     DclientId         s              3a
     Di1               s             10i 0 inz
     Di2               s             10i 0 inz
     Di3               s             10i 0 inz
     Di4               s             10i 0 inz
     DinJson           s               a   Len(2000000) varying
     DintCo#           s              3  0
     Djson             s               a   len(8500000) varying
     Dkey              s             50a   varying
     Dnode1            s                   Like(Yajl_Val)
     Dnode2            s                   Like(Yajl_Val)
     Dnode3            s                   Like(Yajl_Val)
     Dnode4            s                   Like(Yajl_Val)
     d strAuthority    s             10a
     DstrCo#           s              3a
     DstrErrorMsg      s            500a   Varying
     DsysDate          s               d
     DsysDate7         s              7  0
     DsysTime6         s              6  0
     Dval              s                   Like(yajl_val)

     DaryCodes         s              5    dim(9999) inz
     DdsExposureInfo   ds                  qualified inz
     D state                          2a
     D rateSet                        3a
     D codes                          5    dim(9999)
     DaryExposureFilter...
     D                 ds                  likeds(dsExposureInfo) dim(100) inz
     DnullExposureInfo...
     D                 s                   like(dsExposureInfo) inz

     DdsGlobal         ds                  qualified inz
     D app#                           9  0 inz
     D group                          3  0 inz
     D employer                       9  0 inz
     D year                           3  0 inz
     D showResponses                  1n   inz
     D requiredOnly                   1a   inz
     D template                      40a   inz varying

     D PSDS          ESDS                  extname(SZQ1P)

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJson
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy wPrUsrPrms
      /include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('WIQUEST')
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
       Select;
       When inRequest = 'ListQuestions';
          ListQuestions();
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

       //=================================================================
       // Get start up information
       //=================================================================
       Begsr PgmStart;
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

       //=================================================================
     PBuildQuestionInfo...
     P                 b
     DBuildQuestionInfo...
     D                 pi
     DinId#                           7  0 const
     DinChild                         1n   const options(*nopass)

     Di1               s             10i 0 inz
     Di2               s             10i 0 inz
     DisChild          s              1n   inz
     DisEofChildren    s              1n   inz
     DisEofWIQUPACD    s              1n   inz
     DisEofWIQUPEXP    s              1n   inz
     DstrDesc          s            100a   varying inz
     DstrRateSet       s              3a   inz
     DstrRequired      s              1a   inz
     DstrSQL           s           5000a   varying inz(' ')
     DstrSQLWhere      s           5000a   varying inz(' ')
     DstrState         s              2a   inz

     DdsChildren       ds                  qualified inz
     D id#                            7  0
     D operator                      25
     D operand                       25
     DdsChild          ds                  likeds(dsChildren) dim(10)
     DdsWIQUP        e ds                  extname(WIQUP)    qualified inz
     DdsWIQUPacd     e ds                  extname(WIQUPACD) qualified inz
     DdsWIQUPexp     e ds                  extname(WIQUPEXP) qualified inz
     DdsWIQUPscd     e ds                  extname(WIQUPSCD) qualified inz
      /Free
       If %parms >= 2;
          isChild = inChild;
       Endif;

       Exec SQL select * into :dsWIQUP
        from WIQUP
        where QUco# = :intCo# and QUid# = :inId#
        fetch first row only;

       Reset strRequired;
       Exec SQL select 'Y' into :strRequired
        from WITMPQST
        where TMco# = :intCo# and TMquestionId# = :inId#
         and TMname = :dsGlobal.template;
       If dsWIQUP.QUtype = 'Exposure Code';
          strRequired = 'Y';
       Endif;

       Select;
       When isChild;
       When dsGlobal.requiredOnly = 'Y' and strRequired <> 'Y';
          Return;
       Endsl;

       If not isChild;
          json += '{';
       Endif;
       json +=
        BuildJsonNode('questionType':%trim(dsWIQUP.QUtype)) + ',' +
        BuildJsonNode('code':%trim(dsWIQUP.QUcode)) + ',' +
        BuildJsonNode('id':%char(dsWIQUP.QUid#)) + ',' +
        BuildJsonNode('sequence':%char(dsWIQUP.QUseq#)) + ',' +
        BuildJsonNode('status':dsWIQUP.QUstat) + ',' +
        BuildJsonNode('question':%trim(dsWIQUP.QUquestion)) + ',' +
        BuildJsonNode('answerType':%trim(dsWIQUP.QUanswer)) + ',' +
        BuildJsonNode('required':strRequired) + ',';
       Select;
       When dsWIQUP.QUtype = 'ACORD';
          Exsr BuildSqlWIQUPACD;
       When dsWIQUP.QUtype = 'Exposure Code';
          Exsr BuildSqlWIQUPEXP;
       When dsWIQUP.QUtype = 'Schedule Rating';
          Exsr BuildSqlWIQUPSCD;
       Other;
          json = %scanrpl(',':'':json:%len(json)-1);
          json += %trim(GetResponseJson(dsWIQUP.QUid#:*blanks:*zeros))+',';
       Endsl;
       Exsr BuildChildren;
       json = %scanrpl(',':'':json:%len(json));
       json += '},';

       //=================================================================
       // Build all children of parent
       //=================================================================
       Begsr BuildChildren;
        strSql = 'select QUid#, QUprntOpr, QUprntOpd ' +
         ' from WIQUP ' +
         ' where QUco# = ' + %char(intCo#) +
         ' and QUparent = ' + %char(dsWIQUP.QUid#);
        Exec SQL close cursorChildren;
        Exec SQL prepare children from :strSql;
        Exec SQL declare cursorChildren cursor for children;
        Exec SQL open cursorChildren;
        Reset dsChild;
        json += '"children": [';
        For i1 = 1 to 10;
           Exec SQL fetch next from cursorChildren into :dsChildren;
           Select;
           When not IsValidFetch(sqlStt);
              isEofChildren = *on;
              Leave;
           Endsl;
           dsChild(i1) = dsChildren;
        Endfor;
        For i2 = 1 to i1-1;
           json += '{' +
            BuildJsonNode('operator':%trim(dsChild(i2).operator)) + ',' +
            BuildJsonNode('operand':%trim(dsChild(i2).operand)) + ',';
            BuildQuestionInfo(dsChild(i2).id#:*on);
        Endfor;
        json = %scanrpl(',':'':json:%len(json));
        json += '],';
        Exec SQL close cursorChildren;
       Endsr;

       //=================================================================
       // Build SQL to retrieve data - WIQUPACD
       //=================================================================
       Begsr BuildSqlWIQUPACD;
        strSql = 'select * ' +
         ' from WIQUPACD ' +
         ' where QUco# = ' + %char(intCo#) +
         ' and QUid# = ' + %char(dsWIQUP.QUid#);
        Exec SQL close cursorWIQUPACD;
        Exec SQL prepare questACD from :strSql;
        Exec SQL declare cursorWIQUPACD cursor for questACD;
        Exec SQL open cursorWIQUPACD;
        Exsr ProcessSqlWIQUPACD;
        Exec SQL close cursorWIQUPACD;
       Endsr;

       //=================================================================
       // Build SQL to retrieve data - WIQUPEXP
       //=================================================================
       Begsr BuildSqlWIQUPEXP;
        strSql = 'select * ' +
         ' from WIQUPEXP ' +
         ' where QUco# = ' + %char(intCo#) +
         ' and QUid# = ' + %char(dsWIQUP.QUid#) +
         ' order by QUco#, QUid#, QUexpCode';
        Exec SQL close cursorWIQUPEXP;
        Exec SQL prepare questEXP from :strSql;
        Exec SQL declare cursorWIQUPEXP cursor for questEXP;
        Exec SQL open cursorWIQUPEXP;
        Exsr ProcessSqlWIQUPEXP;
        Exec SQL close cursorWIQUPEXP;
       Endsr;

       //=================================================================
       // Build SQL to retrieve data - WIQUPSCD
       //=================================================================
       Begsr BuildSqlWIQUPSCD;
        strSql = 'select * ' +
         ' from WIQUPSCD ' +
         ' where QUco# = ' + %char(intCo#) +
         ' and QUid# = ' + %char(dsWIQUP.QUid#);
        Exec SQL close cursorWIQUSPCD;
        Exec SQL prepare questSCD from :strSql;
        Exec SQL declare cursorWIQUPSCD cursor for questSCD;
        Exec SQL open cursorWIQUPSCD;
        Exsr ProcessSqlWIQUPSCD;
        Exec SQL close cursorWIQUPSCD;
       Endsr;

       //=================================================================
       // Process records - WIQUPACD
       //=================================================================
       Begsr ProcessSqlWIQUPACD;
        Reset dsWIQUPACD;
        json += '"accordInfo": {';
        Exec SQL fetch next from cursorWIQUPACD into :dsWIQUPACD;
        If IsValidFetch(sqlStt);
           json +=
            BuildJsonNode('accord#':%char(dsWIQUPACD.QUacord#)) + ',' +
            BuildJsonNode('accord#FL':%char(dsWIQUPACD.QUacord#FL));
        Endif;
        json += '}' + %trim(GetResponseJson(dsWIQUP.QUid#:*blanks:*zeros))+',';
       Endsr;

       //=================================================================
       // Process records - WIQUPEXP
       //=================================================================
       Begsr ProcessSqlWIQUPEXP;
        json += '"exposureCodeList": [';
        Reset isEofWIQUPEXP;
        Dou isEofWIQUPEXP;
           Exec SQL fetch next from cursorWIQUPEXP into :dsWIQUPEXP;
           Select;
           When not IsValidFetch(sqlStt);
              isEofWIQUPEXP = *on;
              Iter;
           Endsl;
           // List all requested states if the exposure is for all
           For i1 = 1 to 999;
              dsExposureInfo = aryExposureFilter(i1);
              aryCodes = aryExposureFilter(i1).codes;
              Select;
              When i1 = 1 and aryExposureFilter(i1) = nullExposureInfo;
              When aryExposureFilter(i1) = nullExposureInfo;
                 Leave;
              When dsWIQUPEXP.QUst <> *blank
               and dsWIQUPEXP.QUst <> aryExposureFilter(i1).state;
                 Iter;
              When aryCodes(1) <> *blanks
               and %lookup(%editc(dsWIQUPEXP.QUexpCode:'X'):aryCodes) < 1;
                 Iter;
              Endsl;
              strState = dsWIQUPEXP.QUst;
              Select;
              When i1 > 1 and dsWIQUPEXP.QUst = *blank;
                 Leave;
              When dsWIQUPEXP.QUst = *blank;
                 strState = aryExposureFilter(i1).state;
              Endsl;
           // Determine which Rate Set Code can be used
              Reset strRateSet;
              // Rate Set Code is passed in
              If strRateSet = *blanks
               and dsWIQUPEXP.QUrateSet <> *blank;
                 strRateSet = dsWIQUPEXP.QUrateSet;
              Endif;
              // Rate Set Code is defaulted in group
              If strRateSet = *blanks and dsGlobal.group <> *zero;
                 Exec sql
                  select RRDRAT into :strRateSet
                   from WDRRP
                   where RRFND = :dsGlobal.group and RRST = :strState
                    and RREFDT <= :sysDate7
                   order by RRCO#, RRFND, RREFDT desc
                   limit 1;
              Endif;
              // Rate Set Code is determined by state
              If strRateSet = *blanks;
                 Exec sql
                  select PRRTCD into :strRateSet
                   from WMPRP
                   where PRCO# = :intCo#
                    and PRSTAT = 'A' and PRCLAS = :dsWIQUPEXP.QUexpCode
                    and PREFDT <= :sysDate7
                    and PRRTCD in (select RSRTCD from WMRSP
                                    where RRJRST = :strState)
                   order by PRco#, PRclas, PRefdt desc
                   limit 1;
              Endif;
              // Rate Set Code is the latest in the file
              If strRateSet = *blanks;
                 Exec sql
                  select PRRTCD into :strRateSet
                   from WMPRP
                   where PRCO# = :intCo#
                    and PRSTAT = 'A' and PRCLAS = :dsWIQUPEXP.QUexpCode
                    and PREFDT <= :sysDate7
                   order by PRco#, PRclas, PRefdt desc
                   limit 1;
              Endif;
              // Get the class code description
              Reset strDesc;
              Exec sql
               select PRDESC Into :strDesc
                from WMPRP
                where PRCO# = :intCo#
                 and PRSTAT = 'A' and PRCLAS = :dsWIQUPEXP.QUexpCode
                 and PRefdt <= :sysDate7
                 and PRRTCD = :strRateSet
                order by PRco#, PRrtcd, PRclas, PRefdt desc
                limit 1;
              json += '{' +
               BuildJsonNode('rateSetCode':dsWIQUPEXP.QUrateSet) + ',' +
               BuildJsonNode('ratingState':strState) + ',' +
               '"exposure":{' +
                BuildJsonNode('code':%char(dsWIQUPEXP.QUexpCode)) + ',' +
                BuildJsonNode('description':strDesc) + '}' +
                %trim(GetResponseJson(dsWIQUP.QUid#:strState:
                 dsWIQUPEXP.QUexpCode))+
               '},';
           Endfor;
        Enddo;
        json = %scanrpl(',':'':json:%len(json));
        json += '],';
       Endsr;

       //=================================================================
       // Process records - WIQUPSCD
       //=================================================================
       Begsr ProcessSqlWIQUPSCD;
        Reset dsWIQUPSCD;
        json += '"scheduleRatingInfo": {';
        Exec SQL fetch next from cursorWIQUPSCD into :dsWIQUPSCD;
        If IsValidFetch(sqlStt);
           json +=
            BuildJsonNode('ratingState':dsWIQUPSCD.QUrateSt) + ',' +
            BuildJsonNode('factorByState':%char(dsWIQUPSCD.QUfactor));
        Endif;
        json += '}' +
         %trim(GetResponseJson(dsWIQUP.QUid#:dsWIQUPSCD.QUrateSt:*zeros))+',';
       Endsr;
      /End-free
     PBuildQuestionInfo...
     P                 e

       //=================================================================
     PGetResponseJson  b
     DGetResponseJson  pi           150a   varying
     D inId#                          7  0 const
     D inState                        2    const
     D inClass                        5  0 const
      *
     DoutJson          s                   like(GetResponseJson) inz
     DstrResponse      s             25    varying inz
      /Free
       Reset outJson;
       Reset strResponse;
       Exec SQL
        select APresp into :strResponse
         from WIAPPANS
         where APco# = :intCo#
          and APapp# = :dsGlobal.app#
          and APid#  = :inId#
          and APrtCd = :inState
          and APclas = :inClass;
       outJson += ',' + BuildJsonNode('response':strResponse);
       Return outJson;
      /End-free
     PGetResponseJson  e

       //=================================================================
     PGetTemplateInternal...
     P                 b
     DGetTemplateInternal...
     D                 pi            40    varying
     DinCo#                           3    const
     DinFnd                           3    const
     DinYear                          3    const
     DinEmp#                          9    const
     DinUserType                     25    const

     DaryTemplate      s             40    dim(2) varying inz
     DaryTemplateType  s             10    dim(2) varying inz
     DdecCo#           s              3s 0 inz
     DdecEmp#          s              9s 0 inz
     DdecFnd           s              3s 0 inz
     DdecYear          s              3s 0 inz
     DoutTemplate      s                   like(GetTemplateInternal) inz
     DstrStatus        s              1a   inz
     DstrUserType      s             25a   inz
     DstrTemplate      s               a   len(8500000) varying inz

     DGetTemplateExternal...
     D                 pr                  extpgm('WIVALID')
     DoutMode                         1a   const
     DoutCo#                          3s 0 const
     DoutFnd                          3s 0 const
     DoutYear                         3s 0 const
     DoutEmp#                         9s 0 const
     DoutUserType                    25a
     DinStatus                        1a
     DinTemplate                       a   len(8500000) varying
      /Free
       strUserType = inUserType;
       Monitor;
          decCo# = %int(inCo#);
          decFnd = %int(inFnd);
          decYear = %int(inYear);
          decEmp# = %int(inEmp#);
       On-error;
       Endmon;
       GetTemplateExternal(
        'C':decCo#:decFnd:decYear:decEmp#:strUserType:strStatus:strTemplate);
       Select;
       When strStatus = 'E';
          Return *blanks;
       Other;
          Exsr GetTemplateFromJson;
          Return outTemplate;
       Endsl;
       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetTemplateFromJson;
        Reset outTemplate;
        node1 = yajl_string_load_tree(strTemplate:strErrorMsg);
        If strErrorMsg = ' ';
           Reset i1;
           Dow YAJL_object_loop(node1:i1:key:val);
              Select;
              When key = 'templateList';
                 Reset outTemplate;
                 node2 = YAJL_object_find(node1:key);
                 Reset i2;
                 Dow YAJL_array_loop(node2:i2:node3);
                    Reset i3;
                    Dow YAJL_object_loop(node3:i3:key:val);
                       Select;
                       When key = 'template';
                          aryTemplate(i2) = YAJL_get_string(val);
                       When key = 'type';
                          aryTemplateType(i2) = YAJL_get_string(val);
                       Endsl;
                    Enddo;
                 Enddo;
              Endsl;
           Enddo;
        Endif;
        // The first returned template will be Quoting, if available; otherwise Pricing
        outTemplate = aryTemplate(1);
       Endsr;
      /End-free
     PGetTemplateInternal...
     P                 e

       //=================================================================
     PListQuestions    b
     DListQuestions    pi

     Da1               s             10i 0 inz
     DaryAllQuestions...
     D                 s              7  0 dim(99999) inz(-1)
     DisEofWIQUP       s              1n   inz
     DisListInactive   s              1n   inz
     DstrCode          s              5a
     DstrSQL           s           5000a   varying inz(' ')
     DstrSQLWhere      s           5000a   varying inz(' ')
     DstrTemplate      s            500a   varying inz

     DdsIn             ds                  qualified inz
     D questionType                  40a
     D requiredOnly                   1a
     D questionId                     7a
     D exposureFilter                 1n
     D company                        3a
     D group                          3a
     D year                           3a
     D employer                       9a
     D template                      40a
     D userType                      25a

     DdsEmpExp         ds                  qualified inz
     D rateSet                        3a
     D state                          2a
     D code                           5  0
     DdsEmpExpSave     ds                  likeds(dsEmpExp) inz
     DdsWIQUP        e ds                  extname(WIQUP)    qualified inz
     DdsWIQUPacd     e ds                  extname(WIQUPACD) qualified inz
     DdsWIQUPexp     e ds                  extname(WIQUPEXP) qualified inz

      /Free
       Exsr GetParms;
       If ioRtCd <> 'Error';
          Exsr BuildSqlWIQUP;
       Endif;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       //Exsr BuildStub;
       Return;

       //=================================================================
       // Build SQL to retrieve data - WIQUP
       //=================================================================
       Begsr BuildSqlWIQUP;
        strSqlWhere += 'where QUco# = ' + %char(intCo#);
        If dsIn.questionType <> *blanks;
           strSqlWhere += ' and QUtype = ''' + %trim(dsIn.questionType) + '''';
        Endif;
        If dsIn.questionId <> *blanks;
           strSqlWhere += ' and QUid# = ' + %trim(dsIn.questionId);
           isListInactive = *on;
        Else;
         //strSqlWhere += ' and QUparent = 0';
        Endif;
        Select;
        When dsGlobal.template <> *blanks and dsIn.requiredOnly = 'Y';
           // ACORD, Policy
           strSqlWhere += ' and (QUid# in ' +
            '(select distinct TMquestionId# ' +
            '  from WITMPQST ' +
            '  where TMco# = ' + %char(intCo#) +
            '   and TMNAME in (''' + %trim(dsGlobal.template) + '''))' +
            ' or QUtype not in (''ACORD'',''Policy''))';
        Endsl;
        If dsIn.exposureFilter;
           // Exposure Code
           strSqlWhere += ' and ((QUid# in ' +
            '(select distinct b.QUID# from WIQUPEXP b' +
            '  where (';
           For i1 = 1 to 999;
              dsExposureInfo = aryExposureFilter(i1);
              If aryExposureFilter(i1) = nullExposureInfo;
                 Leave;
              Endif;
              strSqlWhere += ' (b.QUST in (''  '',' +
               '''' + aryExposureFilter(i1).state + ''')';
              For i2 = 1 to 99999;
                 Select;
                 When aryExposureFilter(i1).codes(i2) = *blanks;
                    Leave;
                 When i2 = 1;
                    strSqlWhere += ' and b.QUEXPCODE in (';
                 Endsl;
                 strCode = aryExposureFilter(i1).codes(i2);
                 strSqlWhere += aryExposureFilter(i1).codes(i2) + ',';
              Endfor;
              If i2 > 1;
                 strSqlWhere = %scanrpl(',':'':strSqlWhere:%len(strSqlWhere)) +
                  ')';
              Endif;
              strSqlWhere += ') or';
           Endfor;
           strSqlWhere = %scanrpl('or':'':strSqlWhere:%len(strSqlWhere)-1)+')';
           strSqlWhere += ' order by b.QUID#))';
           strSqlWhere += ' or QUtype not in (''Exposure Code''))';
           // Schedule Rating
           strSqlWhere += ' and ((QUid# in ' +
            '(select distinct c.QUID# from WIQUPSCD c' +
            '  where c.QUrateSt in (''  '',';
           For i1 = 1 to 999;
              dsExposureInfo = aryExposureFilter(i1);
              If aryExposureFilter(i1) = nullExposureInfo;
                 Leave;
              Endif;
              strSqlWhere += '''' + aryExposureFilter(i1).state + ''',';
           Endfor;
           strSqlWhere = %scanrpl(',':'':strSqlWhere:%len(strSqlWhere)-1)+')';
           strSqlWhere += ' order by c.QUID#))';
           strSqlWhere += ' or QUtype not in (''Schedule Rating''))';
        Endif;
        If not isListInactive;
           strSqlWhere += ' and QUstat <> ''I''';
        Endif;
        strSql = 'select * ' +
         ' from WIQUP ' + strSqlWhere +
         ' order by QUco#, QUseq#, QUid#';
        Exec SQL close cursorWIQUP;
        Exec SQL prepare quest from :strSql;
        Exec SQL declare cursorWIQUP cursor for quest;
        Exec SQL open cursorWIQUP;
        Exsr ProcessSqlWIQUP;
        Exec SQL close cursorWIQUP;
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListQuestions}');
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        Reset dsGlobal;
        Reset dsIn;
        inJson = inParms;
        node1 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           Reset i1;
           json += '"requestParms" : {';
           Dow YAJL_object_loop(node1:i1:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'userType';
                 dsIn.userType = YAJL_get_string(val);
              When key = 'requiredOnly';
                 dsIn.requiredOnly = YAJL_get_string(val);
              When key = 'questionType';
                 dsIn.questionType = YAJL_get_string(val);
              When key = 'questionId';
                 dsIn.questionId = YAJL_get_string(val);
              When key = 'exposures';
                 Exsr GetParmsExposure;
              When key = 'company';
                 dsIn.company = YAJL_get_string(val);
              When key = 'group';
                 dsIn.group = YAJL_get_string(val);
                 Monitor;
                    dsGlobal.group =  %int(dsIn.group);
                 On-error;
                 Endmon;
              When key = 'year';
                 dsIn.year = YAJL_get_string(val);
                 Monitor;
                    dsGlobal.year =  %int(dsIn.year);
                 On-error;
                 Endmon;
              When key = 'employer';
                 dsIn.employer = YAJL_get_string(val);
                 Monitor;
                    dsGlobal.employer =  %int(dsIn.employer);
                 On-error;
                 Endmon;
              When key = 'app#';
                 Monitor;
                    dsGlobal.app# =  %int(YAJL_get_string(val));
                 On-error;
                 Endmon;
              When key = 'template';
                 dsIn.template = YAJL_get_string(val);
              Endsl;
           Enddo;
           json = json + '}';
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtCd);
        Endif;
        Monitor;
           intCo# = %int(dsIn.company);
        On-error;
        Endmon;
        dsGlobal.showResponses = *on;
        dsGlobal.requiredOnly = dsIn.requiredOnly;
        Select;
        When dsIn.template <> *blank;
           strTemplate = %trim(dsIn.template);
        When dsIn.employer <> *blank;
           strTemplate = GetTemplateInternal(
            dsIn.company:dsIn.group:dsIn.year:dsIn.employer:dsIn.userType);
           If strTemplate = *blank;
              AddErrorMessageJson('Template information not found for employer.'
               :ioRtCd);
              Leavesr;
           Else;
            //BuildGiantJson(json:'templateList':%trim(strTemplate));
           Endif;
        Endsl;
        dsGlobal.template = strTemplate;
        Select;
        When dsGlobal.app# <> *zero;
        When dsIn.employer <> *blank;
           Exec SQL
            select EMapp# into :dsGlobal.app#
             from WIEMP
             where EMco# = :dsIn.company
              and EMfnd  = :dsIn.group
              and EMfyr = :dsIn.year
              and EMemp# = :dsIn.employer;
        Endsl;
        Select;
        When dsIn.exposureFilter <> *on and dsIn.employer <> *blank;
           Exsr LoadEmployerExposures;
        Endsl;
       Endsr;

       //=================================================================
       // Break parms out from inParms - Exposure
       //=================================================================
       Begsr GetParmsExposure;
        json = %scanrpl('""':'[':json:%len(json)-1);
        node2 = YAJL_object_find(node1:key);
        Reset i2;
        Dow YAJL_array_loop(node2:i2:node3);
           json += '{';
           Reset i3;
           Dow YAJL_object_loop(node3:i3:key:val);
              dsIn.exposureFilter = *on;
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'rateSetCode';
                 aryExposureFilter(i2).rateSet = YAJL_get_string(val);
              When key = 'ratingState';
                 aryExposureFilter(i2).state = YAJL_get_string(val);
              When key = 'exposureCodeList';
                 json = %scanrpl('""':'[':json:%len(json)-1);
                 node4 = YAJL_object_find(node3:key);
                 Reset i4;
                 Dow YAJL_array_loop(node4:i4:val);
                    json += '"' + %trim(YAJL_get_string(val)) + '",';
                    aryExposureFilter(i2).codes(i4) =
                     %scanrpl(' ':'0':RightJstfy(YAJL_get_string(val):5):1:5);
                 Enddo;
                 json = %scanrpl(',':'],':json:%len(json));
              Endsl;
           Enddo;
           SortA %subarr(aryExposureFilter(i2).codes:1:i4-1);
           json = %scanrpl(',':'},':json:%len(json));
        Enddo;
        json = %scanrpl(',':'':json:%len(json));
        json += ']';
       Endsr;

       //=================================================================
       // Load employer exposures to filters
       //=================================================================
       Begsr LoadEmployerExposures;
        strSql = 'select distinct ELRTCD, RSJRST, EPCLAS ' +
         ' from WDELP ' +
         ' join WMRSP ' +
         '  on ELCO#=RSCO# and ELRTCD=RSRTCD ' +
         ' join WMEPP ' +
         '  on ELCO#=EPCO# and ELFND=EPFND and ELEMP#=EPEMP# and ELDIV=EPDIV' +
         '   and ELFYR=EPFYR ' +
         ' where ELCO#=' + dsIn.company + ' and ELFND=' + dsIn.group +
         '  and ELFYR=' + dsIn.year + ' and ELEMP#=' + dsIn.employer +
         '  and EPSTAT = ''A'' ' +
         ' order by ELRTCD, RSJRST';
        Exec SQL close cursorEmpExp;
        Exec SQL prepare sqlEmpExp from :strSql;
        Exec SQL declare cursorEmpExp cursor for sqlEmpExp;
        Exec SQL open cursorEmpExp;
        Reset i2;
        Reset i4;
        Dou not IsValidFetch(sqlstt);
           Exec sql fetch next from cursorEmpExp into :dsEmpExp;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           When dsEmpExp.rateSet <> dsEmpExpSave.rateSet
            and dsEmpExp.state <> dsEmpExpSave.state;
              i2 += 1;
              Reset i4;
           Endsl;
           dsEmpExpSave = dsEmpExp;
           i4 += 1;
           aryExposureFilter(i2).rateSet = dsEmpExp.rateSet;
           aryExposureFilter(i2).state = dsEmpExp.state;
           aryExposureFilter(i2).codes(i4) = %editc(dsEmpExp.code:'X');
           dsIn.exposureFilter = *on;
        Enddo;
        Exec SQL close cursorEmpExp;
       Endsr;

       //=================================================================
       // Process records - WIQUP
       //=================================================================
       Begsr ProcessSqlWIQUP;
        // Create a list of all questions to compare against children meeting criteria
        If dsIn.questionId = *blanks;
           Reset a1;
           Reset isEofWIQUP;
           Dou isEofWIQUP;
              Exec SQL fetch next from cursorWIQUP into :dsWIQUP;
              Select;
              When not IsValidFetch(sqlStt);
                 isEofWIQUP = *on;
                 Iter;
              Endsl;
              a1 += 1;
              aryAllQuestions(a1) = dsWIQUP.QUid#;
           Enddo;
           Exec SQL close cursorWIQUP;
           Exec SQL open cursorWIQUP;
        Endif;
        json += ', "questionList": [';
        Reset isEofWIQUP;
        Dou isEofWIQUP;
           Exec SQL fetch next from cursorWIQUP into :dsWIQUP;
           a1 = %lookup(dsWIQUP.QUparent:aryAllQuestions:1);
           Select;
           When not IsValidFetch(sqlStt);
              isEofWIQUP = *on;
              Iter;
           When %lookup(dsWIQUP.QUparent:aryAllQuestions:1) <> 0;
              Iter;
           Endsl;
           BuildQuestionInfo(dsWIQUP.QUid#:*off  );
        Enddo;
        json = %scanrpl(',':'':json:%len(json));
        json += ']';
       Endsr;

      /End-free
     PListQuestions    e
