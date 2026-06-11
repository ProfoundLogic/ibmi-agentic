     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL':'QC2LE':'CGIDEV2/TEMPLATE2') DECEDIT('0.')
     H*====================================================================
     H* Create on 09/24/2018 by c:62 for Project SY406
     H*====================================================================
     H* Description: Retrieves Darts Information
     H*
     H*
     H*====================================================================
     H* inParms -
     H*   key
     H*   user
     H*   diary date
     H*   team
     H*====================================================================
     H* Revisions:
     H*
     H*  01/07/22  C:62  Support: 100366(099)
     H*                  Correct remark code sql
     H*
     H*  12/09/21  C:33  Support: 99529
     H*                  Trim DARTS json (no comments)
     H*
     H* 10/18/21 C:33 Project: CN026
     H*               Include agent key types in selections.
     H*
     H* 07/07/21 c:62 Project WC2781
     H*               Add parameter to include Incoming Parms
     H*
     H*====================================================================
     Fsmupl     if   e           k disk    rename(pup01:pup00)
     Fsmsll     if   e           k disk    rename(psl01:psl00)
     Fsdcmla    if   e           k disk    rename(pcm01:pcm0a)
     Fsdail     if   e           k disk    rename(pai01:pai00)
     Fsmarl     if   e           k disk    rename(par01:par00)
     Fsmmdl     if   e           k disk    rename(pmd01:pmd00)
     Fstctl1    if   e           k disk

     D PSDS          ESDS                  extname(SZQ1P)

     D* this size needs to change
     D array           s               a   len(32767)
     d inJson          s               a   len(2000000) varying
     D json            s               a   len(8500000) varying

     d maxProdSub      c                   3
     d maxRmCd         c                   9999
     d aryProdSub      ds                  inz dim(maxProdSub) qualified
     d  sub                          10
     d aryRmkCode      ds                  inz dim(maxRmCd) qualified
     d  rmCd                          5
     d  beginsW                       1
     d dsProdSub       ds                  inz likeds(aryProdSub)
     d dsRmkCode       ds                  inz likeds(aryRmkCode)

     d intCo#          s              3  0
     d intUserCo#      s              3  0
     d isAvailable     s              1n   inz('1')
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     d isIncludeAttachments...
     d                 s              1n   inz
     d isIncludeBody...
     d                 s              1n   inz
     d isIncludeHistory...
     d                 s              1n   inz
     d***  Begin Add  ***  10/18/21  ***************
     d isShowTypeAgent...
     d                 s              1n   inz('0')
     d***  End   Add  ***  10/18/21  ***************
     d isShowTypeClaims...
     d                 s              1n   inz('0')
     d isShowTypePolicy...
     d                 s              1n   inz('0')
     d group#          s              3  0
     d groupError      s               n   Inz(*Off)
     d strRmkDate      s              8
     d rmkDate7        s                   Like(UPdt)
     d rmkDate8        s              8  0
     d sequence#       s              5  0
     d strAuthority    s             10a
     d strCo#          s              3a
     d strCvtPgm       s             10    inz
     d strEnv          s              4a
     d strMsg          s            500a   varying
     d strRoleKeys     s           1000a   varying inz
     d strRolesJson    s          32000    varying inz
     d strSql          s               a   len(10000) varying inz
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     d dateError       s               n   Inz(*Off)
     d comma           s              1    Inz(' ')
     d i               s              5  0 Inz(0)

     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')
     D sqlStmtStr      s            400a
     D moreRows        s               n   Inz(*On)
     D clientID        s              3    inz('000')

     D docNode         s                   like(yajl_val)
     D i0              s             10i 0 inz
     D i1              s             10i 0 inz
     D i2              s             10i 0 inz
     D i3              s             10i 0 inz
     D key             s             50a   varying
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     D val             s                   like(yajl_val)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy wPrGroup
      /include yajl_h

      /copy CGIDEV2/qrpglesrc,prototypeb
      /copy CGIDEV2/qrpglesrc,usec

     dBuildFormat      pr                  ExtPgm('SRKCVT1')
     d outPgm                        10    const
     d ioKey                         79
     d inName                        79
     d inRtCd                         1
     d inData1                       79
     d inData2                       79

     dBuildFormatName  pr                  ExtPgm(strCvtPgm)
     d outPgm                        10    const
     d outKey                        40    const
     d inName                        79    const
     d inData1                       79
     d inData2                       79

     dCR001R           pr                  ExtPgm('CR001R')
     d outPgm                        10    const
     d outKey                        40    const
     d inName                        79    const
     d inData1                       79    const
     d inData2                       79    const

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      // Get Clob Data
     d GetClob         pr                  extpgm('DA003R')
     d   inKey                       40
     d   inSeq#                       5  0
     d   inAltSeq#                    5  0
     d   inPosition                  15  0
     d   inLength                    15  0
     d   outClob                  32000a   varying

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

      *entry
     DRequestPgm       pr                  ExtPgm('SIDARTS')
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

       Exsr PgmStart;

       Select;
       When inRequest = 'GetRemark';
          GetRemark();
       When inRequest = 'ListRemarks';
          ListRemarks();
       When inRequest = 'ListRemarksbyKey';
          ListRemarks();
        //ProcDartsListByKey();
       When inRequest = 'ListRemarksbyUser';
          ListRemarks();
        //ProcDartsListByUser();
       When inRequest = 'RemarksInformation';
        //ProcSingleDartsInfo();
          Exsr BuildDeprication;
       Other;
          Exsr BuildStub;
       EndSl;

       outJSON = json;

       *inlr = *on;

       //==============================================================
       // Build deprication results
       //==============================================================
       Begsr BuildDeprication;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') no longer used.');
       Endsr;

       //==============================================================
       // Build stub results
       //==============================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
       Endsr;

       //--------------------------------------------------------
       Begsr GetRoleInformation;
       //--------------------------------------------------------
        //Retrieve Roles
        strRolesJson = zhbgetvar('roles');
        node1 = yajl_string_load_tree(strRolesJson:strMsg);
        If strMsg <> *blanks;
         //Leavesr;
        Endif;
        Reset i1;
        Dow YAJL_array_loop(node1:i1:val);
           strRoleKeys += ',''' + YAJL_get_string(val) + '''';
        Enddo;
        If %len(strRoleKeys) >= 1;
           strRoleKeys = %scanrpl(',':'':strRoleKeys:1:1);
        Else;
           // No roles to process
         //***  Begin Add  ***  10/18/21  ***************
           isShowTypeAgent = *on;
         //***  End   Add  ***  10/18/21  ***************
           isShowTypeClaims = *on;
           isShowTypePolicy = *on;
        Endif;
        strSql = 'select distinct(RLprodSub) ' +
         'from SIRLPPRD ' +
         ' where RLco# = ' + %char(intCo#) +
         '  and RLkey in (' + strRoleKeys + ')' +
         '  and RLproduct = ''iDARTS''';
        Exec sql close cursorProdSub;
        Exec sql prepare sqlProdSub from :strSql;
        Exec sql declare cursorProdSub cursor for sqlProdSub;
        Exec sql open cursorProdSub;
        Reset i1;
        Dou not IsValidFetch(sqlstt);
           Exec SQL fetch next from cursorProdSub into :dsProdSub;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           When dsProdSub.sub = *blanks;
            //***  Begin Add  ***  10/18/21  ***************
              isShowTypeAgent = *on;
            //***  End   Add  ***  10/18/21  ***************
              isShowTypeClaims = *on;
              isShowTypePolicy = *on;
         //***  Begin Add  ***  10/18/21  ***************
           When dsProdSub.sub = 'Agent';
              isShowTypeAgent = *on;
         //***  End   Add  ***  10/18/21  ***************
           When dsProdSub.sub = 'Claims';
              isShowTypeClaims = *on;
           When dsProdSub.sub = 'Policy';
              isShowTypePolicy = *on;
           Endsl;
           i1 += 1;
           aryProdSub(i1) = dsProdSub;
        Enddo;
        If %len(strRoleKeys) < 1;
           strRoleKeys = '''''';
        Endif;
        //*** Begin Chg *** 01/07/22 **************************************
        //*** strSql = 'with countRMK as ' +
        //***  '(select count(distinct(RLkey)) as keyCount ' +
        //***  '  from SIRLPRMK ' +
        //***  '   where RLco# = ' + %char(intCo#) +
        //***  '    and RLkey in (' + strRoleKeys + ')) ' +
        //***  'select RLrmCd, RLbeginW ' +
        //***  ' from SIRLPRMK ' +
        //***  ' where RLco# = ' + %char(intCo#) +
        //***  '  and RLkey in ' +
        //***  '   (case ' +
        //***  '    when (select keyCount from countRMK) > 0 ' +
        //***  '     then (select distinct(RLkey) from SIRLPRMK ' +
        //***  '            where RLco# = ' + %char(intCo#) +
        //***  '             and RLkey in (' + strRoleKeys + ')) ' +
        //***  '    else (select distinct(RLkey) from SIRLPRMK ' +
        //***  '           where RLco# = ' + %char(intCo#) +
        //***  '            and RLkey in ('' '')) end)';
        strSql = 'with countRMK as ' +
         '(select count(distinct(RLkey)) as keyCount ' +
         '  from SIRLPRMK ' +
         '   where RLco# = ' + %char(intCo#) +
         '    and RLkey in (' + strRoleKeys + ')) ' +
         'select RLrmCd, RLbeginW ' +
         ' from SIRLPRMK a ' +
         ' where RLco# = ' + %char(intCo#) +
         '  and RLkey in ' +
         '   (case ' +
         '    when (select keyCount from countRMK) > 0 ' +
         '     then (select distinct(b.RLkey) from SIRLPRMK b ' +
         '            where b.RLco# = ' + %char(intCo#) +
         '              and b.RLkey = a.RLkey ' +
         '              and b.RLkey in (' + strRoleKeys + ')) ' +
         '    else (select distinct(RLkey) from SIRLPRMK ' +
         '           where RLco# = ' + %char(intCo#) +
         '            and RLkey in ('' '')) end)';
        //*** End   Chg *** 01/07/22 **************************************
        Exec sql close cursorRmkCode;
        Exec sql prepare sqlRmkCode from :strSql;
        Exec sql declare cursorRmkCode cursor for sqlRmkCode;
        Exec sql open cursorRmkCode;
        Reset i1;
        Dou not IsValidFetch(sqlstt);
           Exec SQL fetch next from cursorRmkCode into :dsRmkCode;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           Endsl;
           i1 += 1;
           aryRmkCode(i1) = dsRmkCode;
        Enddo;
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        Clear json;
        strAuthority = ioRtCd;
        Clear ioRtCd;
        Chain (inUser) pup00;
        intCo# = UPco#;
        intUserCo# = intCo#;
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';
        Exsr GetRoleInformation;
       Endsr;

      /End-free
       //===================================================================
       // Build Single Remark
       //===================================================================
     pBuildRemarkDetails...
     p                 b
     dBuildRemarkDetails...
     d                 pi
     d inKey                         40    const
     d inSeq#                         5  0 const

     ddecAltSeq#       s              5  0
     ddecAttachCount   s             10i 0 inz
     ddecCount         s             15  0
     ddecLength        s             15  0 inz(32000)
     ddecPosition      s             15  0 inz(1)
     dintSeq#          s             10i 0 inz
     djsonAttach       s               a   inz len(10000) varying
     djsonBody         s               a   inz len(1000000) varying
     dstrClob          s               a   inz len(32000) varying
     dstrData1         s             79    inz
     dstrData2         s             79    inz
     dstrFormat        s            500    inz varying
     dstrKey           s             79    inz
     dstrName          s             79    inz
     dstrRmkCodeDesc   s             40    inz varying
     dstrRtCd          s              1    inz
     dstrSummCodeDesc  s             80    inz varying

     ddsSDCMP        e ds                  extname(SDCMP) qualified inz
     ddsSMARP        e ds                  extname(SMARP) qualified inz
     ddsSMMDP        e ds                  extname(SMMDP) qualified inz

      /Free
       Exsr BuildSQL;
       Exsr BuildJson;
       Return;
       //=================================================================
       // Create output json
       //=================================================================
       Begsr BuildJson;
        Select;
        When ioRtCd = 'Error';
           Leavesr;
        When not IsAvailableRemark(dsSDCMP.CMpNam:dsSDCMP.CMrmCd);
           If isAvailable;
              AddErrorMessageJson(
               'Remark(s) not returned due to restrictions.');
              ioRtCd = 'Info Only';
              isAvailable = *off;
           Endif;
           Leavesr;
        Endsl;
        Exsr BuildJsonAttachments;
        Exsr BuildJsonBody;
        Exsr BuildJsonFormat;
        json += '{';
         json += '"details":{';
          json += '"dartsKey":{';
           json += BuildJsonNode('type':dsSDCMP.CMcode) + ',';
           json += BuildJsonNode('key':dsSDCMP.CMkey) + ',';
           json += BuildJsonNode('formatted':strFormat) + ',';
          json = %scanrpl(',':'':json:%len(json)-1);
          json += '},';
          json += BuildJsonNode('sequence':%char(dsSDCMP.CMseq#)) + ',';
          json += BuildJsonNode('date':%char(dsSDCMP.CMcDat):'date') + ',';
          json += '"remarkCode":{';
           json += BuildJsonNode('code':dsSDCMP.CMrmCd) + ',';
           json += BuildJsonNode('description':strRmkCodeDesc) + ',';
          json = %scanrpl(',':'':json:%len(json)-1);
          json += '},';
          json += BuildJsonNode('numberOfattachments':%char(decAttachCount))
           + ',';
         json = %scanrpl(',':'':json:%len(json)-1);
         json += '},';
         json += '"diary":{';
          json += BuildJsonNode('workItem':dsSDCMP.CMdiFl) + ',';
          json += BuildJsonNode('completionDate':%char(dsSDCMP.CMdCDt):'date')
           + ',';
          json += BuildJsonNode('user':dsSDCMP.CMuser) + ',';
         json = %scanrpl(',':'':json:%len(json)-1);
         json += '},';
         json += BuildJsonNode('subject':%trim(dsSDCMP.CMcomm)) + ',';
         json += jsonBody;
         Exsr BuildJsonHistory;
         json += jsonAttach;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
        //Exsr BuildStub;
       Endsr;

       //=================================================================
       // Create output json - attachments
       //=================================================================
       Begsr BuildJsonAttachments;
        jsonAttach = '"attachments":[';
        If dsSDCMP.CMuncN <> *blank;
           decAttachCount += 1;
           jsonAttach += '{';
            jsonAttach += BuildJsonNode('documentPath'
             :%trim(dsSDCMP.CMuncN)) + ',';
            jsonAttach += BuildJsonNode('description':strRmkCodeDesc) + ',';
           jsonAttach = %scanrpl(',':'':jsonAttach:%len(jsonAttach)-1);
           jsonAttach += '},';
        Endif;
        Dou not IsValidFetch(sqlStt);
           Exec SQL fetch next from cursorSMMDP into :dsSMMDP;
           Select;
           When not IsValidFetch(sqlStt);
              Leave;
           Endsl;
           decAttachCount += 1;
           jsonAttach += '{';
            jsonAttach += BuildJsonNode('documentPath':dsSMMDP.MDuncP) + ',';
            jsonAttach += BuildJsonNode('description':dsSMMDP.MDoDsc) + ',';
           jsonAttach = %scanrpl(',':'':jsonAttach:%len(jsonAttach)-1);
           jsonAttach += '},';
        Enddo;
        jsonAttach = %scanrpl(',':'':jsonAttach:%len(jsonAttach)-1);
        jsonAttach += '],';
        If not isIncludeAttachments;
           Reset jsonAttach;
        Endif;
       Endsr;

       //=================================================================
       // Create output json - body
       //=================================================================
       Begsr BuildJsonBody;
        Reset jsonBody;
        If not isIncludeBody;
           Leavesr;
        Endif;
        jsonBody = '"body":"';
        decAltSeq# = 0;
        For decPosition = 1 to %size(jsonBody) by decLength;
           GetClob(dsSDCMP.CMkey:dsSDCMP.CMseq#:decAltSeq#:decPosition:decLength
            :strClob);
           jsonBody += %trim(RmvExceptJSON(strClob));
        Endfor;
        jsonBody += '",';
       Endsr;

       //=================================================================
       // Create output json - formatted
       //=================================================================
       Begsr BuildJsonFormat;
        strKey = dsSDCMP.CMkey;
        Reset strName;
        Reset strData1;
        Reset strData2;
        Monitor;
           BuildFormat(dsSDCMP.CMpNam:strKey:strName:strRtCd:strData1:strData2);
        On-error;
           AddErrorMessageJson('Formatting failed.');
        Endmon;
        strFormat  = %trim(strKey);
        If strName <> *blanks;
           strFormat += ' <br /> ' + %trim(strName);
        Endif;
        If strData1 <> *blanks;
           strFormat += ' <br /> ' + %trim(strData1);
        Endif;
        If strData2 <> *blanks;
           strFormat += ' <br /> ' + %trim(strData2);
        Endif;
       Endsr;

       //=================================================================
       // Create output json - history
       //=================================================================
       Begsr BuildJsonHistory;
        If not isIncludeHistory;
           Leavesr;
        Endif;
        json += '"history":[';
        Dou not IsValidFetch(sqlStt);
           Exec SQL fetch next from cursorSMARP into :dsSMARP;
           Select;
           When not IsValidFetch(sqlStt);
              Leave;
           Endsl;
           json += '{';
            json += BuildJsonNode('note':dsSMARP.ARcm75) + ',';
            json += BuildJsonNode('user':dsSMARP.ARuser) + ',';
            json += BuildJsonNode('date':%char(dsSMARP.ARcDat):'date') + ',';
            json += BuildJsonNode('time':%char(dsSMARP.ARtime)) + ',';
           json = %scanrpl(',':'':json:%len(json)-1);
           json += '},';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '],';
       Endsr;

       //=================================================================
       // Build SQL
       //=================================================================
       Begsr BuildSQL;
        Exec sql
         select * into :dsSDCMP
          from SDCMP
          where CMkey = :inKey and CMseq# = :inSeq#;
        If not IsValidFetch(sqlstt);
           AddErrorMessageJson(
            'Remark not retrieved for requesting information.':ioRtCd);
           Leavesr;
        Endif;
        // Attachments
        strSql = 'select * from SMARP ' +
         'where ARkey = ''' + inKey + ''' and ARseq# = ' + %char(inSeq#) +
         ' order by ARkey, ARseq#, ARaSq#';
        Exec sql close cursorSMARP;
        Exec sql prepare sqlSMARP from :strSql;
        Exec sql declare cursorSMARP cursor for sqlSMARP;
        Exec sql open cursorSMARP;
        // History
        strSql = 'select * from SMMDP ' +
         'where MDkey = ''' + inKey + ''' and MDseq# = ' + %char(inSeq#) +
         ' order by MDkey, MDseq#, MDtrDt, MDtrTm';
        Exec sql close cursorSMMDP;
        Exec sql prepare sqlSMMDP from :strSql;
        Exec sql declare cursorSMMDP cursor for sqlSMMDP;
        Exec sql open cursorSMMDP;
        // Remark Code
        Exec sql
         select CTdesc into :strRmkCodeDesc
          from STCTP
          where CTrmCd = :dsSDCMP.CMrmCd;
        // Summary Code
        Exec sql
         select SMdesc into :strSummCodeDesc
          from STSMP
          where SMsmCd = :dsSDCMP.CMsmCd;
       Endsr;
      /End-free
     pBuildRemarkDetails...
     p                 e
       //===================================================================
       // Check Remark against role authorities
       //===================================================================
     pIsAvailableRemark...
     p                 b
     dIsAvailableRemark...
     d                 pi             1n
     d inPgm                         10    const
     d inCode                         5    const

     disValidCode      s              1n   inz
     dstrType          s              2    inz
      /Free
       Exec sql
        select KTkTyp into :strType
         from SMKTP
         where KTpNam = :inPgm;
       Select;
       //***  Begin Add  ***  10/18/21  ***************
       When isShowTypeAgent  and strType = 'WA';
       //***  End   Add  ***  10/18/21  ***************
       When isShowTypeClaims and strType = 'WC';
       When isShowTypePolicy and strType = 'WE';
       Other;
          Return *off;
       Endsl;
       For i1 = 1 to maxRmCd;
          Select;
          When aryRmkCode(i1).rmCd = *blank;
             isValidCode = i1 = 1;
             Leave;
          When aryRmkCode(i1).rmCd = inCode;
             isValidCode = *on;
             Leave;
          When aryRmkCode(i1).beginsW = 'Y' and
           aryRmkCode(i1).rmCd =
            %subst(inCode:1:%len(%trim(aryRmkCode(i1).rmCd)));
             isValidCode = *on;
             Leave;
          Endsl;
       Endfor;
       Return isValidCode;
      /End-free
     pIsAvailableRemark...
     p                 e
       //===================================================================
       // Get Remark
       //===================================================================
     pGetRemark...
     p                 b
     dGetRemark...
     d                 pi

     dintSeq#          s             10i 0 inz

     ddsIn             ds                  qualified inz
     d keyType                        2
     d key                           40
     d seq                            5

      /Free
       Exsr GetParms;
       Exsr BuildJson;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       Return;
       //=================================================================
       // Create output json
       //=================================================================
       Begsr BuildJson;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        isIncludeAttachments = *on;
        isIncludeBody        = *on;
        isIncludeHistory     = *on;
        json += ', "remark":';
        BuildRemarkDetails(dsIn.key:intSeq#);
        If ioRtCd = 'Info Only';
           json += '{}';
        Endif;
        json = %scanrpl(',':'':json:%len(json)-1);
        //Exsr BuildStub;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetRemark}');
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

        node0 = yajl_string_load_tree(inParms: errMsg500);
        If errMsg500 <> *blank;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtCd);
           AddErrorMessageJson(errMsg500:ioRtCd);
           Leavesr;
        Endif;
        json += ' "requestParms":{';
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyType';
              dsIn.keyType = YAJL_get_string(val);
           When key = 'key';
              dsIn.key = YAJL_get_string(val);
           When key = 'sequence';
              dsIn.seq = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
        yajl_tree_free(node0);
        Exsr Validate;
       Endsr;

       //=================================================================
       // Validate incoming parameters
       //=================================================================
       Begsr Validate;
        Monitor;
           intSeq# = %int(dsIn.seq);
        On-Error;
           AddErrorMessageJson('Sequence has non-numeric characters.':ioRtCd);
        Endmon;
       Endsr;
      /End-free
     pGetRemark...
     p                 e
       //===================================================================
       // List Remarks
       //===================================================================
     pListRemarks...
     p                 b
     dListRemarks...
     d                 pi

     dmaxRecords       c                   9999

     di1               s             10i 0 inz
     dintSeq#          s             10i 0 inz
     ddtRemark         s              7    inz('000000')
     ddt7Remark        s              7  0
     ddt8Remark        s              8  0

     DrsSDCMP          s                   sqltype(RESULT_SET_LOCATOR)
     ddsSDCMP        e ds                  extname(sdcmpDS1)
     d                                     qualified dim(maxRecords)

     ddsIn             ds                  qualified inz
     d keyType                        2
     d key                           40
     d user                          10
     d date                           8
     d team                           1    inz('N')
     d*** Begin Add *** 07/07/21 *******************************************
     d chrIncludeIncomingParms...
     d                 s              5a   Inz(' ')
     d indIncludeParms...
     d                 s               n
     d*** End   Add *** 07/07/21 *******************************************
      /Free
       Exsr GetParms;
       Exsr BuildSQL;
       Exsr BuildJson;
       //*** Begin Chg *** 07/07/21 ****************************************
       //*** json += ProcessErrorMessagesJson('*PRE');
       If ioRtCd = 'Error';
          json += ProcessErrorMessagesJson('*PRE');
       EndIf;
       //*** End   Chg *** 07/07/21 ****************************************
       json = %scanrpl(',':'':json:1:1);
       Return;
       //=================================================================
       // Create output json
       //=================================================================
       Begsr BuildJson;
        If ioRtCd = 'Error';
           Leavesr;
        Endif;
        isIncludeAttachments = *off;
        isIncludeBody        = *off;
        //*** Begin Chg *** 07/07/21 ***************************************
        //*** isIncludeHistory     = *off;
        If indIncludeParms;
           isIncludeHistory = *off;
        Else;
           isIncludeHistory = *on;
        EndIf;
        //*** End   Chg *** 07/07/21 ***************************************
        json += ', "remarksListing":[ ';
        For i1 = 1 to maxRecords;
           Select;
           When dsSDCMP(i1).CMkey = *blank;
              Leave;
           When dsIn.key <> *blank and dsIn.key <> dsSDCMP(i1).CMkey;
              Iter;
           Endsl;
           BuildRemarkDetails(dsSDCMP(i1).CMkey:dsSDCMP(i1).CMseq#);
        EndFor;
        json = %scanrpl(',':'':json:%len(json)-1);
        json = %trim(json) + ' ] ';
        //Exsr BuildStub;
       Endsr;

       //=================================================================
       // Build SQL
       //=================================================================
       Begsr BuildSQL;
        Exec SQL close dartsList;
        Select;
        When dsIn.user <> *blanks;
          Exec SQL call sgetdii(:dsIn.user,:dtRemark,:dsIn.team);
          Exec SQL associate result set locators
               (:rsSDCMP) with procedure sgetdii;
        Other;
          Exec SQL call sgetdri(:dsIn.key);
          Exec SQL associate result set locators
               (:rsSDCMP) with procedure sgetdri;
        Endsl;
        Exec SQL allocate dartsList cursor for result set :rsSDCMP;
        Exec SQL fetch dartsList for :maxRecords rows into :dsSDCMP;
        Exec SQL close dartsList;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        If json <> *blank;
           json += ',';
        Endif;
        json += BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListRemarks}');
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        node0 = yajl_string_load_tree(inParms: errMsg500);
        If errMsg500 <> *blank;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtCd);
           AddErrorMessageJson(errMsg500:ioRtCd);
           Leavesr;
        Endif;
        //*** Begin Add *** 07/07/21 ***************************************
        //* We need to get the parameter to determine if the request parms
        //* are to be returned.
        indIncludeParms = *On;
        Monitor;
           node = YAJL_object_find(node0:'includeIncomingParms');
           chrIncludeIncomingParms = YAJL_get_string(node);
        On-Error;
        EndMon;
        If chrIncludeIncomingParms = 'false';
           indIncludeParms = *Off;
        EndIf;
        //*** End   Add *** 07/07/21 ***************************************

        //*** Begin Chg *** 07/07/21 ***************************************
        //***json += ' "requestParms":{';
        If indIncludeParms;
           json += ' "requestParms":{';
        EndIf;
        //*** End   Chg *** 07/07/21 ***************************************
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           //*** Begin Chg *** 07/07/21 ************************************
           //***json += BuildJsonNode(key:YAJL_get_string(val)) + ',';
           If indIncludeParms;
              json += BuildJsonNode(key:YAJL_get_string(val)) + ',';
           EndIf;
           //*** End   Chg *** 07/07/21 ************************************
         //BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyType';
              dsIn.keyType = YAJL_get_string(val);
           When key = 'key';
              dsIn.key = YAJL_get_string(val);
           When key = 'dartsKey';
              dsIn.key = YAJL_get_string(val);
           When key = 'diaryUser';
              dsIn.user = YAJL_get_string(val);
           When key = 'remarkDate';
              dsIn.date = YAJL_get_string(val);
           When key = 'team';
              dsIn.team = YAJL_get_string(val);
           Endsl;
        Enddo;
        //*** Begin Chg *** 07/07/21 ************************************
        //***json = %scanrpl(',':'':json:%len(json)-1);
        //***json += '}';
        If indIncludeParms;
           json = %scanrpl(',':'':json:%len(json)-1);
           json += '}';
        EndIf;
        //*** End   Chg *** 07/07/21 ************************************
        yajl_tree_free(node0);
        Exsr Validate;
       Endsr;

       //=================================================================
       // Validate incoming parameters
       //=================================================================
       Begsr Validate;
        // Diary Information only validated if user is passed
        If dsIn.user <> *blank;
           Chain (dsIn.user) pup00;
           If not %found;
              AddErrorMessageJson('Invalid User ID.':ioRtCd);
           Endif;
           If dsIn.date <> *blank and dsIn.date <> '00000000';
              Monitor;
                 dt8Remark = %dec(dsIn.date:8:0);
              On-error;
                 AddErrorMessageJson('Invalid Numeric Data - Remark Date.'
                  :ioRtCd);
              Endmon;
              dt7Remark = DtCYMD(dt8Remark);
              dtRemark = %editc(dt7Remark:'X');
              If dt7Remark = 0;
                 AddErrorMessageJson('Invalid Remark Date.':ioRtCd);
              Endif;
           Endif;
           If dsIn.team <> *blank and dsIn.team <> 'Y' and dsIn.team <> 'N';
              AddErrorMessageJson('Invalid Team Flag.':ioRtCd);
           Endif;
        EndIf;
       Endsr;
      /End-free
     pListRemarks...
     p                 e
