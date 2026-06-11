     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H/TITLE SIHUBPRC - Interface request processor
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Portal interface request processor      *
     H*                                                               *
     H* CALLED BY: AJAX request thru HTTP server                      *
     H*                                                               *
     H* CALLS TO: Request handler process                             *
     H*                                                               *
     H* CREATED: 06/04/18  C:33  PROJECT: SY390                       *
     H*                                                               *
     H*    Compile option - DEFINE(ALLOW_DEBUG) should be removed     *
     H*                     when creating a deployment version        *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 12/14/21  C:33  Project: CN027                                *
     H*                 Remove user name from error message           *
     H*****************************************************************
      *
     D/copy sPrAtrium
     D/copy sPrJson
     D/copy sPrRunCmd
     D/copy sPrSql
     D/copy sPrString
     D/copy sPrUsrPrms
     D/include yajl_h

      /copy CGIDEV2/qrpglesrc,prototypeb
      /copy CGIDEV2/qrpglesrc,usec

      * API Declarations
      * Standard Out API
     D QtmhWrStout     PR                  ExtProc('QtmhWrStout')
     D   DataVar                  32767A   Options(*varsize) Const
     D   DataVarLen                  10I 0 Const
     D   ErrorCode                 8000A   Options(*varsize)

      * Error Data Structure
     D ErrCode         DS                  Qualified
     D   BytesProv                   10I 0 Inz(0)
     D   BytesAvail                  10I 0
      *
     d CallRequest     pr                  ExtPgm(strProgram)
     d   inRequest                  256
     d   inUser                      10
     d   ioParms                           like(sizeParm)
     d   outJSON                           like(sizeJSON)
     d   ioRtnCode                   10a
     d CallRequestTst  pr                  ExtPgm(strProgram)
     d   inParms                      9  0 const
     d   outJSON                  32690
     d   ioRtnCode                    1A
      *
     dCheckObj         pr                  extPgm('SRCHKOBJ')
     d inName                        10    const
     d inType                         7    const
     d inRtcd                         1
      *
     dCheckObjIFS      pr                  extPgm('CHKIFSOBJ')
     d inName                       256    const
     d outFound                       1n
      *
     dCopyDft          pr                  extPgm('CPYIFSC')
     d inObjFrom                   1024    const
     d inObjTo                     1024    const
     d outRtCd                        2
      *
     dRunCommand       pr                  extPgm('QCMDEXC')
     D inCmd                       2750    const
     d inCmdLen                      15  5 const
      *
     dSetInitLibl      pr                  extPgm('M0002C')
     d inApp                          3    const
     d inMetL                        10    const
     d inOVR1                        70    const
     d inOVR2                        70    const
     d inOVR3                        70    const
     d inOVR4                        70    const
     d inOVR5                        70    const
     d inRTCD                         1    const

     D psds          esds                  extname(szq1p)
     D daIFSOut        s               n   dtaara
     D CRLF            C                   x'0D25'

     D i0              s             10i 0 inz
     D i1              s             10i 0 inz
     D i2              s             10i 0 inz
     D i3              s             10i 0 inz
     D key             s             50a   varying
     d node0           s                   like(yajl_val)
     d node1           s                   like(yajl_val)
     d node2           s                   like(yajl_val)
     d node3           s                   like(yajl_val)
     d val             s                   Like(yajl_val)

     D dec#OfVars      s             10i 0
     D intCo#          s              3  0 inz
     D inJSON          s                   like(sizeJSON)
     D intLen          S              5  0
     D intPosE         S              5  0
     D intPosS         S              5  0
     D ioRtnCode       s             10a
     D isError         S               n   inz
     D isRolesRequired...
     D                 s              1n   inz
     D isTesting       S               n   inz('0')
     D isUseIFSIn      s               n   inz('0')
     D isUseIFSLog     s               n   inz('0')
     D isUseIFSOut     s               n   inz('0')
     D JSON            s                   like(sizeJSON)
     D sizeIO          s               a   len(8250000) varying
     D sizeJSON        s                   like(sizeIO)
     D sizeParm        s                   like(sizeIO)
     D strCmd          s           2750a   varying
     D strEnv          s              3
     D strEnvOld       s              3
     D strJob          s             28    inz
     D strJSON         s               a   len(8250000) inz
     d strMsg          s            500a   Varying
     D strParms        s                   like(sizeParm)
     D strParmsIfs     s               a   len(8250000) inz
     D strPathUser     s            256a
     D strRolesJson    s          32000    varying inz
     D strProgram      S             10    inz
     D strRequest      s            256
     D strRequestInfo  s               a   len(8250000) varying inz
     D strSavedQuery   s          32767    varying
     D strSQL          s          32000    varying inz
     D strUser         s             10
     D strUserId       s            256
     D strUserLibl     S           2750A   varying
     D strErrorMsg     s            256    varying inz
     D jumpCount       s              3  0 inz

     D dsJob           ds                  inz qualified
     D  number                        6
     D  name                         10
     D  user                         10

     D dsFatalError    ds
     D                               10i 0 inz(0)
     D                               10i 0 inz(0)

     DhubInfo         uds                  dtaara('SIHUBPRC') qualified inz
     D env                            3

      // IFS usage variables
     D fd              S             10I 0
     D flags           S             10U 0
     D mode            S             10U 0
     D Msg             S             50A
     D Len             S             10I 0

     D conDir          c                   '/www/portal/'
     D conRequest      c                   '/request.txt'

      /Free
       Exec SQL
        set option commit = *none;

       strCmd = 'CRTDTAARA DTAARA(QTEMP/SIHUBPRC) TYPE(*CHAR) LEN(100)';
       Monitor;
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
       In hubInfo;
       strEnvOld = hubInfo.env;

       Dow jumpCount<5;
          Select;
          When GetRequest();
          Other;
             isError = *on;
          Endsl;
          Exsr RtvValues;
          If not isError or isUseIFSIn;
             Exsr SetLibl;
             If isError = *on;
                Leave;
             Endif;
             strUser = GetUser(strUserId);
             strProgram = GetProgram(strRequest);
             intCo# = GetUserInfo(strUser);
             If isUseIFSIn;
                Exsr IfsGetRequest;
                strParms = strParmsIfs;
             Endif;
             Leave;
          Endif;
          If isError;
             strCmd = 'DLYJOB DLY(1)';
             RunCommand(strCmd:%len(strCmd));
             jumpCount=jumpCount+1;
          Endif;
       EndDo;
       // This will close service program files
       If strEnv <> strEnvOld or dsJob <> *blank;
          strCmd = 'RCLACTGRP ACTGRP(*ELIGIBLE)';
          Monitor;
             RunCmd(strCmd:%len(strCmd));
          On-error;
          Endmon;
       Endif;
       Exsr LogRequest;
       BuildHubJSON();
       // Write JSON to IFS
       If isUseIFSOut;
          Exsr IfsPutResults;
       Endif;

       hubInfo.env = strEnv;
       Out hubInfo;

       *InLr = *On;

       //=======================================================================
       // Retrieve the input parms from the IFS
       //=======================================================================
       Begsr IfsGetRequest;
        strPathUser = IfsSetup('Request');
        // Look in IFSIO_H for descriptions
        flags = O_RDONLY + O_TEXTDATA;
        fd = open(%trim(strPathUser):flags);
        Select;
        When fd < 0;
           isError = *on;
           strErrorMsg ='"rtnCode": "Unable to open IFS file: ' +
            %trim(strPathUser) + '."';
        Other;
           callp read(fd:%addr(strParmsIfs):%size(strParmsIfs));
           callp close(fd);
        Endsl;
       Endsr;

       //=======================================================================
       // Log the requested information to the IFS
       //=======================================================================
       Begsr IfsPutRequest;
        If not isUseIFSLog;
           Leavesr;
        Endif;
        strJSON = %trim(JSON) + CRLF;
        strPathUser = IfsSetup('RequestLog');
        // Look in IFSIO_H for descriptions
        flags = O_WRONLY + O_CREAT + O_TRUNC + O_TEXTDATA;
        mode =  S_IRUSR + S_IWUSR + S_IRGRP + S_IROTH;
        fd = open(%trim(strPathUser):flags:mode);
        Select;
        When fd < 0;
           strErrorMsg ='"rtnCode": "Unable to open Request Log IFS file: ' +
            %trim(strPathUser) + '."';
        Other;
           callp write(fd:%addr(strRequestInfo):%len(%trim(strRequestInfo)));
           callp close(fd);
        Endsl;
       Endsr;

       //=======================================================================
       // Load the results to the IFS
       //=======================================================================
       Begsr IfsPutResults;
        strJSON = %trim(JSON) + CRLF;
        strPathUser = IfsSetup('Results');
        // Look in IFSIO_H for descriptions
        flags = O_WRONLY + O_CREAT + O_TRUNC + O_TEXTDATA;
        mode =  S_IRUSR + S_IWUSR + S_IRGRP + S_IROTH;
        fd = open(%trim(strPathUser):flags:mode);
        Select;
        When fd < 0;
           isError = *on;
           strErrorMsg ='"rtnCode": "Unable to open IFS file: ' +
            %trim(strPathUser) + '."';
        Other;
           callp write(fd:%addr(strJSON):%len(%trim(strJSON)));
           callp close(fd);
        Endsl;
       Endsr;

       //=======================================================================
       // Log request for future analysis
       //=======================================================================
       Begsr LogRequest;
        Exsr IfsPutRequest;
        strSQL = 'create table ' + %trim(Q1pLib) + '/REQUEST_LOG ' +
         '(REQUEST_TIMESTAMP TIMESTAMP NOT NULL WITH DEFAULT,' +
         ' REQUEST           CLOB      NOT NULL WITH DEFAULT)';
        Exec SQL execute immediate :strSQL;
        strSQL = 'insert into ' + %trim(Q1pLib) + '/REQUEST_LOG ' +
         '(REQUEST_TIMESTAMP, REQUEST) ' +
         'values(current timestamp, ' +
         '''' + %trim(strRequestInfo) + '  ';
        // Ensure sql statement is closed
        %subst(strSQL:%len(strSQL)-1:2) = ''')';
        Exec SQL execute immediate :strSQL;
       Endsr;

       //=======================================================================
       // RtvValues - Retrieve Values from query string
       //=======================================================================
       Begsr RtvValues;
        Reset strRequestInfo;

        Monitor;
           In daIFSOut;
        On-error;
        Endmon;

        //Retrieve Request
        strRequest = zhbgetvar('request');
        strRequestInfo += 'request=' + %trim(strRequest);
        If strRequest = *blanks;
           isError = *on;
        Endif;

        //Retrieve User
        strUserId = zhbgetvar('user');
        strRequestInfo += '&user=' + %trim(strUserId);
        If strUserId = *blanks;
           isError = *on;
        Endif;

        //Retrieve Roles
        strRolesJson = zhbgetvar('roles');
        strRequestInfo += '&roles=' + strRolesJson;
        If strRolesJson = *blanks and isRolesRequired;
           isError = *on;
        Endif;

        //Retrieve Environment
        strEnv = zhbgetvar('env');
        strRequestInfo += '&env=' + %trim(strEnv);
        If strEnv = *blanks;
           strEnv = 'LIV';
        Endif;

        //Retrieve request parameters
        strParms = zhbgetvar('parms');
        strRequestInfo += '&parms=' + %trim(strParms);
        strParms = %scanrpl('*END':'':strParms);
        Select;
        When strParms = '{input:*IFS}';
           isUseIFSIn = *on;
           isUseIFSOut = *on;
        When daIFSout;
           isUseIFSOut = *on;
        Endsl;

        //Retrieve job information
        strJob = zhbgetvar('job');
        strRequestInfo += '&job=' + %trim(strJob);
        strJob = %scanrpl('*END':'':strJob);
        Select;
        When strJob <> *blanks;
           intPosS = 1;
           intPosE = %scan('/':strJob:intPosS);
           intLen = intPosE-intPosS;
           dsJob.number = %subst(strJob:intPosS:intLen);
           intPosS = intPosE+1;
           intPosE = %scan('/':strJob:intPosS);
           intLen = intPosE-intPosS;
           dsJob.user = %subst(strJob:intPosS:intLen);
           intPosS = intPosE+1;
           dsJob.name = %subst(strJob:intPosS);
        Other;
           Reset dsJob;
        Endsl;


       Endsr;

       //=======================================================================
       // SetLibl - Set library list for AJAX call
       //=======================================================================
       Begsr SetLibl;
        Monitor;
           Dou 1=1;
              Select;
              When dsJob <> *blank;
                 // Get job libl and set
                 strUserLibl = RtvUserLibl(dsJob.name:dsJob.user:dsJob.number);
                 If strUserLibl <> 'X' and strUserLibl <> ' ';
                    strCmd = 'CHGLIBL LIBL(' + %Trim(strUserLibl) + ')';
                    RunCommand(strCmd:%len(strCmd));
                    Leave;
                 Endif;
                 // Error default to LIV libl
                 strEnv = 'LIV';
              Endsl;
              // Set preset libl
              SetInitLibl(strEnv:' ':' ':' ':' ':' ':' ':' ');
           Enddo;
        On-error;
          // send error response
          isError = *on;
          strErrorMsg ='"rtnCode": "Error setting library list."';
        Endmon;
       Endsr;
      /End-Free

     P*===========================================================================
     P* Build JSON return string
     P*===========================================================================
     PBuildHubJSON     b
     DBuildHubJSON     pi
     D inCall#                       15  0 const options(*nopass)

     D decCall#        s             15  0 inz(1)
     D start           s             10i 0
     D len             s             10i 0
     D splitSize       c                   32000
       // In order to build a data processing program, it will have to
       //  accecpt three parameters:
       //  1. request - pass the request along to the processing program
       //  2. user - iSeries user profile
       //  3. parm list - this will be the parms needed to get the requested
       //                  data
       //               - if the data set is so large that it can not be
       //                  contained within a single JSON string, the data
       //                  should be truncated and parameters needed to
       //                  continue processing should be passed back here
       //  4. json string - this will be the data in JSON format (32690a)
       //  5. return code - this will return the results of the data program
       //     -> 'Sucess' - the data program completed normally
       //     -> 'more...' - the data program should be called again with
       //                     the paramters passed back
       //     -> generic code - any value can be passed back to send info
       //                        to the portal
      /Free
       If %parms >= 1 and inCall# <> *zero;
          decCall# = inCall#;
       Endif;
       Clear JSON;
       // Build JSON Output
       JSON = 'Content-Type: text/plain' + CRLF + CRLF;
       JSON = %trim(JSON) + '{ ';
       Select;
       When not CheckSecurity(strUserId:strUser);
          JSON = %trim(JSON) + strErrorMsg;
       When isError = *on;
          // send error response
          If strErrorMsg <> *Blanks;
             JSON = %trim(JSON) + ' ' + strErrorMsg;
          Else;
             JSON = %trim(JSON) + '"rtnCode": "Invalid Request - '
              + %Trim(strRequestInfo) + '"';
          Endif;
       Other;
          Monitor;
             Select;
             When isTesting;
              //CallRequestTst(000000007:inJSON:ioRtnCode);
             Other;
                CallRequest(strRequest:strUser:strParms:inJSON:ioRtnCode);
             Endsl;
             JSON = %trim(JSON) + '"rtnCode": "' + %trimr(ioRtnCode);
             If inJSON <> *blanks;
                JSON = %trim(JSON) + '", ' + %TrimR(inJSON);
             Endif;
          On-Error;
             JSON = %trim(JSON) + '"rtnCode": "Call failed: PGM='
              + %trim(strProgram) + ', parms='
              + %trim(RmvExceptJSON(strParms)) + '", '
              + '"job":"JOB(' + %editc(q1jobn:'X') + '/'
              + %trim(q1cUsr) + '/'+ %trim(q1job) + ')"';
          Endmon;
       Endsl;

       JSON = %trim(JSON) + ' }' + CRLF;

       // Write JSON to Standard Out
       start = 1;
       Dou start > %len(json);
          len = %len(json) - start + 1;
          If len > splitSize;
             len = splitSize;
          Endif;
          QtmhWrStout(%subst(JSON:start:len):len:ErrCode);
          start += splitSize;
       Enddo;

       If ioRtnCode = 'more...';
          BuildHubJSON(decCall#+1);
       Endif;
      /End-free
     PBuildHubJSON     e

     P*===========================================================================
     P* Look for the next query
     P*===========================================================================
     PCheckSecurity    b
     DCheckSecurity    pi             1n
     D inUserId                     256
     D inUser                        10

     D isPassed        s              1n   inz('1')

     ddecLvl           s              3  0 inz(0)
     dstrAuth          s             10a   varying inz
     dstrRoleKey       s             40a   varying inz

     ddsAuthority      ds                  qualified inz
     d auth                          10a   varying
     d lvl                            3  0
      /Free
       If strRolesJson <> *blanks;
          Exsr ChkRoles;
       Endif;
       Select;
       When not isPassed;
          isError = *on;
        //***  Begin Add  ***  12/14/21  **********************
        //strErrorMsg ='"rtnCode": "User, ' + %trim(inUserId) +
        // ', not authorized to process ' +
        // 'request (' + %trim(strRequest) + ')."';
          strErrorMsg ='"rtnCode": "User not authorized to process ' +
           'request (' + %trim(strRequest) + ')."';
        //***  End   Add  ***  12/14/21  **********************
       Endsl;
       Return isPassed;
       //==============================
       Begsr ChkRoles;
        node1 = yajl_string_load_tree(strRolesJson:strMsg);
        If strMsg <> *blanks;
           strErrorMsg ='"rtnCode": "Roles: Invalid json."';
           Return *off;
        Endif;
        Reset i1;
        Reset dsAuthority;
        Dow YAJL_array_loop(node1:i1:val);
           strRoleKey = YAJL_get_string(val);
           Exec sql
            with
            roles as
            (select *
              from SIRLPPRD
              where RLstat <> 'I' and RLco# = :intCo# and RLkey = :strRoleKey
              order by RLkey, RLproduct, RLprodSub desc),
            associations as
            (select *
              from SIHUBPRCA
              where SIstat <> 'I' and SIrequest = :strRequest),
            countAssociation as
            (select SIrequest, count(*) associationCount
              from associations
              group by SIrequest),
            roleAssociation as
            (select SIrequest, SIproduct, SIsubProd, RLkey, RLauth,
              case
               when RLkey is null then ' '
               else RLauth end as auth
              from associations
              left join roles
               on SIproduct=RLproduct
                and (SIsubProd=RLprodSub or RLprodSub = ' ')
              where RLkey is not null),
            authorities as
            (select RLauth,
              case
               when auth is null
                and (select associationCount from countAssociation) is null
                then '*All'
               when auth is null then ' '
               else auth end as authType,
              case
               when RLauth = '*All'    then 50
               when RLauth = '*Update' then 40
               when RLauth = '*View'   then 30
               when (select associationCount from countAssociation) is null
                then 50
               when a.SIrequest is null  then 20
               else 10 end as authLvl
              from SIHUBPRC b
              left join roleAssociation a on a.SIrequest=b.SIrequest
              where b.SIrequest = :strRequest
              order by b.SIrequest)
            select authType, authLvl
             into :dsAuthority
             from authorities
             order by authLvl desc
             limit 1;
           Select;
           When not IsValidFetch(sqlstt);
           When dsAuthority.lvl > decLvl;
              strAuth = dsAuthority.auth;
              decLvl = dsAuthority.lvl;
           Endsl;
        Enddo;
        ioRtnCode = strAuth;
        Select;
        When strAuth = *blanks;
           isPassed = *off;
        Endsl;
       Endsr;
      /End-free
     PCheckSecurity    e

     P*===========================================================================
     P* Get the program based on the request
     P*===========================================================================
     PGetProgram       b
     DGetProgram       pi            10
     D inRequest                    256     const
     D
     DstrProgram       s             10    inz
      /Free
       Exec SQL
        select SIprogram
         into :strProgram
         from SIHUBPRC
         where SIrequest = :inRequest;
       Select;
       When isTesting;
          strProgram = 'WTPOLQTE06';
       When sqlStt <> '00000';
          Reset strProgram;
          // send error response
          isError = *on;
          strErrorMsg ='"rtnCode": "Request (' +
           %trim(inRequest) + ') not available."';
       Endsl;
       Return strProgram;
      /End-free
     PGetProgram       e

     P*===========================================================================
     P* Get the request infomration via zhbgetinput
     P*===========================================================================
     PGetRequest       b
     DGetRequest       pi             1n
      /Free
       Monitor;
          // Get the input buffer sent from the browser
          dec#OfVars=zhbGetInput(strSavedQuery:qusec);
       On-error;
          strErrorMsg = 'Read zhbgetinput failed.';
       Endmon;
       Return dec#ofVars > *zero;
      /End-free
     PGetRequest       e

     P*===========================================================================
     P* Get the user based on the access request id
     P*===========================================================================
     PGetUser          b
     DGetUser          pi            10
     D inUserId                     256     const
     D
     DstrUser          s             10    inz
      /Free
       Reset strUser;
       Exec SQL
        select SIusr
         into :strUser
         from SIHUBUSR
         where SIuserId = :inUserId;
       Select;
       When isTesting;
          strUser = 'PDSPGMR';
       When sqlStt <> '00000';
          Exec SQL
           select UPusr
            into :strUser
            from SMUPL
            where UPusr = :inUserId;
          Select;
          When sqlStt <> '00000';
             Reset strUser;
          Endsl;
       Endsl;
       Select;
       When strUser = *blanks;
          // send error response
          isError = *on;
          strErrorMsg ='"rtnCode": "User (' +
           %trim(inUserId) + ') not available."';
       Endsl;
       Return strUser;
      /End-free
     PGetUser          e

     P*===========================================================================
     P* Setup the user IFS, return the requested path
     P*===========================================================================
     PIfsSetup         b
     DIfsSetup         pi           256a
     D inType                        10a   const
     D
     DisRequest        s              1n   inz
     DisRequestLog     s              1n   inz
     DisResults        s              1n   inz
     DoutRtCd          s              2a   inz
     DstrCmd           s            456a
     DstrDir           s            256a
     DstrDirUser       s            256a
     DstrPathDft       s            256a
     DstrPathRequest   s            256a
     DstrPathRequestLog...
     D                 s            256a
     DstrPathResults   s            256a
     DconRequest       c                   '/request.txt'
     DconRequestLog    c                   '/requestLog.txt'
     DconResults       c                   '/results.txt'
      /Free
       strDir = conDir + %trim(strRequest);
       strDirUser = %trim(strDir) + '/' + %trim(strUser);
       strPathDft = %trim(strDir) + conRequest;
       strPathRequest = %trim(strDirUser) + conRequest;
       strPathRequestLog = %trim(strDirUser) + conRequestLog;
       strPathResults = %trim(strDirUser) + conResults;
       Monitor;
          CheckObjIFS(strPathRequest:isRequest);
          CheckObjIFS(strPathResults:isResults);
          CheckObjIFS(strPathRequestLog:isRequestLog);
          If not isRequest or not isResults or not isRequestLog;
             strCmd = 'CRTDIR DIR(''' + %trim(strDirUser) + ''')';
             Monitor;
                RunCmd(strCmd:%len(strCmd));
             On-error;
             Endmon;
             flags = O_WRONLY + O_CREAT + O_TRUNC + O_TEXTDATA;
             mode =  S_IRUSR + S_IWUSR + S_IRGRP + S_IROTH;
             CopyDft(strPathDft:strPathRequest:outRtCd);
             // Setup and clear the request log
             CopyDft(strPathDft:strPathRequestLog:outRtCd);
             fd = open(%trim(strPathRequestLog):flags:mode);
             callp close(fd);
             // Setup and clear the results
             CopyDft(strPathDft:strPathResults:outRtCd);
             fd = open(%trim(strPathResults):flags:mode);
             callp close(fd);
          Endif;
       On-error;
       Endmon;
       Select;
       When inType = 'Request';
          Return strPathRequest;
       When inType = 'RequestLog';
          Return strPathRequestLog;
       When inType = 'Results';
          Return strPathResults;
       Endsl;
      /End-free
     PIfsSetup         e
