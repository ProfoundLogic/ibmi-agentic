     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h BndDir('WTBNDDIR':'YAJL')
      *====================================================================
      * Description:
      *  Groups procedures that return policy table information as
      *   JSON statements
      *
      *====================================================================
      * Revisions:
      *
      * 01/14/22 C:33  Support: 100464
      *               -Include leading zeros (4 digits) w/class codes.
      *               -Compare PRCLAS using digits.
      *
      * 09/16/21 C:079 Project: CN024
      *               Changes for portal support.
      *
      *====================================================================
     fwdell     if   e           k disk    Rename(pel01:pel00) UsrOpn

     d* this size needs to change
     d array           s               a   Len(32767)
     d clientID        s              3a
     d inJson          s               a   Len(2000000) varying
     d intCo#          s              3s 0
     d json            s               a   Len(8500000) varying
     d jsonSub         s               a   Len(8500000) varying
     d strAuthority    s             10a
     d strCo#          s              3a
     d strErrMsgs      s          32000    Inz varying
     d strJsonFilters  s          32000    Inz varying
     d sysDate         s               d
     d sysDate7        s              7  0

      * YAJL variables
     d errMsg          s            500a   Varying Inz(' ')
     d docNode         s                   Like(yajl_val)
     d list            s                   Like(yajl_val)
     d i0              s             10i 0 Inz
     d i1              s             10i 0 Inz
     d i2              s             10i 0 Inz
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d val             s                   Like(yajl_val)
     d key             s             50a   Varying
      /include yajl_h

     d psds          esds                  ExtName(SZQ1P)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy wPrUsrPrms
      /copy wPrEmplr

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

      *entry
     DRequestPgm       pr                  ExtPgm('WITABLE')
     D                              256
     D                               10
     D                                 a   Len(8500000) varying
     D                                 a   Len(8500000) varying
     D                               10
     DRequestPgm       pi
     D inRequest                    256
     D inUser                        10
     D inParms                         a   Len(8500000) varying
     D outJSON                         a   Len(8500000) varying
     D ioRtCd                        10

      /Free
       Exsr PgmStart;
       Select;
       When inRequest = 'ListBillingPlans';
          ListBillingPlans();
       When inRequest = 'ListExposureCodes';
          ListExposureCodes();
       When inRequest = 'ListStates';
          ListStateCodes();
       When inRequest = 'ListQuoteStatuses';
          ListQteStsCodes();
       When inRequest = 'ListTableValues';
          ListTableValues();
       Other;
          Exsr GetParms;
          Exsr BuildOutput;
       Endsl;
       outJSON = json;
       *Inlr = *On;

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
       Begsr GetParms;
       //=================================================================
       Endsr;

       //=================================================================
       Begsr PgmStart;
       //=================================================================
        strAuthority = ioRtCd;
        Clear ioRtCd;
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
       Endsr;
      /End-free

      *=====================================================
      * List Billing Plans
      *=====================================================
      * Input:
      * {
      *   "dates": {
      *             "billStartDate": "",
      *             "policyExpDate": "",
      *             "billOnEffDate": ""
      *            },
      *   "policy": {
      *              "group": "",
      *              "employer": "",
      *              "year": ""
      *             }
      * }
      * Output:
      *   {
      *    "rtnCode": "",
      *    "requestParms":
      *     {
      *      "policyFromDate": "",
      *      "policyToDate": "",
      *      "group": "",
      *      "employer": "",
      *      "year": ""
      *     },
      *    "applicationList":
      *     [{
      *      "billPlanCode": "",
      *      "billPlanDesc": ""
      *     }]
      *   }
      *====================================================
     pListBillingPlans...
     p                 b

     d #OfDays         s              3s 0
     d bs              s                   Dim(12) Like(BSab01)
     d chrBillOnEfdt   s              1a
     d comma           s              1a
     d comma2          s              1a
     d compDate        s               d
     d dsWTBSP       e ds                  ExtName(WTBSP) Inz
     d endDate         s               d
     d errMsgArray     s            500a   Dim(maxSize)
     d idx             s              3s 0
     d intDiv          s              5s 0
     d intEmp#         s              9s 0
     d intFnd          s              3s 0
     d intFyr          s              3s 0
     d i1              s             10i 0 Inz
     d i2              s             10i 0 Inz
     d maxSize         c                   999
     d startDate       s               d
     d strEmp#         s              9a
     d strErrorMsg     s            500a   Varying
     d strFrDtUSA      s              8a
     d strFnd          s              3a
     d strFyr          s              3a
     d strSqlCursor    s               a   Len(32000) Varying
     d strToDtUSA      s              8a

      /Free
       Exsr GetParms;
       If ioRtCd <> 'Error';
          Exsr EditParms;
       Endif;
       Select;
       When ioRtCd <> 'Error';
          Exsr BuildSql;
       When ioRtCd = 'Error';
          Exsr BuildOutput;
       Endsl;
       outJSON = json;
       Return;

       //---------------------------------------------------
       Begsr GetParms;
       //--------------------------------------------------------
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           node = YAJL_object_find(docNode:'dates');
           Reset i1;
           Dow YAJL_object_loop(node:i1:key:val);
              Select;
              When key = 'billStartDate';
                 strFrDtUSA = YAJL_get_string(val);
              When key = 'policyExpDate';
                 strToDtUSA = YAJL_get_string(val);
              When key = 'billOnEffDate';
                 chrBillOnEfDt = YAJL_get_string(val);
                 ELblfl = chrBillOnEfDt;
              Endsl;
           Enddo;
           node = YAJL_object_find(docNode:'policy');
           Reset i1;
           Dow YAJL_object_loop(node:i1:key:val);
              Select;
              When key = 'group';
                 strFnd  = YAJL_get_string(val);
              When key = 'employer';
                 strEmp# = YAJL_get_string(val);
              When key = 'year';
                 strFyr  = YAJL_get_string(val);
              Endsl;
           Enddo;
           strJsonFilters = '"dates":{' +
            %Trim(BuildJsonNode('billStartDate':strFrDtUsa)) + ', ' +
            %Trim(BuildJsonNode('policyExpDate':strToDtUsa)) + ', ' +
            %Trim(BuildJsonNode('billOnEffDate':chrBillOnEfdt)) + '}, ' +
            '"policy":{' +
            %Trim(BuildJsonNode('group':strFnd)) + ', ' +
            %Trim(BuildJsonNode('employer':strEmp#)) + ', ' +
            %Trim(BuildJsonNode('year':strFyr)) + '}';
           json = %Trim(json) + ' "requestParms" : {' + %Trim(strJsonFilters) +
            '}';
        Else;
           errMsgArray(1) = '"JSON filter can''t be read."';
           ioRtCd = 'Error';
        Endif;
       Endsr;
       //--------------------------------------------------------
       Begsr EditParms;
       //--------------------------------------------------------
        Select;
        When strFnd <> *Blanks And strEmp# <> *Blanks And strFyr <> *Blanks;
           Monitor;
              intFnd = %Int(strFnd);
           On-Error;
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Fund has character data."';
              ioRtCd = 'Error';
           Endmon;
           Monitor;
              intFyr = %Int(strFyr);
           On-Error;
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Policy year has character data."';
              ioRtCd = 'Error';
           Endmon;
           Monitor;
              intEmp# = %Int(strEmp#);
           On-Error;
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Employer # has character data."';
              ioRtCd = 'Error';
           Endmon;
           If ioRtCd <> 'Error';
              intDiv = GetStRptDv(intCo#:intFnd:intEmp#);
              Open WDELL;
              Chain (intCo#:intFnd:intEmp#:intDiv:intFyr) pel00;
              If Not(%Found);
                 idx = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idx) = '"Invalid policy key information."';
                 ioRtCd = 'Error';
              Else;
                 startDate = %Date(ELbsdt:*Cymd);
                 endDate = %Date(ELptdt:*Cymd);
              Endif;
              Close WDELL;
           Endif;
        When strFrDtUsa <> *Blanks and strToDtUsa <> *Blanks;
        Other;
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Either valid policy or date information ' +
            'must be passed in."';
           ioRtCd = 'Error';
        EndSl;



        If ioRtCd <> 'Error';
           If strFrDtUSA <> *Blanks;
              Monitor;
                 startDate = %Date(strFrDtUSA:*Usa0);
              On-Error;
                 idx = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idx) = '"Billing start date is invalid."';
                 ioRtCd = 'Error';
              EndMon;
              Monitor;
                 endDate = %Date(strToDtUSA:*Usa0);
              On-Error;
                 idx = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idx) = '"Policy expiration date is invalid."';
                 ioRtCd = 'Error';
              EndMon;
           Endif;
           If chrBillOnEfDt <> *Blank and chrBillOnEfDt <> 'Y';
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Bill on effective date flag must be ' +
              '''Y'' or blank"';
              ioRtCd = 'Error';
           Else;
              ELblfl = chrBillOnEfDt;
           Endif;
        Endif;

       Endsr;
       //--------------------------------------------------------
       Begsr BuildSql;
       //--------------------------------------------------------
        //*** Beg Chg *** 09/16/21 *********************************************
        //***strSqlCursor = 'Select * From WTBSP Where BSco# = ' + strCo# +
        //*** ' And BSstat <> ''I'' Order by BSkey';
        strSqlCursor = 'Select * From WTBSP Where BSco# = ' + strCo# +
         ' And BSstat <> ''I'' And BSportal = ''Y'' Order by BSkey';
        //*** End Chg *** 09/16/21 *********************************************

        Exec SQL Prepare planString From :strSqlCursor;
        Exec Sql Declare pCursor Cursor For planString;
        Exec Sql Open pCursor;

        If SqlStt <> '00000';
        Exec Sql Close pCursor;
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Open SQL statement failed. ' +
            'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X') + '"';
           ioRtCd = 'Error';
           Exsr BuildOutput;
           LeaveSr;
        Endif;

        Dou SqlStt <> '00000';
           Exec Sql Fetch pCursor into :dsWTBSP;
           Select;
           When SqlStt = '00000';
              bs(1)  = BSab01;
              bs(2)  = BSab02;
              bs(3)  = BSab03;
              bs(4)  = BSab04;
              bs(5)  = BSab05;
              bs(6)  = BSab06;
              bs(7)  = BSab07;
              bs(8)  = BSab08;
              bs(9)  = BSab09;
              bs(10) = BSab10;
              bs(11) = BSab11;
              bs(12) = BSab12;
           When SqlStt = '02000' and comma = ',';
              json = %Trim(json) + ' ]';
              Leave;
           When SqlStt = '02000' and comma = *Blank;
              json = json + ', "billingPlanList": [ ]';
              Leave;
           Other;
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Fetch SQL statement failed. ' +
               'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X') + '"';
              ioRtCd = 'Error';
              Leave;
           Endsl;
          // Don't return billing plans on flat cx
           If startDate = endDate;
              Iter;
           Endif;
          // Ensure est deposit plans are accounted for
           Select;
           When BSesd2 <> 0;
               bs(2) = 1;
           When BSesdp <> 0;
               bs(1) = 1;
           Endsl;
          // Find last installment
           For idx = 1 to 11;
              If bs(12 - idx) <> *Zero;
                 idx = 11 - idx;
                 Leave;
              Endif;
           Endfor;
          // Iter if last installment exceeds policy exp date
           compDate = startDate + %Months(idx);
           Select;
           When BSon15 = 'Y';
              #OfDays = %Subdt(compDate:*days);
              compDate = compDate - %Days(#Ofdays);
              compDate = compDate + %Days(15);
           When ELblfl = 'Y';
           Other;
              #OfDays = %Subdt(compDate:*days) - 1;
              compDate = compDate - %Days(#Ofdays);
           Endsl;
           If compDate >= endDate;
              Iter;
           Endif;
           Exsr BuildOutput;
        Enddo;
        Exec Sql Close pCursor;

       Endsr;
       //========================================================
       // Builds Return JSON
       //========================================================
       Begsr BuildOutput;
        If ioRtCd <> 'Error';
           If comma = ',';
              json = %Trim(json) + comma;
           Else;
              json = %Trim(json) + ', "applicationList": [';
              comma = ',';
           Endif;
           json = %Trim(json) +
            '{' +
           %Trim(BuildJsonNode('billPlanCode':BSkey)) + ', ' +
           %Trim(BuildJsonNode('billPlanDesc':BSdesc)) +
            '}';
        Else;
           For idx = 1 to %Elem(errMsgArray);
              If errMsgArray(idx) = *Blanks;
                 Leave;
              EndIf;
              If comma2 <> *Blank;
                 array = %Trim(array) + comma2;
              Else;
                 comma2 = ',';
              Endif;
              array = %Trim(array) + errMsgArray(idx);
           EndFor;
           i2 = %Len(%Trim(json));
           If %Subst(json:i2:1) <> ',';
              json = %Trim(json) + ',';
           Endif;
           json = %Trim(json) + '"errorMsg": [ ' + %trim(array) + ' ]';
        EndIf;
       Endsr;
      /End-Free
     PListBillingPlans...
     P                 e
     P*==================================================================
     P* List Exposure Codes
     P*==================================================================
     PListExposureCodes...
     P                 b
     DListExposureCodes...
     D                 pi

     Di1               s             10i 0 inz
     Di2               s             10i 0 inz
     DinCode           ds                  likeds(dsStringFilter)
     DinDesc           ds                  likeds(dsStringFilter)
     DisContinue       s              1n   inz('1')
     DisEofWMPRP       s              1n   inz('0')
     DisEofWMPXP       s              1n   inz('0')
     DisJsonExt        s              1n   inz('0')
     DisJsonClass      s              1n   inz('0')
     DstrJsonClass     s          32000    inz
     DstrJsonExt       s          32000    inz
     DstrRateSet       s              3
     DstrSqlWMPRP      s            500
     DstrSqlWMPXP      s            500
     DstrSqlWMRSP      s            500

     DdsWMPRP          ds                  inz qualified
     D rtCd                           3
     D class                          5
     D efDt                           7
     D desc                          30
     DdsWMPXP          ds                  inz qualified
     D class                          5
     D ext                            5
     D efDt                           7
     D exDt                           7
     D desc                          30
     DdsWMRSP          ds                  inz qualified
     D rtCd                           3
     D state                          2
     H*    code:{code:value,type:contains/starts with/ends with} (optional)
     H*    description:{code:value,type:contains/starts with/ends with} (optional)
     H*    status:value (optional)
     H*    asOfDate:value (optional)
     DdsStringFilter   ds                  inz qualified
     D value                       2000    varying
     D type                          25    varying
     DdsInFilters      ds                  inz qualified
     D asOfDate                      10    varying
     D status                         1
     D code                                likeds(dsStringFilter)
     D desc                                likeds(dsStringFilter)
     DdsInParms        ds                  inz qualified
     D rateSetCode                    3
     D ratingState                    2
     D group                          3
     D filters                     6000

      /Free
       Exsr GetParms;
       Exsr BuildOutput;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
       // rateSetCode:x,
       // state:x,
       // classCodeList:[
       //  {code:x,extensions:[
       //   {code:x,description:x,effectiveDate:x,expirationDate:x}
       //  ]
       // ]
        Exsr BuildSqlWMRSP;
        Exsr BuildSqlWMPRP;
        json += '"requestParms":{' +
         BuildJsonNode('rateSetCode':dsInParms.rateSetCode) + ',' +
         BuildJsonNode('ratingState':dsInParms.ratingState) + ',' +
         BuildJsonNode('group':dsInParms.group) + ',' +
         ' "filters" : ' + strJsonFilters +
         ' } , ' +
         BuildJsonNode('rateSetCode':dsWMRSP.rtCd) + ', ' +
         BuildJsonNode('ratingState':dsWMRSP.state) + ', ' +
         '"classCodeList":[';
        Exsr ProcessWMPRP;
        json += ']';
        Exec SQL close classCursor;
        Exec SQL close extCursor;
        If strErrMsgs <> *blanks;
           json += ', "errorMsg":[' + %trim(strErrMsgs) + ']';
        Endif;
       Endsr;

       //=================================================================
       // Build the JSON nodes for class information
       //  {code:x,extensions:[extlist]}
       //=================================================================
       Begsr BuildJsonClass;
        Reset strJsonClass;
        Select;
        When not isJsonExt;
           Leavesr;
        Endsl;
        If isJsonClass = *on;
           json = %trim(json) + ',';
        Endif;
        strJsonExt = %trim(strJsonExt) + ']';
        strJsonClass = '{' +
         %trim(BuildJsonNode('code':dsWMPRP.class)) + ', ' +
         '"extensions" : ' + %trim(strJsonExt) +
         ' }';
        isJsonClass = *on;
        Reset isJsonExt;
       Endsr;

       //=================================================================
       // Build the JSON nodes for extension information
       //   {code:x,description:x,effectiveDate:x,expirationDate:x}
       //=================================================================
       Begsr BuildJsonExt;
        Select;
        When dsWMPXP.ext = *blank;
           dsWMPXP.desc = dsWMPRP.desc;
           dsWMPXP.efDt = dsWMPRP.efDt;
        Endsl;
        Select;
        When dsInFilters.desc.value <> *blanks;
           Select;
           When dsInFilters.desc.type = 'exact'
            and %trim(dsWMPXP.desc) = %trim(dsInFilters.desc.value);
              Leavesr;
           When dsInFilters.desc.type = 'contains' and not
            StringFound(%trim(dsWMPXP.desc):%trim(dsInFilters.desc.value):*on);
              Leavesr;
           When dsInFilters.desc.type = 'beginsWith'
            and %scan(%trim(Upper(dsInFilters.desc.value)):
                 (%trim(Upper(dsWMPXP.desc))))
                 <> 1;
              Leavesr;
           When dsInFilters.desc.type = 'endsWith'
            and %scan(%trim(Upper(dsInFilters.desc.value)):
                 (%trim(Upper(dsWMPXP.desc))))
             <> (%len(%trim(dsWMPXP.desc)) + 1 -
                 %len(%trim(dsInFilters.desc.value)));
              Leavesr;
           Endsl;
        Endsl;
        Select;
        When strJsonExt = *blank;
           strJsonExt = '[';
        Other;
           strJsonExt = %trim(strJsonExt) + ',';
        Endsl;
        strJsonExt = %trim(strJsonExt) + '{' +
         %trim(BuildJsonNode('code':dsWMPXP.ext)) + ', ' +
         %trim(BuildJsonNode('description':dsWMPXP.desc)) + ', ' +
         %trim(BuildJsonNode('effectiveDate':dsWMPXP.efDt:'date')) + ', ' +
         %trim(BuildJsonNode('expirationDate':dsWMPXP.exDt:'date')) +
         '}';
        isJsonExt = *on;
       Endsr;

       //=================================================================
       // Build the SQL for WMPRP
       //=================================================================
       Begsr BuildSqlWMPRP;
        strSqlWMPRP =
         'with maxDate as ( ' +
         ' select PRCO#, PRRTCD, PRCLAS, max(PREFDT) as maxDt' +
         '  from WMPRP' +
         '  where PREFDT <= ' + dsInFilters.asOfDate +
         '  group by PRCO#, PRRTCD, PRCLAS' +
         '  order by PRCO#, PRRTCD, PRCLAS' +
         ') ' +
       //***  Begin Chg  ***  01/14/22  ******************  +
       //'select a.PRRTCD, a.PRCLAS, a.PREFDT, a.PRDESC' +
         'select a.PRRTCD, substring(digits(a.PRCLAS),2), a.PREFDT, a.PRDESC' +
       //***  End   Chg  ***  01/14/22  ******************  +
         ' from WMPRP a' +
         ' join maxDate b' +
         '  on (a.PRCO#=b.PRCO# and a.PRRTCD=b.PRRTCD and a.PRCLAS=b.PRCLAS' +
         '   and a.PREFDT=maxDt)' +
         '   where a.PRRTCD = ''' + strRateSet + '''' +
       //***  Begin Chg  ***  01/14/22  ******************  +
       //'    and upper(a.PRCLAS) like upper(''' +
         '    and upper(digits(a.PRCLAS)) like upper(''' +
       //***  End   Chg  ***  01/14/22  ******************  +
               %trim(dsInFilters.code.value) + ''')' +
         '    and a.PRSTAT = ''' + %trim(dsInFilters.status) + '''' +
         ' order by a.PRCO#, a.PRRTCD, a.PRCLAS';
        Exec SQL prepare classCode from :strSqlWMPRP;
        Exec SQL declare classCursor cursor for classCode;
        Exec SQL open classCursor;
       Endsr;

       //=================================================================
       // Build the SQL for WMPXP
       //=================================================================
       Begsr BuildSqlWMPXP;
        Exec SQL close extCursor;
        strSqlWMPXP =
         'with maxDate as (' +
         ' select PXCO#, PXCLAS, PXST, PXEXT, max(PXEFDT) as maxDt' +
         '  from WMPXP' +
         '  where PXEFDT <= ' + dsInFilters.asOfDate +
         '   and PXST = ''' + dsWMRSP.state + '''' +
         '  group by PXCO#, PXCLAS, PXST, PXEXT' +
         '  order by PXCO#, PXCLAS, PXST, PXEXT' +
         ') ' +
         'select a.PXCLAS, a.PXEXT, a.PXEFDT, a.PXEXDT, a.PXDESC' +
         ' from WMPXP a' +
         ' join maxDate b' +
         '  on (a.PXCO#=b.PXCO# and a.PXCLAS=b.PXCLAS and a.PXEXT=b.PXEXT' +
         '   and a.PXEFDT=maxDt)' +
         ' where a.PXCLAS = ' + dsWMPRP.class + ' and ' +
         '  (PXEXDT = 0 or PXEXDT >= ' + dsInFilters.asOfDate + ')' +
         ' order by a.PXCO#, a.PXCLAS, a.PXEXT';
        Exec SQL prepare extCode from :strSqlWMPXP;
        Exec SQL declare extCursor cursor for extCode;
        Exec SQL open extCursor;
       Endsr;

       //=================================================================
       // Build the SQL for WMRSP
       //=================================================================
       Begsr BuildSqlWMRSP;
        Reset dsWMRSP;
        Exec SQL
         select RSRTCD, RSJRST
          into :dsWMRSP
          from WMRSP
          where RSRTCD = :strRateSet;
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        // Load the json tree
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson : errMsg);
        If errMsg = ' ';
           // Load the data structure with the parameter values from the tree
           node = YAJL_object_find(docNode: 'rateSetCode');
           dsInParms.rateSetCode = YAJL_get_string(node);
           node = YAJL_object_find(docNode: 'ratingState');
           dsInParms.ratingState = YAJL_get_string(node);
           node = YAJL_object_find(docNode: 'group');
           dsInParms.group = YAJL_get_string(node);
           // Load the filter tree
           node = YAJL_object_find(docNode: 'filters');
           Reset i1;
           Dow YAJL_object_loop(node:i1:key:val);
              Select;
              When key = 'asOfDate';
                 dsInFilters.asOfDate = YAJL_get_string(val);
              When key = 'status';
                 dsInFilters.status = YAJL_get_string(val);
              When key = 'code';
                 // Load the filter (code) tree
                 node2 = YAJL_object_find(node: 'code');
                 Reset i2;
                 Dow YAJL_object_loop(node2:i2:key:val);
                    Select;
                    When key = 'value';
                       dsInFilters.code.value = YAJL_get_string(val);
                    When key = 'type';
                       dsInFilters.code.type = YAJL_get_string(val);
                    Endsl;
                 Enddo;
              When key = 'description';
                 // Load the filter (code) tree
                 node2 = YAJL_object_find(node: 'description');
                 Reset i2;
                 Dow YAJL_object_loop(node2:i2:key:val);
                    Select;
                    When key = 'value';
                       dsInFilters.desc.value = YAJL_get_string(val);
                    When key = 'type';
                       dsInFilters.desc.type = YAJL_get_string(val);
                    Endsl;
                 Enddo;
              Endsl;
           Enddo;
        Endif;

        Exsr GetRateSetCode;

        strJsonFilters = '{' +
         %trim(BuildJsonNode('asOfDate':dsInFilters.asOfDate)) + ', ' +
         %trim(BuildJsonNode('status':dsInFilters.status)) + ', ' +
         '"code":{' +
          %trim(BuildJsonNode('value':%trim(dsInFilters.code.value))) + ', ' +
          %trim(BuildJsonNode('type':%trim(dsInFilters.code.type))) + '}, ' +
         '"description":{' +
          %trim(BuildJsonNode('value':%trim(dsInFilters.desc.value))) + ', ' +
          %trim(BuildJsonNode('type':%trim(dsInFilters.desc.type))) +  '}' +
          '}';
        // Filters are not required
        If dsInFilters.asOfDate = *blank;
           dsInFilters.asOfDate = %char(SystemDate():*usa0);
        Endif;
        If dsInFilters.status = *blank;
           dsInFilters.status = 'A';
        Endif;
        If dsInFilters.code.type = *blank;
           dsInFilters.code.type = 'contains';
        Endif;
        If dsInFilters.desc.type = *blank;
           dsInFilters.desc.type = 'contains';
        Endif;
        Monitor;
           dsInFilters.asOfDate =
            %editc(DateTo7(%date(%int(dsInFilters.asOfDate):*USA)):'X');
        On-error;
           isContinue = *off;
           ioRtCd = 'error';
           strErrMsgs = '"Invalid ''As of Date''."';
        Endmon;
        Select;
        When dsInFilters.code.type = 'exact';
        When dsInFilters.code.type = 'contains';
           dsInFilters.code.value = '%' + %trim(dsInFilters.code.value) + '%';
        When dsInFilters.code.type = 'beginsWith';
           dsInFilters.code.value = %trim(dsInFilters.code.value) + '%';
        When dsInFilters.code.type = 'endsWith';
           dsInFilters.code.value = '%' + %trim(dsInFilters.code.value);
        Endsl;

       Endsr;

       //=================================================================
       // Get the appropriate rate set code
       //=================================================================
       Begsr GetRateSetCode;
        // Determine which Rate Set Code can be used
        Reset strRateSet;
        // Rate Set Code is passed in
        If strRateSet = *blanks
         and  dsInParms.rateSetCode <> *blank;
           strRateSet =  dsInParms.rateSetCode;
        Endif;
        // Rate Set Code is defaulted in group
        If strRateSet = *blanks and dsInParms.group <> *zero;
           Exec sql
            select RRDRAT into :strRateSet
             from WDRRP
             where RRFND = :dsInParms.group and RRST = :dsInParms.ratingState
              and RREFDT <= :sysDate7
             order by RRCO#, RRFND, RREFDT desc
             limit 1;
        Endif;
       Endsr;

       //=================================================================
       // Process WMPRP
       //=================================================================
       Begsr ProcessWMPRP;
        If not isContinue;
           Leavesr;
        Endif;
        Reset isEofWMPRP;
        Dou isEofWMPRP;
           Exec SQL fetch next from classCursor into :dsWMPRP;
           Reset strJsonExt;
           Reset dsWMPXP;
           Select;
           When sqlStt <> '00000';
              isEofWMPRP = *on;
              Iter;
           Endsl;
           Exsr BuildJsonExt;
           Exsr ProcessWMPXP;
           Exsr BuildJsonClass;
           json = %trim(json) + %trim(strJsonClass);
        Enddo;
       Endsr;

       //=================================================================
       // Process WMPXP
       //=================================================================
       Begsr ProcessWMPXP;
        Exsr BuildSqlWMPXP;
        Reset isEofWMPXP;
        Dou isEofWMPXP;
           Exec SQL fetch next from extCursor into :dsWMPXP;
           Select;
           When sqlStt <> '00000';
              isEofWMPXP = *on;
              Iter;
           Endsl;
           Exsr BuildJsonExt;
        Enddo;
       Endsr;
      /End-Free
     PListExposureCodes...
     P                 e
     P*==================================================================
     P* List State Codes
     P*==================================================================
     PListStateCodes...
     P                 b
     DListStateCodes...
     D                 pi

     Dfilter           s               n   inz(*Off)
     Di1               s             10i 0 inz
     Di2               s             10i 0 inz
     DinRtCd           s              3
     DisContinue       s              1n   inz('1')
     DisEofWMPRP       s              1n   inz('0')
     DisEofWMPXP       s              1n   inz('0')
     DisJsonExt        s              1n   inz('0')
     DisJsonClass      s              1n   inz('0')
     D moreRows        s               n   Inz(*On)
     DstrJsonClass     s          32000    inz
     DstrJsonExt       s          32000    inz
     DsqlStmtStr       s            500

     DdsSTSTP        e ds                  extname(STSTP)

     DdsFilter         ds                  inz qualified
     D stateCode                      2
     D stateName                     40
     D status                         1
     D wcmcInstalled                  1                                         ST1a3 = 'Y'
     D wcmcEdi                        1                                         ST1a4 = 'Y'
     D omitted                        1                                         ST1a5 = 'Y'

      /Free

       Exsr GetParms;
       Exsr BuildOutput;

       //=================================================================
       // Build the State Sql Statement
       //=================================================================
       Begsr BldStateSQL;

          sqlStmtStr = 'Select * from STSTP';

          // Add on Filters
          // Start Where
          If dsFilter.stateCode  <> ' ' Or dsFilter.stateName  <> ' ' Or
             dsFilter.status  <> ' ' Or dsFilter.wcmcInstalled <> ' ' Or
             dsFilter.wcmcEdi <> ' ' Or dsFilter.omitted <> ' ';

             sqlStmtStr =  %Trim(sqlStmtStr) + ' Where';

             // State Code Filter
             If dsFilter.stateCode <> ' ';
                sqlStmtStr  = %Trim(sqlStmtStr) + ' STkey=''' +
                  %Trim(dsFilter.stateCode) + '''';
                filter = *On;
             EndIf;

             // State Name
             If dsFilter.stateName <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And Upper(STname) like ''%' +
                      %Trim(Upper(dsFilter.stateName)) + '%''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' Upper(STname) like ''%' +
                       %Trim(Upper(dsFilter.stateName)) + '%''';
                   filter = *On;
                EndIf;
             EndIf;

             // Status Filter
             If dsFilter.status <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And STstat=''' + dsFilter.status + '''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' STstat=''' + dsFilter.status + '''';
                   filter = *On;
                EndIf;
             EndIf;

             // WCMC Installed Filter
             If dsFilter.wcmcInstalled <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And ST1a3=''' + dsFilter.wcmcInstalled + '''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' ST1a3=''' + dsFilter.wcmcInstalled + '''';
                   filter = *On;
                EndIf;
             EndIf;

             // WCMC EDI Filter
             If dsFilter.wcmcEdi <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And ST1a4=''' + dsFilter.wcmcEdi + '''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' ST1a4=''' + dsFilter.wcmcEdi + '''';
                   filter = *On;
                EndIf;
             EndIf;

             // Omitted Filter
             If dsFilter.omitted <> ' ';
                If filter;
                   If dsFilter.omitted = 'Y';
                      sqlStmtStr = %Trim(sqlStmtStr) +
                         ' And ST1a5=''' + dsFilter.omitted + '''';
                   Else;
                      sqlStmtStr = %Trim(sqlStmtStr) +
                         ' And ST1a5<>''Y''';
                   EndIf;
                Else;
                   If dsFilter.omitted = 'Y';
                      sqlStmtStr = %Trim(sqlStmtStr) +
                         ' ST1a5=''' + dsFilter.omitted + '''';
                   Else;
                      sqlStmtStr = %Trim(sqlStmtStr) +
                         ' ST1a5<>''Y''';
                      filter = *On;
                   EndIf;
                EndIf;
             Else;
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And ST1a5<>''Y''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' ST1a5<>''Y''';
                   filter = *On;
                EndIf;
             EndIf;

          Else;
             sqlStmtStr = %Trim(sqlStmtStr) + ' Where ST1a5<>''Y''';
          EndIf;

       Endsr;

       //=================================================================
       // Build the State json record
       //=================================================================
       Begsr BldStateRcd;

        json = %trim(json) + ' { ' +
           %Trim(BuildJsonNode('stateCode':%Trim(STkey)))  + ',' +
           %Trim(BuildJsonNode('stateName':%Trim(STname))) + ' }';

       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;

        json = %trim(json) + '"requestParms":{' +
         %trim(BuildJsonNode('stateCode':%Trim(dsFilter.stateCode))) + ', '  +
         %trim(BuildJsonNode('stateName':%Trim(dsFilter.StateName))) + ', '  +
         %trim(BuildJsonNode('status':dsFilter.status)) + ', '               +
         %trim(BuildJsonNode('wcmcInstalled':dsFilter.wcmcInstalled)) + ', ' +
         %trim(BuildJsonNode('wcmcEdi':dsFilter.wcmcEdi))          + ', '    +
         %trim(BuildJsonNode('omitted':dsFilter.omitted))          + '}, '   +
         '"stateList": [';

        Exsr ProcessSTSTP;

        json = %trim(json) + ']';

        Exec SQL close stateCursor;

        If strErrMsgs <> *blanks;
           json = %trim(json) + ', "errorMsg":[' + %trim(strErrMsgs) + ']';
        Endif;

        // json = %trim(json) + ' }';

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg);
          If errMsg = ' ';
             // State code
             node = YAJL_object_find(docNode: 'stateCode');
             dsFilter.stateCode = YAJL_get_string(node);
             // State Name
             node = YAJL_object_find(docNode: 'stateName');
             dsFilter.stateName = YAJL_get_string(node);
             // State Status
             node = YAJL_object_find(docNode: 'status');
             dsFilter.status = YAJL_get_string(node);
             // WCMC Installed State
             node = YAJL_object_find(docNode: 'wcmcInstalled');
             dsFilter.wcmcInstalled = YAJL_get_string(node);
             // WCMC EDI  State
             node = YAJL_object_find(docNode: 'wcmcEdi');
             dsFilter.wcmcEdi = YAJL_get_string(node);
             // Ommitted States
             node = YAJL_object_find(docNode: 'omitted');
             dsFilter.omitted = YAJL_get_string(node);
          Endif;

       Endsr;

       //=================================================================
       // Process STSTP
       //=================================================================
       Begsr ProcessSTSTP;

          // Build agency sql statement
          ExSr BldStateSQL;

          // Prepare the SQL statement
          Exec SQL prepare stateStmt from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare stateCursor cursor for stateStmt;

          // Run SQL
          Exec SQL open stateCursor;

          // Get first record if there is one
          Exec SQL fetch next from stateCursor into :dsSTSTP;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             moreRows = *off;
             Exec SQL close stateCursor;
          EndIf;

          // If row was fetched then process
          DoW moreRows = *On;

             // Build Record
             Exsr BldStateRcd;

             // Fetch next Record
             Exec SQL fetch next from stateCursor into :dsSTSTP;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                moreRows = *Off;
                Exec SQL close stateCursor;
             Else;
                // Prepare for next record
                json = %Trim(json) + ' ,';
             EndIf;
          EndDo;

       Endsr;
      /End-Free

     PListStateCodes...
     P                 e
     P*==================================================================
     P* List Quote Status Codes
     P*==================================================================
     PListQteStsCodes...
     P                 b
     DListQteStsCodes...
     D                 pi

     Dfilter           s               n   inz(*Off)
     Di1               s             10i 0 inz
     Di2               s             10i 0 inz
     DinRtCd           s              3
     DisContinue       s              1n   inz('1')
     DisJsonExt        s              1n   inz('0')
     DisJsonClass      s              1n   inz('0')
     DmoreRows         s               n   Inz(*On)
     DstrJsonExt       s          32000    inz
     DsqlStmtStr       s            500

     DdsWTQSP        e ds                  extname(WTQSP)

     DdsFilter         ds                  inz qualified
     D company#                       3
     D quoteSts                       1
     D quoteDesc                     30
     D quoteFlag                      1
     D status                         1                                         ST1a3 = 'Y'

      /Free

       Exsr GetParms;
       Exsr BuildOutput;

       //=================================================================
       // Build the Quote Status Sql Statement
       //=================================================================
       Begsr BldQteStsSQL;

          sqlStmtStr = 'Select * from WTQSP';

          If dsFilter.company# = ' ';
             If UserCo#(inUser) <> 0;
                sqlStmtStr =  %Trim(sqlStmtStr) + ' Where QSco#=' +
                     %EditC(UserCo#(inUser):'X');
                filter = *On;
             EndIf;
          EndIf;

          // Add on Filters
          // Start Where
          If dsFilter.company#  <> ' ' Or dsFilter.quoteSts  <> ' ' Or
             dsFilter.quoteDesc  <> ' ' Or dsFilter.quoteFlag <> ' ' Or
             dsFilter.status <> ' ';

             If filter = *Off;
                sqlStmtStr =  %Trim(sqlStmtStr) + ' Where';
             EndIf;

             // Company#
             If dsFilter.company# <> ' ';
                sqlStmtStr  = %Trim(sqlStmtStr) + ' QSco#=' +
                  dsFilter.company#;
                filter = *On;
             EndIf;

             // Quote Status Code
             If dsFilter.quoteSts <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And QSkey=''' + dsFilter.quoteSts + '''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' QSkey=''' + dsFilter.quoteSts + '''';
                   filter = *On;
                EndIf;
             EndIf;

             // Quote Status Descrption
             If dsFilter.quoteDesc <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And Upper(QSdesc) like ''%' +
                      %Trim(Upper(dsFilter.quoteDesc)) + '%''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' Upper(QSdesc) like ''%' +
                       %Trim(Upper(dsFilter.quoteDesc)) + '%''';
                   filter = *On;
                EndIf;
             EndIf;

             // Quote Flag Filter
             If dsFilter.quoteFlag <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And QSqte=''' + dsFilter.quoteFlag + '''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' QSqte=''' + dsFilter.quoteFlag + '''';
                   filter = *On;
                EndIf;
             EndIf;

             // Status Filter
             If dsFilter.status <> ' ';
                If filter;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And STstat=''' + dsFilter.status + '''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' STstat=''' + dsFilter.status + '''';
                   filter = *On;
                EndIf;
             EndIf;

          EndIf;

       Endsr;

       //=================================================================
       // Build the Quote Status json record
       //=================================================================
       Begsr BldQteStsRcd;

        json = %trim(json) + ' { ' +
           %Trim(BuildJsonNode('quoteStsCode':%Trim(QSkey)))  + ',' +
           %Trim(BuildJsonNode('quoteStsDesc':%Trim(QSdesc))) + ' }';

       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;

        json = %trim(json) + '"requestParms":{' +
         %trim(BuildJsonNode('company#':dsFilter.company#))          + ', '  +
         %trim(BuildJsonNode('quoteSts':%Trim(dsFilter.quoteSts)))   + ', '  +
         %trim(BuildJsonNode('quoteDesc':%Trim(dsFilter.quoteDesc))) + ', '  +
         %trim(BuildJsonNode('quoteFlag':dsFilter.quoteFlag))        + ', '  +
         %trim(BuildJsonNode('status':dsFilter.status))              + '}, ' +
         '"quoteStsList": [';

        Exsr ProcessWTQSP;

        json = %trim(json) + ']';

        Exec SQL close qteStsCursor;

        If strErrMsgs <> *blanks;
           json = %trim(json) + ', "errorMsg":[' + %trim(strErrMsgs) + ']';
        Endif;

        // json = %trim(json) + ' }';

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg);
          If errMsg = ' ';
             // Company #
             node = YAJL_object_find(docNode: 'company#');
             dsFilter.company# = YAJL_get_string(node);
             // Quote Status Code
             node = YAJL_object_find(docNode: 'quoteSts');
             dsFilter.quoteSts = YAJL_get_string(node);
             // Quote Status Description
             node = YAJL_object_find(docNode: 'quoteDesc');
             dsFilter.quoteDesc = YAJL_get_string(node);
             // Quote Status Flag
             node = YAJL_object_find(docNode: 'quoteFlag');
             dsFilter.quoteFlag = YAJL_get_string(node);
             // Status
             node = YAJL_object_find(docNode: 'status');
             dsFilter.status = YAJL_get_string(node);
          Endif;

       Endsr;

       //=================================================================
       // Process WTQSP
       //=================================================================
       Begsr ProcessWTQSP;

          // Build agency sql statement
          ExSr BldQteStsSQL;

          // Prepare the SQL statement
          Exec SQL prepare qteStsStmt from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare qteStsCursor cursor for qteStsStmt;

          // Run SQL
          Exec SQL open qteStsCursor;

          // Get first record if there is one
          Exec SQL fetch next from qteStsCursor into :dsWTQSP;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             moreRows = *off;
             Exec SQL close qteStsCursor;
          EndIf;

          // If row was fetched then process
          DoW moreRows = *On;

             // Build Record
             Exsr BldQteStsRcd;

             // Fetch next Record
             Exec SQL fetch next from qteStsCursor into :dsWTQSP;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                moreRows = *Off;
                Exec SQL close qteStsCursor;
             Else;
                // Prepare for next record
                json = %Trim(json) + ' ,';
             EndIf;
          EndDo;

       Endsr;
      /End-Free

     PListQteStsCodes...
     P                 e

     P*==================================================================
     P* List Table Values
     P*==================================================================
     PListTableValues...
     P                 b

     dstrDtCompare     s              7a   inz
     dstrErrorMsg      s            500a   varying inz
     dstrSql           s           5000a   varying inz
     dstrTable         s            100a   varying inz

     ddsTable          ds                  qualified inz
     d code                          25a   varying
     d desc                         100a   varying
     d status                         1a
      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       //------------------------
       Begsr BuildOutput;
        // SIC, NAIC, , Nature, Part, Cause, WagePeriod, InitialTreatment, Gender
        Select;
        When ioRtCd = 'Error';
           Leavesr;
        When strTable = 'Cause';
           Exsr BuildSqlCause;
        When strTable = 'Gender';
           Exsr BuildSqlGender;
        When strTable = 'InitialTreatment';
           Exsr BuildSqlInitialTreatment;
        When strTable = 'NAIC';
           Exsr BuildSqlNAIC;
        When strTable = 'Nature';
           Exsr BuildSqlNature;
        When strTable = 'Part';
           Exsr BuildSqlPart;
        When strTable = 'PayrollClassifications';
           Exsr BuildSqlPayrollClassifications;
        When strTable = 'SIC';
           Exsr BuildSqlSIC;
        When strTable = 'State';
           Exsr BuildSqlState;
        When strTable = 'TypeOfBusiness';
           Exsr BuildSqlTypeOfBusiness;
        When strTable = 'WagePeriod';
           Exsr BuildSqlWagePeriod;
        When strTable = *blanks;
           strTable = 'STUB';
           Exsr BuildStub;
        Other;
           Exsr BuildStub;
        Endsl;
        Exsr ProcessSQL;
       Endsr;
       //------------------------
       Begsr BuildSqlCause;
        strSql = 'select ' +
         'cast(CAKEY as varchar(25)), cast(CADESC as varchar(100)), CASTAT' +
         ' from WTCAP ' +
         ' where CACO# = ' + strCo# +
         ' order by CAKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlGender;
        strSql = 'select ' +
         'cast(GTKEY as varchar(25)), cast(GTDESC as varchar(100)), GTSTAT' +
         ' from WTGTP ' +
         ' where GTCO# = ' + strCo# + ' and GTTBID = ''GEND''' +
         ' order by GTKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlInitialTreatment;
        strSql = 'select ' +
         'cast(GTKEY as varchar(25)), cast(GTDESC as varchar(100)), GTSTAT' +
         ' from WTGTP ' +
         ' where GTCO# = ' + strCo# + ' and GTTBID = ''INIT''' +
         ' order by GTKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlNAIC;
        strSql = 'select ' +
         'cast(GTKEY as varchar(25)), cast(GTDESC as varchar(100)), GTSTAT' +
         ' from WTGTP ' +
         ' where GTCO# = ' + strCo# + ' and GTTBID = ''NAIC''' +
         ' order by GTKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlNature;
        strSql = 'select ' +
         'cast(NAKEY as varchar(25)), cast(NADESC as varchar(100)), NASTAT' +
         ' from WTNAP ' +
         ' where NACO# = ' + strCo# +
         ' order by NAKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlPart;
        strSql = 'select ' +
         'cast(PAKEY as varchar(25)), cast(PADESC as varchar(100)), PASTAT' +
         ' from WTPAP ' +
         ' where PACO# = ' + strCo# +
         ' order by PAKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlPayrollClassifications;
        strDtCompare = %char(sysDate7);
        strSql = 'with ' +
         'max_date as ( ' +
         ' select PRCLAS, max(PREFDT) eff_date ' +
         '  from WMPRP ' +
         '  where PRCO# = ' + strCo# + ' and ' +
         '   (PREFDT <= ' + strDtCompare + ' and ' +
         '   (PREXDT >= ' + strDtCompare + ' or PREXDT = 0)) ' +
         '  group by PRCLAS ' +
         '  order by PRCLAS), ' +
         'valid_codes as ( ' +
         ' select a.PRCLAS class, a.PREFDT, ' +
         '  cast(lpad(a.PRCLAS,5,''0'') as varchar(25)) code, ' +
         '  cast(a.PRDESC as varchar(100)) desc, a.PRSTAT status ' +
         '  from WMPRP a ' +
         '  join max_date m on (a.PRCLAS = m.PRCLAS and a.PREFDT = m.eff_date)'+
         '  where PRCO# = ' + strCo# +
         '  order by a.PRCLAS), ' +
         'extensions as ( ' +
         ' select ' +
         ' cast(LPAD(PXCLAS,5,''0'') || '' '' || PXEXT as varchar(25)) code, ' +
         ' cast(PXDESC as varchar(100)) desc, PXSTAT status ' +
         ' from WMPXP ' +
         ' where PXCO# = ' + strCo# + ' and ' +
         '  PXCLAS in (select class from valid_codes) and ' +
         '   (PXEFDT <= ' + strDtCompare + ' and ' +
         '   (PXEXDT >= ' + strDtCompare + ' or PXEXDT = 0)) ' +
         '  order by PXCLAS, PXEXT) ' +
         'select code, desc, status from valid_codes ' +
         ' union ' +
         'select code, desc, status from extensions ' +
         ' order by code ';
       Endsr;
       //------------------------
       Begsr BuildSqlSIC;
        strSql = 'select ' +
         'cast(GTKEY as varchar(25)), cast(GTDESC as varchar(100)), GTSTAT' +
         ' from WTGTP ' +
         ' where GTCO# = ' + strCo# + ' and GTTBID = ''SIC''' +
         ' order by GTKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlState;
        strSql = 'select ' +
         'cast(GTKEY as varchar(25)), cast(GTDESC as varchar(100)), GTSTAT' +
         ' from WTGTP ' +
         ' where GTCO# = ' + strCo# + ' and GTTBID = ''WCST''' +
         ' order by GTKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlTypeOfBusiness;
        strSql = 'select ' +
         'cast(GTKEY as varchar(25)), cast(GTDESC as varchar(100)), GTSTAT' +
         ' from WTGTP ' +
         ' where GTCO# = ' + strCo# + ' and GTTBID = ''BUSI''' +
         ' order by GTKEY';
       Endsr;
       //------------------------
       Begsr BuildSqlWagePeriod;
        strSql = 'select ' +
         'cast(GTKEY as varchar(25)), cast(GTDESC as varchar(100)), GTSTAT' +
         ' from WTGTP ' +
         ' where GTCO# = ' + strCo# + ' and GTTBID = ''FRWP''' +
         ' order by GTKEY';
       Endsr;
       //------------------------
       // Build stub results
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' + %trim(q1Pgm) +
         ') successfully executed. {ListTableValues - ' + strTable + '}');
       Endsr;
       //------------------------
       Begsr GetParms;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> *blank;
           AddErrorMessageJson(strErrorMsg:ioRtCd);
           Leavesr;
        Endif;
        json += '"requestParms" : {';
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'table';
              strTable = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //------------------------
       Begsr ProcessSQL;
        If strSql = *blank;
           Leavesr;
        Endif;
        Exec SQL close cursorTable;
        Exec SQL prepare table from :strSql;
        Exec SQL declare cursorTable cursor for table;
        Exec SQL open cursorTable;
        Reset dsTable;
        json += ',"tableValueList": [';
        Dou not IsValidFetch(sqlstt);
           Exec SQL fetch next from cursorTable into :dsTable;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           When dsTable.status <> 'A';
              Iter;
           Endsl;
           json += BuildJsonCode(dsTable.code:dsTable.desc) + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json));
        json += ']';
        Exec SQL close cursorTable;
       Endsr;
      /End-free
     PListTableValues...
     P                 e
