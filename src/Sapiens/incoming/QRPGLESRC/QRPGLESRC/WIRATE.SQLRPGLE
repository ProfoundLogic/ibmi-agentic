     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Description:  Web Service to rate quotes/policies and return info.
     H*
     H* Project: SY406   Date: 08/16/18
     H* Pgmr:    C:65
     H*
     H*====================================================================
     H* inParms -
     H*   name:value
     H*
     H*====================================================================
     H* Revisions:
     H*
     H* 11/11/24 C:333 Project: WC2985
     H*                Apply Deductible to Discounted Premium.
     H*                - change is present in the copybook WIRATESRC.
     H*
     H* 04/18/24 C:291 Project: WC2961
     H*                Recompile for changes done in WIRATESRC for
     H*                Schedule DB/CR applied against Manual Premium.
     H*
     H* 11/04/22 C:65  Project: WC2663     (Inhouse)
     H*                Recast LCM returned from SRLCM to be integer and
     H*                divide by 10000 to create the 4 decimal place value.
     H*
     H* 09/23/22 C:65  Project: Phw#103712 (Inhouse)
     H*                Update the output of payroll rate to return the rate
     H*                from employe payroll file if calculation has been
     H*                done or policy and class code is 'A' rated.  Return
     H*                modified base rate (by applying LCM, TDF, rate dev)
     H*                otherwise.  Coding changes in copybook primarly.
     H*
     H*====================================================================
     Fwdell1    if   e           k disk    rename(pel01:pel01) infds(elfds)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fwieml1    uf   e           k disk    rename(pem01:pem01)
     F                                     prefix(EM)
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fwmeple    IF   E           K DISK    rename(pep01:pep0e)
     Fwmrsl     IF   E           K DISK    rename(prs01:prs00)
     Fwmldl1    IF   E           K DISK    rename(pld01:pld01)
     Fwdrrl     IF   E           K DISK    rename(prr01:prr00)
     Fwtlml1    IF   E           K DISK    rename(plm01:plm01)
     Fwtddl2    IF   E           K DISK    rename(pdd01:pdd02)
     FWMPRL1    IF   E           K DISK    rename(ppr01:ppr01)
     FWMPXL1    IF   E           K DISK    rename(ppx01:ppx01)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PSY01)

     D* this size needs to change
     Darray            s               a   len(32767)
     DinJson           s               a   len(2000000) varying
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying
     DjsonData         s               a   len(8500000) varying
     Dmode             s              1a
     Dstatus           s              1a
     DuserType         s             25a
     dcomma            s              1a
     dcomma1           s              1a
     dcommaDiv         s              1a
     dcommaPol         s              1a
     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')

     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')

     D docNode         s                   like(yajl_val)
     D list            s                   like(yajl_val)
     D key             s             50a   varying
     D node            s                   like(yajl_val)
     D val             s                   like(yajl_val)

     D result          ds                  Qualified
     D  fnd                           3a
     D  emp#                          9a
     D  fyr                           3a
     D  co#                           3a
     D  quote                         1a
     D  postDate                      8a
     D  calculate                     1a
     D  payroll                       1a
     D  userType                     25a

     dclientID         s              3a
     dkeyCo#           s              3s 0
     dkeyFnd           s              3s 0
     dkeyEmp#          s              9s 0
     dkeyFyr           s              3s 0
     dqqCo#            s              3a
     dqqFnd            s              3a
     dqqEmp#           s              9a
     dqqFyr            s              3a
     dqqDiv            s              5a
     dquote            s              1a
     dcalcFlag         s              1a
     dpayFlag          s              1a
     dxxLarge          s              1a
     dxxKey            s             10a
     dxxEFDT           s              7s 0
     dxtOccd           s             11s 2
     dxtNrat           s             11s 2
     dxtAdld           s             11s 2
     dxxPremium        s             15s 2
     disContri         s              1
     dpremWord         s             12
     dpostDate         s              8s 0
     dpostChar         s              8a
     defdtChar         s              8a
     destCode          s              2a
     ddepCode          s              2a
     dadjCode          s              2a
     dARCode           s              2a
     dAR2Code          s              2a
     dcomment1         s              2a
     dcomment2         s              2a
     dcomment3         s              2a
     dfreeCom1         s             30a
     dfreeCom2         s             30a
     dfreeCom3         s             30a
     ddueChar          s              8a
     ddepFlag          s              1a
     dstlFlag          s              1a
     dprtFlag          s              1a
     dovrFlag          s              1a
     dqteFlag          s              1a
     dqueue            s             10a
     drnbFlag          s              1a
     ddartFlag         s              1a
     dhardFlag         s              1a
     dagtFlag          s              1a
     dfinalQte         s              1a
     dremarkCode       s              5a
     dintCo#           s              3  0
     d strAuthority    s             10a
     dstrCo#           s              3a
     dstrStraightThru  s              1a
     d*** begin add ***  09/23/22  *******************************************
     DqqClas           s              5
     DqqEfdt           s              7
     DqqLCM            s              5
     dxxLCM            s              5  4
     dxxTDF            s              5  4
     dxxRDEV           s              5  4
     DxxARAT           s                   like(EPARAT)
     d*** end add *****  09/23/22  *******************************************

     dxtman            s             15s 2
     dxtincr           s             15s 2
     dxtsubj           s             15s 2
     dxtmod            s             15s 2
     dxtstan           s             15s 2
     dxtdisc           s             15s 2
     dxtnorm           s             15s 2
     dxtexp            s             15s 2
     dxtterr           s             15s 2
     dxtprem           s             15s 2
     dxtfee            s             15s 2
     dxttotal          s             15s 2

     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0

     D PSDS          ESDS                  extname(SZQ1P)
     D elfds           ds
     D elrrn                 397    400b 0

      // Effective Date
     d SrEffd          pr             7  0
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d inFYR                          3  0 const
     d inEMP#                         9  0 const
     d inDIV                          5  0 const
     d inDate                         7  0 const
     d inType                         1    options(*nopass) const
     d inClas                         5  0 options(*nopass) const
      // SReffd API prototype
     d SReffdAPI       pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

      // TemplateQuestion Validation
     d Validate        pr                  extpgm('WIVALID')
     d   ##mode                       1a   const
     d   ##CO#                        3s 0 const
     d   ##FND                        3s 0 const
     d   ##FYR                        3s 0 const
     d   ##EMP#                       9s 0 const
     d   ##userType                  25a   const
     d   ##status                     1a
     D   ##JSON                        a   len(8500000) varying

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrString
      /copy wPrGroup
      /copy wPrUsrPrms
      /include yajl_h

      *entry
     DRequestPgm       pr                  ExtPgm('WIRATE')
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


     d W8002C          pr                  ExtPgm('W8002C')
     d   inCo#                        3    const
     d   inFnd                        3    const
     d   inFyr                        3    const
     d   inEmp#                       9    const
     d   inDiv                        5    const
     d   inDate                       8    const
     d   inEstCode                    2    const
     d   inCom1                       2    const
     d   inCom2                       2    const
     d   inCom3                       2    const
     d   inDue                        8    const
     d   inDep                        1    const
     d   inStl                        1    const
     d   inPrt                        1    const
     d   inOvr                        1    const
     d   inSRCode                     2    const
     d   inAdjCode                    2    const
     d   inQuote                      1    const
     d   inQueue                     10    const
     d   inRNB                        1    const
     d   inDart                       1    const
     d   inHard                       1    const
     d   inAgtOrder                   1    const
     d   inFinalQte                   1    const
     d   inRMCD                       5    const
     d   inARCode                     2    const
     d   inEffDate                    8    const
     d   inFreeCom1                  30    const
     d   inFreeCom2                  30    const
     d   inFreeCom3                  30    const
     d   inARCode2                    2    const
     d*** begin add ***  09/23/22  *******************************************
     d SRLCM           pr                  extpgm('SRLCM')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##RTCD                       3
     d   ##CLAS                       5
     d   ##EFDT                       7
     d   ##LCM                        5
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d*** end add *****  09/23/22  *******************************************

      /Free
       Exec Sql Set Option Commit = *None;

       Chain (inUser) pup00;

       Clear json;
       Exsr PgmStart;
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr BuildOutput;
       Endif;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

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
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;

       //* Run calculations unless indicated.
        If calcFlag = 'Y';
           Setll (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
           Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;

           //* Run billing calculation for quote/policy from state rpt div.
           Dow not %eof;
              Chain (keyCo#:keyFnd:keyEmp#:ELDiv) pem00;
              If EMStrp = 'Y';
                 Exsr Calculate;
                 Leave;
              Endif;
              Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
           Enddo;

           //* Run the billing calculations if state reporting division not found.
           If EMStrp <> 'Y';
              Exsr Calculate;
           Endif;

           If ioRtnCode = 'Error';
              Leavesr;
           Endif;
        Endif;

       //* Post-premium validation.
        Clear jsonData;
        Clear status;
        mode = ' ';

        Validate(mode:keyCo#:keyFnd:keyFyr:keyEmp#:userType:status:jsonData);
        strStraightThru = ChkStraightThru(keyCo#:keyFnd:keyEmp#:jsonData);

        Chain (keyCo#:keyFnd:keyEmp#:keyFyr) pem01;
        If %found;
           EMEMValid = status;
           EMEMValRsn = %trim(jsonData);
           Update pem01;
        Endif;

        isContri = GetPrmContFlg(ELCo#:ELFnd);
        If isContri = '1';
           premWord = 'Contribution';
        Else;
           premWord = 'Premium';
        Endif;

        //* Summary fields
        Exsr ClearSummary;

        Clear xxPremium;
        //* Report out the data for quote/policy.
        Setll (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Dow not %eof;
           xxPremium = xxPremium + ELANPR;
           //* Summary totals
           Exsr SumPremium;

           Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Enddo;

        Clear commaPol;
        json += ', "premiumInfo" : [';

        //* Build policy level info and start division array.
        Exsr BuildHeader;

        //* Report out the data for quote/policy.
        Setll (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;

        Dow not %eof;

           Clear xtoccd;
           Clear xtnrat;
           Clear xtadld;

        //* Set up tables/settings
           Chain(n) (ELCO#:ELFND) pex00;
           Chain(n) (ELCO#:ELRTCD) prs00;
           Setgt (ELCO#:ELFND:RSJRST:ELPFDT) prr00;
           Readpe(n) (ELCO#:ELFND:RSJRST) prr00;
           xxEFDT = SrEffd(ELCO#:ELFND:ELFYR:ELEMP#:ELDIV:ELPFDT);
           xxKey = %editc(ELBSCD : 'X');
           Clear xxLarge;
           Chain(n) (ELCO#:xxKey) pbs01;
           If BSLPGM = 'L';
              Chain(n) (ELCO#:ELFND:ELEMP#:ELDIV:ELFYR) pld01;
              If %found;
                 xxLarge = 'Y';
              Endif;
           Endif;

        //* Write info to json
           Exsr BuildRecord;

           Reade (keyCo#:keyFnd:keyFyr:keyEmp#) pel01;
        Enddo;

        //* End of division array
        json = %trim(json) + ' ]';

        //* End of data structure for policy
        json = %trim(json) + ' } ';

        //* End of premiumInfo overall structure
        json = %trim(json) + ' ]';

       Endsr;

       //=================================================================
       // Call Billing to calculate premium.
       //=================================================================
       Begsr Calculate;

        qqCo#  = %editc(ELCo#:'X');
        qqFnd  = %editc(ELFnd:'X');
        qqFyr  = %editc(ELFyr:'X');
        qqEmp# = %editc(ELEmp#:'X');
        qqDiv  = %editc(ELDiv:'X');

        If postDate < ELBSDT;
           postDate = DtMDYY(ELBSDT);
        Endif;
        postChar = %editc(postDate:'X');

        estCode = '  ';
        depCode = '  ';
        adjCode = '  ';
        ARCode  = '  ';
        AR2Code = '  ';

        dueChar = '00000000';
        depFlag = 'Y';
        stlFlag = 'E';
        prtFlag = '2';
        ovrFlag = ' ';
        qteFlag = quote;
        queue = UPOutq;
        rnbFlag = ' ';
        dartFlag = 'N';
        hardFlag = 'N';
        agtFlag = 'N';
        finalQte = 'N';
        remarkCode = '     ';
        efdtChar = '00000000';
        freeCom1 = *blanks;
        freeCom2 = *blanks;
        freeCom3 = *blanks;

        Monitor;
           W8002C(qqCO#:qqFND:qqFYR:qqEMP#:qqDIV:postChar:estCode:comment1:
            comment2:comment3:dueChar:depFlag:stlFlag:prtFlag:ovrFlag:depCode:
            adjCode:qteFlag:queue:rnbFlag:dartFlag:hardFlag:agtFlag:finalQte:
            remarkCode:ARCode:efdtChar:freeCom1:freeCom2:freeCom3:AR2Code);
        On-error;
           AddErrorMessageJson('Billing CL failed, see job log for ' +
            %editc(Q1JobN:'X') + '/' + %trim(Q1CUsr) + '/' + %trim(Q1Job) +
            ' Error: ' + %trim(RmvExceptJSON(%trim(Q1RED))):ioRtnCode);
        Endmon;

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

          Clear result;
          Clear errMsg;
          Clear errMsg500;
          Clear comma;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          errMsg = errMsg500;

          // Parse the parameters if no error from YAJL.
          If errMsg = ' ';

             json = %trim(json) + ' "requestParms" : { ';

             // Load the data structure with the parameter values
             // from the tree
             node = YAJL_object_find(docNode: 'group');
             result.fnd       = YAJL_get_string(node);
             BuildGiantJSON(json :'group' :%trim(result.fnd));

             node = YAJL_object_find(docNode: 'employer');
             result.emp#      = YAJL_get_string(node);
             BuildGiantJSON(json :'employer' :%trim(result.emp#));

             node = YAJL_object_find(docNode: 'year');
             result.fyr       = YAJL_get_string(node);
             BuildGiantJSON(json :'year' :%trim(result.fyr));

             node = YAJL_object_find(docNode: 'company');
             result.co#       = YAJL_get_string(node);
             BuildGiantJSON(json :'company' :%trim(result.co#));

             node = YAJL_object_find(docNode: 'quote');
             result.quote     = YAJL_get_string(node);
             BuildGiantJSON(json :'quote' :%trim(result.quote));

             node = YAJL_object_find(docNode: 'postDate');
             result.postDate  = YAJL_get_string(node);
             BuildGiantJSON(json :'postDate' :%trim(result.postDate));

             node = YAJL_object_find(docNode: 'calculate');
             result.calculate = YAJL_get_string(node);
             BuildGiantJSON(json :'calculate' :%trim(result.calculate));

             node = YAJL_object_find(docNode: 'payroll');
             result.payroll   = YAJL_get_string(node);
             BuildGiantJSON(json :'payroll' :%trim(result.payroll));

             node = YAJL_object_find(docNode: 'userType');
             result.userType  = YAJL_get_string(node);
             BuildGiantJSON(json :'userType' :%trim(result.userType));

             json = %trim(json) + ' } ';

             // Free up the space by removing the tree
             yajl_tree_free(docNode);
          Else;
             AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
             Leavesr;
          EndIf;

          If strAuthority = '*View';
             AddErrorMessageJson('User, ' + %trim(inUser) +
              ', not authorized to process ' +
              'request (' + %trim(inRequest) + ').'
              :ioRtnCode);
             Leavesr;
          Endif;

          //* Fund
          If result.fnd <> ' ';
             Monitor;
                keyFnd = %int(result.fnd);
             On-error;
                AddErrorMessageJson('Invalid group received in incoming ' +
                 'parameters: ' + result.fnd:ioRtnCode);
             Endmon;
          Else;
             Clear keyFnd;
          Endif;

          //* Emp#
          If result.emp# <> ' ';
             Monitor;
                keyEmp# = %int(result.emp#);
             On-error;
                AddErrorMessageJson('Invalid employer received in incoming ' +
                 'parameters: ' + result.emp#:ioRtnCode);
             Endmon;
          Else;
             Clear keyEmp#;
          Endif;

          //* Fyr
          If result.fyr <> ' ';
             Monitor;
                keyFyr = %int(result.fyr);
             On-error;
                AddErrorMessageJson('Invalid year received in incoming ' +
                 'parameters: ' + result.fyr:ioRtnCode);
             Endmon;
          Else;
             Clear keyFyr;
          Endif;

          //* Company
          If result.co# <> ' ';
             Monitor;
                keyCo# = %int(result.co#);
             On-error;
                AddErrorMessageJson('Invalid company received in incoming ' +
                 'parameters: ' + result.co#:ioRtnCode);
             Endmon;
          Else;
             keyCo# = UPCo#;
          Endif;

          //* Quote Status
          If result.quote <> ' ';
             quote = %trim(result.quote);
          Else;
             Clear quote;
          Endif;

          //* Posting Date
          If result.postDate <> ' ';
             Monitor;
                postDate = %int(result.postDate);
             On-error;
                AddErrorMessageJson('Invalid postDate received in incoming ' +
                 'parameters: ' + result.postDate:ioRtnCode);
             Endmon;
          Else;
             postDate = Dateto8(SystemDate());
          Endif;

          //* Calculate premium
          If result.calculate = 'Y' or result.calculate = 'N';
             calcFlag = %trim(result.calculate);
          Else;
             calcFlag = 'Y';
          Endif;

          //* Export payroll rcds.
          If result.payroll = 'Y' or result.payroll = 'N';
             payFlag = %trim(result.payroll);
          Else;
             payFlag = 'Y';
          Endif;

          //* User Type
          If result.userType <> ' ';
             userType = %trim(result.userType);
          Else;
             Clear userType;
          Endif;

          Select;
          When quote = 'Q' and not UseGroupInPortal(keyCo#:keyFnd:'Quote');
             AddErrorMessageJson('Group ' + %char(keyFnd) +
              ' is not included for service requested ' +
              '(' + %trim(inRequest) + ').':ioRtnCode);
          When quote <> 'Q' and not UseGroupInPortal(keyCo#:keyFnd:'Policy');
             AddErrorMessageJson('Group ' + %char(keyFnd) +
              ' is not included for service requested ' +
              '(' + %trim(inRequest) + ').':ioRtnCode);
          Endsl;

       Endsr;

      /End-free

     c/copy wiRateSrc

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

       //===================================================================
       // Check straight through processing based on Validate return json
       //===================================================================
     pChkStraightThru  b
     dChkStraightThru  pi             1
     D inCo#                          3s 0
     D inFnd                          3s 0
     D inEmp#                         9s 0
     d inJson                          a   len(8500000) varying

     di0               s             10i 0 inz
     disUpdate         s              1n   inz
     dnode0            s                   like(yajl_val)
     dstrStraightThru  s              1    inz

     ddsTemplate       ds                  inz qualified
     d status                        10    varying
     d type                          10    varying
      /Free
       node0 = yajl_string_load_tree(inJson : errMsg500);
       Dow YAJL_object_loop(node0:i0:key:val);
          Select;
          When key = 'status';
             dsTemplate.status = YAJL_get_string(val);
          When key = 'type';
             dsTemplate.type = YAJL_get_string(val);
          Endsl;
       Enddo;
       Select;
       When Upper(dsTemplate.status) = 'SUCCESS'
        and Upper(dsTemplate.type) = 'QUOTING';
          strStraightThru = 'Y';
       Endsl;
       Select;
       When Upper(userType) <> 'COMPANY USER';
          isUpdate = *on;
       Endsl;
       If isUpdate;
          Exec sql
           update WMEMP
            set EMsTP = :strStraightThru,
             EMpgm = :q1Pgm, EMuser = :inUser,
             EMdt = :sysDate7, EMtime = :sysTime6
            where EMco# = :inCo# and EMfnd = :inFnd and EMemp# = :inEmp#;
       Endif;
       Return strStraightThru;
      /End-free
     pChkStraightThru  e

       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0 const
     d   inFND                        3  0 const
     d   inFYR                        3  0 const
     d   inEMP#                       9  0 const
     d   inDIV                        5  0 const
     d   inDate                       7  0                  const
     d   inType                       1    options(*nopass) const
     d   inClas                       5  0 options(*nopass) const
      //
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Date        s              7
     d   ##Type        s              1
     d   ##Clas        s              5
     d   outDate       s              7  0
      //

      /free

       ##Type = *blank;
       If %parms >= 7;
          ##Type = inType;
       Endif;

       ##Clas = *blanks;
       If %parms >= 8;
          ##Clas = %editc(inClas: 'X');
       Endif;

       ##CO# = %editc(inCO#: 'X');
       ##FND = %editc(inFND: 'X');
       ##FYR = %editc(inFYR: 'X');
       ##EMP# = %editc(inEMP#: 'X');
       ##DIV = %editc(inDIV: 'X');
       ##Date = %editc(inDate: 'X');

       SReffdAPI(##CO#:##FND:##FYR:##EMP#:##DIV:##Date:##Type:##Clas);

       outDate = %int(##Date);

       Return outDate;
      /end-free
     P SrEffd          E
