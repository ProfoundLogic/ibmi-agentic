     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*NO) ACTGRP(*CALLER)
     h OPTION(*NODEBUGIO: *SRCSTMT)
     H BNDDIR('WTBNDDIR':'YAJL') DECEDIT('0.')
      *====================================================================
      * Create on 04/04/2019 by C:136 for Project SY406
      *====================================================================
      * Description: Program to retrieve and save application comments
      *====================================================================
      *  Revisions:
      *
      *
      *====================================================================
     fwiappcomm uf a e           k disk    rename(pap01:pap0p)
     d psds          esds                  extname(SZQ1P)
      * Variables used for JSON processing
     d docNode         s                   like(yajl_val)
     d key             s             50a   varying
     d inJson          s               a   len(2000000) varying
     d i0              s             10i 0
     d i1              s             10i 0
     d i2              s             10i 0
     d json            s               a   len(8500000) varying inz
     d jsont           s               a   len(8500000) varying
     d jsonSub         s               a   len(8500000) varying
     d list            s                   like(yajl_val)
     d node            s                   like(yajl_val)
     d node0           s                   like(yajl_val)
     d node1           s                   like(yajl_val)
     d node2           s                   like(yajl_val)
     d val             s                   like(yajl_val)
      * Global program variables
     d array           s               a   len(32767)
     d clientID        s              3a
     d comma           s              1a
     d errMsgArray     s            500a   dim(999)
     d isoDate8        s              8s 0
     d ix              s              3s 0
     d intApp#         s              9s 0
     d intCo#          s              3s 0
     d intSeq#         s              6s 0
     d strAuth         s             10a
     d strApp#         s              9a
     d strCo#          s              3a
     d strErrorMsg     s            500a   Varying
     d sysDate7        s              7s 0
     d sysTime6        s              6s 0

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrString
      /copy sPrSql
      /copy wPrGroup
      /copy wPrUsrPrms
      /include yajl_h

     d GetClientID     pr                  Extpgm('RTVCLTID')
     d   outCltID                     3

      *Entry
     dRequestPgm       pr                  ExtPgm('WIAPPCOMM')
     d                              256
     d                               10
     d                                 a   Len(8500000) Varying
     d                                 a   Len(8500000) Varying
     d                               10

     dRequestPgm       pi
     d inRequest                    256
     d inUser                        10
     d inParms                         a   Len(8500000) Varying
     d outJSON                         a   Len(8500000) Varying
     d ioRtnCd                       10

      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;
       Select;
       When inRequest = 'ListAppComments';
          ListAppComments();
       When inRequest = 'SaveAppComments';
          SaveAppComments();
       EndSl;
       *Inlr = *On;
       //=========================================
       // Program Start Tasks
       //=========================================
       Begsr PgmStart;
        strAuth = ioRtnCd;
        Clear ioRtnCd;
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        isoDate8 = %Dec(%Date():*Iso);
        sysDate7 = %Dec(%Date():*Cymd);
        sysTime6 = %Dec(%Time():*hms);
       EndSr;
      /End-Free
       //=========================================
       // Retrieve application comments
       //=========================================
     p ListAppComments...
     p                 b
      /Free
       Select;
       When strAuth = *blanks;
       When strAuth = '*View';
       When strAuth = '*Update';
       When strAuth = '*All';
       Endsl;
       Exsr GetParms;
       Exsr BuildReturnJson;
       outJson = json;
       Return;
       //-----------------------------------------
       Begsr GetParms;
       //-----------------------------------------
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           node = YAJL_object_find(docNode:'applicationNumber');
           strApp# = YAJL_get_string(node);
           Monitor;
              intApp# = %Int(strApp#);
           On-Error;
              errMsgArray(1) = '"Application ID has character data."';
              ioRtnCd = 'Error';
           EndMon;
        Else;
           errMsgArray(1) = '"JSON filter can''t be read."';
           ioRtnCd = 'Error';
        Endif;
       Endsr;
       //-----------------------------------------
       Begsr BuildReturnJson;
       //-----------------------------------------
        json += '"requestParms" : {' +
         %Trim(BuildJsonNode('applicationNumber':strApp#)) + '},' +
         '"applicationComments":{"applicationNumber":' + '"' +
         %Trim(strApp#) + '",' + '"comments":[';
        Exsr ListComments;
        json += ']}';
        If ioRtnCd = 'Error';
           comma = *Blank;
           For i0 = 1 to %Elem(errMsgArray);
              If errMsgArray(i0) = *Blanks;
                 Leave;
              EndIf;
              If comma <> *Blank;
                 array = %Trim(array) + comma;
              Else;
                 comma = ',';
              Endif;
              array = %Trim(array) + errMsgArray(i0);
           EndFor;
           i0 = %CheckR(' ':json);
           Select;
           When i0 = 0;
           Other;
              If %Subst(json:i0:1) <> ',';
                 json = %Trim(json) + ',';
              Endif;
           Endsl;
           json = %trim(json) + ' "errorMsg": [ ' + %trim(array) + ' ] ';
        EndIf;
       Endsr;
       //----------------------------------------
       Begsr ListComments;
       //----------------------------------------
        If ioRtnCd = 'Error';
           LeaveSr;
        Endif;
        comma = *Blank;
        Setll (intCo#:intApp#) pap0p;
        Dou %Eof(WIAPPCOMM);
           Reade (intCo#:intApp#) pap0p;
           If %Eof;
              Leave;
           Endif;
           If comma <> *Blank;
              json += comma;
           Else;
              comma = ',';
           EndIf;
           json += '{';
           json += '"comment":"' + APcomm + '",';
           json += '"timeStamp":"' + APtmStmp + '",';
           json += '"user":"' + APuserId + '"';
           json += '}';
        Enddo;
       Endsr;
      /End-Free
     p ListAppComments...
     p                 e
       //=========================================
       // Save application comments
       //=========================================
     p SaveAppComments...
     p                 b

     d dsIn            ds                  Inz Qualified Dim(99)
     d  comment                            Like(APcomm)
     d  timeStamp                    14a
     d  userId                             Like(APuserId)

     d inttest         s              6s 0

      /Free
       Select;
       When strAuth = *blanks;
       When strAuth = '*View';
          AddErrorMessageJson('User, ' + %trim(inUser) +
           ', not authorized to process ' +
           'request (' + %trim(inRequest) + ').'
           :ioRtnCd);
          outJson += ProcessErrorMessagesJson();
          Return;
       When strAuth = '*Update';
       When strAuth = '*All';
       Endsl;
       Exsr GetParms;
       Exsr BuildReturnJson;
       outJson = json;
       Exsr DataSave;
       Return;
       //----------------------------------------
       Begsr GetParms;
       //----------------------------------------
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           node0 = YAJL_object_find(docNode:'applicationComments');
           i0 = 0;
           Dow YAJL_object_loop(node0:i0:key:val);
              Select;
              When key = 'applicationNumber';
                 strApp# = YAJL_get_string(val);
                 Monitor;
                    intApp# = %Int(strApp#);
                 On-Error;
                    errMsgArray(1) = '"Application ID has character data."';
                    ioRtnCd = 'Error';
                    Leave;
                 EndMon;
              When key = 'comments';
                 node1 = YAJL_object_find(node0:key);
                 i1 = 0;
                 Dow YAJL_array_loop(node1:i1:node2);
                    i2 = 0;
                    Dow YAJL_object_loop(node2:i2:key:val);
                       Select;
                       When key = 'comment';
                          dsIn(i1).comment = YAJL_get_string(val);
                       When key = 'timeStamp';
                          dsIn(i1).timeStamp = YAJL_get_string(val);
                       When key = 'user';
                          dsIn(i1).userId = YAJL_get_string(val);
                       Endsl;
                    Enddo;
                 Enddo;
              Endsl;
           Enddo;
        Else;
           errMsgArray(1) = '"JSON filter can''t be read."';
           ioRtnCd = 'Error';
        Endif;
       Endsr;
       //----------------------------------------
       Begsr BuildReturnJson;
       //----------------------------------------
        json += '"requestParms" : {' +
         '"applicationComments":{"applicationNumber":' + '"' + strApp# + '",' +
         '"comments":[';
        Exsr ListComments;
        json += ']}}';
        If ioRtnCd = 'Error';
           comma = *Blank;
           For i0 = 1 to %Elem(errMsgArray);
              If errMsgArray(i0) = *Blanks;
                 Leave;
              EndIf;
              If comma <> *Blank;
                 array = %Trim(array) + comma;
              Else;
                 comma = ',';
              Endif;
              array = %Trim(array) + errMsgArray(i0);
           EndFor;
           i0 = %CheckR(' ':json);
           Select;
           When i0 = 0;
           Other;
              If %Subst(json:i0:1) <> ',';
                 json = %Trim(json) + ',';
              Endif;
           Endsl;
           json = %trim(json) + ' "errorMsg": [ ' + %trim(array) + ' ] ';
        EndIf;
       Endsr;
       //----------------------------------------
       Begsr DataSave;
       //----------------------------------------
        If ioRtnCd = 'Error';
           LeaveSr;
        Endif;
        For ix = 1 To %Elem(dsIn);
           If dsIn(ix).comment = *Blank;
              Leave;
           EndIf;
           Chain (intCo#:intApp#:dsIn(ix).timeStamp) pap0p;
           APco# = intCo#;
           APapp# = intApp#;
           APcomm   = dsIn(ix).comment;
           APtmStmp = dsIn(ix).timeStamp;
           APuserId = dsIn(ix).userId;
           APdt = sysDate7;
           APtime = sysTime6;
           APpgm = Q1pgm;
           APuser = Q1user;
           If %Found(WIAPPCOMM);
              Update pap0p;
           Else;
              intSeq# = sysTime6 + ix;
              APtmStmp = %Trim(%Editc(isoDate8:'X')) +
               %Trim(%Editc(intSeq#:'X'));
              APidt = sysDate7;
              APipgm = Q1pgm;
              APiusr = Q1user;
              APitim = APtime;
              Write pap0p;
           Endif;
        EndFor;
       Endsr;
       //----------------------------------------
       Begsr ListComments;
       //----------------------------------------
        For i0 = 1 To %Elem(dsIn);
           If dsin(i0).comment = *Blank;
              Leave;
           Endif;
           If comma <> *Blank;
              json += comma;
           Else;
              comma = ',';
           EndIf;
           json += '{';
           json += '"comment":"' + dsIn(i0).comment + '",';
           json += '"timeStamp":"' + %Trim(dsIn(i0).timeStamp) + '",';
           json += '"user":"' + dsIn(i0).userId + '"';
           json += '}';
        EndFor;
       Endsr;
      /End-Free
     p SaveAppComments...
     p                 e

