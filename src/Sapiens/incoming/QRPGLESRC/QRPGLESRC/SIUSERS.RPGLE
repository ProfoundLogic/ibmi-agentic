     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL') DECEDIT('0.')
     H*====================================================================
     H* Create on 10/15/2018 by c:62 for Project SY406
     H*====================================================================
     H* Description: -Retrieves User List
     H*              -Retrieves User X-Ref
     H*
     H*====================================================================
     H* inParms -
     H*   Portal User ID
     H*====================================================================
     H* Revisions:
    H*                                                                   *
    H*  12/09/21  C:33  Support: 99529                                   *
    H*                  Increase throughput                              *
     H*
     H*====================================================================
     Fsmupl     if   e           k disk    rename(pup01:pup00)

     D PSDS          ESDS                  extname(SZQ1P)

     D* this size needs to change
     D array           s               a   len(32767)
     d inJson          s               a   len(2000000) varying
     D json            s               a   len(8500000) varying
     D jsont           s               a   len(8500000) varying
     D jsonSub         s               a   len(8500000) varying

     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')
     D moreRows        s               n   Inz(*On)
     D clientID        s              3    inz('000')
     d strAuthority    s             10a

     D docNode         s                   like(yajl_val)
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D val             s                   like(yajl_val)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrString
      /copy wPrGroup
      /include yajl_h

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      *entry
     DRequestPgm       pr                  ExtPgm('SIUSERS')
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

       // Get the client ID
       GetClientID(clientID);

       Clear json;

       Select;
       When inRequest = 'ListiSeriesUsers';
          ProcISeriesUserList();
       When inRequest = 'GetiSeriesUser';
          ProcISeriesUser();
       EndSl;

       outJSON = json;

       *inlr = *on;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtCd;
        Clear ioRtCd;
       Endsr;

      /End-free
       //===================================================================
       // Process iSeries User Listing
       //===================================================================
     pProcISeriesUserList...
     p                 b
     dProcISeriesUserList...
     d                 pi

     d result          ds                  Qualified
     D***  Begin Chg  ***  12/09/21  ************
     d**userName                     40a
     d  userName                     40a   varying
     D***  End   Chg  ***  12/09/21  ************

     D dsWMUPP       e ds                  extname(WMUPP)
     D sqlStmtStr      s            400a

      /free
          Exsr GetParameters;
          If errMsg = ' ';
             Exsr BldReturnInfo;
          Else;
             Exsr BldErrorsInfo;
          EndIf;

       //=================================================================
       // Build User Information                                            quests
       //=================================================================
       Begsr BldUserInfo;

          // Begin Array
          // End the Array
          json = %Trim(json) + ',"userList":[';

          // Build agency sql statement
          sqlStmtStr = 'Select * from WMUPP Where UPstat <> ''I''';

          // Add on Filters
          // Start Where
          If result.userName <> ' ';
             sqlStmtStr = %Trim(sqlStmtStr) + ' And Upper(UPusNm) like ' +
               '''%'' || Upper(''' + %Trim(result.UserName) + ''') || ''%''';
          EndIf;

          // Prepare the SQL statement
          Exec SQL prepare usrStmt from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare usrCursor cursor for usrStmt;

          // Run SQL
          Exec SQL open usrCursor;

          // Get first record if there is one
          Exec SQL fetch next from usrCursor into :dsWMUPP;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             moreRows = *off;
             Exec SQL close usrCursor;
          EndIf;

          // If row was fetched then process
          DoW moreRows = *On;

             // Build Record
             Exsr BldUserRcd;

             // Fetch next Record
             Exec SQL fetch next from usrCursor into :dsWMUPP;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                moreRows = *Off;
                Exec SQL close usrCursor;
             Else;
                // Prepare for next record
                json = %Trim(json) + ' ,';
             EndIf;
          EndDo;

          // End the Array
          json = %Trim(json) + ' ]';

       EndSr;

       //=================================================================
       // Build the User Record
       //=================================================================
       Begsr BldUserRcd;

        json = %trim(json) + ' { ' +
           BuildJsonNode('userID':%Trim(UPusr)) +
           ',' +
           BuildJsonNode('userName':%Trim(RmvExceptJSON(UPusNm))) +
           ',' +
           BuildJsonNode('userTitle':%Trim(UPtitl)) +
           ' }';

       Endsr;

       //=================================================================
       // Build Error Message Response                                      quests
       //=================================================================
       Begsr BldErrorsInfo;

          // Build the Request Info
          ExSr BldReqInfo;

          // Finish by generating the errors
          json = %Trim(json) + ' "errorMsg": [ ' + %Trim(errMsg) + ' ] ';
          ioRtCd = 'error';

       Endsr;

       //=================================================================
       // Build Request Information                                         quests
       //=================================================================
       Begsr BldReqInfo;

          json = '"requestParms" : {' +
                 %Trim(BuildJsonNode('userName':result.userName)) +
                 '}';

       Endsr;

       //=================================================================
       // Build Return Information                                          quests
       //=================================================================
       Begsr BldReturnInfo;

          // Build the request Parameters
          ExSr BldReqInfo;

          // Build Darts Information
          ExSr BldUserInfo;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';

             // Load the data structure with the parameter values
             // from the tree
             node               = YAJL_object_find(docNode: 'userName');
             result.userName    = YAJL_get_string(node);

             // Free up the space by removing the tree
             yajl_tree_free(docNode);

             //======= Validate the incoming parameters ====================

             // Nothing to validate

          Else;
             errMsg = '"' + %Trim(errMsg500) + '"';
          EndIf;

       Endsr;

      /End-free

     pProcISeriesUserList...
     p                 e
       //===================================================================
       // Process iSeries User
       //===================================================================
     pProcISeriesUser...
     p                 b
     dProcISeriesUser...
     d                 pi


     d result          ds                  Qualified
     D***  Begin Chg  ***  12/09/21  ************
     d**userID**                    256a
     d  userID                      256a   varying
     D***  End   Chg  ***  12/09/21  ************

     d sqlStmtStr      s            400a
     d iUserID         s             10a   Inz(' ')

      /free
          Exsr GetParameters;
          If errMsg = ' ';
             Exsr BldReturnInfo;
          Else;
             Exsr BldErrorsInfo;
          EndIf;

       //=================================================================
       // Return iSeries User ID                                            quests
       //=================================================================
       Begsr BldUserID;

        json = %trim(json) + BuildJsonNode('iSeriesUserID':%Trim(iUserID));

       EndSr;


       //=================================================================
       // Build Error Message Response                                      quests
       //=================================================================
       Begsr BldErrorsInfo;

          // Build the Request Info
          ExSr BldReqInfo;

          // Finish by generating the errors
          json = %Trim(json) + ' "errorMsg": [ ' + %Trim(errMsg) + ' ] ';
          ioRtCd = 'error';

       Endsr;

       //=================================================================
       // Build Request Information                                         quests
       //=================================================================
       Begsr BldReqInfo;

          json = '"requestParms" : {' +
                 %Trim(BuildJsonNode('portalUserID':result.userID)) +
                 '} , ';

       Endsr;

       //=================================================================
       // Build Return Information                                          quests
       //=================================================================
       Begsr BldReturnInfo;

          // Build the request Parameters
          ExSr BldReqInfo;

          // Build Darts Information
          ExSr BldUserID;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';

             // Load the data structure with the parameter values
             // from the tree
             node               = YAJL_object_find(docNode: 'portalUserID');
             result.userID      = YAJL_get_string(node);

             // Free up the space by removing the tree
             yajl_tree_free(docNode);

             //======= Validate the incoming parameters ====================

             // Portal User ID Must be passed
             If result.userID = ' ';
                errMsg = %Trim(errMsg) + '"No Portal User ID passed in"';
             Else;

                // Find iSeries User Profile
                Exec SQL Select SIusr Into :iUserID From SIHUBUSR
                   Where SIuserID = :result.UserID;

                If iUserID = ' ';
                   errMsg = %Trim(errMsg) + '"Unable to return an iSeries ' +
                                            'User ID"';
                EndIf;

             EndIf;

          Else;
             errMsg = '"' + %Trim(errMsg500) + '"';
          EndIf;

       Endsr;

      /End-free

     pProcISeriesUser...
     p                 e
