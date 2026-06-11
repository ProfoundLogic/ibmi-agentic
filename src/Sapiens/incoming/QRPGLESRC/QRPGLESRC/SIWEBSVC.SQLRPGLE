     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL') DECEDIT('0.')
     H*====================================================================
     H* Create on 04/13/2021 by c:62 for Project WC2663
     H*====================================================================
     H* Description: Web-services associative routines
     H*
     H*
     H*====================================================================
     H* Revisions:
     H*
     H*
     H*====================================================================
     D PSDS          ESDS                  extname(SZQ1P)

      *entry
     dRequestPgm       pr                  ExtPgm('SIWEBSVC')
     dRequestPgm       pi

     d* Retrieve Client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     d* Retrieve Library
     d RtvObjectLib    pr                  ExtPgm('RTVOBJLIB')
     d   inObject                    10    Const
     d   inObjectType                 7    Const
     d   outLibrary                  10
     d
     DqSndDtaQ         PR                  EXTPGM('QSNDDTAQ')
     D queueName                     10A   const
     D queueLibrary                  10A   const
     D queueLength                    5P 0 const
     D queueData                  32767A   const options(*varsize)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJobInfo
      /copy sPrJson
      /copy SprModule
      /copy SprRunCmd
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy sPrUsrPrms
      /Include yajl_h

     d inObject        s             10    Inz('SIWEBSVC')
     d inObjectType    s              7    Inz('*PGM')
     d outLibrary      s             10    Inz(' ')

     d queueName       s             10a   Inz('SIWEBSVC')
     d queueLibrary    s             10a
     d queueLength     s              5p 0
     d queueData       s          32767a   Varying

     d sqlStmtStr      s            500a
     d rcdCount        s              9  0
     d inJsonClob_T    s                   SqlType(clob:64000)

      /Free
       Exec Sql Set Option Commit = *None;

       Main();

       *inlr = *on;

       //=================================================================
       // Create the data queue SIWEBSVC
       //=================================================================
     PCreateSiWebSvcDataQueue...
     P                 b
     dCreateSiWebSvcDataQueue...
     d                 pi

     d addDataQueue    s            256

      /free

       // Let's find the data queue library
       RtvObjectLib(inObject:inObjectType:outLibrary);

       // Build command to create the data queue
       addDataQueue = 'CRTDTAQ DTAQ('  + %Trim(outLibrary) +
                      '/SIWEBSVC) MAXLEN(64000) SIZE(*MAX2GB 1)';

       // Create the data queue
       Monitor;
          runCmd(%Trim(addDataQueue):%Len(%Trim(addDataQueue)));
       On-Error;
       EndMon;

      /End-free

     PCreateSiWebSvcDataQueue...
     P                 e

       //=================================================================
       // Extract the Service Transaction from the Json
       //=================================================================
     PGetService       b
     dGetService       pi           100a   varying
     d inJson                     64000a

     D errMsg          s            500a   Varying Inz(' ')
     d outService      s            100a   varying Inz('Unknown')
     D docNode         s                   like(yajl_val)
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D val             s                   like(yajl_val)

      /free

       docNode = yajl_string_load_tree(inJson : errMsg);
       If errMsg = ' ';
          node = YAJL_object_find(docNode: 'transaction');
          outService = YAJL_get_string(node);
       EndIf;

       Return outService;

      /End-free

     PGetService       e
       //=================================================================
       // Main - Main Processing
       //=================================================================
     PMain             b

     d webSvcRRN       s              9  0
     d transaction     s                   SqlType(clob:64000)

      /free
        sqlStmtStr = 'Select WCOtrans, rrn(SIWEBSVC) From SIWEBSVC';

        Exec Sql Prepare sqlSiWebSvc From :sqlStmtStr;
        Exec SQL Declare cursorSiWebSvc Cursor For sqlSIWebSvc;
        Exec SQL Open cursorSiWebSvc;

        Dou Not IsValidFetch(sqlStt);
           Exec SQL fetch next from
                cursorSiWebSvc into :transaction, :webSvcRRN;
           If IsValidFetch(sqlStt);
              // Delete the overflow record
              Exec Sql Delete From SIWEBSVC
                   Where rrn(SIWEBSVC) = :webSvcRRN;
              // Try writing to the data queue again
              If Not LoadSIWEBSVCDataQ(transaction);
                 // Failed to write to data queue so exit program
                 Return;
              EndIf;
           Else;
              // Were all records deleted, if so then schedule a job
              Exec Sql Select count(*) Into :rcdCount
                       From SIWEBSVC;
              If rcdCount = 0;
                 // Schedule job to reorganize SIWEBSVC
                 If Not ScheduleFileReorgination();
                    // Additional actions if needed
                 EndIf;
              EndIf;
              Leave;
           Endif;
        Enddo;

      /End-free

     PMain             e

       //=================================================================
       // Load Data to Data Queue
       //=================================================================
     PLoadSIWEBSVCDataQ...
     P                 b                   Export
     dLoadSIWEBSVCDataQ...
     d                 pi              n
     d inJsonClob                          likeDs(inJsonClob_T)

     d service         s            100    varying
     d timeStamp       s               z
     d success         s               n   Inz(*On)
     d submitSiWebSvc  s            256a
     d chrTimeDelay    s            256a
     d timeDelay       s              5s 0
     d date            s               d   DatFmt(*usa)
     d intMth          s              2s 0
     d intDay          s              2s 0
     d intYear         s              4s 0
     d chrDate         s             10a
     d time            s               t
     d intHr           s              2s 0
     d intMin          s              2s 0
     d intSec          s              2s 0
     d chrTime         s              8a

      /Free

       // Let's find the object to place the data queue
       RtvObjectLib(inObject:inObjectType:outLibrary);

       // Create data queue if it doesn't exist
       CreateSiWebSvcDataQueue();

       // Write info to transaction history
       timeStamp = %TimeStamp();
       service   = GetService(inJsonClob.inJsonClob_T_Data);
       Exec sql Insert Into SiWebSvcT
            Values (:timeStamp,:service,:inJsonClob);

       // Write to data queue.
       queueLibrary = outLibrary;
       queueLength  = inJsonClob.inJsonClob_T_Len;
       queueData    = inJsonClob.inJsonClob_T_Data;

       Monitor;
          qSndDtaQ(queueName:queueLibrary:queueLength:queueData);
       On-Error;
          success = *Off;
          // Write to overflow file
          Exec Sql Insert into SiWebSvc Values (:inJsonClob);
          // Check to see if there is a SIWEBSVC job
          If Not ChkJobJobQ('SIWEBSVC');

             // Retrieve job time delay
             chrTimeDelay = GetModuleProperty('3rd Party Payments':
                            'SubmitDelay');
             If chrTimeDelay = ' ';
                chrTimeDelay='60';
             EndIf;

             timeDelay = %Dec(%Trim(chrTimeDelay):5:0);

             // Job not found so let's submit one.
             ExSr SubmitService;
          EndIf;
       EndMon;

       Return success;

       //==================================================================
       // SubmitService - Submits Service in the future
       //==================================================================
       BegSR SubmitService;

          // Create job schedule time
          // Increment time
          timeStamp = %TimeStamp() + %Minutes(timeDelay);

          // Schedule Date
          chrDate   = %Char(%Date(timestamp):*usa);

          // Schedule Time
          chrTime   = %Char(%Time(timestamp):*hms);

          // Submit invoice job
          submitSiWebSvc = 'SBMJOB CMD(CALL PGM(SIWEBSVC)) ' +
                           'JOB(SIWEBSVC) JOBD(WUSERJ) SCDDATE(''' +
                           chrDate + ''') SCDTIME(''' + chrTime + ''')';

          RunCmd(%Trim(submitSiWebSvc):%Len(%Trim(submitSiWebSvc)));

       EndSR;

      /End-free

     PLoadSIWEBSVCDataQ...
     P                 e
       //=================================================================
       // Overflow file is empty, schedule a job to reorganize SIWEBSVC
       //=================================================================
     PScheduleFileReorgination...
     P                 b
     dScheduleFileReorgination...
     d                 pi              n

     d addJobScde      s            256
     d scheduled       s               n   Inz(*On)

      /free

       addJobScde = 'ADDJOBSCDE JOB(RGZSIWEBSVC) ' +
                    'CMD(RGZPFM FILE(SIWEBSVC) ' +
                    'FRQ(*ONCE) SCDTIME(''23:59:00'') ' +
                    'TEXT(''Reorganize SIWEBSVC'')';

       Monitor;
          runCmd(%Trim(addJobScde):%Len(%Trim(addJobScde)));
       On-Error;
          scheduled = *Off;
       EndMon;

       Return scheduled;

      /End-free

     PScheduleFileReorgination...
     P                 e
