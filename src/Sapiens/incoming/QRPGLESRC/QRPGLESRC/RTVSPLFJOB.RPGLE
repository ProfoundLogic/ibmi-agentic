     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H*====================================================================
     H* Revisions:
     H*
     H* 10/21/15  C:33  36619
     H*                 Check the PRTF SPLFOWN setting
     H*====================================================================

      /INCLUDE QSYSINC/QRPGLESRC,QGYOLJOB
      /INCLUDE QSYSINC/QRPGLESRC,QUSEC

     D QGYOLJOB#       PR                  ExtPgm('QGYOLJOB')
     D  Receiver                           LikeDS(OLJB0200)
     D  ReceiverSz                   10I 0 Const
     D  Format                        8A   Const
     D  ReceiverDef                        Like(ReceiverDef)
     D  ReceiverDefSz                10I 0 Const
     D  ListInfo                           LikeDS(QGYLJBLI)
     D  RecsToRtn                    10I 0 Const
     D  SortInfo                           Const LikeDS(SortInfo)
     D  JobSelInfo                         Const LikeDS(QGYLJBJS)
     D  JobSelInfoSz                 10I 0 Const
     D  FieldsToRtn                  10I 0 Const
     D  FieldKeys                    10I 0 Const Dim(2)                         Only 2 keys needed.
     D  ErrorCode                          LikeDS(QUSEC)
     D  JobSelFormat                  8A   Const

     D QMHSNDPM        PR                  ExtPgm('QMHSNDPM')
     D  MsgId                         7A   Const
     D  QualMsgF                     20A   Const
     D  MsgData                     128A   Const
     D  MsgDataLen                   10I 0 Const
     D  MsgType                      10A   Const
     D  StackEntry                   10A   Const
     D  StackCounter                 10I 0 Const
     D  MsgKey                        4A
     D  ErrorCode                          LikeDS(QUSEC)

     D***  Begin Add  ***  10/21/15  **************************************
     D QDFRPRTA        pr                  extpgm('QDFRPRTA')                   print file attribute
     D                             4000a                                        receiver
     D                               10i 0 const                                receiver length
     D                                8a   const                                format
     D                               20a                                        file name and lib
     D  ErrorCode                          LikeDS(QUSEC)

     DGetSplfOwn       pr            10
     D inPrtf                        10
     D***  End   Add  ***  10/21/15  **************************************

     D QGYCLST         PR                  ExtPgm('QGYCLST')
     D  Handle                        4A   Const
     D  ErrorCode                          LikeDS(QUSEC)

     D RTVSPLFJOB      PR                  ExtPgm('RTVSPLFJOB')
     D  oJobNumber                    6S 0
     D  oJobUser                     10A
     D  oJobName                     10A
     D***  Begin Add  ***  10/21/15  **************************************
     D  iFile                        10A   options(*nopass)
     D***  End   Add  ***  10/21/15  **************************************

     D RTVSPLFJOB      PI
     D  oJobNumber                    6S 0
     D  oJobUser                     10A
     D  oJobName                     10A
     D***  Begin Add  ***  10/21/15  **************************************
     D  iFile                        10A   options(*nopass)
     D***  End   Add  ***  10/21/15  **************************************

     D PSDS           SDS                  Qualified
     D  JobName              244    253A
     D  JobUser              254    263A
     D  JobNumber            264    269S 0
     D  CurrentUser          358    367A

     D ListInfo        DS                  LikeDS(QGYLJBLI)
     D ERRC0100        DS                  LikeDS(QUSEC)

     D* API doesn't seem to like it if DS size is not multiple of 4.
     D OLJB0200        DS                  Qualified
     D  Header                             LikeDS(QGYB0200)
     D  VarData                      20A

     D SortInfo        DS                  Qualified                            Only 1 key needed.
     D  NumKeys                      10I 0
     D  KeyStartPos                  10I 0
     D  KeyLength                    10I 0
     D  KeyType                       5I 0
     D  SortOrder                     1A
     D  Reserved                      1A

     D JobSelInfo      DS                  Qualified
     D  Header                             LikeDS(QGYLJBJS)
     D  PrimaryJobSts                10A
     D  JobType                      10I 0

     D FieldKeys       S             10I 0 Dim(2)

     D ReceiverDef     DS                  Qualified
     D  NumFields                    10I 0
     D  Recs                               LikeDS(ReceiverRec) Dim(2)

     D ReceiverRec     DS                  Qualified
     D  TotDataLen                   10I 0
     D  KeyField                     10I 0
     D  DataType                      1A
     D  Reserved                      3A
     D  FieldLen                     10I 0
     D  DataOffset                   10I 0

     D***  Begin Add  ***  10/21/15  **************************************
     D qdfrprtaDS      DS          4000    qualified
     D  BytesReturned                10i 0 overlay(qdfrprtaDS:1)
     D  BytesAvail                   10i 0 overlay(qdfrprtaDS:5)
     D  PrintFileName                10a   overlay(qdfrprtaDS:9)
     D  Lib                          10a   overlay(qdfrprtaDS:19)
     D  Device                       10a   overlay(qdfrprtaDS:29)
     D  DeviceType                   10a   overlay(qdfrprtaDS:39)
     D  PageSizeLen                  15p 5 overlay(qdfrprtaDS:49)
     D  PageSizeWidth                15p 5 overlay(qdfrprtaDS:57)
     D  LPI                          15p 5 overlay(qdfrprtaDS:77)
     D  CPI                          15p 5 overlay(qdfrprtaDS:85)
     D  OverFlow                     10i 0 overlay(qdfrprtaDS:93)
     D  Text                         50a   overlay(qdfrprtaDS:98)
     D  FMarginDown                  15p 5 overlay(qdfrprtaDS:149)
     D  FMarginAccros                15p 5 overlay(qdfrprtaDS:157)
     D  PrintQuality                 10a   overlay(qdfrprtaDS:219)
     D  SplfOwn                      10a   overlay(qdfrprtaDS:756)
     D***  End   Add  ***  10/21/15  **************************************

     D ErrorMsg        S            128A

     D i               S             10I 0
     D***  Begin Add  ***  10/21/15  **************************************
     D strFile         s             10A
     D***  End   Add  ***  10/21/15  **************************************

     D MsgKey          S              4A

      /FREE
       //***  Begin Add  ***  10/21/15  ***********************************
       If %parms >= 4;
          strFile = iFile;
       Endif;
       //***  End   Add  ***  10/21/15  ***********************************


       //***  Begin Chg  ***  10/21/15  ***********************************
       //********* If PSDS.CurrentUser = PSDS.JobUser;
       Select;

       When strFile <> '*ANY' and GetSplfOwn(strFile) = '*JOB';
          // When specific PRTF is owned by the *JOB
          oJobName   = PSDS.JobName;
          oJobUser   = PSDS.JobUser;
          oJobNumber = PSDS.JobNumber;

       When PSDS.CurrentUser = PSDS.JobUser;
       //***  End   Chg  ***  10/21/15  ***********************************
          // Spooled files always assigned to current job in this case.

          oJobName = PSDS.JobName;
          oJobUser = PSDS.JobUser;
          oJobNumber = PSDS.JobNumber;

       //***  Begin Chg  ***  10/21/15  ***********************************
       //********** Else;
       Other;
       //***  End   Chg  ***  10/21/15  ***********************************

          // Spooled files assigned to a QPRTJOB in this case.
          // In some cases, there can be multiple QPRTJOBs for the user.

          // We will find the most recently created QPRTJOB for the user
          // by asking for 1 record with descending sort on job start
          // date/time.

          ERRC0100 = *ALLx'00'; // Exception message will be sent on error.

          // Gives position and type of field in our receiver variable.
          SortInfo.NumKeys = 1;
          SortInfo.KeyStartPos = %Size(OLJB0200.Header) + 1;
          SortInfo.KeyLength = 13;  // Date/time CYYMMDDHHMMSS
          SortInfo.KeyType = 6;     // Character, no NLS sort.
          SortInfo.SortOrder = '2'; // Descending.
          SortInfo.Reserved = *ALLx'00';

          JobSelInfo = *ALLx'00';
          JobSelInfo.Header.QGYJN07 = 'QPRTJOB';        // Name
          JobSelInfo.Header.QGYUN04 = PSDS.CurrentUser; // User
          JobSelInfo.Header.QGYJNBR = '*ALL';           // Number
          JobSelInfo.Header.QGYJT01 = 'B';              // Type (batch)

          // Primary job status count/offset.
          // Looking for status *OUTQ.
          JobSelInfo.Header.QGYPJSC = 1;
          JobSelInfo.Header.QGYPJSO = %Size(JobSelInfo.Header);
          JobSelInfo.PrimaryJobSts = '*OUTQ';

          // Job type enhanced count/offset.
          // Looking for type = 'Batch - alternate spool user'
          JobSelInfo.Header.QGYJTEC = 1;
          JobSelInfo.Header.QGYJTEO = %Size(JobSelInfo.Header) +
                                      %Size(JobSelInfo.PrimaryJobSts);
          JobSelInfo.JobType = 240;

          FieldKeys(1) = 402;  // Job start date/time.
          FieldKeys(2) = 1016; // Job type enhanced. Only because API requires
                               // this when selecting on this field.

          QGYOLJOB#(OLJB0200 :
                    %Size(OLJB0200) :
                    'OLJB0200' :
                    ReceiverDef :
                    %Size(ReceiverDef) :
                    ListInfo :
                    1 :
                    SortInfo :
                    JobSelInfo :
                    %Size(JobSelInfo) :
                    2 :
                    FieldKeys :
                    ERRC0100 :
                    'OLJS0200');

          QGYCLST(ListInfo.QGYRH05 : ERRC0100);

          ErrorMsg = '';
          If ListInfo.QGYIC05 <> 'C';

            ErrorMsg = 'Job list incomplete.';

          ElseIf ListInfo.QGYRRTN01 <> 1;

            ErrorMsg = 'Job not found.';

          ElseIf ListInfo.QGYTR05 > 1;

            // Our algorithm here depends on the API having put the
            // job start date/time in a specific position in the data.

            // If more than one record and the date/time is not where
            // we told the API to sort from, then we could have the
            // wrong job.

            // If this error ever happens, we need to adjust so that
            // filtering is not done on job type so that we have only
            // one key field returned.
            For i = 1 To %Elem(FieldKeys);

              If ReceiverDef.Recs(i).KeyField = 402;

                If ReceiverDef.Recs(i).DATAOFFSET <> SortInfo.KeyStartPos - 1;

                  ErrorMsg = 'Job could not be located reliably due to ' +
                             'list sorting problem.';

                EndIf;

                Leave;

              EndIf;

            EndFor;

          EndIf;

          If ErrorMsg = '';

            // Looks like we found it.

            oJobName = OLJB0200.Header.QGYJNU00;
            oJobUser = OLJB0200.Header.QGYUNU00;
            oJobNumber = %Dec(OLJB0200.Header.QGYJNBRU00 : 6 : 0);

          Else;

            // No good.
            ErrorMsg = 'Unable to determine job for spooled file assignment. ' +
                       ErrorMsg;

            QMHSNDPM('CPF9897' :
                     'QCPFMSG   *LIBL' :
                     ErrorMsg :
                     %Len(%Trim(ErrorMsg)) :
                     '*ESCAPE' :
                     '*PGMBDY' :
                     1 :
                     MsgKey :
                     ERRC0100);

          EndIf;

       //***  Begin Chg  ***  10/21/15  ***********************************
       //********** EndIf;
       Endsl;
       //***  End   Chg  ***  10/21/15  ***********************************

        *InLr = *On;

      /END-FREE

       //***  Begin Add  ***  10/21/15  ***********************************
     P*====================================================================
     P* Get the SPLFOWN setting for the PRTF
     P*====================================================================
     PGetSplfOwn       b
     DGetSplfOwn       pi            10
     D inPrtf                        10

     D strPrtf         s             20

      /Free
       strPrtf = inPrtf + '*LIBL';
       Callp QDFRPRTA(qdfrprtaDS:%size(qdfrprtaDS):'PRTF0100':strPrtf:errC0100);
       Return qdfrprtaDS.splfOwn;
      /End-free
     PGetSplfOwn       e
       //***  End   Add  ***  10/21/15  ***********************************
