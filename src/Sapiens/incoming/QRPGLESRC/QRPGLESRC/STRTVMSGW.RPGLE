     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*====================================================================
    H/TITLE STRTVMSGW - Retreive MSGW Information
    H*====================================================================
    H* Description:
    H*   This program will determine the number of messages:
    H*    - Completion
    H*    - Inquiry
    H*   The majority of this program was copied from the internet and
    H*    therfore may not conform to all standards.
    H*
    H*  Created: 11/28/12     C:33     Project: SY266
    H*====================================================================
    H* Revisions:
     H*
     H*  04/27/15 C:33  PHW32288
     H*                -Check for message queue prior to retrieving msgs.
     H*                -Add check for queue in CL.
     H*
     H*  09/09/14 C:180 Support: OP129 86723
     H*                 Check for message overflow (150 messages max)
     H*
     H*  07/11/14 C:290 Support: OP129 86121
     H*                 Not needed.                                         W
     H*
     H*  06/30/14 C:298 Support: OP114 85945
     H*           C:33  Check for an overflow of number of messages returned.
     H*
     H* 03/13/14  C:33  -Add full messages to JSON
     H*                 -Rewrite to use RTVMSG from CL
    H*====================================================================
     dintInfoLen       s             10u 0 inz(306)
     dintNbrKeys       s             10u 0 inz(0)
     dintNbrRcds       s             10u 0 inz(0)
     dintSizeMsgSel    s             10u 0 inz(0)
     dstrKeyList       s            400
     D*
     dstrUser          s             10    inz
     dstrJobName       s             10    inz('*ALL')
     dstrJobNumber     s              6    inz('*ALL')
     dflgTypeOfJob     s              1    inz('B')
     disMsgW           s              1n   inz('0')
     dintCompMsg       s              5  0
     dintInqMsg        s              5  0
     DstrMsgQueue      s             44a
     DisBuildMsgs      s              1n   inz(*off)
     d*
     D#ofFields        c                   4

     DdsLSTM0100       ds                  qualified
     D binOffSetEnt                   4b 0
     D binOffSetFlds                  4b 0
     D bin#OfFields                   4b 0
     D binMsgSeverity                 4b 0
     D strMsgId                       7
     D strMsgType                     2
     D strMsgKey                      4
     D strMsgFile                    10
     D strMsgFileLib                 10
     D strMsgQue                     10
     D strMsgQueLib                  10
     D strDateSent                    7
     D strTimeSent                    6

     DdsQueueInfo      ds                  qualified
     D user1Queue0                    1a   Inz('1')
     D queueName                     10a
     D queueLib                      10a

     DdsMsgListInfo    ds                  qualified inz
     D totalRcds                     10i 0
     D rcdsRtrnd                     10i 0
     D handle                         4a
     D rcdLength                     10i 0
     D infoComplt                     1a
     D dateTime                      13a
     D listStatus                     1a
     D                                1a
     D infoLength                    10i 0
     D firstRec                      10i 0
     D                               40a

     DdsMsgSel         ds                  qualified
     D listDirection                 10a   inz('*PRV')
     D                                2a
     D severityCrit                  10i 0 inz(0)
     D maxMsgLen                     10i 0 inz(-1)
     D maxHelpLen                    10i 0 inz(0)
     D selectOffset                  10i 0 inz(44)
     D selectNumber                  10i 0 inz(1)
     D strKeyOffset                  10i 0 inz(54)
     D fieldOffset                   10i 0 inz(58)
     D rtnFields                     10i 0 inz(#ofFields)
     D criteria                      10a   dim(1) Inz('*ALL')
     D strKey                         4a   dim(1) Inz(x'FFFFFFFF')
     D fieldId                       10i 0 dim(#ofFields)

     DListInfoDs       ds                  qualified inz
     D TotalRecs                     10i 0
     D RecsRtrnd                     10i 0
     D Handle                         4a
     D RecLength                     10i 0
     D InfoComplt                     1a
     D DateTime                      13a
     D ListStatus                     1a
     D                                1a
     D InfoLength                    10i 0
     D FirstRec                      10i 0
     D                               40a

     DMsgData          ds                  qualified
     D BytesReturn                   10i 0
     D BytesAvail                    10i 0
     D LenMsgReturn                  10i 0
     D LenMsgAvail                   10i 0
     D LenMsgHelpRtn                 10i 0
     D LenMsgHelpAvl                 10i 0
     D Msg                         2048a

     DdsReplaceData    ds                  qualified
     D binOffset                      4b 0
     D binFieldLen                    4b 0
     D binIdentifier                  4b 0
     D strTypeOfData                  1
     D strStsOfData                   1
     D strReserved                   14
     D binLenOfData                   4b 0
     D strData                      256a    varying

     DReplacementData...
     D                 s            256a   varying inz
     DcompCount        s              5  0 inz
     DinqCount         s              5  0 inz

     DmaxMsgArray      c                   150
     DdsMsgInfo        ds                  inz qualified
     D aryMsgInfo                          dim(maxMsgArray) descend
     D  strMsgType                    2    overlay(aryMsgInfo)
     D  strDateSent                   7    overlay(aryMsgInfo:*next)
     D  strTimeSent                   6    overlay(aryMsgInfo:*next)
     D  strMsgId                      7    overlay(aryMsgInfo:*next)
     D  strMsgFile                   10    overlay(aryMsgInfo:*next)
     D  strMsgFileLib                10    overlay(aryMsgInfo:*next)
     D  strMsgQue                    10    overlay(aryMsgInfo:*next)
     D  strMsgQueLib                 10    overlay(aryMsgInfo:*next)
     D  strMsg                      132    overlay(aryMsgInfo:*next)
     D   strMsgSort                  15    overlay(aryMsgInfo:1)
     DnumberOfFields   c                   Const(4)
     Drecord#          s             10i 0

     D*entry
     DStRtvMsgW        pr                  ExtPgm('STRTVMSGW')
     D inGenParm                    512
     D outJSON                    32767
     D outRtnCode                     1a
     DStRtvMsgW        pi
     D inGenParm                    512
     D outJSON                    32767
     D outRtnCode                     1a

     DBuildMsgArray    pr
     D inMsgInfo                  32767    const options(*varsize)
     D inRcvrType                     1    const
     D inCompCount                         const like(intCompMsg)
     D inInqCount                          const like(intInqMsg)

     DBuildMsgJSON     pr

     DCloseList        pr                  ExtPgm('QGYCLST')
     D handle                         4a
     D error                        256a   Options(*varsize)

     DCountMsgs        pr
     D completion                     5  0
     D inquiry                        5  0

     DCloseMsgQ        pr

     DOpenMsgQ         pr              n
     D msgQ                          10a   Const
     D criteria                      10a   Options(*nopass) const

     DListFromCL       pr             4
     D inMsgQ                        10a   const

     DMsgQExists       pr              n
     D msgQ                          10a   const

     DReadMsgQ         pr              n

     DCheckObj         pr                  ExtPgm('SRCHKOBJ')
     D object                        10a   const
     D objectType                     7a   const
     D isError                        1n

     DGetListEntries   pr                  ExtPgm('QGYGTLE')
     D rcvr                                LikeDs(dsLSTM0100)
     D rcvrLen                       10i 0 const
     D handle                         4a
     D listInfo                            likeds(dsMsgListInfo)
     D recs2Rtrn                     10i 0 const
     D rec#2Rtrn                     10i 0
     D error                        256a   options(*varsize)

     DRetrieveMsg      pr                  ExtPgm('QMHRTVM')
     D MsgInfo                             Like(MsgData)
     D LenMsgInfo                    10i 0 Const
     D Format                         8a   Const
     D MsgId                          7a   Const
     D MsgFile                       20a   Const
     D ReplaceData                         Const Like(ReplacementData)
     D LenReplaceData...
     D                               10i 0 Const
     D SubValues                     10a   Const
     D FmtCtlChar                    10a   Const
     D Error                        256a   Options(*varsize)

     DOpenListMsgs     pr                  ExtPgm('QGYOLMSG')
     D                            32766    options(*varsize)
     D rcvrLen                             const like(intInfoLen)
     D                               80
     D #OfRcds                             const like(intNbrRcds)
     D sortInfo                       1a   const
     D msgSelection                        const like(dsMsgSel)
     D sizeMsgSel                          const like(intSizeMsgSel)
     D queueInfo                           const like(dsQueueInfo)
     D msgQueue                            const like(strMsgQueue)
     D error                               like(QUSEC)

      /copy QSYSINC/QRPGLESRC,QUSEC
      /copy qsysinc/qrpglesrc,qmhrcvpm
      /copy sPrAtrium

      /Free
       strUser = inGenParm;

       Select;
       When strUser = '*SYSTEM';
          strUser = 'QSYSOPR';
       Endsl;

       Clear outJSON;

       Select;
       //When ListFromCL(strUser) = '*GS';
       //   Return;
       When not MsgQExists(strUser);
          Clear outJSON;
          BuildJSON(outJSON:'ErrorMsg':'Message queue ' + %trim(strUser) +
           ' does not exist.  Please contact system administrator.');
          *inlr = *on;
          Return;
       When ListFromCL(strUser) = '*END';
          isBuildMsgs = *on;
       When OpenMsgQ(strUser);
          If ReadMsgQ();
             CloseMsgQ();
          Endif;
       Other;
          CountMsgs(intCompMsg:intInqMsg);
       Endsl;

       //outJSON = '{';
       BuildJSON(outJSON:'CompletionMsgs':%char(intCompMsg));
       BuildJSON(outJSON:'InquiryMsgs':%char(intInqMsg));
       If isBuildMsgs;
          BuildMsgJSON();
       Endif;

       *inlr = *on;
       Return;

      /End-free

      //==================================================================
       // Build message array
      //==================================================================
     PBuildMsgArray    b
     DBuildMsgArray    pi
     D inMsgInfo                  32767    const options(*varsize)
     D inRcvrType                     1    const
     D inCompCount                         const like(intCompMsg)
     D inInqCount                          const like(intInqMsg)

     DdsInMsgInfo      ds                  inz qualified
     D strMsgType                     2
     D strDateSent                    7
     D strTimeSent                    6
     D strMsgId                       7
     D strMsgFile                    10
     D strMsgFileLib                 10
     D strMsgQue                     10
     D strMsgQueLib                  10
     D strMsg                       132

     DintTotalCount    s             +2    like(intInqMsg)
     DinMsgLSTM        ds                  likeds(dsLSTM0100)
     DinMsgRcvr        ds                  qualified
     D OSNextMsg                     10i 0
     D OSFields                      10i 0
     D NbrFlds                       10i 0
     D MsgSev                        10i 0
     D strMsgId                       7
     D strMsgType                     2
     D strMsgKey                      4
     D strMsgFile                    10
     D strMsgFileLib                 10
     D strMsgQue                     10
     D strMsgQueLib                  10
     D strDateSent                    7
     D strTimeSent                    6

     D strReplaceBin   s              4    inz('*YES')

      /Free
       intTotalCount = inCompCount + inInqCount;
       If intTotalCount <= maxMsgArray;
          Select;
          When inRcvrType = 'R';
             inMsgRcvr = inMsgInfo;
             dsInMsgInfo.strMsgType    = inMsgRcvr.strMsgType;
             dsInMsgInfo.strMsgId      = inMsgRcvr.strMsgId;
             dsInMsgInfo.strMsgFile    = inMsgRcvr.strMsgFile;
             dsInMsgInfo.strMsgFileLib = inMsgRcvr.strMsgFileLib;
             dsInMsgInfo.strMsgQue     = inMsgRcvr.strMsgQue;
             dsInMsgInfo.strMsgQueLib  = inMsgRcvr.strMsgQueLib;
             dsInMsgInfo.strDateSent   = inMsgRcvr.strDateSent;
             dsInMsgInfo.strTimeSent   = inMsgRcvr.strTimeSent;
          When inRcvrType = 'L';
             inMsgLSTM = inMsgInfo;
             dsInMsgInfo.strMsgType    = inMsgLSTM.strMsgType;
             dsInMsgInfo.strMsgId      = inMsgLSTM.strMsgId;
             dsInMsgInfo.strMsgFile    = inMsgLSTM.strMsgFile;
             dsInMsgInfo.strMsgFileLib = inMsgLSTM.strMsgFileLib;
             dsInMsgInfo.strMsgQue     = inMsgLSTM.strMsgQue;
             dsInMsgInfo.strMsgQueLib  = inMsgLSTM.strMsgQueLib;
             dsInMsgInfo.strDateSent   = inMsgLSTM.strDateSent;
             dsInMsgInfo.strTimeSent   = inMsgLSTM.strTimeSent;
          Endsl;

          RetrieveMsg(MsgData: %len(MsgData): 'RTVM0100':
           dsInMsgInfo.strMsgId:
           dsInMsgInfo.strMsgFile + dsInMsgInfo.strMsgFileLib:
           replacementData:%len(replacementData):strReplaceBin:
           '*NO': QUSEC);
          If MsgData.Msg <> *blank;
             dsInMsgInfo.strMsg = MsgData.Msg;
          Endif;

          dsMsgInfo.aryMsgInfo(intTotalCount) = dsInMsgInfo;
       Endif;
      /End-free

     PBuildMsgArray    e

      //==================================================================
       // Build JSON string containing all message info
      //==================================================================
     PBuildMsgJSON     b
     DBuildMsgJSON     pi

     Di                s              5  0
     DsaveType         s              2

      /Free
       Sorta dsMsgInfo.strMsgSort;

       For i = 1 to maxMsgArray;
          Select;
          When dsMsgInfo.strMsgType(i) = *blank;
             Iter;
          When dsMsgInfo.strMsgType(i) <> saveType;
             saveType = dsMsgInfo.strMsgType(i);
             Select;
             When outJSON <> *blanks and i = 1;
                outJSON = %trim(outJSON) + ',';
             When outJSON <> *blanks;
                outJSON = %trim(outJSON) + '],';
             Endsl;
             Select;
             When dsMsgInfo.strMsgType(i) = '01';
                outJSON = %trim(outJSON) + ' "Completion": [';
             When dsMsgInfo.strMsgType(i) = '05';
                outJSON = %trim(outJSON) + ' "Inquiry": [';
             Endsl;
          When dsMsgInfo.strMsgType(i) = saveType;
             outJSON = %trim(outJSON) + ',';
          Endsl;
          outJSON = %trim(outJSON) + ' {';
          BuildJSON(outJSON:'date':dsMsgInfo.strDateSent(i));
          BuildJSON(outJSON:'time':dsMsgInfo.strTimeSent(i));
          BuildJSON(outJSON:'message':dsMsgInfo.strMsg(i));
          outJSON = %trim(outJSON) + '}';
       Endfor;

       If outJSON <> *blanks and
        dsMsgInfo.strMsgType(1) <> *blank;
          outJSON = %trim(outJSON) + ']';
       Endif;

       Return;
      /End-free

     PBuildMsgJSON     e

      //==================================================================
       // Count messages for selected user
      //==================================================================
     PCountMsgs        b
     DCountMsgs        pi
     D completion                     5  0
     D inquiry                        5  0

     D compCount       s              5  0 inz
     D inqCount        s              5  0 inz
     D record#         s             10i 0 inz
     D rcvr            s              1a

      /Free

       Reset QUSEC;
       dsQueueInfo.queueName = strUser;
       //dsMsgSel.criteria = '*MNR'  // Messages needing reply
       //dsMsgSel.criteria = '*MNNR' // Messages not needing reply
       //dsMsgSel.fieldID(1) = 0301; // Receive message without data
       //dsMsgSel.fieldID(1) = 0302; // Receive message with data
       dsMsgSel.fieldID(1) = 0302;   // Receive message with data

       //OpenListMsgs(dsLSTM0100:%size(dsLSTM0100):dsMsgListInfo:-1:'0'
       // :dsMsgSel:%size(dsMsgSel):dsQueueInfo:strMsgQueue:QUSEC);
       OpenListMsgs(rcvr:%size(rcvr):dsMsgListInfo:-1:'0'
        :dsMsgSel:%size(dsMsgSel):dsQueueInfo:strMsgQueue:QUSEC);

       Dou QUSEI <> *blank; // Error
          Reset QUSEC;
          record# += 1;
          GetListEntries(dsLSTM0100:%size(dsLSTM0100):dsMsgListInfo.handle:
           dsMsgListInfo:1:record#:QUSEC);
          Select;
          When QUSEI <> *blank;
          When dsLSTM0100.strMsgType = '01';
             compCount += 1;
             BuildMsgArray(dsLSTM0100:'L':compCount:inqCount);
          When dsLSTM0100.strMsgType = '05';
             inqCount += 1;
             BuildMsgArray(dsLSTM0100:'L':compCount:inqCount);
          Endsl;
       Enddo;

       CloseList(dsMsgListInfo.handle:QUSEC);

       // Return values
       completion = compCount;
       inquiry = inqCount;

       Return;
      /End-free

     PCountMsgs        e

     PCloseMsgQ        b                   Export
     DCloseMsgQ        pi

     DqGyClst          pr                  ExtPgm('QGYCLST')
     D Handle                         4a
     D Error                        256a   Options(*varsize)

      /free
       qGyClst(ListInfoDs.Handle:QUSEC);
      /end-free

     PCloseMsgQ        e

     POpenMsgQ         b                   Export
     DOpenMsgQ         pi              n
     D msgQ                          10a   const
     D criteria                      10a   options(*nopass) const
     D*
     DqGyOlMsg         pr                  ExtPgm('QGYOLMSG')
     D Rcvr                           1a
     D RcvrLength                    10i 0 Const
     D ListInfo                            LikeDs(ListInfoDs)
     D Recs2Rtrn                     10i 0 Const
     D Sort                           1a   Const
     D Select                              LikeDs(SelectDs)
     D LenSelect                     10i 0 Const
     D QueueInfo                     21a   Const
     D MsgQueue                      44a
     D Error                        256a   Options(*varsize)

     DQueueInfoDs      ds                  qualified
     D UserOrQueue                    1a   Inz('0')
     D QueueName                     10a
     D QueueLib                      10a

     DSelectDs         ds                  qualified
     D Direction                     10a   Inz('*PRV')
     D                                2a
     D Severity                      10i 0 Inz(0)
     D MaxMsgLen                     10i 0 Inz(-1)
     D MaxHelpLen                    10i 0 Inz(0)
     D OSSelect                      10i 0 Inz(44)
     D NbrSelect                     10i 0 Inz(1)
     D OSMsgKeys                     10i 0 Inz(54)
     D OSFieldIDs                    10i 0 Inz(58)
     D NbrFields                     10i 0 Inz(numberOfFields)
     D Criteria                      10a   Dim(1) Inz('*ALL')
     D MsgKey                         4a   Dim(1) Inz(x'FFFFFFFF')
     D FldID                         10i 0 Dim(numberOfFields)

     DMsgQueueUsed     s             44a
     DRcvr             s              1a
     DretField         s               n   inz('1')

      /free
       Monitor;
          Clear record#;
          Reset QUSEC;

          //  set queue name
          QueueInfoDs.QueueName = MsgQ;

          //  select messages to return
          If %parms() = 2;
             SelectDs.Criteria(1) = Criteria;
          Endif;

          //  select fields to return
          SelectDs.FldID(1) = 1001;      //  Reply status
          SelectDs.FldID(2) = 0201;      //  Replacement data or
          SelectDs.FldID(3) = 0601;      //  qualified sender job name
          SelectDs.FldID(4) = 0301;      //  Message

          //  list messages
          qGyOlMsg(Rcvr: %size(Rcvr): ListInfoDs: -1: '0': SelectDs:
           %size(SelectDs): QueueInfoDs: MsgQueueUsed: QUSEC);

          //  check for errors
          If QUSEI <> *blank;
             retField = *off;
          Endif;
       On-Error;
          retField = *off;
       Endmon;

        //  close list on error
       If retField = *off;
          CloseMsgQ();
       Endif;

       Return retField;
      /end-free
     POpenMsgQ         e

      //===================================================================
      // Create list from CL (RTVMSG)
      //===================================================================
     PListFromCL       b
     DListFromCL       pi             4
     D inMsgQ                        10a   const

     DStRtvMsgwC       pr                  ExtPgm('STRTVMSGWC')
     D ioMsgKey                       4
     D outUser                       10    const
     D inMsg                        132
     D inMsgType                      2
     D inMsgId                        7
     D inMsgF                        10
     D inMsgfLib                     10
     D inSenderFmt                   80

     DstrMsgKey        s              4    inz('*TOP')
     DstrMsg           s            132
     DstrMsgType       s              2
     DstrMsgId         s              7
     DstrMsgF          s             10
     DstrMsgFLib       s             10
     DdsSenderFmt      ds            80
     D strJobName                    10
     D strJobUser                    10
     D strJob#                        6
     D strSendPgm                    16
     D strDateSent                    7
     D strTimeSent                    6
     D strSentTo                     14
     D strSenderType                  1
     D strSentToType                  1

     DdsInMsgInfo      ds                  inz qualified
     D strMsgType                     2
     D strDateSent                    7
     D strTimeSent                    6
     D strMsgId                       7
     D strMsgFile                    10
     D strMsgFileLib                 10
     D strMsgQue                     10
     D strMsgQueLib                  10
     D strMsg                       132

      /Free
       Dou strMsgKey = '*END';
          StRtvMsgwC(strMsgKey:inMsgQ:strMsg:strMsgType:strMsgId:strMsgF:
           strMsgFLib:dsSenderFmt);
          Select;
          When strMsgKey = '*END';
          //***  Begin Add  ***  04/27/15  *******************************
          When strMsgKey = '*NOQ';
             Clear outJSON;
             BuildJSON(outJSON:'ErrorMsg':'Message queue for ' +
              %trim(strUser) +
              ' was not found.  Please contact system administrator.');
             Leave;
          //***  End   Add  ***  04/27/15  *******************************
          When strMsgKey = '*GS';
             Clear outJSON;
             BuildJSON(outJSON:'ErrorMsg':'Message queue ' +
              %trim(strUser) + ' is allocated to another job.  +
              Possibly a green screen session.');
             Leave;
          When (intInqMsg + intCompMsg) >= maxMsgArray;
             Leave;
          Other;
             If strMsgType = '01' or strMsgType = '05';
                Select;
                When strMsgType = '01';
                   intCompMsg += 1;
                When strMsgType = '05';
                   intInqMsg += 1;
                Endsl;
                dsInMsgInfo.strMsg        = strMsg;
                dsInMsgInfo.strMsgType    = strMsgType;
                dsInMsgInfo.strDateSent   = strDateSent;
                dsInMsgInfo.strTimeSent   = strTimeSent;
                dsInMsgInfo.strMsgId      = strMsgId;
                dsInMsgInfo.strMsgFile    = strMsgF;
                dsInMsgInfo.strMsgFileLib = strMsgFLib;

                dsMsgInfo.aryMsgInfo(intInqMsg + intCompMsg) =
                 dsInMsgInfo;
             Endif;
          Endsl;
       Enddo;

       Return strMsgKey;
      /End-Free

     PListFromCL       e


     PMsgQExists       b                   Export
     DMsgQExists       pi              n
     D msgQ                          10a   const

     DisError          s              1n

      /Free
       Callp CheckObj(msgQ:'*MSGQ':isError);
       Return not isError;
      /End-free

     PMsgQExists       e

     PReadMsgQ         b                   Export
     DReadMsgQ         pi              n

     DqGyGtLe          pr                  ExtPgm('QGYGTLE')
     D Rcvr                                LikeDs(RcvrDs)
     D RcvrLen                       10i 0 Const
     D Handle                         4a
     D ListInfo                            LikeDs(ListInfoDs)
     D Recs2Rtrn                     10i 0 Const
     D Rec#2Rtrn                     10i 0
     D Error                        256a   Options(*varsize)

     DqMhRtvM          pr                  ExtPgm('QMHRTVM')
     D MsgInfo                             Like(MsgData)
     D LenMsgInfo                    10i 0 Const
     D Format                         8a   Const
     D MsgId                          7a   Const
     D MsgFile                       20a   Const
     D ReplaceData                         Const Like(ReplacementData)
     D LenReplaceData...
     D                               10i 0 Const
     D SubValues                     10a   Const
     D FmtCtlChar                    10a   Const
     D Error                        256a   Options(*varsize)

     DListDataDs       ds                  Based(pListDataDs)
     D                                     qualified
     D OSNextFld                     10i 0
     D LenFldInfo                    10i 0
     D FieldID                       10i 0
     D TypeOfData                     1a
     D StsOfData                      1a
     D                               14a
     D LenOfData                     10i 0
     D Data                       32000a

     DMsgData          ds                  qualified
     D BytesReturn                   10i 0
     D BytesAvail                    10i 0
     D LenMsgReturn                  10i 0
     D LenMsgAvail                   10i 0
     D LenMsgHelpRtn                 10i 0
     D LenMsgHelpAvl                 10i 0
     D Msg                         2048a

     DRcvrDs           ds                  qualified inz
     D OSNextMsg                     10i 0
     D OSFields                      10i 0
     D NbrFlds                       10i 0
     D MsgSev                        10i 0
     D MsgID                          7a
     D MsgType                        2a
     D MsgKey                         4a
     D MsgFile                       10a
     D MsgFileLib                    10a
     D MsgQ                          10a
     D MsgQLib                       10a
     D DateSent                       7a
     D TimeSent                       6a
     D*ReplaceData                         like(dsReplaceData)
     D EntryArr                1  32767a   Dim(32767)

     DSelectDs         ds                  qualified
     D Direction                     10a   Inz('*PRV')
     D                                2a
     D Severity                      10i 0 Inz(0)
     D MaxMsgLen                     10i 0 Inz(-1)
     D MaxHelpLen                    10i 0 Inz(0)
     D OSSelect                      10i 0 Inz(44)
     D NbrSelect                     10i 0 Inz(1)
     D OSMsgKeys                     10i 0 Inz(54)
     D OSFieldIDs                    10i 0 Inz(58)
     D NbrFields                     10i 0 Inz(numberOfFields)
     D Criteria                      10a   Dim(1) Inz('*ALL')
     D MsgKey                         4a   Dim(1) Inz(x'FFFFFFFF')
     D FldID                         10i 0 Dim(numberOfFields)

     DdsInMsgInfo      ds                  inz qualified
     D strMsgType                     2
     D strDateSent                    7
     D strTimeSent                    6
     D strMsgId                       7
     D strMsgFile                    10
     D strMsgFileLib                 10
     D strMsgQue                     10
     D strMsgQueLib                  10
     D strMsg                       132

     DreplacementData...
     D                 s            512a   varying
     DintRepDataLen    s             10i 0
     DbinRepDataLen    s              4b 0
     DretField         s               n   inz('1')
     DDataLocation     s              5u 0
     Di                s              5u 0
     Dx                s              3u 0

      /free
       Monitor;
       Dou QUSEI <> *blank; // Error
          record# += 1;
          Reset QUSEC;

          //  retrieve entry from list
          Clear rcvrDs;
          qGyGtLe(RcvrDs: %size(RcvrDs): ListInfoDs.Handle: ListInfoDs:
           1:record#: QUSEC);

          If QUSEI = *blank; // No Error
             Select;
             When rcvrDs.msgType = '01';
                intCompMsg += 1;
             When rcvrDs.msgType = '05';
                intInqMsg += 1;
             Endsl;
             // parse out msg from list entry
             DataLocation = RcvrDs.OSFields + 1;
             For x = 0 to (RcvrDs.NbrFlds - 1);
                pListDataDs = %addr( RcvrDs.EntryArr(DataLocation) );
                DataLocation += ListDataDs.LenFldInfo;
                Select;
                When ListDataDs.FieldID = 1001;
                // control field => initialize
                   Clear i;
                   Iter;
                When ListDataDs.FieldID = 201;
                // store replacement data
                   intRepDataLen = ListDataDs.LenOfData;
                   binRepDataLen = intRepDataLen;
                   replacementData = %subst(ListDataDs.Data: 1:
                    intRepDataLen);
                When ListDataDs.FieldID = 601;
                // store job info
       //          QualifiedJob = %subst(ListDataDs.Data: 1:
       //           ListDataDs.LenOfData);
                When ListDataDs.FieldID = 301
                 and rcvrDs.MsgId <> *blank;
                // combine message w/ replacement data
                   qMhRtvM(MsgData: %len(MsgData): 'RTVM0100':
                    rcvrDs.MsgId:rcvrDs.msgFile + rcvrDs.MsgFileLib:
                    replacementData:intRepDataLen:'*YES':
                    '*NO': QUSEC);
                   qMhRtvM(MsgData: %len(MsgData): 'RTVM0100':
                    rcvrDs.MsgId:rcvrDs.msgFile + rcvrDs.MsgFileLib:
                    replacementData:binRepDataLen:'*YES':
                    '*NO': QUSEC);
                   qMhRtvM(MsgData: %len(MsgData): 'RTVM0100':
                    rcvrDs.MsgId:rcvrDs.msgFile + rcvrDs.MsgFileLib:
                    replacementData:%len(replacementData):'*YES':
                    '*NO': QUSEC);
                   If MsgData.Msg <> *blank;
                   // store in Msg
                      dsInMsgInfo.strMsg = %subst(msgData.Msg: 1:
                       msgData.LenMsgReturn);
                   Else;
                   // if result is blank, use msg from list
                      dsInMsgInfo.strMsg = %subst(ListDataDs.Data: 1:
                       ListDataDs.LenOfData);
                   Endif;
                   dsInMsgInfo.strMsgType = rcvrDs.msgType;
                   dsInMsgInfo.strDateSent = rcvrDs.dateSent;
                   dsInMsgInfo.strTimeSent = rcvrDs.timeSent;
                   dsInMsgInfo.strMsgId = rcvrDs.msgId;
                   dsInMsgInfo.strMsgFile = rcvrDs.msgFile;
                   dsInMsgInfo.strMsgFileLib = rcvrDs.msgFileLib;
                   dsInMsgInfo.strMsgQueLib = rcvrDs.msgQ;
                   dsInMsgInfo.strMsgQueLib = rcvrDs.msgQLib;

                   dsMsgInfo.aryMsgInfo(intInqMsg + intCompMsg) =
                    dsInMsgInfo;
                Endsl;
             Endfor;
          Endif;
       Enddo;
       On-Error;
         retField = *off;
       Endmon;

       //  close list on error
       If retField = *off;
          CloseMsgQ();
       Endif;

       Return retField;
      /end-free

     PReadMsgQ         e
