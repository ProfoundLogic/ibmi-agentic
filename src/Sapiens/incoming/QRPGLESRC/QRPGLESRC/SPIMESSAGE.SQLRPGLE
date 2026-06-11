     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H nomain
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*  04/27/22 C:62  Project: 129WWZ                                   *
    H*                 - Cast message text so it is corret ccsid         *
     H*                 - Add message_type parm to select main errors.
    H*                                                                   *
    H*  07/01/20 C:79  Project: 148WBU                                   *
    H*                -Add RTVMSGD procedure                             *
    H*********************************************************************
     D*
     D/copy sprmessage
     D/copy sprstring
     P*=====================================================================
     P* sndPgmMsg - sends a program message to the current program
     P*             message queue.
     P*  Parameter 1 - 10 character message ID
     P*  Parameter 2 - 10 character message type
     P*  Parameter 3 - 10 character message file
     P*  Parameter 4 - 10 character message file library
     P*  Parameter 5 - varible character message data
     P*  Returns - nothing
     P*=====================================================================
     PsndPgmMsg        B                   Export
     DsndPgmMsg        PI                  opdesc
     D msgInId                       10    const
     D msgInType                     10    const
     D msgFile                       10    const
     D msgLib                        10    const
     D msgInDta                   32766    const options(*varsize:*nopass)
     D
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
     D
     D msgData         s                   like(msgInDta)
     D msgDtaLen       s              6  0
     D msgFileLib      s             20
     D msgID           s             10
     D msgKey          s              9b 0
     D msgQueue        s             10
     D msgQueNbr       s              9b 0
     D msgType         s             10
     D
     DErrorDS          DS                  inz
     D bytesProv               1      4b 0
     D bytesAval               5      8b 0
     D messageID               9     15
     D err##                  16     16
     D messageDta             17    116
     D
     C                   move      msgInId       msgID
     C                   move      msgInType     msgType
     C                   movel     msgFile       msgFileLib
     C                   move      msgLib        msgFileLib
     C
     C                   if        %parms >= 5
     C                   callp     CEEDOD(5:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C                   move      inLen         msgDtaLen
     C                   move      msgInDta      msgData
     C                   else
     C                   clear                   msgDtaLen
     C                   clear                   msgDtaLen
     C                   endif
     C
     C                   if        msgType = '*STATUS'
     C                   movel(p)  '*EXT'        msgQueue
     C                   else
     C                   movel(p)  '*'           msgQueue
     C                   endif
     C
     C                   call      'QMHSNDPM'
     C                   parm                    msgId
     C                   parm                    msgFileLib
     C                   parm                    msgData
     C                   parm                    msgDtaLen
     C                   parm                    msgType
     C                   parm                    msgQueue
     C                   parm      1             msgQueNbr
     C                   parm                    msgKey
     C                   parm                    errorDS
     C
     C                   return
     PsndPgmMsg        E
     P*=====================================================================
     P* rmvMsg - remove program messages
     P*  Parameter 1 - 10 character message ID
     P*  Returns - nothing
     P*=====================================================================
     PrmvMsg           B                   Export
     DrmvMsg           PI
     D rmvID                         10    const options(*nopass)
     D
     D defRmvMsg       s             10    inz('*ALL      ')
     D callStack       s             10    inz('*         ')
     D callStackC      s              9b 0 inz(1)
     D rmvKey          s              4
     D rmvMsg          s             10
     D
     DErrorDS          DS                  inz
     D bytesProv               1      4b 0
     D bytesAval               5      8b 0
     D messageID               9     15
     D err##                  16     16
     D messageDta             17    116
     D
     C                   if        %parms = 1
     C                   move      rmvID         rmvMsg
     C                   else
     C                   move      defRmvMsg     rmvMsg
     C                   endif
     C
     C                   call      'QMHRMVPM'
     C                   parm                    callStack
     C                   parm                    callStackC
     C                   parm                    rmvKey
     C                   parm                    rmvMsg
     C                   parm                    errorDS
     C
     C                   return
     PrmvMsg           E

     P*=====================================================================
     P* RtvMsg - retrieve message
     P*  Parameter 1 - job info (job#/user/job) (* for current)
     P*  Parameter 2 - run info
     P*                 type - *FIRST/*LAST/*PRV/*NEXT
     P*                 pos  - position to start from
     P*                 pgm  - program sending the message
     P*                 lib  - library sending the message
     P*  Returns - message info
     P*                 pos  - position of message
     P*                 id   - message id
     P*                 text - message text
     P*=====================================================================
     PRtvMsg           B                   Export
     DRtvMsg           PI          1050
     D inJob                         28    const options(*nopass)
     D inInfo                              const options(*nopass)
     D                                     likeds(dsRtvMsgInfo)
     D*** Begin Add *** 04/27/22 *******************************************
     D inMessageType                 13    const varying options(*nopass)
     D*** End   Add *** 04/27/22 *******************************************

     D defRmvMsg       s             10    inz('*ALL      ')
     D callStack       s             10    inz('*         ')
     D callStackC      s              9b 0 inz(1)
     D rmvKey          s              4
     D rmvMsg          s             10

     daryMsgInfoNull   s              5i 0 dim(5)
     ddsMsgInfo        ds                  qualified inz
     D pos                           10i 0
     D msgId                          7
     D msgText                     1024
     d lib                           10
     d pgm                           10
     ddsMsgRtnInfo     ds                  likeds(dsRtnMsgInfo) inz

     DstrJob           s             28    inz
     DstrSql           s           5000    inz
     DstrSqlFrom       s           1000    inz
     DstrSqlWhere      s           1000    inz
     DstrSqlOrder      s           1000    inz

      /Free
       If %parms >= 1;
          strJob = inJob;
       Endif;
       If %parms >= 2;
          dsRtvMsgInfo = inInfo;
       Endif;
       If strJob = *blanks;
          strJob = '*';
       Endif;
       If dsRtvMsgInfo.type = *blank;
          dsRtvMsgInfo.type = '*LAST';
       Endif;
       strSqlFrom = 'from table(qsys2.joblog_info(''' + %trim(strJob) +
        ''')) a';
       If dsRtvMsgInfo.lib <> *blanks;
          strSqlWhere = %trim(strSqlWhere) + '/¢' + 'FROM_LIBRARY like (''' +
           %trim(dsRtvMsgInfo.lib) + ''')';
       Endif;
       If dsRtvMsgInfo.pgm <> *blanks;
          strSqlWhere = %trim(strSqlWhere) + '/¢' + 'FROM_PROGRAM like (''' +
           %trim(dsRtvMsgInfo.pgm) + ''')';
       Endif;
       strSqlOrder = 'order by ORDINAL_POSITION';
       Select;
       When dsRtvMsgInfo.type = '*FIRST';
       When dsRtvMsgInfo.type = '*LAST';
          strSqlOrder = %trim(strSqlOrder) + ' desc';
       When dsRtvMsgInfo.type = '*PRV';
          strSqlOrder = %trim(strSqlOrder) + ' desc';
          strSqlWhere = %trim(strSqlWhere) + '/¢' + 'ORDINAL_POSITION < ' +
           %char(dsRtvMsgInfo.pos);
       When dsRtvMsgInfo.type = '*NEXT';
          strSqlWhere = %trim(strSqlWhere) + '/¢' + 'ORDINAL_POSITION > ' +
           %char(dsRtvMsgInfo.pos);
       Endsl;
       strSqlWhere = ScanReplace('/¢':'Where ':%trim(strSqlWhere):1:2);
       strSqlWhere = ScanReplace('/¢':' and ' :%trim(strSqlWhere):2);
       //*** Begin Add *** 04/27/22 ***************************************
       If %parms() >= 3 And inMessageType <> ' ';
          strSqlWhere = %Trim(strSqlWhere) +
                        ' And MESSAGE_TYPE =''' + inMessageType + '''';
       EndIf;
       //*** End   Add *** 04/27/22 ***************************************
       //*** Begin Chg *** 04/27/22 ***************************************
       // strSql = 'select ORDINAL_POSITION, MESSAGE_ID, MESSAGE_TEXT, ' +
       //  'FROM_LIBRARY, FROM_PROGRAM ' +
       //  %trim(strSqlFrom) + ' ' + %trim(strSqlWhere) + ' ' +
       //  %trim(strSqlOrder) + ' ' + 'limit 1';
       strSql = 'select ORDINAL_POSITION, MESSAGE_ID, ' +
        'Cast(MESSAGE_TEXT as VarChar(1024)), ' +
        'FROM_LIBRARY, FROM_PROGRAM ' +
        %trim(strSqlFrom) + ' ' + %trim(strSqlWhere) + ' ' +
        %trim(strSqlOrder) + ' ' + 'limit 1';
       //*** End   Chg *** 04/27/22 ***************************************
       Exec SQL Prepare sqlMsg From :strSql;
       Exec SQL Declare cursorMsg Cursor For sqlMsg;
       Exec SQL Open cursorMsg;
       If SQLSTT = '00000';
          Exec SQL Fetch next From cursorMsg into :dsMsgInfo;
       Endif;
       If SQLSTT <> '00000';
          Clear dsMsgInfo;
       Endif;
       Exec SQL Close cursorMsg;
       dsMsgRtnInfo = dsMsgInfo;
       Return dsMsgRtnInfo;
      /End-free
     PRtvMsg           e
     P*** Beg Add 07/01/20 *************************************************
     P*=====================================================================
     P* RtvMsgD - retrieve message description
     P*  Parameter 1 - input message File
     P*  Parameter 2 - input message ID
     P*                 type - *FIRST/*LAST/*PRV/*NEXT
     P*                 pos  - position to start from
     P*  Returns - message description
     P*=====================================================================
     PRtvMsgD          B                   Export
     DRtvMsgD          PI           500
     D inMsgF                        10    const options(*nopass)
     D inMsgID                        7    const options(*nopass)

     d dsRtvM0100      ds                  qualified inz
     D bytesReturn                   10i 0
     D bytesAvl                      10i 0
     D rtnMsgLen                     10i 0
     D rtnMsgAvl                     10i 0
     D rtnHlpLen                     10i 0
     D rtnHlpAvl                     10i 0
     D rtnMsg                      1024A

     d dsMsgFile       ds                  qualified inz
     D msgFileNm                     10a
     D msgFileLib                    10a   Inz('*LIBL')

     d dsErrorCode     ds                  qualified inz
     D ErrBytesProv                  10i 0 Inz(%Size(dsErrorCode))
     D ErrBytesAvl                   10i 0 Inz(100)
     D ErrMsgID                       7A
     D Reserved                       1A
     D Msgdata                     3000A

     d  MsgInfo        ds                  LikeDs(dsRtvM0100)

     d  MsgInfoLen     s             10i 0 Inz(%Size(dsRtvM0100))
     d  Format         s              8a   Inz('RTVM0100')
     d  MsgID          s              7a
     d  MsgF           s                   Like(dsMsgFile)
     d  RepData        s              1a   Inz(*blanks)
     d  RepDataLen     s             10i 0 Inz(1)
     d  RepSubValues   s             10a   Inz('*YES')
     d  RetCntChrs     s             10a   Inz('*NO')
     d  ErrorCode      s                   Like(dsErrorcode)
      /Free
                         MsgID = inMsgID;
                         dsMsgFile.MsgFileNm  = InMsgF;
                         msgF = dsMsgFile;
      /End-free
     C                   call      'QMHRTVM'
     C                   parm                    msgInfo
     C                   parm                    msgInfoLen
     C                   parm                    Format
     C                   parm                    msgID
     C                   parm                    msgF
     C                   parm                    RepData
     C                   parm                    RepDataLen
     C                   parm                    RepSubValues
     C                   parm                    RetCntChrs
     C                   parm                    Errorcode
      /Free
       Return %Subst(msgInfo.rtnMsg:1:msgInfo.rtnMsgLen);
      /End-free
     PRtvMsgD          e
     P*** End Add 07/01/20 *************************************************
