     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/TITLE SHWOBJLCK - Show Object Lock
     h/DEFINE   PROFOUNDUI
     h dftactgrp(*no) actgrp('QILE')
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Object Locks
     h*
     h*  Called from: Menu
     h*  Calls to: None
     h*    RTVCLTID
     h*
     h*  Created on 2/6/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h*  10/25/12 C:284 Project: 114WGD
     h*                 CSI Retrofit of Custom Code to Base
     h*
     h**********************************************************************
     fshwObjLck cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('SHWOBJLCK')
     f                                     Sfile(sflObjLock:rrnObjLock)
     f*                                    InfDs(fids01)

     D CrtUsrSpc       pr                  ExtPgm('QUSCRTUS')
     D   userSpace                   20a   Const
     D   extAttr                     10a   Const
     D   initSize                    10i 0 Const
     D   initVal                      1a   Const
     D   publicAuth                  10a   Const
     D   text                        50a   Const
     D   replace                     10a   Const
     D   errorCode                32766a   Options(*varsize)

     D RtvPtrUS        pr                  ExtPgm('QUSPTRUS')
     D   userSpace                   20a   Const
     D   pointer                       *

     D LstObjLck       pr                  ExtPgm('QWCLOBJL')
     D   userSpace                   20a   Const
     D   format                       8a   Const
     D   object                      20a   Const
     D   objType                     10a   Const
     D   member                      10a   Const
     D   errorCode                32766a   Options(*varsize)

     D GetCurrentUser  pr            10a
     D   strJobIn                    26a   Const

     D*****************************************************
     D* API error code data structure
     D*****************************************************
     D dsEC            ds
     D*                                    Bytes Provided (size of struct)
     D  dsECBytesP             1      4i 0 INZ(256)
     D*                                    Bytes Available (returned by API)
     D  dsECBytesA             5      8i 0 INZ(0)
     D*                                    Msg ID of Error Msg Returned
     D  dsECMsgID              9     15a
     D*                                    Reserved
     D  dsECReserv            16     16a
     D*                                    Msg Data of Error Msg Returned
     D  dsECMsgDta            17    256a

     D*****************************************************
     D* List API generic header data structure
     D*****************************************************
     D dsLH            ds                   BASED(p_UserSpace)
     D*                                     Filler
     D   dsLHFill1                  103a
     D*                                     Status (I=Incomplete,C=Complete
     D*                                             F=Partially Complete)
     D   dsLHStatus                   1a
     D*                                     Filler
     D   dsLHFill2                   12a
     D*                                     Header Offset
     D   dsLHHdrOff                  10i 0
     D*                                     Header Size
     D   dsLHHdrSiz                  10i 0
     D*                                     List Offset
     D   dsLHLstOff                  10i 0
     D*                                     List Size
     D   dsLHLstSiz                  10i 0
     D*                                     Count of Entries in List
     D   dsLHEntCnt                  10i 0
     D*                                     Size of a single entry
     D   dsLHEntSiz                  10i 0

     D*****************************************************
     D*  List Object Locks API format OBJL0100
     D*****************************************************
     D dsOL            ds                  based(p_Entry)
     D*                                     Job Name
     D  dsOL_JobName                 10a
     D*                                     Job User Name
     D  dsOL_UserName                10a
     D*                                     Job Number
     D  dsOL_JobNbr                   6a
     D*                                     Lock State
     D  dsOL_LckState                10a
     D*                                     Lock Status
     D  dsOL_LckSts                  10i 0
     D*                                     Lock Type
     D  dsOL_LckType                 10i 0
     D*                                     Member (or *BLANK)
     D  dsOL_Member                  10a
     D*                                     1=Shared File, 0=Not Shared
     D*                                        (or 0=not applicable)
     D  dsOL_Share                    1a
     D*                                     Lock Scope
     D  dsOL_LckScope                 1a
     D*                                     Thread identifier
     d  dsOL_ThreadID                 8a


     d p_UserSpace     s               *
     d p_Entry         s               *
     d Msg             s             50a
     d x               s             10i 0
     d objName         s             10a
     d objLib          s             10a
     d objType         s             10a
     d member          s             10a

     C     *entry        plist
     c                   parm                    objName
     C                   parm                    objLib
     c                   parm                    objType
     c                   parm                    member


      /free

       scObjLock=%Trim(objLib) + '/' + %Trim(objName);

       //===========================================================================================
       // Create a user space to store output of
       // the list object locks API
       //===========================================================================================
       CrtUsrSpc('OBJLOCKS  QTEMP': 'USRSPC':1: x'00': '*ALL':
                   'Output of List Object Locks API': '*YES': dsEC);
       If dsECBytesA > 0 ;
          Msg = 'QUSCRTUS error ' + dsECMsgID;
          return;
       EndIf;

       //===========================================================================================
       // Dump the Object Locks to the user space
       //===========================================================================================
       LstObjLck('OBJLOCKS  QTEMP': 'OBJL0100':objName+objLib: objType:
                  member: dsEC);
       If dsECBytesA > 0;
          Msg = 'QWCLOBJL error ' + dsECMsgID;
          Return;
       EndIf;

       //===========================================================================================
       // Get a pointer to the user space
       //===========================================================================================
       RtvPtrUS('OBJLOCKS  QTEMP': p_UserSpace);

       //===========================================================================================
       // Clear Subfile
       //===========================================================================================
         rrnObjLock=0;
         clrObjLock=*On;
         dspObjLock=*Off;
         Write rcdObjLock;
         dspObjLock=*On;
         clrObjLock=*Off;
       //===========================================================================================
       // Read each entry in the list and display the lock details
       //===========================================================================================
         For       x = 0 to (dsLHEntCnt-1);
            p_Entry = p_UserSpace + (dsLHLstOff + (dsLHEntSiz*x));

            // Job Name
            scJobName = %trimr(dsOL_JobNbr) + '/' +
                  %trimr(dsOL_UserName) + '/' + %trimr(dsOL_JobName);

            // Lock State
            scLockStat = dsOL_LckState;

            // Lock Status
            Select;
            When dsOL_LckSts = 1;
               scLockSts = 'HELD';
            When dsOL_LckSts = 2;
               scLockSts = 'WAIT';
            When dsOL_LckSts = 2;
               scLockSts = 'REQ';
            EndSl;

            // Lock Type
            Select;
            When dsOL_LckType = 1;
               scLockType = 'OBJECT';
            When dsOL_LckType = 2;
               scLockType = 'MBR CTL BLK';
            When dsOL_LckType = 3;
               scLockType = 'MBR ACC PTH';
            When dsOL_LckType = 4;
               scLockType = 'MBR DATA';
            EndSl;

            // Member
            scMember = dsOL_Member;

            // Share Lock
            If dsOL_Share = '1';
               scShared = 'YES';
            Else;
               scShared = 'NO';
            EndIf;

            // Current User
            scCurrUser=GetCurrentUser(dsOL_JobName+dsOL_UserName+dsOL_JobNbr);

            rrnObjLock=rrnObjLock+1;
            Write sflObjLock;

         EndFor;

       //===========================================================================================
       // Display Screen
       //===========================================================================================
         If rrnObjLock=0;
            scJobName='Object is not locked.';
            rrnObjLock=rrnObjLock+1;
            Write sflObjLock;
         EndIf;
         ExFmt RcdObjLock;

         *inLR = *On;
      /end-free
       //===========================================================================================
       // Get the current user
       //===========================================================================================
     P GetCurrentUser  B
     D GetCurrentUser  PI            10
     d   strJobIn                    26    Const

     D QusrJobI        PR                  EXTPGM('QUSRJOBI')
     D   RcvVar                   32766A   options(*varsize)
     D   RcvVarLen                   10I 0 const
     D   Format                       8A   const
     D   QualJob                     26A   const
     D   InternJob                   16A   const
     D   ErrorCode                32766A   options(*nopass:*varsize)

     D dsJob           DS
     D  dsJobBytesRtn                10I 0
     D  dsJobBytesAvl                10I 0
     D  dsJobName                    10A
     D  dsJobUser                    10A
     D  dsJobNumber                   6A
     D  dsJobIntern                  16A
     D  dsJobStatus                  10A
     D  dsJobType                     1A
     D  dsJobSubtype                  1A
     D  dsJobSwitch                   8A
     D  dsJobEndSts                   1A
     D  dsSbsName                    10A
     D  dsSbsLibName                 10A
     D  dsCurrentUser                10A   Inz(' ')
     D  dsDbcsCapable                 1A
     D  dsExitKey                     1A
     D  dsCancelKey                   1A
     D  dsProdRtnKey                 10I 0
     D  dsUserRtnKey                 10I 0
     D  dsProgRtnKey                 10I 0
     D  dsSpecialEnv                 10A
     D  dsDeviceName                 10A
     D  dsGrpPrfName                 10A
     D  dsGrpPrfSupp                150A
     D  dsJobUserId                  10A
     D  dsJobUserSett                 1A
     D  dsClientIp                   15A
     D  dsReserved                    2A
     D  dsTimeZoneOff                10I 0
     D  dsTimeZoneLen                10I 0

      /free
       QusrJobI(dsJob: %size(dsJob):'JOBI0600':strJobIn:*Blanks);
       Return dsCurrentUser;
      /End-Free

     P GetCurrentUser  E
