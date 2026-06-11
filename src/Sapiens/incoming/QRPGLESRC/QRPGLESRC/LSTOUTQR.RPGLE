      *=======================================================================
      * PROGRAM - LSTOUTQR
      * PURPOSE - list ocntents of outq to a db file
      * WRITTEN - 07/06/06
      * AUTHOR  - MG
      *
      * PROGRAM DESCRIPTION
      *   This program will provide a list of spooled files that exist in
      *   the outq specified.  The spooled file info will be put into a new DB f
      *
      *
      * INPUT PARAMETERS
      *   Description        Type  Size    How Used
      *   -----------        ----  ----    --------
      *   QQoutq             Char  10      outq name that will be accessed
      *   QQoqlib            Char  10      outq library
      *
      * INDICATOR USAGE
      *   n/a
      *
      *========================================================================

     fLSTOUTQP  O    e             disk
      *
      * Program Info
      *
     d PgmInfo        SDS
     d  @JobName             244    253
     d  @UserId              254    263
     d  @JobNumber           264    269  0
      *
      * API - List spooled files
      *
     d QUSLSPL         PR                  extpgm('QUSLSPL')
      * required parameters
     d   UsrSpc                      20A   const
     d   Format                       8A   const
     d   UserName                    10A   const
     d   QualOutQ                    20A   const
     d   FormType                    10A   const
     d   UserData                    10A   const
      * optional group 1:
     d   ErrorCode                32766A   options(*nopass: *varsize)
      * optional group 2:
     d   QualJob                     26A   options(*nopass) const
     d   FieldKeys                   10I 0 options(*nopass: *varsize)
     d                                     dim(9999)
     d   NumFields                   10I 0 options(*nopass) const
      * optional group 3:
     d   AuxStgPool                  10I 0 options(*nopass) const
      * optional group 4:
     d   JobSysName                   8A   options(*nopass) const
     d   StartCrtDate                 7A   options(*nopass) const
     d   StartCrtTime                 6A   options(*nopass) const
     d   EndCrtDate                   7A   options(*nopass) const
     d   EndCrtTime                   6A   options(*nopass) const
      *
     d QUSCRTUS        PR                  extpgm('QUSCRTUS')
     d   UsrSpc                      20A   const
     d   ExtAttr                     10A   const
     d   InitialSize                 10I 0 const
     d   InitialVal                   1A   const
     d   PublicAuth                  10A   const
     d   Text                        50A   const
     d   Replace                     10A   const
     d   ErrorCode                32766A   options(*nopass: *varsize)
      *
     d QUSPTRUS        PR                  extpgm('QUSPTRUS')
     d   UsrSpc                      20A   const
     d   Pointer                       *
      *
     d QUSDLTUS        PR                  extpgm('QUSDLTUS')
     d   UsrSpc                      20A   const
     d   ErrorCode                32766A   options(*varsize)
      *
     d p_UsrSpc        s               *
     d dsLH            DS                   based(p_UsrSpc)
     d                                      qualified
     d   Filler1                    103A
     d   Status                       1A
     d   Filler2                     12A
     d   HdrOffset                   10I 0
     d   HdrSize                     10I 0
     d   ListOffset                  10I 0
     d   ListSize                    10I 0
     d   NumEntries                  10I 0
     d   EntrySize                   10I 0
      *
     d p_Entry         s               *
     d dsSF            DS                   based(p_Entry)
     d                                      qualified
     d   JobName                     10A
     d   UserName                    10A
     d   JobNumber                    6A
     d   SplfName                    10A
     d   SplfNbr                     10I 0
     d   SplfStatus                  10I 0
     d   OpenDate                     7A
     d   OpenTime                     6A
     d   Schedule                     1A
     d   SysName                     10A
     d   UserData                    10A
     d   FormType                    10A
     d   OutQueue                    10A
     d   OutQueueLib                 10A
     d   AuxPool                     10I 0
     d   SplfSize                    10I 0
     d   SizeMult                    10I 0
      *
     d   TotalPages                  10I 0
     d   CopiesLeft                  10I 0
     d   Priority                     1A
     d   Reserved1                    3A
      *
     d dsEC            DS                  qualified
     d  BytesProvided                10I 0 inz(%size(dsEC))
     d  BytesAvail                   10I 0 inz(0)
     d  MessageID                     7A
     d  Reserved2                     1A
     d  MessageData                 240A
      *
      * constants
      *
     d Low             c                   const('abcdefghijklmnopqrstuvwxyz')
     d MYSPACE         c                   const('SPLFLIST  QTEMP     ')
     d Q               c                   const('''')
     d Up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     d OutQName        ds
     d   OutQ                        10A   inz(*blanks)
     d   OutQLib                     10A   inz(*blanks)
      *
      *  Field Definitions.
      *
     d CmdString       s            256    inz(*blanks)
     d CmdLength       s             15  5 inz(0)
     d JobInfo         s            256    inz(*blanks)
     d Len             s              4  0
     d Loop            s              1    inz('Y')
     d QDATAQ          s             10    inz('MS2MOVESPL')
     d QLIB            s             10    inz('REROUTE')
     d QLEN            s              5  0 inz(256)
     d QDATA           s            256
     d QWAIT           s              5  0

     d size            s             10I 0
     d sf              s             10I 0 inz(1)
     d wJobName        s             10    inz(*blanks)
     d wSPLFName       s             10    inz(*blanks)
     d wUserData       s             10    inz(*blanks)
      *
      *
      *========================================================================
      * MAIN LINE
      *========================================================================
      *
     c     *entry        plist
     c                   parm                    qqoutq           10
     c                   parm                    qqoqlib          10
      *
      *
     c                   eval      OutQ    = qqoutq
     c                   eval      OutQLib = qqoqlib
     c                   exsr      $GetSPLFList
      *
      *

     c                   eval      *inlr = *on

      *========================================================================
      * $GetSPLFList - Get Spooled File List
      *========================================================================
     c     $GetSPLFList  begsr
      *
      * set this to zero to let OS/400 handle errors
      *
     c                   eval      dsEC.BytesProvided = 0
      *
      * Make space for (approx) 10000 spooled files to be listed
      *
     c                   eval      size = %size(dsLH) + 512 +
     c                                    (%size(dsSF) * 10000)
      *
      * Create a user space
      * List spooled files to the user space
      * Get a pointer to the returned user space
      *
      /free
         // Create a user space
         QUSCRTUS(MYSPACE: 'USRSPC': size: x'00': '*ALL':
          'Temp User Space for QUSLSPL API':  '*YES': dsEC);

         // List spooled files to the user space
         QUSLSPL(MYSPACE: 'SPLF0300': '*ALL': OutQName:
            '*ALL': '*ALL': dsEC);

         // Get a pointer to the returned user space
         QUSPTRUS(MYSPACE: p_UsrSpc);
      /end-free
      *
      * Loop through list, for each spooled file, display the
      * Status: 1=RDY , 2=OPN, 3=CLO, 4=SAV, 5=WRT, 6=HLD,
      *         7=MSGW, 8=PND, 9=PRT,10=FIN,11=SND,12=DFR
      *
     c                   eval      p_Entry = p_UsrSpc + dsLH.ListOffset
     c                   eval      sf = 1
     c                   dow       sf <= dsLH.NumEntries
      *
      * currently only move status = 1 ready
      *
     c                   if        dsSF.SplfStatus = 1
     c                   exsr      $MoveSplf
     c                   endif
      *
     c                   eval      p_Entry = p_Entry + dsLH.EntrySize
     c                   eval      sf = (sf + 1)
     c                   enddo
      *
      * delete user space
      *
      /free
          QUSDLTUS(MYSPACE: dsEC);
      /end-free
      *
      *
     c                   endsr

      *========================================================================
      * $MoveSplF  - Move the spooled file
      *========================================================================
     c     $MoveSPlf     begsr
      *
     c                   eval      JobInfo =
     c                             %trim(dsSF.JobNumber) +
     c                             %trim('/') + %trim(dsSF.UserName) +
     c                             %trim('/') + %trim(dsSF.JobName)
      *
     c                   eval      wSPLFName = dsSF.SplfName
     c                   eval      wJobName  = dsSF.JobName
     c                   eval      wUserData = dsSF.UserData
     c***
     c***
     C                   eval      jobnam = dsSF.jobname
     c                   eval      usrnam = dsSF.userName
     C                   eval      jobnbr = dsSF.jobNumber
     c                   eval      splnam  = dsSF.splfname
     c***
     C                   eval      splnbr = dsSF.splfnbr
     c                   eval      splsts = dsSF.splfstatus
     C                   eval      opndat = dsSF.OpenDate
     c                   eval      opntim  = dsSF.OpenTime
     C                   eval      sched  = dsSF.schedule
     c                   eval      sysnam = dsSF.sysName
     C                   eval      usrdta = dsSF.UserData
     c                   eval      Frmtyp  = dsSF.FormType
     c***
     C                   eval      outq   = dsSF.OutQueue
     c                   eval      outqlb = dsSF.OutQueueLib
     C                   eval      auxpol = dsSF.AuxPool
     c                   eval      splfsz  = dsSF.SplfSize
     C                   eval      szmult = dsSF.sizeMult
     c                   eval      totpgs = dsSF.TotalPages
     C                   eval      cpyslf = dsSF.CopiesLeft
     c                   eval      prty    = dsSF.Priority
     c                   eval      rsvrd1  = dsSF.Reserved1
     C***
     c                   write     LSTOQR
     C***
      *
      * Move the spool file
      * CHGSPLFA FILE(QPSUPRTF) JOB(546777/FLANARY/QPADEV0041)
      * SPLNBR(*LAST) CRTDATE(*LAST)  OUTQ(JAMIELIB/JAMIEOUT)
      *
     c*********          eval      CmdString = %trim('CHGSPLFA') +
     c*                            %trim('~FILE(')+%trim(wSPLFName)+%trim(')')+
     c*                            %trim('~JOB(') + %trim(JobInfo) + %trim(')')+
     c*                            %trim('~SPLNBR(') +
     c*                            %trim(%char(dsSF.SplfNbr)) + %trim(')')
     c*                            + ' OUTQ(' + %Trim(MSZLIBT)  + %trim('/') +
     c*                            %trim(MSZOUTQT)  + %trim(')')
     c******             eval      CmdString = %xlate('~' : ' ' : CmdString)
      *
      * Move the spooled file ...
      *
     c                   eval      CmdLength = %len(%trim(CmdString))
     c*****************  call(e)   'QCMDEXC'
     c*****************  parm                    CmdString
     c*****************  parm                    CmdLength
      *
      * SNDMSG MSG('Moved spooled file') TOMSGQ(*SYSOPR)
      *
     c**********         eval      CmdString = %trim('SNDMSG MSG(') +
     c*                            %trim(Q) + %trim('Moved spooled file~')   +
     c*                            %trim('~FILE(')+%trim(wSPLFName)+%trim(')')+
     c*                            %trim('~JOB(') + %trim(JobInfo) + %trim(')')+
     c*                            %trim('~SPLNBR(') +
     c*                            %trim(%char(dsSF.SplfNbr)) + %trim(')')  +
     c*                             ' To Outq(' + %Trim(MSZLIBT)  + %trim('/') +
     c*                            %trim(MSZOUTQT)  + %trim(Q) + %trim(')')   +
     c************                 '  TOMSGQ(*SYSOPR)'
     c***********        eval      CmdString = %xlate('~' : ' ' : CmdString)
      *
      * send the message
      *
     c                   eval      CmdLength = %len(%trim(CmdString))
     c**************     call(e)   'QCMDEXC'
     c**************     parm                    CmdString
     c***************    parm                    CmdLength

     c                   endsr
