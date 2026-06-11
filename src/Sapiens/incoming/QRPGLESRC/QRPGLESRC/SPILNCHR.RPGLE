     H copyright('Progressive Data Solutions, Inc 2002')
     H nomain
    H*********************************************************************
    H* Revisions:                                                        *
    H*                                                                   *
    H* 11/02/10  C:183 Project: SY195                                    *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
    H*                 get the root folder name.                         *
    H*                                                                   *
    H*********************************************************************
     H
     Fslnpf     if   e             disk
     F
     D/copy sprRunCmd
     D/copy sprlnchr
     DgetRecord        PR
     D
     D recRead         s               n

     C     cmdParms      plist
     C                   parm                    handle
     C                   parm                    command
     C                   parm                    option
     C                   parm                    parameter1
     C                   parm                    parameter2
     C                   parm                    result
     C

     PaddBookMrk       B                   Export
     DaddBookMrk       PI
     D name                                like(parameter1) const
     C                   movel(p)  'WBOOKMADD'   command
     C                   movel     *blanks       option
     C                   movel(p)  name          parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PaddBookMrk       E

     PcheckFile        B                   Export
     DcheckFile        PI              n
     D file                                like(parameter1) value
     D
     D exists          s               n
     D
     C                   movel(p)  'CHKFILE'     command
     C                   movel     *blanks       option
     C                   movel(p)  file          parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   if        result = 'TRUE'
     C                   eval      exists = *on
     C                   endif
     C                   return    exists
     PcheckFile        E

     PcloseLnchr       B                   Export
     DcloseLnchr       PI
     C                   call      'LNCCLOSE'
     C                   parm                    handle
     C                   return
     PcloseLnchr       E

     PcloseWord        B                   Export
     DcloseWord        PI
     C                   movel(p)  'WORDCLOSE'   command
     C                   movel     *blanks       option
     C                   movel     *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PcloseWord        E

     PendComm          B                   Export
     DendComm          PI
     C                   movel(p)  'END'         command
     C                   movel     *blanks       option
     C                   movel     *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PendComm          E

     PgetDftSvr        B
     DgetDftSvr        PI            10
     D
     D server          s             10
     C                   callp     getRecord
     C                   eval      server = lnDftSvr
     C                   return    server
     PgetDftSvr        E

     PgetGenDir        B                   Export
     DgetGenDir        PI            50
     D
     D dir             s             50
     C                   eval      dir = %trim(getNetSvr)
     C                             + 'pds\generated documents\'
     C*************                + 'windows\temp\'
     C                   return    dir
     PgetGenDir        E

     PgetNetSvr        B                   Export
     DgetNetSvr        PI            20
     D
     D server          s             20
     C                   callp     getRecord
     C                   eval      server = '"\\' + %trim(lnNetSvr) + '\'
     C                   return    server
     PgetNetSvr        E

     PgetQDLSDir       B                   Export
     DgetQDLSDir       PI            50
     D
     D dir             s             50
      *** Beg chg ***** 11/02/10 ***********************************************
     D root            s             10    inz
     C                   call      'RTVDOCROOT'
     C                   parm                    root
     C******             eval      dir = %trim(getNetSvr)
     C******                       + 'qdls\'
     C                   eval      dir = %trim(getNetSvr)
     C                             + %trim(root) + '\'
      *** End chg ***** 11/02/10 ***********************************************
     C                   return    dir
     PgetQDLSDir       E

     PgetRecord        B
     DgetRecord        PI
     D
     C                   if        not recRead
     C     1             chain     slnpf                              99
     C                   eval      recRead = *on
     C                   endif
     C
     C                   return
     PgetRecord        E

     PgetShlDir        B                   Export
     DgetShlDir        PI            50
     D
     D dir             s             50
     C                   eval      dir = %trim(getNetSvr)
     C                             + 'pds\shell documents\'
     C*************                + 'program files\microsoft office\'
     C                   return    dir
     PgetShlDir        E

     PinsertFile       B                   Export
     DinsertFile       PI
     D fileName                            like(parameter1) const
     C                   movel(p)  'WINSERTF'    command
     C                   movel     *blanks       option
     C                   movel(p)  fileName      parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PinsertFile       E

     PinsertText       B                   Export
     DinsertText       PI
     D text                                like(parameter1) const
     C                   movel(p)  'WTYPETEXT'   command
     C                   movel     *blanks       option
     C                   movel(p)  text          parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PinsertText       E

     PnewDocument      B                   Export
     DnewDocument      PI
     D template                            like(parameter1) const
     C                   movel(p)  'WNEWFILE'    command
     C                   movel     *blanks       option
     C                   movel(p)  template      parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PnewDocument      E

     PopenAsRead       B                   Export
     DopenAsRead       PI
     D document                            like(parameter1) const
     C***                movel(p)  'WDOCUMENT'   command
     C                   movel(p)  'WOPENFILE'   command
     C                   movel     *blanks       option
     C                   movel(p)  document      parameter1
     C***                movel(p)  'READONLY'    parameter2
     C                   movel(p)  '*ONLY'       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PopenAsRead       E

     PopenFile         B                   Export
     DopenFile         PI
     D document                            like(parameter1) const
     C                   movel(p)  'WOPENFILE'   command
     C                   movel     *blanks       option
     C                   movel(p)  document      parameter1
     C                   movel(p)  *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PopenFile         E
     P
     PopenLnchr        B                   Export
     DopenLnchr        PI
     D opnHndle                            like(handle) const
     D                                     options(*nopass)
     D server                              like(svrAddr) const
     D                                     options(*nopass)
     D sid                                 like(ccsid) const
     D                                     options(*nopass)
     C                   select
     C                   when      %parms = 3
     C                   movel(p)  sid           ccsid
     C                   movel(p)  server        svrAddr
     C                   movel(p)  opnHndle      handle
     C                   when      %parms = 2
     C                   movel(p)  '*JOB'        ccsid
     C                   movel(p)  server        svrAddr
     C                   movel(p)  opnHndle      handle
     C                   when      %parms = 1
     C                   movel(p)  '*JOB'        ccsid
     C                   eval      svrAddr = getDftSvr
     C                   movel(p)  opnHndle      handle
     C                   when      %parms = 0
     C                   movel(p)  '*JOB'        ccsid
     C                   eval      svrAddr = getDftSvr
     C                   movel(p)  '*ONLY'       handle
     C                   endsl
     C
     C                   call      'LNCOPEN'
     C                   parm                    handle
     C                   parm                    svrAddr
     C                   parm                    ccsid
     C
     C                   return
     PopenLnchr        E

     PopenWord         B                   Export
     DopenWord         PI
     C                   movel(p)  'WORDOPEN'    command
     C                   movel     *blanks       option
     C                   movel     *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PopenWord         E
     P
     PpageBreak        B                   Export
     DpageBreak        PI
     C                   movel(p)  'WPAGEBRK'    command
     C                   movel     *blanks       option
     C                   movel     *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PpageBreak        E

     PprintWord        B                   Export
     DprintWord        PI
     D printer                       10    value
     D copies                         3  0 value
     D
     D parm            s                   like(parameter1)
     D cmd             s             25
     D
     C                   eval      parm = 'Background=False'
     C                   if        printer <> *blanks
     C                   eval      cmd = 'CHKOBJ ' + printer + ' *DEVD'
     C                   callp(e)  runCmd(cmd : 25)
     C                   if        not %error
     C                   eval      parm = %trim(parm) +
     C                                    '; Printer=' + printer
     C                   endif
     C                   endif
     C                   if        copies <> 0
     C                   eval      parm = %trim(parm) +
     C                                    '; Copies=' +
     C                                    %trim(%editc(copies:'Z'))
     C                   endif
     C
     C                   movel(p)  'WPRINT'      command
     C                   movel     *blanks       option
     C                   movel(p)  parm          parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PprintWord        E

     PrplBookMrk       B                   Export
     DrplBookMrk       PI
     D name                                like(parameter1) const
     D value                               like(parameter2) const
     D                                     options(*nopass)
     C                   movel(p)  'WBOOKMARK'   command
     C                   movel     *blanks       option
     C                   movel(p)  name          parameter1
     C                   if        %parms = 2
     C                   movel(p)  value         parameter2
     C                   else
     C                   movel     *blanks       parameter2
     C                   endif
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PrplBookMrk       E

     PsaveAs           B                   Export
     DsaveAs           PI
     D saveName                            like(parameter1) const
     C                   movel(p)  'WSAVEAS'     command
     C                   movel     *blanks       option
     C                   movel(p)  saveName      parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PsaveAs           E

     PsaveFile         B                   Export
     DsaveFIle         PI
     C                   movel(p)  'WSAVE'       command
     C                   movel     *blanks       option
     C                   movel(p)  *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PsaveFile         E
     P
     PsendDoc          B                   Export
     DsendDoc          PI
     D recipients                          like(parameter1) const
     C                   movel(p)  'WSENDTO'     command
     C                   movel     *blanks       option
     C                   movel(p)  recipients    parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PsendDoc          E

     PshowWord         B                   Export
     DshowWord         PI
     C                   movel(p)  'WORDSHOW'    command
     C                   movel     *blanks       option
     C                   movel     *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PshowWord         E
     P
     PendMail          B                   Export
     DendMail          PI
     C                   movel(p)  'MAILEND'     command
     C                   movel     *blanks       option
     C                   movel     *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PendMail          E

     PsendMail         B                   Export
     DsendMail         PI
     C                   movel(p)  'MAILSEND'    command
     C                   movel     *blanks       option
     C                   movel     *blanks       parameter1
     C                   movel     *blanks       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PsendMail         E
     P
     PstartMail        B                   Export
     DstartMail        PI
     D recipients                          like(parameter1) const
     D subject                             like(parameter2) const
     C                   movel(p)  'MAILPREP'    command
     C                   movel     *blanks       option
     C                   movel(p)  recipients    parameter1
     C                   movel(p)  subject       parameter2
     C                   call      'LNCCMD'      cmdParms
     C                   return
     PstartMail        E
