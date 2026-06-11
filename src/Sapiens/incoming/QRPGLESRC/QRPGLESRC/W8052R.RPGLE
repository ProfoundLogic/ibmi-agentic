      ***************************************************************
      * (C) Copyright Fiserv, Inc., 1997 - 2005                    **
      *                                                            **
      * Program:     W8052R                                        **
      * Description: EDI A49 Annual Report Prompt                  **
      *                                                            **
      * Created By:  C:167  Proj:WC1405   Date:  12/29/05          **
      ***************************************************************
      *  REVISIONS:                                                **
      *                                                            **
      ***************************************************************
      *  INDICATOR USAGE:                                          **
      *                                                            **
      ***************************************************************

      *  DISPLAY
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8052f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *  USER PARAMETERS MASTER FILE
     Fwmupl     if   e           k disk
      *  STATE CODE TABLE FILE
     Fststl1    if   e           k disk
      *  COMPANY MASTER FILE
     Fsmcol     if   e           k disk
      *  SYSTEM TABLE FILE
     Fwtgtl     if   e           k disk
      *  EDI LOG FILE
     Fwmlgl     if   e           k disk
      *
     D  kycode         s              4
     D  kymtcd         s             10
     D  qqcall         s              1
     D  qqco#          s              3
     D  qqduedt        s              8
     D  qqid           s              4
     D  qqkey          s             10
     D  qqmntcde       s              2
     D  qqprds         s             40
     D  qqrtc          s              1
     D  qqjst          s              2
     D  qqyear         s              4
     D  srdfld         s             80
     D  srdlen         s              3  0
     D  srdti          s              8  0
     D  srdto          s             10
     D  srerr          s              1
     D  srffmt         s              8
     D  srtfmt         s              8
     D  wstkey         s             10    inz(*blank)
      *
     D scduedate       ds
     D  scdtmm                 1      2  0
     D  scdtdd                 3      4  0
     D  scdtyr                 5      8  0
      *
     D lgduedate       ds
     D  lgdtmm                 1      2  0
     D  lgdtdd                 3      4  0
     D  lgdtyr                 5      8  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  KEY LIST FOR THE USER PARAMETER FILE
     C     upkey         klist
     C                   kfld                    q1user
      *
      *  KEY LIST FOR THE COMPANY FILE
     C     cokey         klist
     C                   kfld                    upco#
      *
      *  KEY LIST FOR THE SYSTEM TABLE FILE
     C     sykey         klist
     C                   kfld                    upco#
     C                   kfld                    kycode
     C                   kfld                    kymtcd
      *
     C     *entry        plist
     C                   parm                    qqprds
     C                   parm                    qqcall
      *
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *
     C                   eval      scuser = q1user
     C                   eval      scmtcd = 'AN'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     upkey         chain     pup01                              60
     C                   if        *in60 = *on
     C                   eval      upco# = *zero
     C                   endif
      *
      *  GET THE COMPANY RECORD
      *
     C     cokey         chain     pco01                              60
     C                   movel     coname        srdfld
     C                   call      'SRCENTER'
     C                   parm                    srdfld           80
     C                   parm      40            srdlen            3 0
     C                   movel     srdfld        scconm
     C                   eval      scprds = qqprds
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   eval      q2recf = *blank
     C                   movel     'SCR01'       q2recf
      *
      *   DISPLAY ALL FORMATS
      *
     C     strt          tag
      *
     C                   exfmt     scr01
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C                   if        *in11 = *on
      *
      *   COMMAND 03 = END OF JOB
      *
     C                   if        *inkc = *on
     C                   eval      *inlr = *on
     C                   goto      term
     C                   endif
      *
      *   COMMAND 12 = PRIOR SCREEN
      *
     C                   if        *inkl = *on
     C                   eval      *inlr = *on
     C                   goto      term
     C                   endif
      *
      *   COMMAND 10 = SEARCH
      *
     C                   if        *inkj = *on
     C                   select
     *
     * Maintenance type code search
     *
     C     hfld          wheneq    'SCMTCD'
      *
     C                   move      *blanks       qqrtc
     C                   move      *blanks       qqkey
      *
     C                   call      'W2493R'
     C                   parm                    qqprds
     C                   parm                    qqcall
     C                   parm      'SMTC'        qqid
     C                   parm                    qqkey
     C                   parm                    qqrtc
      *
     C     qqrtc         ifeq      'A'
     C                   goto      term
     C                   endif
      *
     C     qqrtc         ifeq      'B'
     C     qqrtc         oreq      ' '
     C                   eval      *in51 = *on
     C                   goto      strt
     C                   endif
      *
     C     qqrtc         ifeq      '*'
     C                   movel     qqkey         scmtcd
     C                   eval      *in51 = *on
     C                   endif
     C                   endsl
     *
     C                   endif                                                  *inkj = *on
     C                   goto      strt
      *
     C                   endif                                                  *in11 = *on
      *
      *   GOTO THE APPROPRIATE SCREEN
     C     q2recf        cabeq     'SCR01'       scr01t
      *
      *   PROCESS REQUEST SCREEN
     C     scr01t        tag
      *
      *  VALIDATE STATE
     C                   if        scjst = *blank
     C                   eval      *in20 = *on
     C                   goto      strt
     C                   endif
     C                   movel     scjst         wstkey
     C     wstkey        chain     pst01                              60
     C                   if        *in60 = *on
     C                   eval      *in21 = *on
     C                   goto      strt
     C                   endif
      *
      *  VALIDATE YEAR
     C                   if        scyear = *zero
     C                   eval      *in22 = *on
     C                   goto      strt
     C                   endif
      *
      *  VALIDATE RUN DATE
     C                   if        scdudt = *zero
     C                   eval      *in23 = *on
     C                   goto      strt
     C                   endif
     C                   eval      srdti = scdudt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scdudt
     C                   movel     srdto         scduedate
     C                   if        srerr = '1'
     C                   eval      *in24 = *on
     C                   goto      strt
     C                   endif
      *
      *  VALIDATE MAINTENANCE TYPE CODE
     C                   eval      kycode = 'SMTC'
     C                   clear                   kymtcd
     C                   movel     scmtcd        kymtcd
     C     sykey         chain     pgt01                              60
     C                   if        *in60 = *on
     C                   eval      *in25 = *on
     C                   goto      strt
     C                   endif
      *
      *  NO DUPLICATES ALLOWED
     C                   eval      *in28 = *off
     C     *loval        setll     plg01
     C                   dow       *in91 = *off
     C                   read      plg01                                  91
     C                   if        *in91 = *off
     C                   eval      srdti = lgdue
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         lgduedate
     C                   if        srerr = '0'
     C                   if        scdtyr = lgdtyr
     C                             and scmtcd = lgmtc
     C                             and scjst = lgjst
     C                   eval      *in28 = *on
     C                   goto      strt
     C                   endif
     C                   endif
     C                   endif
     C                   enddo
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
     C                   move      upco#         qqco#
     C                   eval      qqjst = scjst
     C                   move      scyear        qqyear
     C                   move      scdudt        qqduedt
     C                   eval      qqmntcde = scmtcd
     C                   call      'W8052C'
     C                   parm                    qqco#
     C                   parm                    qqjst
     C                   parm                    qqyear
     C                   parm                    qqduedt
     C                   parm                    qqmntcde
      *
      *  END THE PROGRAM
     C     term          tag
     C                   eval      *inlr = *on
