     h/TITLE W7202R - prompt for payroll by class
     H* ============================================================= *
     H*  (C) Copyright Progressive Data Solutions, Inc., 2002
     H*                                                               *
     H*  10/29/02  C:132  Project: WC1093                             *
     H*                   Commercial Payroll by Class Report          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H* ============================================================= *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw7202f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     infds(fids01)
     fwmfnl     if   e           k disk
     fsmcol     if   e           k disk
     fwmupl     if   e           k disk
     f****wdf1l     if   e           k disk
     d*
     dSA030R           PR                  extpgm('SA030R')
     d ##sufx                         3
     d*
     d                 ds
     d  dsdttm                 1     12  0
     d  dstime                 1      6  0
     d  dsdate                 7     12  0
     d                 ds
     d  xxrecf                 1     10
     d  xxscr#                 2      5
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)
      *
     d filnam          s             10
     d libnam          s             10
     d mbrnam          s             10
     d qqcall          s              1
     d qqco#           s              3
     d qqpfdt          s              8
     d qqptdt          s              8
     d qqtype          s              1
     d qqrtfl          s              1
     d qqfnd           s              3
     d qqfyr           s              3
     d qqoutq          s             10
     d qqprds          s             40
     d srdfld          s             80
     d srdlen          s              3  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d xxpfdt          s              7  0 inz
     d xxptdt          s              7  0 inz
     d xxfnd           s              3  0
     d xxfyr           s              3  0
     d*
     D ##sufx          s              3
     D xxrptl          s             23
     d*
     d TITLE           C                   CONST('   Commercial Payroll b-
     d                                     y Class Report')
     c*
     c*   KEY LIST FOR THE COMPANY FILE
     c*
     c     cokey         klist
     c                   kfld                    upco#
     c*
     c*  USER PARAMETER FILE
     c*
     c     upkey         klist
     c                   kfld                    q1user
     c*
     c*  GROUP MASTER FILE
     c*
     c     fnkey         klist
     c                   kfld                    upco#
     c                   kfld                    xxfnd
     c*
     c*  POLICY YEAR KEY LIST
     c*
     c     f1key         klist
     c                   kfld                    upco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c*
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c*  GET THE USER PARAMETER FILE
     c*
     c     upkey         chain     pup01                              60
     c                   if        (*in60)
     c                   move      *zero         upco#
     c                   endif
     c*
     c*  GET THE COMPANY RECORD
     c*
     c     strt01        tag
     c     cokey         chain     pco01                              60
     c                   movel(p)  coname        srdfld
     c                   call      'SRCENTER'
     c                   parm                    srdfld
     c                   parm      40            srdlen
     c                   movel     srdfld        scconm
     c                   move      upfnd         scfnd
     c*
     c*  GET THE REQUESTED RECORD, LOAD INFORMATION
     c*
     c                   move      scfnd         xxfnd
     c     fnkey         chain     pfn01                              60
     c                   movel     fnname        scname
     c*
     c                   time                    dsdttm
     c                   move      q1user        scuser
     c                   movel     title         qqprds
     c                   move      qqprds        scprds
     c*
     c*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     c*
     c                   move      *blank        q2recf
     c                   movel     'SCR01'       q2recf
     c*
     C                   callp     SA030R(##sufx)
     C                   if        ##sufx = *blanks
     C                   eval      xxrptl = 'PDSRPT' + ', if selected.'
     C                   else
     C                   eval      xxrptl = 'PDSRPT' + ##sufx + ', if selected.'
     C                   endif
     C                   movel     xxrptl        scrptl
     c*
     c*
     c*   DISPLAY ALL FORMATS
     c*
     c     strt          tag
     C*
     c                   if        (q2recf = 'SCR01')
     c                   exfmt     scr01
     c                   endif
     c*
     c*   PROCESS ALL COMMAND KEYS
     c*
     c                   if        (*in11)
     c*
     c*   COMMAND 03 = END OF JOB
     c*
     c                   if        (*inkc)
     c                   move      '1'           *inlr
     c                   goto      term
     c                   endif
     c*
     c*   PROCESS HELP REQUESTS
     c*
     c                   if        (*in10)
     c*
     c                   move      q2recf        xxrecf
     c                   movel     'QHLP    '    filnam
     c                   movel     '*LIBL   '    libnam
     c                   move      q1pgm         mbrnam
     c                   move      xxscr#        mbrnam
     c                   call      'M0006C'
     c                   parm                    filnam
     c                   parm                    libnam
     c                   parm                    mbrnam
     c                   move      xxrecf        q2recf
     c                   goto      strt
     c                   endif
     c*
     c*   PROCESS CHANGE OF USER PARAMETERS
     c*
     c                   if        (*inkm)
     c*
     c                   call      'W0006R'
     c*
     c*  GET THE USER PARAMETER FILE
     c*
     c     upkey         chain     pup01                              60
     c                   if        (*in60)
     c                   move      *zero         upco#
     c                   endif
     c                   goto      strt01
     c                   endif
     C*
     c                   endif
     c*
     c*   Process REQUEST Screen
     c*
     c     SCR01T        tag
     c*
     c*  VALIDATE the From and To Dates as valid ascending dates
     c*
     c                   if        (scpfdt = *zeros) OR (scptdt = *zeros)
     c                   move      '1'           *in21
     c                   move      *blank        q2recf
     c                   movel     'SCR01'       q2recf
     c                   goto      strt
     c                   endif
     c*
     c                   if        (scpfdt <> *zero)
     C*
     c                   eval      srdti = scpfdt
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scpfdt
     C*
     c                   if        (srerr = *on)
     c                   move      *on           *in22
     c                   goto      strt
     c                   endif
     c                   endif
     c*
     c*  VALIDATE THE TO DATE
     c*
     c                   if        (scptdt <> *zero)
     c*
     c                   eval      srdti = scptdt
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scptdt
     c*
     c                   if        (srerr = *on)
     c                   move      *on           *in23
     c                   move      *on           *in30
     c                   goto      strt
     c                   endif
     c                   endif
     c*
     c*  VALIDATE FROM DATE NOT GREATER THAN TO DATE
     c*
     c                   eval      srdti = scpfdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxpfdt
     c*
     c                   eval      srdti = scptdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxptdt
     c*
     c                   if        (xxpfdt > xxptdt)
     c                   move      *on           *in24
     c                   move      *on           *in30
     c                   goto      strt
     c                   endif
     c*
     c*  VALIDATE the Output Type Flag P/F/B
     c*
     c                   if        (sctype <> 'P') and (sctype <> 'F') and
     c                             (sctype <> 'B')
     c                   move      '1'           *in25
     c                   move      *on           *in30
     c                   move      *blank        q2recf
     c                   movel     'SCR01'       q2recf
     c                   goto      strt
     c                   endif
     c*
     c*  VALIDATE the Output Type Flag P/F/B
     c*
     c                   if        (scrtfl <> 'Y') and (scrtfl <> ' ')
     c                   move      '1'           *in26
     c                   move      *on           *in30
     c                   move      *blank        q2recf
     c                   movel     'SCR01'       q2recf
     c                   goto      strt
     c                   endif
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     c                   move      upco#         qqco#
     c                   move      scfnd         qqfnd
     c                   movel(p)  scpfdt        qqpfdt
     c                   movel(p)  scptdt        qqptdt
     c                   move      sctype        qqtype
     c                   move      scrtfl        qqrtfl
     c                   movel(p)  upoutq        qqoutq
     c                   call      'W7202C'
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqpfdt
     c                   parm                    qqptdt
     c                   parm                    qqtype
     c                   parm                    qqrtfl
     c                   parm                    qqoutq
      *
      ** =========================================================== **
      *  TERM - End the Program
      ** =========================================================== **
      *
     c     term          tag
     c                   move      '1'           *inlr
      ** =========================================================== **
