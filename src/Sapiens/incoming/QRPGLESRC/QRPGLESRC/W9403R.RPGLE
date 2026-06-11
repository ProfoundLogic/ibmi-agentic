     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H* ============================================================= *
    H/title W9403R - Update Files within 9xx Test ENVIRONMENT
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
    H* ============================================================= *
     H*  W9403R  - Update of Files within new 9xx Test ENVIRONMENT    *
     H*                                                               *
     H*  DESCRIPTION:  This program read various files and updates    *
     H*                for the new 9xx Test Environment. Changes the  *
     H*                Menu options from live to test.                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W9401C                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W9401R                                                     *
     H*                                                               *
     H*  CREATED: 11/28/07  PROGRAMMER: C:132  PROJECT: 132WJL        *
     H*                                                               *
     H* ============================================================= *
     H*  REVSIONS:                                                    *
     H*                                                               *
     H*  01/19/10 C:198 Project: 132WJL changes                       *
     H*                 Combined processing of files in one program.  *
     H*                 It was split between W9401C and W9403R.       *
     H*                                                               *
     H* ============================================================= *
     f*
     f*** Beg Chg *** 01/19/10 ****************************************
     f* Subscriber Master file
     f*******fmsmp     uf   e             disk
     f*
     f* WC Group Master file
     f*******fmfnp     uf   e             disk
     f*
     f* Menu Options file
     f*******mmopp     uf   e             disk
     f*
     f* System Company file
     f*******smcop     uf   e             disk
     f*
     f* Routing Options file
     f*******smorp     uf   e             disk
     f*
     f* Group Master file
     f*******wdf2p     uf   e             disk
     f*
     f* Group Master file
     f*******wmfnp     uf   e             disk            RENAME(PFN01:PFN02)
     f*
     f* Bank Master file
     f*******wmbap     uf   e             disk
     f*
     f* Bank Information File
     f*******wmbip     uf   e             disk
     f*
     f* Subscriber Master file
     ffmsmp     uf   e             disk    usropn
     f*
     f* WC Group Master file
     ffmfnp     uf   e             disk    usropn
     f*
     f* System Company file
     fsmcop     uf   e             disk    usropn
     f*
     f* Routing Options file
     fsmorp     uf   e             disk    usropn
     f*
     f* Group Master file
     fwdf2p     uf   e             disk    usropn
     f*
     f* Group Master file
     fwmfnp     uf   e             disk    usropn  RENAME(PFN01:PFN02)
     f*
     f* Bank Master file
     fwmbap     uf   e             disk    usropn
     f*
     f* Bank Information File
     fwmbip     uf   e             disk    usropn
     f*
     f*** End Chg *** 01/19/10 ****************************************
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d ##fsuffx        s              3
     d ##tsuffx        s              3
     d ##DataCtr       s              1
     d ##ToLib         s             10
     d fromLib         s             10
     d FromFile        s             10
     d errorType       s              1
     d TestNme         s              8
     d TstOutq         s              4    inz
     d xxCltId         s              6    inz
     d zzCltId         s              6    inz
     d xxWrkA6         s              6    inz
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d*
     d*** Beg Add *** 01/19/10 ********************************************
     dqca              s             56    dim(2) ctdata perrcd(1)
     d                 ds
     dqqcmd                    1     56a
     dqqlib                   14     22a
     dlength           s             15  5 inz
     d##err            s              1
     d##lib            s             10
     d##file           s             10
     d##suffx          s              3
     dxxtitl           s                   like(ortitl)
     d*** End Add *** 01/19/10 ********************************************
    c*====================================================================
    c* MAINLINE - Main processing for program
    c*====================================================================
      *
     c     *entry        plist
     c                   parm                    ##fsuffx                       From Suffix
     c                   parm                    ##tsuffx                       TO Suffix
     c                   parm                    ##DataCtr                      DataCenter if = "Y"
     c                   parm                    ##ToLib                        File Update Library
     c
     c                   time                    dsdttm
     c
     c                   exsr      pgmstr
      * If Indicator *IN69 is on, no updates will be performed, used for testing only
      * Just move *OFF to *IN69 when doing actual testing of files or when live.
     c                   move      *OFF          *in69
     c
     c                   eval      TestNme = 'TEST' + ##tsuffx + '-'
     c
     c                   eval      xxCltId = 'CLT' + ##fsuffx
     c                   eval      zzCltId = 'CLT' + ##tsuffx
     c
     c                   if         ##fsuffx <> *blanks
     c                   eval      TstOutq = 'P' + ##fsuffx
     c                   endif
      *
      * ====================================================================== *
      *
      * Check Subscriber file for new 9xx environment.
      *
     c                   read      fmsmp                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'FMSMP'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   eval      smname =  TestNme + smname
     c  n69              update    psm01
     c                   read      fmsmp                                  99
     c                   enddo
      *
      * Check WC Group file for new 9xx environment.
      *
     c                   read      fmfnp                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'FMFNP'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   eval      fnname =  TestNme + fnname
     c                   eval      fnshnm =  TestNme + fnshnm
     c  n69              update    pfn01
     c                   read      fmfnp                                  99
     c                   enddo
      *
      * Check WC Group Auxiliary file for new 9xx environment.
      *
     c                   read      wdf2p                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'WDF2P'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   movel     f2fold        xxWrkA6
     c                   If        xxWrkA6 = xxCltID
     c                   movel     zzCltID       f2fold
     c  n69              update    pf201
     c                   EndIf
     c                   read      wdf2p                                  99
     c                   enddo
      *
      * Read the menu options file and change applications from Live
      * To Test. If monthly Option change to Month Test.
      *
     c*** Beg Del *** 01/19/10 *************************************************
     c********           read      mmopp                                  99
     c********           dow       *in99 = *off
     c********
     c********           select
     c********           when      opapp = 'LIV'
     c********           movel     'TST'         opapp
     c**n69***           update    pop01
     c*
     c********           when      opapp = 'MTH'
     c********           movel     'TSM'         opapp
     c**n69***           update    pop01
     c*
     c********           when      opapp = 'QTE'
     c********           movel     'TSQ'         opapp
     c**n69***           update    pop01
     c*
     c********           other
     c********           eval      ErrorType = 'M'
     c********           CLEAR                   FromFile
     c********           eval      FromFile = 'MMOPP' + '-' + OPAPP
     c********           movel(P)  opexcl        FromLib
     c********           exsr      ErrLst
     c**n69***           delete    pop01
     c*
     c********           EndSl                                                  OPAPP = "LIV"
     c********
     c********           read      mmopp                                  99
     c********           enddo
      *
     c*** End Del *** 01/19/10 *************************************************
      * Check System Company file for new 9xx environment.
      *
     c                   read      smcop                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'SMOPP'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   eval      coname =  TestNme + coname
     c  n69              update    pco01
     c                   read      smcop                                  99
     c                   enddo
      *
      * Check Routing file for new 9xx environment.
      *
     c                   read      smorp                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'SMORP'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   move      *OFF          *in70
     c*** Beg Add *** 01/19/10 *************************************************
     C                   if        ortitl <> *blanks
     C                   move(p)   ortitl        xxdata           60
     C     ' '           check     xxdata        n                 3 0
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      xxtitl
     C                   endif
     C                   endif
     c*** End Add *** 01/19/10 *************************************************
     c*** Beg Chg *** 01/19/10 *************************************************
     c**********         eval      ortitl =  TestNme + ortitl
     c                   eval      ortitl =  TestNme + xxtitl
     c*** End Chg *** 01/19/10 *************************************************
     c*** Beg Add *** 01/19/10 *************************************************
     *
     C                   movel(p)  ortitl        ##data           80
     C                   move      *blanks       ortitl
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng            3 0
     C                   movel(p)  ##data        ortitl
     c*** End Add *** 01/19/10 *************************************************
     c                   if        tstOutq <> *blanks
     c     TstOutq       scan      oroutq                                 70
     c   70              movel     TstOutq       oroutq
     c                   endif
     c  n69              update    por01
     c                   read      smorp                                  99
     c                   enddo
      *
      * Check bank Master file for new 9xx environment.
      *
     c                   read      wmbap                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'WMBAP'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   eval      baname =  TestNme + baname
     c  n69              update    pba01
     c                   read      wmbap                                  99
     c                   enddo
      *
      * Check Bank Information file for new 9xx environment and make it's
      * records invalid.
     c                   read      wmbip                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'WMBIP'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   eval      biai01 =  TestNme + biai01
     c                   eval      bibnk# =  *hival
     c                   eval      bitrns =  *hival
     c  n69              update    pbi01
     c                   read      wmbip                                  99
     c                   enddo
      *
      * Fix Group Master file for new 9xx environment.
      *
     c                   read      wmfnp                                  99
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        *in99 = *on
     c                   call      'W9401R'
     c                   parm      'S'           ##Err
     c                   parm      ##tolib       ##lib
     c                   parm      'WMFNP'       ##file
     c                   parm      ##tsuffx      ##suffx
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c                   dow       *in99 = *off
     c                   eval      fnname =  TestNme + fnname
     c                   eval      fnshnm =  TestNme + fnshnm
     c  n69              update    pfn02
     c                   read      wmfnp                                  99
     c                   enddo
      *
      * ====================================================================== *
     c                   close     fmsmp
     c                   close     fmfnp
     c********           close     mmopp
     c                   close     smcop
     c                   close     smorp
     c                   close     wdf2p
     c                   close     wmfnp
     c                   close     wmbap
     c                   close     wmbip
     c                   movel(p)  qca(2)        qqcmd
     c                   movel(p)  ##tolib       qqlib
     c                   call      'QCMDEXC'
     c                   parm                    qqcmd
     c                   parm      56            length
      *
      *   Terminate Program
     c                   eval      *inlr = *on
     c
      * ====================================================================== *
     c* ERRLST - SUBROUTINE TO LIST ERRORS ON FILE CHANGES.
     c*======================================================================= *
     c*
     c     ErrLst        BEGSR
     c*
     c                   CALL      'W9401R'
     c                   parm                    ErrorType
     c                   parm                    FromLib
     c                   parm                    FromFile
     c                   parm                    ##TSuffx
     c
     c                   ENDSR                                                  STRING
     c
      * ====================================================================== *
     c*** Beg Add *** 01/19/10 *************************************************
     c* pgmstr - subroutine at the start of the program
     c*======================================================================= *
     c*
     c     pgmstr        begsr
     c*
     c                   exsr      addlib
     c                   open      fmsmp
     c                   open      fmfnp
     c*******            open      mmopp
     c                   open      smcop
     c                   open      smorp
     c                   open      wdf2p
     c                   open      wmfnp
     c                   open      wmbap
     c                   open      wmbip
     c*
     c                   endsr
     c
      * ====================================================================== *
     c* addlib - subroutine to add the library in the list
     c*======================================================================= *
     c*
     c     addlib        begsr
     c*
     c                   movel(p)  qca(1)        qqcmd
     c                   movel(p)  ##tolib       qqlib
     c                   call      'QCMDEXC'
     c                   parm                    qqcmd
     c                   parm      56            length
     c*
     c                   endsr
     c
     c*** End Add *** 01/19/10 *************************************************
** CTDATA QCA
ADDLIBLE LIB(         )
RMVLIBLE LIB(         )
