    Hcopyright('(c) Copyright Fiserv, Inc., 2007')
     H* ============================================================= *
    H/title W9402R - Update Files within 9xx Test ENVIRONMENT
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
    H* ============================================================= *
     H*  W9402R1 - Update of CLATCH File in new 9xx Test ENVIRONMENT  *
     H*                                                               *
     H*  DESCRIPTION:  This program read CLATCH file and update it    *
     H*                for the new 9xx Test Environment.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W9401C                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W9401R                                                     *
     H*                                                               *
     H*  CREATED: 12/13/07  PROGRAMMER: C:132  PROJECT: 132WJL        *
     H*                                                               *
     H* ============================================================= *
     H*  REVSIONS:                                                    *
     H*                                                               *
     H* ============================================================= *
     f*
     f* Claims Attachement Dump file
     fclatch    uf   e             disk
     f*
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
     d xxFlrId         s              8    inz
     d zzFlrId         s              8    inz
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d*
     d                 ds
     d  cmuncn                 1    255
     d  xcwrka8                1      8
     d  xcwrka47               9    247
     d  xcwrka5                1      5
     d  xcwrka50               6    250
     d*
      * ====================================================================== *
      *
     c     *entry        plist
     c                   parm                    ##fsuffx                       From Suffix
     c                   parm                    ##tsuffx                       TO Suffix
     c                   parm                    ##DataCtr                      DataCenter if = "Y"
     c                   parm                    ##ToLib                        File Update Library
     c
     c                   time                    dsdttm
     c
      * If Indicator *IN69 is on, no updates will be performed, used for testing only
      * Just move *OFF to *IN69 when doing actual testing of files or when live.
     c                   move      *OFF          *in69
     c
      * Setup Test variables for folder updates
     c
     c                   eval      xxFlrId = '\PDS\' + ##fsuffx
     c                   eval      zzFlrId = '\PDS\' + ##tsuffx
      *
      * Check Claims Atachment Dump for CW 932
      *
     c                   read      clatch                                 99
      *
     c                   If        *in99
     c                   CLEAR                   FromFile
     c                   eval      FromFile = 'CLATCH' + ' ***'
     c                   exsr      ErrLst
     c                   endif
      *
     c                   dow       *in99 = *off
     c                   if        ##fsuffx = *blanks AND xxFlrId = xcWrkA5
     c                   eval      cmuncn =  zzFlrId + xcWrkA50
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxFlrId = xcWrkA8
     c                   eval      cmuncn =  zzFlrId + xcWrkA47
     c                   endif
     c  n69              update    pcm01
     c                   read      clatch                                 99
     c                   enddo
      *
      * ====================================================================== *
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
     c                   eval      ErrorType = 'S'
     c                   Eval      FromLib  = ##ToLib
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
