    Hcopyright('(c) Copyright Fiserv, Inc., 2007')
     H* ============================================================= *
    H/title W9402R2- Update Files within 9xx Test ENVIRONMENT
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
    H* ============================================================= *
     H*  W9402R2 - Update of LVLCHKNO File in new 9xx Test ENVIRONMENT*
     H*                                                               *
     H*  DESCRIPTION:  This program read LVLCHKNO file and update it  *
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
     flvlchkno  uf   e             disk
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
     d TestNme         s              8
     d TstOutq         s              4    inz
     d xxPDSFIL        s             10    inz
     d zzPDSFIL        s             10    inz
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
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
     c
     c                   eval      xxPDSFIL = 'PDSFIL' + ##fsuffx
     c                   eval      zzPDSFIL = 'PDSFIL' + ##tsuffx
     c
      *
      * Check Objects for no level check
      *
     c                   read      lvlchkno                               99
      *
     c                   If        *in99
     c                   CLEAR                   FromFile
     c                   eval      FromFile = 'LVLCHKNO'+ '**'
     c                   exsr      ErrLst
     c                   endif
      *
     c                   dow       *in99 = *off
     c                   IF        odlbnm = xxPDSFIL
     c                   eval      odlbnm = zzPDSFIL
     c                   endIf
     c                   IF        odlbl  = xxPDSFIL
     c                   eval      odlbl  = zzPDSFIL
     c                   endIf
     c  n69              update    QLIDOBJD
     c                   read      lvlchkno                               99
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
