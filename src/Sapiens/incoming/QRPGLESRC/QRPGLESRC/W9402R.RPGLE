     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H* ============================================================= *
    H/title W9402R - Update Files within 9xx Test ENVIRONMENT
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
    H* ============================================================= *
     H*  W9402R  - Update of System Files in new 9xx Test ENVIRONMENT *
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
     H*  CREATED: 12/06/07  PROGRAMMER: C:132  PROJECT: 132WJL        *
     H*                                                               *
     H* ============================================================= *
     H*  REVISIONS:                                                   *
    H*                                                               *
    H*  11/02/10 C:183 Project: SY195                                *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT  *
    H*                 to get the root folder name.                  *
     H*                                                               *
     H*  07/21/10 C:198 Project: 132WJL changes                       *
     H*                 Added one more file WTENP to change enfldr    *
     H*                 from CLT132 to CLT932.                        *
     H*                                                               *
     H*  01/19/10 C:198 Project: 132WJL changes                       *
     H*                 Added another field in file saoip: oitdir     *
     H*                 to have its path changed from 132 to 932.     *
     H*                                                               *
     H* ============================================================= *
     f*
     f* Workstation ID/Control file
     fimwsp     uf   e             disk
     f*
     f* Object Index file
     fsmi1p     uf   e             disk
     f*
     f* Import Log file
     fsaoip     uf   e             disk
     f*
     f*** Beg Add *** 07/21/10 ***************************************
     f* Endorsement Form Tabl File
     fwtenp     uf   e             disk
     f*
     f*** End Add *** 07/21/10 ***************************************
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
     d TstOutq         s              4    inz
     d*
     d*** Beg Chg *** 05/18/10 **********************************
     d*xxCltId***      s              7    inz
     d*zzCltId***      s              7    inz
     d xxCltId         s              8    inz
     d zzCltId         s              8    inz
     d*** End Chg *** 05/18/10 **********************************
     d xxFlrId         s              8    inz
     d zzFlrId         s              8    inz
     d xxQdlsUc        s              7    inz
     d xxQdlsLc        s              7    inz
     d zzQdlsTo        s              7    inz
     d xxPDSOBJ        s             10    inz
     d zzPDSOBJ        s             10    inz
     d xxPDSFIL        s             10    inz
     d zzPDSFIL        s             10    inz
      *** Beg add ***** 11/02/10 ***********************************************
     D root            s             10    inz
     D up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              c                   'abcdefghijklmnopqrstuvwxyz'
      *** End add ***** 11/02/10 ***********************************************
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d*
     d                 ds
     d  xoxdir                 1     80
     d  xowrka8                1      8
     d  xowrka72               9     80
     d  xowrka5                1      5
     d  xowrka75               6     80
     d*
     d                 ds
     d  xxfldr                 1     32
     d  xxclt                  1      3
     d  xxcltid1               4      4
     d  xxcltid2               5      5
     d  xxcltid3               6      6
     d  xxrfldr                7     32
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
      *** Beg add ***** 11/02/10 ***********************************************
     C                   call      'RTVDOCROOT'
     C                   parm                    root
      *** End add ***** 11/02/10 ***********************************************
     c
      * If Indicator *IN69 is on, no updates will be performed, used for testing only
      * Just move *OFF to *IN69 when doing actual testing of files or when live.
     c                   move      *OFF          *in69
     c
      * Setup Test variables for folder updates
     c
     c*** Beg Chg *** 01/19/10 ******************************************
     c*****              eval      xxCltId = 'PDS/' + ##fsuffx
     c*****              eval      zzCltId = 'PDS/' + ##tsuffx
     c
     c*****              eval      xxFlrId = '\PDS\' + ##fsuffx
     c*****              eval      zzFlrId = '\PDS\' + ##tsuffx
     c
     c                   eval      xxCltId = '/PDS/' + ##fsuffx
     c                   eval      zzCltId = '/PDS/' + ##tsuffx
     c
     c                   eval      xxFlrId = '/PDS/' + ##fsuffx
     c                   eval      zzFlrId = '/PDS/' + ##tsuffx
     c*** End Chg *** 01/19/10 ******************************************
     c
      *** Beg chg ***** 11/02/10 ***********************************************
     c*******            eval      xxQdlsUc = 'QDLS' + ##fsuffx
     c*******            eval      xxQdlsLc = 'qdls' + ##fsuffx
     c*******            eval      zzQdlsTo = 'QDLS' + ##tsuffx
     c                   eval      xxQdlsUc = %trim(%xlate(lo:up:ROOT))
     c                              + ##fsuffx
     c                   eval      xxQdlsLc = %trim(%xlate(up:lo:ROOT))
     c                              + ##fsuffx
     c                   eval      zzQdlsTo = %trim(%xlate(lo:up:ROOT))
     c                              + ##fsuffx
      *** End chg ***** 11/02/10 ***********************************************
     c
     c                   eval      xxPDSOBJ = 'PDSOBJ' + ##fsuffx
     c                   eval      zzPDSOBJ = 'PDSOBJ' + ##tsuffx
     c
     c                   eval      xxPDSFIL = 'PDSFIL' + ##fsuffx
     c                   eval      zzPDSFIL = 'PDSFIL' + ##tsuffx
     c
     c                   if         ##fsuffx <> *blanks
     c                   eval      TstOutq = 'P' + ##fsuffx
     c                   endif
      *
      * ====================================================================== *
      *
      * Check Index Queue Description File for both blank From Suffix and existing suffix
      *
     c                   read      imwsp                                  99
      *
     c                   If        *in99
     c                   CLEAR                   FromFile
     c                   eval      FromFile = 'IMWSP' + ' ***'
     c                   exsr      ErrLst
     c                   endif
      *
     c                   dow       *in99 = *off
     c                   IF        wssrlb = xxPDSOBJ
     c                   eval      wssrlb = zzPDSOBJ
     c                   endIf
     c                   IF        wssslb = xxPDSOBJ
     c                   eval      wssslb = zzPDSOBJ
     c                   endIf
     c                   IF        wsvrql = xxPDSOBJ
     c                   eval      wsvrql = zzPDSOBJ
     c                   endIf
     c                   IF        wsvqlb = xxPDSOBJ
     c                   eval      wsvqlb = zzPDSOBJ
     c                   endIf
     c  n69              update    pws01
     c                   read      imwsp                                  99
     c                   enddo
      *
      * Check Import Log file for new 9xx environment, for all three folder fields
      *
     c                   read      saoip                                  99
      *
     c                   If        *in99
     c                   CLEAR                   FromFile
     c                   eval      FromFile = 'SAOIP' + ' ***'
     c                   exsr      ErrLst
     c                   endif
      *
     c                   dow       *in99 = *off
     c                   movel(p)  oifdir        xoxdir
     c                   if        ##fsuffx = *blanks AND xxFlrId = xoWrkA5
     c                   eval      oifdir =  zzFlrId + xoWrkA75
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxCltId = xoWrkA8
     c                   eval      oifdir =  zzFlrId + xoWrkA72
     c                   endif
     c                   movel(p)  oiadir        xoxdir
     c                   if        ##fsuffx = *blanks AND xxFlrId = xoWrkA5
     c                   eval      oiadir =  zzFlrId + xoWrkA75
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxCltId = xoWrkA8
     c                   eval      oiadir =  zzFlrId + xoWrkA72
     c                   endif
     c                   movel(p)  oiedir        xoxdir
     c                   if        ##fsuffx = *blanks AND xxFlrId = xoWrkA5
     c                   eval      oiedir =  zzFlrId + xoWrkA75
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxCltId = xoWrkA8
     c                   eval      oiedir =  zzFlrId + xoWrkA72
     c                   endif
     C*** Beg Add *** 01/19/10 ****************************************
     c                   movel(p)  oitdir        xoxdir
     c                   if        ##fsuffx = *blanks AND xxFlrId = xoWrkA5
     c                   eval      oitdir =  zzFlrId + xoWrkA75
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxCltId = xoWrkA8
     c                   eval      oitdir =  zzFlrId + xoWrkA72
     c                   endif
     C*** End Add *** 01/19/10 ****************************************
     c  n69              update    poi01
     c                   read      saoip                                  99
     c                   enddo
      *
      * Check Object Index file for new 9xx environment, for all three folder fields
      *
     c                   read      smi1p                                  99
      *
     c                   If        *in99
     c                   CLEAR                   FromFile
     c                   eval      FromFile = 'SMI1P' + ' ***'
     c                   exsr      ErrLst
     c                   endif
      *
     c                   dow       *in99 = *off
     c                   movel(p)  i1idir        xoxdir
     c                   if        ##fsuffx = *blanks AND xxFlrId = xoWrkA5
     c                   eval      i1idir =  zzFlrId + xoWrkA75
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxCltId = xoWrkA8
     c                   eval      i1idir =  zzFlrId + xoWrkA72
     c                   endif
     c                   movel(p)  i1adir        xoxdir
     c                   if        ##fsuffx = *blanks AND xxFlrId = xoWrkA5
     c                   eval      i1adir =  zzFlrId + xoWrkA75
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxCltId = xoWrkA8
     c                   eval      i1adir =  zzFlrId + xoWrkA72
     c                   endif
     c                   movel(p)  i1edir        xoxdir
     c                   if        ##fsuffx = *blanks AND xxFlrId = xoWrkA5
     c                   eval      i1edir =  zzFlrId + xoWrkA75
     c                   endif
     c                   if        ##fsuffx <> *blanks AND xxCltId = xoWrkA8
     c                   eval      i1edir =  zzFlrId + xoWrkA72
     c                   endif
     c  n69              update    pi101
     c                   read      smi1p                                  99
     c                   enddo
      *
     c*** Beg Add *** 07/21/10 *************************************************
      * Check file for enfldr = CLTxxx to be  changed to CLT9xx
      *
     c                   read      wtenp                                  99
      *
     c                   If        *in99
     c                   CLEAR                   FromFile
     c                   eval      FromFile = 'WTENP' + ' ***'
     c                   exsr      ErrLst
     c                   endif
      *
     c                   dow       *in99 = *off
     c                   movel(p)  enfldr        xxfldr
     c                   if        xxclt  = 'CLT'
     c                   eval      xxcltid1 = '9'
     c                   endif
     C                   movel(p)  xxfldr        enfldr
     c  n69              update    pen01
     c                   read      wtenp                                  99
     c                   enddo
      *
     c*** End Add *** 07/21/10 *************************************************
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
