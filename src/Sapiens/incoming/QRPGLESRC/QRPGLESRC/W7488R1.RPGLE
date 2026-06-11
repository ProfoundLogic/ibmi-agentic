     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title Checking to find the right folder name
     H*====================================================================
     H*  Created: 11/16/09   C:198  Project: WC1819 Phase 5.
     H*====================================================================
    H*  Revisions:
     H*
/* 01H* 02/11/10  C:198 Project: WC1819 Phase 5 Priss 41
/*   H*                 corrected the retrieval of folder name.
/*   H*
     H*
     H*====================================================================
    F*====================================================================
     F* Physical Files
    F*====================================================================
     Fssypf     if   e             disk
     D*====================================================================
     D* Data Structures
     D*====================================================================
     DtheSuffix        ds             3
     D syfl08                         1
     D syfl09                         1
     D syfl10                         1
    D*====================================================================
    D* Stand Alone Fields
    D*====================================================================
     Dfolder           s             80
     Dfolder_save      s             80
     Dsuffix           s              3
     D*** Beg Add *** 02/11/10 ********************************************
     D IfsObj          s            256a   inz
     D IfsObjFnd       s               n   inz
     D xxcmd           s            200    inz(*blanks)
     D xxcmdln         s             15  5 inz(*zeros)
     Dprefix           s             80
     D*** End Add *** 02/11/10 ********************************************
    D*====================================================================
     C*
     C     *entry        plist
     C                   parm                    folder
     C*
     C                   movel     folder        xxpds             4
     C*
/*** C*** Beg Del ***  02/11/10  ************************************************
     C*********          if        (xxpds <> 'QDLS/') and (folder <> *blanks)
     C*
     C*********          eval      folder_save = %trim(folder)
     C*
     C*********          read      ssypf                                  99
     C*
     C*********          if        *in99 = *off
     C*********          movel     theSuffix     suffix
     C*
     C*********          if        %subst(syfl11:2:1) = 'Y'
     C*
     C*********          if        suffix <> *blanks
     C*********          eval      folder =  suffix
     C*********                             + '/'
     C*********                             +folder_save
     C*********          else
     C*
     C*********          eval      folder = folder_save
     C*
     C*********          endif                                                  suffix <> ' '
     C*
     C*********          endif                                                  syfl11:2:1 = 'Y'
     C*
     C*********          endif                                                  *in99 = *off
     C*
     C*********          endif                                                  xxpds <> 'QDLS/'
/*** C*** End Del ***  02/11/10  ************************************************
/*** C*** Beg Add ***  02/11/10  ************************************************
     C*
     C                   Call      'SA030R'
     C                   parm                    suffix
     C                   parm                    prefix
     C*
     C                   If        prefix <> *blanks
     C                   Eval      IfsObj = %trim(prefix) + '/' + %trim(folder)
     C                   else
     C                   Eval      IfsObj = %trim(folder)
     C                   endif
     C*
     C*** check for folder
     C                   Call      'CHKIFSOBJ'
     C                   parm                    IfsObj
     C                   parm                    IfsObjFnd
     C*
     C                   If        not IfsObjFnd
     C*
     C                   eval      xxcmd= 'MKDIR DIR(''' +%trim(IfsObj)+ ''')'
     C                              + ' DTAAUT(*INDIR)'
     C                              + ' OBJAUT(*INDIR) CRTOBJAUD(*SYSVAL)'
     C                              + ' CRTOBJSCAN(*PARENT) RSTDRNMUNL(*NO)'
     C
     C*
     C                   eval      xxcmdln = %len(xxcmd)
     C*
     C                   Call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    xxcmdln
     C                   endif
     C*
/*** C*** End Add *** 01/07/10  ************************************************
     C*
     C                   Return
