     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title Query Response File prompt
     H*====================================================================
     H*  Created: 08/14/09   C:232  Project: WC1819 Phase 5.
     H*====================================================================
    H*  Revisions:
     H*
/* 07H* 07/07/10  C:198 Project: WC1819 Priss 113
/*   H*                 fixed the issue where test file might override
/*   H*                 the file from production in CMS.
/*   H*
     H* 01/07/10  C:220 Project: WC1819 Phase 5
     H*                 Changed from QDLS to IFS and corrected the logic
     H*                 to get folder name.
     H*
     H*====================================================================

    F*  Physical file definition (for maintaining record)
     FW7489F    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fssypf     if   e             disk

    D*================================================================
    D*  External Data Structure
    D*================================================================

     D*====================================================================
     D* Data Structures
     D*====================================================================
     DtheSuffix        ds             3
     D syfl08                         1
     D syfl09                         1
     D syfl10                         1
     C*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     Dfolder           s             80    inz('CMS')
     Dfolder_save      s             80
     Dsuffix           s              3
     D IfsObj          s            256a   inz
     D IfsObjFnd       s               n   inz
     D comapi          c                   const('WA001R')
     D Title           s             40a   inz
     D Call            s             10a   inz
     D ##CO#           s              3  0 inz
     D ##FND           s              3  0 inz
     D ##CONM          s             40a   inz
     D ##FNAM          s             40a   inz
     D Que             s             10a   inz
     D xxCO#           s              3    inz
     D*** Beg Add *** 01/07/10 ***********************************
     D xxcmd           s            200    inz(*blanks)
     D xxcmdln         s             15  5 inz(*zeros)
     D*** End Add *** 01/07/10 ***********************************
     D*** Beg Add *** 07/07/10 ***********************************
     D xxenv           s              1
     D*** End Add *** 07/07/10 ***********************************

    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*================================================================
    C*  KLIST definitions
    C*================================================================

    C*================================================================
    C*                  M A I N L I N E
    C*================================================================
     C                   movel     folder        xxpds             4
     C*
     C*** Beg Del *** 01/07/10 **********************************************
     C*
     C******             if        (xxpds <> 'QDLS/') and (folder <> *blanks)
     C*
     C******             eval      folder_save = %trim(folder)
     C*
     C******             read      ssypf                                  99
     C*
     C******             if        *in99 = *off
     C******             movel     theSuffix     suffix
     C*
     C******             if        %subst(syfl11:2:1) = 'Y'
     C*
     C******             if        suffix <> *blanks
     C******             eval      folder =  suffix
     C******                                + '/'
     C******                                +folder_save
     C******             else
     C*
     C******             eval      folder = folder_save
     C*
     C******             endif                                                  suffix <> ' '
     C*
     C******             endif                                                  syfl11:2:1 = 'Y'
     C*
     C******             endif                                                  *in99 = *off
     C*
     C******             endif                                                  xxpds <> 'QDLS/'
     C*
     C******             Eval      IfsObj = 'QDLS/' +%trim(folder) + '/'
     C******                                + 'WXQRP.TXT'
     C*
     C*****              Eval      IfsObj = '/QDLS/CMS/WXQRP.TXT'
     C*** End Del *** 01/07/10 **********************************************
     C*** Beg Add *** 01/07/10 **********************************************
     C*
     C                   Call      'SA030R'
     C                   parm                    suffix
     C                   parm                    prefix           80
     C*
     C*** Beg Add *** 07/07/10 **********************************************
     C                   call      'SRTEST'
     C                   parm                    xxenv
     C*** End Add *** 07/07/10 **********************************************
     C                   If        prefix <> *blanks
     C*** Beg Add *** 07/07/10 **********************************************
     C                   If        xxenv = 'T'
     C                   Eval      IfsObj = %trim(prefix) + '/TEST/' +
     C                                      %trim(folder)
     C                   Else
     C*** End Add *** 07/07/10 **********************************************
     C                   Eval      IfsObj = %trim(prefix) + '/' + %trim(folder)
     C*** Beg Add *** 07/07/10 **********************************************
     C                   EndIf
     C*** End Add *** 07/07/10 **********************************************
     C                   else
     C*** Beg Add *** 07/07/10 **********************************************
     C                   If        xxenv = 'T'
     C                   Eval      IfsObj = 'TEST/' +
     C                                      %trim(folder)
     C                   Else
     C*** End Add *** 07/07/10 **********************************************
     C                   Eval      IfsObj = %trim(folder)
     C*** Beg Add *** 07/07/10 **********************************************
     C                   EndIf
     C*** End Add *** 07/07/10 **********************************************
     C                   endif
     C*
     C*** check for folder
     C                   Call      'CHKIFSOBJ'
     C                   parm                    IfsObj
     C                   parm                    IfsObjFnd

     C                   If        not IfsObjFnd
     C*
     C                   eval      xxcmd= 'MKDIR DIR(''' +%trim(IfsObj)+ ''')'
     C                              + ' DTAAUT(*INDIR)'
     C                              + ' OBJAUT(*INDIR) CRTOBJAUD(*SYSVAL)'
     C                              + ' CRTOBJSCAN(*PARENT) RSTDRNMUNL(*NO)'
     C*
     C                   eval      xxcmdln = %len(xxcmd)
     C*
     C                   Call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    xxcmdln
     C                   endif                                                   DIR not found
     C*
     C*** check for file
     C                   Eval      IfsObj = %trim(IfsObj)
     C                                      + '/' + 'WXQRP.TXT'
     C*
     C                   Call      'CHKIFSOBJ'
     C                   parm                    IfsObj
     C                   parm                    IfsObjFnd
     C*
     C                   If        not IfsObjFnd
     C*
     C*** End Add *** 01/07/10 **********************************************
     C                   exfmt     SRC02#
     C                   else
     C                   exfmt     SRC01#

     C                   call      comapi
     C                   parm                    ##CO#
     C                   parm                    ##FND
     C                   parm                    ##CONM
     C                   parm                    ##FNAM

     C                   movel     ##CO#         xxco#

     C                   If        not *inkc
     C                   Call      'W7489C'
     C                   parm                    xxCO#
     C                   parm                    Que
     C                   endif

     C                   endif                                                   .TXT file found

     C                   Return
