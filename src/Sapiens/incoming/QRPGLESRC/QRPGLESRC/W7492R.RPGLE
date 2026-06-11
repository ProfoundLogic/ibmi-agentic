     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS folder location
     H*====================================================================
     H*  Created: 05/18/10   C:198  Project: WC1879
     H*====================================================================
     H*  Revisions:
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
     D*
    D*====================================================================
    D* Stand Alone Fields
    D*====================================================================
     Dfolder           s             80
     D IfsObj          s            256a   inz
     D IfsObjFnd       s               n   inz
    D*====================================================================
     C*
     C     *entry        plist
     C                   parm                    @@ifs            30
     C                   parm                    @@icd9           30
     C                   parm                    @@fnd             1
     C*
     C                   if        @@ifs <> ' ' and @@icd9 <> ' '
     C*
     C                   Eval      folder = %trim(@@ifs) + '/' + %trim(@@icd9)
     C                   Eval      @@fnd = ' '
     C*
     C                   Eval      IfsObj = %trim(folder)
     C*
     C*** check for folder
     C                   Call      'CHKIFSOBJ'
     C                   parm                    IfsObj
     C                   parm                    IfsObjFnd
     C*
     C                   If        not IfsObjFnd
     C                   eval      @@fnd = '0'
     C                   endif                                                  not found
     C*
     C                   endif                                                  @@ifs <>  ' '
     C*
     C                   seton                                        lr
