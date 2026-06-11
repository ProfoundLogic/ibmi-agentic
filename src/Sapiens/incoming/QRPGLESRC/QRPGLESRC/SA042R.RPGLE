     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/TITLE SA002R - API to retrieve Medicare Secondary Payer Flag
     H*====================================================================
     H*  Description:
     H*
     H*  Access System Control file record 'SY' to obtain values for SYFL12
     H*   (Secondary Payer installed when pos 2 = 'Y')
     H*
     H*  Created:  05/20/09  C:33  Project: WC1770
     H*====================================================================
     H*  Revisions:
     H*
     H*====================================================================
     H* System Control file
     FSSYLF     if   e           k disk
     D*
     D @@Flag          s              1
     D kkKey           s                   like(SYKEY)
     C*
     C     *entry        plist
     C                   parm                    @@Flag                         GL/AP INST
     C*
     C                   clear                   SYFL12
     C*
     C*  Access System Control file 'SY' record
     C*
     C                   eval      kkKey = 'SY'
     C     kkKey         chain     ssy01
     C                   eval      @@Flag = %subst(SYFL12:2:1)
     C*
     C                   return
