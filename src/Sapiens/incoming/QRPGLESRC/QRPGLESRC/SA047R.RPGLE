     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/TITLE SA047R - API to retrieve Scriptura Module System Flag
     H*====================================================================
     H*  Description:
     H*
     H*  Access System Control file record 'SY' to obtain values for SYFL11
     H*   (Notifications Module enabled when position 7 = 'Y')
     H*
     H*  Created:  12/03/14  C:65  Project: SY251
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
     C                   parm                    @@Flag                         Scriptura
     C*
     C                   clear                   SYFL11
     C*
     C*  Access System Control file 'SY' record
     C*
     C                   eval      kkKey = 'SY'
     C     kkKey         chain     ssy01
     C                   eval      @@Flag = %subst(SYFL11:7:1)
     C*
     C                   return
