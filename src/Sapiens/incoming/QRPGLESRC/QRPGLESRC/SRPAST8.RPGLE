     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     D@@date1          s              8  0
     D@@numdays        s              5  0
     D@@err            s              1
     D*
     D@@numdays2       s              5  0
     D*
     C     *entry        plist
     C                   parm                    @@date1
     C                   parm                    @@numdays
     C                   parm      *in99         @@err
     D*
     C     futdat        plist
     C                   parm                    @@date1
     C                   parm                    @@numdays2
     C     *in99         parm                    @@err
     C*
     C     @@numdays     ifgt      0
     C                   z-sub     @@numdays     @@numdays2
     C                   else
     C                   z-add     @@numdays     @@numdays2
     C                   endif
     C                   call      'SRFUTR8'     futdat
     C                   return
