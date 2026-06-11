     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  Revisions:
     H*  04/08/98  C:69  Changed program to no longer call SRCVTDAT
     H****************************************************************
     D@@date1          s              8  0
     D@@date2          s              8  0
     D@@diff           s              5  0
     D@@err            s              1
     D*
     Ddate1            s               d   datfmt(*iso)
     Ddate2            s               d   datfmt(*iso)
     D*
     C     *entry        plist
     C                   parm                    @@date1
     C                   parm                    @@date2
     C                   parm                    @@diff
     C                   parm      *in99         @@err
     C*
     C                   move      *zeros        @@diff
     C*
     C                   if        @@date1 < 1000000
     C     *mdy          test(d)                 @@date1                99
     C  N99*mdy          move      @@date1       date1
     C                   else
     C     *usa          test(d)                 @@date1                99
     C  N99*usa          move      @@date1       date1
     C                   endif
     C*
     C                   if        *in99 = *OFF
     C*
     C                   if        @@date2 < 1000000
     C     *mdy          test(d)                 @@date2                99
     C  N99*mdy          move      @@date2       date2
     C                   else
     C     *usa          test(d)                 @@date2                99
     C  N99*usa          move      @@date2       date2
     C                   endif
     C*
     C                   if        *in99=*OFF
     C     date2         subdur    date1         @@diff:*days           99
     C                   endif
     C                   endif
     C                   return
