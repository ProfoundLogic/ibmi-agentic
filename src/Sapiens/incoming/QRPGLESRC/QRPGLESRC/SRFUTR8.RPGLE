     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  Revisions:
     H*  04/09/98  C:69  Changed program to no longer call SRCVTDAT
     H****************************************************************
     D@@date1          s              8  0
     D@@numdays        s              5  0
     D@@err            s              1
     D*
     Ddate1            s               d   datfmt(*iso)
     Ddate2            s               d   datfmt(*usa)
     D*
     C     *entry        plist
     C                   parm                    @@date1
     C                   parm                    @@numdays
     C                   parm      *in99         @@err
     C*
     C                   if        @@date1 < 1000000
     C     *mdy          test(d)                 @@date1                99
     C  N99*mdy          move      @@date1       date1
     C                   else
     C     *usa          test(d)                 @@date1                99
     C  N99*usa          move      @@date1       date1
     C                   endif
     C*
     C                   if        *in99 = *off
     C     date1         adddur    @@numdays:*d  date2                  99
     C                   if        *in99 = *off
     C                   move      date2         @@date1
     C                   endif
     C                   endif
     C*
     C                   return
