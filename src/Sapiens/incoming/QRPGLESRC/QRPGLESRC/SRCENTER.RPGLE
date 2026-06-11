     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT

     C     *ENTRY        PLIST
     C                   PARM                    @@STR            80
     C                   PARM                    @@LEN             3 0
     C*
     C     *LIKE         DEFINE    @@STR         NEWSTR
     C*
     C                   MOVE      *BLANKS       NEWSTR
     C     @@STR         IFNE      *BLANKS
     C     ' '           CHECK     @@STR         X                 3 0
     C                   SUBST(P)  @@STR:X       @@STR
     C     ' '           CHECKR    @@STR         X
     C     @@LEN         SUB       X             X
     C     X             IFLT      *ZERO
     C                   SUB       X             X
     C                   ENDIF
     C                   DIV       2             X
     C                   CAT(P)    @@STR:X       NEWSTR
     C                   ENDIF
     C                   MOVEL(P)  NEWSTR        @@STR
     C*
     C                   RETURN
