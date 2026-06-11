     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT

     D                 DS                  INZ
     D  ZZCOMM                 1     15
     D  ZZFND                  1      3
     D  ZZEMP#                 4     12
     D  ZZFYR                 13     15
     **

     C     *ENTRY        PLIST
     C                   PARM                    @@PLCY           15
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     **
     **  BUILD THE POLICY NUMBER
     **
     C                   MOVE      @@FND         ZZFND             3
     C                   MOVE      @@EMP#        ZZEMP#            9
     C                   MOVE      @@FYR         ZZFYR             3
     C                   MOVE      ZZCOMM        @@PLCY
     **
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
