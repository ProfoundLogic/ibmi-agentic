     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FROSPLCPY  IP   E             DISK
     FSPL13288  O    F  132        PRINTER
     F                                     INFDS(FIDS01)

     D FIDS01        E DS                  EXTNAME(SZQ4P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  SPLFLD                 1    202
     D  XXSKP                  1      3
     D  XXSPC                  4      4
     D  DATA                   5    136

     C                   MOVE      XXSKP         SKIPB             3 0
     C                   MOVE      XXSPC         SPACEB            1 0
     C*
     C     SKIPB         CASNE     *ZERO         SKIPSR
     C     SPACEB        CASNE     *ZERO         SPACSR
     C                   END
     C                   EXCEPT    DATAL
      *------------------------------------------------------------------*
     C     SKIPSR        BEGSR
     C*
     C     Q4LIN#        IFLT      SKIPB
     C*
     C     SKIPB         SUB       Q4LIN#        X                 5 0
     C                   DO        X
     C                   EXCEPT    SPACE1
     C                   END
     C*
     C                   ELSE
     C*
     C                   EXCEPT    NEWPAG
     C     SKIPB         SUB       1             X
     C*
     C                   DO        X
     C                   EXCEPT    SPACE1
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
      *------------------------------------------------------------------*
     C     SPACSR        BEGSR
     C*
     C                   DO        SPACEB
     C*
     C                   EXCEPT    SPACE1
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     OSPL13288  E            DATAL          0
     O                       DATA               132
     O*
     O          E            SPACE1      1
     O*
     O          E            NEWPAG           01
     O*
