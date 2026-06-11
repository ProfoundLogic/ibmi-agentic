     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* 06/22/94 C:22
     H* REVISED TO INLCUDE A PAGE EJECT PRIOR TO EACH REPORT.
     H*******************************************************
     FROSPLCPY  IP   E             DISK
     FSPL19866  O    F  198        PRINTER
     F                                     INFDS(FIDS01)

     D FIDS01        E DS                  EXTNAME(SZQ4P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  SPLFLD                 1    202
     D  XXSKP                  1      3
     D  XXSPC                  4      4
     D  DATA                   5    202

     C                   MOVE      XXSKP         SKIPB             3 0
     C                   MOVE      XXSPC         SPACEB            1 0
     C*
     C*** BEGIN ADD 06/22/94 ********************
     C     *IN86         CASEQ     '0'           SKIPPG
     C*** END ADD 06/24/94 **********************
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
     C*** BEGIN ADD 06/24/94 ************************
     C     SKIPPG        BEGSR
     C                   EXCEPT    NEWPAG
     C                   MOVE      '1'           *IN86
     C                   ENDSR
     C*** END ADD 06/22/94 *********************
     OSPL19866  E            DATAL          0
     O                       DATA               198
     O*
     O          E            SPACE1      1
     O*
     O          E            NEWPAG           01
     O*
