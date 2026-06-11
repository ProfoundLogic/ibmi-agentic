     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMDOP     UF   E             DISK
     F*

     C*
     C     1             SETLL     PDO01
     C                   READ      PDO01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     DOODCI        IFNE      *BLANKS
     C                   MOVEL     'Y'           DOOPTF
     C                   UPDATE    PDO01
     C                   ENDIF
     C*
     C                   READ      PDO01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
