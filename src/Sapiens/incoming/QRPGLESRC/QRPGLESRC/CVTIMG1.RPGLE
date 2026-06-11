     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMIQP     UF   E             DISK

     C     1             SETLL     PIQ01
     C                   READ      PIQ01                                  60
     C     *IN60         DOWEQ     *OFF
     C****
     C*
     C                   MOVE      'N'           IQFL01
     C                   MOVE      'N'           IQFL02
     C                   MOVE      'N'           IQFL03
     C*
     C                   UPDATE    PIQ01
     C*
     C*
     C                   READ      PIQ01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
