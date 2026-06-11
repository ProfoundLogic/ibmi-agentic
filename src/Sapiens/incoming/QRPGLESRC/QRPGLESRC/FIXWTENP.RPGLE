     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FWTENP     UF   E           K DISK

     C*
     C                   READ      PEN01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      'S'           ENSNGL
     C                   UPDATE    PEN01
     C                   READ      PEN01                                  60                R
     C                   END
     C*
     C                   MOVE      '1'           *INLR
