     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXMMOPP - BLANK OUT APPLICATION FIELD
     FMMOPP     UF   E           K DISK

     C*
     C                   READ      POP01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      'WWW'         OPAPP
     C                   UPDATE    POP01
     C                   READ      POP01                                  60                R
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
