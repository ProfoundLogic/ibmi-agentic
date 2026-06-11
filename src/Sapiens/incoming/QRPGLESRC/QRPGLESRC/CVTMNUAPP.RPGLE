     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTMNUAPP - PUT APPLICATION (LIBRARY LIST) VALUE INTO MMOPP RECORDS
     FMMOPP     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXAPP             3
     C*
     C                   READ      POP01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      XXAPP         OPAPP
     C                   UPDATE    POP01
     C*
     C                   READ      POP01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
