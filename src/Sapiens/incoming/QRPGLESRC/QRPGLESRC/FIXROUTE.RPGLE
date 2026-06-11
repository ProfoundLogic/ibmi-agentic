     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXROUTE - ASSIGN CORRECT OUTQ FOR ALL ROUTING RECORDS
     FSMORL     UF   E           K DISK

     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQROUT           10
     C*
     C                   READ      POR01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVEL     QQROUT        OROUTQ
     C                   UPDATE    POR01
     C                   READ      POR01                                  60                R
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
