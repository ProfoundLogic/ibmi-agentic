     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDEMEXP1 - UPDATE CO#
     FEMEXP     UF   E           K DISK
      *

     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
      *
     C                   MOVE      QQCO#         XXCO#             3 0
      *
      *   EXPECTED LOSS & DISCOUNT RATIO FILE
      *
     C                   READ      PEX01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
     C     EXCO#         IFEQ      *ZERO
     C                   Z-ADD     XXCO#         EXCO#
     C                   UPDATE    PEX01
     C                   ENDIF
      *
     C                   READ      PEX01                                  60
     C                   END                                                    ENDDOW 60
      *
     C                   MOVE      '1'           *INLR
