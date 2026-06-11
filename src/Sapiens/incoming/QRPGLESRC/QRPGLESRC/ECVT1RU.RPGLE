     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE ECVT1RU - CONVERSION OF COMPANY # FOR EMEXP (UPDATE)
     FEMEXP     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFCO#            3
     C                   PARM                    QQTCO#            3
     C*
     C                   MOVE      QQFCO#        XXFCO#            3 0
     C                   MOVE      QQTCO#        XXTCO#            3 0
     C*
     C*** EXPECTED LOSS & DISCOUNT RATIO FILE
     C                   READ      PEX01                                  60
     C     *IN60         DOWEQ     '0'
     C     EXCO#         IFEQ      XXFCO#
     C                   Z-ADD     XXTCO#        EXCO#
     C                   UPDATE    PEX01
     C                   END                                                    END EQ XXFCO#
     C                   READ      PEX01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
