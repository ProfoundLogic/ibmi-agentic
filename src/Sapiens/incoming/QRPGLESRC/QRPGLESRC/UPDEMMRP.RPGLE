     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE ECVT1RU - CONVERSION OF COMPANY # FOR EMEXP (UPDATE)
     FEMMRP     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTST            3
     C*
     C                   MOVE      QQRTST        XXRTST            3
     C*
     C*** PAYROLL FILE
     C                   READ      PMR01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     XXRTST        MRDRSC
     C                   UPDATE    PMR01
     C                   READ      PMR01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
