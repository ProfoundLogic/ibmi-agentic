     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTWMPDP - CONVERSION OF SPECIALTY CODE
     FWMPDL     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QFSPCD            3
     C                   PARM                    QTSPCD            3
     C*
     C*** PROVIDER MASTER FILE
     C                   READ      PPD01                                  60
     C     *IN60         DOWEQ     '0'
     C     PDSPCD        IFEQ      QFSPCD
     C                   MOVE      QTSPCD        PDSPCD
     C                   UPDATE    PPD01
     C                   END                                                    END EQ QFSPCD
     C                   READ      PPD01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
