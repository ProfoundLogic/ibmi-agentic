     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTUMLCP - CONVERSION OF COMPANY # UMLCP
     FUMLCP     UF   E           K DISK

     I*
     IPLC01
     I              LCSRC10                     LCSR10
     I              LCSRC11                     LCSR11
     I              LCSRC12                     LCSR12
     I              LCLT10                      XCLT10
     I              LCLT11                      XCLT11
     I              LCLT12                      XCLT12

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQTCO#            3
     C*
     C                   MOVE      QQTCO#        XXTCO#            3 0
     C*
     C*** PRE-CERT LETTER FILE
     C                   READ      PLC01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXTCO#        LCCO#
     C                   UPDATE    PLC01
     C                   READ      PLC01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
