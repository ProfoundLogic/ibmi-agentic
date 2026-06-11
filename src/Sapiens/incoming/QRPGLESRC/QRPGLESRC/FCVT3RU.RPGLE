     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FCVT3RU - CONVERSION OF EFFECTIVE DATE FOR FMRCP  (YYMMDD)
     FFMRCL     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFDT             6
     C                   PARM                    QQTDT             6
     C*
     C                   MOVE      QQFDT         XXFDT             6 0
     C                   MOVE      QQTDT         XXTDT             6 0
     C*
     C*** R&C CODE FILE
     C                   READ      PRC01                                  60
     C     *IN60         DOWEQ     '0'
     C     RCEFDT        IFEQ      XXFDT
     C                   Z-ADD     XXTDT         RCEFDT
     C                   UPDATE    PRC01
     C                   END                                                    END EQ XXFDT
     C                   READ      PRC01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
