     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTFMRXP - CONVERSION OF COMPANY # FMRXP
     FFMRXP     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQTCO#            3
     C*
     C                   MOVE      QQTCO#        XXTCO#            3 0
     C*
     C*** NDC DRUG CODE FILE
     C                   READ      PRX01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXTCO#        RXCO#
     C                   UPDATE    PRX01
     C                   READ      PRX01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
