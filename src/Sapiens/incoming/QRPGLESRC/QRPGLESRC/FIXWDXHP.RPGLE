     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDXHP - CORRECT TRANSACTION DATE AND MAINTENANCE DATE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDXHP     UF   E           K DISK

     C*
     C*   Read every record in the file
     C                   READ      PXH01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     XHPGM         IFEQ      'W0164R'
     C     XHRDAT        ANDEQ     *ZERO
     C*
     C     XHTRDT        IFLT      100000
     C     XHDT          ORLT      100000
     C                   Z-ADD     XHCDAT        XHTRDT
     C                   Z-ADD     XHCDAT        XHDT
     C                   MOVEL     'W0164X'      XHPGM
     C*
     C                   UPDATE    PXH01
     C                   ENDIF                                                  XXTRDT<100000
     C*
     C                   ENDIF                                                  XHPGM='W0164R'
     C*
     C                   READ      PXH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
