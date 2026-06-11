     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWMXAP - Remove Decimal-Date Errors in XARDIV
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/09/06  C:33  SUPPORT WA136 54913                          *
     H*                                                               *
     H*****************************************************************
     FWMXAP     UF   E           K DISK
      *

     C                   READ      PXA01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C                   UPDATE    PXA01
     C                   READ      PXA01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
      *
