     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMEMP - REMOVE DECIMAL DATA ERRORS IN FILE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDCXP     UF   E           K DISK
      *

     C                   READ      PCX01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   UPDATE    PCX01
      *
     C                   READ      PCX01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
