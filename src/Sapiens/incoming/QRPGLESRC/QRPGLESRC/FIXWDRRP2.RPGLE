     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDELP - REMOVE DECIMAL DATA ERRORS IN FILE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDRRP     UF   E           K DISK
      *

     C                   READ      PRR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     RRASFL        IFEQ      *BLANK
     C                   MOVE      'E'           RRASFL
     C                   ENDIF
      *
     C                   UPDATE    PRR01
      *
     C                   READ      PRR01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
