     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE FIXWMEAP - REMOVE DECIMAL DATA ERRORS IN FILE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMEAP     UF   E           K DISK
      *

     C                   READ      PEA01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   UPDATE    PEA01
      *
     C                   READ      PEA01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
