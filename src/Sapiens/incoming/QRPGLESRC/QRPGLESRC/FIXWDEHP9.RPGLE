     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHP7 - CORRECT TRANS DATE ON FIXED RECORDS
     H*****************************************************************
     H*                                                               *
     H* SETUP RECORDS FOR CVTINPRC3 PROCESS.                          *
     H*                                                               *
     H*****************************************************************
     FWDEHP     UF   E             DISK

     C*
     C                   READ      PEH01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     EHUSER        IFEQ      'PDSCVT'
     C     EHIAPR        ANDEQ     0
     C     EHIMPR        ANDEQ     0
     C*
     C                   Z-ADD     EHTRDT        EHIDT
     C                   Z-ADD     0             EHTRDT
      *
     C                   UPDATE    PEH01
      *
     C                   ENDIF                                                  EHUSER IFEQ ON
      *
     C                   READ      PEH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C                   MOVE      *ON           *INLR
