     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX28063 - REMOVE EMP # 18000 RECORDS FROM NCCI FILES
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWMSHP     UF   E           K DISK
     FWXNAP     UF   E           K DISK
     FWXNAP1    UF   E           K DISK
     F                                     RENAME(PNA01:PNA02)
     FWXNEP     UF   E           K DISK
      *

     C                   READ      PSH01                                  60
     C     *IN60         DOWEQ     *OFF
     C     SHEMP#        IFEQ      18000
     C                   DELETE    PSH01
     C                   ENDIF
     C                   READ      PSH01                                  60
     C                   ENDDO
      *
     C                   READ      PNA01                                  60
     C     *IN60         DOWEQ     *OFF
     C     NAEMP#        IFEQ      18000
     C                   DELETE    PNA01
     C                   ENDIF
     C                   READ      PNA01                                  60
     C                   ENDDO
      *
     C                   READ      PNA02                                  60
     C     *IN60         DOWEQ     *OFF
     C     NAEMP#        IFEQ      18000
     C                   DELETE    PNA02
     C                   ENDIF
     C                   READ      PNA02                                  60
     C                   ENDDO
      *
     C                   READ      PNE01                                  60
     C     *IN60         DOWEQ     *OFF
     C     NEEMP#        IFEQ      18000
     C                   DELETE    PNE01
     C                   ENDIF
     C                   READ      PNE01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
