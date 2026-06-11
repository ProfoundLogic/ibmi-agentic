     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXARTY - CORRECT MISSING A/R TYPE AND DESC
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMEAP     UF   E             DISK
     FWTABL     IF   E           K DISK

     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    XXKEY            10
      *
     C                   READ      PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAARTY        IFEQ      *BLANKS
     C     EADESC        OREQ      *BLANKS
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     EADESC        IFEQ      *BLANKS
     C                   MOVEL(P)  TTDESC        EADESC
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      *BLANKS
     C                   MOVE      TTTYPE        EAARTY
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  EAARTY IFEQ
      *
     C                   READ      PEA01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
