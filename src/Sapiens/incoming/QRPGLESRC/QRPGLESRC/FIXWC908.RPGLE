     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWC908 - MOVE INCR LIMITS DESCRIPTION TO STAT.
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDRRL     UF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      *
      *  GROUP DETAIL
      *

     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C                   READ      PFN01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     FNCO#         XXCO#             3 0
     C                   Z-ADD     FNFND         XXFND             3 0
     C*
     C     F1KY          SETGT     PF101
     C     F1KY          READPE    PF101                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     F1EMLB        ANDNE     *BLANKS
     C*
     C     F1KY          CHAIN     PRR01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RREMLB        IFEQ      *BLANKS
     C                   MOVEL(P)  F1EMLB        RREMLB
     C                   Z-ADD     F1ACC         RRACC
     C                   Z-ADD     F1DIS         RRDIS
     C                   Z-ADD     F1AGG         RRAGG
     C                   UPDATE    PRR01
     C                   ENDIF
      *
     C     F1KY          READE     PRR01                                  60
      *
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   READ      PFN01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
