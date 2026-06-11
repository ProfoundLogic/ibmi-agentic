     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXROLL - RESET BILLING SCHEDULE CODE FROM ROLLOVER.
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL1    UF   E           K DISK
      *

     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XPFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
     C                   Z-ADD     45            XXCO#             3 0
     C                   Z-ADD     1             XXFND             3 0
     C                   Z-ADD     99            XXFYR             3 0
     C                   Z-ADD     98            XPFYR             3 0
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ELBSCD        IFEQ      1
      *
     C                   Z-ADD     ELEMP#        XXEMP#            9 0
     C                   Z-ADD     ELDIV         XXDIV             5 0
     C     ELKY1         CHAIN     PEL02                              50
      *
     C     *IN50         IFEQ      *OFF
     C     ELBSCD        ANDNE     1
     C                   MOVE      ELBSCD        SVBSCD            3
     C     ELKY2         CHAIN     PEL01                              60
     C                   MOVE      SVBSCD        ELBSCD
     C                   UPDATE    PEL01
     C                   ENDIF                                                  *IN50 IFEQ
      *
     C                   ENDIF                                                  ELBSCD IFEQ
      *
     C     ELKY          READE     PEL01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
