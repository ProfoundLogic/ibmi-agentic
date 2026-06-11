     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIX27938 - MAKE EACH RECORD UNIQUE            C:62 07/26/01  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMRHL     UF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     RHKY          KLIST
     C                   KFLD                    RHCO#
     C                   KFLD                    RHFND
     C                   KFLD                    RHFYR
     C                   KFLD                    RHCASE
     C                   KFLD                    RHEFDT
     C*
     C     *LIKE         DEFINE    RHTIME        XXTIME
     C*
     C                   READ(N)   PRH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     1             X                 4 0
     C*
     C     RHKY          READE     PRH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   ADD       X             RHTIME
     C                   ADD       1             X
     C                   UPDATE    PRH01
     C*
     C     RHKY          READE     PRH01                                  99
     C*
     C                   ENDDO
     C*
     C     RHKY          SETGT     PRH01
     C*
     C                   READ(N)   PRH01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
