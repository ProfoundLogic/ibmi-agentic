     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIX28589 -                                    C:62 08/10/01  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMCMP     UF   E             DISK
     FWMCHL     IF   E           K DISK
     F*

     C*
     C     CHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C                   READ      PCM01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     CHKY          CHAIN     PCH01                              99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        ANDEQ     'O'
     C*
     C                   Z-ADD     CHDT          CMIDT
     C                   MOVEL     CHUSER        CMIUSR
     C                   MOVEL     CHPGM         CMIPGM
     C                   UPDATE    PCM01
     C*
     C                   ELSE
     C*
     C                   UPDATE    PCM01
     C*
     C                   ENDIF
     C*
     C                   READ      PCM01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
