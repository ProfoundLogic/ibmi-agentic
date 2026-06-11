     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE RPSEQ#
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWMCHL     UF   E           K DISK

     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C                   READ      PCH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     CHEFDT        IFNE      CHDT
     C     CHPGM         ANDEQ     'W8608R'
     C*
     C                   Z-ADD     CHDT          CHEFDT
     C                   MOVE      'FX'          CHPGM
     C                   UPDATE    PCH01
     C*
     C                   ENDIF
     C*
     C                   READ      PCH01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
