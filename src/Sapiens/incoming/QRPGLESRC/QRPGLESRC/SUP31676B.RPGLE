     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SUP31676 - UPDATE RPSEQ#
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDRPL     UF   E           K DISK
     FS31676L   IF   E           K DISK

     C**************************************************************************
     C*
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C                   KFLD                    PATRDT
     C*
     C     *LIKE         DEFINE    PAFND         SVFND
     C     *LIKE         DEFINE    PAFYR         SVFYR
     C     *LIKE         DEFINE    PACASE        SVCASE
     C     *LIKE         DEFINE    PACHK#        SVCHK#
     C     *LIKE         DEFINE    PATRDT        SVTRDT
     C     *LIKE         DEFINE    PASEQ#        X
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PPA01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     PAFND         IFEQ      SVFND
     C     PAFYR         ANDEQ     SVFYR
     C     PACASE        ANDEQ     SVCASE
     C     PACHK#        ANDEQ     SVCHK#
     C     PATRDT        ANDEQ     SVTRDT
     C     RPKY          READE     PRP01                                  98
     C                   ELSE
     C     RPKY          CHAIN     PRP01                              98
     C                   MOVE(P)   PAFND         SVFND
     C                   MOVE(P)   PAFYR         SVFYR
     C                   MOVE(P)   PACASE        SVCASE
     C                   MOVE(P)   PACHK#        SVCHK#
     C                   MOVE(P)   PATRDT        SVTRDT
     C                   ENDIF
     C*
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   UPDATE    PRP01
     C                   ENDIF
     C*
     C                   READ      PPA01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
