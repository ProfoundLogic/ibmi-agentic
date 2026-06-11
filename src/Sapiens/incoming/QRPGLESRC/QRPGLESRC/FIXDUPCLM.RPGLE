     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXDUPCLM - FIX DUP CLAIMS
     H*****************************************************************
     FWMCML1    UF   E           K DISK

     C*
     C     *LIKE         DEFINE    CMCO#         SVCO#
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMFYR         SVFYR
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C                   CLEAR                   SVCO#
     C                   CLEAR                   SVFND
     C                   CLEAR                   SVFYR
     C                   CLEAR                   SVCASE
     C*
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     CMCO#         IFEQ      SVCO#
     C     CMFND         ANDEQ     SVFND
     C     CMFYR         ANDEQ     SVFYR
     C     CMCASE        ANDEQ     SVCASE
     C*
     C                   Z-ADD     999           CMCO#
     C                   UPDATE    PCM01
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CMCO#         SVCO#
     C                   Z-ADD     CMFND         SVFND
     C                   Z-ADD     CMFYR         SVFYR
     C                   Z-ADD     CMCASE        SVCASE
     C*
     C                   ENDIF                                                  CMCO#=SVCO#
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           *INLR
