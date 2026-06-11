     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSMARL1    UF   E           K DISK
      *
      *

     C                   READ      PAR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ARKEY         IFEQ      SVKEY
     C     ARSEQ#        ANDEQ     SVSEQ#
     C     ARASQ#        ANDEQ     SVASQ#
     C                   DELETE    PAR01
     C                   ELSE
     C                   MOVE      ARKEY         SVKEY            40
     C                   Z-ADD     ARSEQ#        SVSEQ#            5 0
     C                   Z-ADD     ARASQ#        SVASQ#            5 0
     C                   ENDIF
      *
     C                   READ      PAR01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      '1'           *INLR
