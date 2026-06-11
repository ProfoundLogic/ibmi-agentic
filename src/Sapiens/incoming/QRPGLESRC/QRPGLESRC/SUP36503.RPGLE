     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE WDPAP PAPGTP
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDPAL     UF   E           K DISK

     C*
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PPA01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     PATYPE        IFEQ      'R'
     C     PAPGTP        ANDEQ     'G'
     C*
     C                   MOVE      *BLANKS       PAPGTP
     C                   UPDATE    PPA01
     C*
     C                   ENDIF
     C*
     C                   READ      PPA01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
