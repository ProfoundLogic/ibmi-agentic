     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMDOP     IF   E             DISK
     FIAFTLA    UF   E           K DISK
     F*

     C*
     C     1             SETLL     PDO01
     C                   READ      PDO01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     DODSTS        IFEQ      'DL'
     C     DOFL02        ANDEQ     'D'
     C** GET ALL FAX TRANS TO REMOVE ***
     C     DODCID        CHAIN     PFT01                              61
     C     *IN61         DOWEQ     *OFF
     C                   DELETE    PFT01
     C     DODCID        READE     PFT01                                  61
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   READ      PDO01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
