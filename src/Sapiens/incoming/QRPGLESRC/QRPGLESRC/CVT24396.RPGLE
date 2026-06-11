     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE
     H*****************************************************************
     H* Description:  This pgm loads a unique sequence # into each    *
     H*   EDI log history record.                                     *
     H*                                                               *
     H*****************************************************************
     FWMLGL     UF   E           K DISK

     C*-------------------------------------------------------------------------
     C                   READ      PLG01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     LGCASE        IFNE      SVCASE
     C                   Z-ADD     1             X                 5 0
     C                   Z-ADD     LGCASE        SVCASE            7 0
     C                   ENDIF
     C*
     C                   Z-ADD     X             LGDSQH
     C                   UPDATE    PLG01
     C*
     C                   ADD       1             X
     C*
     C                   READ      PLG01                                  99
     C                   ENDDO
     C*
     C*  End of program
     C*
     C                   SETON                                        LR
