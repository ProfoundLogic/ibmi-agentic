     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMIEP     UF   E             DISK
     F*

     C*
     C     1             SETLL     PIE01
     C                   READ      PIE01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     IEDOCT        IFEQ      'OWNLETT '
     C                   MOVEL     *BLANKS       IEDOCT
     C                   MOVEL     'TEST    '    IEDOCT
     C                   MOVEL     *BLANKS       IEQID
     C                   MOVEL     'TEST    '    IEQID
     C                   UPDATE    PIE01
     C                   ENDIF
     C*
     C                   READ      PIE01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
