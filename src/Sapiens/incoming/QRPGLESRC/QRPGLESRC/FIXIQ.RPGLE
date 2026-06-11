     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMIQP     UF   E             DISK
     F*

     C                   READ      PIQ01                                  60
     C     *IN60         DOWEQ     *OFF
     C     IQFL01        IFEQ      'Y'
     C                   MOVE      *BLANKS       IQDOCT
     C                   MOVEL     'CLMMSC  '    IQDOCT
     C                   UPDATE    PIQ01
     C                   ENDIF
     C                   READ      PIQ01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
