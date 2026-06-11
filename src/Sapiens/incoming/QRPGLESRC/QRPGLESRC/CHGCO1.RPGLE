     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FFTGIP     UF   E             DISK
     F*

     C                   READ      PGI01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      101           GICO#
     C                   UPDATE    PGI01
     C                   READ      PGI01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
