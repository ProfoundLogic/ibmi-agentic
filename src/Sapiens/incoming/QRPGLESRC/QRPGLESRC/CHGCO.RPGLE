     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FFTGTP     UF   E             DISK
     F*

     C                   READ      PGT01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      101           GTCO#
     C                   UPDATE    PGT01
     C                   READ      PGT01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
