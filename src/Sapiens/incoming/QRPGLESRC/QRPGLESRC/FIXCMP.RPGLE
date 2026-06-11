     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSDCML5    UF   E           K DISK
     F*

     C                   MOVEL     'REJECT  '    ##KEY            40
     C*
     C     ##KEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     CMPNAM        IFNE      'REJECT  '
     C                   MOVEL     'REJECT  '    CMPNAM
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C     ##KEY         READE     PCM01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
