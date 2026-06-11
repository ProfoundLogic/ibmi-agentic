     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*
      * DELETE LIBS AND STUFF IF FOUND ***************
     F*
     FIMTTL     UF   E           K DISK
     F*

     C*
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   CALL      'IS203C9'
     C                   PARM                    FILNAM
     C                   PARM                    FILNBR
     C                   PARM                    JOBNBR
     C                   PARM                    JOBNAM
     C                   PARM                    USRNAM
     C                   PARM                    TSLIB
     C                   PARM      *BLANKS       RTNC              1
     C*
     C     RTNC          IFEQ      '*'
     C                   MOVEL     'D'           TTSTAT
     C                   UPDATE    PTT01
     C                   ENDIF
     C                   READ      PTT01                                  60
     C                   ENDDO
     C*
     C*
     C                   MOVE      '1'           *INLR
