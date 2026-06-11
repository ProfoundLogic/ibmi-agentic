     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIPDS0160  O    F  132        PRINTER
     F*

     C                   MOVE      'IPDS0160'    FNAME             8
     C*
     C     CYCLE         TAG
     C*
     C                   EXCEPT    TEST
     C*
     C                   MOVE      'FONT    '    OPTION            8
     C                   MOVEL     '1137    '    PARMS
     C                   EXSR      PRPQ
     C*
     C                   EXCEPT    TEST
     C*
     C*
     C                   MOVE      'EMPHASIS'    OPTION            8
     C                   MOVEL     'ON      '    PARMS
     C                   EXSR      PRPQ
     C*
     C                   EXCEPT    TEST
     C*
     C                   MOVE      'FONT    '    OPTION            8
     C                   MOVEL     '0005    '    PARMS
     C                   EXSR      PRPQ
     C*
     C                   EXCEPT    TEST
     C*
     C*
     C*
     C                   MOVE      'EMPHASIS'    OPTION            8
     C                   MOVEL     'OFF     '    PARMS
     C                   EXSR      PRPQ
     C*
     C                   EXCEPT    TEST
     C*
     C                   MOVE      '1'           *INLR
     C*
     C     PRPQ          BEGSR
     C*
     C                   CALL      'SUBR50  '
     C                   PARM                    FNAME
     C                   PARM                    OPTION
     C                   PARM                    PARMS            80
     C                   PARM      *BLANK        RTCD              2
     C*
     C                   ENDSR
     OIPDS0160  E            TEST             20
     O                                           65 'THIS IS A Test Test IIii'
