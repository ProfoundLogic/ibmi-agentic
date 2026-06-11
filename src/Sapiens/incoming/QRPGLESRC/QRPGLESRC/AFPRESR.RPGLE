     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0

     I****************************************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFB              6
     C*
     C                   EXCEPT    HEAD1
     C*
     C     QQFB          IFNE      '*FRONT'
     C                   EXCEPT    HEAD2
     C                   ENDIF
     **(((((((((((((((((((
     C*
     C*** END PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      ****
     OQSYSPRT   E            HEAD1          1 06
     O*****************************************************************
     O*      PROGRAM NAME                                             *
     O*****************************************************************
     O                                           19 'TEST OVERLAY PRINT'
     O          E            HEAD2          1 06
     O                                           19 'TEST OVERLAY PRINT'
