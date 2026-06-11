     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FWDUSL4    UF   E           K DISK

     I*

     C     USKY          KLIST
     C                   KFLD                    QQRPT             8
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRPT
     C                   PARM                    QQSTAT            1
     C*
     C     USKY          CHAIN     PUS01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      QQSTAT        USSTAT
     C                   UPDATE    PUS01
     C     USKY          READE     PUS01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
     C***END PROGRAM
