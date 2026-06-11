     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSMPRL2    UF   E           K DISK
     F*

     C     PRKY          KLIST
     C                   KFLD                    XXDCID           12
     C*
     C                   MOVEL     'DLS04045'    XXDCID
     C                   MOVE      '.002'        XXDCID
     C*
     C*
     C     PRKY          CHAIN     PPR01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       PRDCID
     C                   MOVE      *BLANKS       PRERDS
     C                   MOVEL     'P'           PRSTAT
     C                   UPDATE    PPR01
     C     PRKY          READE     PPR01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
