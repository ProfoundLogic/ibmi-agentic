     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSMDLL     IP   E           K DISK
     FSDCMLE    UF   E           K DISK

     C*
     C     CMKY          KLIST
     C                   KFLD                    DLKEY
     C                   KFLD                    DLSEQ#
     C*
     C     DLOPTF        IFEQ      'Y'
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMFL03        CMFL05
     C                   UPDATE    PCM01
     C                   ENDIF
     C                   ENDIF
