     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     FWTCRP     UF   E             DISK

     C                   READ      PCR01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CREFDT        IFNE      *ZERO
     C                   DELETE    PCR01
     C                   ENDIF                                                  CREFDT IFNE
     C*
     C                   READ      PCR01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   SETON                                        LR
