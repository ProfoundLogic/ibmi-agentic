     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FFMCMP     IF   E             DISK
     FFMFNL     IF   E           K DISK

     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C*
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'WA721R'
     C                   PARM      CMCO#         $$CO#             3 0
     C                   PARM      FNWCF#        $$FND             3 0
     C                   PARM      CMFYR         $$FYR             3 0
     C                   PARM      CMCASE        $$CASE            7 0
     C                   PARM      CMCMNU        $$NRSE            3
     C*
     C                   ENDIF
     C                   READ      PCM01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
