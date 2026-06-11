     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO
     H*****************************************************************
     H* Description:                                                  *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWACPL     UF   E           K DISK
     FWARPL     UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQPRDT            7            CYYMMDD
     C*
     C*  Key list
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXPRDT            7 0
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*----------------------------------------------------------------
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQPRDT        XXPRDT            7 0
     C*
     C*  Start of main loop
     C*
     C     CPKY          CHAIN     PCP01                              99
     C*
     C     *IN99         DOWEQ     '0'
     C                   MOVE      *BLANKS       CPSTA0
     C                   MOVE      *BLANKS       CPSTA1
     C                   UPDATE    PCP01
     C*
     C     CPKY          READE     PCP01                                  99
     C                   ENDDO
     C*
     C     RPKY          CHAIN     PRP01                              99
     C*
     C     *IN99         DOWEQ     '0'
     C     RPSTAT        IFEQ      'Y'
     C                   MOVE      *BLANKS       RPSTAT
     C                   ENDIF
     C                   UPDATE    PRP01
     C*
     C     RPKY          READE     PRP01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
