     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC936R                                        **
     H* DESCRIPTION: PURGE ROUTINE FOR INACTIVE REMARKS - DELETE   **
     H*              FROM SYSTEM FILES.                            **
     H*                                                            **
     H* CREATED BY:  WENDY DILLARD        DATE:  05/12/92          **
     H***************************************************************
     H* REVISIONS:                                                 **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  50              GENERAL CHAIN/DOWEQ                       **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     H* FILES:                                                     **
     H* PRIMARY FILE -                                             **
     H* SDCMLE:  REMARK DETAIL FILE (KEY/SEQ#/DT)                  **
     H* PDCMP:  SAVE FILE                                          **
     H*                                                            **
     H* SMARL:   ADDITIONAL REMARKS FILE  (KEY/SEQ#/ASQ#)          **
     H* PMARP:  SAVE FILE                                          **
     H***************************************************************
     FSDCMLE    UF   E           K DISK
     FSMARL     UF   E           K DISK
     FPMARP     O    E           K DISK
     F                                     RENAME(PAR01:PAR02)

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  ADDITIONAL REMARKS FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*  ADDITIONAL REMARKS PURGE FILE
     C*
     C     ARKYP         KLIST
     C                   KFLD                    ARKEY
     C                   KFLD                    ARSEQ#
     C                   KFLD                    ARASQ#
     C*
     C*  REMARK DETAIL FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ARKEY
     C                   KFLD                    ARSEQ#
     C*
     C*   READ REMARK DETAIL RECORD
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CMSTAT        IFEQ      'I'
     C*
     C*   PROCESS ADDITIONAL REMARKS RECORDS
     C*
     C     ARKY          CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C     ARDT          IFLT      CMDT
     C                   DELETE    PAR01
     C                   END
     C*
     C     ARKY          READE     PAR01                                  50
     C                   END                                                    DOWEQ PAR01
     C*
     C*   PROCESS REMARK DETAIL RECORD
     C*
     C                   DELETE    PCM01
     C*
     C                   END                                                    END STAT = I
     C*
     C*   READ NEXT RECORD
     C*
     C                   READ      PCM01                                  60
     C                   END                                                    ENDDOW *IN60
     C*
     C*  DELETE ANY REMAINING ADDITIONAL REMARKS THAT DON'T HAVE A
     C*     CORRESPONDING KEY IN SDCMP
     C*
     C                   MOVEL     *LOVAL        CMKEY
     C                   MOVEL     *LOVAL        CMSEQ#
     C     ARKY          SETLL     PAR01
     C                   READ      PAR01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   WRITE     PAR02
     C*
     C                   DELETE    PAR01
     C                   END
     C*
     C                   READ      PAR01                                  50
     C                   END
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
