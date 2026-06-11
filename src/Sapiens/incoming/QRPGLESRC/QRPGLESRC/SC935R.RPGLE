     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC935R                                        **
     H* DESCRIPTION: PURGE ROUTINE FOR INACTIVE REMARKS -          **
     H*              WRITE TO PURGE FILES                          **
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
     H* SDCMLB:  REMARK DETAIL FILE (KEY)                          **
     H*                                                            **
     H* PDCMP:  SAVE FILE                                          **
     H* PDCML:   LOGICAL OVER SAVE FILE (KEY/SEQ#/ASQ#)            **
     H*                                                            **
     H* SMARL:   ADDITIONAL REMARKS FILE  (KEY/SEQ#/ASQ#)          **
     H* PMARP:  SAVE FILE                                          **
     H***************************************************************
     FSDCMLB    IF   E           K DISK
     FPDCMP     O    E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSMARL     IF   E           K DISK
     FPMARP     O    E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FPMARL     IF   E           K DISK
     F                                     RENAME(PAR01:PAR03)

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    I*  Rename the duplicate field names.
    I*
     Ipcm01
     I              cmuncn                      oxuncn
     I              cmshrn                      oxshrn
     I              cmifsp                      oxifsp

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
     C     ARKYP         CHAIN     PAR03                              51
     C     *IN51         IFEQ      '1'
     C     ARDT          ANDLT     CMDT
     C                   WRITE     PAR02                                        PURGE FILE
     C                   END
     C*
     C     ARKY          READE     PAR01                                  50
     C                   END                                                    DOWEQ PAR01
     C*
     C*   WRITE REMARK DETAIL RECORD IF THIS IS AN INACTIVE REMARK
     C*
     C                   WRITE     PCM02                                        PURGE FILE
     C                   END                                                    END STAT = I
     C*
     C*** READ NEXT RECORD
     C*
     C                   READ      PCM01                                  60
     C*
     C                   END                                                    ENDDOW *IN60
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
