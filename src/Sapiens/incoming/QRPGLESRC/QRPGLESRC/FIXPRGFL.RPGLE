     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     FIXPRGFL                                      **
     H* DESCRIPTION: FIX PURGE FLAGS ON DIARY REMARKS              **
     H*              REVIEWS ALL DARTS DIARY REMARKS.  IF PURGE    **
     H*              FLAG IS BLANK, CHAINS TO REMARK CODE TABLE    **
     H*              FILE AND IF '# DAYS TO KEEP ON SYSTEM' IS     **
     H*              NOT EQUAL TO ZERO, LOADS A 'Y' INTO PURGE     **
     H*              FLAG (CMFL02).                                **
     H*                                                            **
     H* CREATED BY:  WENDY DENEHY                                  **
     H*    PROJECT:  ATD                                           **
     H*       DATE:  07/28/93                                      **
     H***************************************************************
     H* REVISIONS:                                                 **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  50              GENERAL CHAIN/DOWEQ                       **
     H*  51              GENERAL CHAIN/DOWEQ                       **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     H* FILES:                                                     **
     H* PRIMARY FILE -                                             **
     H*  SDCMP: REMARK DETAIL FILE                                 **
     H*                                                            **
     H*  STDTL1: REMARK CODE FILE (RMCD)                           **
     H***************************************************************
     FSDCMP     UF   E           K DISK
     FSTCTL1    IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  REMARK TYPE FILE
     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     CTKY          KLIST
     C                   KFLD                    CMRMCD
     C*
     C                   TIME                    DSDTTM
     C*
     C*   READ REMARK DETAIL RECORD
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CMDIFL        IFEQ      'Y'
     C     CMFL02        IFNE      'Y'
     C     CTKY          CHAIN     PCT01                              61
     C     *IN61         IFEQ      '0'
     C     CTDPRG        IFNE      *ZERO
     C                   MOVE      'Y'           CMFL02
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   UPDATE    PCM01
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   READ      PCM01                                  60
     C                   END                                                    ENDDOW *IN60
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
