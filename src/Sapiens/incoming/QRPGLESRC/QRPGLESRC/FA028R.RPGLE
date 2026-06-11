     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA028R - WRITE/UPDATE WCMC CLAIM MASTER INFO
     H*****************************************************************
     H*                                                               *
     H* CREATED: 04/12/99  C:61  PROJECT 96WCC                        *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS                                                    *
     H*                                                               *
     H* 60 - 61    GENERAL USAGE (CHAIN)                              *
     H*                                                               *
     H*****************************************************************
      *
      *
     FFMCML1    UF   E           K DISK
     F                                     INFDS(DBFDS1)
      *
      *
     FFMFNL1    IF   E           K DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
      *
     D                 DS
     D  XXSCL#                 1     25
     D  XXSCLX                17     23
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *  CLAIM MASTER KEY LIST
      *

     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
      *
      *
      *
     C     *ENTRY        PLIST
P01  C                   PARM                    @@RTCD            1            Return Code
P02  C                   PARM                    @@CLT             6            Client
P03  C                   PARM                    @@PROC            3            Processing
P04  C                   PARM                    @@CO#             3
P05  C                   PARM                    @@SUB#            5
P06  C                   PARM                    @@FND             3
P07  C                   PARM                    @@FYR             3
P08  C                   PARM                    @@CASE            7
      *
P27  C                   PARM                    @@CMNU            3            NURSE
      *
      *
      *
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      * Initialize
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@SUB#        XXSUB#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
      *
     C                   MOVE      *OFF          @@RTCD
      *
      * Get Subscriber number
      *
     C     @@SUB#        IFEQ      *BLANKS
     C     FNKY          CHAIN     PFN01                              61
      *
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXFND
     C                   EXSR      CLMUPD
      *
     C                   ELSE
      *
     C                   MOVE      *ON           @@RTCD
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   EXSR      CLMUPD
     C                   ENDIF
      *
      *  END THE PROGRAM
      *
     C                   SETON                                        LR
      *
      *
      *
      *****************************************************************
      *    SUBROUTINE - CLMUPD - CLAIM UPDATE
      *****************************************************************
     C     CLMUPD        BEGSR
      *
      * Get Claim Master; If record found then update
      *
     C     CMKEY         CHAIN     PCM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     Q1PRMS        ANDGT     8
      *
     C                   SELECT
      **********
      * CLT096
      **********
      *    PROC = 001
      *
     C     @@CLT         WHENEQ    'CLT096'
     C     @@PROC        ANDEQ     '001'
     C                   MOVEL     @@CMNU        CMCMNU
      *
      **********
      * CLT0XX
      **********
      *
      *
      *
      *
      **********
      * OTHER
      **********
     C                   OTHER
     C                   MOVE      *ON           @@RTCD
      *
     C                   ENDSL
      *
     C                   ELSE
      *
     C                   MOVE      *ON           @@RTCD
     C                   ENDIF                                                  *IN60
      *
      *   UPDATE RECORD
      *
     C     @@RTCD        IFEQ      *OFF
      *
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
      *
     C                   UPDATE    PCM01
     C                   ENDIF
      *
     C                   ENDSR
      *
