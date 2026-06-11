     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - WA035R - UPDATE WCMS W/ WCMC: CLAIM MASTER INFO               A
     H*****************************************************************
     H*                                                               *
     H*    CREATED: 04/12/99  C:61  Proj: 96WCC                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 00/00/00 C:00 Req/Proj:                                       *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*  60         GENERAL USAGE (CHAIN)                             *
     H*                                                               *
     H*****************************************************************
      *
     FWMCML1    UF   E           K DISK
     F                                     INFDS(DBFDS1)
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D DBFDS1          DS
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1            WCMS fields
     C                   PARM                    @@CLT             6            Client
     C                   PARM                    @@PROC            3            Processing
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
      *
     C                   PARM                    @@NRSE            3            NURSE
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
      *  CLAIM MASTER KEY LIST
      *
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
      *  FUND MASTER KEY LIST
      *
      *
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
      *
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#            5 0
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
      *
     C                   MOVE      KKCO#         ##CO#
     C                   MOVE      KKSUB#        ##SUB#
     C                   MOVE      KKFND         ##FND
      *
     C                   MOVE      *OFF          @@RTCD
      *
      *
     C                   CALL      'FA700R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##WCF#            3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
      *
     C                   MOVE      ##WCF#        KKWCF#            3 0
      *
      *   GET CLAIM MASTER AND UPDATE
      *
     C     CMKY          CHAIN     PCM01                              99
      *
      *   MOVE FIELDS FOR UPDATE
      *
     C     *IN99         IFEQ      '0'
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
     C                   MOVEL(P)  @@NRSE        CMNRSE
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
      *
     C                   ENDSL
      *
      *    Update record
      *
     C     @@RTCD        IFEQ      *OFF
      *
     C                   MOVE(P)   Q1USER        CMUSER
     C                   MOVE(P)   Q1PGM         CMPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSTIME        CMTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
      *
     C                   UPDATE    PCM01
     C                   ENDIF                                                  @@RTCD = *OFF
      *
     C                   ELSE
      *
     C                   MOVE      *ON           @@RTCD
     C                   ENDIF                                                  *IN99 IFEQ 0
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
      *
     C                   MOVE      '1'           *INLR
      *
