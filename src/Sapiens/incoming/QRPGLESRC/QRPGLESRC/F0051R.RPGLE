     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*
     H*  PROGRAM: F0051R - Convert NDC'S Drug Code File
     H*
     H*****************************************************************
     H*  Created on 08/08/2007 by C:191 for Project 45MGA
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  04/25/18  C:180 PROJECT: 114MJS
     H*                  Recompile only.
     H*
     H*****************************************************************
     FFXRBP     IF   E             DISK
     FFMRXL     UF A E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* Constants
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
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
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     RXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNDC
     C*
     C     *LIKE         DEFINE    RXCO#         KKCO#
     C     *LIKE         DEFINE    RXNDC         KKNDC
     C*
     C*  Initialization Section
     C*
     C                   MOVE      @@CO#         KKCO#
     C*
     C                   READ      PRB01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  RBNDC5        KKNDC
     C*
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      UPDATR
     C                   ELSE
     C                   EXSR      WRITER
     C                   ENDIF
     C*
     C                   READ      PRB01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C***************************************************************
     C* UPDATE FMRXP
     C***************************************************************
     C     UPDATR        BEGSR
     C*
     C                   Z-ADD     KKCO#         RXCO#
     C                   MOVEL(P)  RBNDC5        RXNDC
     C                   MOVEL(P)  RBPCAT        RXPTYP
     C                   MOVEL(P)  RBPNAM        RXNAME
     C                   MOVEL(P)  RBFMCD        RXFORM
     C*
     C                   SELECT
     C     RBQUAN        WHENEQ    'EA'
     C                   MOVEL(P)  'EA'          RXSTRN
     C     RBQUAN        WHENEQ    'DOZ'
     C                   MOVEL(P)  'DZ'          RXSTRN
     C                   ENDSL
     C*
     C     LO:HI         XLATE     RBUNIT        RXMEAS
     C                   MOVE(P)   RBSIZE        RXPSZE
     C                   MOVEL(P)  RBDOSE        RXUNTD
     C                   MOVE      RBAWED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         RXAWPD
     C                   MOVE      RBACUP        RXAWP
     C                   MOVE      RBAFPU        RX1AWP
     C                   MOVEL     RBPN5         RXONDC
     C                   MOVEL(P)  RBSDSC        RXSTRG
     C*
     C***** MAINTENANCE FIELDS ****************************************
     C*
     C                   MOVEL     'A'           RXSTAT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RXTIME
     C                   MOVEL(P)  Q1USER        RXUSER
     C                   MOVEL(P)  Q1PGM         RXPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXDT
     C*
     C                   UPDATE    PRX01
     C*
     C                   ENDSR
     C***************************************************************
     C* WRITES FMRXP
     C***************************************************************
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   PRX01
     C*
     C                   Z-ADD     KKCO#         RXCO#
     C                   MOVEL(P)  RBNDC5        RXNDC
     C                   MOVEL(P)  RBPCAT        RXPTYP
     C                   MOVEL(P)  RBPNAM        RXNAME
     C                   MOVEL(P)  RBFMCD        RXFORM
     C*
     C                   SELECT
     C     RBQUAN        WHENEQ    'EA'
     C                   MOVEL(P)  'EA'          RXSTRN
     C     RBQUAN        WHENEQ    'DOZ'
     C                   MOVEL(P)  'DZ'          RXSTRN
     C                   ENDSL
     C*
     C     LO:HI         XLATE     RBUNIT        RXMEAS
     C                   MOVE(P)   RBSIZE        RXPSZE
     C                   MOVEL(P)  RBDOSE        RXUNTD
     C                   MOVE      RBAWED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         RXAWPD
     C                   MOVE      RBACUP        RXAWP
     C                   MOVE      RBAFPU        RX1AWP
     C                   MOVEL     RBPN5         RXONDC
     C                   MOVEL(P)  RBSDSC        RXSTRG
     C*
     C***** MAINTENANCE FIELDS ****************************************
     C*
     C                   MOVEL     'A'           RXSTAT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RXTIME
     C                   MOVEL(P)  Q1USER        RXUSER
     C                   MOVEL(P)  Q1USER        RXIUSR
     C                   MOVEL(P)  Q1PGM         RXPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXDT
     C                   MOVEL     SRDTO         RXIDT
     C*
     C                   WRITE     PRX01
     C*
     C                   ENDSR
