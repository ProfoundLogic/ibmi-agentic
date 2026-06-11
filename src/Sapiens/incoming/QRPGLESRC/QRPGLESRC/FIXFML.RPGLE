     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXFML  - FIX THE FSN, LSN, MID AND AST       C:62 12/22/99  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFMCML1    IF   E           K DISK
     FFAEOP     UF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CMKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C                   READ      PEO01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     EOLSN         ANDEQ     *BLANKS
     C*
     C                   MOVEL(P)  CMFSN         EOFSN
     C                   MOVEL(P)  CMMID         EOMID
     C                   MOVEL(P)  CMLSN         EOLSN
     C                   MOVEL(P)  CMAST         EOAST
     C*
     C                   UPDATE    PEO01
     C*
     C                   ELSE
     C*
     C                   UPDATE    PEO01
     C*
     C                   ENDIF
     C*
     C                   READ      PEO01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
