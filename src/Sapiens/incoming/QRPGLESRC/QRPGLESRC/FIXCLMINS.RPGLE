     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXCLMINS - LOAD SELECTED FIELDS FOR ALL CLAIM FILES
     FWTNCP     UF   E           K DISK
     FWTNAP     UF   E           K DISK
     FWTPAP     UF   E           K DISK
     FWTCAP     UF   E           K DISK
     FWTRTP     UF   E           K DISK
     FWTPCP     UF   E           K DISK
     FWTB2P     UF   E           K DISK
     FWTB7P     UF   E           K DISK
     FWTLRP     UF   E           K DISK
     FWTRCP     UF   E           K DISK
     FWTRSP     UF   E           K DISK
     FWTSPP     UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO1             3
     C                   PARM                    QQCO2             3
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
     C                   MOVE      QQCO1         XXCO1             3 0
     C                   MOVE      QQCO2         XXCO2             3 0
     C                   TIME                    DSDTTM
     C*
     C                   READ      PNC01                                  60
     C     *IN60         DOWEQ     '0'
     C     NCCO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         NCCO#
     C                   MOVEL     Q1USER        NCUSER
     C                   MOVEL     Q1PGM         NCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NCDT
     C                   Z-ADD     DSTIME        NCTIME
     C                   UPDATE    PNC01
     C                   END
     C                   READ      PNC01                                  60
     C                   END
     C*
     C                   READ      PNA01                                  60
     C     *IN60         DOWEQ     '0'
     C     NACO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         NACO#
     C                   MOVEL     Q1USER        NAUSER
     C                   MOVEL     Q1PGM         NAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C                   Z-ADD     DSTIME        NATIME
     C                   UPDATE    PNA01
     C                   END
     C                   READ      PNA01                                  60
     C                   END
     C*
     C                   READ      PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C     PACO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         PACO#
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   Z-ADD     DSTIME        PATIME
     C                   UPDATE    PPA01
     C                   END
     C                   READ      PPA01                                  60
     C                   END
     C*
     C                   READ      PCA01                                  60
     C     *IN60         DOWEQ     '0'
     C     CACO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         CACO#
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C                   UPDATE    PCA01
     C                   END
     C                   READ      PCA01                                  60
     C                   END
     C*
     C                   READ      PRT01                                  60
     C     *IN60         DOWEQ     '0'
     C     RTCO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         RTCO#
     C                   MOVEL     Q1USER        RTUSER
     C                   MOVEL     Q1PGM         RTPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RTDT
     C                   Z-ADD     DSTIME        RTTIME
     C                   UPDATE    PRT01
     C                   END
     C                   READ      PRT01                                  60
     C                   END
     C*
     C                   READ      PPC01                                  60
     C     *IN60         DOWEQ     '0'
     C     PCCO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         PCCO#
     C                   MOVEL     Q1USER        PCUSER
     C                   MOVEL     Q1PGM         PCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C                   Z-ADD     DSTIME        PCTIME
     C                   UPDATE    PPC01
     C                   END
     C                   READ      PPC01                                  60
     C                   END
     C*
     C                   READ      PB201                                  60
     C     *IN60         DOWEQ     '0'
     C     B2CO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         B2CO#
     C                   MOVEL     Q1USER        B2USER
     C                   MOVEL     Q1PGM         B2PGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B2DT
     C                   Z-ADD     DSTIME        B2TIME
     C                   UPDATE    PB201
     C                   END
     C                   READ      PB201                                  60
     C                   END
     C*
     C                   READ      PB701                                  60
     C     *IN60         DOWEQ     '0'
     C     B7CO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         B7CO#
     C                   MOVEL     Q1USER        B7USER
     C                   MOVEL     Q1PGM         B7PGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B7DT
     C                   Z-ADD     DSTIME        B7TIME
     C                   UPDATE    PB701
     C                   END
     C                   READ      PB701                                  60
     C                   END
     C*
     C                   READ      PLR01                                  60
     C     *IN60         DOWEQ     '0'
     C     LRCO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         LRCO#
     C                   MOVEL     Q1USER        LRUSER
     C                   MOVEL     Q1PGM         LRPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LRDT
     C                   Z-ADD     DSTIME        LRTIME
     C                   UPDATE    PLR01
     C                   END
     C                   READ      PLR01                                  60
     C                   END
     C*
     C                   READ      PRC01                                  60
     C     *IN60         DOWEQ     '0'
     C     RCCO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         RCCO#
     C                   MOVEL     Q1USER        RCUSER
     C                   MOVEL     Q1PGM         RCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C                   Z-ADD     DSTIME        RCTIME
     C                   UPDATE    PRC01
     C                   END
     C                   READ      PRC01                                  60
     C                   END
     C*
     C                   READ      PRS01                                  60
     C     *IN60         DOWEQ     '0'
     C     RSCO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         RSCO#
     C                   MOVEL     Q1USER        RSUSER
     C                   MOVEL     Q1PGM         RSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RSDT
     C                   Z-ADD     DSTIME        RSTIME
     C                   UPDATE    PRS01
     C                   END
     C                   READ      PRS01                                  60
     C                   END
     C*
     C                   READ      PSP01                                  60
     C     *IN60         DOWEQ     '0'
     C     SPCO#         IFEQ      XXCO1
     C                   Z-ADD     XXCO2         SPCO#
     C                   MOVEL     Q1USER        SPUSER
     C                   MOVEL     Q1PGM         SPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SPDT
     C                   Z-ADD     DSTIME        SPTIME
     C                   UPDATE    PSP01
     C                   END
     C                   READ      PSP01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
