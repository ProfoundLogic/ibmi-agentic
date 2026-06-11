     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXEHTIME - FIX EHTIME IN WDEHP
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDEHL6    UF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    EHCO#         KKCO#
     C     *LIKE         DEFINE    EHFND         KKFND
     C     *LIKE         DEFINE    EHEMP#        KKEMP#
     C     *LIKE         DEFINE    EHDIV         KKDIV
     C     *LIKE         DEFINE    EHFYR         KKFYR
     C     *LIKE         DEFINE    EHEFDT        KKEFDT
     C     *LIKE         DEFINE    EHDT          KKDT
     C     *LIKE         DEFINE    EHTIME        KKTIME
     C*
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C*
     C                   READ      PEH01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     COUNT         IFNE      *ZEROS
     C     EHFND         IFEQ      KKFND
     C     EHEMP#        ANDEQ     KKEMP#
     C     EHDIV         ANDEQ     KKDIV
     C     EHFYR         ANDEQ     KKFYR
     C     EHEFDT        ANDEQ     KKEFDT
     C     EHDT          ANDEQ     KKDT
     C     EHTIME        ANDEQ     KKTIME
     C*
     C                   MOVEL     Q1PGM         EHPGM
     C                   ADD       COUNT         EHTIME
     C                   ADD       1             COUNT
     C*
     C                   ELSE
     C                   Z-ADD     EHFND         KKFND
     C                   Z-ADD     EHEMP#        KKEMP#
     C                   Z-ADD     EHDIV         KKDIV
     C                   Z-ADD     EHFYR         KKFYR
     C                   Z-ADD     EHEFDT        KKEFDT
     C                   Z-ADD     EHDT          KKDT
     C                   Z-ADD     EHTIME        KKTIME
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C                   ELSE
     C                   Z-ADD     EHFND         KKFND
     C                   Z-ADD     EHEMP#        KKEMP#
     C                   Z-ADD     EHDIV         KKDIV
     C                   Z-ADD     EHFYR         KKFYR
     C                   Z-ADD     EHEFDT        KKEFDT
     C                   Z-ADD     EHDT          KKDT
     C                   Z-ADD     EHTIME        KKTIME
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C                   UPDATE    PEH01
     C                   READ      PEH01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
