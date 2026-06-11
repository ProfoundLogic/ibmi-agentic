     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX29681 - FIX EATIME IN WMEAP
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMEAL6    UF   E           K DISK

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
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAEMP#        KKEMP#
     C     *LIKE         DEFINE    EADIV         KKDIV
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EAPODT        KKPODT
     C     *LIKE         DEFINE    EADT          KKDT
     C     *LIKE         DEFINE    EATIME        KKTIME
     C*
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C*
     C                   READ      PEA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     COUNT         IFNE      *ZEROS
     C     EAFND         IFEQ      KKFND
     C     EAEMP#        ANDEQ     KKEMP#
     C     EADIV         ANDEQ     KKDIV
     C     EAFYR         ANDEQ     KKFYR
     C     EAPODT        ANDEQ     KKPODT
     C     EADT          ANDEQ     KKDT
     C     EATIME        ANDEQ     KKTIME
     C*
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   ADD       COUNT         EATIME
     C                   ADD       1             COUNT
     C*
     C                   ELSE
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAEMP#        KKEMP#
     C                   Z-ADD     EADIV         KKDIV
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EAPODT        KKPODT
     C                   Z-ADD     EADT          KKDT
     C                   Z-ADD     EATIME        KKTIME
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C                   ELSE
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAEMP#        KKEMP#
     C                   Z-ADD     EADIV         KKDIV
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EAPODT        KKPODT
     C                   Z-ADD     EADT          KKDT
     C                   Z-ADD     EATIME        KKTIME
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C                   READ      PEA01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
