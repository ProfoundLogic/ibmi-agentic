     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXEHTIME - FIX TIME FIELD ON WDEHP RECORDS
     H*****************************************************************
     H*
     H*
     H*****************************************************************
     FWDEHL11   UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHTIME        XXTIME
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   READ      PEH01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     EHCO#         IFNE      XXCO#
     C     EHFND         ORNE      XXFND
     C     EHFYR         ORNE      XXFYR
     C     EHEMP#        ORNE      XXEMP#
     C     EHDIV         ORNE      XXDIV
     C*
     C                   Z-ADD     EHCO#         XXCO#
     C                   Z-ADD     EHFND         XXFND
     C                   Z-ADD     EHFYR         XXFYR
     C                   Z-ADD     EHEMP#        XXEMP#
     C                   Z-ADD     EHDIV         XXDIV
     C                   Z-ADD     EHTIME        XXTIME
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     XXTIME        EHTIME
     C*
     C                   UPDATE    PEH01
     C*
     C                   ADD       1             XXTIME
     C*
     C                   READ      PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
