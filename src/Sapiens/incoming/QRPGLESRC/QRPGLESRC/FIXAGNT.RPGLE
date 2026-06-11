     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXAGNT - AGENT FIX - COPIES AGSTF1 TO AGMKRP
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMAGP     UF   E           K DISK

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
     C     *LIKE         DEFINE    AGDT          XXDATE
     C*
     C                   READ      PAG01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL     AGSTF1        AGMKRP
     C                   MOVEL     Q1USER        AGUSER
     C                   MOVEL     Q1PGM         AGPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   Z-ADD     XXDATE        AGDT
     C                   Z-ADD     DSTIME        AGTIME
     C                   UPDATE    PAG01
     C                   READ      PAG01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
