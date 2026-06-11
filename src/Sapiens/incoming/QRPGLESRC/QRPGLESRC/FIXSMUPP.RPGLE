     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXSMUPP- UPDATES STATUS FROM ' ' TO 'A'
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 12/22/00  C:61                                      *
     H*                                                               *
     H*****************************************************************
      *
     FSMUPP     UF   E           K DISK
      *
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
      *

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
     C                   READ      PUP01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     UPSTAT        IFEQ      *BLANK
     C                   MOVE      'A'           UPSTAT
      *
     C                   MOVEL     Q1USER        UPUSER
     C                   MOVEL     Q1PGM         UPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        UPTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
      *
     C                   UPDATE    PUP01
     C                   ENDIF
      *
     C                   READ      PUP01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
      *
