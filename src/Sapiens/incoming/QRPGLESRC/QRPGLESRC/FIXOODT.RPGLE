     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXOODT - FIX OODT FROM FIXWMOOP
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/22/98  C:91  PROJECT SUPPORT(WA091/12339)        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMOOP     UF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*_______________________________________________________________

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    OODT          X7DATE
     C*_______________________________________________________________
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DATE
     C*_______________________________________________________________
     C*
     C                   READ      POO01                                  60
     C*
     C     *IN60         DOWEQ     *OFF                                         WH NOT EOF
     C*
     C     OOPGM         IFEQ      'FIXWMOOP'
     C     OODT          ANDGT     X7DATE
     C*
     C                   Z-ADD     X7DATE        OODT
     C                   Z-ADD     DSTIME        OOTIME
     C                   MOVEL(P)  Q1PGM         OOPGM
     C                   MOVEL(P)  Q1USER        OOUSER
     C*
     C                   UPDATE    POO01
     C*
     C                   ENDIF                                                  OOPGM=FIXWMOOP
     C*
     C                   READ      POO01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
