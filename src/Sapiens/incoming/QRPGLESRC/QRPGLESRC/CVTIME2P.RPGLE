     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CVTIME2P - CREATES IME2P RECORDS                         *
     H*****************************************************************
     H* CREATED ON 06/12/2003 BY C:62 FOR PROJECT 114MEX
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWMFNP     IF   E             DISK
     FIME2P     O    E             DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDY(YY)
     C                   PARM      '*CYMD'       SRTFMT            8            YYMD
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PFN01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   CLEAR                   PE201
     C                   Z-ADD     FNFND         E2FND
     C                   MOVEL(P)  'MMREV'       E2RMCD
     C                   MOVEL(P)  'R'           E2AACT
     C                   MOVEL(P)  'A'           E2STAT
     C*
     C                   MOVEL(P)  Q1USER        E2USER
     C                   MOVEL(P)  Q1USER        E2IUSR
     C                   MOVEL     Q1PGM         E2PGM
     C                   Z-ADD     DSTIME        E2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E2DT
     C*
     C                   WRITE     PE201
     C*
     C                   READ      PFN01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
