     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXSMORP - Update record for W0164R
     H*****************************************************************
     H*                                                               *
     H*  09/02/05  C:61  PROJECT WC1359  129WLT                       *
     H*                  Created.                                     *
     H*                                                               *
     H*****************************************************************
      *
     FSMORL     IF A E           K DISK
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
      *===========================================================
      *  KLIST
      *===========================================================
      *
      *  Output Routing
      *

     C     ORKY          KLIST
     C                   KFLD                    XXPGM            10
     C                   KFLD                    XXFIL#            1
      *
      *
      *===========================================================
      *  *LIKE DEFN
      *===========================================================
      *
      *
      *===========================================================
      *  PLIST
      *===========================================================
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *===========================================================
      *  MAIN
      *===========================================================
     C                   MOVEL(P)  'W0164R'      XXPGM            10
     C                   MOVE      '0'           XXFIL#            1
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '4'           XXFIL#            1
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  'W0164R'      XXPGM            10
     C                   MOVE      '0'           XXFIL#            1
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '4'           ORFIL#            1
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        ORUSER
     C                   MOVE      Q1PGM         ORPGM
     C                   Z-ADD     DSTIME        ORTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ORDT
      *
     C                   WRITE     POR01
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
      *
