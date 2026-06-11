     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* FIXPNAM
     H***************************************************************
     H*  CREATED: 06/05/00  C:61  SUPPORT DR109 23102
     H***************************************************************
      *
      *  DARTS file SDCMP:
      *  Change all records with CMPNAM = W0171R to have CMPNAM = W0050R
      *
      *
     FSDCMP     UF   E             DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=====================================================================
      *

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *
     C                   TIME                    DSDTTM
      *
      *  Change all records with CMPNAM = W0171R to have CMPNAM = W0050R
      *
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
      *
     C     CMPNAM        IFEQ      'W0171R'
     C                   MOVEL(P)  'W0050R'      CMPNAM
      *
     C                   MOVEL(P)  Q1USER        CMUSER
     C                   MOVEL(P)  Q1PGM         CMPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
      *
     C                   MOVEL     DSTIME        CMTIME
      *
     C                   UPDATE    PCM01
     C                   ENDIF
      *
     C                   READ      PCM01                                  60
     C                   ENDDO
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
