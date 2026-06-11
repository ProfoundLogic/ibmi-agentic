     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWMAU002 - FIX AUDIT RECEIVED DATES                          R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     Updates audit received date with audit print date if      *
     H*     audit recieved date is not entered.                       *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:61                                            *
     H*   CREATED:    04/27/01    support wa112 27242                 *
     H*****************************************************************
      *
     FWMAUP     UF   E             DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
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
      *
     C                   READ      PAU01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     AURCVD        IFEQ      *ZERO
     C     AUPRDT        ANDNE     *ZERO
     C                   Z-ADD     AUPRDT        AURCVD
      *
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C                   MOVEL     DSTIME        AUTIME
      *
     C                   UPDATE    PAU01
     C                   ENDIF
      *
     C                   READ      PAU01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   MOVE      *ON           *INLR
      *
