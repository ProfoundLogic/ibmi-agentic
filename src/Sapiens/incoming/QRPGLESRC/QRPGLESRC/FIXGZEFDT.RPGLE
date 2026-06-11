     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXGZEFDT - ASSIGN 1/1/1901 TO GZEFDT IF ZERO
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM ASSIGNS VALUE TO GZEFDT           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MCFIXCL                                                    *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 09/03/02  PROGRAMMER: C:79  PROJECT: FS244          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  READ  - PGZ01                                           *
     H*****************************************************************
     FFMGZP     UF   E             DISK
     F*

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*

     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C                   TIME                    DSDTTM
     C*
     C                   READ      PGZ01                                  99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C     GZEFDT        IFEQ      0
     C*
     C                   Z-ADD     0010101       GZEFDT
     C*
     C                   MOVEL(P)  Q1USER        GZUSER
     C                   MOVEL(P)  Q1PGM         GZPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        GZTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C*
     C                   UPDATE    PGZ01
     C*
     C                   ENDIF
     C*
     C* GET NEXT RECORD
     C*
     C                   READ      PGZ01                                  99
     C*
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C                   SETON                                        LR
