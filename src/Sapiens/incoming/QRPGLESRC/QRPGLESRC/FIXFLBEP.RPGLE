     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*******************************************************************
     H/TITLE FIXFLBEP - CORRECT FLORIDA BEAST RECORD
     H*******************************************************************
     H* Created on 10/06/2003 by c:62 for Project FL045
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*******************************************************************
     FFMBEL     UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     BEKY          KLIST
     C                   KFLD                    KKAST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKSTYP
     C*
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C*
     C                   MOVEL(P)  'FL'          KKAST
     C                   MOVEL(P)  'H'           KKSYS
     C                   MOVEL(P)  'H'           KKBTYP
     C                   MOVEL     *BLANKS       KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  'F0363RFL'    BEDEPG
     C                   MOVEL(P)  'F0463RFL'    BEEDDT
     C                   MOVEL(P)  Q1USER        BEUSER
     C                   MOVEL(P)  Q1PGM         BEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BEDT
     C                   Z-ADD     DSTIME        BETIME
     C                   UPDATE    PBE01
     C                   ENDIF
     C*
     C                   MOVEL(P)  'FL'          KKAST
     C                   MOVEL(P)  'P'           KKSYS
     C                   MOVEL(P)  'N'           KKBTYP
     C                   MOVEL     *BLANKS       KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  'F0453RFL'    BEEDDT
     C                   MOVEL(P)  Q1USER        BEUSER
     C                   MOVEL(P)  Q1PGM         BEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BEDT
     C                   Z-ADD     DSTIME        BETIME
     C                   UPDATE    PBE01
     C                   ENDIF
     C*
     C                   SETON                                        LR
