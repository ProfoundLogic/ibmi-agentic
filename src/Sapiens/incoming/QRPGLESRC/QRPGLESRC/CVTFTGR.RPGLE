     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTFTGR - CONVERT GENERIC TABLE ENTRIES
     H**************************************************************************
     FFTGTP     UF   E           K DISK
     FFTGIP     UF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    GIDT          XXDATE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C* READ THE TABLE FILE
     C*
     C                   READ      PGT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     'ZZ'          GTTBID
     C                   MOVE      'SC'          GTTBID
     C                   MOVE(P)   Q1PGM         GTPGM
     C                   MOVEL     Q1USER        GTUSER
     C                   Z-ADD     XXDATE        GTDT
     C                   Z-ADD     DSTIME        GTTIME
     C                   UPDATE    PGT01
     C*
     C                   READ      PGT01                                  60
     C                   END
     C*
     C*
     C* READ THE ITEM FILE
     C*
     C                   READ      PGI01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     'ZZ'          GITBID
     C                   MOVE      'SC'          GITBID
     C                   MOVE(P)   Q1PGM         GIPGM
     C                   MOVEL     Q1USER        GIUSER
     C                   Z-ADD     XXDATE        GIDT
     C                   Z-ADD     DSTIME        GITIME
     C                   UPDATE    PGI01
     C*
     C                   READ      PGI01                                  60
     C                   END
     C*
     C* END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
