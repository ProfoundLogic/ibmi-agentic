     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* FXCARDAT: WRITE ENTERED DATE WITH CAIDT                       *
     H*****************************************************************
     FFDCAP     UF   E             DISK

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
     C     *LIKE         DEFINE    CAIDT         XXDATE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   READ      PCA01                                  99
     C     *IN99         DOWEQ     *OFF
     C     CARDAT        IFEQ      *ZEROS
     C                   Z-ADD     CAIDT         CARDAT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   MOVEL     Q1USER        CAUSER
     C                   UPDATE    PCA01
     C                   ELSE
     C                   EXCEPT    PCA0A
     C                   ENDIF                                                  *IN99 = *OFF
     C                   READ      PCA01                                  99
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     OPCA01     E            PCA0A
