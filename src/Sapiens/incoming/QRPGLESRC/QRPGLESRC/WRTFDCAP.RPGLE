     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: WRTFDCAP
     H*    DESCRIPTION: THIS PROGRAM WITH COPY THE VALUES OF THE OLD
     H*                 AMOUNT FIELDS INTO THE NEW AMOUNT FIELDS
     H*
     H*                 CATOTC -> CACHRG
     H*                 CATOTP -> CAPAID
     H*                 CATOTD -> CAMRA
     H*                 CATOTA -> CAMRAA
     H*
     H*****************************************************************
     FFDCAP     UF   E             DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   READ      PCA01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     CATOTC        CACHRG
     C                   Z-ADD     CATOTP        CAPAID
     C                   Z-ADD     CATOTD        CAMRA
     C                   Z-ADD     CATOTA        CAMRAA
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   UPDATE    PCA01
     C                   READ      PCA01                                  99
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
