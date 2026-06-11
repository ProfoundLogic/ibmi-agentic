     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* FXCAGLAG: SET CAFLAG TO 'P' OF BILL STATUS IS X OR D          *
     H*****************************************************************
     FFAEOP     IF   E             DISK
     FFDCAL13   UF   E           K DISK

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
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*
     C                   READ      PEO01                                  99
     C     *IN99         DOWEQ     *OFF
     C     EOBLST        IFEQ      'X'
     C     EOBLST        OREQ      'D'
     C     EOBLST        OREQ      'C'
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C     CAKY          CHAIN     PCA01                              98
     C     *IN98         DOWEQ     *OFF
     C*
     C     EOBLST        IFEQ      'C'
     C                   MOVE      ' '           CAFLAG
     C                   ELSE
     C                   MOVE      'P'           CAFLAG
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  98
     C                   ENDDO
     C                   ENDIF
     C                   READ      PEO01                                  99
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
