     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: WRTFDCAP2
     H*    DESCRIPTION: THIS PROGRAM WRITES THE EOB# INTO THE CHECK
     H*                 NUMBER FIELD IF THE BILL HAS BEEN COMPLETED.
     H*                 THIS PROGRAM WILL BE RUN ONE TIME FOR TASB
     H*                 FOR TESTING OF NEW PROVIDER BILLS PROJECT.
     H*
     H*****************************************************************
     FFAEOP     IF   E             DISK
     FFDCALH    UF   E           K DISK

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
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C*
     C                   READ      PEO01                                  99
     C     *IN99         DOWEQ     *OFF
     C     EOBLST        IFEQ      'C'
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C     CAKY          CHAIN     PCA01                              98
     C     *IN98         DOWEQ     *OFF
     C                   Z-ADD     CAEOB#        CACHK#
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
     C     CAKY          READE     PCA01                                  98
     C                   ENDDO                                                  *IN98
     C*
     C                   ENDIF                                                  EOBLST = 'C'
     C                   READ      PEO01                                  99
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
