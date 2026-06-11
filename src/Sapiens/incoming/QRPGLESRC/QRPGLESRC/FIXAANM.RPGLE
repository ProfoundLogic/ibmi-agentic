     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXAANM - AGENT NAME FIX COPIES AGNAME -> AANAME
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 02/03/98  C:77  PROJECT SUPPORT(WA093 #10155)       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMAGL     IF   E           K DISK
     FWMAAP     UF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* AGENT MASTER FILE SEARCH KEY
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    AADT          XXDATE
     C     *LIKE         DEFINE    AACO#         XXCO#
     C     *LIKE         DEFINE    AAFEIN        XXFEIN
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C*
     C                   READ      PAA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     AANAME        IFEQ      *BLANK
     C                   Z-ADD     AACO#         XXCO#
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C*
     C     AGKY          CHAIN     PAG01                              61
     C*
     C     *IN61         IFEQ      '0'
     C                   MOVEL     AGNAME        AANAME
     C                   MOVEL     Q1USER        AAUSER
     C                   MOVEL     Q1PGM         AAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   Z-ADD     XXDATE        AADT
     C                   Z-ADD     DSTIME        AATIME
     C                   UPDATE    PAA01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READ      PAA01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
