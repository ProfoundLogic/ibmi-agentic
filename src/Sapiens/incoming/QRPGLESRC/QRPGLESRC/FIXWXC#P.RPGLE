     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWXC#P - Initialize WCPOLS CX/Rein Transaction File
     H*****************************************************************
     H*                                                               *
     H*   Load Sequence number for each co#/fnd/emp#/fyr/mldt         *
     H*                                                               *
     H*   CALLED FROM: COMMAND ENTRY                                  *
     H*                                                               *
     H*   CALLS TO:    SRCVTDAT                                       *
     H*                                                               *
     H*   PROGRAMMER: C:61                                            *
     H*   CREATED:    06/13/01    PROJECT: WC1038                     *
     H*                                                               *
     H*****************************************************************
      *
     FWXC#L2    UF   E           K DISK
      *
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
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
     C     *LIKE         DEFINE    CRCO#         SVCO#
     C     *LIKE         DEFINE    CRFND         SVFND
     C     *LIKE         DEFINE    CREMP#        SVEMP#
     C     *LIKE         DEFINE    CRFYR         SVFYR
     C     *LIKE         DEFINE    CRMLDT        SVMLDT
     C     *LIKE         DEFINE    CRSEQ#        XXSEQ#
      *
     C                   Z-ADD     0             SVCO#
     C                   Z-ADD     0             SVFND
     C                   Z-ADD     0             SVEMP#
     C                   Z-ADD     0             SVFYR
     C                   Z-ADD     0             SVMLDT
      *
     C                   READ      PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     CRSEQ#        IFEQ      *ZERO
      *
     C     CRCO#         IFNE      SVCO#
     C     CRFND         ORNE      SVFND
     C     CREMP#        ORNE      SVEMP#
     C     CRFYR         ORNE      SVFYR
     C     CRMLDT        ORNE      SVMLDT
     C                   Z-ADD     CRCO#         SVCO#
     C                   Z-ADD     CRFND         SVFND
     C                   Z-ADD     CREMP#        SVEMP#
     C                   Z-ADD     CRFYR         SVFYR
     C                   Z-ADD     CRMLDT        SVMLDT
     C                   Z-ADD     0             XXSEQ#
     C                   ENDIF
      *
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        CRSEQ#
      *
      *
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C                   MOVEL     DSTIME        CRTIME
      *
     C                   UPDATE    PCR01
     C                   ENDIF                                                  CRSEQ# = 0
      *
     C                   READ      PCR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   MOVE      *ON           *INLR
      *
      *
