     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* UPDATE THE GENERIC TABLE DESCRIPTION FOR SELECTED IDS
     H**
     H* CREATED: 06/27/01 BY C:79 FOR MCMNT
     H**
     H*****************************************************************
     H*
     FSMCOP     IF   E           K DISK
     FFTGTL     UF   E           K DISK

     D*
     D GTDSC           S             31    DIM(1) CTDATA PERRCD(1)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** GET COMPANY #
     C                   READ      PCO01                                  90
     C*
     C                   Z-ADD     COCO#         KKCO#             3 0
     C                   MOVEL     'SVTY'        KKTBID
     C                   MOVEL     'HS'          KKKEY
     C*
     C*** PROCESS FTGTP
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  GTDSC(1)      GTDESC
     C*
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   UPDATE    PGT01
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   RETURN
** FTGTP DESCRIP **|*******************|*******************|
HOSPITAL SURGICAL - OUTPATIENT
