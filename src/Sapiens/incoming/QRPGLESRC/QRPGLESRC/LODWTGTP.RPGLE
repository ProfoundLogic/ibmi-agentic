     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H* UPDATE THE GENERIC TABLE FILE
     H**
     H* CREATED: 02/01/10   C: 247
     H**
     H*****************************************************************
     H*
     FSMCOP     IF   E             DISK
     FWTGTL     UF A E           K DISK

     D*
     D PROD            S             10    DIM(4) CTDATA PERRCD(1)              KEY, DESC
     D PDES            S             40    DIM(4) ALT(PROD)
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
     C     GTKEY1        KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKTBID            4
     C                   KFLD                    KKKEY            10
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C*** GET COMPANY #
     C*
     C                   READ      PCO01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     COCO#         KKCO#
     C*
     C*** PROCESS WTGTP - AXCI
     C*
     C                   MOVEL(P)  'AXCI'        KKTBID
     C                   MOVEL(P)  '09'          KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   CLEAR                   PGT01
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     'Death'       GTDESC
     C                   MOVEL     'AXCI'        GTTBID
     C                   MOVEL     '09'          GTKEY
     C                   MOVE      'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*
     C                   ENDIF
     C*
     C*** PROCESS WTGTP - AXKY
     C*
     C     1             DO        4             X                 2 0
     C*
     C                   MOVEL(P)  'AXKY'        KKTBID
     C                   MOVEL(P)  PROD(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     'AXKY'        GTTBID
     C                   MOVEL     PROD(X)       GTKEY
     C                   MOVEL     PDES(X)       GTDESC
     C                   MOVE      'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   READ      PCO01                                  99
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
** FTGTP KEYS & DESCRIPTIONS (PROD, PDES)
CLMRETEN  Percentage of Group Retention
#WEEKS    Number of weeks paid
CLMNCCI   NCCI codes
PAYMENT2  Payment Auto DARTS 2
