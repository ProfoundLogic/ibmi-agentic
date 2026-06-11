     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* UPDATE THE GENERIC TABLE FILE
     H**
     H* CREATED: 09/22/98   MIKE FLOWER
     H**
     H* LAST CHANGED FOR UPDATE TAPE: 05/10/99
     H*****************************************************************
     H*
     FSMCOP     IF   E             DISK
     FFTGTL     UF A E           K DISK
     FFTGIL     IF A E           K DISK

     D*
     D CSSO            S             10    DIM(2) CTDATA PERRCD(1)              KEY, DESC
     D ODES            S             40    DIM(2) ALT(CSSO)
     D TOUT            S             10    DIM(3) CTDATA PERRCD(1)              TBL ID
     D TDES            S             40    DIM(3) ALT(TOUT)
     D CSLD            S             10    DIM(3) CTDATA PERRCD(1)              TBL ID
     D DDES            S             40    DIM(3) ALT(CSLD)
     D CSSP            S             10    DIM(2) CTDATA PERRCD(1)              TBL ID
     D PDES            S             40    DIM(2) ALT(CSSP)
     D IDDS            S             40    DIM(4) CTDATA PERRCD(1)
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
     C     GIKEY         KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKTBID            4
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
     C                   MOVEL(P)  'CSSO'        KKTBID
     C*
     C     GIKEY         CHAIN     PGI01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL     IDDS(1)       GIDESC
     C                   MOVEL     'CSSO'        GITBID
     C                   MOVE      'A'           GISTAT
     C                   MOVEL(P)  'CODE'        GIFN01
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  'TOUT'        KKTBID
     C*
     C     GIKEY         CHAIN     PGI01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL     IDDS(2)       GIDESC
     C                   MOVEL     'TOUT'        GITBID
     C                   MOVE      'A'           GISTAT
     C                   MOVEL(P)  'CODE'        GIFN01
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  'CSLD'        KKTBID
     C*
     C     GIKEY         CHAIN     PGI01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL     IDDS(3)       GIDESC
     C                   MOVEL     'CSLD'        GITBID
     C                   MOVE      'A'           GISTAT
     C                   MOVEL(P)  'CODE'        GIFN01
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  'CSSP'        KKTBID
     C*
     C     GIKEY         CHAIN     PGI01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL     IDDS(4)       GIDESC
     C                   MOVEL     'CSSP'        GITBID
     C                   MOVE      'A'           GISTAT
     C                   MOVEL(P)  'CODE'        GIFN01
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C*
     C                   ENDIF
     C*
     C*** PROCESS FTGTP - CSSO
     C*
     C     1             DO        2             X                 2 0
     C*
     C                   MOVEL(P)  'CSSO'        KKTBID
     C                   MOVEL(P)  CSSO(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     ODES(X)       GTDESC
     C                   MOVEL     'CSSO'        GTTBID
     C                   MOVEL     CSSO(X)       GTKEY
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
     C*** PROCESS FTGTP - FORM
     C*
     C     1             DO        3             X                 2 0
     C*
     C                   MOVEL(P)  'TOUT'        KKTBID
     C                   MOVEL(P)  TOUT(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     TDES(X)       GTDESC
     C                   MOVEL     'TOUT'        GTTBID
     C                   MOVEL     TOUT(X)       GTKEY
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
     C*** PROCESS FTGTP - PROD
     C*
     C     1             DO        3             X                 2 0
     C*
     C                   MOVEL(P)  'CSLD'        KKTBID
     C                   MOVEL(P)  CSLD(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     DDES(X)       GTDESC
     C                   MOVEL     'CSLD'        GTTBID
     C                   MOVEL     CSLD(X)       GTKEY
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
     C*** PROCESS FTGTP - PROD
     C*
     C     1             DO        2             X                 2 0
     C*
     C                   MOVEL(P)  'CSSP'        KKTBID
     C                   MOVEL(P)  CSSP(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     PDES(X)       GTDESC
     C                   MOVEL     'CSSP'        GTTBID
     C                   MOVEL     CSSP(X)       GTKEY
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
** FTGTP KEYS & DESCRIPTIONS (CSSO, ODES)
A         BY CLAIMANT NAME
N         BY PDS CASE#
** FTGTP KEYS & DESCRIPTIONS (TOUT, TDES)
B         BOTH REPORT & FILE
F         FILE ONLY
R         REPORT ONLY
** FTGTP KEYS & DESCRIPTIONS (CSLD, DDES)
B         BOTH PRIMARY & SECONDARY CASE LOADS
1         PRIMARY CASE LOAD ONLY
2         SECONDARY CASE LOAD ONLY
** FTGTP KEYS & DESCRIPTIONS (CSSP, PDES)
S         STAFF CODE
SG        SUB/GROUP
** FTGTP KEYS & DESCRIPTIONS (IDDS)
CASE REPORT SORT ORDER
TYPE OF OUTPUT
CASE LOAD REPORT OPTIONS
CASE STATUS SORT PRIMARY
