     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSMCOP     IF   E           K DISK
     FSTGTL     UF A E           K DISK
     FSTGIL     UF A E           K DISK

     D*
     D ITID            S              4    DIM(15) CTDATA PERRCD(1)             TBL ID, DESC
     D IDES            S             40    DIM(15) ALT(ITID)
     D TKEY            S             10    DIM(50) CTDATA PERRCD(1)             KEY, DESC
     D TDES            S             40    DIM(50) ALT(TKEY)
     D TTID            S              4    DIM(50) CTDATA PERRCD(1)             TBL ID
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
     C     GIKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
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
     C*** GET COMPANY #
     C                   READ      PCO01                                  90
     C     *IN90         DOWEQ     *OFF
     C                   Z-ADD     COCO#         KKCO#
     C*
     C*** PROCESS FTGIP
     C                   DO        15            X                 2 0
     C*
     C     ITID(X)       IFNE      *BLANKS
     C*
     C                   MOVEL     ITID(X)       KKTBID
     C*
     C     GIKEY         CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL     IDES(X)       GIDESC
     C                   MOVEL     ITID(X)       GITBID
     C                   MOVE      'A'           GISTAT
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C********             ELSE
     C*********            UPDATPGI01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*** PROCESS FTGTP
     C                   DO        50            X                 2 0
     C*
     C     TKEY(X)       IFNE      *BLANKS
     C*
     C                   MOVEL     TKEY(X)       KKKEY
     C                   MOVEL     TTID(X)       KKTBID
     C*
     C     GTKEY1        CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     TDES(X)       GTDESC
     C                   MOVEL     TTID(X)       GTTBID
     C                   MOVEL     TKEY(X)       GTKEY
     C                   MOVE      'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*********            ELSE
     C*********            UPDATPGT01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   READ      PCO01                                  90
     C                   ENDDO                                                  DO *IN90
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
** STGIP ID'S & DESCRIPTIONS (ITID, IDES)
DLTY      DEPARTMENT LETTER TYPE














** STGTP KEYS & DESCRIPTIONS (TKEY, TDES)
C         CASE LETTER
P         PRE-CERT LETTER
E         EOMB LETTER















































** FTGTP TABLE ID  (TTID)
DLTY
DLTY
DLTY















































