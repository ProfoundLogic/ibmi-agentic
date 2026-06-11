     Hcopyright('(c) 2018 Sapiens, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H* UPDATE THE GENERIC TABLE FILE for Type of Business to include
     H* an acronym for type.
     H**
     H* CREATED: 01/10/18 Project: 150WAD/WC2587  C: 65
     H**
     H*****************************************************************
     H*
     FSMCOP     IF   E             DISK
     FWTGTL     UF   E           K DISK

     D*
     D key             S             10    DIM(14) CTDATA PERRCD(1)              KEY, acronym
     D acro            S             40    DIM(14) ALT(key)
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
     C*** PROCESS WTGTP - BUSI table
     C*
     C                   MOVEL(P)  'BUSI'        KKTBID
     C*
     C     1             DO        14            X                 2 0
     C*
     C                   MOVEL(P)  key(X)        KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*
     C                   If        GTCF01 = ' '
     C*
     C                   Movel(p)  acro(X)       GTCF01
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   Update    PGT01
     C*
     C                   Endif
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
** WTGTP KEYS & Acronyms
1         IND
2         PART
3         CORP
4
5         JV
6         LP
7
8
9
A         LLC
B         EST
C         TRST
D         LLP
E         ASSOC
