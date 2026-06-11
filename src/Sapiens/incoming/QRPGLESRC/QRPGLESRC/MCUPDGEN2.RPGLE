     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* CONVERT EXISTING TABLE ENTRIES TO ANOTHER TABLE ID
     H**
     H* CREATED: 09/22/98   MIKE FLOWER
     H**
     H* LAST CHANGED FOR UPDATE TAPE: 05/10/99
     H*****************************************************************
     H*
     FSMCOP     IF   E           K DISK
     FFTGTL     UF   E           K DISK
     FFTGIL     UF   E           K DISK

     D*
     D ITID            S              4    DIM(15) CTDATA PERRCD(1)             TBL ID, DESC
     D IDES            S             40    DIM(15) ALT(ITID)
     D************        TKEY    1  50 10   TDES   40   KEY, DESC
     D************        TTID    1  50  4               TBL ID
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
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL     IDES(X)       GIDESC
     C*
     C     GITBID        IFEQ      'ILOC'
     C                   MOVEL(P)  'ZZIL'        GITBID
     C                   ENDIF
     C*
     C     GITBID        IFEQ      'ISIT'
     C                   MOVEL(P)  'ZZIS'        GITBID
     C                   ENDIF
     C*
     C***                  MOVE 'A'       GISTAT
     C***                  MOVEL'CODE'    GIFN01    P
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C     *IN99         IFEQ      *ON
     C****************     WRITEPGI01
MJF  C                   ELSE
MJF  C                   UPDATE    PGI01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*** PROCESS FTGTP
     C                   DO        2             X                 2 0
     C     X             IFEQ      1
     C                   MOVEL(P)  'ILOC'        KKTBID
     C                   ENDIF
     C*
     C     X             IFEQ      2
     C                   MOVEL(P)  'ISIT'        KKTBID
     C                   ENDIF
     C*
     C     GIKEY         SETLL     PGT01
     C*
     C     *IN99         DOUEQ     *ON
     C*
     C********** TKEY,X    IFNE *BLANKS
     C*
     C********             MOVELTKEY,X    KKKEY
     C*
     C     GIKEY         READE     PGT01                                  99
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C**************       MOVELTDES,X    GTDESC
     C     GTTBID        IFEQ      'ILOC'
     C                   MOVEL(P)  'ZZIL'        GTTBID
     C                   ENDIF
     C     GTTBID        IFEQ      'ISIT'
     C                   MOVEL(P)  'ZZIS'        GTTBID
     C                   ENDIF
     C**************       MOVELTTID,X    GTTBID
     C**************       MOVELTKEY,X    GTKEY
     C***************      MOVE 'A'       GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C     *IN99         IFEQ      *ON
     C***************      WRITEPGT01
     C                   ELSE
     C                   UPDATE    PGT01
     C                   ENDIF
     C*
     C*************        ENDIF
     C*
     C                   ENDDO
     C                   ENDDO
     C*
     C                   READ      PCO01                                  90
     C                   ENDDO                                                  DO *IN90
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
** FTGIP ID'S & DESCRIPTIONS (ITID, IDES)
ILOCICD9 LOCATION CODE (GENERAL USAGE)
ISITICD9 SITE CODE (GENERAL USAGE)













