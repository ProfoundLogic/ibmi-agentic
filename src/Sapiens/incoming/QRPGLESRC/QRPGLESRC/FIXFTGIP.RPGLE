     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* UPDATE THE GENERIC TABLE ID HEADERS FOR SELECTED IDS
     H**
     H* CREATED: 12/01/00 BY C:62 FOR MC183
     H**
     H*****************************************************************
     H*
     FSMCOP     IF   E           K DISK
     FFTGIL     UF   E           K DISK

     D*
     D ITID            S              4    DIM(11) CTDATA PERRCD(1)
     D HDDS            S             20    DIM(33) CTDATA PERRCD(3)
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
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** GET COMPANY #
     C                   READ      PCO01                                  90
     C     *IN90         DOWEQ     *OFF
     C                   Z-ADD     COCO#         KKCO#             3 0
     C*
     C*** PROCESS FTGIP
     C     1             DO        11            X                 2 0
     C*
     C                   MOVEL(P)  ITID(X)       KKTBID            4
     C*
     C     GIKEY         CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     X             WHENEQ    1
     C                   MOVEL(P)  HDDS(1)       GIFH01
     C*
     C     X             WHENEQ    2
     C                   MOVEL(P)  HDDS(4)       GIFH01
     C*
     C     X             WHENEQ    3
     C                   MOVEL(P)  HDDS(7)       GIFH01
     C*
     C     X             WHENEQ    4
     C                   MOVEL(P)  HDDS(10)      GIFH01
     C*
     C     X             WHENEQ    5
     C                   MOVEL(P)  HDDS(13)      GIFH01
     C*
     C     X             WHENEQ    6
     C                   MOVEL(P)  HDDS(16)      GICH01
     C*
     C     X             WHENEQ    7
     C                   MOVEL(P)  HDDS(19)      GIFH01
     C                   MOVEL(P)  HDDS(20)      GI5H01
     C*
     C     X             WHENEQ    8
     C                   MOVEL(P)  HDDS(22)      GICH01
     C*
     C     X             WHENEQ    9
     C                   MOVEL(P)  HDDS(25)      GIFH01
     C                   MOVEL(P)  HDDS(26)      GICH01
     C                   MOVEL(P)  HDDS(27)      GICH02
     C*
     C     X             WHENEQ    10
     C                   MOVEL(P)  HDDS(28)      GIFH01
     C*
     C     X             WHENEQ    11
     C                   MOVEL(P)  HDDS(31)      GIFH01
     C*
     C                   ENDSL
     C*
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   UPDATE    PGI01
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   READ      PCO01                                  90
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
** FTGIP ID'S
BILT
FLBT
ZZBT
BTYP
CLST
DSTY
ISTS
KYSC
MDR
MSTS
PHTY
** FTGIP HEADERS **|*******************|*******************|
System type code . .
Inpatient/Outpatient
Inpatient/Outpatient
System type code . .
OH BWC 'Allowed' . .
Discount calc pgm
OH BWC 'Allowed' . .OH BWC stat sort seq
Taxonomy code x-ref
Type of data . . . .Tape name  . . . . .Conversion pgm . . .
OH BWC x-ref values
System type code . .
