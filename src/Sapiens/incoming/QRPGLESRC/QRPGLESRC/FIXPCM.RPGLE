     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* ADD GENERIC TABLE IDS AND ITEMS.
     H**
     H* CREATED: 04/21/02 BY C:79 FOR GA015
     H**
     H*****************************************************************
     H*
     FSMCOP     IF   E           K DISK
     FFTGIL     UF A E           K DISK
     FFTGTL     UF A E           K DISK

     D*
     D ITID            S              4    DIM(3) CTDATA PERRCD(1)
     D DESC            S             29    DIM(3) CTDATA PERRCD(1)
     D ITEM            S              1    DIM(4) CTDATA PERRCD(1)
     D ITDS            S             35    DIM(4) CTDATA PERRCD(1)
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
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    GTKEY         KKKEY
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
     C     1             DO        3             X                 2 0
     C*
     C                   MOVEL(P)  ITID(X)       KKTBID            4
     C*
     C     GIKEY         CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     X             WHENEQ    1
     C                   MOVEL(P)  DESC(1)       GIDESC
     C                   MOVEL(P)  ITID(1)       GITBID
     C*
     C     X             WHENEQ    2
     C                   MOVEL(P)  DESC(2)       GIDESC
     C                   MOVEL(P)  ITID(2)       GITBID
     C*
     C     X             WHENEQ    3
     C                   MOVEL(P)  DESC(3)       GIDESC
     C                   MOVEL(P)  ITID(3)       GITBID
     C*
     C                   ENDSL
     C*
     C                   MOVE      COCO#         GICO#
     C                   MOVEL     'A'           GISTAT
     C                   MOVEL     'CODE'        GIFN01
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*** PROCESS FTGIP
     C*
     C* GAPM
     C*
     C                   MOVEL     ITID(1)       KKTBID
     C*
     C                   MOVEL     ITEM(1)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(1)       GTDESC
     C                   MOVEL     ITEM(1)       GTKEY
     C                   MOVEL     ITID(1)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C                   MOVEL     ITEM(4)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(4)       GTDESC
     C                   MOVEL     ITEM(4)       GTKEY
     C                   MOVEL     ITID(1)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C* TXPM
     C*
     C                   MOVEL     ITID(2)       KKTBID
     C*
     C                   MOVEL     ITEM(1)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(1)       GTDESC
     C                   MOVEL     ITEM(1)       GTKEY
     C                   MOVEL     ITID(2)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C                   MOVEL     ITEM(4)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(4)       GTDESC
     C                   MOVEL     ITEM(4)       GTKEY
     C                   MOVEL     ITID(2)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C* ZZPM
     C*
     C                   MOVEL     ITID(3)       KKTBID
     C                   MOVEL     ITEM(1)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(1)       GTDESC
     C                   MOVEL     ITEM(1)       GTKEY
     C                   MOVEL     ITID(3)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C                   MOVEL     ITEM(2)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(2)       GTDESC
     C                   MOVEL     ITEM(2)       GTKEY
     C                   MOVEL     ITID(3)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C                   MOVEL     ITEM(3)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(3)       GTDESC
     C                   MOVEL     ITEM(3)       GTKEY
     C                   MOVEL     ITID(3)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C                   MOVEL     ITEM(4)       KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     ITDS(4)       GTDESC
     C                   MOVEL     ITEM(4)       GTKEY
     C                   MOVEL     ITID(3)       GTTBID
     C                   EXSR      WRTITM
     C                   ENDIF
     C*
     C                   READ      PCO01                                  90
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*================================================================
     C* SUBROUTINE    WRTITM
     C*
     C     WRTITM        BEGSR
     C*
     C                   MOVE      COCO#         GTCO#
     C                   MOVEL     'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*
     C                   ENDSR
     C*================================================================
** FTGIP ID'S
GAPM
TXPM
ZZPM
** FTGIP HEADERS ******************************************|
GEORGIA PROCEDURE CODE METHOD
TEXAS PROCEDURE CODE METHOD
PROCEDURE CODE METHOD
** FTGTP ITEMS ********************************************|
0
4
5
9
** FTGTP ITEM DESCRIPTIONS ********************************|
PROCEDURE CODE LEFT BLANK
USE CPT IN PROCEDURE CODE FIELDS
USE HCPCS IN PROCEDURE CODE FIELDS
USE ICD9 IN PROCEDURE CODE FIELDS
