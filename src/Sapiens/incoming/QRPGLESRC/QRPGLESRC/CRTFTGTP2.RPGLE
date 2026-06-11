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
     FFTGIL     IF A E           K DISK
     FFTGTL     IF A E           K DISK

     D*
     D TKY             S              3    DIM(7) CTDATA PERRCD(1)
     D TDS             S             40    DIM(7) ALT(TKY)
     D SKY             S              3    DIM(7) CTDATA PERRCD(1)
     D SDS             S             40    DIM(7) ALT(SKY)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D TDSC            C                   CONST('CREDENTIAL TYPE')
     D SDSC            C                   CONST('CREDENTIAL STATUS')

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     GIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTDESC        KKDESC
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** GET COMPANY #
     C                   READ      PCO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     COCO#         KKCO#             3 0
     C*
     C                   MOVEL     'CRTP'        KKTBID
     C                   MOVEL(P)  TDSC          KKDESC
     C                   EXSR      WRTGIP
     C     1             DO        7             X                 3 0
     C*
     C                   MOVEL(P)  TKY(X)        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL(P)  KKTBID        GTTBID
     C                   MOVEL(P)  KKKEY         GTKEY
     C                   MOVEL(P)  TDS(X)        GTDESC
     C*
     C                   MOVEL(P)  'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEL     'CSTS'        KKTBID
     C                   MOVEL(P)  SDSC          KKDESC
     C                   EXSR      WRTGIP
     C     1             DO        7             X                 3 0
     C*
     C                   MOVEL(P)  SKY(X)        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL(P)  KKTBID        GTTBID
     C                   MOVEL(P)  KKKEY         GTKEY
     C                   MOVEL(P)  SDS(X)        GTDESC
     C*
     C                   MOVEL(P)  'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     PGT01
     C*
     C                   ENDIF
     C                   ENDDO
     C                   READ      PCO01                                  99
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* WRTGIP - WRITE FTGIP
     C*****************************************************************
     C     WRTGIP        BEGSR
     C*
     C     GIKY          CHAIN     PGI01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL(P)  KKTBID        GITBID
     C                   MOVEL(P)  KKDESC        GIDESC
     C                   MOVEL(P)  'CODE'        GIFN01
     C*
     C                   MOVEL(P)  'A'           GISTAT
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C                   ENDIF
     C*
     C                   ENDSR
** TKY  - TDS                              |
TE1TEMPORARY EXCEPTION LEVEL 1
TE2TEMPORARY EXCEPTION LEVEL 2
AD1APPROVED DOCTOR LEVEL 1
ADXAPPROVED DOCTOR LEVEL 1 W/ NON-MEDICAL
AD2APPROVED DOCTOR LEVEL 2
DDLAPPROVED DESIGNATED DOCTOR
MMICERTIFIED MMI/IR
** SKY  - SDS
APPAPPROVED
EXPEXPIRED
GRDGRANTED
MODMODIFIED
RMDREMOVED
RTDRESTRICTED
WD WITHDRAWN
