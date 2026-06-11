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
     FFTGTL     IF A E           K DISK

     D*
     D DKY             S             10    DIM(3) CTDATA PERRCD(1)
     D DDS             S             40    DIM(3) ALT(DKY)
     D FKY             S             10    DIM(38) CTDATA PERRCD(1)
     D FDS             S             40    DIM(38) ALT(FKY)
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
     C                   READ      PCO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     COCO#         KKCO#             3 0
     C*
     C                   MOVEL     'DOSE'        KKTBID
     C     1             DO        3             X                 3 0
     C*
     C                   MOVEL(P)  DKY(X)        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL(P)  KKTBID        GTTBID
     C                   MOVEL(P)  KKKEY         GTKEY
     C                   MOVEL(P)  DDS(X)        GTDESC
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
     C                   MOVEL     'FORM'        KKTBID
     C     1             DO        38            X                 3 0
     C*
     C                   MOVEL(P)  FKY(X)        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL(P)  KKTBID        GTTBID
     C                   MOVEL(P)  KKKEY         GTKEY
     C                   MOVEL(P)  FDS(X)        GTDESC
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
** DKY  - DDS
S         STANDARD PACKAGING
U         UNIT-OF-USE PACKAGING
X         UNIT-DOSE PACKAGING
** FKY  - FDS
APB       AEROSOL POWDER, BREATH ACTIVATED
ARB       AEROSOL, BREATH ACTIVATED
ARP       AEROSOL, POWDER
ARS       AEROSOL, SOLUTION
BEA       BEADS
CCR       CAPSULE CONTROLLED-RELEASE
CDR       CAPSULE DELAYED RELEASE
CEC       CAPSULE ENTERIC COATED
CEP       CAPSULE ENTERIC COATED PARTICLES
CSP       CAPSULE SPRINKLED
C24       CAPSULE EXTENDED RELEASE, 24 HR
GAS       GAS
IUD       IUD
LCR       LIQUID CONTROLLED-RELEASE
LOL       LOLLIPOP
MIS       MISCELLANEOUS
PAT       PATCH
P24       PATCH 24 HR
P72       PATCH 72 HR
PBI       PATCH BIWEEKLY
PWE       PATCH WEEKLY
PEF       POWDER EFFERVESCENT
PUD       PUDDING
RIN       RING
SHE       SHEET
SOR       SOLUTION RECONSTITUTED
SPT       SPIRIT
STR       STRIP
SUR       SUSPENSION RECONSTITUTED
CHW       TABLET CHEW
TCR       TABLET CONTROLLED-RELEASE
TDR       TABLET DELAYED RELEASE
TDP       TABLET DISPERSIBLE
TSO       TABLET SOLUBLE
T12       TABLET SR 12HR
T24       TABLET SR 24HR
TSU       TABLET SUBLINGUAL
TAR       TAR
