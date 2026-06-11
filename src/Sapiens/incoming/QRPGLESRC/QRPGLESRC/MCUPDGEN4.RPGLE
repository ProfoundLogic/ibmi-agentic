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

     D*
     D PROD            S             10    DIM(5) CTDATA PERRCD(1)              KEY, DESC
     D PDES            S             40    DIM(5) ALT(PROD)
     D FORM            S             10    DIM(41) CTDATA PERRCD(1)             TBL ID
     D FDES            S             40    DIM(41) ALT(FORM)
     D MEAS            S             10    DIM(13) CTDATA PERRCD(1)             TBL ID
     D MDES            S             40    DIM(13) ALT(MEAS)
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
     C*** PROCESS FTGTP - PROD
     C*
     C     1             DO        5             X                 2 0
     C*
     C                   MOVEL(P)  'PROD'        KKTBID
     C                   MOVEL(P)  PROD(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     PDES(X)       GTDESC
     C                   MOVEL     'PROD'        GTTBID
     C                   MOVEL     PROD(X)       GTKEY
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
     C     1             DO        41            X                 2 0
     C*
     C                   MOVEL(P)  'FORM'        KKTBID
     C                   MOVEL(P)  FORM(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     FDES(X)       GTDESC
     C                   MOVEL     'FORM'        GTTBID
     C                   MOVEL     FORM(X)       GTKEY
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
     C     1             DO        13            X                 2 0
     C*
     C                   MOVEL(P)  'MEAS'        KKTBID
     C                   MOVEL(P)  MEAS(X)       KKKEY
     C*
     C     GTKEY1        CHAIN     PGT01                              98
     C*
     C     *IN98         IFEQ      *ON
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     MDES(X)       GTDESC
     C                   MOVEL     'MEAS'        GTTBID
     C                   MOVEL     MEAS(X)       GTKEY
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
04        RX/REPACKAGED BRAND NAME PRODUCT
06        RESERVED
09        OTC/REPACKAGED
10        RX/REPACKAGED BRANDED GENERIC PRODUCT
11        RX/CHEMICAL FOR COMPOUNDING
** FTGTP KEYS & DESCRIPTIONS (FORM, FDES)
ARA       AEROSOL LIQUID W/ADAPTER
ARD       AEROSOL SOLID W/ADAPTER
ARO       AEROSOL SOLID
BAR       BAR
BLO       BLOCK
CAK       CAKE
CAT       CATHETER
CER       CAPSULE, EXTENDED RELEASE
CHI       CHIP
CLA       CLAMP
CNT       CONCENTRATE
CON       CONNECTOR
COT       COTTON
CRY       CRYSTAL
CTB       CHEWABLE TABLET
DSK       DISK
ECC       ENTERIC COATED CAPSULE
ECT       ENTERIC COATED TABLET
FIL       FILM
FOA       FOAM
GEF       GRANULE, EFFERVESCENT
GER       GRANULE, EXTENDED RELEASE
GFS       GEL FORMING SOLUTION
GRR       GRANULE FOR RECONSTITUTION
GUM       GUM
ICR       INSERT, CONTROLLED RELEASE
IMP       IMPLANT
LUM       LUMP
ODT       ORALLY DISINTEGRATING TABLET
PAK       PATIENT PAK
PDI       POWDER FOR INJECTION
PDR       POWDER FOR RECONSTITUTION
PEL       PELLET
POD       POD
SER       SUSPENSION, EXTENDED RELEASE
SHA       SHAMPOO
TCP       TABLET, COATED PARTICLES
TDM       FILM, EXTENDED RELEASE
TEF       TABLET, EFFERVESCENT
TER       TABLET, EXTENDED RELEASE
WAX       WAX
** FTGTP KEYS & DESCRIPTIONS (MEAS, MDES)
A         CC (CUBIC CENTIMETERS)
B         ML (MILLILITERS)
C         GM (GRAMS)
D         OZ (DRY OUNCES)
E         LB (POUNDS)
F         PT (PINTS)
G         GR (GRAINS)
H         MG (MILLIGRAMS)
I         GAL (GALLONS)
U         IU (INTERNATIONAL UNITS)
M         OZ (LIQUID OUNCES)
N         QT (QUARTS)
P         DU (DISPENSING UNITS)
