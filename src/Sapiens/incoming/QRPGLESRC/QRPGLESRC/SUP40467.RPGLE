     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE RPSEQ#
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FWTGIL     IF A E           K DISK
     FWTGTL     O    E           K DISK

     D*
     D ST              S              2    DIM(37) CTDATA PERRCD(1)
     D DESC            S             20    DIM(37) ALT(ST)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D XXDESC          C                   CONST('WCSTAT NCCI States')
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*
     C     GIKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXTBID            4
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C     Q1USER        CHAIN     PUP01                              99
     C*----------------------------------------------------------------
     C                   MOVEL     'WCST'        XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     UPCO#         GICO#
     C                   MOVEL     'WCST'        GITBID
     C                   MOVEL     XXDESC        GIDESC
     C                   MOVEL     'State'       GIFN01
     C                   MOVEL     'A'           GISTAT
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   Z-ADD     DSTIME        GITIME
     C                   Z-ADD     XXDATE        GIDT
     C                   WRITE     PGI01
     C*
     C*  Start of main loop
     C*
     C                   DO        36            X                 3 0
     C*
     C                   Z-ADD     UPCO#         GTCO#
     C                   MOVEL     'WCST'        GTTBID
     C                   MOVEL     ST(X)         GTKEY
     C                   MOVEL     DESC(X)       GTDESC
     C                   MOVEL     'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   Z-ADD     DSTIME        GTTIME
     C                   Z-ADD     XXDATE        GTDT
     C                   WRITE     PGT01
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C**************************************************************************
** ST - NCCI WCSTAT States
AKAlaska
ALAlabama
ARArkansas
AZArizona
COColorado
CTConnecticut
DCDistrict of Columbia
FLFlorida
GAGeorgia
HIHawaii
IAIowa
IDIdaho
ILIllinois
INIndiana
KSKansas
KYKentucky
LALouisiana
MDMaryland
MEMaine
MOMissouri
MSMississippi
MTMontana
NCNorth Carolina
NENebraska
NHNew Hampshire
NMNew Mexico
NVNevada
OKOklahoma
OROregon
RIRhode Island
SCSouth Carolina
SDSouth Dakota
TNTennessee
TXTexas
UTUtah
VTVermont
WIWisconsin
