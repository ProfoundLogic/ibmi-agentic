     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA202R - LINE ITEM ERROR CHGECK - FDMGP                  DATA
     H*
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA002R                                            *
     H*    CREATED: 12/15/97  C:56  Project: FS139                    *
     H*    DESCRIPTION: THIS API WILL CHECK TO SEE IF GIVEN LINE      *
     H*                 ITEM HAS ANY ERRORS IN BILL ENTRY ERROR       *
     H*                 MESSAGE FILE - FDMGP.                         *
     H*                 IF CHAIN TO FDMGP FAILS 'F' IS RETURNED       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFDMGL     IF   E           K DISK
     FFDMGL1    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F*

     IPMG02
     I              MGCO#                       M2CO#
     I              MGSUB#                      M2SUB#
     I              MGFND                       M2FND
     I              MGFYR                       M2FYR
     I              MGCASE                      M2CASE
     I              MGDCS#                      M2DCS#
     I              MGDSEQ                      M2DSEQ
     I              MGMSEQ                      M2MSEQ
     I              MGEOB#                      M2EOB#
     I              MGMSG#                      M2MSG#
     I              MGMSFX                      M2MSFX
     I              MGCSI                       M2CSI
     I              MGSRC                       M2SRC
     I              MGSTAT                      M2STAT
     I              MGUSER                      M2USER
     I              MGPGM                       M2PGM
     I              MGTIME                      M2TIME
     I              MGDT                        M2DT
     I              MGIUSR                      M2IUSR
     I              MGIPGM                      M2IPGM
     I              MGITIM                      M2ITIM
     I              MGIDT                       M2IDT

     C*
     C** KEY LIST FOR BILL ENTRY ERROR MESSAGE FILE WITH DCS#
     C*
     C     MGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDCS#
     C                   KFLD                    XXDSEQ
     C*
     C** KEY LIST FOR BILL ENTRY ERROR MESSAGE FILE WITH EOB#
     C*
     C     MGKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1             1
     C                   PARM                    @@CO#             3             2
     C                   PARM                    @@SUB#            5             3
     C                   PARM                    @@FND             3             4
     C                   PARM                    @@FYR             3             5
     C                   PARM                    @@CASE            7             6
     C                   PARM                    @@DCS#            5             7
     C                   PARM                    @@EOB#            9             8
     C                   PARM                    @@DSEQ            5             9
     C*
     C     *LIKE         DEFINE    MGCO#         XXCO#
     C     *LIKE         DEFINE    MGSUB#        XXSUB#
     C     *LIKE         DEFINE    MGFND         XXFND
     C     *LIKE         DEFINE    MGFYR         XXFYR
     C     *LIKE         DEFINE    MGCASE        XXCASE
     C     *LIKE         DEFINE    MGDCS#        XXDCS#
     C     *LIKE         DEFINE    MGDSEQ        XXDSEQ
     C     *LIKE         DEFINE    MGEOB#        XXEOB#
     C*
     C                   MOVE(P)   @@CO#         XXCO#
     C                   MOVE(P)   @@SUB#        XXSUB#
     C                   MOVE(P)   @@FND         XXFND
     C                   MOVE(P)   @@FYR         XXFYR
     C                   MOVE(P)   @@CASE        XXCASE
     C                   MOVE(P)   @@DCS#        XXDCS#
     C                   MOVE(P)   @@EOB#        XXEOB#
     C                   MOVE(P)   @@DSEQ        XXDSEQ
     C*
     C     XXDCS#        IFNE      *ZEROS
     C     MGKY1         CHAIN     PMG01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      'F'           @@RTCD
     C                   ENDIF                                                  *IN99 IFEQ 0
     C                   ENDIF                                                  XXDCS# IFNE *ZEROS
     C*
     C     XXEOB#        IFNE      *ZEROS
     C     MGKY2         CHAIN     PMG02                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      'F'           @@RTCD
     C                   ENDIF                                                  *IN99 IFEQ 0
     C                   ENDIF                                                  XXEOB# IFNE *ZEROS
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
