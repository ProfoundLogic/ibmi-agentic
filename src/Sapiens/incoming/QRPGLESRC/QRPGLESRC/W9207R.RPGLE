     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W9207R - PURGE A/R BATCH ENTRIES
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM WILL READ ALL THE RECORDS IN WMBPP (RECORDS      *
     H* REQUESTED TO BE PURGED) AND DELETE THEIR EQUIVALENT IN        *
     H* THE A/R BATCH FILE WDEAP, IF THE RECORDS IN WMBPP HAVE NOT    *
     H* YET BEEN PROCESSED (I.E. COMPLETION DATE IS ZERO)             *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W1200R (SCREEN ONE, COMMAND 9)                              *
     H*   W6000C
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 07/24/91               PROJECT: MD021                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*   LR  LAST RECORD INDICATOR                                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWDEAL     UF   E           K DISK
     FWMBPL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE BATCH PURGE FILE
     C*
     C     BPKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EASTAT        XXSTAT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      'T'           XXSTAT
     C     BPKEY         CHAIN     PBP01                              50
     C     *IN50         DOWEQ     '0'
     C     BPCMDT        IFEQ      *ZEROS
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C     EATFDT        IFLE      BPPGDT
     C                   DELETE    PEA01
     C                   END                                                    EATFDT <= BPPGDT
     C     EAKEY         READE     PEA01                                  60
     C                   END                                                    *IN60
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BPCMDT
     C*
     C                   UPDATE    PBP01
     C                   END                                                    BPCMDT = 0
     C     BPKEY         READE     PBP01                                  50
     C                   END                                                    *IN50
     C*
     C                   MOVE      '1'           *INLR
