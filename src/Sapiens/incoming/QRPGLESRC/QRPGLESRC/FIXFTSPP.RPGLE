     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
    H/TITLE FIXFTSPP - UPDATE SPECIALTY CODE TABLE FOR SPECIALTY CODE CROSS REF
    H*
    H**************************************************************************
    H*  Created on 12/13/2006 by c:182 for Project TN005                      *
    H**************************************************************************
    H*  Revisions:                                                            *
    H*                                                                        *
    H**************************************************************************
     FFTSPL1    UF   E           K DISK
    F*
     FSMCOP     IF   E           K DISK
    F*
     FFXSPP     IF   E             DISK
    F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
    D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*----------------------------------------------------------------
    C*  SPECIALTY CODE TABLE
    C*
     C     SPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*----------------------------------------------------------------
    C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*----------------------------------------------------------------
    C*       Field defines
    C*
     C     *LIKE         DEFINE    SPDT          XXDATE
     C     *LIKE         DEFINE    SPCO#         KKCO#
     C     *LIKE         DEFINE    SPKEY         KKKEY
    C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
    C*
    C* READ THE SMCOP TO GET THE COMPANY NUMBER
    C*
     C                   READ      SMCOP                                  59
    C*
     C     *IN59         DOWEQ     '0'
    C*
    C* READ THE UPDATE FOR FTSPP TABLE
    C*
     C                   READ      PFX01                                  60
    C*
     C     *IN60         DOWEQ     '0'
    C*
     C                   MOVE      COCO#         KKCO#
     C                   MOVEL     FXKEY         KKKEY
    C*
     C     SPKY          CHAIN     FTSPL1                             61
     C*
     C     *IN61         IFEQ      '0'
     C*
     C                   MOVEL     FXXREF        SPXREF
     C                   MOVE(P)   Q1PGM         SPPGM
     C                   MOVEL     Q1USER        SPUSER
     C                   Z-ADD     XXDATE        SPDT
     C                   Z-ADD     DSTIME        SPTIME
     C*
     C                   UPDATE    PSP01
     C*
     C                   ENDIF                                                  IFEQ  *IN61
     C*
     C                   READ      PFX01                                  60
     C*
     C                   END                                                    DOWEQ *IN60
     C*
     C                   READ      SMCOP                                  59
     C*
     C                   END                                                    DOWEQ *IN59
     C*
     C                   MOVE      '1'           *INLR
