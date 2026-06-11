     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA030R - WRITE/UPDATE WCMC CLAIM STAFF CODES
     H*****************************************************************
     H* Description:  This pgm updates the WCMC staff codes based on  *
     H*    the WCMS staff codes passed in the parms.                  *
     H*                                                               *
     H* Called from:  W4301R                                          *
     H* Calls to   :  none                                            *
     H*                                                               *
     H* Created:  5/23/00     C:15                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    99         GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCML1    UF   E           K DISK
     FFMFNL1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*-------------------------------------------------------------------------
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQWCF#            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQCMNU            3            NRSE
     C                   PARM                    QQCMSU            3            *BLANKS
     C                   PARM                    QQCLAD            3            CLEX
     C                   PARM                    QQCLSU            3            SUPR
     C                   PARM                    QQCLFA            3            FEAD
     C                   PARM                    QQRTCD            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXWCF#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQWCF#        XXWCF#
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C*
     C*  Convert the WCF# to WCMC SUB# and FND.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXFND
     C*
     C*  Update the WCMC claim staff codes.
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      QQCMNU        CMCMNU
     C****                 MOVE QQCMSU    CMCMSU
     C                   MOVE      QQCLAD        CMCLAD
     C                   MOVE      QQCLSU        CMCLSU
     C                   MOVEL     QQCLFA        CMCLFA
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C                   RETURN
     C*-------------------------------------------------------------------------
