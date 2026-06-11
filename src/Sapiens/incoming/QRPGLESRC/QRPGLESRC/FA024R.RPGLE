     H*****************************************************************
     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H* API - FA024R - RETURNS WCMC STAFF CODES FROM FMCMP            *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA024R                                            *
     H*    CREATED: 08/08/05  C:79 FROM CLT025 VERSION                *
     H*    DESCRIPTION: RETURNS WCMC STAFF CODES FROM FMCMP           *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  F8262R1 - UPLOAD SCANDIRECT MEDICAL BILLS                    *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H** 09/07/21 C: 079 Support: 129/97891                           *
     H*                  Bring base to base                           *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCML1    IF   E           K DISK
     FFMFNL1    IF   E           K DISK
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3             1
     C                   PARM                    @@FND             3             2
     C                   PARM                    @@FYR             3             3
     C                   PARM                    @@CASE            7             4
     C                   PARM                    @@CMNU            3             5
     C                   PARM                    @@CLAD            3             6
     C                   PARM                    @@CMSU            3             7
     C                   PARM                    @@CLSU            3             8
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
     C** KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C*
     C** KEY LIST FOR CASE MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKGRP
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNWCF#        KKWCF#
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKGRP
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*
     C** GET MANAGED CARE SUBSCRIBER NUMBER AND GROUP
     C*
     C                   MOVEL     @@CO#         KKCO#
     C                   MOVEL     @@FND         KKWCF#
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL     FNSUB#        KKSUB#
     C                   MOVEL     FNFND         KKGRP
     C*
     C                   ENDIF
     C*
     C** GET WCMC CLAIM MASTER RECORD
     C*
     C                   MOVEL     @@FYR         KKFYR
     C                   MOVEL     @@CASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL     CMCMNU        @@CMNU
     C                   MOVEL     CMCLAD        @@CLAD
     C                   MOVEL     CMCMSU        @@CMSU
     C                   MOVEL     CMCLSU        @@CLSU
     C*
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
