     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0345R1 - EOMB BATCH WORKFILE BUILD                      *
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* CALLED FROM: F0345C1                                          *
     H* CALLS TO:    NONE                                             *
     H*                                                               *
     H* PROGRAMMER: C:62                                              *
     H* CREATED:    09/10/01  PROJECT: FS231                          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 99 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     FFAEAL1    IF   E           K DISK
     F*
     FFYEAP     O    E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
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
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EASUB#        XXSUB#
     C     *LIKE         DEFINE    EAFND         XXFND
     C*
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXSUB#
     C*
     C* SET UP FOR READ OF EOB FILE
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL     *BLANKS       EAEPFL
     C*
     C                   WRITE     PEA02
     C*
     C     EAKY          READE     PEA01                                  99
     C*
     C                   ENDDO                                                  *IN99 DOWEQ
     C*
     C     TERM          TAG
     C*
     C                   SETON                                        LR
