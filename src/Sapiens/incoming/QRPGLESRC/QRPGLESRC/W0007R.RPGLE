     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0007R - UPDATE USER PARAM. WITH SYSTEM DATE
     H****************************************************************
     H* MODIFICATIONS:
     H*              LJQ  8-23-91   ADDED SYSTEM    USER PARM FILE.
     H*                             FILE NEEDS TO BE UPDATED WHEN
     H*                             WC USER PARM IS UPDATED
     H****************************************************************
     FWMUPL     UF   E           K DISK
     FSMUPL     UF   E           K DISK
     F                                     RENAME(PUP01:PUP02)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
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
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60        WMUPP
     C     UPKEY         CHAIN     PUP02                              65        SMUPP
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C                   UPDATE    PUP01
     C                   END
     C     *IN65         IFEQ      '0'                                          LJQ 8-23-91
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C                   UPDATE    PUP02                                        |
     C                   END                                                    |
     C*
     C                   MOVE      '1'           *INLR
     C*
