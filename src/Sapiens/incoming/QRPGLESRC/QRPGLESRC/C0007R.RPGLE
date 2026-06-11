     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0007R - UPDATE USER PARAMETERS
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program Updates the User Parameters               **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 09/11/02 C:15 Support: OP051/34115                           *
     H*               Added code to prevent UPLVL from being updated *
     H*               in SMUPP.                                      *
     H*                                                              *
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   _____  (_____): __________________________ (___)         **
     H****************************************************************
     H*
     FCMUPL     UF   E           K DISK
     FSMUPL     UF   E           K DISK
     F                                     RENAME(PUP01:PUP02)

     D*** End add ***** 09/11/02 *************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** Begin add *** 09/11/02 *************************************
     IPUP02
     I              UPLVL                       U2LVL
     I              UPTYPE                      U2TYPE
     I              UPUSNM                      U2USNM
     I              UPTITL                      U2TITL
     I              UPOUTQ                      U2OUTQ
     I              UPPRDT                      U2PRDT
     I              UPPAL                       U2PAL
     I              UPRAL                       U2RAL

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
     C     UPKEY         CHAIN     PUP02                              41
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C                   UPDATE    PUP01
     C  N41              UPDATE    PUP02
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
