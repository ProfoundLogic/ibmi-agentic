     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0006R - CHANGE USER PARAMETERS
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program Allows Changes to User Parameters         **
     H**   CALLED FROM:                                             **
     H**     Numerous Programs                                      **
     H**   CALLS TO:                                                **
     H**     SRDCK  - Check for Valid Date                          **
     H**     C0003C - Verify an Outque                              **
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 09/11/13 C:35 Support: WC063/83459                           *
     H*               Load 'A' to UPSTAT instead of blank.           *
     H*                                                              *
     H* 09/11/02 C:15 Support: OP051/34115                           *
     H*               Added code to prevent UPLVL from being updated *
     H*               in SMUPP.                                      *
     H*                                                              *
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   _____  (_____): __________________________ (___)         **
     H****************************************************************
     H*
     FC0006F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     UF A E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FCMUPL     UF A E           K DISK
     FSMCOL     IF   E           K DISK

     D UN              S             40    DIM(1) CTDATA PERRCD(1)
     D*** End add ***** 09/11/02 *************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
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
     C                   KFLD                    XXUSR            10
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      UN(1)         SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      Q1USER        XXUSR
     C     UPKEY         CHAIN     PUP02                              41
     C     UPKEY         CHAIN     PUP01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UPCO#         SCCO#
     C                   MOVE      UPOUTQ        SCOUTQ
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZERO         SCCO#
     C                   MOVE      *BLANK        SCOUTQ
     C                   MOVE      *ZERO         SCDT
     C*
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C     SCR01T        TAG
     C*
     C*  THE COMPANY NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE STATUS CODE MUST BE 'A'
     C*
     C     COSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK THE PROCESS DATE FOR A VALID DATE
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK THE OUTQ FOR A VALID VALUE
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C     XXFOND        IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C                   MOVE      Q1USER        UPUSR
     C                   MOVE      SCCO#         UPCO#
     C                   MOVE      SCOUTQ        UPOUTQ
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
      ******** BEG CHG **** 09/11/13 ***************************************
     C****************** MOVE      *BLANK        UPSTAT
     C                   MOVE      'A'           UPSTAT
      ******** END CHG **** 09/11/13 ***************************************
     C                   MOVE      Q1USER        UPUSER
     C                   MOVE      Q1PGM         UPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C                   Z-ADD     DSTIME        UPTIME
     C*
     C     *IN40         IFEQ      '0'
     C     *IN41         IFEQ      '0'
     C                   UPDATE    PUP02
     C                   END
     C                   UPDATE    PUP01
     C                   ELSE
     C                   WRITE     PUP01
     C     *IN41         IFEQ      '1'
     C                   WRITE     PUP02
     C                   END
     C                   END
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UN
        CHANGE USER PARAMETERS
