     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8603R1 - PROMPT FOR DAILY CLAIMS REGISTER
     H*****************************************************************
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      W8603C1 - SUBMITTER                                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 03/30/00  C:118  PROJECT: WC577                               *
     H*                  MADE V3 DISPLAY FILE CHANGES.                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  20,24  INVALID DATE                                          *
     H*  21     TO DATE LESS THAN FROM DATE                           *
     H*  22,25  ONLY ONE DATE ENTERED                                 *
     H*  23     POSITION CURSOR TO APPROPRIATE DATE FIELD             *
     H*  60     GENERAL CHAIN                                         *
     H*****************************************************************
     FW8603F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* KEY List For The Compnay Master File
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C* KEY List For The User Parameter Master File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     *LIKE         DEFINE    UPDT          XXFDAT
     C     *LIKE         DEFINE    UPDT          XXTDAT
     C*
     C     STRT01        TAG
     C*
     C*  Get The User Parameter File
     C*
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
     C*
     C*  Get the Company Record
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*                    MOVE Q1USER    SCUSER
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   Load the Record Format Info For First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*   Process All Command KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=*ON
     C*
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*********************
     C*  Process SCR01
     C*********************
     C*
     C*   Both dates must be entered
     C*
     C                   SELECT
     C*
     C     SCDATF        WHENEQ    *ZERO
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C*
     C     SCDATT        WHENEQ    *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C*   Validate the Date Range Entered
     C*
     C                   Z-ADD     SCDATF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C     SRERR         CABEQ     '1'           STRT                     20
     C*
     C                   Z-ADD     SCDATT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*   The dates must be in ascending order.
     C*
     C     XXTDAT        IFLT      XXFDAT
     C*
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCDATT IFLT SCDATF
     C*
     C* Call the CLP to Submit the List
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVEL     SCDATF        QQFRDT            8
     C                   MOVEL     SCDATT        QQTODT            8
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C*
     C                   CALL      'W8603C1'
     C                   PARM                    QQCO#
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
