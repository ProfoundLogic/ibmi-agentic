     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9620RLA                                          *
     H*    DESCRIPTION: LA DATA SUBMISSION (PROMPT)                   *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      F9521CLA - LA DATA SUBMISSION (BUILDS QUERY)             *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  22 - ERROR IF TO DATE FIELD IS BLANK
     H*  23 - ERROR IF FROM DATE FIELD IS BLANK
     H*  25 - ERROR IF SCFILT=BLANKS OR SCFILT<>'P or H'
     H*  26 - ERROR IF FROM DATE NOT VALID
     H*  27 - ERROR IF TO DATE NOT VALID
     H*  28 - ERROR IF DATES NOT ASCENDING
     H*  30 - TURN ON IF SCFILT=P
     H*  31 - TURN ON IF SCFILT=H
     H*****************************************************************
     FF9520FLA  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Entry Parameters
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET COMPANY NAME & CENTER IF EXISTS
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C* Clear all fields before screen is loaded
     C*
     C                   MOVE      *BLANKS       SCFILT
     C                   MOVE      *BLANKS       SCFRDT
     C                   MOVE      *BLANKS       SCTODT
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display Screen Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* Process All Command Keys
     C*
     C************************************
     C* F3 = EXIT                        *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C****************************
     C* CHOOSE PROCESSING        *
     C****************************
     C     SCR01T        TAG
     C*
     C     SCFILT        IFNE      'P'
     C     SCFILT        ANDNE     'H'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFILT        IFEQ      'P'
     C                   SETON                                        30
     C                   END
     C*
     C     SCFILT        IFEQ      'H'
     C                   SETON                                        31
     C                   END
     C*
     C* Validate the Entered "FROM DATE"
     C*
     C     SCFRDT        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the Entered "TO DATE"
     C*
     C     SCTODT        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the Entered Dates to Verify Ascending Order
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT             7 0
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C     XXFDT         CABGT     XXTDT         STRT                 28
     C*
     C*  CALL SUBMITTER PGM
     C*
     C                   Z-ADD     UPCO#         XXCO#             3 0
     C                   MOVEL     'LA'          XXST              2
     C   30              MOVEL     'P'           XXSYS             1
     C   31              MOVEL     'H'           XXSYS             1
     C                   MOVEL     SCFRDT        XXFRDT            8
     C                   MOVEL     SCTODT        XXTODT            8
     C                   CALL      'F9521CLA'
     C                   PARM                    XXCO#
     C                   PARM                    XXST
     C                   PARM                    XXSYS
     C                   PARM                    XXFRDT
     C                   PARM                    XXTODT
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
