     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7580R - PROMPT FOR SELF REPORTING LATE NOTICES
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/10/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     F*
     F* Prompt Screen
     F*
     FW7580F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     F*
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameters Master File
     F*
     FWMUPL     IF   E           K DISK
     F*
     F* GROUP Master File
     F*
     FWMFNL     IF   E           K DISK
     F*
     F* GROUP Detail File
     F*
     FWDF1L     IF   E           K DISK

     D*
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
     C* Key List For The Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List For THe User Parameters File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List For The GROUP Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C* Key List For The GROUP Detail File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C* Enter Parameters
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Get the User Parameter File
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Get the Company Record
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDTE
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNPZIP        SCZP              3 0
     C*
     C                   MOVE      Q1USER        SCUSER
     C*
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C* Load the Record Format Info for First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 01 = End of Job
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* Process Help Requests
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C* Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C* Get the User Parameter File
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C* Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C*  Validate the POLICY Year Entered
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the Run Date Entered
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCDTE         QQDTE             8
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W7580C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQDTE
     C                   PARM                    QQOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
