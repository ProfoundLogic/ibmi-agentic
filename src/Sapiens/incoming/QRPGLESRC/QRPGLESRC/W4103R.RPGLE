     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4103R - PROMPT FOR PRINT MULTIPLE CLAIM FORMS
     H*****************************************************************
     H*  CALLED FROM:                                                 *
     H*      MENU                                                     *
     H*  CALLS TO:                                                    *
     H*      W4100C                                                   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 09/10/99  C:118 PROJECT:WC577                                 *
     H*                 Made file name modification in display and    *
     H*                 adjusted spacing of F keys.                   *
     H*                                                               *
     H* 12/31/97  C:35  PROJECT: WC577                                *
     H*                 CHANGED TO DEFAULT RUN DATE TO CURRENT DATE.  *
     H*                                                               *
     H* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
     H* !!  ADD NEW REVISIONS AT THE TOP OF THE REVISIONS SECTION  !! *
     H* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
     H*                                                               *
     H*****************************************************************
     F*
     F* Prompt screen
     FW4103F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company master file
     FSMCOL     IF   E           K DISK
     F*
     F* User parameter master file
     FWMUPL     IF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
      **** BEGIN ADD **** 12/31/97 ************************************
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      **** END   ADD **** 12/31/97 ************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List For The Compnay Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List For The User Parameter Master File
     C*
     C     UPKEY         KLIST
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     STRT01        TAG
     C*
     C*  Get The User Parameter File
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
      **** BEGIN DEL **** 12/31/97 ************************************
     C******************** Z-ADDUPPRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCDTE
      **** END   DEL **** 12/31/97 ************************************
     C*
     C*  Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
      **** BEGIN ADD **** 12/31/97 ************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
      **** END   ADD **** 12/31/97 ************************************
     C***** BEGIN DEL **** 09/10/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 09/10/99 ***********************************
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
     C                   END
     C*
     C*   Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = End of Job
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   Process Help Requests
     C*
     C     *IN10         IFEQ      '1'
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
     C*   Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   Process Request Screen
     C*
     C* Validate the Run Date Entered
     C*
      **** BEGIN DEL **** 12/31/97 ************************************
     C******************** Z-ADDSCDTE     SRDTI
     C******************** CALL 'SRCVTDAT'DATCHK
     C******************** MOVELSRDTO     SCDTE
      **** END   DEL **** 12/31/97 ************************************
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* Call the CLP to Submit the List
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C*
     C                   MOVE      SCDTE         QQDTE             8
     C*
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W4100C'
     C                   PARM                    QQCO#
     C                   PARM                    QQDTE
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
