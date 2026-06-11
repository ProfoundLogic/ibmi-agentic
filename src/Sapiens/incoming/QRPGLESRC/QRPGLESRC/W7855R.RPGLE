     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7855R - CLAIMS WITH NO ACTIVITY REPORT - PRMPT PGM
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO ENTER THE FYR AND       **
     H**     DATE THEY WANT THE CLAIMS WITH NO ACTIVITY REPORT TO    **
     H**     PRINT FOR.                                              **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**   CALLS TO:                                                 **
     H**     W7855C                                                  **
     H**   PROGRAMMER: C:10                                          **
     H**   CREATED:    9/23/93      PROJECT: WC220                   **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                               *
     H*  05/02/01 C:134 PROJECT: WC577
     H*                 UPDATE SO CURRENT GROUP NUMBER DISPLAYS
     H*                 AFTER USER PARAMETERS ARE CHANGED.
     H*
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7855F IN SCREEN.     *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      20      WCR0517 - Fund Year Invalid for Fund.          **
     H**      21      MCR0500 - Invalid Date in field.               **
     H**      60      General Usage (CHAIN)                          **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO                                        **
     H**    WMUPL (PUP01): USER                                      **
     H**    WMFNL (PFN01): CO/FND                                    **
     H**    WDF1L (PF101): CO/FND/FYR                                **
     H*****************************************************************
     FW7855F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
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
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  KEY LIST FOR FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
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
     C     TOP           TAG
     C*
     C                   TIME                    DSDTTM
     C*
     C*  CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     *ZERO         SCFYR
     C                   Z-ADD     *ZERO         SCNADT
     C*
     C*  CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    *INKA
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*** BEGIN ADD ****05/02/01*******************************************
     C                   MOVE      UPFND         SCFND
     C*** END   ADD ****05/02/01*******************************************
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
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
     C                   END                                                    *IN10
     C*
     C                   END
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  EDIT FUND YEAR IF ENTERED
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     SCFYR         IFNE      *ZERO
     C*
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  EDIT 'NO ACTIVITY SINCE' DATE
     C*
     C                   Z-ADD     SCNADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCNADT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      SCNADT        QQNADT            8
     C                   CALL      'W7855C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQNADT
     C                   PARM                    UPOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
