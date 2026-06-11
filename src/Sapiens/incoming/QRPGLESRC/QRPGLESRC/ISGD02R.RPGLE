     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H/TITLE ISGD02R - Prompt for DARTS                               *
     H*****************************************************************
     H* Description: This program prompts the user to enter info that *
     H*              will be used to call the DARTS remarks pgm       *
     H*              The criteria for DARTS in this pgm are:          *
     H*                CO/FND                                         *
     H*                                                               *
     H* Called from: SC011R                                           *
     H* Call to:     SC010R                                           *
     H*                                                               *
     H* Created: 02/01/2002  C:87  Project: DR173                     *
     H*****************************************************************
     H* Indicators:                                                   *
     H*   90 - Controls main loop                                     *
     H*   91 - Error indicator                                        *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* xx/xx/xx  c:xxx  Project: xxxxx                               *
     H*    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *
     H*                                                               *
     H*****************************************************************
     FISGD02F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F* System company file
     FSMCOL     IF   E           K DISK
     F*
     F* User parameter file
     FSMUPL     IF   E           K DISK
     F*
     F* Key type file
     FSMKTL     IF   E           K DISK
     F*
     F* Fund file
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     F*

     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*  Dart key data structure
     D                 DS
     D  @@KEY                  1     40
     D  @@CO#                  1      3  0
     D  @@FND                  4      6  0
     D  @@QTE                 40     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C****************************************************************
     C*  KEY LISTS
     C****************************************************************
     C*  Key list for the company file
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  Key list for the user parameter file
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Key list for the key type file
     C     KTKY          KLIST
     C                   KFLD                    QQID
     C*
     C*  Key list for the fund file
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKFND
     C*
     C****************************************************************
     C*  ENTRY PARAMETERS
     C****************************************************************
     C     *ENTRY        PLIST
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQID              2
     C*
     C*    Parm list for call to fund selection program
     C     FNDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C*
     C****************************************************************
     C*  *LIKE DEFN
     C****************************************************************
     C     *LIKE         DEFINE    FNFND         KKFND
     C*
     C****************************************************************
     C*  Main processing
     C****************************************************************
     C*  Call subroutine to do preprocessing
     C                   EXSR      PGMSTR
     C*
     C*  Load the rocord format info for the first time
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*   Main Loop - process until user ends
     C     *IN90         DOUEQ     *ON
     C                   SETOFF                                         90
     C*
     C*     Output screen 1
     C  N90Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*     PROCESS SCREEN
     C                   SELECT
     C*
     C*       Process command keys
     C     *IN11         WHENEQ    *ON
     C     *IN90         ANDEQ     *OFF
     C                   EXSR      FNCKEY
     C*
     C                   OTHER
     C*       Goto the appropriate screen
     C  N90Q2RECF        CASEQ     'SCR01'       SCR01T
     C  N90Q2RECF        CASEQ     'SCR02'       SCR02T                         FUTURE USE
     C                   ENDCS
     C*
     C*       All edits have been passed
     C*         Call the DARTS program
     C  N90
     CANN91              EXSR      SRDART
     C*
     C                   ENDSL                                                  END PROCESS SCREEN
     C*
     C                   ENDDO                                                  *IN90=*ON
     C*
     C*   end the program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*===============================================================
     C*   FNCKEY - Process function keys
     C*===============================================================
     C     FNCKEY        BEGSR
     C*
     C*   F3=Exit
     C*   *******
     C   KC              SETON                                          90
     C*
     C*   F12=Cancel
     C*   **********
     C     *INKL         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   SETON                                          90
     C                   ENDIF
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*   F10=Search
     C*   **********
     C   KJ              EXSR      SEARCH
     C*
     C*   Process help requests
     C*   *********************
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        XXRECF
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C                   ENDIF                                                  *IN10=*ON
     C*
     C*   F13=Change user parms
     C*   *********************
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C                   EXSR      USRVLS
     C                   ENDIF                                                  *INKM=*ON
     C*
     C                   ENDSR                                                  FNCKEY
     C*
     C*===============================================================
     C*   PGMSTR - Does processing when program is first called
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
     C*  Chain to the key type file
     C     KTKY          CHAIN     PKT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  KTKTYP        SCKTYP
     C                   MOVEL(P)  KTPDES        SCDESC
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  Call the subroutine to get user parms
     C                   EXSR      USRVLS
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C*   SCR01T - Process request screen 01
     C*===============================================================
     C     SCR01T        BEGSR
     C*
     C*  Call subroutine to set the DARTS key
     C                   EXSR      SETKEY
     C*
     C*  Call the verification program
     C                   CALL      KTVPGM
     C                   PARM                    @@KEY
     C                   PARM                    @@RTCD            1
     C*
     C                   SELECT
     C     @@RTCD        WHENEQ    '1'
     C                   SETON                                        9121
     C*
     C     @@RTCD        WHENEQ    '2'
     C                   SETON                                        9122
     C                   ENDSL
     C*
     C                   ENDSR                                                  SCR01T
     C*
     C*===============================================================
     C*   SCR02T - Process request screen 02
     C*===============================================================
     C     SCR02T        BEGSR
     C                   ENDSR                                                  SCR02T
     C*
     C*===============================================================
     C*   SEARCH - Subroutine to handle the F10 search process
     C*===============================================================
     C     SEARCH        BEGSR
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*     Search for Group
     C     HFLD          IFEQ      'SCFND'
     C*       Call PGM to display selection screen
     C                   MOVE      UPCO#         ##CO#
     C                   CALL      'W0010R'      FNDPL
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     ##FND         IFNE      *BLANK
     C                   MOVE      ##FND         SCFND
     C                   ENDIF                                                  ##FND=' '
     C*
     C                   MOVE      SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      *BLANKS       SCFNDN
     C  N60              MOVEL     FNNAME        SCFNDN
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDIF                                                  HFLD=SCFND
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   ENDSR                                                  SEARCH
     C*
     C*===============================================================
     C*   SETKEY - sets the DARTS key
     C*===============================================================
     C     SETKEY        BEGSR
     C*
     C                   CLEAR                   @@KEY
     C                   MOVE      UPCO#         @@CO#
     C                   MOVE      SCFND         @@FND
     C                   MOVE      *BLANKS       @@QTE
     C*
     C                   ENDSR                                                  SETKEY
     C*
     C*===============================================================
     C*   SRDART - subroutine to call the DARTS program
     C*===============================================================
     C     SRDART        BEGSR
     C*
     C*  Call subroutine to set the DARTS key
     C                   EXSR      SETKEY
     C*
     C*  Call the DARTS pgm
     C                   CALL      'SC010R'
     C                   PARM                    @@KEY
     C                   PARM      KTPNAM        @@PGM            10
     C*
     C                   ENDSR                                                  SRDART
     C*
     C*===============================================================
     C*   USRVLS - Gets the user parameter file and sets the company
     C*            value for the screen.  Calls UOTHER for any other
     C*            user parm values needed.
     C*===============================================================
     C     USRVLS        BEGSR
     C*
     C*  Get the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
     C   60              MOVE      *ZERO         UPFND
     C*
     C*    Get the company record
     C     COKY          CHAIN     PCO01                              60
     C                   MOVE      *BLANKS       SRDFLD
     C  N60              MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*    Do any other processing based on the user parm file needed
     C                   EXSR      UOTHER
     C*
     C                   ENDSR                                                  USRVLS
     C*
     C*===============================================================
     C*   UOTHER - Initializes screen fields, other than the company
     C*            field, based on the user parameter file
     C*===============================================================
     C     UOTHER        BEGSR
     C*
     C*  Get the fund record
     C                   MOVE      UPFND         SCFND
     C                   MOVE      SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      *BLANKS       SCFNDN
     C  N60              MOVEL     FNNAME        SCFNDN
     C*
     C                   ENDSR                                                  UOTHER
