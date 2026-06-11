     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W8563R - Zurich File Extract
     H*****************************************************************
     H*  Program Description:                                         *
     H*    Prompt program for Zurich File Extract Process.            *
     H*                                                               *
     H*  Called from: Menu                                            *
     H*  Calls to   : W8365C - Zurich File Extract - Submitter        *
     H*                                                               *
     H*****************************************************************
     H*  Created on 09/15/2009 by c:62 for Project 114WTR             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
/* 06H* 12/14/15  C:308  Project: WC2370                              *
/*   H*                  Single Group Call on Group Select Programs   *
     H*                                                               *
     H*****************************************************************
     FW8563F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D OPTNM           C                   CONST('Zurich File Extract')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZEROS        UPCO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE(P)   OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD
     C                   PARM      40            SRDLEN
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   TIME                    DSDTTM
     C*
     C     STRT          TAG
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   SETON                                        LR
     C                   GOTO      TERM
     C                   ENDIF
     C***********************************
     C*   F10 = SEARCH                  *
     C***********************************
     C     *INKJ         IFEQ      *ON
     C*
     C*
     C                   ENDIF                                                  *INKJ *ON
     C                   ENDIF                                                  *IN11 *ON
     C***************************
     C*   PROCESS REQUEST SCREEN*
     C***************************
     C     SCR01T        TAG
     C*
     C*  INVALID DATE
     C*
     C                   Z-ADD     SCMEDT        SRDTI
B01  C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     SRDTO         SCMEDT
     C                   ENDIF
     C*
     C* Month-End Date must be valid month-end
     C*
     C                   MOVEL     SCMEDT        XXMNTH            2
     C                   SELECT
     C     XXMNTH        WHENEQ    '01'
     C                   MOVEL     '0131'        SCMEDT
     C     XXMNTH        WHENEQ    '02'
     C                   MOVEL     '0229'        SCMEDT
     C                   Z-ADD     SCMEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   MOVEL     '0228'        SCMEDT
     C                   ENDIF
     C     XXMNTH        WHENEQ    '03'
     C                   MOVEL     '0331'        SCMEDT
     C     XXMNTH        WHENEQ    '04'
     C                   MOVEL     '0430'        SCMEDT
     C     XXMNTH        WHENEQ    '05'
     C                   MOVEL     '0531'        SCMEDT
     C     XXMNTH        WHENEQ    '06'
     C                   MOVEL     '0630'        SCMEDT
     C     XXMNTH        WHENEQ    '07'
     C                   MOVEL     '0731'        SCMEDT
     C     XXMNTH        WHENEQ    '08'
     C                   MOVEL     '0831'        SCMEDT
     C     XXMNTH        WHENEQ    '09'
     C                   MOVEL     '0930'        SCMEDT
     C     XXMNTH        WHENEQ    '10'
     C                   MOVEL     '1031'        SCMEDT
     C     XXMNTH        WHENEQ    '11'
     C                   MOVEL     '1130'        SCMEDT
     C     XXMNTH        WHENEQ    '12'
     C                   MOVEL     '1231'        SCMEDT
     C                   ENDSL
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   Z-ADD     SCMEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##MEDT
     C****** Beg Add ***** 12/14/15 ****************************************
     C                   MOVEL     '000'         ##FND
     C****** End Add ***** 12/14/15 ****************************************
     C*
     C                   CALL      'W8563C'                             99
     C                   PARM                    ##MEDT            7
     C****** Beg Add ***** 12/14/15 ****************************************
     C                   PARM                    ##FND             3
     C****** End Add ***** 12/14/15 ****************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   SETON                                        LR
