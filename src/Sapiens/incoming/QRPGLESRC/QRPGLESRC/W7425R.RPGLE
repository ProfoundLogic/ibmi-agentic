     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*****************************************************************
     H/TITLE W7425R-Prompt-Dates for Agent Commission Amount Breakdown
     F*****************************************************************
     FW7425F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D OPTNM           C                   CONST('Agent Commission -
     D                                     Amount Breakdown -
     D                                     Report')
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    QQPRDS        XXPRDS
     C     *LIKE         DEFINE    QQCALL        XXCALL
     C*_______________________________________________________________
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*_______________________________________________________________
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      QQCALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C     Q1PRMS        IFGE      1
     C     QQPRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  QQPRDS        XXPRDS
     C                   ELSE
     C                   MOVEL(P)  OPTNM         XXPRDS
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   MOVEL(P)  XXPRDS        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   TIME                    DSDTTM
     C*_______________________________________________________________
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C                   CALL      COMAPI        COMPL                  99
     C*
     C                   MOVEL     ##CONM        SCCONM
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL(P)  ##FNAM        SCNAME
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*_______________________________________________________________
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCFDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCTDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C*  F3=EXIT
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*  PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'W0006R'                             99
     C*
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  *INKM=*ON
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'                             99
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10=*ON
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   Validate the "From Date" Entered
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C     SRERR         CABEQ     *ON           STRT                     21
     C*
     C*  Validate the "To Date" Entered
     C*
     C     SCTDT         IFEQ      *ZERO
     C                   Z-ADD     DSDATE        SCTDT
     C                   ENDIF                                                  SCTDT=*ZERO
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C     SRERR         CABEQ     *ON           STRT                     22
     C*
     C*  Validate the Entered Dates for Ascending Order
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT             7 0
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C*
     C     XXFDT         CABGT     XXTDT         STRT                 23
     C*_______________________________________________________________
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCFND         #@FND
     C                   MOVE      SCFDT         ##FDT
     C                   MOVE      SCTDT         ##TDT
     C*
     C                   CALL      'W7425C'
     C                   PARM                    #@CO#             3
     C                   PARM                    #@FND             3
     C                   PARM                    ##FDT             8
     C                   PARM                    ##TDT             8
     C                   PARM                    ##OUTQ           10
     C                   PARM      'D'           ##MODE            1
     C*
     C*_______________________________________________________________
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
