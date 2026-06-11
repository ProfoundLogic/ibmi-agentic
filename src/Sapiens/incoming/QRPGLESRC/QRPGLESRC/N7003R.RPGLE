     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE N7003R - UNBILLED CHARGES REPORT:PBILLS - PROMPT PROGRAM
     H*****************************************************************
     H*  CREATED 04/05/01 BY C:62 FOR PROJECT AR034                   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FN7003F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)

     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSYR                  11     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
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
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKSUB#
     C*
     C     *LIKE         DEFINE    SMSUB#        KKSUB#
     C     *LIKE         DEFINE    SMCO#         XXCO#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   ENDIF
     C*
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPSUB#        KKSUB#
     C*
     C*  GET THE COMPANY RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      @@PRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*****************************************************************
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*   COMMAND 03 = END OF JOB     *
     C*********************************
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C***********************************
     C*  F10=SEARCH                     *
     C***********************************
     C*
     C     *INKJ         IFEQ      '1'
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     28     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN27                          PC AT SCFLAG
     C                   ELSE
     C                   MOVE      *ON           *IN28                          PC AT SCSUB#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB#
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFLAG'
     C                   MOVE      *ON           *IN26
     C     HFLD          WHENEQ    'SCFRDT'
     C                   MOVE      *ON           *IN29
     C     HFLD          WHENEQ    'SCTODT'
     C                   MOVE      *ON           *IN30
     C     HFLD          WHENEQ    'SCRTYP'
     C                   MOVE      *ON           *IN31
     C                   ENDSL
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ IFEQ 1
     C*
     C***********************************
     C*   PROCESS HELP REQUESTS
     C***********************************
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN11
     C*
     C********** END OF COMMAND KEY SECTION *************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*****************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C***  SUBSCRIBER NUMBER MUST BE VALID, IF NOT BLANK
     C*
     C     SCSUB#        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20                           INVLD SB#
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C***  FLAG MUST EITHER BE 'H', 'R', OR BLANK ...
     C*
     C     SCFLAG        IFNE      *BLANKS
     C     SCFLAG        ANDNE     'H'
     C     SCFLAG        ANDNE     'R'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   VALIDATE THE DATE ENTERED IF DATE RANGE IS REQUESTED
     C*
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ORNE      *ZEROS
     C*
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     22
     C*
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     23
     C*
     C                   MOVE      SCFRDT        SRFRDC            8 0
     C                   MOVE      SCTODT        SRTODC            8 0
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C                   ENDIF                                                  USE DATE
     C*                                                    RANGE?
     C*
     C*   THE REPORT TYPE MUST BE 'D', 'S', OR 'B'
     C*
     C     SCRTYP        IFNE      'D'
     C     SCRTYP        ANDNE     'S'
     C     SCRTYP        ANDNE     'B'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* ALL EDITS HAVE BEEN PASSED -
     C*
     C*****************************************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE STATEMENT.
     C*
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      SCSUB#        ##SUB#
     C                   MOVE      SCFRDT        ##FRDT
     C                   MOVE      SCTODT        ##TODT
     C*
     C                   CALL      'N7003C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FRDT            8
     C                   PARM                    ##TODT            8
     C                   PARM      SCRTYP        ##RTYP            1
     C                   PARM      SCFLAG        ##FLAG            1
     C                   PARM      UPOUTQ        ##OUTQ           10
     C                   PARM      'D'           ##MODE            1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
