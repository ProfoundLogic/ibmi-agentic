     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F7035R - PROMPT FOR R & C PROCEDURE CODE FILE LISTING
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/23/98  C:90  PROJECT: MC181                                *
     H*                 CHANGED F10 TO CALL F2310R INSTEAD            *
     H*                 OF F0527R                                     *
     H*                                                               *
     H* 04/07/98  C:89  PROJECT: MC181                                *
     H*                 CHANGED F6 TO F10 FOR SEARCH                  *
     H*                 VERSION 4 STANDARDS                           *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  20    INVALID CODE SELECTED                                  *
     H*  21    MCR0764-CURSOR MUST BE POSITIONED ON CODE FIELD FOR SEARCH
     H*  22    POSITION CURSOR AT FROM                                *
     H*  23    POSITION CURSOR AT TO                                  *
     H*****************************************************************
     FF7035F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
      **** BEGIN ADD **** 04/22/98 ************************************
     FFMRCP     IF   E             DISK
      **** END   ADD **** 04/22/98 ************************************

     D*
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
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
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 04/07/98 ************************************
     C******************** MOVE Q1USER    SCUSER
      **** END   DEL **** 04/07/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
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
     C*********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*****************************
     C*  F3 = EXIT                *
     C*****************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*****************************
     C*  F10 = CODE SEARCH        *
     C*****************************
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** *INKF     IFEQ '1'
     C     *INKJ         IFEQ      '1'
      **** END   CHG **** 04/07/98 ************************************
      **** BEGIN CHG **** 04/07/98 ************************************
     C******************** Z-ADD0         LIN     20
     C******************** Z-ADD0         COL     20
     C********** Q2CPOS    DIV  256       LIN
     C******************** MVR            COL     20
     C********************
     C********** LIN       IFNE 11
     C******************** MOVE '1'       *IN21
     C******************** GOTO STRT
     C******************** END
     C     HFLD          IFEQ      'SCFROM'
     C     HFLD          OREQ      'SCTO'
      **** END   CHG **** 04/07/98 ************************************
     C*
      **** BEGIN CHG **** 04/22/98 ************************************
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     HFLD          IFEQ      'SCFROM'
     C     *INKL         CABEQ     *ON           STRT                     22
     C                   ELSE
     C     *INKL         CABEQ     *ON           STRT                     23
     C                   ENDIF                                                  IFEQ 'SCFROM'
      *
     C     ##RRN         CHAIN     PRC01                              24
      **** BEGIN ADD **** 04/23/98 ************************************
     C     *IN24         IFEQ      *ON
     C     HFLD          CABEQ     'SCFROM'      STRT                     22
     C     HFLD          CABEQ     'SCTO'        STRT                     23
     C                   ELSE
     C     HFLD          IFEQ      'SCFROM'
     C                   MOVE      RCCPT4        SCFROM
     C                   MOVE      *ON           *IN23
     C                   ELSE
     C                   MOVE      RCCPT4        SCTO
     C                   MOVE      *ON           *IN22
     C                   ENDIF                                                  IFEQ 'SCFROM'
      **** END   ADD **** 04/23/98 ************************************
     C                   GOTO      STRT
     C******************** MOVE *BLANKS   QQZIPA  3        ADD 02/13/92
     C******************** MOVE SCFROM    QQCPT4  5
     C******************** Z-ADD*ZEROS    QQEFDT  80
     C******************** CALL 'F0527R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           QQZIPA           ADD 02/13/92
     C******************** PARM           QQCPT4
     C******************** PARM           QQEFDT
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C********** QQRTCD    IFEQ 'B'
     C********** QQCPT4    OREQ *ZERO
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 38
     C********** COL       ANDLE42
     C********** HFLD      IFEQ 'SCFROM'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE '1'       *IN22
     C******************** ELSE
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 47
     C********** COL       ANDLE51
     C********** HFLD      IFEQ 'SCTO'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE '1'       *IN23
     C******************** END                             HFLD IFEQ
     C******************** END                             HFLD IFEQ
     C******************** END                             END EQ B/ZERO
     C*
     C********** QQRTCD    IFEQ '*'
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 38
     C********** COL       ANDLE42
     C********** HFLD      IFEQ 'SCFROM'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE QQCPT4    SCFROM
     C******************** MOVE '1'       *IN23
     C******************** ELSE
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 47
     C********** COL       ANDLE51
     C********** HFLD      IFEQ 'SCTO'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE QQCPT4    SCTO
     C******************** MOVE '1'       *IN23
     C******************** END                             HFLD IFEQ
     C******************** END                             HFLD IFEQ
     C                   END                                                    IFEQ *IN24  '
     C******************** GOTO STRT
     C*
      **** END   CHG **** 04/22/98 ************************************
     C                   END                                                    HFLD 'SCFROM'
      **** BEGIN CHG **** 04/07/98 ************************************
     C******************** END                             END *INKF
     C                   END                                                    END *INKJ
      **** END   CHG **** 04/07/98 ************************************
     C*****************************
     C*  *IN10 = HELP             *
     C*****************************
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
     C                   END                                                    IFEQ *IN10
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    IFEQ *INKM
     C*
     C                   END                                                    IFEQ *IN11
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   SCFROM MUST BE LESS THAN OR EQUAL TO  SCTO
     C*
     C     SCTO          CABLT     SCFROM        STRT                   20
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#             3
     C                   CALL      'F7035C'
     C                   PARM                    X@CO#
     C                   PARM                    SCFROM
     C                   PARM                    SCTO
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
