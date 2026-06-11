     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W2430R - REPORT ORDERING FREQUENCY CODE MAINTENANCE
     H*****************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS USERS TO ENTER FREQUENCY    *
     H*               CODES WHICH ARE USED BY THE REPORT ORDERING     *
     H*               SYSTEM.                                         *
     H*                                                               *
     H* CALLED FROM: NONE                                             *
     H* CALLS TO:    NONE                                             *
     H* PROGRAMMER:  15       PROJECT: 59WBZ       09/29/94           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/17/99  C:118  PROJECT: WC577                              *
     H*                   Made V3 screen chgs.                        *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   *IN09   -   INQUIRY MODE                                    *
     H*   *IN40   -   NEW RECORD                                      *
     H*   *IN60   -   GENERAL USAGE                                   *
     H*  SRC01:                                                       *
     H*   *IN20   -   FREQUENCY CODE CANNOT BE BLANK                  *
     H*   *IN21   -   RECORD NOT FOUND                                *
     H*  SRC02:                                                       *
     H*   *IN20   -   DESRCIPTION CANNOT BE BLANK                     *
     H*****************************************************************
     FW2430F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTFQL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
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
     C* KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR THE FREQUENCY CODE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
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
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    FQKEY         XXKEY
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C* GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKY          CHAIN     PUP01                              60
     C  N60              Z-ADD     UPCO#         XXCO#
     C*
     C* GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCCODE
     C*
     C* DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C***** BEGIN CHG **** 08/17/99 ***********************************
     C********** F5 = FREQUENCY CODE SEARCH
     C**********
     C********** *INKE     IFEQ '1'
     C*
     C*          F10=SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C***** BEGIN CHG **** 08/17/99 ***********************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCODE        XXKEY
     C                   CALL      'W2431R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCCODE
     C***** BEGIN DEL **** 08/17/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/17/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C     QQRTC         CABEQ     'B'           STRT
     C     XXKEY         CABEQ     *BLANK        STRT
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C* F12 = PREVIOUS
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C* F20 = CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C* PROCESS HELP REQUESTS
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
     C***** BEGIN DEL **** 08/17/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/17/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C* PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 08/17/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/17/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C*
     C                   END
     C*
     C* GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C* PROCESS SCREEN 1
     C*
     C*
     C     SCR01T        TAG
     C*
     C* THE FREQUENCY CODE CANNOT BE BLANK
     C*
     C     SCCODE        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCODE        XXKEY
     C     TBKY          CHAIN     PFQ01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PFQ01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      FQDESC        SCDESC
     C                   MOVE      FQSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCDESC
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C* PROCESS SCREEN 2
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           END02
     C*
     C* THE FREQUENCY CODE DESCRIPTION CANNOT BE BLANK
     C*
     C     SCDESC        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* ALL EDITS HAVE BEEN PASSED
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCODE        XXKEY
     C     TBKY          CHAIN     PFQ01                              40
     C                   MOVE      XXCO#         FQCO#
     C                   MOVE      *BLANK        FQKEY
     C                   MOVEL     SCCODE        FQKEY
     C                   MOVE      SCDESC        FQDESC
     C                   MOVE      SCSTAT        FQSTAT
     C                   MOVE      Q1USER        FQUSER
     C                   MOVE      Q1PGM         FQPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FQDT
     C                   Z-ADD     DSTIME        FQTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PFQ01
     C                   ELSE
     C                   WRITE     PFQ01
     C                   END
     C*
     C     END02         TAG
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCCODE
     C                   GOTO      STRT
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPFQ01     E            PFQ01E
**  TABLE UA
-- ADD --
- UPDATE -
