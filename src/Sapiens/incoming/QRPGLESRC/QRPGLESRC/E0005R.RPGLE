     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE E0005R - USER PARAMETER FILE MAINTENANCE
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 11/22/04  C:33  Seq#: EX071/44603                             *
     H*                 Allow user name to be changed.                *
     H*****************************************************************
     FE0005F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FEMUPL     UF A E           K DISK
     FSMCOL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D***  Begin Add  ***  11/22/04  **************************************
     D OPTNM           C                   CONST('User Parameters')
     D***  End   Add  ***  11/22/04  **************************************
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
     C*   PARM LIST FOR INPUT DATA.
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
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR            10
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C***  Begin Chg  ***  11/22/04  **************************************
     C**********           MOVE QQPRDS    SCPRDS
     C     Q1PRMS        IFGE      1
     C     QQPRDS        ANDNE     *BLANKS
     C                   MOVEL     QQPRDS        ##DATA
     C                   ELSE
     C                   MOVEL     OPTNM         ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  End   Chg  ***  11/22/04  **************************************
     C*
     C***  Begin Add  ***  11/22/04  **************************************
     C     *LIKE         DEFINE    QQCALL        XXCALL
     C                   CLEAR                   XXCALL
     C*
     C     Q1PRMS        IFGE      2
     C***  End   Add  ***  11/22/04  **************************************
     C     QQCALL        COMP      '1'                                    09
     C***  Begin Add  ***  11/22/04  **************************************
     C                   MOVE      QQCALL        XXCALL
     C                   ENDIF
     C***  End   Add  ***  11/22/04  **************************************
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
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 10 = SEARCH LIST CALL
     C*
     C     *INKJ         IFEQ      '1'
     C                   CALL      'E0505R'
     C***  Begin Chg  ***  11/22/04  **************************************
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C                   PARM      SCPRDS        XXPRDS           40
     C                   PARM                    XXCALL
     C***  End   Chg  ***  11/22/04  **************************************
     C                   PARM                    SCUSR
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCUSR         OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  GOTO THE SYSTEM FOR THE USER PROFILE INFO
     C*
     C                   CALL      'S0002C'
     C                   PARM                    SCUSR
     C                   PARM                    X1USNM           50
     C                   PARM      ' '           XXFOND            1
     C*
     C     XXFOND        IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCUSR         XXUSR
     C     UPKEY         CHAIN     PUP01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PUP01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      UPCO#         SCCO#
     C     UPUSNM        IFGT      *BLANK
     C                   MOVE      UPUSNM        SCUSNM
     C                   ELSE
     C                   MOVE      X1USNM        SCUSNM
     C                   END
     C                   MOVE      UPTITL        SCTITL
     C                   MOVE      UPSTAF        SCSTAF
     C                   MOVE      UPOUTQ        SCOUTQ
     C                   MOVE      UPIPGM        SCIPGM
     C                   Z-ADD     UPLVL         SCLVL
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C*
     C                   MOVE      UPTYPE        SCTYPE
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *ZERO         SCCO#
     C                   MOVE      X1USNM        SCUSNM
     C                   MOVE      *BLANK        SCTITL
     C                   MOVE      *BLANK        SCSTAF
     C                   MOVE      *BLANK        SCOUTQ
     C                   MOVE      *ZERO         SCDT
     C                   MOVE      *BLANK        SCIPGM
     C                   Z-ADD     *ZERO         SCLVL
     C                   MOVE      *BLANK        SCTYPE
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C***  Begin Add  ***  11/22/04  **************************************
     C*
     C*  User Name must not be blank
     C*
     C     SCUSNM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   END
     C***  End   Add  ***  11/22/04  **************************************
     C*
     C*  THE TITLE MUST NOT BE BLANK
     C*
     C     SCTITL        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE COMPANY NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         CABEQ     '1'           STRT                     23
     C*
     C*   CHECK THE PROCESS DATE FOR A VALID DATE
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE STAFF CODE MUST NOT BE BLANK
     C*
     C     SCSTAF        CABEQ     *BLANK        STRT                     21
     C*
     C*  CHECK THE OUTQ FOR A VALID VALUE
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C     XXFOND        IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     UPKEY         CHAIN     PUP01                              40
     C                   MOVE      SCUSR         UPUSR
     C                   MOVE      SCCO#         UPCO#
     C                   MOVE      SCUSNM        UPUSNM
     C                   MOVE      SCTITL        UPTITL
     C                   MOVE      SCSTAF        UPSTAF
     C                   MOVE      SCOUTQ        UPOUTQ
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C*
     C                   MOVE      *BLANK        UPSTAT
     C                   MOVE      Q1USER        UPUSER
     C                   MOVE      Q1PGM         UPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C*
     C                   Z-ADD     DSTIME        UPTIME
     C                   MOVE      SCIPGM        UPIPGM
     C                   Z-ADD     SCLVL         UPLVL
     C                   MOVE      SCTYPE        UPTYPE
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PUP01
     C                   ELSE
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPIDT
     C*
     C                   MOVE      Q1USER        UPIUSR
     C                   WRITE     PUP01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCUSR
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPUP01     E            PUP01E
**  TABLE UA
-- ADD --
- UPDATE -
