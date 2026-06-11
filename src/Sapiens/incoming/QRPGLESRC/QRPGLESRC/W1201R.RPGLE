     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W1201R - SYSTEM A/R BATCH CODE MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*       THIS PROGRAM ALLOWS THE USER TO ADD, DELETE, OR CHANGE  *
     H*       SYSTEM A/R BATCH CODES AND DESCRIPTIONS AND TYPES.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*       MENU OPTION                                             *
     H*  CALLS TO:                                                    *
     H*       NONE                                                    *
     H*  PROGRAMMER: MARIE NGUYEN                                     *
     H*  CREATED:    07/02/91                                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*       09      INQUIRY MODE                                    *
     H*       20      SCREEN 1: CODE CANNOT BE BLANK                  *
     H*       20      SCREEN 2: DESCRIPTION CANNOT BE BLANK           *
     H*       21      SCREEN 1: RECORD NOT FOUND FOR SPECIFIED INFO   *
     H*       21      SCREEN 2: TYPE MUST BE 'Y' OR 'N'               *
     H*       40      CHAIN & READ SYSTEM A/R BATCH CODE TABLE FILE   *
     H*       60      GENERAL CHAINS & READS                          *
     H*                                                               *
     H*****************************************************************
     FW1201F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     UF A E           K DISK

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
     C*  SYSTEM A/R BATCH CODE KEY LIST
     C*
     C     SYKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
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
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCCODE
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
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        SYKEY
     C                   MOVEL     SCCODE        SYKEY
     C                   CALL      'W1202R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SYKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     SYKEY         SCCODE
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SYKEY         OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   END
     C*
     C*   CHANGE STATUS
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
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
     C*  THE SYSTEM A/R BATCH CODE MAY NOT BE BLANK
     C*
     C     SCCODE        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCODE        XXKEY
     C     SYKY1         CHAIN     PSY01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PSY01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      SYDESC        SCDESC
     C                   MOVE      SYSTAT        SCSTAT
     C                   MOVE      SYTYPE        SCTYPE
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
     C                   MOVE      *BLANK        SCTYPE
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE DESCRIPTION MAY NOT BE BLANK
     C*
     C     SCDESC        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  TYPE MUST BE 'Y' OR 'N'. 'Y' = POST BATCH ENTRIES TO WCMS
     C*
     C     SCTYPE        IFNE      'Y'
     C     SCTYPE        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCODE        XXKEY
     C     SYKY1         CHAIN     PSY01                              40
     C                   MOVE      UPCO#         SYCO#
     C                   MOVE      *BLANK        SYKEY
     C                   MOVEL     SCCODE        SYKEY
     C                   MOVE      SCDESC        SYDESC
     C                   MOVE      SCTYPE        SYTYPE
     C                   MOVE      SCSTAT        SYSTAT
     C                   MOVE      Q1USER        SYUSER
     C                   MOVE      Q1PGM         SYPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDT
     C*
     C                   Z-ADD     DSTIME        SYTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PSY01
     C                   ELSE
     C                   WRITE     PSY01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCCODE
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPSY01     E            PSY01E
**  TABLE UA
-- ADD --
- UPDATE -
