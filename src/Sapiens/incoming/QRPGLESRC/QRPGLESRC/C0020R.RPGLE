     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0020R - QUOTE LOG MASTER MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Quote Log Master Maintenance       **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   ______ (_____): ___________________ (___/___)            **
     H****************************************************************
     FC0020F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL2    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FCMUPL     IF   E           K DISK
     FCMQLL     UF A E           K DISK
     FCMAGL2    IF   E           K DISK
     FCTABL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QLEFDT                 1      7  0
     D  XXYEAR                 2      3  0
     D                 DS
     D  SA                     1     40
     D                                     DIM(20) ASCEND
     D  SCCC01                 1      2
     D  SCCC02                 3      4
     D  SCCC03                 5      6
     D  SCCC04                 7      8
     D  SCCC05                 9     10
     D  SCCC06                11     12
     D  SCCC07                13     14
     D  SCCC08                15     16
     D  SCCC09                17     18
     D  SCCC10                19     20
     D  SCCC11                21     22
     D  SCCC12                23     24
     D  SCCC13                25     26
     D  SCCC14                27     28
     D  SCCC15                29     30
     D  SCCC16                31     32
     D  SCCC17                33     34
     D  SCCC18                35     36
     D  SCCC19                37     38
     D  SCCC20                39     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPUP02
     I              UPCO#                       UPCO#2

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE KEY LISTS
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY2        KLIST
     C                   KFLD                    SCUNCD
     C*
     C*  TYPE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  QUOTE LOG FILE
     C*
     C     QLKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCQUT#
     C*
     C     QLKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C*
     C*  AGENT MASTER FILE KEY LIST
     C*
     C     AGKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXACC#            5 0
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
     C     *LIKE         DEFINE    QLQUT#        XXQUT#
     C     *LIKE         DEFINE    QLQUT#        XXQUTR
     C                   MOVE      'Y'           XXNRW             1
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              50
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
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C     XXNRW         IFEQ      'N'
     C     QLKEY         DELETE    PQL01                              60
     C                   END
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C     XXNRW         IFEQ      'N'
     C     QLKEY         DELETE    PQL01                              60
     C                   END
     C                   MOVE      'Y'           XXNRW
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCQUT#
     C                   MOVE      *BLANK        SCNEW
     C                   MOVE      *BLANK        SCRNEW
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH QUOTE LOG FILE - QUOTE ORDER - SCREEN 1
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQQUT#            7 0
     C                   MOVE      SCQUT#        QQQUT#
     C                   CALL      'C0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQQUT#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQQUT#        SCQUT#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQQUT#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C*
     C*  FUNCTION 05 = AGENT SEARCH - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   Z-ADD     SCAGT#        QQACC#            5 0
     C                   CALL      'C0623R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQACC#
     C                   PARM                    QQRTC             1
     C                   Z-ADD     QQACC#        SCAGT#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQACC#        OREQ      *ZERO
     C                   MOVE      '1'           *IN70
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH QUOTE LOG FILE BY NAME, SCR01
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       QQQUT#
     C                   CALL      'C0521R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQQUT#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQQUT#        SCQUT#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCQUT#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C*
     C*  SEARCH UNDERWRITERS - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      *BLANK        QQUNCD            3
     C                   MOVEL     SCUNCD        QQUNCD
     C                   CALL      'C0507R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQUNCD
     C                   PARM                    QQRTC             1
     C                   MOVE      QQUNCD        SCUNCD
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQUNCD        OREQ      *BLANK
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN73
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*   CHECK FOR TABLE SEARCH IN COVERAGE CODES
     C*
     C                   Z-ADD     1             X                 2 0
     C     '? '          LOOKUP    SA(X)                                  50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2001R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SA(X)
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C     X             ADD       79            A                 2 0
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN74
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     X             ADD       80            A
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN74
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   COUNTY CODE SEARCH
     C*
     C     SCCNTY        IFEQ      '?'
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCCNTY        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C2221R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCCNTY
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN17
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN18
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
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
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
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
     C*   NEW QUOTE FIELD MUST BE BLANK IN INQUIRY MODE
     C*
     C     *IN09         IFEQ      '1'
     C     SCNEW         ANDNE     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   RENEWAL QUOTE FIELD MUST BE BLANK IN INQUIRY MODE
     C*
     C     *IN09         IFEQ      '1'
     C     SCRNEW        ANDNE     *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE VALUE IN NEW QUOTE FIELD
     C*
     C     *IN09         IFEQ      '0'
     C     SCNEW         ANDNE     'Y'
     C     SCNEW         ANDNE     ' '
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE VALUE IN RENEWAL QUOTE FIELD
     C*
     C     *IN09         IFEQ      '0'
     C     SCRNEW        ANDNE     'Y'
     C     SCRNEW        ANDNE     ' '
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   ONLY ONE FUNCTION CAN BE SELECTED AT A TIME.
     C*
     C     *IN09         IFEQ      '0'
     C     SCNEW         ANDNE     *BLANK
     C     SCRNEW        ANDNE     *BLANK
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMPUTER WILL GENERATE QUOTE NUMBER IF NEW QUOTE
     C*
     C     *IN09         IFEQ      '0'
     C     SCQUT#        ANDNE     *ZERO
     C     SCNEW         ANDEQ     'Y'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   MUST HAVE AN EXISTING QUOTE NUMBER IF RENEWAL QUOTE
     C*
     C     *IN09         IFEQ      '0'
     C     SCQUT#        ANDEQ     *ZERO
     C     SCRNEW        ANDEQ     'Y'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK IF NEW QUOTE
     C*
     C     SCNEW         IFEQ      'Y'
     C*
     C*   GENERATE QUOTE NUMBER AND BLANK OUT FIELDS
     C*
     C     QLKY1         SETGT     PQL01
     C                   READP     PQL01                                  60
     C     *IN60         IFEQ      *OFF
     C     UPCO#         ANDNE     QLCO#
     C                   MOVE      *ON           *IN60
     C                   END
      *
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             SCQUT#
     C*
     C                   ELSE
     C*
     C                   EXCEPT    PQL01E
     C     QLQUT#        ADD       1             SCQUT#
     C                   END
     C*
     C*   WRITE RECORD TO KEEP NEW QUOTE NUMBER
     C*
     C                   MOVE      UPCO#         QLCO#
     C                   MOVE      SCQUT#        QLQUT#
     C                   MOVE      Q1USER        QLUSER
     C                   MOVE      Q1PGM         QLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLDT
     C                   Z-ADD     DSTIME        QLTIME
     C                   WRITE     PQL01
     C                   MOVE      'N'           XXNRW
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCFIN
     C                   MOVE      *BLANK        SCDBA
     C                   MOVE      *BLANK        SCADR1
     C                   MOVE      *BLANK        SCADR2
     C                   MOVE      *BLANK        SCCTY
     C                   MOVE      *BLANK        SCCNTY
     C                   MOVE      *BLANK        SCST
     C                   MOVEL     *BLANK        SCZIP1
     C                   MOVE      *BLANK        SCZIP2
     C                   MOVE      *BLANK        SCEFDT
     C                   MOVE      *BLANK        SCFEIN
     C                   MOVE      *BLANK        SCAGT#
     C                   MOVE      *BLANK        SCORT
     C                   MOVE      *BLANK        SCYIB
     C                   MOVE      *BLANK        SCNOB
     C                   MOVE      *BLANK        SCSTAC
     C                   MOVE      *BLANK        SCPRMQ
     C                   MOVE      *BLANK        SCPFEE
     C                   MOVE      *BLANK        SCPLTY
     C                   MOVE      *BLANK        SCUNCD
     C                   MOVE      *BLANK        SCCC01
     C                   MOVE      *BLANK        SCCC02
     C                   MOVE      *BLANK        SCCC03
     C                   MOVE      *BLANK        SCCC04
     C                   MOVE      *BLANK        SCCC05
     C                   MOVE      *BLANK        SCCC06
     C                   MOVE      *BLANK        SCCC07
     C                   MOVE      *BLANK        SCCC08
     C                   MOVE      *BLANK        SCCC09
     C                   MOVE      *BLANK        SCCC10
     C                   MOVE      *BLANK        SCCC11
     C                   MOVE      *BLANK        SCCC12
     C                   MOVE      *BLANK        SCCC13
     C                   MOVE      *BLANK        SCCC14
     C                   MOVE      *BLANK        SCCC15
     C                   MOVE      *BLANK        SCCC16
     C                   MOVE      *BLANK        SCCC17
     C                   MOVE      *BLANK        SCCC18
     C                   MOVE      *BLANK        SCCC19
     C                   MOVE      *BLANK        SCCC20
     C*
     C                   ELSE
     C*
     C*  CHECK IF RENEWAL QUOTE
     C*
     C     SCRNEW        IFEQ      'Y'
     C*
     C                   Z-ADD     SCQUT#        XXQUT#
     C*
     C*   GET QUOTE FOR UPDATE
     C*
     C     QLKEY         CHAIN     PQL01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   EXCEPT    PQL01E
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      QLFIN         SCFIN
     C                   MOVE      QLDBA         SCDBA
     C                   MOVE      QLADR1        SCADR1
     C                   MOVE      QLADR2        SCADR2
     C                   MOVE      QLCTY         SCCTY
     C                   MOVE      QLCNTY        SCCNTY
     C                   MOVE      QLST          SCST
     C                   MOVEL     QLZIP         SCZIP1
     C                   MOVE      QLZIP         SCZIP2
     C                   ADD       1             XXYEAR
     C                   Z-ADD     QLEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      QLFEIN        SCFEIN
     C                   MOVE      QLAGT#        SCAGT#
     C                   MOVE      QLORT         SCORT
     C                   MOVE      QLYIB         SCYIB
     C                   MOVE      QLNOB         SCNOB
     C                   MOVE      QLSTAC        SCSTAC
     C                   MOVE      *ZERO         SCPRMQ
     C                   MOVE      *ZERO         SCPFEE
     C                   MOVE      QLPLTY        SCPLTY
     C                   MOVE      QLUNCD        SCUNCD
     C                   MOVE      QLCC01        SCCC01
     C                   MOVE      QLCC02        SCCC02
     C                   MOVE      QLCC03        SCCC03
     C                   MOVE      QLCC04        SCCC04
     C                   MOVE      QLCC05        SCCC05
     C                   MOVE      QLCC06        SCCC06
     C                   MOVE      QLCC07        SCCC07
     C                   MOVE      QLCC08        SCCC08
     C                   MOVE      QLCC09        SCCC09
     C                   MOVE      QLCC10        SCCC10
     C                   MOVE      QLCC11        SCCC11
     C                   MOVE      QLCC12        SCCC12
     C                   MOVE      QLCC13        SCCC13
     C                   MOVE      QLCC14        SCCC14
     C                   MOVE      QLCC15        SCCC15
     C                   MOVE      QLCC16        SCCC16
     C                   MOVE      QLCC17        SCCC17
     C                   MOVE      QLCC18        SCCC18
     C                   MOVE      QLCC19        SCCC19
     C                   MOVE      QLCC20        SCCC20
     C*
     C                   END
     C*
     C*   GENERATE QUOTE NUMBER
     C*
     C     QLKY1         SETGT     PQL01
     C                   READP     PQL01                                  60
     C     *IN60         IFEQ      *OFF
     C     UPCO#         ANDNE     QLCO#
     C                   MOVE      *ON           *IN60
     C                   END
      **
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             SCQUT#
     C                   ELSE
     C                   EXCEPT    PQL01E
     C     QLQUT#        ADD       1             SCQUT#
     C                   END
     C                   Z-ADD     SCQUT#        XXQUTR
     C*
     C*   WRITE RECORD TO KEEP NEW QUOTE NUMBER
     C*
     C                   MOVE      UPCO#         QLCO#
     C                   MOVE      SCQUT#        QLQUT#
     C                   MOVE      Q1USER        QLUSER
     C                   MOVE      Q1PGM         QLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLDT
     C                   Z-ADD     DSTIME        QLTIME
     C                   WRITE     PQL01
     C                   MOVE      'N'           XXNRW
     C*
     C*
     C                   ELSE
     C*
     C*   GET QUOTE FOR UPDATE
     C*
     C     QLKEY         CHAIN     PQL01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   EXCEPT    PQL01E
     C                   MOVE      'Y'           XXNRW
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      QLFIN         SCFIN
     C                   MOVE      QLDBA         SCDBA
     C                   MOVE      QLADR1        SCADR1
     C                   MOVE      QLADR2        SCADR2
     C                   MOVE      QLCTY         SCCTY
     C                   MOVE      QLCNTY        SCCNTY
     C                   MOVE      QLST          SCST
     C                   MOVEL     QLZIP         SCZIP1
     C                   MOVE      QLZIP         SCZIP2
     C                   Z-ADD     QLEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      QLFEIN        SCFEIN
     C                   MOVE      QLAGT#        SCAGT#
     C                   MOVE      QLORT         SCORT
     C                   MOVE      QLYIB         SCYIB
     C                   MOVE      QLNOB         SCNOB
     C                   MOVE      QLSTAC        SCSTAC
     C                   MOVE      QLPRMQ        SCPRMQ
     C                   MOVE      QLPFEE        SCPFEE
     C                   MOVE      QLPLTY        SCPLTY
     C                   MOVE      QLUNCD        SCUNCD
     C                   MOVE      QLCC01        SCCC01
     C                   MOVE      QLCC02        SCCC02
     C                   MOVE      QLCC03        SCCC03
     C                   MOVE      QLCC04        SCCC04
     C                   MOVE      QLCC05        SCCC05
     C                   MOVE      QLCC06        SCCC06
     C                   MOVE      QLCC07        SCCC07
     C                   MOVE      QLCC08        SCCC08
     C                   MOVE      QLCC09        SCCC09
     C                   MOVE      QLCC10        SCCC10
     C                   MOVE      QLCC11        SCCC11
     C                   MOVE      QLCC12        SCCC12
     C                   MOVE      QLCC13        SCCC13
     C                   MOVE      QLCC14        SCCC14
     C                   MOVE      QLCC15        SCCC15
     C                   MOVE      QLCC16        SCCC16
     C                   MOVE      QLCC17        SCCC17
     C                   MOVE      QLCC18        SCCC18
     C                   MOVE      QLCC19        SCCC19
     C                   MOVE      QLCC20        SCCC20
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR02B
     C*
     C*  NAMED INSURED CANNOT BE BLANK
     C*
     C     SCFIN         IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADDRESS CANNOT BE BLANK
     C*
     C     SCADR1        IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  CLIENT CITY CANNOT BE BLANK
     C*
     C     SCCTY         IFEQ      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE COUNTY CODE MUST BE VALID
     C*
     C                   MOVEL     SCCNTY        XXKEY
     C     TBKEY         CHAIN     PCY01                              41
     C     *IN41         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C     CYSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE STATE MUST BE VALID
     C*
     C     SCST          LOOKUP    ST                                     50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  ZIP CODE CANNOT BE BLANK
     C*
     C     SCZIP1        IFEQ      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C     SCEFDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   AGENT NUMBER MUST BE VALID
     C*
     C                   MOVE      SCAGT#        XXACC#
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         CABEQ     '1'           STRT                     27
     C*
     C*    ORGANIZATION TYPE MUST BE I, P, C, J, OR O
     C*
     C     SCORT         IFNE      'I'
     C     SCORT         ANDNE     'P'
     C     SCORT         ANDNE     'C'
     C     SCORT         ANDNE     'J'
     C     SCORT         ANDNE     'O'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   STATUS OF ACCOUNT MUST BE 1, 2, 3, 4, 5 OR 6
     C*
     C     SCSTAC        IFNE      '1'
     C     SCSTAC        ANDNE     '2'
     C     SCSTAC        ANDNE     '3'
     C     SCSTAC        ANDNE     '4'
     C     SCSTAC        ANDNE     '5'
     C     SCSTAC        ANDNE     '6'                                          *TMN 7-17-90
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*   UNDERWRITERS CODE MUST BE VALID
     C*
     C     UPKEY2        CHAIN     PUP02                              50
     C     *IN50         CABEQ     '1'           STRT                     33
     C*
     C*  POLICY FEE RANGE IS 0 - $250
     C*
     C     SCPFEE        IFEQ      *ZERO
     C                   GOTO      CONT
     C                   END
     C     SCPFEE        IFLT      0.00
     C     SCPFEE        ORGT      250.00
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C*   TYPE OF POLICY MUST BE CPP, GL-, AL-, CF-, IM-, BU-, FR-
     C*
     C     CONT          TAG
     C     SCPLTY        IFNE      'CPP'
     C     SCPLTY        ANDNE     'GL-'
     C     SCPLTY        ANDNE     'CA-'
     C     SCPLTY        ANDNE     'AL-'
     C     SCPLTY        ANDNE     'CF-'
     C     SCPLTY        ANDNE     'IM-'
     C     SCPLTY        ANDNE     'BU-'
     C     SCPLTY        ANDNE     'FR-'
     C     SCPLTY        ANDNE     'GA-'
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE THE COVERAGE CODE FIELDS
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     20
     C     *BLANK        LOOKUP    SA(X)                              50
     C*
     C     *IN50         IFEQ      '1'
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SA(X)         XXKEY
     C*
     C     TBKEY         CHAIN     PCV01                              50
     C*
     C     *IN50         IFEQ      '1'
     C     X             ADD       79            E                 2 0
     C                   MOVE      '1'           *IN(E)
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C     CVSTAT        IFEQ      'I'
     C     X             ADD       79            E
     C                   MOVE      '1'           *IN(E)
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     20            X
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*   CANNOT HAVE DUPLICATE SECTIONS ATTACHED
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     20
     C     *BLANK        LOOKUP    SA(X)                              50
     C*
     C     *IN50         IFEQ      '1'
     C                   MOVEL     SA(X)         XXCKSA            2
     C                   Z-ADD     1             Z                 2 0
     C     XXCKSA        LOOKUP    SA(Z)                                  50
     C*
     C     *IN50         IFEQ      '1'
     C                   ADD       1             Z
     C     XXCKSA        LOOKUP    SA(Z)                                  50
     C     *IN50         IFEQ      '1'
     C     Z             ADD       79            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     20            X
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     QLKEY         CHAIN     PQL01                              40
     C                   MOVE      UPCO#         QLCO#
     C                   MOVE      SCQUT#        QLQUT#
     C                   MOVE      SCFIN         QLFIN
     C                   MOVE      SCDBA         QLDBA
     C                   MOVE      SCADR1        QLADR1
     C                   MOVE      SCADR2        QLADR2
     C                   MOVE      SCCTY         QLCTY
     C                   MOVE      SCCNTY        QLCNTY
     C                   MOVE      SCST          QLST
     C                   MOVEL     SCZIP1        QLZIP
     C                   MOVE      SCZIP2        QLZIP
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLEFDT
     C                   MOVE      SCFEIN        QLFEIN
     C                   MOVE      SCAGT#        QLAGT#
     C                   MOVE      SCORT         QLORT
     C                   MOVE      SCYIB         QLYIB
     C                   MOVE      SCNOB         QLNOB
     C                   MOVE      SCSTAC        QLSTAC
     C                   MOVE      SCPRMQ        QLPRMQ
     C                   MOVE      SCPFEE        QLPFEE
     C                   MOVE      SCPLTY        QLPLTY
     C                   MOVE      SCUNCD        QLUNCD
     C                   MOVE      SCCC01        QLCC01
     C                   MOVE      SCCC02        QLCC02
     C                   MOVE      SCCC03        QLCC03
     C                   MOVE      SCCC04        QLCC04
     C                   MOVE      SCCC05        QLCC05
     C                   MOVE      SCCC06        QLCC06
     C                   MOVE      SCCC07        QLCC07
     C                   MOVE      SCCC08        QLCC08
     C                   MOVE      SCCC09        QLCC09
     C                   MOVE      SCCC10        QLCC10
     C                   MOVE      SCCC11        QLCC11
     C                   MOVE      SCCC12        QLCC12
     C                   MOVE      SCCC13        QLCC13
     C                   MOVE      SCCC14        QLCC14
     C                   MOVE      SCCC15        QLCC15
     C                   MOVE      SCCC16        QLCC16
     C                   MOVE      SCCC17        QLCC17
     C                   MOVE      SCCC18        QLCC18
     C                   MOVE      SCCC19        QLCC19
     C                   MOVE      SCCC20        QLCC20
     C                   MOVE      Q1USER        QLUSER
     C                   MOVE      Q1PGM         QLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLDT
     C                   Z-ADD     DSTIME        QLTIME
     C*
     C     SCNEW         IFEQ      'Y'
     C                   Z-ADD     *ZERO         QLQUTL
     C                   Z-ADD     *ZERO         QLQUTN
     C                   MOVE      'Y'           XXNRW
     C                   END
     C*
     C     SCRNEW        IFEQ      'Y'
     C                   Z-ADD     XXQUT#        QLQUTL
     C                   MOVE      'Y'           XXNRW
     C                   END
     C**
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PQL01
     C                   ELSE
     C                   WRITE     PQL01
     C                   END
     C*
     C*   UPDATE OLD QUOTE WITH RENEWAL QUOTE
     C*
     C     SCRNEW        IFEQ      'Y'
     C                   Z-ADD     XXQUT#        SCQUT#
     C     QLKEY         CHAIN     PQL01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     XXQUTR        QLQUTN
     C                   UPDATE    PQL01
     C                   END
     C                   END
     C*
     C     SCR02B        TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCQUT#
     C                   MOVE      *BLANK        SCNEW
     C                   MOVE      *BLANK        SCRNEW
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*
     OPQL01     E            PQL01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
