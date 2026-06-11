     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SY100R - SYSTEM-WIDE STATE TABLE FILE MAINTENANCE
     H******************************************************************
     H* REVISIONS:
     H*
     H* 03/09/93 C:05  Project: FS100
     H*                Added new fields for: In-State Adj flag
     H*                                      F0020F Screen ID
     H*                                      F0070F Screen ID
     H*                                      5 Generic 1A fields
     H*                                      5 Generic 10A fields
     H*
     H* 04/15/94 C:20  REQ/PROJ: 128/FS127
     H*                Added new fields for: BILL SUB-TYPE
     H*                                      F0070F Screen ID
     H*
     H* 04/19/94 C:20  REQ/PROJ: 128/FS127
     H*                ADDED USE OF ST1A2 FOR USE OF CPT4 CODES ON SCR04
     H*                OF BILL ENTRY
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H*  40    CHAIN ON STABL/PST01                                    *
     H* SCR01:                                                         *
     H*  21    CANNOT INQUIRE ON AN ADD                                *
     H* SCR02:                                                         *
     H*  20    SCM0503- NAME REQUIRED                                  *
     H*  64    DELETE NOT ALLOWED                                      *
     H******************************************************************
     FSY100F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSTABL     UF A E           K DISK

     D UA              S             10    DIM(3) CTDATA PERRCD(1)              ADD/UPD/INQ MSG
     D CD              S             20    DIM(1) CTDATA PERRCD(1)              F23 MSG
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
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    STKEY         STKEY1
     C*
     C*   ENTRY PARM LIST
     C     *ENTRY        PLIST
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
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C********************************************
     C* SCR01 = STATE CODE  SEARCH               *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'SY600R'
     C                   PARM                    SCST
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCST          OREQ      *BLANKS
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C                   END                                                    END *INKE
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
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
     C                   END                                                    END *IN10
     C********************************************
     C*   F20 = CHANGE STATUS                    *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C                   MOVEL     CD(1)         SCCMDD                          |
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE INJURY TYPE CODE          *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVE      *BLANKS       STKEY1
     C                   MOVEL     SCST          STKEY1
     C     STKEY1        DELETE    PST01                              60
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                     END *INKX
     C********************************************
     C                   END                                                    END CMDKEYS
     C*************************************************************************
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
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      *BLANKS       STKEY1
     C                   MOVEL     SCST          STKEY1
     C     STKEY1        CHAIN     PST01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PST01E
     C*
     C*** IF INQUIRY - DISPLAY "INQUIRY" MODE, ELSE DISPLAY "UPDATE" MODE
     C     *IN09         IFEQ      '1'
     C                   MOVE      UA(3)         SCMODE                         INQUIRY
     C                   ELSE
     C                   MOVE      UA(2)         SCMODE                         UPDATE
     C                   END
     C                   MOVE      STNAME        SCNAME
     C                   MOVE      STINST        SCINST
     C                   MOVE      ST20SC        SC20SC
     C                   MOVE      ST70SC        SC70SC
     C*** BEGIN ADD *** 04/15/94 ***********************************************
     C                   MOVEL     STBS01        SCBS01
     C                   MOVEL     STBS02        SCBS02
     C                   MOVEL     STBS03        SCBS03
     C                   MOVEL     STBS04        SCBS04
     C                   MOVEL     STBS05        SCBS05
     C                   MOVEL     STBS06        SCBS06
     C                   MOVEL     STBS07        SCBS07
     C                   MOVEL     STBS08        SCBS08
     C                   MOVEL     STBS09        SCBS09
     C                   MOVEL     STBS10        SCBS10
     C                   MOVEL     ST7001        SC7001
     C                   MOVEL     ST7002        SC7002
     C                   MOVEL     ST7003        SC7003
     C                   MOVEL     ST7004        SC7004
     C                   MOVEL     ST7005        SC7005
     C                   MOVEL     ST7006        SC7006
     C                   MOVEL     ST7007        SC7007
     C                   MOVEL     ST7008        SC7008
     C                   MOVEL     ST7009        SC7009
     C                   MOVEL     ST7010        SC7010
     C*** END ADD *** 04/15/94 *************************************************
     C                   MOVEL     ST1A2         SCCPT4                         ADD 04/19/94
     C*
     C                   MOVE      STSTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C                   MOVEL     CD(1)         SCCMDD                         |
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                         |
     C                   END                                                    |
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCINST
     C                   MOVE      *BLANKS       SC20SC
     C                   MOVE      *BLANKS       SC70SC
     C*** BEGIN ADD *** 04/15/94 ***********************************************
     C                   MOVE      *BLANKS       SCBS01
     C                   MOVE      *BLANKS       SCBS02
     C                   MOVE      *BLANKS       SCBS03
     C                   MOVE      *BLANKS       SCBS04
     C                   MOVE      *BLANKS       SCBS05
     C                   MOVE      *BLANKS       SCBS06
     C                   MOVE      *BLANKS       SCBS07
     C                   MOVE      *BLANKS       SCBS08
     C                   MOVE      *BLANKS       SCBS09
     C                   MOVE      *BLANKS       SCBS10
     C                   MOVE      *BLANKS       SC7001
     C                   MOVE      *BLANKS       SC7002
     C                   MOVE      *BLANKS       SC7003
     C                   MOVE      *BLANKS       SC7004
     C                   MOVE      *BLANKS       SC7005
     C                   MOVE      *BLANKS       SC7006
     C                   MOVE      *BLANKS       SC7007
     C                   MOVE      *BLANKS       SC7008
     C                   MOVE      *BLANKS       SC7009
     C                   MOVE      *BLANKS       SC7010
     C*** END ADD *** 04/15/94 *************************************************
     C                   MOVE      *BLANKS       SCCPT4                          ADD 04/19/94
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*****************
     C     SCR02T        TAG
     C*
     C*  NAME CANNOT BE EQUAL TO BLANKS
     C     SCNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANKS       STKEY1
     C                   MOVEL     SCST          STKEY1
     C     STKEY1        CHAIN     PST01                              40
     C                   MOVE      SCNAME        STNAME
     C                   MOVE      SCSTAT        STSTAT
     C                   MOVE      Q1USER        STUSER
     C                   MOVE      Q1PGM         STPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C                   Z-ADD     DSTIME        STTIME
     C                   MOVE      SCINST        STINST
     C                   MOVE      SC20SC        ST20SC
     C                   MOVE      SC70SC        ST70SC
     C                   MOVE      *BLANKS       ST1A1
     C**********           MOVE *BLANKS   ST1A2            DEL 04/19/94
     C                   MOVE      *BLANKS       ST1A3
     C                   MOVE      *BLANKS       ST1A4
     C                   MOVE      *BLANKS       ST1A5
     C                   MOVE      *BLANKS       ST10A1
     C                   MOVE      *BLANKS       ST10A2
     C                   MOVE      *BLANKS       ST10A3
     C                   MOVE      *BLANKS       ST10A4
     C                   MOVE      *BLANKS       ST10A5
     C*** BEGIN ADD *** 04/15/94 ***********************************************
     C                   MOVEL     SCBS01        STBS01
     C                   MOVEL     SCBS02        STBS02
     C                   MOVEL     SCBS03        STBS03
     C                   MOVEL     SCBS04        STBS04
     C                   MOVEL     SCBS05        STBS05
     C                   MOVEL     SCBS06        STBS06
     C                   MOVEL     SCBS07        STBS07
     C                   MOVEL     SCBS08        STBS08
     C                   MOVEL     SCBS09        STBS09
     C                   MOVEL     SCBS10        STBS10
     C                   MOVEL     SC7001        ST7001
     C                   MOVEL     SC7002        ST7002
     C                   MOVEL     SC7003        ST7003
     C                   MOVEL     SC7004        ST7004
     C                   MOVEL     SC7005        ST7005
     C                   MOVEL     SC7006        ST7006
     C                   MOVEL     SC7007        ST7007
     C                   MOVEL     SC7008        ST7008
     C                   MOVEL     SC7009        ST7009
     C                   MOVEL     SC7010        ST7010
     C*** END ADD *** 04/15/94 *************************************************
     C                   MOVEL     SCCPT4        ST1A2
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PST01
     C                   ELSE
     C                   MOVEL     SCST          STKEY
     C                   MOVEL     Q1USER        STIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STIDT
     C                   WRITE     PST01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCST
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPST01     E            PST01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
**  TABLE CD
F23=Delete
