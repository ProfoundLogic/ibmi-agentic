     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKTB99R- USER CODE MAINTENANCE
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    SOURCE NAME IS REQUIRED                                 *
     H* SCR02:                                                         *
     H*  21    DESCRIPTION IS REQUIRED                                 *
     H******************************************************************
     FBKTB99F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKTBL     UF A E           K DISK
     FBKSYL1    IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
0195 D SCL             S             80    DIM(1) CTDATA PERRCD(1)
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
     C*   KEY LIST FOR THE USER PROFILE FILE.
     C*
     C     TBLKEY        KLIST
     C                   KFLD                    TBLID
     C                   KFLD                    TBLVAL
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     'PP'          TBLID             2
     C     'BK'          CHAIN     PSY01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVEL     SYNAME        SCCONM
     C                   ELSE
     C                   MOVEL     '*SETUP *'    SCCONM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL     'PP'          TBLID             2
     C                   MOVEL     Q1PGM         TBLVAL
     C     TBLKEY        CHAIN     PPP01                              54
     C     *IN54         IFEQ      '0'
     C                   EXCEPT    PPP01E
     C                   MOVEL     PPDESC        SCPGNM
     C                   MOVEL     PPCLPW        XXCLPW           10
     C                   MOVEL     PP20PN        XX20PN           10
     C                   MOVEL     PP20PM        XX20PM            1
     C                   MOVEL     PP20PW        XX20PW           10
     C********************************************
     C* PPPW  = IF PASSWORD IS REQUIRED          *
     C********************************************
     C     PPPW          IFNE      *BLANK
     C                   CALL      'BKSE99R'
     C                   PARM                    SCCONM
     C                   PARM                    SCPGNM
     C                   PARM                    PPPW
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END PPPW
     C********************************************
     C                   ELSE
     C                   MOVE      *BLANK        XXCLPW           10
     C                   MOVE      *BLANK        XX20PW           10
     C                   MOVE      *BLANK        XX20PM            1
     C                   MOVE      *BLANK        XX20PW           10
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
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
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C********************************************
     C* SCR01 = TABLE FILE SEARCH                *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKTI99R'
     C                   PARM                    SCPGNM
     C                   PARM                    X@UPTC           10
     C                   PARM      QQCALL        QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     X@UPTC        IFGT      *BLANKS
     C                   MOVEL     X@UPTC        SCUPTC
     C                   END
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCUPTC        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C                   END                                                    END *INKE
     C*
     C********************************************
     C*   F20 = HOT KEY PROGRAM REQUEST          *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     XX20PN        ANDNE     *BLANK
     C********************************************
     C*  VALIDATE PASSWORD IF REQUIRED           *
     C********************************************
     C     XX20PW        IFNE      *BLANK
     C                   MOVEL     'L'           QQRTC
     C                   CALL      'BKSE99R'
     C                   PARM                    SCCONM
     C                   PARM                    SCPGNM
     C                   PARM                    XX20PW
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'L'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END XX20PW
     C********************************************
     C* F20 = CALL REQUESTED PROGRAM             *
     C********************************************
     C*
     C                   CALL      XX20PN
     C                   PARM      XX20PM        QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END XXPN
     C*
     C********************************************
     C*   F23 = COMMAND LINE REQUEST             *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C********************************************
     C*   PW  = IF PASSWORD IS REQUIRED          *
     C********************************************
     C     XXCLPW        IFNE      *BLANK
     C                   MOVEL     'L'           QQRTCD
     C                   CALL      'BKSE99R'
     C                   PARM                    SCCONM
     C                   PARM                    SCPGNM
     C                   PARM                    XXCLPW
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'L'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   Z-ADD     9             SCLSTR           15 5
     C                   CALL      'QCMDEXC '
     C                   PARM                    SCL(1)
     C                   PARM                    SCLSTR
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END COMMAND
     C**
     C                   END                                                    END CMDKEYS
     C**                                                               ********
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
     C********************************************
     C*  PROGRAM NAME MUST BE NOT EQUAL TO BLANKS*
     C********************************************
     C*
     C     SCUPTC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     'UP'          TBLID             2
     C                   MOVE      *BLANKS       TBLVAL
     C                   MOVEL     SCUPTC        TBLVAL           10
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     TBLKEY        CHAIN     PUP01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PUP01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     UPDESC        SCDESC
     C                   MOVEL     UPAU01        SCAU01
     C                   MOVEL     UPAU02        SCAU02
     C                   MOVEL     UPAU03        SCAU03
     C                   MOVEL     UPAU04        SCAU04
     C                   MOVEL     UPAU05        SCAU05
     C                   MOVEL     UPAU06        SCAU06
     C                   MOVEL     UPAU07        SCAU07
     C                   MOVEL     UPAU08        SCAU08
     C                   MOVEL     UPAU09        SCAU09
     C                   MOVEL     UPAU10        SCAU10
     C*
     C********************************************
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCAU01
     C                   MOVE      *BLANKS       SCAU02
     C                   MOVE      *BLANKS       SCAU03
     C                   MOVE      *BLANKS       SCAU04
     C                   MOVE      *BLANKS       SCAU05
     C                   MOVE      *BLANKS       SCAU06
     C                   MOVE      *BLANKS       SCAU07
     C                   MOVE      *BLANKS       SCAU08
     C                   MOVE      *BLANKS       SCAU09
     C                   MOVE      *BLANKS       SCAU10
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C********************************************
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS   *
     C********************************************
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     TBLKEY        CHAIN     PUP01                              40
     C*
     C                   MOVE      'UP'          UPTBL
     C                   MOVEL     SCUPTC        UPVAL
     C                   MOVEL     SCDESC        UPDESC
     C                   MOVEL     SCAU01        UPAU01
     C                   MOVEL     SCAU02        UPAU02
     C                   MOVEL     SCAU03        UPAU03
     C                   MOVEL     SCAU04        UPAU04
     C                   MOVEL     SCAU05        UPAU05
     C                   MOVEL     SCAU06        UPAU06
     C                   MOVEL     SCAU07        UPAU07
     C                   MOVEL     SCAU08        UPAU08
     C                   MOVEL     SCAU09        UPAU09
     C                   MOVEL     SCAU10        UPAU10
     C*
     C                   MOVE      Q1USER        UPCUSR
     C                   MOVEL     Q1PGM         UPIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPCDT
     C*
     C                   Z-ADD     DSTIME        UPCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PUP01
     C                   ELSE
     C                   MOVE      Q1USER        UPIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPIDT
     C*
     C                   Z-ADD     DSTIME        UPITIM
     C                   WRITE     PUP01
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCUPTC
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCAU01
     C                   MOVE      *BLANKS       SCAU02
     C                   MOVE      *BLANKS       SCAU03
     C                   MOVE      *BLANKS       SCAU04
     C                   MOVE      *BLANKS       SCAU05
     C                   MOVE      *BLANKS       SCAU06
     C                   MOVE      *BLANKS       SCAU07
     C                   MOVE      *BLANKS       SCAU08
     C                   MOVE      *BLANKS       SCAU09
     C                   MOVE      *BLANKS       SCAU10
     C                   GOTO      STRT
     C*
     C********************************************
     C*  END THE PROGRAM                         *
     C********************************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPUP01     E            PUP01E
     OPPP01     E            PPP01E
**  TABLE UA
-- ADD --
- UPDATE -
**   CALLED PROCEDURES
CALL QCMD
