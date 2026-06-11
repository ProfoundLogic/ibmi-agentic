     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKI521R- FIT TAX CODE FILE MAINTENANCE
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    TAX CODE NAME IS REQUIRED                               *
     H* SCR02:                                                         *
     H*  21    DESCRIPTION IS REQUIRED                                 *
     H******************************************************************
     FBKI521F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKTBL     UF A E           K DISK
     FBKSYL1    IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
0195 D SCL             S             80    DIM(1) CTDATA PERRCD(1)
     D FQ              S             15    DIM(5) CTDATA PERRCD(1)
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
     C*   KEY LIST FOR THE TABLE FILE
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
     C                   CALL      'BKD521R'
     C                   PARM                    SCPGNM
     C                   PARM                    X@FREQ            1
     C                   PARM                    X@MS              1
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     X@FREQ        IFNE      *BLANKS
     C                   MOVEL     X@FREQ        SCFREQ
     C                   MOVEL     X@MS          SCMS
     C                   END
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFREQ        OREQ      *BLANKS
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
     C                   MOVEL     'L'           QQRTCD
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
     C*  FREQ  CODE MUST BE NOT EQUAL TO BLANKS  *
     C********************************************
     C*
     C     SCFREQ        IFNE      '1'
     C     SCFREQ        ANDNE     '2'
     C     SCFREQ        ANDNE     '3'
     C     SCFREQ        ANDNE     '4'
     C     SCFREQ        ANDNE     '5'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCFREQ        F                 1 0
     C                   MOVE      FQ(F)         SCFQD
     C*
     C********************************************
     C*  STATUS CODE MUST BE NOT EQUAL TO BLANKS *
     C********************************************
     C*
     C     SCMS          IFNE      '1'
     C     SCMS          ANDNE     '2'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     SCMS          IFEQ      '1'
     C                   MOVEL     'Single  '    SCMSD
     C                   ELSE
     C                   MOVEL     'Married '    SCMSD
     C                   END
     C*
     C                   MOVEL     'FT'          TBLID             2
     C                   MOVE      *BLANKS       TBLVAL           10
     C                   MOVEL     SCFREQ        VAL2              2
     C                   MOVE      SCMS          VAL2
     C                   MOVEL     VAL2          TBLVAL           10
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     TBLKEY        CHAIN     PFT01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PFT01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     FTDESC        SCDESC
     C                   MOVEL     FTALLW        SCALLW
     C                   MOVEL     FTO01         SCFO01
     C                   MOVEL     FTO02         SCFO02
     C                   MOVEL     FTO03         SCFO03
     C                   MOVEL     FTO04         SCFO04
     C                   MOVEL     FTO05         SCFO05
     C                   MOVEL     FTO06         SCFO06
     C                   MOVEL     FTO07         SCFO07
     C                   MOVEL     FTA01         SCFA01
     C                   MOVEL     FTA02         SCFA02
     C                   MOVEL     FTA03         SCFA03
     C                   MOVEL     FTA04         SCFA04
     C                   MOVEL     FTA05         SCFA05
     C                   MOVEL     FTA06         SCFA06
     C                   MOVEL     FTA07         SCFA07
     C                   MOVEL     FTR01         SCFR01
     C                   MOVEL     FTR02         SCFR02
     C                   MOVEL     FTR03         SCFR03
     C                   MOVEL     FTR04         SCFR04
     C                   MOVEL     FTR05         SCFR05
     C                   MOVEL     FTR06         SCFR06
     C                   MOVEL     FTR07         SCFR07
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
     C                   MOVEL     SCFQD         SCDESC
     C                   MOVE      SCMSD         SCDESC
     C                   MOVE      *BLANKS       SCALLW
     C                   MOVE      *BLANKS       SCFO01
     C                   MOVE      *BLANKS       SCFO02
     C                   MOVE      *BLANKS       SCFO03
     C                   MOVE      *BLANKS       SCFO04
     C                   MOVE      *BLANKS       SCFO05
     C                   MOVE      *BLANKS       SCFO06
     C                   MOVE      *BLANKS       SCFO07
     C                   MOVE      *BLANKS       SCFA01
     C                   MOVE      *BLANKS       SCFA02
     C                   MOVE      *BLANKS       SCFA03
     C                   MOVE      *BLANKS       SCFA04
     C                   MOVE      *BLANKS       SCFA05
     C                   MOVE      *BLANKS       SCFA06
     C                   MOVE      *BLANKS       SCFA07
     C                   MOVE      *BLANKS       SCFR01
     C                   MOVE      *BLANKS       SCFR02
     C                   MOVE      *BLANKS       SCFR03
     C                   MOVE      *BLANKS       SCFR04
     C                   MOVE      *BLANKS       SCFR05
     C                   MOVE      *BLANKS       SCFR06
     C                   MOVE      *BLANKS       SCFR07
     C*
     C********************************************
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
     C     *IN09         IFEQ      '0'
     C                   MOVEL     'FT'          TBLID             2
     C     TBLKEY        CHAIN     PFT01                              40
     C*
     C                   MOVE      'FT'          FTTBL
     C                   MOVEL     SCFREQ        VAL2
     C                   MOVE      SCMS          VAL2
     C                   MOVEL     VAL2          FTVAL
     C                   MOVEL     SCDESC        FTDESC
     C                   MOVEL     SCDESC        FTDESC
     C                   MOVEL     SCALLW        FTALLW
     C                   MOVEL     SCFO01        FTO01
     C                   MOVEL     SCFO02        FTO02
     C                   MOVEL     SCFO03        FTO03
     C                   MOVEL     SCFO04        FTO04
     C                   MOVEL     SCFO05        FTO05
     C                   MOVEL     SCFO06        FTO06
     C                   MOVEL     SCFO07        FTO07
     C                   MOVEL     SCFA01        FTA01
     C                   MOVEL     SCFA02        FTA02
     C                   MOVEL     SCFA03        FTA03
     C                   MOVEL     SCFA04        FTA04
     C                   MOVEL     SCFA05        FTA05
     C                   MOVEL     SCFA06        FTA06
     C                   MOVEL     SCFA07        FTA07
     C                   MOVEL     SCFR01        FTR01
     C                   MOVEL     SCFR02        FTR02
     C                   MOVEL     SCFR03        FTR03
     C                   MOVEL     SCFR04        FTR04
     C                   MOVEL     SCFR05        FTR05
     C                   MOVEL     SCFR06        FTR06
     C                   MOVEL     SCFR07        FTR07
     C*
     C                   MOVE      Q1USER        FTCUSR
     C                   MOVEL     Q1PGM         FTIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FTCDT
     C*
     C                   Z-ADD     DSTIME        FTCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PFT01
     C                   ELSE
     C                   MOVE      Q1USER        FTIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FTIDT
     C*
     C                   Z-ADD     DSTIME        FTITIM
     C                   WRITE     PFT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCMS
     C                   MOVE      *BLANKS       SCFREQ
     C                   MOVE      *BLANKS       SCDESC
     C                   GOTO      STRT
     C*
     C********************************************
     C*  END THE PROGRAM                         *
     C********************************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPFT01     E            PFT01E
     OPPP01     E            PPP01E
**  TABLE UA
-- ADD --
- UPDATE -
**   CALLED PROCEDURES
CALL QCMD
**  FREQUENCY
Weekly
Bi-Weekly
Semi-Monthly
Monthly
Daily
