     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKI524R- WORKERS COMP RATE MAINT
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    TAX CODE NAME IS REQUIRED                               *
     H* SCR02:                                                         *
     H*  21    DESCRIPTION IS REQUIRED                                 *
     H******************************************************************
     FBKI524F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FPRWCP     UF A E           K DISK
     FBKSYL1    IF   E           K DISK
     FBKTBL     IF   E           K DISK
     FBKCFL1    IF   E           K DISK

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
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     WCKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C     COKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C     TBLKEY        KLIST
     C                   KFLD                    TBLID             2
     C                   KFLD                    TBLVAL           10
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
     C                   CALL      'BKD524R'
     C                   PARM                    SCPGNM
     C                   PARM                    X@CO#             3 0
     C                   PARM                    X@FAC#            3 0
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     X@CO#         IFNE      *ZEROS
     C                   MOVEL     X@CO#         SCCO#
     C                   MOVEL     X@FAC#        SCFAC#
     C                   END
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCCO#         OREQ      *ZERO
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
     C*  VALIDATE ENTRIES                        *
     C********************************************
     C*
     C     COKEY         CHAIN     PCF01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     WCKEY         CHAIN     PWC01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PWC01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      WCRT1         SCRT1
     C                   MOVE      WCRT2         SCRT2
     C                   MOVE      WCRT3         SCRT3
     C                   MOVE      WCRT4         SCRT4
     C                   MOVE      WCRT5         SCRT5
     C                   MOVE      WCRT6         SCRT6
     C                   MOVE      WCRT7         SCRT7
     C                   MOVE      WCRT8         SCRT8
     C                   MOVE      WCRT9         SCRT9
     C                   MOVE      WCDP1         SCDP1
     C                   MOVE      WCDP2         SCDP2
     C                   MOVE      WCDP3         SCDP3
     C                   MOVE      WCDP4         SCDP4
     C                   MOVE      WCDP5         SCDP5
     C                   MOVE      WCDP6         SCDP6
     C                   MOVE      WCDP7         SCDP7
     C                   MOVE      WCDP8         SCDP8
     C                   MOVE      WCDP9         SCDP9
     C                   MOVE      WCAC1         SCAC1
     C                   MOVE      WCAC2         SCAC2
     C                   MOVE      WCAC3         SCAC3
     C                   MOVE      WCAC4         SCAC4
     C                   MOVE      WCAC5         SCAC5
     C                   MOVE      WCAC6         SCAC6
     C                   MOVE      WCAC7         SCAC7
     C                   MOVE      WCAC8         SCAC8
     C                   MOVE      WCAC9         SCAC9
     C                   MOVE      WCSB1         SCSB1
     C                   MOVE      WCSB2         SCSB2
     C                   MOVE      WCSB3         SCSB3
     C                   MOVE      WCSB4         SCSB4
     C                   MOVE      WCSB5         SCSB5
     C                   MOVE      WCSB6         SCSB6
     C                   MOVE      WCSB7         SCSB7
     C                   MOVE      WCSB8         SCSB8
     C                   MOVE      WCSB9         SCSB9
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
     C                   MOVE      *BLANKS       SCRT1
     C                   MOVE      *BLANKS       SCRT2
     C                   MOVE      *BLANKS       SCRT3
     C                   MOVE      *BLANKS       SCRT4
     C                   MOVE      *BLANKS       SCRT5
     C                   MOVE      *BLANKS       SCRT6
     C                   MOVE      *BLANKS       SCRT7
     C                   MOVE      *BLANKS       SCRT8
     C                   MOVE      *BLANKS       SCRT9
     C                   MOVEL     *BLANKS       SCDP1
     C                   MOVEL     *BLANKS       SCDP2
     C                   MOVEL     *BLANKS       SCDP3
     C                   MOVEL     *BLANKS       SCDP4
     C                   MOVEL     *BLANKS       SCDP5
     C                   MOVEL     *BLANKS       SCDP6
     C                   MOVEL     *BLANKS       SCDP7
     C                   MOVEL     *BLANKS       SCDP8
     C                   MOVEL     *BLANKS       SCDP9
     C                   MOVEL     *BLANKS       SCAC1
     C                   MOVEL     *BLANKS       SCAC2
     C                   MOVEL     *BLANKS       SCAC3
     C                   MOVEL     *BLANKS       SCAC4
     C                   MOVEL     *BLANKS       SCAC5
     C                   MOVEL     *BLANKS       SCAC6
     C                   MOVEL     *BLANKS       SCAC7
     C                   MOVEL     *BLANKS       SCAC8
     C                   MOVEL     *BLANKS       SCAC9
     C                   MOVEL     *BLANKS       SCSB1
     C                   MOVEL     *BLANKS       SCSB2
     C                   MOVEL     *BLANKS       SCSB3
     C                   MOVEL     *BLANKS       SCSB4
     C                   MOVEL     *BLANKS       SCSB5
     C                   MOVEL     *BLANKS       SCSB6
     C                   MOVEL     *BLANKS       SCSB7
     C                   MOVEL     *BLANKS       SCSB8
     C                   MOVEL     *BLANKS       SCSB9
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
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     WCKEY         CHAIN     PWC01                              40
     C*
     C                   MOVE      SCCO#         WCCO#
     C                   MOVE      SCFAC#        WCFAC#
     C                   MOVE      SCRT1         WCRT1
     C                   MOVE      SCRT2         WCRT2
     C                   MOVE      SCRT3         WCRT3
     C                   MOVE      SCRT4         WCRT4
     C                   MOVE      SCRT5         WCRT5
     C                   MOVE      SCRT6         WCRT6
     C                   MOVE      SCRT7         WCRT7
     C                   MOVE      SCRT8         WCRT8
     C                   MOVE      SCRT9         WCRT9
     C                   MOVE      SCDP1         WCDP1
     C                   MOVE      SCDP2         WCDP2
     C                   MOVE      SCDP3         WCDP3
     C                   MOVE      SCDP4         WCDP4
     C                   MOVE      SCDP5         WCDP5
     C                   MOVE      SCDP6         WCDP6
     C                   MOVE      SCDP7         WCDP7
     C                   MOVE      SCDP8         WCDP8
     C                   MOVE      SCDP9         WCDP9
     C                   MOVE      SCAC1         WCAC1
     C                   MOVE      SCAC2         WCAC2
     C                   MOVE      SCAC3         WCAC3
     C                   MOVE      SCAC4         WCAC4
     C                   MOVE      SCAC5         WCAC5
     C                   MOVE      SCAC6         WCAC6
     C                   MOVE      SCAC7         WCAC7
     C                   MOVE      SCAC8         WCAC8
     C                   MOVE      SCAC9         WCAC9
     C                   MOVE      SCSB1         WCSB1
     C                   MOVE      SCSB2         WCSB2
     C                   MOVE      SCSB3         WCSB3
     C                   MOVE      SCSB4         WCSB4
     C                   MOVE      SCSB5         WCSB5
     C                   MOVE      SCSB6         WCSB6
     C                   MOVE      SCSB7         WCSB7
     C                   MOVE      SCSB8         WCSB8
     C                   MOVE      SCSB9         WCSB9
     C*
     C                   MOVE      Q1USER        WCCUSR
     C                   MOVEL     Q1PGM         WCIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WCCDT
     C*
     C                   Z-ADD     DSTIME        WCCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PWC01
     C                   ELSE
     C                   MOVE      Q1USER        WCIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WCIDT
     C*
     C                   Z-ADD     DSTIME        WCITIM
     C                   WRITE     PWC01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   GOTO      STRT
     C*
     C********************************************
     C*  END THE PROGRAM                         *
     C********************************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPWC01     E            PWC01E
**  TABLE UA
-- ADD --
- UPDATE -
**   CALLED PROCEDURES
CALL QCMD
