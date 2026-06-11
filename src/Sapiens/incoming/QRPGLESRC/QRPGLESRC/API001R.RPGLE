     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API001R - WORK A/P SYSTEM CONTROLS
     H******************************************************************
     H******************************************************************
     FAPI001F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPAPL1    UF A E           K DISK
     FBKSYL1    IF   E           K DISK

     D SCF             S              1    DIM(24)                              FUNCTION FLAGS
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
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     'BK'          CHAIN     PSY01                              54
     C     'AP'          CHAIN     PAP01                              54
     C     *IN40         IFEQ      '0'
     C                   MOVE      APVDR         SCVDR
     C                   MOVE      APTVDR        SCTVDR
     C                   MOVE      APDEV         SCDEV
     C                   Z-ADD     APCO#         SCCO#
     C                   Z-ADD     APFAC#        SCFAC#
     C                   ELSE
     C                   MOVE      *ZEROS        SCVDR
     C                   MOVE      *ZEROS        SCTVDR
     C                   MOVE      *BLANKS       SCDEV
     C                   Z-ADD     *ZEROS        SCCO#
     C                   Z-ADD     *ZEROS        SCFAC#
     C                   MOVE      *BLANKS       SCGL
     C                   END
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C***                                                              *****
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
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   PROCESS THE SCREEN
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     'AP'          CHAIN     PAP01                              40
     C*
     C                   MOVE      'AP'          APAPP
     C                   MOVE      SCVDR         APVDR
     C                   MOVE      SCTVDR        APTVDR
     C                   MOVE      SCDEV         APDEV
     C                   Z-ADD     SCCO#         APCO#
     C                   Z-ADD     SCFAC#        APFAC#
     C*
     C                   MOVE      Q1USER        APCUSR
     C                   MOVE      Q1PGM         APIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APCDT
     C*
     C                   Z-ADD     DSTIME        APCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PAP01
     C                   ELSE
     C                   MOVE      Q1USER        APIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        APITIM
     C                   WRITE     PAP01
     C                   END
     C*
     C                   MOVEL     '1'           *INLR
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
