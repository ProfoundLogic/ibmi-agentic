     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKI010R - CHART OF ACCOUNTS MASTER
     H******************************************************************
     H******************************************************************
     FBKXLATF   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FXLATE     UF A E           K DISK
     FBKSYL1    IF   E           K DISK

     D UA              S             10    DIM(3) CTDATA PERRCD(1)
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
     C**         *ENTRY    PLIST
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
     C                   MOVE      ' '           QQTYPE            1
     C                   MOVE      ' '           QQCALL            1
     C*
     C*   KEY LIST FOR THE CHART OF ACCOUNTS
     C*
     C     GLKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCGLD#
     C                   KFLD                    SCACC#
     C                   KFLD                    SCSUB#
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C     QQTYPE        COMP      'G'                                    08
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     'BK'          CHAIN     PSY01                              54
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
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
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
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXFAC#
     C                   MOVE      *ZEROS        XXGLD#
     C                   MOVE      *ZEROS        XXACC#
     C                   MOVE      *ZEROS        XXSUB#
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKDLATE'
     C                   PARM                    SCTYPE            1
     C                   PARM                    XXCO#             3 0
     C                   PARM                    XXFAC#            3 0
     C                   PARM                    XXGLD#            3 0
     C                   PARM                    XXACC#            3 0
     C                   PARM                    XXSUB#            3 0
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      ' '
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     XXCO#         IFGT      *ZERO
     C                   MOVE      XXCO#         SCCO#
     C                   MOVE      XXFAC#        SCFAC#
     C                   MOVE      XXGLD#        SCGLD#
     C                   MOVE      XXACC#        SCACC#
     C                   MOVE      XXSUB#        SCSUB#
     C                   END
     C                   GOTO      SCR01T
     C                   END
     C                   END                                                    END SCR01
     C*
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
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C********************************************
     C*   F10 = DELETE RECORD                    *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     QQCALL        ANDEQ     'D'
     C     GLKEY         CHAIN     PXL01                              40
     C     *IN40         IFEQ      '0'
     C                   DELETE    PXL01
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
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
     C     GLKEY         CHAIN     PXL01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PXL01E
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C                   MOVE      XLCO#         SCCO#
     C                   MOVE      XLFAC#        SCFAC#
     C                   MOVE      XLDESC        SCDESC
     C                   MOVE      XLGLD#        SCGLD#
     C                   MOVE      XLACC#        SCACC#
     C                   MOVE      XLSUB#        SCSUB#
     C                   MOVE      XLOLD         SCOLD
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCOLD
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
     C     GLKEY         CHAIN     PXL01                              40
     C*
     C                   MOVE      SCCO#         XLCO#
     C                   MOVE      SCFAC#        XLFAC#
     C                   MOVE      SCDESC        XLDESC
     C                   MOVE      SCGLD#        XLGLD#
     C                   MOVE      SCACC#        XLACC#
     C                   MOVE      SCSUB#        XLSUB#
     C                   MOVE      SCOLD         XLOLD
     C                   MOVE      Q1USER        XLCUSR
     C                   MOVE      Q1PGM         XLIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XLCDT
     C*
     C                   Z-ADD     DSTIME        XLCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PXL01
     C                   ELSE
     C                   MOVE      Q1USER        XLIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XLIDT
     C*
     C                   Z-ADD     DSTIME        XLITIM
     C                   WRITE     PXL01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPXL01     E            PXL01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
