     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API011R - VENDOR DISTRIBUTION FILE
     H******************************************************************
     FAPI011F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPVDP     UF A E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  D                      1     27  0
     D                                     DIM(9)
     D  SCGLD1                 1      3  0
     D  SCGLD2                 4      6  0
     D  SCGLD3                 7      9  0
     D  SCGLD4                10     12  0
     D  SCGLD5                13     15  0
     D  SCGLD6                16     18  0
     D  SCGLD7                19     21  0
     D  SCGLD8                22     24  0
     D  SCGLD9                25     27  0
     D                 DS
     D  A                      1     27  0
     D                                     DIM(9)
     D  SCACC1                 1      3  0
     D  SCACC2                 4      6  0
     D  SCACC3                 7      9  0
     D  SCACC4                10     12  0
     D  SCACC5                13     15  0
     D  SCACC6                16     18  0
     D  SCACC7                19     21  0
     D  SCACC8                22     24  0
     D  SCACC9                25     27  0
     D                 DS
     D  S                      1     27  0
     D                                     DIM(9)
     D  SCSUB1                 1      3  0
     D  SCSUB2                 4      6  0
     D  SCSUB3                 7      9  0
     D  SCSUB4                10     12  0
     D  SCSUB5                13     15  0
     D  SCSUB6                16     18  0
     D  SCSUB7                19     21  0
     D  SCSUB8                22     24  0
     D  SCSUB9                25     27  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQVDR
     C                   PARM                    XXGLD#
     C                   PARM                    XXACC#
     C                   PARM                    XXSUB#
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
     C     *LIKE         DEFINE    VDVDR         QQVDR
     C*
     C                   Z-ADD     *ZERO         D
     C                   Z-ADD     *ZERO         A
     C                   Z-ADD     *ZERO         S
     C                   TIME                    DSDTTM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     QQVDR         CHAIN     PVD01                              40
     C*
     C                   MOVE      XXGLD#        SCGLD1
     C                   MOVE      XXACC#        SCACC1
     C                   MOVE      XXSUB#        SCSUB1
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PVD01E
     C*
     C                   MOVE      VDGLD1        SCGLD1
     C                   MOVE      VDACC1        SCACC1
     C                   MOVE      VDSUB1        SCSUB1
     C                   MOVE      VDPCT1        SCPCT1
     C                   MOVE      VDAMT1        SCAMT1
     C                   MOVE      VDDES1        SCDES1
     C*
     C                   MOVE      VDGLD2        SCGLD2
     C                   MOVE      VDACC2        SCACC2
     C                   MOVE      VDSUB2        SCSUB2
     C                   MOVE      VDPCT2        SCPCT2
     C                   MOVE      VDAMT2        SCAMT2
     C                   MOVE      VDDES2        SCDES2
     C*
     C                   MOVE      VDGLD3        SCGLD3
     C                   MOVE      VDACC3        SCACC3
     C                   MOVE      VDSUB3        SCSUB3
     C                   MOVE      VDPCT3        SCPCT3
     C                   MOVE      VDAMT3        SCAMT3
     C                   MOVE      VDDES3        SCDES3
     C*
     C                   MOVE      VDGLD4        SCGLD4
     C                   MOVE      VDACC4        SCACC4
     C                   MOVE      VDSUB4        SCSUB4
     C                   MOVE      VDPCT4        SCPCT4
     C                   MOVE      VDAMT4        SCAMT4
     C                   MOVE      VDDES4        SCDES4
     C*
     C                   MOVE      VDGLD5        SCGLD5
     C                   MOVE      VDACC5        SCACC5
     C                   MOVE      VDSUB5        SCSUB5
     C                   MOVE      VDPCT5        SCPCT5
     C                   MOVE      VDAMT5        SCAMT5
     C                   MOVE      VDDES5        SCDES5
     C*
     C                   MOVE      VDGLD6        SCGLD6
     C                   MOVE      VDACC6        SCACC6
     C                   MOVE      VDSUB6        SCSUB6
     C                   MOVE      VDPCT6        SCPCT6
     C                   MOVE      VDAMT6        SCAMT6
     C                   MOVE      VDDES6        SCDES6
     C*
     C                   MOVE      VDGLD7        SCGLD7
     C                   MOVE      VDACC7        SCACC7
     C                   MOVE      VDSUB7        SCSUB7
     C                   MOVE      VDPCT7        SCPCT7
     C                   MOVE      VDAMT7        SCAMT7
     C                   MOVE      VDDES7        SCDES7
     C*
     C                   MOVE      VDGLD8        SCGLD8
     C                   MOVE      VDACC8        SCACC8
     C                   MOVE      VDSUB8        SCSUB8
     C                   MOVE      VDPCT8        SCPCT8
     C                   MOVE      VDAMT8        SCAMT8
     C                   MOVE      VDDES8        SCDES8
     C*
     C                   MOVE      VDGLD9        SCGLD9
     C                   MOVE      VDACC9        SCACC9
     C                   MOVE      VDSUB9        SCSUB9
     C                   MOVE      VDPCT9        SCPCT9
     C                   MOVE      VDAMT9        SCAMT9
     C                   MOVE      VDDES9        SCDES9
     C*
     C                   END
     C*
     C     SHOW          TAG
     C                   EXFMT     SCR29
     C*
     C***                                                              *****
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQCALL
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F2 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKB         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXFAC#
     C                   MOVE      *ZEROS        XXGLD#
     C                   MOVE      *ZEROS        XXACC#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      ' '           SCTYPE
     C                   CALL      'BKD010R'
     C                   PARM      '4'           SCTYPE            1
     C                   PARM                    XXCO#             3 0
     C                   PARM                    XXFAC#            3 0
     C                   PARM                    XXGLD#            3 0
     C                   PARM                    XXACC#            3 0
     C                   PARM                    XXSUB#            3 0
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C**                                                               ********
     C     XXGLD#        IFNE      *ZERO
     C     XXACC#        ORNE      *ZERO
     C     XXSUB#        ORNE      *ZERO
     C**                                                               ********
     C     1             DO        9             I                 3 0
     C     D(I)          IFEQ      *ZERO
     C     A(I)          ANDEQ     *ZERO
     C     S(I)          ANDEQ     *ZERO
     C                   MOVE      XXGLD#        D(I)
     C                   MOVE      XXACC#        A(I)
     C                   MOVE      XXSUB#        S(I)
     C                   ADD       40            I
     C                   MOVE      '1'           *IN(I)
     C                   GOTO      SHOW
     C                   END
     C                   END
     C**                                                               ********
     C                   END
     C                   GOTO      SHOW
     C**                                                               ********
     C                   END
     C**                                                               ********
     C*
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C**
     C                   END                                                    END CMDKEYS
     C**                                                               ********
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     QQVDR         CHAIN     PVD01                              40
     C*
     C                   MOVE      QQVDR         VDVDR
     C*
     C                   MOVE      SCGLD1        VDGLD1
     C                   MOVE      SCACC1        VDACC1
     C                   MOVE      SCSUB1        VDSUB1
     C                   MOVE      SCPCT1        VDPCT1
     C                   MOVE      SCAMT1        VDAMT1
     C                   MOVE      SCDES1        VDDES1
     C*
     C                   MOVE      SCGLD2        VDGLD2
     C                   MOVE      SCACC2        VDACC2
     C                   MOVE      SCSUB2        VDSUB2
     C                   MOVE      SCPCT2        VDPCT2
     C                   MOVE      SCAMT2        VDAMT2
     C                   MOVE      SCDES2        VDDES2
     C*
     C                   MOVE      SCGLD3        VDGLD3
     C                   MOVE      SCACC3        VDACC3
     C                   MOVE      SCSUB3        VDSUB3
     C                   MOVE      SCPCT3        VDPCT3
     C                   MOVE      SCAMT3        VDAMT3
     C                   MOVE      SCDES3        VDDES3
     C*
     C                   MOVE      SCGLD4        VDGLD4
     C                   MOVE      SCACC4        VDACC4
     C                   MOVE      SCSUB4        VDSUB4
     C                   MOVE      SCPCT4        VDPCT4
     C                   MOVE      SCAMT4        VDAMT4
     C                   MOVE      SCDES4        VDDES4
     C*
     C                   MOVE      SCGLD5        VDGLD5
     C                   MOVE      SCACC5        VDACC5
     C                   MOVE      SCSUB5        VDSUB5
     C                   MOVE      SCPCT5        VDPCT5
     C                   MOVE      SCAMT5        VDAMT5
     C                   MOVE      SCDES5        VDDES5
     C*
     C                   MOVE      SCGLD6        VDGLD6
     C                   MOVE      SCACC6        VDACC6
     C                   MOVE      SCSUB6        VDSUB6
     C                   MOVE      SCPCT6        VDPCT6
     C                   MOVE      SCAMT6        VDAMT6
     C                   MOVE      SCDES6        VDDES6
     C*
     C                   MOVE      SCGLD7        VDGLD7
     C                   MOVE      SCACC7        VDACC7
     C                   MOVE      SCSUB7        VDSUB7
     C                   MOVE      SCPCT7        VDPCT7
     C                   MOVE      SCAMT7        VDAMT7
     C                   MOVE      SCDES7        VDDES7
     C*
     C                   MOVE      SCGLD8        VDGLD8
     C                   MOVE      SCACC8        VDACC8
     C                   MOVE      SCSUB8        VDSUB8
     C                   MOVE      SCPCT8        VDPCT8
     C                   MOVE      SCAMT8        VDAMT8
     C                   MOVE      SCDES8        VDDES8
     C*
     C                   MOVE      SCGLD9        VDGLD9
     C                   MOVE      SCACC9        VDACC9
     C                   MOVE      SCSUB9        VDSUB9
     C                   MOVE      SCPCT9        VDPCT9
     C                   MOVE      SCAMT9        VDAMT9
     C                   MOVE      SCDES9        VDDES9
     C*
     C                   MOVE      Q1USER        VDCUSR
     C                   MOVE      Q1PGM         VDIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VDCDT
     C*
     C                   Z-ADD     DSTIME        VDCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PVD01
     C                   ELSE
     C                   MOVE      Q1USER        VDIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VDIDT
     C*
     C                   Z-ADD     DSTIME        VDITIM
     C                   WRITE     PVD01
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPVD01     E            PVD01E
