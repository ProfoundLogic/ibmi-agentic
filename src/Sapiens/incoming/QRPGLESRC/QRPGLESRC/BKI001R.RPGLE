     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKI001R - COMPANY MASTER MAINTENANCE
     H*********************************************************************
     H* REVISIONS:                                                        *
     H* 11/04/94 C:22 - UNABLE TO UPDATE RECORDS. REMOVED THIRD GL NUMBER *
     H*********************************************************************
     FBKI001F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKCFL1    UF A E           K DISK
     FAPBKL1    IF   E           K DISK
     FBKCAL1    IF   E           K DISK
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
     C     *ENTRY        PLIST
     C                   PARM                    QQTYPE            1
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
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C     GLKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFAC#            3 0
     C                   KFLD                    XXGLD#            3 0
     C                   KFLD                    XXACC#            3 0
     C                   KFLD                    XXSUB#            3 0
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C     QQCALL        COMP      '8'                                    98
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           CODE              1
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
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C     QQRTC         CABEQ     ' '           STRT
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKE
     C********************************************
     C*   F7 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKG         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXFAC#
     C                   MOVE      *ZEROS        XXGLD#
     C                   MOVE      *ZEROS        XXACC#
     C                   MOVE      *ZEROS        XXSUB#
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'BKD010R'
     C                   PARM                    CODE
     C                   PARM                    XXCO#
     C                   PARM                    XXFAC#
     C                   PARM                    XXGLD#
     C                   PARM                    XXACC#
     C                   PARM                    XXSUB#
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      ' '
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     XXCO#         IFGT      *ZERO
     C                   MOVE      XXCO#         SCCO1
     C                   MOVE      XXFAC#        SCFAC1
     C                   MOVE      XXGLD#        SCDPT1
     C                   MOVE      XXACC#        SCACC1
     C                   MOVE      XXSUB#        SCSUB1
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKG
     C********************************************
     C*   F8 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKH         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXFAC#
     C                   MOVE      *ZEROS        XXGLD#
     C                   MOVE      *ZEROS        XXACC#
     C                   MOVE      *ZEROS        XXSUB#
     C                   CALL      'BKD010R'
     C                   PARM                    CODE
     C                   PARM                    XXCO#
     C                   PARM                    XXFAC#
     C                   PARM                    XXGLD#
     C                   PARM                    XXACC#
     C                   PARM                    XXSUB#
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      ' '
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     XXCO#         IFGT      *ZERO
     C                   MOVE      XXCO#         SCCO2
     C                   MOVE      XXFAC#        SCFAC2
     C                   MOVE      XXGLD#        SCDPT2
     C                   MOVE      XXACC#        SCACC2
     C                   MOVE      XXSUB#        SCSUB2
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKH
     C********************************************
     C*   F9 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXFAC#
     C                   MOVE      *ZEROS        XXGLD#
     C                   MOVE      *ZEROS        XXACC#
     C                   MOVE      *ZEROS        XXSUB#
     C                   CALL      'BKD010R'
     C                   PARM                    CODE
     C                   PARM                    XXCO#
     C                   PARM                    XXFAC#
     C                   PARM                    XXGLD#
     C                   PARM                    XXACC#
     C                   PARM                    XXSUB#
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      ' '
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     XXCO#         IFGT      *ZERO
     C                   MOVE      XXCO#         SCCO3
     C                   MOVE      XXFAC#        SCFAC3
     C                   MOVE      XXGLD#        SCDPT3
     C                   MOVE      XXACC#        SCACC3
     C                   MOVE      XXSUB#        SCSUB3
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKI
     C********************************************
     C*   F10= SEARCH LIST CALL TO BANK MASTER   *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C                   MOVE      *ZEROS        XXBNK#            3 0
     C                   MOVE      '1'           QQRTC
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'BKD020R'
     C                   PARM                    XXBNK#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      ' '
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     XXBNK#        IFNE      *ZERO
     C                   MOVE      XXBNK#        SCBNK#
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKJ
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
     C*  THE COMPANY & FACILITY ARE BOTH REQUIRED ENTRIES.
     C*
     C     SCCO#         IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFAC#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     CFKEY         CHAIN     PCF01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCF01E
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C                   MOVE      CFNAME        SCNAME
     C                   MOVE      CFADR1        SCADR1
     C                   MOVE      CFADR2        SCADR2
     C                   MOVE      CFCITY        SCCITY
     C                   MOVE      CFST          SCST
     C                   MOVE      CFZIP1        SCZIP1
     C                   MOVE      CFZIP2        SCZIP2
     C                   MOVE      CFBNK#        SCBNK#
     C                   MOVE      CFFEIN        SCFEIN
     C                   MOVE      CFCO1         SCCO1
     C                   MOVE      CFFAC1        SCFAC1
     C                   MOVE      CFDPT1        SCDPT1
     C                   MOVE      CFACC1        SCACC1
     C                   MOVE      CFSUB1        SCSUB1
     C                   MOVE      CFCO2         SCCO2
     C                   MOVE      CFFAC2        SCFAC2
     C                   MOVE      CFDPT2        SCDPT2
     C                   MOVE      CFACC2        SCACC2
     C                   MOVE      CFSUB2        SCSUB2
     C                   MOVE      CFCO3         SCCO3
     C                   MOVE      CFFAC3        SCFAC3
     C                   MOVE      CFDPT3        SCDPT3
     C                   MOVE      CFACC3        SCACC3
     C                   MOVE      CFSUB3        SCSUB3
     C                   Z-ADD     CFJAN         SCJAN
     C                   Z-ADD     CFFEB         SCFEB
     C                   Z-ADD     CFMAR         SCMAR
     C                   Z-ADD     CFAPR         SCAPR
     C                   Z-ADD     CFMAY         SCMAY
     C                   Z-ADD     CFJUN         SCJUN
     C                   Z-ADD     CFJUL         SCJUL
     C                   Z-ADD     CFAUG         SCAUG
     C                   Z-ADD     CFSEP         SCSEP
     C                   Z-ADD     CFOCT         SCOCT
     C                   Z-ADD     CFNOV         SCNOV
     C                   Z-ADD     CFDEC         SCDEC
     C*
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCITY
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *BLANKS       SCZIP1
     C                   MOVE      *BLANKS       SCZIP2
     C                   MOVE      *BLANKS       SCBNK#
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCCO1
     C                   MOVE      *BLANKS       SCFAC1
     C                   MOVE      *BLANKS       SCDPT1
     C                   MOVE      *BLANKS       SCACC1
     C                   MOVE      *BLANKS       SCSUB1
     C                   MOVE      *BLANKS       SCCO2
     C                   MOVE      *BLANKS       SCFAC2
     C                   MOVE      *BLANKS       SCDPT2
     C                   MOVE      *BLANKS       SCACC2
     C                   MOVE      *BLANKS       SCSUB2
     C                   MOVE      *BLANKS       SCCO3
     C                   MOVE      *BLANKS       SCFAC3
     C                   MOVE      *BLANKS       SCDPT3
     C                   MOVE      *BLANKS       SCACC3
     C                   MOVE      *BLANKS       SCSUB3
     C                   Z-ADD     0             SCJAN
     C                   Z-ADD     0             SCFEB
     C                   Z-ADD     0             SCMAR
     C                   Z-ADD     0             SCAPR
     C                   Z-ADD     0             SCMAY
     C                   Z-ADD     0             SCJUN
     C                   Z-ADD     0             SCJUL
     C                   Z-ADD     0             SCAUG
     C                   Z-ADD     0             SCSEP
     C                   Z-ADD     0             SCOCT
     C                   Z-ADD     0             SCNOV
     C                   Z-ADD     0             SCDEC
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
     C*  NAME CANNOT BE EQUAL TO BLANKS
     C     SCNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     *IN98         IFEQ      '0'
     C*
     C** IF BANK CODE IS BLANK SEARCH FOR A GOOD ONE.
     C*
     C     'BK'          CHAIN     PSY01                              60
     C   60              MOVE      *BLANKS       SYINS1
     C     SYINS1        IFEQ      'Y'
     C     SCBNK#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCBNK#        IFNE      *ZERO
     C     SCBNK#        CHAIN     PBK01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C** VALIDATE COMPANIES
     C*
     C     SCCO1         IFGT      *ZERO
     C     SCCO1         ANDNE     SCCO#
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCO2         IFGT      *ZERO
     C     SCCO2         ANDNE     SCCO#
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCO3         IFGT      *ZERO
     C     SCCO3         ANDNE     SCCO#
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C** VALIDATE G/L NUMBERS
     C*
     C                   MOVE      SCCO1         XXCO#
     C                   MOVE      SCFAC1        XXFAC#
     C                   MOVE      SCDPT1        XXGLD#
     C                   MOVE      SCACC1        XXACC#
     C                   MOVE      SCSUB1        XXSUB#
     C     GLKEY         CHAIN     PCA01                              25
     C     *IN25         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCCO2         XXCO#
     C                   MOVE      SCFAC2        XXFAC#
     C                   MOVE      SCDPT2        XXGLD#
     C                   MOVE      SCACC2        XXACC#
     C                   MOVE      SCSUB2        XXSUB#
     C     GLKEY         CHAIN     PCA01                              26
     C     *IN26         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     CFKEY         CHAIN     PCF01                              40
     C*
     C                   MOVE      SCCO#         CFCO#
     C                   MOVE      SCFAC#        CFFAC#
     C                   MOVE      SCNAME        CFNAME
     C                   MOVE      SCADR1        CFADR1
     C                   MOVE      SCADR2        CFADR2
     C                   MOVE      SCCITY        CFCITY
     C                   MOVE      SCST          CFST
     C                   MOVE      SCZIP1        CFZIP1
     C                   MOVE      SCZIP2        CFZIP2
     C                   MOVE      SCBNK#        CFBNK#
     C                   MOVE      SCFEIN        CFFEIN
     C                   MOVE      SCCO1         CFCO1
     C                   MOVE      SCFAC1        CFFAC1
     C                   MOVE      SCDPT1        CFDPT1
     C                   MOVE      SCACC1        CFACC1
     C                   MOVE      SCSUB1        CFSUB1
     C                   MOVE      SCCO2         CFCO2
     C                   MOVE      SCFAC2        CFFAC2
     C                   MOVE      SCDPT2        CFDPT2
     C                   MOVE      SCACC2        CFACC2
     C                   MOVE      SCSUB2        CFSUB2
     C                   MOVE      SCCO3         CFCO3
     C                   MOVE      SCFAC3        CFFAC3
     C                   MOVE      SCDPT3        CFDPT3
     C                   MOVE      SCACC3        CFACC3
     C                   MOVE      SCSUB3        CFSUB3
     C                   Z-ADD     SCJAN         CFJAN
     C                   Z-ADD     SCFEB         CFFEB
     C                   Z-ADD     SCMAR         CFMAR
     C                   Z-ADD     SCAPR         CFAPR
     C                   Z-ADD     SCMAY         CFMAY
     C                   Z-ADD     SCJUN         CFJUN
     C                   Z-ADD     SCJUL         CFJUL
     C                   Z-ADD     SCAUG         CFAUG
     C                   Z-ADD     SCSEP         CFSEP
     C                   Z-ADD     SCOCT         CFOCT
     C                   Z-ADD     SCNOV         CFNOV
     C                   Z-ADD     SCDEC         CFDEC
     C*
     C                   MOVE      Q1USER        CFCUSR
     C                   MOVE      Q1PGM         CFIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CFCDT
     C*
     C                   Z-ADD     DSTIME        CFCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCF01
     C                   ELSE
     C                   MOVE      Q1USER        CFFUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CFIDT
     C*
     C                   Z-ADD     DSTIME        CFITIM
     C                   WRITE     PCF01
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
     OPCF01     E            PCF01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
