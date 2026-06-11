     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE N0540R - T/M CHARGES SEARCH                              *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*  97    POSITION SEARCH ON SRV CODE                            *
     H*  98    CORRECT EOMB FOR SEARCH                                *
     H*  99    PROGRAM CALLED FROM N0040R                             *
     H* N99    PROGRAM CALLED FROM WC PROGRAM                         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     FN0540F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     F***NDTML   IF  E           K        DISK
     FNDTML2    IF   E           K DISK
     F                                     RENAME(PTM01:PTM02)
     FFMCML1    IF   E           K DISK
     FFMFNL1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FSSYLF     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* INITIALIZE SECTION
     C*
     C                   MOVE      *BLANKS       QQLAST           56
     C                   MOVE      *BLANKS       QQFRST           15
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVE      *BLANKS       PCFL              1
     C                   MOVE      *BLANKS       XSKEY             2
     C                   MOVE      *BLANKS       SCCODE
     C                   Z-ADD     *ZEROS        RRN2              5 0
     C                   Z-ADD     *ZEROS        SVRRN2            5 0
     C                   Z-ADD     *ZEROS        XXRRN2            5 0
     C*
     C* KEY LIST FOR THE T/M DETAIL CHARGE FILE
     C*
     C     TMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     TMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXCODE
     C                   KFLD                    XXMSEQ
     C                   KFLD                    XXSVDT            7 0
     C*
     C     TMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY
     C*
     C* PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOMB            9
     C                   PARM                    QQRTCD            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C     QQSUB#        IFEQ      *BLANKS
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      FNSUB#        XXSUB#
     C                   ELSE
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   END
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQEOMB        XXEOB#            9 0
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQSUB#        IFEQ      *BLANKS
     C*********            MOVE FNWCF#    SCSUB#
     C                   MOVE      FNSUB#        SCSUB#
     C                   ELSE
     C                   MOVE      QQSUB#        SCSUB#
     C                   END
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN99
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   END
     C*
     C* CHAIN TO THE SYSTEM CONTROL FILE
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        FBDT04
     C                   END
     C*
     C* GET CLAIMANT NAME
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#             9
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCNAM
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       SCCNAM
     C                   END
     C*
     C* INITIALIZE SCREEN
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C                   MOVE      '0'           *IN97
     C*
     C* MAIN LINE CODE
     C*
     C     MAINLN        TAG
     C*
     C* SCREEN EXECUTION AREA
     C*
     C                   EXSR      SCREXC
     C*
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
     C*
     C* CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCCODE        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXCODE            4
     C                   MOVEL     SCCODE        XXCODE
     C     SCMSEQ        IFNE      *ZEROS
     C                   MOVE      *BLANKS       XXMSEQ
     C                   MOVE      SCMSEQ        XXMSEQ            5 0
     C     SCCHRG        IFNE      *ZEROS
     C                   MOVE      *BLANKS       XXCHRG            6 0
     C                   MOVE      SCCHRG        XXCHRG
     C                   END
     C                   END
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *ZEROS        SCMSEQ
     C                   MOVE      *ZEROS        SCCHRG
     C                   MOVE      '1'           *IN97
     C                   GOTO      TOP
     C                   END                                                    END SCCODE
     C*
     C     SCCHRG        IFNE      *ZEROS
     C     SCMSEQ        ANDNE     *ZEROS
     C     SCCODE        ANDEQ     *BLANKS
     C                   MOVE      *ZEROS        SCCHRG
     C                   MOVE      *ZEROS        SCMSEQ
     C                   GOTO      TOP
     C                   END
     C*
     C     SCMSEQ        IFNE      *ZEROS
     C     SCCODE        ANDEQ     *BLANKS
     C                   MOVEL     *ZEROS        SCMSEQ
     C                   GOTO      TOP
     C                   END
     C*
     C                   Z-ADD     RRN2          SVRRN2
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '1'
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN50
     C*
     C                   GOTO      TOP                                          RE-SET SUBFILE
     C*
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C**************************************************************************
     C* I N I T - SUBROUTINE TO INITIALIZE SCREEN                              *
     C**************************************************************************
     C     INZSR         BEGSR
     C*
     C   97TMKY1         SETLL     PTM02                              60
     C  N97TMKY2         SETLL     PTM02                              60
     C**N99N97   TMKY      SETLLPTM01                60
     C*
     C* CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C                   ENDSR
     C**************************************************************************
     C* L O A D - SUBROUTINE TO LOAD DATA TO SCREEN                            *
     C**************************************************************************
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C                   MOVE      'Y'           PCFL
     C*
     C* LOAD THE SUBFILE SCREEN WITH UPTO 9 LINES
     C*
     C                   DO        9             X                 5 0          NEST-A
     C*
     C     TMKY2         READE     PTM02                                  60
     C***99      TMKY2     READEPTM02                    60
     C**N99      TMKY      READEPTM01                    60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'                                          NEST-B
     C                   MOVE      '1'           *IN98
     C*
     C   99TMEOB#        IFEQ      XXEOB#                                       NEST-C
     C                   MOVE      '0'           *IN98
     C                   END                                                    NEST-C
     C*
     C     *IN98         IFEQ      '1'                                          NEST-D
     C                   ADD       1             RRN2
     C                   MOVEL     TMMSCD        SCMSCD
     C                   Z-ADD     TMSEQ#        SCMSEQ
     C                   MOVEL     TMSTAF        SCSTAF
     C                   Z-ADD     TMSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSVDT
     C                   Z-ADD     TMSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSVDT
     C                   MOVEL     TMACT         SCACT
     C                   MOVEL     TMDESC        SCDESC
     C                   Z-ADD     TMMILE        SCMILE
     C                   Z-ADD     TMHRS         SCHOUR
     C                   Z-ADD     TMMSCG        SCMISC
     C                   MOVEL     TMEOB#        SCEOMB
     C*
     C* SAVE 1ST SUBFILE RRN OF CURRENT PAGE
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   END                                                    NEST-D
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END                                                    NEST-B
     C*
     C                   END                                                    NEST-A
     C*
     C* INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCMSCD
     C                   Z-ADD     *ZEROS        SCMSEQ
     C                   MOVE      *BLANKS       SCSTAF
     C                   Z-ADD     *ZEROS        SCSVDT
     C                   MOVE      *BLANKS       SCACT
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        SCMILE
     C                   Z-ADD     *ZEROS        SCHOUR
     C                   Z-ADD     *ZEROS        SCMISC
     C                   MOVE      *BLANKS       SCEOMB
     C                   Z-ADD     1             RRN2
     C*
     C* SAVE 1ST SUBFILE RRN OF THE CURRENT PAGE
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C* TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     XXRRN2        #@$2
     C*
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C* S C R E X C - SUBROUTINE TO DISPLAY THE SCREEN                         *
     C**************************************************************************
     C     SCREXC        BEGSR
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C                   ENDSR
     C*
     C/EJECT
     C**************************************************************************
     C* F K E Y S - SUBROUTINE TO PROCESS FUNCTION KEYS                        *
     C**************************************************************************
     C     FKEYS         BEGSR
     C*
     C     *INKA         IFEQ      '1'
     C                   END
     C*
     C     *INKB         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C     *INKD         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKE         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F6 = MED SVC SRCH                  *
     C****************************************
     C     *INKF         IFEQ      '1'
     C                   CALL      'F0538R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    SCMSCD
     C                   PARM                    SCMSEQ
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   GOTO      TOP
     C                   END                                                    *INKF
     C*
     C*
     C     *INKG         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKH         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKI         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKJ         IFEQ      '1'
     C                   END
     C*
     C     *INKK         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C     *INKM         IFEQ      '1'
     C                   END
     C*
     C     *INKN         IFEQ      '1'
     C                   END
     C*
     C     *INKP         IFEQ      '1'
     C                   END
     C*
     C     *INKQ         IFEQ      '1'
     C                   END
     C*
     C     *INKR         IFEQ      '1'
     C                   END
     C*
     C     *INKS         IFEQ      '1'
     C                   END
     C*
     C     *INKT         IFEQ      '1'
     C                   END
     C*
     C     *INKU         IFEQ      '1'
     C                   END
     C*
     C     *INKV         IFEQ      '1'
     C                   END
     C*
     C     *INKW         IFEQ      '1'
     C                   END
     C*
     C     *INKX         IFEQ      '1'
     C                   END
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
     C*
     C                   ENDSR
     C/EJECT
