     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKI010R - CHART OF ACCOUNTS MASTER
     H******************************************************************
     H* REVISIONS:                                                     *
     H* 02/07/02 C:136 SUPPORT 31175 WA065                             *
     H*                DELETED F10 (DELETE RECORD)                     *
     H*                                                                *
     H*  11/19/93 C:22 ADDED CODE TO CHECK FOR ACTIVITY IF USER        *
     H*                ATTEMPTS TO DELETE AN ACCOUNT                   *
     H******************************************************************
     FBKI010F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKCAL1    UF A E           K DISK
     FBKSYL1    IF   E           K DISK
      *** BEG DEL **** 02/07/02 ***********************************************
     F*******GLDEL1  IF  E           K        DISK                           UC
     F*******GLHSL1  IF  E           K        DISK                           UC
     F*******GLTRL3  IF  E           K        DISK                           UC
      *** BEG DEL **** 02/07/02 ***********************************************

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
     C*   KEY LIST FOR THE CHART OF ACCOUNTS
     C*
     C     GLKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCGLD#
     C                   KFLD                    SCACC#
     C                   KFLD                    SCSUB#
     C*
     C*   KEY LIST FOR THE CURRENT PERIOD DETAIL FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCGLD#
     C                   KFLD                    SCACC#
     C                   KFLD                    SCSUB#
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     HSKEY         KLIST
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
     C                   MOVE      ' '           SCTYPE
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKD010R'
     C                   PARM                    SCTYPE
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
      *** BEG DEL **** 02/07/02 ***********************************************
     C********** *INKJ     IFEQ '1'
     C********** Q2RECF    ANDEQ'SCR02'
     C********** QQCALL    ANDEQ'D'
     C**********           OPEN GLDEL1                 60
     C********** *IN60     IFEQ '0'
     C********** DEKEY     CHAINPDE01                61
     C********** *IN61     IFEQ '0'
     C**********           MOVE '1'       *IN22
     C**********           CLOSEGLDEL1
     C**********           GOTO STRT
     C**********           END
     C**********           CLOSEGLDEL1
     C**********           ELSE
     C**********           MOVE '1'       *IN26
     C**********           GOTO STRT
     C**********           END
     C**********           OPEN GLHSL1                 60
     C********** *IN60     IFEQ '0'
     C********** HSKEY     CHAINPHS01                61
     C********** *IN61     IFEQ '0'
     C**********           MOVE '1'       *IN25
     C**********           CLOSEGLHSL1
     C**********           GOTO STRT
     C**********           END
     C**********           CLOSEGLHSL1
     C**********           ELSE
     C**********           MOVE '1'       *IN26
     C**********           GOTO STRT
     C**********           END
     C**********           OPEN GLTRL3                 60
     C********** *IN60     IFEQ '0'
     C********** *IN61     DOUEQ'1'
     C**********           READ PTR01                    61
     C********** SCCO#     IFEQ TRCO#
     C********** SCFAC#    ANDEQTRFAC#
     C********** SCGLD#    ANDEQTRGLD#
     C********** SCACC#    ANDEQTRACC#
     C********** SCSUB#    ANDEQTRSUB#
     C**********           MOVE '1'       *IN24
     C**********           CLOSEGLTRL3
     C**********           GOTO STRT
     C**********           END
     C**********           END
     C**********           ELSE
     C**********           MOVE '1'       *IN26
     C**********           GOTO STRT
     C**********           END
     C********** GLKEY     CHAINPCA01                40
     C********** *IN40     IFEQ '0'
     C**********           DELETPCA01
     C**********           END
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
      *** BEG DEL **** 02/07/02 ***********************************************
     C*
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
     C     GLKEY         CHAIN     PCA01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCA01E
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C                   MOVE      CACO#         SCCO#
     C                   MOVE      CAFAC#        SCFAC#
     C                   MOVE      CADESC        SCDESC
     C                   MOVE      CAGLD#        SCGLD#
     C                   MOVE      CAACC#        SCACC#
     C                   MOVE      CASUB#        SCSUB#
     C                   MOVE      CAFLG1        SCFLG1            1
     C                   MOVE      CAFLG2        SCFLG2            1
     C                   MOVE      CAFLG3        SCFLG3            1
     C                   MOVE      CAFLG4        SCFLG4            1
     C                   MOVE      CATYPE        SCTYPE
     C                   MOVE      CACODE        SCCODE
     C                   MOVE      CATOTL        SCTOTL
     C*          *IN08     IFEQ '1'
     C                   Z-ADD     CAFISC        SCCF00
     C                   Z-ADD     CACF01        SCCF01
     C                   Z-ADD     CACF02        SCCF02
     C                   Z-ADD     CACF03        SCCF03
     C                   Z-ADD     CACF04        SCCF04
     C                   Z-ADD     CACF05        SCCF05
     C                   Z-ADD     CACF06        SCCF06
     C                   Z-ADD     CACF07        SCCF07
     C                   Z-ADD     CACF08        SCCF08
     C                   Z-ADD     CACF09        SCCF09
     C                   Z-ADD     CACF10        SCCF10
     C                   Z-ADD     CACF11        SCCF11
     C                   Z-ADD     CACF12        SCCF12
     C                   Z-ADD     CAPFIS        SCLF00
     C                   Z-ADD     CALF01        SCLF01
     C                   Z-ADD     CALF02        SCLF02
     C                   Z-ADD     CALF03        SCLF03
     C                   Z-ADD     CALF04        SCLF04
     C                   Z-ADD     CALF05        SCLF05
     C                   Z-ADD     CALF06        SCLF06
     C                   Z-ADD     CALF07        SCLF07
     C                   Z-ADD     CALF08        SCLF08
     C                   Z-ADD     CALF09        SCLF09
     C                   Z-ADD     CALF10        SCLF10
     C                   Z-ADD     CALF11        SCLF11
     C                   Z-ADD     CALF12        SCLF12
     C                   Z-ADD     CABG01        SCBG01
     C                   Z-ADD     CABG02        SCBG02
     C                   Z-ADD     CABG03        SCBG03
     C                   Z-ADD     CABG04        SCBG04
     C                   Z-ADD     CABG05        SCBG05
     C                   Z-ADD     CABG06        SCBG06
     C                   Z-ADD     CABG07        SCBG07
     C                   Z-ADD     CABG08        SCBG08
     C                   Z-ADD     CABG09        SCBG09
     C                   Z-ADD     CABG10        SCBG10
     C                   Z-ADD     CABG11        SCBG11
     C                   Z-ADD     CABG12        SCBG12
     C*                    END
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDESC
     C*                    MOVE *BLANKS   SCCO#
     C*                    MOVE *BLANKS   SCFAC#
     C*                    MOVE *BLANKS   SCGLD#
     C*                    MOVE *BLANKS   SCACC#
     C*                    MOVE *BLANKS   SCSUB#
     C                   MOVE      'Y'           SCFLG1
     C                   MOVE      *BLANKS       SCFLG2
     C                   MOVE      *BLANKS       SCFLG3
     C                   MOVE      *BLANKS       SCFLG4
     C                   MOVE      *BLANKS       SCTYPE
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCTOTL
     C                   Z-ADD     0             CACF00
     C                   Z-ADD     0             CACF01
     C                   Z-ADD     0             CACF02
     C                   Z-ADD     0             CACF03
     C                   Z-ADD     0             CACF04
     C                   Z-ADD     0             CACF05
     C                   Z-ADD     0             CACF06
     C                   Z-ADD     0             CACF07
     C                   Z-ADD     0             CACF08
     C                   Z-ADD     0             CACF09
     C                   Z-ADD     0             CACF10
     C                   Z-ADD     0             CACF11
     C                   Z-ADD     0             CACF12
     C                   Z-ADD     0             CALF00
     C                   Z-ADD     0             CALF01
     C                   Z-ADD     0             CALF02
     C                   Z-ADD     0             CALF03
     C                   Z-ADD     0             CALF04
     C                   Z-ADD     0             CALF05
     C                   Z-ADD     0             CALF06
     C                   Z-ADD     0             CALF07
     C                   Z-ADD     0             CALF08
     C                   Z-ADD     0             CALF09
     C                   Z-ADD     0             CALF10
     C                   Z-ADD     0             CALF11
     C                   Z-ADD     0             CALF12
     C                   Z-ADD     0             CABG01
     C                   Z-ADD     0             CABG02
     C                   Z-ADD     0             CABG03
     C                   Z-ADD     0             CABG04
     C                   Z-ADD     0             CABG05
     C                   Z-ADD     0             CABG06
     C                   Z-ADD     0             CABG07
     C                   Z-ADD     0             CABG08
     C                   Z-ADD     0             CABG09
     C                   Z-ADD     0             CABG10
     C                   Z-ADD     0             CABG11
     C                   Z-ADD     0             CABG12
     C                   Z-ADD     0             CACURR
     C                   Z-ADD     0             CAPFIS
     C                   Z-ADD     0             CAFISC
     C                   Z-ADD     0             CAAD98
     C                   Z-ADD     0             CAAD99
     C*
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
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C*  TYPE CODE IS 1-4
     C     SCTYPE        IFNE      '1'
     C     SCTYPE        ANDNE     '2'
     C     SCTYPE        ANDNE     '3'
     C     SCTYPE        ANDNE     '4'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  CODE IS X,Y, OR BLANK
     C     SCCODE        IFNE      ' '
     C     SCCODE        ANDNE     'X'
     C     SCCODE        ANDNE     'Y'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  TOTAL LEVEL  1-4
     C*
     C     SCTOTL        IFNE      ' '
     C     SCTOTL        ANDNE     '0'
     C     SCTOTL        ANDNE     '1'
     C     SCTOTL        ANDNE     '2'
     C     SCTOTL        ANDNE     '3'
     C     SCTOTL        ANDNE     '4'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     GLKEY         CHAIN     PCA01                              40
     C*
     C                   MOVE      SCCO#         CACO#
     C                   MOVE      SCFAC#        CAFAC#
     C                   MOVE      SCDESC        CADESC
     C                   MOVE      SCGLD#        CAGLD#
     C                   MOVE      SCACC#        CAACC#
     C                   MOVE      SCSUB#        CASUB#
     C                   MOVE      SCFLG1        CAFLG1
     C                   MOVE      SCFLG2        CAFLG2
     C                   MOVE      SCFLG3        CAFLG3
     C                   MOVE      SCFLG4        CAFLG4
     C                   MOVE      SCTYPE        CATYPE
     C                   MOVE      SCCODE        CACODE
     C                   MOVE      SCTOTL        CATOTL
     C     *IN08         IFEQ      '0'
     C                   GOTO      TAGOV
     C                   END
     C                   Z-ADD     SCCF00        CAFISC
     C                   Z-ADD     SCCF01        CACF01
     C                   Z-ADD     SCCF02        CACF02
     C                   Z-ADD     SCCF03        CACF03
     C                   Z-ADD     SCCF04        CACF04
     C                   Z-ADD     SCCF05        CACF05
     C                   Z-ADD     SCCF06        CACF06
     C                   Z-ADD     SCCF07        CACF07
     C                   Z-ADD     SCCF08        CACF08
     C                   Z-ADD     SCCF09        CACF09
     C                   Z-ADD     SCCF10        CACF10
     C                   Z-ADD     SCCF11        CACF11
     C                   Z-ADD     SCCF12        CACF12
     C                   Z-ADD     SCLF00        CAPFIS
     C                   Z-ADD     SCLF01        CALF01
     C                   Z-ADD     SCLF02        CALF02
     C                   Z-ADD     SCLF03        CALF03
     C                   Z-ADD     SCLF04        CALF04
     C                   Z-ADD     SCLF05        CALF05
     C                   Z-ADD     SCLF06        CALF06
     C                   Z-ADD     SCLF07        CALF07
     C                   Z-ADD     SCLF08        CALF08
     C                   Z-ADD     SCLF09        CALF09
     C                   Z-ADD     SCLF10        CALF10
     C                   Z-ADD     SCLF11        CALF11
     C                   Z-ADD     SCLF12        CALF12
     C                   Z-ADD     SCBG01        CABG01
     C                   Z-ADD     SCBG02        CABG02
     C                   Z-ADD     SCBG03        CABG03
     C                   Z-ADD     SCBG04        CABG04
     C                   Z-ADD     SCBG05        CABG05
     C                   Z-ADD     SCBG06        CABG06
     C                   Z-ADD     SCBG07        CABG07
     C                   Z-ADD     SCBG08        CABG08
     C                   Z-ADD     SCBG09        CABG09
     C                   Z-ADD     SCBG10        CABG10
     C                   Z-ADD     SCBG11        CABG11
     C                   Z-ADD     SCBG12        CABG12
     C*
     C     TAGOV         TAG
     C                   MOVE      Q1USER        CACUSR
     C                   MOVE      Q1PGM         CAIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CACDT
     C*
     C                   Z-ADD     DSTIME        CACTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCA01
     C                   ELSE
     C                   MOVE      Q1USER        CAIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAIDT
     C*
     C                   Z-ADD     DSTIME        CAITIM
     C                   WRITE     PCA01
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
     OPCA01     E            PCA01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
