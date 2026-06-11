     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC070R - PROGRAM/PATH FILE MAINTENANCE
     H******************************************************************
     H* REVISIONS:
     H*== 08/09/91 === LJD === PROJ: 90007/D0007 =====================*
     H*   Added processing to write corresponding record format       *
     H*       with each custom group ID.  This will be important      *
     H*       in the loading of SCR03 in SC010R.                      *
     H*   NOTE:  RECORD FORMAT FIELDS ARE HIDDEN IN DSPF SC070F       *
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20       PATH ID REQUIRED                                     *
     H* SCR02:                                                         *
     H*  20-29    CUSTOM ID (1-10) INVALID CUSTOM TYPE ENTERED         *
     H*  30       INVALID PATH ID ENTERED                              *
     H*  31       Y/N REQUIRED FOR ALLOWING USER TO ENTER A CUSTOM     *
     H*              TYPE ID OUTSIDE OF RANGE                          *
     H*  32       AT LEAST ONE CUSTOM ID MUST BE ENTERED               *
     H*  33       ALL 10 CUSTOM IDS REQUIRED TO ENTER A NEXT PATH ID   *
     H******************************************************************
     FSC070F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMPIL     UF A E           K DISK
     FSMCGL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CI                     1     20
     D                                     DIM(10)                              CUSTOM ID
     D  SCCI01                 1      2
     D  SCCI02                 3      4
     D  SCCI03                 5      6
     D  SCCI04                 7      8
     D  SCCI05                 9     10
     D  SCCI06                11     12
     D  SCCI07                13     14
     D  SCCI08                15     16
     D  SCCI09                17     18
     D  SCCI10                19     20
     D                 DS
     D  SR                     1     10
     D                                     DIM(10)                              SCRN RCD FMT
     D  SCRF01                 1      1
     D  SCRF02                 2      2
     D  SCRF03                 3      3
     D  SCRF04                 4      4
     D  SCRF05                 5      5
     D  SCRF06                 6      6
     D  SCRF07                 7      7
     D  SCRF08                 8      8
     D  SCRF09                 9      9
     D  SCRF10                10     10
     D*
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
     C*   KEY LIST FOR THE PATH ID MASTER FILE
     C     PIKEY         KLIST
     C                   KFLD                    XXPATH            8
     C*
     C*   KEY LIST FOR CUSTOM GROUP ID FILE
     C     CGKEY         KLIST
     C                   KFLD                    XXCSID            2
     C*
     C                   TIME                    DSDTTM
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
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   F5  = PATH ID SEARCH                   *
     C********************************************
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'SC570R'
     C                   PARM                    SCPATH
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPATH        OREQ      *BLANK
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
     C*   F10 = CUSTOM GROUP ID SEARCH           *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   DO        10            A                 5 0
     C*
     C                   MOVEL     CI(A)         XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANKS       XXCSID
     C                   MOVE      *BLANKS       XXRFMT            1
     C                   CALL      'SC520R'
     C                   PARM                    XXCSID
     C                   PARM                    XXRFMT
     C                   PARM                    QQRTCD            1
     C                   MOVE      *BLANKS       CI(A)
     C                   MOVE      XXCSID        CI(A)
     C***
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END ?
     C                   END                                                    END DO
     C*
     C                   END                                                    ENDIF SCR02
     C*
     C                   END                                                    END *INKJ
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
     C*   F23 = DELETE CODE                      *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     SCPATH        XXPATH
     C     PIKEY         DELETE    PPI01                              60
     C                   MOVE      *BLANKS       SCPATH
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                     END *INKX
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
     C*  PATH ID CANNOT BE EQUAL TO BLANKS
     C     SCPATH        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCPATH        CHAIN     PPI01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PPI01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      PICI01        SCCI01
     C                   MOVE      PICI02        SCCI02
     C                   MOVE      PICI03        SCCI03
     C                   MOVE      PICI04        SCCI04
     C                   MOVE      PICI05        SCCI05
     C                   MOVE      PICI06        SCCI06
     C                   MOVE      PICI07        SCCI07
     C                   MOVE      PICI08        SCCI08
     C                   MOVE      PICI09        SCCI09
     C                   MOVE      PICI10        SCCI10
     C                   MOVE      PINXPI        SCNXPI
     C                   MOVE      PIALOR        SCALOR
     C*
     C                   MOVE      PIRF01        SCRF01
     C                   MOVE      PIRF02        SCRF02
     C                   MOVE      PIRF03        SCRF03
     C                   MOVE      PIRF04        SCRF04
     C                   MOVE      PIRF05        SCRF05
     C                   MOVE      PIRF06        SCRF06
     C                   MOVE      PIRF07        SCRF07
     C                   MOVE      PIRF08        SCRF08
     C                   MOVE      PIRF09        SCRF09
     C                   MOVE      PIRF10        SCRF10
     C*
     C                   MOVE      PISTAT        SCSTAT
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
     C                   MOVE      *BLANKS       SCCI01
     C                   MOVE      *BLANKS       SCCI02
     C                   MOVE      *BLANKS       SCCI03
     C                   MOVE      *BLANKS       SCCI04
     C                   MOVE      *BLANKS       SCCI05
     C                   MOVE      *BLANKS       SCCI06
     C                   MOVE      *BLANKS       SCCI07
     C                   MOVE      *BLANKS       SCCI08
     C                   MOVE      *BLANKS       SCCI09
     C                   MOVE      *BLANKS       SCCI10
     C*
     C                   MOVE      *BLANKS       SCRF01
     C                   MOVE      *BLANKS       SCRF02
     C                   MOVE      *BLANKS       SCRF03
     C                   MOVE      *BLANKS       SCRF04
     C                   MOVE      *BLANKS       SCRF05
     C                   MOVE      *BLANKS       SCRF06
     C                   MOVE      *BLANKS       SCRF07
     C                   MOVE      *BLANKS       SCRF08
     C                   MOVE      *BLANKS       SCRF09
     C                   MOVE      *BLANKS       SCRF10
     C*
     C                   MOVE      *BLANKS       SCNXPI
     C                   MOVE      'N'           SCALOR
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
     C*
     C     SCR02T        TAG
     C*
     C*  AT LEAST ONE CUSTOM ID MUST BE ENTERED
     C                   MOVE      '1'           *IN32
     C                   DO        10            A                 5 0
     C     CI(A)         IFNE      *BLANKS
     C                   MOVE      '0'           *IN32
     C                   END                                                    END NE *BLANKS
     C                   END                                                    END DO
     C     *IN32         CABEQ     '1'           STRT
     C*
     C*  IF NOT BLANK, VALID CUSTOM IDS MUST BE ENTERED
     C                   DO        10            A                 5 0
     C*
     C     CI(A)         IFNE      *BLANKS
     C*
     C     A             ADD       19            ER                2 0
     C                   MOVEL     CI(A)         XXCSID
     C     CGKEY         CHAIN     PCG01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(ER)
     C                   GOTO      STRT
     C*
     C                   ELSE
     C                   MOVE      CGRFMT        SR(A)
     C*
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SR(A)
     C*
     C*
     C                   END                                                    END NE *BLANKS
     C*
     C                   END                                                    END DO
     C*
     C*  ALL 10 CUSTOM IDS REQUIRED TO ENTER A CONTINUING (NEXT) PATH ID
     C                   MOVE      '0'           *IN33
     C     SCNXPI        IFNE      *BLANKS
     C                   DO        10            A                 5 0
     C     CI(A)         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLANKS
     C                   END                                                    END DO
     C                   END                                                    END NE *BLANKS
     C*
     C*  VALIDATE NEXT PATH ID
     C     SCNXPI        IFNE      *BLANKS
     C                   MOVE      SCNXPI        XXPATH
     C     PIKEY         CHAIN     PPI01                              30
     C     *IN30         CABEQ     '1'           STRT
     C                   END
     C*
     C*  ALLOW OUTSIDE RANGE FLAG MUST BE Y OR N
     C     SCALOR        IFNE      'Y'
     C     SCALOR        ANDNE     'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      SCPATH        XXPATH
     C     PIKEY         CHAIN     PPI01                              40
     C                   MOVE      SCCI01        PICI01
     C                   MOVE      SCCI02        PICI02
     C                   MOVE      SCCI03        PICI03
     C                   MOVE      SCCI04        PICI04
     C                   MOVE      SCCI05        PICI05
     C                   MOVE      SCCI06        PICI06
     C                   MOVE      SCCI07        PICI07
     C                   MOVE      SCCI08        PICI08
     C                   MOVE      SCCI09        PICI09
     C                   MOVE      SCCI10        PICI10
     C*
     C                   MOVE      SCRF01        PIRF01
     C                   MOVE      SCRF02        PIRF02
     C                   MOVE      SCRF03        PIRF03
     C                   MOVE      SCRF04        PIRF04
     C                   MOVE      SCRF05        PIRF05
     C                   MOVE      SCRF06        PIRF06
     C                   MOVE      SCRF07        PIRF07
     C                   MOVE      SCRF08        PIRF08
     C                   MOVE      SCRF09        PIRF09
     C                   MOVE      SCRF10        PIRF10
     C*
     C                   MOVE      SCNXPI        PINXPI
     C                   MOVE      SCALOR        PIALOR
     C                   MOVE      SCSTAT        PISTAT
     C                   MOVE      Q1USER        PIUSER
     C                   MOVE      Q1PGM         PIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PIDT
     C*
     C                   Z-ADD     DSTIME        PITIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPI01
     C                   ELSE
     C                   MOVEL     SCPATH        PIPATH
     C                   MOVEL     Q1PGM         PIPNAM
     C                   MOVEL     Q1USER        PIIUSR
     C                   WRITE     PPI01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCPATH
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPPI01     E            PPI01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
