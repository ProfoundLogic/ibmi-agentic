     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE M0001R - MENU PROCESSOR OPTION MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  11/19/92 C:20 -ADDED APPLICATION FLAG SEARCH/EDIT(SCR02 F6)  *
     H*                -COMMAND TO EXECUTE IF MENU = Y HAS TO HAVE    *
     H*                 APPLICATION FLAG FILLED IN(LAST CHAR)         *
     H*****************************************************************
     FM0001F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FMMOPL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXEXCL                 1    230
     D  SCEXCL                 1    230
     D  XXEXFL               230    230

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
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
     C*   KEY LIST FOR THE OPTION FILE
     C*
     C     OPKEY         KLIST
     C                   KFLD                    XXMNAM           10
     C                   KFLD                    XXOPTN            4
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*                    WRITESCR99
     C                   MOVEL     'SCR02'       Q2RECF
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   F3 = EXIT                    *
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C**********************************
     C*   F12 = PREVIOUS               *
     C**********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C**********************************
     C*   F5 = RESTART FROM SCR01      *
     C**********************************
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCMNAM
     C                   MOVE      *BLANK        SCOPTN
     C                   END
     C                   GOTO      STRT
     C                   END
     C**********************************
     C*   F6 = OPTIONS SEARCH          *
     C**********************************
     C     *INKF         IFEQ      '1'
     C*** BEGIN ADD *** 11/19//92 *************************************
     C     Q2RECF        IFEQ      'SCR01'
     C*** END ADD *** 11/19/92 ****************************************
     C                   CALL      'M0501R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCMNAM
     C                   PARM                    SCOPTN
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCMNAM        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*** BEGIN ADD *** 11/19//92 *************************************
     C                   END                                                    END SCR01
     C*** END ADD *** 11/19/92 ****************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       QQAPFL            1
     C                   CALL      'M0502R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQAPFL
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQAPFL        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQAPFL        XXEXFL
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END SCR01
     C*** END ADD *** 11/19/92 ****************************************
     C                   END
     C**********************************
     C*   *IN10 PROCESS HELP REQUESTS  *
     C**********************************
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     'SMNU    '    LIBNAM           10
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
     C                   END
     C**********************************
     C*   F20 = CHANGE STATUS          *
     C**********************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN50
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN50
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C**********************************
     C*   F23 - DELETE RECORD          *
     C**********************************
     C     *INKX         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     OPKEY         DELETE    POP01                              55
     C                   MOVE      '0'           *IN50
     C   55              EXFMT     SCR04
     C  N55              MOVE      *BLANK        SCMNAM
     C  N55              MOVE      *BLANK        SCOPTN
     C  N55              MOVEL     'SCR01'       Q2RECF
     C   55              MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C**********
     C                   END
     C************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE MENU NAME CANNOT BE BLANK
     C*
     C     SCMNAM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE OPTION IS NOT BLANK CHECK FOR A MENU RECORD FIRST
     C*
     C     SCOPTN        IFNE      *BLANK
     C                   MOVE      SCMNAM        XXMNAM
     C                   MOVE      *BLANK        XXOPTN
     C     OPKEY         CHAIN     POP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ELSE
     C                   EXCEPT    POP01E
     C     OPMYN         IFNE      'Y'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCMNAM        XXMNAM
     C                   MOVE      SCOPTN        XXOPTN
     C*
     C     OPKEY         CHAIN     POP01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      OPSTAT        SCSTAT
     C                   MOVE      OPMYN         SCMYN
     C                   MOVE      OPAPP         SCAPP
     C                   MOVE      OPEXCL        SCEXCL
     C                   MOVE      OPYNO         SCYNO
     C                   MOVE      OPMMSC        SCMMSC
     C                   MOVE      OPMINS        SCMINS
     C                   MOVE      OPTY01        SCTY01
     C                   MOVE      OPTY02        SCTY02
     C                   MOVE      OPTY03        SCTY03
     C                   MOVE      OPTY04        SCTY04
     C                   MOVE      OPTY05        SCTY05
     C                   MOVE      OPTY06        SCTY06
     C                   MOVE      OPMNTT        SCMNTT
     C                   MOVE      OPTX01        SCTX01
     C                   MOVE      OPTX02        SCTX02
     C                   MOVE      OPTX03        SCTX03
     C                   MOVE      OPTX04        SCTX04
     C                   MOVE      OPTX05        SCTX05
     C                   MOVE      OPTX06        SCTX06
     C                   MOVE      OPTX07        SCTX07
     C                   MOVE      OPTX08        SCTX08
     C                   MOVE      OPTX09        SCTX09
     C                   MOVE      OPTX10        SCTX10
     C                   MOVE      OPTX11        SCTX11
     C                   MOVE      OPTX12        SCTX12
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANK        SCMYN
     C                   MOVE      *BLANK        SCAPP
     C                   MOVE      *BLANK        SCEXCL
     C                   MOVE      *BLANK        SCYNO
     C                   MOVE      *BLANK        SCMMSC
     C                   MOVE      *BLANK        SCMINS
     C                   MOVE      *BLANK        SCTY01
     C                   MOVE      *BLANK        SCTY02
     C                   MOVE      *BLANK        SCTY03
     C                   MOVE      *BLANK        SCTY04
     C                   MOVE      *BLANK        SCTY05
     C                   MOVE      *BLANK        SCTY06
     C                   MOVE      *BLANK        SCMNTT
     C                   MOVE      *BLANK        SCTX01
     C                   MOVE      *BLANK        SCTX02
     C                   MOVE      *BLANK        SCTX03
     C                   MOVE      *BLANK        SCTX04
     C                   MOVE      *BLANK        SCTX05
     C                   MOVE      *BLANK        SCTX06
     C                   MOVE      *BLANK        SCTX07
     C                   MOVE      *BLANK        SCTX08
     C                   MOVE      *BLANK        SCTX09
     C                   MOVE      *BLANK        SCTX10
     C                   MOVE      *BLANK        SCTX11
     C                   MOVE      *BLANK        SCTX12
     C*
     C                   END
     C*
     C                   MOVE      '0'           *IN50
     C*
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN50
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE MENU FLAG MUST BE Y/N
     C*
     C     SCMYN         IFNE      'Y'
     C     SCMYN         ANDNE     'N'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  OPTION TITLE CANNOT BE BLANK
     C*
     C     SCMNTT        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF A MENU THEN WE USE THE MENU SEC LEVEL, IF NOT THEN WE
     C*  USE THE OTHER SEC LEVEL
     C*
     C     SCMYN         IFEQ      'Y'
     C     SCMINS        ANDNE     *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     SCMYN         IFEQ      'N'
     C     SCMMSC        ANDNE     *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  COMMAND Y/N MUST BE VALID
     C*
     C     SCYNO         IFNE      'Y'
     C     SCYNO         ANDNE     'N'
     C*
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 11/19/92 **************************************
     C     SCMYN         IFEQ      'Y'
     C                   Z-ADD     230           T                 3 0
     C     1             SUBST     SCEXCL:T      XXFLAG            1    60
     C*
     C                   MOVE      *BLANKS       QQAPFL
     C                   MOVE      XXFLAG        QQAPFL
     C                   CALL      'M0502R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQAPFL
     C                   PARM      'V'           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      'E'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** END ADD *** 11/19/92 ****************************************
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE SECOND SCREEN
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR03T        TAG
     C*
     C*
     C     *IN09         IFEQ      '0'
     C*
     C     OPKEY         CHAIN     POP01                              40
     C*
     C                   MOVE      SCMNAM        OPMNAM
     C                   MOVE      SCOPTN        OPOPTN
     C                   MOVE      SCMYN         OPMYN
     C                   MOVE      SCAPP         OPAPP
     C                   MOVE      SCEXCL        OPEXCL
     C                   MOVE      SCYNO         OPYNO
     C                   MOVE      SCMMSC        OPMMSC
     C                   MOVE      SCMINS        OPMINS
     C                   MOVE      SCTY01        OPTY01
     C                   MOVE      SCTY02        OPTY02
     C                   MOVE      SCTY03        OPTY03
     C                   MOVE      SCTY04        OPTY04
     C                   MOVE      SCTY05        OPTY05
     C                   MOVE      SCTY06        OPTY06
     C                   MOVE      SCTX01        OPTX01
     C                   MOVE      SCTX02        OPTX02
     C                   MOVE      SCTX03        OPTX03
     C                   MOVE      SCTX04        OPTX04
     C                   MOVE      SCTX05        OPTX05
     C                   MOVE      SCTX06        OPTX06
     C                   MOVE      SCTX07        OPTX07
     C                   MOVE      SCTX08        OPTX08
     C                   MOVE      SCTX09        OPTX09
     C                   MOVE      SCTX10        OPTX10
     C                   MOVE      SCTX11        OPTX11
     C                   MOVE      SCTX12        OPTX12
     C                   MOVE      SCMNTT        OPMNTT
     C                   MOVE      SCSTAT        OPSTAT
     C                   MOVE      Q1USER        OPUSER
     C                   MOVE      Q1PGM         OPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OPDT
     C                   Z-ADD     DSTIME        OPTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    POP01
     C                   ELSE
     C                   WRITE     POP01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C**********           MOVE *BLANKS   SCMNAM    CHANGED 9/12-SAJ
     C                   MOVE      *BLANKS       SCOPTN
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPOP01     E            POP01E
**  TABLE UA
-- ADD --
- UPDATE -
