     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC090R - Key Definition File Maintenance
     H*****************************************************************
      * REVISIONS:
      * 07-24-91 LJQ    - PROJ # SC030 - CLIENT# 003
      *                   ADDED FIELD TO FILE FOR KEY CONVERION PGM
      *****************************************************************
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    PROGRAM NAME REQUIRED                                   *
     H* SCR02:                                                         *
     H******************************************************************
     FSC090F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMKDL     UF A E           K DISK

     D*
     D KY              S              1    DIM(40)                              KEY DATA
     D H1              S              1    DIM(8)                               HEAD TEMP
     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D D               S             40    DIM(1) CTDATA PERRCD(1)
     D*
     D                 DS
     D  MP                     1     79
     D                                     DIM(79)                              KEY DEF TO DSP ARRY
     D  KEYOUT                 1     79
     D*
     D*
     D                 DS
     D  HD                     1     64
     D                                     DIM(8)                               HEAD DESCS
     D  SCHD01                 1      8
     D  SCHD02                 9     16
     D  SCHD03                17     24
     D  SCHD04                25     32
     D  SCHD05                33     40
     D  SCHD06                41     48
     D  SCHD07                49     56
     D  SCHD08                57     64
     D*
     D                 DS
     D  HL                     1     40  0
     D                                     DIM(8)                               HEAD LENGTHS
     D  SCHL01                 1      5  0
     D  SCHL02                 6     10  0
     D  SCHL03                11     15  0
     D  SCHL04                16     20  0
     D  SCHL05                21     25  0
     D  SCHL06                26     30  0
     D  SCHL07                31     35  0
     D  SCHL08                36     40  0
     D*
     D                 DS
     D  DS                     1     40  0
     D                                     DIM(8)                               DATA FLD START POS
     D  SCDS01                 1      5  0
     D  SCDS02                 6     10  0
     D  SCDS03                11     15  0
     D  SCDS04                16     20  0
     D  SCDS05                21     25  0
     D  SCDS06                26     30  0
     D  SCDS07                31     35  0
     D  SCDS08                36     40  0
     D*
     D                 DS
     D  DL                     1     40  0
     D                                     DIM(8)                               DATA FLD LENGTH
     D  SCDL01                 1      5  0
     D  SCDL02                 6     10  0
     D  SCDL03                11     15  0
     D  SCDL04                16     20  0
     D  SCDL05                21     25  0
     D  SCDL06                26     30  0
     D  SCDL07                31     35  0
     D  SCDL08                36     40  0
     D*
     D*
     D                 DS
     D  DD                     1     40
     D                                     DIM(8)                               DATA FLD DECIMALS
     D  SCDD01                 1      5
     D  SCDD02                 6     10
     D  SCDD03                11     15
     D  SCDD04                16     20
     D  SCDD05                21     25
     D  SCDD06                26     30
     D  SCDD07                31     35
     D  SCDD08                36     40
     D*
     D*
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
     C*** INITIALIZE THE DATA STRUCTURES
     C*
     C*
     C                   Z-ADD     *ZEROS        HL
     C                   Z-ADD     *ZEROS        DS
     C                   Z-ADD     *ZEROS        DL
     C                   MOVEL     *BLANKS       DD
     C*
     C*
     C*   KEY LIST FOR KEY DEFINITION MASTER FILE
     C     PPKEY         KLIST
     C                   KFLD                    SCPGMN
     C*
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
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C********************************************
     C* SCR01 = KEY DEFINITION SRCH              *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'SC590R'
     C                   PARM                    SCPGMN
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPGMN        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKE
     C********************************************
     C* F6 SCR02 - DISPLAY KEY INFORMATION       *
     C********************************************
     C     *INKF         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      '1'           *IN62
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       QQKEY
     C                   MOVEL     *BLANK        QQRTCD
     C                   MOVEA     D             QQKEY
     C*
     C                   EXSR      SRKCVT
     C*
     C*
     C     QQRTCD        IFNE      'N'
     C                   MOVEL     QQKEY         SCKEY
     C                   ELSE
     C                   MOVEA     D             SCKEY
     C                   END
     C*
     C                   XFOOT     HL            SCHLTL
     C*
     C*
     C                   XFOOT     DL            SCDLTL
     C*
     C                   GOTO      STRT
     C                   END                                                    END SCR02
     C                   END                                                    END KF
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
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
     C     PPKEY         DELETE    PKD01                              60
     C                   MOVE      *BLANKS       SCPGMN
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
     C*  PROGRAM NAME CANNOT BE EQUAL TO BLANKS
     C     SCPGMN        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCPGMN        CHAIN     PKD01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PKD01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     KDHD01        SCHD01
     C                   MOVEL     KDHD02        SCHD02
     C                   MOVEL     KDHD03        SCHD03
     C                   MOVEL     KDHD04        SCHD04
     C                   MOVEL     KDHD05        SCHD05
     C                   MOVEL     KDHD06        SCHD06
     C                   MOVEL     KDHD07        SCHD07
     C                   MOVEL     KDHD08        SCHD08
     C*
     C                   MOVE      KDHL01        SCHL01
     C                   MOVE      KDHL02        SCHL02
     C                   MOVE      KDHL03        SCHL03
     C                   MOVE      KDHL04        SCHL04
     C                   MOVE      KDHL05        SCHL05
     C                   MOVE      KDHL06        SCHL06
     C                   MOVE      KDHL07        SCHL07
     C                   MOVE      KDHL08        SCHL08
     C*
     C*
     C                   MOVE      KDDS01        SCDS01
     C                   MOVE      KDDS02        SCDS02
     C                   MOVE      KDDS03        SCDS03
     C                   MOVE      KDDS04        SCDS04
     C                   MOVE      KDDS05        SCDS05
     C                   MOVE      KDDS06        SCDS06
     C                   MOVE      KDDS07        SCDS07
     C                   MOVE      KDDS08        SCDS08
     C*
     C*
     C                   MOVE      KDDL01        SCDL01
     C                   MOVE      KDDL02        SCDL02
     C                   MOVE      KDDL03        SCDL03
     C                   MOVE      KDDL04        SCDL04
     C                   MOVE      KDDL05        SCDL05
     C                   MOVE      KDDL06        SCDL06
     C                   MOVE      KDDL07        SCDL07
     C                   MOVE      KDDL08        SCDL08
     C*
     C*
     C                   MOVEL     KDDD01        SCDD01
     C                   MOVEL     KDDD02        SCDD02
     C                   MOVEL     KDDD03        SCDD03
     C                   MOVEL     KDDD04        SCDD04
     C                   MOVEL     KDDD05        SCDD05
     C                   MOVEL     KDDD06        SCDD06
     C                   MOVEL     KDDD07        SCDD07
     C                   MOVEL     KDDD08        SCDD08
     C                   MOVEL     KDFILE        SCFIL
     C                   MOVEL     KDLIB         SCLIB
     C*+++ BGN ADD  LJQ 7-24-91 ++++++++++++++++++++++++++++++++++++++*
     C*
     C* CONVERSION PROGRAM NAME
     C*
     C                   MOVEL     KDCVTP        SCCVTP
     C*+++ END ADD  LJQ 7-24-91 ++++++++++++++++++++++++++++++++++++++*
     C*
     C                   MOVE      KDSTAT        SCSTAT
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
     C*  HD      CANNOT BE EQUAL TO BLANKS
     C     SCHD01        IFEQ      *BLANK
     C     SCHD02        ANDEQ     *BLANK
     C     SCHD03        ANDEQ     *BLANK
     C     SCHD04        ANDEQ     *BLANK
     C     SCHD05        ANDEQ     *BLANK
     C     SCHD06        ANDEQ     *BLANK
     C     SCHD07        ANDEQ     *BLANK
     C     SCHD08        ANDEQ     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  XFOOT HL   CANT BE GT 32
     C                   XFOOT     HL            SCHLTL
     C     SCHLTL        CABGT     32            STRT                 21
     C*
     C*  XFOOT DL   CANT BE GT 40
     C                   XFOOT     DL            SCDLTL
     C     SCDLTL        CABGT     40            STRT                 22
     C*
     C                   DO        8             F                 5 0
     C     HD(F)         IFNE      *BLANK
     C     HL(F)         CABLE     *ZERO         STRT                   2323      LENTGH NOT LE0
     C     DS(F)         CABLE     *ZERO         STRT                   2525      NEW *********0
     C                   END
     C                   END
     C*
     C                   DO        8             F                 5 0
     C     DS(F)         IFNE      *ZERO
     C     DL(F)         CABLE     *ZERO         STRT                   2424      LENGTH NOT LE0
     C                   END
     C                   END
     C*
     C*** IF DECIMALS ENTERED, MUST BE NUMERIC               (FUTURE USE)
     C*** USE *IN26 FOR THIS EDIT ***
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     PPKEY         CHAIN     PKD01                              40
     C                   MOVE      SCHD01        KDHD01
     C                   MOVEL     SCHD02        KDHD02
     C                   MOVEL     SCHD03        KDHD03
     C                   MOVEL     SCHD04        KDHD04
     C                   MOVEL     SCHD05        KDHD05
     C                   MOVEL     SCHD06        KDHD06
     C                   MOVEL     SCHD07        KDHD07
     C                   MOVEL     SCHD08        KDHD08
     C*
     C                   MOVE      SCHL01        KDHL01
     C                   MOVE      SCHL02        KDHL02
     C                   MOVE      SCHL03        KDHL03
     C                   MOVE      SCHL04        KDHL04
     C                   MOVE      SCHL05        KDHL05
     C                   MOVE      SCHL06        KDHL06
     C                   MOVE      SCHL07        KDHL07
     C                   MOVE      SCHL08        KDHL08
     C*
     C*
     C                   MOVE      SCDS01        KDDS01
     C                   MOVE      SCDS02        KDDS02
     C                   MOVE      SCDS03        KDDS03
     C                   MOVE      SCDS04        KDDS04
     C                   MOVE      SCDS05        KDDS05
     C                   MOVE      SCDS06        KDDS06
     C                   MOVE      SCDS07        KDDS07
     C                   MOVE      SCDS08        KDDS08
     C*
     C*
     C                   MOVE      SCDL01        KDDL01
     C                   MOVE      SCDL02        KDDL02
     C                   MOVE      SCDL03        KDDL03
     C                   MOVE      SCDL04        KDDL04
     C                   MOVE      SCDL05        KDDL05
     C                   MOVE      SCDL06        KDDL06
     C                   MOVE      SCDL07        KDDL07
     C                   MOVE      SCDL08        KDDL08
     C*
     C*
     C                   MOVEL     SCDD01        KDDD01
     C                   MOVEL     SCDD02        KDDD02
     C                   MOVEL     SCDD03        KDDD03
     C                   MOVEL     SCDD04        KDDD04
     C                   MOVEL     SCDD05        KDDD05
     C                   MOVEL     SCDD06        KDDD06
     C                   MOVEL     SCDD07        KDDD07
     C                   MOVEL     SCDD08        KDDD08
     C                   MOVEL     SCFIL         KDFILE
     C                   MOVEL     SCLIB         KDLIB
     C*
     C                   MOVE      SCSTAT        KDSTAT
     C                   MOVE      Q1USER        KDUSER
     C                   MOVE      Q1PGM         KDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KDDT
     C*
     C                   Z-ADD     DSTIME        KDTIME
     C*
     C* CONVERSION PROGRAM NAME
     C*
     C                   MOVEL     SCCVTP        KDCVTP
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PKD01
     C                   ELSE
     C                   MOVEL     SCPGMN        KDPGMN
     C                   MOVEL     Q1PGM         KDPNAM
     C                   MOVEL     Q1USER        KDIUSR
     C                   WRITE     PKD01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCPGMN
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C********************************************************************
     ******** SUBROUTINE FOR KEY FIELD CONVERSION ************************
     C********************************************************************
     C     SRKCVT        BEGSR
     C*
     C*
      **** QQKEY IS THE KEY DATA VALUE PASED IN FROM THE CALLING PROGRAM,
      **** 79A - 40 BYTES AR INPUT TO THIS PROGRAM.  THE QQPGM FIELD CONTAINS
      **** THE NAME OF THE PROGRAM USED TO GET THE KEY DEF RCD.  THIS FIELD IS
      **** THE KEY TO THE KEY DEFINITION FILE.
      ****    THE QQKEY VALUE WILL BE PASSED BACK TO THE CALLING PROGRAM
      ****    AS A 79 BYTE FIELD, FORMATTED WITH THE HEADING FOR THE KEY DATA.
     C*
     C*
     C                   MOVEA     QQKEY         KY(1)
     C***
     C*
     C*
     C                   Z-ADD     1             Z                 5 0
     C                   Z-ADD     1             A                 5 0
     C                   Z-ADD     1             X                 5 0
     C                   Z-ADD     1             Y                 5 0
     C                   MOVE      *BLANKS       MP
     C*
     C     X             DOWLE     79
     C*
     C     HD(A)         IFNE      *BLANKS
     C                   Z-ADD     HL(A)         Y
     C*
     C                   MOVEA     HD(A)         H1
     C*
     C                   Z-ADD     1             G                 5 0
     C*
     C                   DO        Y
     C                   MOVEL     H1(G)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             G
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   Z-ADD     DS(A)         C                 5 0
     C                   Z-ADD     DL(A)         B                 5 0
     C                   DO        B
     C                   MOVEL     KY(C)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             C
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   ADD       1             A                 5 0
     C     A             CABGT     8             DONE
     C*
     C                   END
     C*
     C     DONE          TAG
     C*
     C*
     C     KEYOUT        IFNE      *BLANK
     C                   MOVEL     KEYOUT        QQKEY
     C                   ELSE
     C                   MOVEL     *BLANK        QQKEY            79
     C                   MOVEL     'N'           QQRTCD            1
     C                   END
     C*
     C*
     C                   ENDSR
     C*
     OPKD01     E            PKD01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
**
**********##########**********##########
