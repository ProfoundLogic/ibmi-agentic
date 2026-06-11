     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0220R - CHAPTER MASTER MAINTENANCE
     FW0220F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMCPL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D QQPRDS          C                   CONST('Chapter Maintenance')
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

     C*
     C*   KEY LIST FOR THE CHAPTER MASTER FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCCHAP
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQCALL            1
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C                   PARM                    QQKEY            10
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
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C*
     C                   MOVEL(P)  QQPRDS        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C     QQCALL        COMP      '1'                                    09
     C*
     C     TOP           TAG
     C*
     C                   MOVEL     *BLANKS       CONAME
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C  N60              MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C  N60              MOVEL     FNNAME        SCNAME
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
     C                   EXFMT     SCR02
     C                   END
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   MOVEL     'A'           QQRTC
     C                   GOTO      TERM
     C                   END
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   CALL      'W0720R'
     C                   PARM                    SCPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFND
     C                   PARM                    SCCHAP
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCHAP        OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     *BLANKS       QQRTC
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*  CHANGE STATUS
     C*
     C     *INKU         IFEQ      *ON
     C     *IN40         ANDEQ     *OFF
     C*
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF                                                  SCSTAT=A
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKU=*ON/*IN40=*OFF
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10=*ON
     C*
     C                   ENDIF                                                  *IN11=*ON
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
     C*  CHAPTER NUMBER MUST BE GREATER THAN ZERO
     C*
     C     SCCHAP        CABLE     *ZERO         STRT                   2020
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     CPKY          CHAIN     WMCPL                              40
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     UA(2)         SCMODE
     C                   MOVE      CPCHAP        SCCHAP
     C                   MOVEL     CPSTAT        SCSTAT
     C                   MOVEL     CPDESC        SCDESC
     C*
     C                   ELSE
     C*
     C     *IN09         CABEQ     *ON           STRT                     22
     C*
     C                   MOVEL     UA(1)         SCMODE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANK        SCDESC
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
     C*  DESCRIPTION CANNOT BE BLANK
     C*
     C     SCDESC        CABEQ     *BLANK        STRT                     21
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      *OFF
     C*
     C                   Z-ADD     XXCO#         CPCO#
     C                   Z-ADD     SCFND         CPFND
     C                   MOVEL     SCCHAP        CPCHAP
     C                   MOVEL     SCDESC        CPDESC
     C                   MOVE      SCSTAT        CPSTAT
     C                   MOVE      Q1USER        CPUSER
     C                   MOVE      Q1PGM         CPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CPDT
     C*
     C                   Z-ADD     DSTIME        CPTIME
     C*
     C     *IN40         IFEQ      *OFF
     C                   UPDATE    PCP01
     C                   ELSE
     C                   WRITE     PCP01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCCHAP
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
**  TABLE UA
-- ADD --
- UPDATE -
