     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE A2421R - BANK INFORMATION FILE SEARCH
     H*****************************************************************
     FA2421F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FAPBIL     IF   E           K DISK
     F*******************************************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*******************************************************************

     C*
     C*   KEY LIST FOR THE BANK INFORMATION FILE
     C*
     C     BIKEY         KLIST
     C                   KFLD                    SCCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY1            3
     C                   PARM                    QQKEY2            3
     C                   PARM                    QQRTCD            1
     C**************************************************************
     C* DEFINITIONS
     C     *LIKE         DEFINE    S2CO#         SVCO#
     C     *LIKE         DEFINE    S2FND         SVFND
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      QQKEY1        SCCO#
     C*
     C                   EXSR      INIT
     C*
     C     TOP           TAG
     C     BIKEY         SETLL     PBI01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = EXIT                         *
     C****************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   Z-ADD     POS2          RRN2
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C******************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     S2CO#         IFNE      SVCO#
     C     S2FND         ORNE      SVFND
     C                   MOVE      *BLANKS       SVCO#
     C                   MOVE      *BLANKS       SVFND
     C                   MOVE      S2CO#         SVCO#
     C                   MOVE      S2FND         SVFND
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVEL     S2CO#         SCCO#
     C                   Z-ADD     0             RRN2
     C                   GOTO      TOP
     C                   END
     C*
     C     TAGRC         TAG
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C     SCSEL         IFEQ      '1'
     C                   MOVE      SCCO#         QQKEY1
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C                   END
     C                   GOTO      STRT
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             SCRRN
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      PBI01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C     X             IFEQ      1
     C                   Z-ADD     RRN2          POS2              5 0
     C                   END
     C                   MOVE      BICO#         SCCO#
     C                   MOVE      BIAI01        SCBKNM
     C                   MOVE      *BLANKS       SCSEL
     C                   Z-ADD     RRN2          SCRRN
     C                   MOVE      '0'           *IN06
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01S
     C                   MOVE      '0'           *IN01
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCBKNM
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             POS2
     C                   Z-ADD     1             SCRRN
     C                   MOVE      '1'           *IN06
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01S
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     POS2          #@$2
     C                   Z-ADD     RRN2          POS2
     C*
     C                   ENDSR
