     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS530R - PRINTER SRVR ID FILE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, delete, change, or    *
     H*    view PRINTER SRVR ID Records.                              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    IS150R - PRINTER SRVR ID SRCH                              *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:    8/8/97               PROJECT:                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    INVALID WORKSTAION ID                                  **
     H* SCR02:                                                        **
     H*  20    JOB NAME    REQUIRED                                   **
     H*  21    XXXXXXXXXXXXXXXXXXX                                    **
     H*  22    XXXXXXXXXXXXXXXXXXX                                    **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ Document Type FILE                        **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMPSL1  (PWS01): WSID                                      *
     H*
     H*****************************************************************
     FIS550F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMPSL1    UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
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
     C                   PARM                    QQPDSC           40
     C                   PARM                    QQCALL            1
     C*
     C*   PARM LIST Document Type Search
     C*
     C     PL0130        PLIST
     C                   PARM                    SCPSID
     C                   PARM                    QQRTC             1
     C*
     C*
     C*
     C*   KEY LIST FOR INDEX QUEUE   FILE
     C     PSKY          KLIST
     C                   KFLD                    SCPSID
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
     C* SCR01 = WORKSTATION ID SEARCH            *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'IS150R'      PL0130
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPSID        OREQ      *BLANK
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
     C*   F23 = DELETE WORKSTATION ID RCD        *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     PSKY          DELETE    PPS01                              60
     C                   MOVE      *BLANK        SCPSID
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKX
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
     C*  INDEX QUEUE   CANNOT BE EQUAL TO BLANKS
     C*
     C     SCPSID        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCPSID        CHAIN     PPS01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PPS01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      PSJBNM        SCJBNM
     C                   MOVE      PSPSID        SCPSID
     C                   MOVE      PSLUNM        SCLUNM
     C                   MOVE      PSPQUE        SCPQUE
     C                   MOVE      PSPQLB        SCPQLB
     C                   MOVE      PSIDRV        SCIDRV
     C*
     C                   MOVE      PSSTAT        SCSTAT
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
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCJBNM
     C                   MOVE      *BLANKS       SCLUNM
     C                   MOVE      *BLANKS       SCPQUE
     C                   MOVE      *BLANKS       SCPQLB
     C                   MOVE      *BLANKS       SCIDRV
     C                   MOVE      *BLANKS       SCSTAT
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
     C*****************
     C     SCR02T        TAG
     C*
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS
     C*
     C     SCJBNM        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** VALIDATE LIBS / SCAN CAPABLE Y OR N , AND VALID NAMES?
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     PSKY          CHAIN     PPS01                              40
     C                   MOVEL     SCJBNM        PSJBNM
     C                   MOVEL     SCPSID        PSPSID
     C                   MOVE      SCLUNM        PSLUNM
     C                   MOVE      SCPQUE        PSPQUE
     C                   MOVE      SCPQLB        PSPQLB
     C                   MOVE      SCIDRV        PSIDRV
     C                   MOVE      SCSTAT        PSSTAT
     C                   MOVEL     Q1USER        PSLUSR
     C                   MOVEL     Q1PGM         PSLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      PSLDT
     C                   Z-ADD     DSTIME        PSLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPS01
     C*
     C*** CREATE THE DATA QUEUES IF THEY DONT EXIST ***
     C                   CALL      'ISCRTDQ3'
     C                   PARM                    PSPQUE
     C                   PARM                    PSPQLB
     C                   PARM                    RTNCDE            1
     C*
     C                   ELSE
     C                   MOVEL     SCPSID        PSPSID
     C                   MOVEL     SCJBNM        PSJBNM
     C                   MOVE      SCLUNM        PSLUNM
     C                   MOVE      SCPQUE        PSPQUE
     C                   MOVE      SCPQLB        PSPQLB
     C                   MOVE      SCIDRV        PSIDRV
     C                   MOVE      SCSTAT        PSSTAT
     C                   MOVEL     Q1USER        PSIUSR
     C                   MOVEL     Q1PGM         PSIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      PSIDT
     C                   Z-ADD     DSTIME        PSITIM
     C*
     C                   WRITE     PPS01
     C*
     C                   CALL      'ISCRTDQ3'
     C                   PARM                    PSPQUE
     C                   PARM                    PSPQLB
     C                   PARM                    RTNCDE            1
     C*
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCPSID
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPPS01     E            PPS01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
