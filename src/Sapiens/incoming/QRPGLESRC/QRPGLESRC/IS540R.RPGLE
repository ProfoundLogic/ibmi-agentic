     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS540R - TRANSLATION SERVER FILE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, delete, change, or    *
     H*    view TRANSLATION SERVER PARAMETERS                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:    8/8/97               PROJECT:                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    XXXXXXXXXXXXXXXXXXX                                    **
     H*  21    XXXXXXXXXXXXXXXXXXX                                    **
     H*  22    XXXXXXXXXXXXXXXXXXX                                    **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ TRANSLATION SERVER FILE                   **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMTSL   (PTS01): TSID (ALWAYS 'TS')                        *
     H*****************************************************************
     FIS540F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMTSL     UF A E           K DISK

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
      /Free
       scPrDs = 'Translation Server Maintenance';
      /End-Free
     C*
     C                   MOVEL     'TS'          SCTSID            2
     C*
     C*
     C*   KEY LIST FOR TRNASLATION SERVER FILE
     C     TSKY          KLIST
     C                   KFLD                    SCTSID
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
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCTSID        CHAIN     PTS01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PTS01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      TSLUNM        SCLUNM
     C                   MOVE      TSTRCV        SCTRCV
     C                   MOVE      TSTRLB        SCTRLB
     C                   MOVE      TSTSND        SCTSND
     C                   MOVE      TSTSLB        SCTSLB
     C                   MOVE      TSIDRV        SCIDRV
     C                   MOVE      TSTFLR        SCTFLR
     C*
     C                   MOVE      TSSTAT        SCSTAT
     C*
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C*****              MOVEL     CD(1)         SCCMDD                         |
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                         |
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
     C                   MOVE      *BLANKS       SCLUNM
     C                   MOVE      *BLANKS       SCTRCV
     C                   MOVE      *BLANKS       SCTRLB
     C                   MOVE      *BLANKS       SCTSND
     C                   MOVE      *BLANKS       SCTSLB
     C                   MOVE      *BLANKS       SCIDRV
     C                   MOVE      *BLANKS       SCTFLR
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
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
     C*****              MOVEL     CD(1)         SCCMDD                          |
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE TRANSLATION SERVER RCD    *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     TSKY          DELETE    PTS01                              60
     C                   GOTO      TERM
     C                   END                                                    END *INKX
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*****************
     C     SCR01T        TAG
     C*
     C*
     C*** VALIDATE LIBS / SCAN CAPABLE Y OR N , AND VALID NAMES?
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     TSKY          CHAIN     PTS01                              40
     C                   MOVEL     SCTSID        TSTSID
     C                   MOVE      SCLUNM        TSLUNM
     C                   MOVE      SCTRCV        TSTRCV
     C                   MOVE      SCTRLB        TSTRLB
     C                   MOVE      SCTSND        TSTSND
     C                   MOVE      SCTSLB        TSTSLB
     C                   MOVE      SCIDRV        TSIDRV
     C                   MOVE      SCTFLR        TSTFLR
     C                   MOVE      SCSTAT        TSSTAT
     C                   MOVEL     Q1USER        TSLUSR
     C                   MOVEL     Q1PGM         TSLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      TSLDT
     C                   Z-ADD     DSTIME        TSLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PTS01
     C*
     C*** CREATE THE DATA QUEUES IF THEY DONT EXIST ***
     C                   CALL      'ISCRTDQ'
     C                   PARM                    TSTSND
     C                   PARM                    TSTSLB
     C                   PARM                    TSTRCV
     C                   PARM                    TSTRLB
     C                   PARM                    RTNCDE            1
     C*
     C                   ELSE
     C                   MOVEL     SCTSID        TSTSID
     C                   MOVE      SCLUNM        TSLUNM
     C                   MOVE      SCTRCV        TSTRCV
     C                   MOVE      SCTRLB        TSTRLB
     C                   MOVE      SCTSND        TSTSND
     C                   MOVE      SCTSLB        TSTSLB
     C                   MOVE      SCIDRV        TSIDRV
     C                   MOVE      SCTFLR        TSTFLR
     C                   MOVE      SCSTAT        TSSTAT
     C                   MOVEL     Q1USER        TSIUSR
     C                   MOVEL     Q1PGM         TSIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      TSIDT
     C                   Z-ADD     DSTIME        TSITIM
     C*
     C                   WRITE     PTS01
     C*
     C*** CREATE THE DATA QUEUES IF THEY DONT EXIST ***
     C                   CALL      'ISCRTDQ'
     C                   PARM                    TSTSND
     C                   PARM                    TSTSLB
     C                   PARM                    TSTRCV
     C                   PARM                    TSTRLB
     C                   PARM                    RTNCDE            1
     C*
     C                   END
     C                   END
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPTS01     E            PTS01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
