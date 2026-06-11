     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS540R - SPLIT       SERVER FILE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, delete, change, or    *
     H*    view SPLIT       SERVER PARAMETERS                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:    8/8/97               PROJECT:                    *
     H*****************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*  06/09/20  C:212 Project : SY420                                *
     H*                  Mask SSN                                       *
     H*                  recompile only                                 *
     H*                                                                 *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    XXXXXXXXXXXXXXXXXXX                                    **
     H*  21    XXXXXXXXXXXXXXXXXXX                                    **
     H*  22    XXXXXXXXXXXXXXXXXXX                                    **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ SPLIT       SERVER FILE                   **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMTSL   (PSS01): SSID (ALWAYS 'TS')                        *
     H*****************************************************************
     FIS545F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMSSL     UF A E           K DISK
     FIMDTL     IF   E           K DISK

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
     C*
      /Free
       scPrDs = 'Split Server Maintenance';
      /End-Free
     C                   MOVEL     'SS'          SCSSID            2
     C*
     C*
     C*   KEY LIST FOR TRNASLATION SERVER FILE
     C     SSKY          KLIST
     C                   KFLD                    SCSSID
     C*
     C*   KEY LIST FOR DOC TYPE       FILE
     C     DTKY          KLIST
     C                   KFLD                    SCDCTP
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
     C     SCSSID        CHAIN     PSS01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PSS01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      SSLUNM        SCLUNM
     C                   MOVE      SSSRCV        SCSRCV
     C                   MOVE      SSSRLB        SCSRLB
     C                   MOVE      SSSSND        SCSSND
     C                   MOVE      SSSSLB        SCSSLB
     C                   MOVE      SSIDRV        SCIDRV
     C                   MOVE      SSTFLR        SCTFLR
     C                   MOVE      SSDCTP        SCDCTP
     C*
     C                   MOVE      SSSTAT        SCSTAT
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
     C                   MOVE      *BLANKS       SCSRCV
     C                   MOVE      *BLANKS       SCSRLB
     C                   MOVE      *BLANKS       SCSSND
     C                   MOVE      *BLANKS       SCSSLB
     C                   MOVE      *BLANKS       SCIDRV
     C                   MOVE      *BLANKS       SCTFLR
     C                   MOVE      *BLANKS       SCDCTP
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
     C*
     C****************************************
     C*  F10 = TABLE FILE SEARCH             *
     C****************************************
     C     *INKJ         IFEQ      '1'
     C*
     C     FLD           IFEQ      'SCDCTP  '
     C*** CALL IMGQ SEARCH PROGRAM
     C                   CALL      'IS100R'
     C                   PARM                    SCDCTP
     C                   PARM                    ##DSC            40
     C                   PARM                    ##RTC             1
     C*
     C     ##RTC         CABEQ     'A'           TERM                     LR
     C     ##RTC         CABEQ     'B'           STRT
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C                   END                                                    END *INKJ
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
     C*   F23 = DELETE SPLIT       SERVER RCD    *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     SSKY          DELETE    PSS01                              60
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
     C     DTKY          CHAIN     PDT01                              60
     C     *IN60         CABEQ     *ON           STRT                     22
     C*
     C*** VALIDATE LIBS / SCAN CAPABLE Y OR N , AND VALID NAMES?
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     SSKY          CHAIN     PSS01                              40
     C                   MOVEL     SCSSID        SSSSID
     C                   MOVE      SCLUNM        SSLUNM
     C                   MOVE      SCSRCV        SSSRCV
     C                   MOVE      SCSRLB        SSSRLB
     C                   MOVE      SCSSND        SSSSND
     C                   MOVE      SCSSLB        SSSSLB
     C                   MOVE      SCIDRV        SSIDRV
     C                   MOVE      SCTFLR        SSTFLR
     C                   MOVE      SCDCTP        SSDCTP
     C                   MOVE      SCSTAT        SSSTAT
     C                   MOVEL     Q1USER        SSLUSR
     C                   MOVEL     Q1PGM         SSLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      SSLDT
     C                   Z-ADD     DSTIME        SSLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PSS01
     C*
     C*** CREATE THE DATA QUEUES IF THEY DONT EXIST ***
     C                   CALL      'ISCRTDQ'
     C                   PARM                    SSSSND
     C                   PARM                    SSSSLB
     C                   PARM                    SSSRCV
     C                   PARM                    SSSRLB
     C                   PARM                    RTNCDE            1
     C*
     C                   ELSE
     C                   MOVEL     SCSSID        SSSSID
     C                   MOVE      SCLUNM        SSLUNM
     C                   MOVE      SCSRCV        SSSRCV
     C                   MOVE      SCSRLB        SSSRLB
     C                   MOVE      SCSSND        SSSSND
     C                   MOVE      SCSSLB        SSSSLB
     C                   MOVE      SCIDRV        SSIDRV
     C                   MOVE      SCTFLR        SSTFLR
     C                   MOVE      SCDCTP        SSDCTP
     C                   MOVE      SCSTAT        SSSTAT
     C                   MOVEL     Q1USER        SSIUSR
     C                   MOVEL     Q1PGM         SSIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      SSIDT
     C                   Z-ADD     DSTIME        SSITIM
     C*
     C                   WRITE     PSS01
     C*
     C*** CREATE THE DATA QUEUES IF THEY DONT EXIST ***
     C                   CALL      'ISCRTDQ'
     C                   PARM                    SSSSND
     C                   PARM                    SSSSLB
     C                   PARM                    SSSRCV
     C                   PARM                    SSSRLB
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
     OPSS01     E            PSS01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
