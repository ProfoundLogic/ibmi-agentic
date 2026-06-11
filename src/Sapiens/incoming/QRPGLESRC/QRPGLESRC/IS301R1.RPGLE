     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS301R1 - FAX ROUTING - ADD/UPDATE ENTRY
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, UPDATE A ROUTING ENTRY*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    FAX PORT OR TSID,  NOT BOTH                            **
     H*  21    FAX PORT OR TSID ALREADY EXISTS                        **
     H*  22    DESCRIPTION CANT BE BLANK                              **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMRTL   (PRT01): TSID                                      *
     H*
     H*****************************************************************
     FIS301F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMRTL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPTSI           20
     C                   PARM                    QQCALL            1
     C*
      /Free
       scPrDs = 'Add Fax Routing Entry';
      /End-Free
     C*
     C*   KEY LIST FOR DOC INDEX FILE
     C     RTKY          KLIST
     C                   KFLD                    XXTSID           20
     C*
     C                   TIME                    DSDTTM
     C*
     C*********  QQCALL    COMP '1'                      09
     C     TOP           TAG
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     QQPTSI        IFNE      *BLANKS
     C                   MOVEL     QQPTSI        XXTSID
     C                   ENDIF
     C     RTKY          CHAIN     PRT01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PRT01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C     RTFXPT        IFEQ      'Y'
     C                   MOVEL     RTTSID        SCIFXP
     C                   ELSE
     C                   MOVE      RTTSID        SCTSID
     C                   ENDIF
     C                   MOVEL     RTDESC        SCDESC
     C*
     C*
     C                   MOVE      RTSTAT        SCSTAT
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
     C                   MOVE      *BLANKS       SCTSID
     C                   MOVE      *BLANKS       SCIFXP
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*
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
     C                   MOVEL     'A'           QQCALL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F10 = SEARCH                           *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C*
     C     FLD           IFEQ      'SCIFXP  '
     C*** CALL FAX PORT SEARCH PROGRAM
     C                   CALL      'IS140R'
     C                   PARM                    SCIFXP
     C                   PARM      *BLANKS       ##DSC            40
     C                   PARM      'S'           ##RTC             1
     C*
     C     ##RTC         CABEQ     'A'           TERM                     LR
     C     ##RTC         CABEQ     'B'           STRT
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C***
     C                   END                                                    END *INKL
     C********************************************
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'B'           QQCALL
     C                   GOTO      TERM
     C                   END                                                    END *INKL
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
     C*   F23 = DELETE ROUTING RCD        *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     RTKY          DELETE    PRT01                              60
     C                   MOVE      *BLANK        XXTSID
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     ' '           QQCALL
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
     C*
     C     SCR01T        TAG
     C*
     C     *IN40         IFEQ      *ON
     C*  CANT HAVE TSID AND FAX PORT
     C*
     C     SCTSID        IFEQ      *BLANKS
     C     SCIFXP        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  CANT HAVE TSID AND FAX PORT
     C*
     C     SCTSID        IFNE      *BLANKS
     C     SCIFXP        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCTSID        IFNE      *BLANKS
     C                   MOVEL     SCTSID        XXTSID
     C                   ELSE
     C                   MOVEL     SCIFXP        XXTSID
     C                   ENDIF
     C*
     C*
     C*  TSI CODE ENTERED CANT ALREADY EXIST
     C*
     C*** ONLY FOR ADD DO THIS EDIT ***************************
     C     RTKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*** DESC NOT EQUAL BLANKS    ***************************
     C     SCDESC        CABEQ     *BLANKS       STRT                     22
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     RTKY          CHAIN     PRT01                              40
     C*
     C     SCTSID        IFNE      *BLANKS
     C                   MOVEL     SCTSID        RTTSID
     C                   MOVEL     'N'           RTFXPT
     C                   ELSE
     C                   MOVEL     SCIFXP        RTTSID
     C                   MOVEL     'Y'           RTFXPT
     C                   ENDIF
     C                   MOVEL     SCDESC        RTDESC
     C                   MOVE      SCSTAT        RTSTAT
     C                   MOVEL     Q1USER        RTLUSR
     C                   MOVEL     Q1PGM         RTLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      RTLDT
     C                   Z-ADD     DSTIME        RTLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRT01
     C                   ELSE
     C*
     C     SCTSID        IFNE      *BLANKS
     C                   MOVEL     SCTSID        RTTSID
     C                   MOVEL     'N'           RTFXPT
     C                   ELSE
     C                   MOVEL     SCIFXP        RTTSID
     C                   MOVEL     'Y'           RTFXPT
     C                   ENDIF
     C                   MOVEL     SCDESC        RTDESC
     C                   MOVE      SCSTAT        RTSTAT
     C                   MOVEL     Q1USER        RTIUSR
     C                   MOVEL     Q1PGM         RTIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      RTIDT
     C                   Z-ADD     DSTIME        RTITIM
     C*
     C                   WRITE     PRT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCTSID
     C                   MOVE      *BLANKS       XXTSID
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPRT01     E            PRT01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
