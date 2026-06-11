     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS140R1 - FAX PORT CONTROL FILE ADD/UPDATE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, UPDATE A FAX PORT IN  *
     H*    THE FAX PORT CONTROL FILE (IMFPP)                          *
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
     H*    IMFPL   (PRT01): FPRT                                      *
     H*                                                               *
     H*****************************************************************
     FIS140F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMFPL     UF A E           K DISK
     FIMIQL     IF   E           K DISK
     FIMDTL     IF   E           K DISK

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
     C                   PARM                    QQFPRT           12
     C                   PARM                    QQCALL            1
     C*
      /Free
       scPrDs = 'Add/Change Fax Port File';
      /End-Free
     C*
     C*   KEY LIST FOR DOC INDEX FILE
     C     FPKY          KLIST
     C                   KFLD                    XXFPRT           12
     C*
     C*
     C*   KEY LIST FOR DOC TYPE  ontrol File
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
     C*
     C*
     C     PL0100        PLIST
     C                   PARM                    SCDOCT
     C                   PARM      *BLANKS       XXDESC           40
     C                   PARM      *BLANKS       R@RTC             1
     C*
     C                   TIME                    DSDTTM
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
     C     QQFPRT        IFNE      *BLANKS
     C                   MOVEL     QQFPRT        XXFPRT
     C                   ENDIF
     C     FPKY          CHAIN     PFP01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PFP01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     FPFPRT        SCFPRT
     C                   MOVEL     FPDOCT        SCDOCT
     C                   MOVEL     FPDESC        SCDESC
     C                   MOVEL     FPTSIR        SCTSIR
     C                   MOVEL     FPLBR         SCLBR
     C                   MOVEL     FPPART        SCPART
     C                   MOVEL     FPPPGM        SCPPGM
     C                   MOVEL     FPPLIB        SCPLIB
     C                   MOVEL     FPIMGQ        SCIMGQ
     C*
     C*
     C                   MOVE      FPSTAT        SCSTAT
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
     C                   MOVE      *BLANKS       SCFPRT
     C                   MOVEL     *BLANKS       SCDOCT
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCTSIR
     C                   MOVEL     *BLANK        SCLBR
     C                   MOVEL     *BLANKS       SCPART
     C                   MOVEL     *BLANKS       SCPPGM
     C                   MOVEL     *BLANKS       SCPLIB
     C                   MOVEL     *BLANKS       SCIMGQ
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
     C     FLD           IFEQ      'SCIMGQ  '
     C*** CALL IMGQ SEARCH PROGRAM
     C                   CALL      'IS120R'
     C                   PARM                    SCIMGQ
     C                   PARM      *BLANKS       ##DSC            40
     C                   PARM      *BLANK        ##RTC             1
     C*
     C     ##RTC         CABEQ     'A'           TERM                     LR
     C     ##RTC         CABEQ     'B'           STRT
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C     FLD           IFEQ      'SCDOCT  '
     C*** CALL FAX PORT SEARCH PROGRAM
     C*** CALL DOC TYPE CH PROGRAM
     C                   CALL      'IS100R'      PL0100
     C     R@RTC         CABEQ     'A'           TERM
     C*
     C     R@RTC         IFEQ      'B'
     C     SCDOCT        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
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
     C     FPKY          DELETE    PFP01                              60
     C                   MOVE      *BLANK        XXFPRT
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
     C*
     C     SCFPRT        CABEQ     *BLANKS       STRT                     28
     C*
     C*
     C*
     C*  FAX PORT CODE ENTERED CANT ALREADY EXIST
     C*
     C*** ONLY FOR ADD DO THIS EDIT ***************************
     C     FPKY          CHAIN     PFP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*** DESC NOT EQUAL BLANKS    ***************************
     C     SCDESC        CABEQ     *BLANKS       STRT                     22
     C*
     C*** TSI ROUTING FLAG = Y/N  ***************************
     C     SCTSIR        IFNE      'Y'
     C     SCTSIR        ANDNE     'N'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*** LOAD BALACNE FLAG = Y/N  ***************************
     C     SCLBR         IFNE      'Y'
     C     SCLBR         ANDNE     'N'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*** PARTIAL LOOKUP = Y/N  ***************************
     C     SCPART        IFNE      'Y'
     C     SCPART        ANDNE     'N'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*** PARTIAL LOOKUP PGM/LIB **************************
     C     SCPART        IFEQ      'Y'
     C     SCPPGM        CABEQ     *BLANKS       STRT                     26
     C     SCPLIB        CABEQ     *BLANKS       STRT                     26
     C                   ENDIF
     C*
     C*
     C*
     C*** IMAGE QUEUE VALID     ***************************
     C     SCIMGQ        CHAIN     PIQ01                              60
     C     *IN60         CABEQ     *ON           STRT                     27
     C*
     C*
     C*  DOC TYPE    must Exist
     C*
     C     DTKY          CHAIN     PDT01                              60
     C     *IN60         CABEQ     *ON           STRT                     29
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     FPKY          CHAIN     PFP01                              40
     C*
     C                   MOVEL     SCFPRT        FPFPRT
     C                   MOVEL     SCDESC        FPDESC
     C                   MOVEL     SCDOCT        FPDOCT
     C                   MOVEL     SCTSIR        FPTSIR
     C                   MOVEL     SCLBR         FPLBR
     C                   MOVEL     SCPART        FPPART
     C                   MOVEL     SCPPGM        FPPPGM
     C                   MOVEL     SCPLIB        FPPLIB
     C                   MOVEL     SCIMGQ        FPIMGQ
     C                   MOVE      SCSTAT        FPSTAT
     C                   MOVEL     Q1USER        FPLUSR
     C                   MOVEL     Q1PGM         FPLPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      FPLDT
     C                   Z-ADD     DSTIME        FPLTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PFP01
     C                   ELSE
     C*
     C     SCFPRT        IFNE      *BLANKS
     C                   MOVEL     SCFPRT        FPFPRT
     C                   MOVEL     SCDOCT        FPDOCT
     C                   MOVEL     SCDESC        FPDESC
     C                   MOVEL     SCTSIR        FPTSIR
     C                   MOVEL     SCLBR         FPLBR
     C                   MOVEL     SCPART        FPPART
     C                   MOVEL     SCPPGM        FPPPGM
     C                   MOVEL     SCPLIB        FPPLIB
     C                   MOVEL     SCIMGQ        FPIMGQ
     C                   MOVE      SCSTAT        FPSTAT
     C                   MOVEL     Q1USER        FPIUSR
     C                   MOVEL     Q1PGM         FPIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      FPIDT
     C                   Z-ADD     DSTIME        FPITIM
     C*
     C                   WRITE     PFP01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCFPRT
     C                   MOVE      *BLANKS       XXFPRT
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPFP01     E            PFP01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
