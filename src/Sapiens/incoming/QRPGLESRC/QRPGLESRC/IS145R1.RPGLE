     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS145R1 - FAX ROUTING - ADD/UPDATE LOAD BALANCE RCDS
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
     H*    IMRTL   (PLB01): TSID                                      *
     H*
     H*****************************************************************
     FIS145F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMLBL     UF A E           K DISK
     FIMIQL     IF   E           K DISK
     FIMFPL     IF   E           K DISK

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

     DstandardKeys     s                   like(av_keys)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFAXP           20
     C                   PARM                    QQCALL            1
     C*
     C*
     C*   KEY LIST FOR DOC INDEX FILE
     C     LBKY          KLIST
     C                   KFLD                    XXFAXP           12
     C*
      /Free
       scPrDs = 'Add/Change Load Balance entry';
       standardKeys = 'F3=Exit'  + '   ' +   'F10=Search' + '   ' +
        'F12=Cancel' + '   ' + 'F20=Change Sts';
      /End-Free
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
     C                   MOVEL     QQFAXP        XXFAXP
     C     LBKY          CHAIN     PLB01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PLB01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     LBIFXP        SCIFXP
     C                   MOVEL     LBIMGQ        SCIMGQ
     C*
     C*
     C                   MOVE      LBSTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C                   MOVEL     CD(1)         SCCMDD                         |
      /Free
       av_keys = %trim(standardKeys) + '   ' + cd(1);
      /End-free
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                         |
      /Free
       av_keys = %trim(standardKeys);
      /End-free
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
     C                   MOVE      *BLANKS       SCIMGQ
     C                   MOVE      *BLANKS       SCIFXP
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
      /Free
       av_keys = %trim(standardKeys);
      /End-free
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
     C     FLD           IFEQ      'SCIMGQ  '
     C*** CALL IMGQ SEARCH PROGRAM
     C                   CALL      'IS120R'
     C                   PARM                    SCIMGQ
     C                   PARM                    ##DSC            40
     C                   PARM                    ##RTC             1
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
     C                   MOVEL     CD(1)         SCCMDD                          |
      /Free
       av_keys = %trim(standardKeys) + '   ' + cd(1);
      /End-free
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
      /Free
       av_keys = %trim(standardKeys);
      /End-free
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE ROUTING RCD        *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     LBKY          DELETE    PLB01                              60
     C                   MOVE      *BLANK        XXFAXP
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
     C*
     C*  TSI CODE ENTERED CANT ALREADY EXIST
     C*
     C*** ONLY FOR ADD DO THIS EDIT ***************************
     C     LBKY          CHAIN     PLB01                              60
     C  N60LBIMGQ        CABEQ     SCIMGQ        STRT                     21
     C*
     C                   ENDIF
     C*
     C     SCIFXP        CABEQ     *BLANKS       STRT                     23
     C     SCIFXP        CHAIN     PFP01                              60
     C     *IN60         CABEQ     *ON           STRT                     23
     C*
     C*** DESC NOT EQUAL BLANKS    ***************************
     C     SCIMGQ        CABEQ     *BLANKS       STRT                     22
     C*
     C     SCIMGQ        CHAIN     PIQ01                              60
     C     *IN60         CABEQ     *ON           STRT                     22
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     LBKY          CHAIN     PLB01                              40
     C*
     C                   MOVEL     SCIFXP        LBIFXP
     C                   MOVEL     SCIMGQ        LBIMGQ
     C                   MOVE      SCSTAT        LBSTAT
     C                   MOVEL     Q1USER        LBLUSR
     C                   MOVEL     Q1PGM         LBLPGM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PLB01
     C                   ELSE
     C*
     C                   MOVEL     SCIFXP        LBIFXP
     C                   MOVEL     SCIMGQ        LBIMGQ
     C                   MOVE      SCSTAT        LBSTAT
     C                   MOVEL     Q1USER        LBIUSR
     C                   MOVEL     Q1PGM         LBIPGM
     C*
     C                   WRITE     PLB01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       XXFAXP
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPLB01     E            PLB01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
