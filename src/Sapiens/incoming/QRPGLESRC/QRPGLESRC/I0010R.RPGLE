     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE I0010R - FAX RECIPIENT MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, delete, change, or    *
     H*    view fascimile recipient information.  This is a           *
     H*    Fascimile/400 Interface program.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    I0510R - Fax Recipient Id Search                           *
     H*    I0315R - Output Class Search                               *
     H*                                                               *
     H*  PROGRAMMER: Marie Nguyen                                     *
     H*  CREATED:    10/24/91             PROJECT: AJX                *
     H******************************************************************
     H* INDICATOR USAGE                                               **
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    INVALID RECIPIENT ID                                   **
     H* SCR02:                                                        **
     H*  20    DESCRIPTION REQUIRED                                   **
     H*  21    FAX PHONE # REQUIRED                                   **
     H*  22    OUTPUT CLASS INVALID                                   **
     H*  23    OUTPUT CLASS INACTIVE                                  **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ FAX RECIPIENT FILE                        **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMRFL   (PRF01): RCID                                      *
     H*    IMOCL   (POC01): OUTC                                      *
     H*****************************************************************
     FI0010F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMFRL     UF A E           K DISK
     FIMOCL     IF   E           K DISK

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
     C*   PARM LIST FAX RECIPIENT ID SEARCH
     C*
     C     PL0510        PLIST
     C                   PARM                    SCRCID
     C                   PARM                    QQRTC             1
     C*
     C*   PARM LIST OUTPUT CLASS SEARCH
     C*
     C     PL0315        PLIST
     C                   PARM                    SCOUTC
     C                   PARM                    QQRTC
     C*
     C*   KEY LIST FOR FAX RECIPIENT FILE
     C     FRKY          KLIST
     C                   KFLD                    SCRCID
     C*
     C*   KEY LIST FOR OUTPUT CLASS FILE
     C     OCKY          KLIST
     C                   KFLD                    SCOUTC
     C*
      /Free
       scPrDs = 'Fax Recipient Maintenance';
      /End-Free
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
     C*** BEG CHG *** 03/08/07 *****************************
     C****F5*=*SEARCH*LIST*CALL                  *
     C********** *INKE     IFEQ '1'
     C********************************************
     C*   F10 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKJ         IFEQ      *ON
     C*** END CHG *** 03/08/07 *****************************
     C********************************************
     C* SCR01 = RECIPIENT ID SEARCH              *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'I0510R'      PL0510
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCRCID        OREQ      *BLANK
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
     C*   F10 = TABLE SEARCH                     *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C********************************************
     C*  OUTPUT CLASS SEARCH - SCR02             *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C*** BEG CHG *** 03/08/07 *****************************
     C*******    SCOUTC    IFEQ '? '
     C*******    SCOUTC    OREQ ' ?'
     C     HFLD          IFEQ      'SCOUTC'
     C*** END CHG *** 03/08/07 *****************************
     C                   CALL      'I0315R'      PL0315
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCOUTC        OREQ      *BLANK
     C                   MOVE      '1'           *IN30                          STAY @ CLASS FIELD
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT                     31     GO ON TO TIME FIELD
     C                   END                                                    END '? '
     C                   END                                                    END SCR02
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
     C*   F23 = DELETE RECIPIENT                 *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     FRKY          DELETE    PFR01                              60
     C                   MOVE      *BLANK        SCRCID
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKX
     C                   END                                                    END CMDKEYS
     C*                                                                ********
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
     C*  RECIPIENT ID CANNOT BE EQUAL TO BLANKS
     C*
     C     SCRCID        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCRCID        CHAIN     PFR01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PFR01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      FRDESC        SCDESC                         DESCRIPTION
     C                   MOVE      FRKEY         SCKEY                          APPLICATION KEY
     C                   MOVE      FRFPH#        SCFPH#                         TO FAX PHONE#
     C                   MOVE      FROUTC        SCOUTC                         OUTPUT CLASS
     C                   MOVE      FRDSTM        SCDSTM                         DEFAULT SEND TIME
     C                   MOVE      FRCPTI        SCCPTI                         COVER PAGE TITLE
     C                   MOVE      FRCPT1        SCCPT1                         TO--LINES 1-3
     C                   MOVE      FRCPT2        SCCPT2
     C                   MOVE      FRCPT3        SCCPT3
     C                   MOVE      FRCPF1        SCCPF1                         FROM--LINES 1-3
     C                   MOVE      FRCPF2        SCCPF2
     C                   MOVE      FRCPF3        SCCPF3
     C                   MOVE      FRCPCM        SCCPCM                         COMMENT
     C*
     C                   MOVE      FRSTAT        SCSTAT
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
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCFPH#
     C                   MOVE      *BLANKS       SCOUTC
     C                   MOVE      *ZEROS        SCDSTM
     C                   MOVE      *BLANKS       SCCPTI
     C                   MOVE      *BLANKS       SCCPT1
     C                   MOVE      *BLANKS       SCCPT2
     C                   MOVE      *BLANKS       SCCPT3
     C                   MOVE      *BLANKS       SCCPF1
     C                   MOVE      *BLANKS       SCCPF2
     C                   MOVE      *BLANKS       SCCPF3
     C                   MOVE      *BLANKS       SCCPCM
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
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
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  FAX PHONE # CANNOT BE EQUAL TO BLANKS
     C*
     C     SCFPH#        CABEQ     *BLANKS       STRT                     21
     C*
     C*  OUTPUT CLASS MUST EXIST IN IMOCP & ACTIVE
     C*
     C     SCOUTC        CABEQ     *BLANKS       STRT                     22
     C     OCKY          CHAIN     POC01                              22
     C     *IN22         CABEQ     '1'           STRT
     C  N22OCSTAT        CABEQ     'I'           STRT                     23
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     FRKY          CHAIN     PFR01                              40
     C                   MOVEL     SCDESC        FRDESC
     C                   MOVE      SCKEY         FRKEY
     C                   MOVE      SCFPH#        FRFPH#
     C                   MOVE      SCOUTC        FROUTC
     C                   MOVE      SCDSTM        FRDSTM
     C                   MOVE      SCCPTI        FRCPTI
     C                   MOVE      SCCPT1        FRCPT1
     C                   MOVE      SCCPT2        FRCPT2
     C                   MOVE      SCCPT3        FRCPT3
     C                   MOVE      SCCPF1        FRCPF1
     C                   MOVE      SCCPF2        FRCPF2
     C                   MOVE      SCCPF3        FRCPF3
     C                   MOVE      SCCPCM        FRCPCM
     C                   MOVE      SCSTAT        FRSTAT
     C                   MOVEL     Q1USER        FRUSER
     C                   MOVEL     Q1PGM         FRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRDT
     C*
     C                   Z-ADD     DSTIME        FRTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PFR01
     C                   ELSE
     C                   MOVEL     SCRCID        FRRCID
     C                   WRITE     PFR01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCRCID
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPFR01     E            PFR01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
