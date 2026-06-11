     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE I0015R - OUTPUT CLASS MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, delete, change, or    *
     H*    view output classes for Fascimile/400 Interface.           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    I0315R - Output Class Search                               *
     H*                                                               *
     H*  PROGRAMMER: Marie Nguyen                                     *
     H*  CREATED:    10/23/91             PROJECT: AJX                *
     H******************************************************************
     H* INDICATOR USAGE                                               **
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    OUTPUT CLASS REQUIRED                                  **
     H* SCR02:                                                        **
     H*  20    DESCRIPTION REQUIRED                                   **
     H*  21    ACTION CODE INVALID                                    **
     H*  22    FROM TIME CANNOT BE ZERO                               **
     H*  23    TO TIME CANNOT BE ZERO                                 **
     H*  24    FROM & TO TIME MUST BE IN ASCENDING ORDER              **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ OUTPUT CLASS FILE                         **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMOCL   (POC01): OUTC                                      *
     H*****************************************************************
     FI0015F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMOCL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXTIME                 1      6  0
     D  XXMIN                  3      4  0
     D  XXSEC                  5      6  0
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
     C*   PARM LIST OUTPUT CLASS SEARCH
     C*
     C     PL0315        PLIST
     C                   PARM                    SCOUTC
     C                   PARM                    QQRTC             1
     C*
     C*   KEY LIST FOR OUTPUT CLASS FILE
     C     OCKY          KLIST
     C                   KFLD                    SCOUTC
     C*
     C                   TIME                    DSDTTM
      /Free
       scPRDS = 'Output Class Profile';
      /End-Free
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
     C* SCR01 = OUTPUT CLASS SEARCH              *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'I0315R'      PL0315
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCOUTC        OREQ      *BLANK
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
     C*   F23 = DELETE CODE                      *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     OCKY          DELETE    POC01                              60
     C                   MOVE      *BLANKS       SCOUTC
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
     C*  OUTPUT CLASS CANNOT BE EQUAL TO BLANKS
     C*
     C     SCOUTC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCOUTC        CHAIN     POC01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    POC01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     OCDESC        SCDESC
     C                   MOVE      OCACTN        SCACTN
     C                   MOVE      OCFTIM        SCFTIM
     C                   MOVE      OCTTIM        SCTTIM
     C*
     C                   MOVE      OCSTAT        SCSTAT
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
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'T'           SCACTN
     C                   MOVE      *ZEROS        SCFTIM
     C                   MOVE      *ZEROS        SCTTIM
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
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ACTION MUST BE 'H' FOR HOLD OR 'T' FOR TRANSMIT
     C*
     C     SCACTN        IFNE      'H'
     C     SCACTN        ANDNE     'T'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  FROM & TO TIME MUST BE GREATER THAN 0 AND LESS THAN 24:00:01
     C*
     C     SCFTIM        CABLE     *ZEROS        STRT                   2222
     C     SCFTIM        CABGT     240000        STRT                 22
     C                   Z-ADD     SCFTIM        XXTIME
     C     XXMIN         CABGT     59            STRT                 22
     C     XXSEC         CABGT     59            STRT                 22
     C*
     C     SCTTIM        CABLE     *ZEROS        STRT                   2323
     C     SCTTIM        CABGT     240000        STRT                 23
     C                   Z-ADD     SCTTIM        XXTIME
     C     XXMIN         CABGT     59            STRT                 23
     C     XXSEC         CABGT     59            STRT                 23
     C*
     C*  FROM & TO TIME MUST BE IN ASCENDING ORDER
     C*
     C     SCFTIM        CABGT     SCTTIM        STRT                 24
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     OCKY          CHAIN     POC01                              40
     C                   MOVEL     SCDESC        OCDESC
     C                   MOVE      SCACTN        OCACTN
     C                   MOVE      SCFTIM        OCFTIM
     C                   MOVE      SCTTIM        OCTTIM
     C                   MOVE      SCSTAT        OCSTAT
     C                   MOVEL     Q1USER        OCUSER
     C                   MOVEL     Q1PGM         OCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OCDT
     C*
     C                   Z-ADD     DSTIME        OCTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    POC01
     C                   ELSE
     C                   MOVEL     SCOUTC        OCOUTC
     C                   MOVEL     Q1USER        OCUSER
     C                   WRITE     POC01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCOUTC
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPOC01     E            POC01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
