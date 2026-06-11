     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE SC123R - DISPLAY FAX LOG
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays all existing OUTGOING FAX records    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*                                                               *
     H*  PROGRAMMER: 02                                               *
     H*  CREATED:    07/20/95             PROJECT:                    *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/16/99 C:61  SUPPORT OP048 18833                            *
     H*                Correct display of time.                       *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     H*                                                               *
     H*                                                               *
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTION         *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILE USAGE:                                                  *
     H*    SAFTL   (PFT01):
     H*                                                               *
     H*****************************************************************
      *
      *
     FSC123F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FSAFTL1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FAXFRM          C                   CONST('                ')

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*   KEY LIST FOR INCOMING FAX FILE
     C*
     C     FTKY          KLIST
     C                   KFLD                    SCUSRI
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C                   MOVE      *BLANKS       SCUSRI                         REPOSITION KEY
     C*
      /Free
        scPRDS = 'Outgoing Fax Log';
      /End-Free
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     FTKY          SETLL     PFT01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
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
     C                   MOVE      'A'           QQRTCD            1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   MOVE      #@$2          RRN2
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCUSRI        IFNE      *BLANKS
     C                   GOTO      TOP
     C                   END
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD     N
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C***
     C*** LOAD QQ FIELDS HERE &&&*((*((*()
     C                   CALL      'SC124R'
     C                   PARM      SCJOBN        QQJOBN           10            SPFL JOB NAME
     C                   PARM      SCUSRN        QQUSRN           10
     C                   PARM      SCJOB#        QQJOB#            6            SPLF JOB NUMBER
     C                   PARM      SCFILN        QQFILN           10            SPLF NAME  4
     C                   PARM      SCFIL#        QQFIL#            3            SPLF NUMBER
     C                   PARM      *BLANK        QQRCOD            1
     C*
     C*
     C     QQRCOD        IFNE      'E'                                          (DARTS CREATED)
     C*
     C*
     C*
     C                   ENDIF
     C***
      *
     C                   MOVEL     ' '           SCSEL
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN67
     C                   GOTO      STRT
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   ELSE                                                   USER BLANK OUT SELCT
     C                   UPDATE    SCR01S
     C                   GOTO      CKSEL
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE                                                   ELSE 0 *IN50
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN50
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
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANK        SCSEL
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      PFT01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C                   MOVEL     FTSUSR        SCSUSR
     C                   MOVEL     FTCPT1        SCCPT1
     C                   MOVEL     FTFPH#        SCFPH#
     C*
     C                   Z-ADD     FTFCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFCDT
     C*
      ***** BEG DEL ******* 08/16/99 **********************************
     C*****************    MOVELFTFCTM    SCFCTM
      ***** END DEL ******* 08/16/99 **********************************
      ***** BEG ADD ******* 08/16/99 **********************************
     C                   Z-ADD     FTFCTM        SCFCTM
      ***** END ADD ******* 08/16/99 **********************************
     C                   MOVEL     FTFAXC        SCFAXC
     C*
     C                   MOVEL     FTJOBN        SCJOBN                         SPFL JOB NAME
     C                   MOVEL     FTUSRN        SCUSRN
     C                   MOVEL     FTJOB#        SCJOB#                         SPLF JOB NUMBER
     C                   MOVEL     FTFILN        SCFILN                         SPLF NAME  4
     C                   MOVEL     FTFIL#        SCFIL#                         SPLF NUMBER
     C*
     C     FTFAXC        IFNE      'E'
     C                   MOVE      '1'           *IN67
     C                   ELSE
     C                   MOVE      '0'           *IN67
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANKS       SCSUSR
     C                   MOVEL     *BLANKS       SCCPT1
     C                   MOVEL     *BLANKS       SCFPH#
     C                   MOVEL     *ZEROS        SCFCDT
      ***** BEG DEL ******* 08/16/99 **********************************
     C*****************    MOVEL*ZEROS    SCFCTM
      ***** END DEL ******* 08/16/99 **********************************
      ***** BEG ADD ******* 08/16/99 **********************************
     C                   Z-ADD     0             SCFCTM
      ***** END ADD ******* 08/16/99 **********************************
     C                   MOVEL     *BLANKS       SCFAXC
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   MOVE      '1'           *IN67
     C                   WRITE     SCR01S
     C                   END
     C                   MOVE      '0'           *IN67
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   MOVEL     *BLANK        SCUSRI
     C*
     C                   ENDSR
