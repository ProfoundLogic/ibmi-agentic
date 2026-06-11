     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS146R - SEARCH    DIST/LB ENTRIES                       *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF DIST/LB RECORDS       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/07/00 C:128 PROJECT: DR122                                 *
     H*                Changed to use new 7/0 date field in IMDLP.    *
     H***                                                             *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTION         *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMDLL   (PDL01): FAXPORT - VIEW 1                          *
     H*****************************************************************
     FIS146F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F*
      *** BEGIN CHG ***07/17/00 ************************************************
     F******IMDLL2  IF  E           K        DISK
     FIMDLL4    IF   E           K DISK
      *** END CHG *****07/17/00 ************************************************

     D*
     D NOLNK           C                   CONST('No entries')
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C     DLKY          KLIST
     C                   KFLD                    SCFAXP
     C     DLKY1         KLIST
     C                   KFLD                    SCQUE            10
     C*
     C     DLKYX         KLIST
     C                   KFLD                    SCID
     C                   KFLD                    SCIMGQ           10
     C*
     C     GTKY          KLIST
     C                   KFLD                    DLID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDLID           10
     C                   PARM                    QQRTCD            1
      /Free
       scPrDs = 'Work with Distribution/Load Balance ID';
      /End-Free
     C*
     C*
      *------------------------*
      *   PARAMET2R LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C                   MOVE      *BLANKS       SCFAXP                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCQUE                          REPOSITION KEY
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     DLKY          SETLL     PDL01                              60
     C*
     C                   MOVEL     *BLANKS       SCFAXP
     C                   MOVEL     *BLANKS       SCQUE
     C*
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
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
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CA
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN01
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
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
     C                   MOVE      #@$2A         RRN2A
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C****************************************
     C*   *INKC -  EXIT                      *
     C****************************************
     C     *INKC         IFEQ      '1'                                          EXIT
     C                   MOVEL     'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P
     C*
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCFAXP        IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C                   READC     SCR01SA                                50
     C     *IN50         IFEQ      '0'
     C*
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01SA
     C*
     C                   GOTO      STRT
     C                   ELSE                                                   USER BLANK OUT SELCT
     C     SCSEL         IFEQ      ' '
     C                   UPDATE    SCR01SA
     C                   GOTO      CKSEL
     C                   END                                                    END EQ ' '
     C                   END                                                    END EQ '1'
     C*
     C     SCSEL         IFEQ      '1'
     C*** SELECT
     C                   MOVEL     SCID          QQDLID
     C                   MOVE      ' '           QQRTCD
     C                   UPDATE    SCR01SA
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  ELSE EQ '1'
     C*
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE                                                   ELSE 0 *IN50
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN50
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
     C                   Z-ADD     0             RRN2A             5 0
     C                   Z-ADD     0             RRN2B             5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01  '     Q2RECF
     C                   MOVEL     'B'           @VIEW             1
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
     C*
     C                   READ      PDL01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2A
     C                   MOVEL     DLID          SCID
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C     GTKY          SETGT     PDL01
     C                   ENDDO
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2A         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCIMGQ
     C                   MOVE      *BLANK        SCSTAT            1
     C                   MOVEL     NOLNK         SCID
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   ENDIF
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2A         #@$2A
     C*
     C                   MOVEL     *BLANK        SCID
     C*
     C                   ENDSR
