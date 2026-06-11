     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS120R2 - WORK WITH QUEUE ID'S - SHARED, USER            *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF ALL SHARED QUEUE ID'S(USER)*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
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
     H*    IMIQL3  (PRT01): ID    - VIEW 1                            *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  05/09/03 - C:087  Project: support
     H*             Change pgm to use the DARTS security file, SMSLP,
     H*             to condition records
      *
      * 09/06/02 C:87  Project: DR170
      *                Subfile processing was causing the program to end
      *                in error.  Modified appropriate code to correct.
      *
      * 06/04/02 C:87  Project: DR170
      *                Added a function key, F8, to display/hide the
      *                number of documents in an indexing queue.  The
      *                # of docs is initially not calculated but if the
      *                user presses F8, the number of docs (for those
      *                queues that have docs) is displayed.  You can
      *                hide this value by pressing F8 again.
     H*
     H*  04/18/01 - C:117  PROJECT 114IBZ
     H*             CONDITION THE RECORDS THAT WILL BE DISPLAYED
     H*             AGAINST THE USER'S SECURITY LEVEL
     H*
     H*****************************************************************
     FIS120F3   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F*
     FIMIQL     IF   E           K DISK
     F                                     RENAME(PIQ01:PIQ02)
     F*
     FIMIQL3    IF   E           K DISK
     FSMSLL2    IF   E           K DISK
     FIMIELE    IF   E           K DISK

     D*
     D NOLNK           C                   CONST('No Queues to display')
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
     C     IQKY          KLIST
     C                   KFLD                    SCQID
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    Q1USER
     C*
      **
     C     *ENTRY        PLIST
     C                   PARM                    QQQID            10
     C                   PARM                    QQDESC           40
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQRTCD        FRMTO             1
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C* IND. 45 MEANS THIS IS SELECT OF FROM QUEUE!
     C* NOT 45 IS SELECT OF TO QUEUE ****
     C     FRMTO         COMP      'F'                                    45
      /Free
       Select;
       When frmTo = 'F';
          scPrDs = 'Select from Queue';
       Other;
          scPrDs = 'Select to Queue';
       Endsl;
      /End-free
     C                   MOVEL     QQQID         SCQID            10
      **
     C                   Z-ADD     0             SC#RC1
     C*
     C     Q1USER        CHAIN     PIQ02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     IQDESC        SCDSC1
     C                   MOVEL     IQID          SCID1
     C                   MOVEL     *BLANKS       SCSEL1
     C                   MOVE      *ON           *IN89
     C*   loop through and count number of documents
     C     IQID          CHAIN     PIE01                              61
     C     *IN61         DOWEQ     *OFF
     C                   ADD       1             SC#RC1
     C     IQID          READE     PIE01                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C                   ELSE
     C                   MOVE      *OFF          *IN89
     C*
     C                   ENDIF
     C*
     C                   MOVEL     '*All    '    SCQUET
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     IQKY          SETLL     PIQ01                              60
     C*
     C*
     C*   CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     *BLANKS       SCQID
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
     C                   Z-ADD     Q2LSFR        RRNTRK            5 0
     C                   ENDIF
     C                   MOVE      '0'           *IN01
     C*
     C*====================================================================
     C*   PROCESS ALL COMMAND KEYS
     C*====================================================================
     C*
     C     *IN11         IFEQ      '1'
     C*====================================================================
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C*====================================================================
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
     C                   MOVE      *OFF          *IN10
     C                   END                                                    END *IN10
     C*====================================================================
     C*   F8 = SHOW #OF DOCS                 *
     C*====================================================================
     C     *INKH         IFEQ      '1'
     C     '0'           COMP      *IN80                                  80
     C*
     C                   EXSR      INIT
     C     *LOVAL        SETLL     PIQ01                              60
     C*
     C*              CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     RRNTRK        #SFRCS            3 0
     C     #SFRCS        DOULE     0
     C                   EXSR      LOAD
     C                   SUB       13            #SFRCS
     C                   ENDDO                                                  #SFRCS<=0
     C*
     C*    Set the cursor to the first record of the page to be displayed
     C                   Z-ADD     RRNTRK        #@$2A
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKH='1'
     C*
     C*====================================================================
     C*   *IN18 -  ROLLUP REQUEST            *
     C*====================================================================
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C*====================================================================
     C*   *INKC -  EXIT                      *
     C*====================================================================
     C     *INKC         IFEQ      '1'                                          EXIT
     C                   MOVEL     'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P
     C*
     C*====================================================================
     C*   *INKL -  Cancel                    *
     C*====================================================================
     C     *INKL         IFEQ      '1'                                          EXIT
     C                   MOVEL     'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*
     C*   IF A REPOSITION WAS REQUESTED
     C*
     C     SCQID         IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF
     C*                                                                     ****
     C     SCSEL1        IFNE      *BLANKS
     C     SCSEL1        CABNE     '1'           STRT                 2020
     C                   MOVEL     SCID1         QQQID
     C                   MOVEL     SCDSC1        QQDESC
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C                   READC     SCR01SA                                50
     C     *IN50         IFEQ      '0'
     C*
     C*   IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*   A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01SA
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   INVALID SEL     LCT
     C* * *
     C     SCSEL         IFEQ      ' '
     C                   UPDATE    SCR01SA
     C                   GOTO      CKSEL
     C                   END                                                    END EQ ' '
     C*
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** Edit Entry
     C*
     C*-------------------------------------------------------------------
     C* select "1" - Select
     C*-------------------------------------------------------------------
     C     SCSEL         IFEQ      '1'
      *
     C                   MOVE      ' '           QQRTCD
     C*
     C                   MOVEL     SCID          QQQID
     C                   MOVEL     SCDESC        QQDESC
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                     EQ '1'
     C*
     C*   IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*   WITHOUT A SELECTED RECORD
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
     C                   MOVEL     *BLANK        SCSEL
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01  '     Q2RECF
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
      /Free
       *in21 = *on;
      /End-free
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C     NXT1          TAG
     C*
     C                   Z-ADD     0             SC#REC
     C*
     C                   READ      PIQ01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C*
     C     SLKY          CHAIN     PSL01                              59
     C                   MOVE      IQSECV        XXSECV            3 0
     C     *IN60         IFEQ      '0'
     C     *IN59         ANDEQ     '0'
     C     SLSECL        ANDGE     XXSECV
     C*
     C     IQID          CABEQ     SCID1         NXT1
     C*
     C                   ADD       1             RRN2A
     C                   MOVEL     IQID          SCID
     C                   MOVEL     IQDESC        SCDESC
     C*
     C*  If *in80 is *on, loop through and count number of documents
     C     IQID          CHAIN     PIE01                              61
     C   80*IN61         DOWEQ     *OFF
     C                   ADD       1             SC#REC
     C     IQID          READE     PIE01                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
      /Free
       Select;
       When *in21 = *on;
          *in21 = *off;
       Endsl;
      /End-free
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     *IN59         ANDEQ     *OFF
     C     X             ANDGE     1
     C     X             SUB       1             X
     C                   ENDIF
     C                   MOVE      '1'           *IN05
     C                   ENDIF
     C                   ENDDO
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2A         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCID
     C                   MOVEL     NOLNK         SCDESC
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   END
     C*
     C* TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2A         #@$2A
     C*
     C                   ENDSR
