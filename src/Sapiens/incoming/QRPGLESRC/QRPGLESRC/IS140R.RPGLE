     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS140R - WORK WITH FAX PORTS                             *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF FAX PORTS                  *
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
     H*    IMFPL   (PRT01): FPRT  - VIEW 1                            *
     H*****************************************************************
     FIS140F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F*
     FIMFPL     UF   E           K DISK

     D*
     D NOLNK           C                   CONST('No Fax Ports      ')
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
     C     FPKY          KLIST
     C                   KFLD                    SCFAXP
     C*
     C*
     C     FPKYD         KLIST
     C                   KFLD                    SCFPRT
     C*
     C*
      **
     C     *ENTRY        PLIST
     C                   PARM                    QQFAXP           12
     C                   PARM                    QQDESC           40
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQFAXP        SCFAXP           12
      *
      /Free
       scPrds = 'Work with Fax Ports';
      /End-Free
      **
     C*** IF The Rtn code comes in as a 'S', then this is a search, and
     c*** we dont allow Create of records. Also Don't allow the Use of
     C*** select "2" or Select "4", Instead only allow use of Select=1
     C***
     C     QQRTCD        IFEQ      'S'
     C                   MOVE      *ON           *IN88
     C                   MOVE      *BLANK        QQRTCD
     C                   ENDIF
     C*
      *------------------------*
     C*
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     FPKY          SETLL     PFP01                              60
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
     C                   MOVEL     *BLANKS       SCFAXP
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
     C                   MOVE      '0'           *IN01
     C*
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
     C                   MOVE      *OFF          *IN10
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
     C****************************************
     C*   *INKF -  Perform ADD of new Fax Port
      *            Call ADD Screen
      *
     C****************************************
     C     *INKF         IFEQ      '1'                                          ROLLUP
     C     *IN88         ANDNE     *ON                                          SEARCH ONLY
     C*
     C                   CALL      'IS140R1'
     C                   PARM      *BLANKS       Q#FAXP           12
     C                   PARM      *BLANK        Q#RTC             1
     C*
     C     Q#RTC         CABEQ     'A'           TERM
     C     Q#RTC         CABEQ     'B'           TOP
     C*
     C     Q#RTC         CABEQ     *BLANK        TOP
     C*
     C                   GOTO      TOP
     C                   END                                                    END *INKF        P
     C*
     C*
     C*
     C****************************************
     C*   *INKL -  Cancel                    *
     C****************************************
     C     *INKL         IFEQ      '1'                                          EXIT
     C                   MOVEL     'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END  *INKC       P
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*********************
     C*** IF A REPOSITION WAS REQUESTED
     C*********************
     C     SCFAXP        IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF
     C*********************
     C**************************************************************************
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C                   READC     SCR01SA                                50
     C     *IN50         IFEQ      '0'
     C*
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '2'
     C     SCSEL         ANDNE     '4'
     C     SCSEL         ANDNE     '1'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01SA
     C                   GOTO      STRT
     C*
     C                   ELSE                                                   USER BLANK OUT SELCT
     C*
     C     *IN88         IFEQ      *ON                                           Srch only
     C     SCSEL         IFEQ      '2'                                             not allowd
     C     SCSEL         OREQ      '4'                                             "2" or "4"
     C                   MOVE      *ON           *IN20
     C                   UPDATE    SCR01SA
     C                   GOTO      STRT
     C                   ENDIF
     C                   ELSE                                                    Not Srch
     C     SCSEL         IFEQ      '1'                                             Not allowd
     C                   MOVE      *ON           *IN20                               "1"
     C                   UPDATE    SCR01SA
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***
     C     SCSEL         IFEQ      ' '
     C                   UPDATE    SCR01SA
     C                   GOTO      CKSEL
     C                   END                                                    END EQ ' '
     C                   END                                                    END EQ '1'
     C*
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** Edit Entry
     C*
     C*-------------------------------------------------------------------
     C**** select "1" - Select
     C*-------------------------------------------------------------------
     C     SCSEL         IFEQ      '1'
      *
     C                   MOVE      ' '           QQRTCD
     C*
     C                   MOVEL     SCFPRT        QQFAXP
     C                   MOVEL     SCDESC        QQDESC
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                     EQ '1'
     C*
     C*-------------------------------------------------------------------
     C**** select "2" - Edit entry
     C*-------------------------------------------------------------------
     C*
     C     SCSEL         IFEQ      '2'
      *
     C                   MOVE      ' '           QQRTCD
     C*
     C                   CALL      'IS140R1'
     C                   PARM                    SCFPRT
     C                   PARM      *BLANK        Q#RTC
     C*
     C     Q#RTC         CABEQ     'A'           TERM
     C     Q#RTC         CABEQ     'B'           TOP
     C*
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C                   UPDATE    SCR01SA
     C*
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  ELSE EQ '2'
      * * * Remove Entry
     C     SCSEL         IFEQ      '4'
     C***
     C***
     C     FPKYD         DELETE    PFP01                              60
     C                   MOVE      ' '           QQRTCD
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C                   UPDATE    SCR01SA
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  ELSE EQ '2'
     C****
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
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      PFP01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2A
     C                   MOVEL     FPFPRT        SCFPRT
     C                   MOVEL     FPDESC        SCDESC
     C*
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   ENDIF
     C                   ENDDO
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2A         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCFPRT
     C                   MOVE      *BLANK        SCDESC
     C                   MOVEL     NOLNK         SCDESC
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2A         #@$2A
     C*
     C                   ENDSR
