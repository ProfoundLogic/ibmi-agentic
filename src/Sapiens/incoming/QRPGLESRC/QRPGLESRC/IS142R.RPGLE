     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS142R - WORK WITH COVER PAGE ENTRIES                    *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF COVER PAGE ENTRIES    *
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
     H*    IMCVL   (PCV01): COVI - VIEW 1                          *
     H*    IMCVL1  (PCV01): QUEUE - VIEW 2                            *
     H*****************************************************************
     FIS142F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F                                     SFILE(SCR01SB:RRN2B)
     F*
     FIMCVL     IF   E           K DISK
     FIMCVL1    IF   E           K DISK
     F                                     RENAME(PCV01:PCV02)
     FIMCVL3    UF   E           K DISK
     F                                     RENAME(PCV01:PCV03)

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
     C     CVKY          KLIST
     C                   KFLD                    SCCOVI
     C     CVKY1         KLIST
     C                   KFLD                    SCDSC
     C*
     C     CVKYX         KLIST
     C                   KFLD                    SCCVID
     C                   KFLD                    SCDESC
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCOVI           10
     C                   PARM                    QQDSC            40
     C                   PARM                    QQRTCD            1
     C*
      /Free
       scPrDs = 'Work with Cover Page Entries';
      /End-Free
     C                   MOVEL     QQRTCD        ##RC              1            SAVE IN VAL
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C* * * CALLED AS A SEARCH ****
     C     ##RC          IFEQ      'S'
     C                   MOVE      *ON           *IN29
     C                   ELSE
     C                   MOVEL     *OFF          *IN29
     C                   ENDIF
     C*
      *------------------------*
      *   PARAMET2R LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C                   MOVE      *BLANKS       SCCOVI                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCDSC                          REPOSITION KEY
     C                   MOVE      QQCOVI        SCCOVI
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     @VIEW         IFEQ      'A'
     C     CVKY          SETLL     PCV01                              60
     C                   ELSE
     C     CVKY1         SETLL     PCV02                              60
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SCCOVI
     C                   MOVEL     *BLANKS       SCDSC
     C*
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C     @VIEW         IFEQ      'A'
     C                   WRITE     SCR01CA
     C                   ELSE
     C                   WRITE     SCR01CB
     C                   ENDIF
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     @VIEW         IFEQ      'A'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CA
     C                   ENDIF
     C                   ELSE
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01CB
     C                   ENDIF
     C                   MOVE      '0'           *IN01
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
     C*   *IN10 -  PROCESS HELP REDSCSTS     *
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
     C*   *IN18 -  ROLLUP REDSCST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C     @VIEW         IFEQ      'A'
     C                   MOVE      #@$2A         RRN2A
     C                   ELSE
     C                   MOVE      #@$2B         RRN2B
     C                   ENDIF
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
     C*   *INKF -  CREATE NEW COVER PAGE RECORD *
     C****************************************
     C     *INKF         IFEQ      '1'                                          ROLLUP
     C                   CALL      'IS142R1'
     C                   PARM      *BLANKS       QQCOVI           10
     C                   PARM      *BLANKS       QQDESC           10
     C                   PARM      *BLANKS       QQRTC1            1
     C     QQRTC1        CABEQ     'A'           TERM
     C*
     C                   GOTO      TOP
     C                   END                                                    END *INKF        P
     C*
     C*
     C****************************************
     C*   *INKK -  CHANGE VIEW               *
     C****************************************
     C     *INKK         IFEQ      '1'                                          ROLLUP
     C     @VIEW         IFEQ      'A'
     C                   MOVEL     'B'           @VIEW
     C                   ELSE
     C                   MOVEL     'A'           @VIEW
     C                   ENDIF
     C*
     C                   GOTO      TOP
     C                   END                                                    END *INKK        P
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     @VIEW         IFEQ      'A'
     C     SCCOVI        IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF
     C                   ELSE
     C     SCDSC         IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF
     C                   ENDIF
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     CKSEL         TAG
     C     @VIEW         IFEQ      'A'
     C                   READC     SCR01SA                                50
     C                   ELSE
     C                   READC     SCR01SB                                50
     C                   ENDIF
     C     *IN50         IFEQ      '0'
     C*
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     ' '
     C     SCSEL         ANDNE     '5'
     C     *IN29         ANDEQ     *ON
     C                   MOVE      '1'           *IN20
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCSEL         IFEQ      '1'
     C     *IN29         ANDEQ     *OFF
     C                   MOVE      '1'           *IN20
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '2'
     C     SCSEL         ANDNE     '1'
     C     SCSEL         ANDNE     '4'
     C     SCSEL         ANDNE     '8'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C                   ELSE                                                   USER BLANK OUT SELCT
     C     SCSEL         IFEQ      ' '
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C                   GOTO      CKSEL
     C                   END                                                    END EQ ' '
     C                   END                                                    END EQ '1'
     C*
     C     SCSEL         IFEQ      '1'
     C*** EDIT ITEM
     C                   MOVEL     SCCVID        QQCOVI
     C                   MOVEL     SCDESC        QQDSC
     C                   MOVEL     '*'           QQRTCD
     C*
     C                   GOTO      TERM
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELSE EQ '2'
     C*
     C*
     C     SCSEL         IFEQ      '2'
     C*** EDIT ITEM
     C                   MOVE      ' '           QQRTCD
     C                   CALL      'IS142R1'
     C                   PARM      SCCVID        QQCOVI           10
     C                   PARM      SCDESC        QQDESC           10
     C                   PARM      *BLANKS       QQRTC1            1
     C     QQRTC1        CABEQ     'A'           TERM
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELSE EQ '2'
     C*
     C     SCSEL         IFEQ      '4'
     C*** REMOVE
     C                   MOVE      ' '           QQRTCD
     C     CVKYX         DELETE    PCV03                              60
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   GOTO      TOP
     C                   ENDIF                                                  ELSE EQ '2'
     C*
     C*
     C     SCSEL         IFEQ      '5'
     C*** GET THE ***
     C*** IMAGE DISPLAY
     C                   MOVEL     *BLANKS       QQRTN
     C                   MOVEL     'GENERIC '    X6DTYP            8
     C                   MOVEL     *ALL'@'       X6DESC           40
     C*
     C                   CALL      'IS211C'
     C                   PARM                    SCDCID           12
     C                   PARM                    SCFLR             8
     C                   PARM                    SCSFLR           12
     C                   PARM      X6DTYP        XXDOCT            8
     C                   PARM      X6DESC        XXDESC           40
     C                   PARM      *BLANKS       QQRTN             6
      *
      *
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELSE EQ '5'
     C*
     C     SCSEL         IFEQ      '8'
     C*** CHG STATUS (INACTIVATE/ACTIVATE)
     C                   MOVE      ' '           QQRTCD
     C     CVKYX         CHAIN     PCV03                              60
     C     *IN60         IFEQ      *OFF
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           CVSTAT
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           CVSTAT
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   UPDATE    PCV03
     C                   END
     C                   MOVEL     *BLANKS       SCSEL
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELSE EQ '2'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE                                                   ELSE 0 *IN50
     C   29              GOTO      TERM
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
     C     @VIEW         IFEQ      'B'
     C                   MOVEL     'B'           @VIEW             1
     C                   ELSE
     C                   MOVEL     'A'           @VIEW
     C                   ENDIF
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
     C     @VIEW         IFEQ      'A'
     C                   READ      PCV01                                  60
     C                   ELSE
     C                   READ      PCV02                                  60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     @VIEW         IFEQ      'A'
     C                   ADD       1             RRN2A
     C                   ELSE
     C                   ADD       1             RRN2B
     C                   ENDIF
     C                   MOVEL     CVCVID        SCCVID
     C                   MOVEL     CVDESC        SCDESC
     C                   MOVEL     CVSTAT        SCSTAT
     C                   MOVEL     CVDCID        SCDCID
     C                   MOVEL     CVFLR         SCFLR
     C                   MOVEL     CVSFLR        SCSFLR
     C*
     C*
     C                   MOVE      '0'           *IN06
     C     @VIEW         IFEQ      'A'
     C                   WRITE     SCR01SA
     C                   ELSE
     C                   WRITE     SCR01SB
     C                   ENDIF
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     @VIEW         IFEQ      'A'
     C     RRN2A         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCCVID
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVEL     NOLNK         SCDESC
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   END
     C                   ELSE
     C     RRN2B         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCCVID
     C                   MOVEL     NOLNK         SCDESC
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   Z-ADD     1             RRN2B
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SB
     C                   ENDIF
     C                   ENDIF
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C     @VIEW         IFEQ      'A'
     C                   Z-ADD     RRN2A         #@$2A
     C                   ELSE
     C                   Z-ADD     RRN2B         #@$2B
     C                   ENDIF
     C*
     C                   MOVEL     *BLANK        SCDESC
     C                   MOVEL     *BLANK        SCSTAT
     C                   MOVEL     *BLANK        SCCVID
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCFLR
     C*
     C                   ENDSR
