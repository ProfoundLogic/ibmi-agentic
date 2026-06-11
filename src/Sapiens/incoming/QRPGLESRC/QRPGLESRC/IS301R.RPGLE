     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS301R - WORK WITH FAX ROUTING                           *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program displays A LIST OF FAX ROUTING ENTRIES        *
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
     H*    IMRTL   (PRT01): TSID  - VIEW 1                            *
     H*    IMRTL1  (PRT01): DESC  - VIEW 2                            *
     H*****************************************************************
     FIS301F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2A)
     F                                     SFILE(SCR01SB:RRN2B)
     F*
     FIMRTL     IF   E           K DISK
     FIMRTL1    IF   E           K DISK
     F                                     RENAME(PRT01:PRT02)

     D*
     D NOLNK           C                   CONST('No routing entries')
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
     C     RTKY          KLIST
     C                   KFLD                    SCTSIP
     C     RTKY1         KLIST
     C                   KFLD                    SCDSCP
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C*
      /Free
       scPrDs = 'Work with Fax Routing';
      /End-Free
     C*
      *------------------------*
      *   PARAMET2R LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C                   MOVE      *BLANKS       SCTSIP                         REPOSITION KEY
     C                   MOVE      *BLANKS       SCDSCP                         REPOSITION KEY
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
     C     RTKY          SETLL     PRT01                              60
     C                   ELSE
     C     RTKY1         SETLL     PRT02                              60
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SCTSIP
     C                   MOVEL     *BLANKS       SCDSCP
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
     C*   *INKF -  CREATE NEW ROUTING RECORD *
     C****************************************
     C     *INKF         IFEQ      '1'                                          ROLLUP
     C                   CALL      'IS301R1'
     C                   PARM      *BLANKS       QQPTSI           20
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
     C     SCTSIP        IFNE      *BLANKS
     C                   GOTO      TOP
     C                   ENDIF
     C                   ELSE
     C     SCDSCP        IFNE      *BLANKS
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
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     '2'
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
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C     SCSEL         IFEQ      '1'
     C*** WORK WITH ACTIONS
     C*
     C                   MOVE      ' '           QQRTCD
     C                   CALL      'IS302R'
     C                   PARM                    SCTSID
     C                   PARM                    SCDESC
     C                   PARM                    Q@RTC             1
     C*
     C     Q@RTC         CABEQ     'A'           TERM
     C*
     C*
     C                   MOVEL     *BLANKS       SCSEL
     C     @VIEW         IFEQ      'A'
     C                   UPDATE    SCR01SA
     C                   ELSE
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELSE EQ '1'
     C     SCSEL         IFEQ      '2'
     C*** EDIT ITEM
     C                   MOVE      ' '           QQRTCD
     C                   CALL      'IS301R1'
     C                   PARM      SCTSID        QQPTSI           20
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
     C                   READ      PRT01                                  60
     C                   ELSE
     C                   READ      PRT02                                  60
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
     C                   MOVEL     RTTSID        SCTSID
     C                   MOVEL     RTDESC        SCDESC
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
     C                   MOVE      *BLANK        SCTSID
     C                   MOVEL     NOLNK         SCDESC
     C                   Z-ADD     1             RRN2A
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   END
     C                   ELSE
     C     RRN2B         IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANK        SCTSID
     C                   MOVEL     NOLNK         SCDESC
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
     C                   MOVEL     *BLANK        SCTSID
     C                   MOVEL     *BLANK        SCDESC
     C*
     C                   ENDSR
