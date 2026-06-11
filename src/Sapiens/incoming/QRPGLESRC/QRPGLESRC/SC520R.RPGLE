     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC520R - CUSTOM GROUP ID SEARCH                          *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H******************************************************************
     H* REVISIONS
     H*      07/20/95  C:03   RECOMPILED DUE TO DAMAGED OBJECT
     H******************************************************************
     FSC520F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FSMCGL     IF   E           K DISK

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

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     CGKEY         KLIST
     C                   KFLD                    SCUCID
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUCID            2
     C                   PARM                    QQRFMT            1
     C                   PARM                    QQRTCD            1
     C                   MOVE      QQUCID        SCUCID
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     CGKEY         SETLL     PCG01                              60
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
B01  C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
E01  C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
B01  C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
B02  C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
E02  C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
B02  C     *INKL         IFEQ      '1'
     C*
B03  C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
E03  C                   END
     C*
E02  C                   END                                                    END *INKL
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N10
B02  CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
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
E02  C                   END                                                    END *IN10
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
B02  C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
E02  C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      STRT
E01  C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
B01  C     SCUCID        IFNE      *BLANKS
     C                   GOTO      TOP
E01  C                   END                                                    END SCUCID NE BLKS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C*
     C                   READC     SCR01S                                 50
B01  C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
B02  C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQUCID
     C                   MOVEL     SCCSID        QQUCID
     C                   MOVE      *BLANK        QQRFMT
     C                   MOVE      SCRFMT        QQRFMT
X02  C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
B03  C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
E03  C                   END                                                    END NE ' '
E02  C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
X01  C                   ELSE
     C                   GOTO      TERM
E01  C                   END                                                    END 0 *IN50
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
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
B01  C                   DO        13            X                 5 0
     C*
     C                   READ      PCG01                                  60
     C*
B02  C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
E02  C                   END
     C*
B02  C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C                   MOVEL     CGCSID        SCCSID
     C                   MOVEL     CGRFMT        SCRFMT
     C                   MOVEL     CGDESC        SCDESC
     C                   MOVEL     CGRCDS        SCRCDS
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
X02  C                   ELSE
     C                   MOVE      '1'           *IN05
E02  C                   END
E01  C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
B01  C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCCSID
     C                   MOVE      *BLANKS       SCRFMT
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCRCDS
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
E01  C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   MOVEL     *BLANK        SCUCID
     C*
     C                   ENDSR
