     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC901R - DARTS CASE TRANSFER
     H*  Program Description: This allows the user to change the key
     H*                       associated with a remark in the event of
     H*                       a case transfer.  (All remarks for a
     H*                       certain case need to be transferred to
     H*                       the new case number.
     H*****************************************************************
     H* REVISIONS
     H*****************************************************************
     H* INDICATOR USAGE
     H*   20    SCM0503-VALUE CANNOT BE EQUAL TO BLANKS
     H*   21    SCM0608-INVALID KEY ENTERED
     H*   22    SCM0503-VALUE CANNOT BE EQUAL TO BLANKS
     H*****************************************************************
     H* FILE SPECIFICATIONS  * * *
     H*****************************************************************
     FSC901F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     FSDCMLA    IF   E           K DISK
     F*

     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D* * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

     C*
     C*   KEY LIST FOR THE COMMENT(REMARK) DETAIL FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    SCFRKY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTC             1
     C*
      /Free
       scPrDs = 'DARTS Case Transfer';
      /End-Free
     C                   TIME                    DSDTTM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
      *------------------------*
      *** DISPLAY POINT
      *------------------------*
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
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
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C*
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N60
     CAN 60              MOVE      '0'           *IN10                            TEMP * STUFF
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
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*
      *****************************************************************
      *   GOTO THE APPROPRIATE SCREEN
      *****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C*   Edit the FROM key field to make sure it exists
     C*
     C     SCFRKY        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   TO key cannot be equal to BLANKS
     C*
     C     SCTOKY        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   All edits have passed, call the submit program
     C*
     C                   MOVE      *BLANKS       QQQUE            10
     C                   MOVE      *BLANKS       QQMODE            1
     C                   MOVE      *BLANKS       QQRTC
     C*
     C                   CALL      'SC901C'
     C                   PARM                    SCFRKY
     C                   PARM                    SCTOKY
     C                   PARM                    QQQUE
     C                   PARM                    QQMODE
     C                   PARM                    QQRTC
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
