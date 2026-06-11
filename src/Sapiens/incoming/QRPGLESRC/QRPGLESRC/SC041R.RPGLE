     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC040R - DEFAULT FOLDER MAINTENANCE
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09       INQUIRY MODE                                         *
     H*  20       SCM0503-FOLDER REQUIRED                              *
     H*  21       SCM0521-FOLDER DOES NOT EXIST                        *
     H*  22       SCM0517-NOT AUTHORIZED TO FOLDER                     *
     H*  23       SCM0XXX-REQUEST NOT ALLOWED WITH FOLDER              *
     H*  24       SCM0XXX-THERE ARE NO FOLDERS THAT YOU HAVE AUTHORIZA-*
     H*                   TION TO.                                     *
     H*  40       CHAIN TO SMSLL                                       *
     H******************************************************************
     FSC041F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)

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
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKEY         KLIST
     C                   KFLD                    XXUSER
     C*
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFOLD           80
     C*
     C                   MOVE      QQFOLD        SCFOLD           80
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
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
     C****************************************
     C*  F5 = WORK WITH FOLDERS             *
     C****************************************
     C     *INKE         IFEQ      '1'
     C***********
     C*  SCR01  *  WORK WITH FOLDERS
     C***********
     C     Q2RECF        IFEQ      'SCR01'
     C*
      * * * WRKFLR SCREEN
     C*
     C                   MOVEL     SCFOLD        XXFOLD           80            DEFAULT FOLDER
     C                   MOVEL     *BLANK        QQRTCD            1            ERR RTN CODE
     C*
     C                   CALL      'SWRKFLR'                                    "WRKFLR" OFC API
     C                   PARM                    XXFOLD                         DFLT FOLDER
     C                   PARM                    QQRTCD                         ERR ID CODE
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
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
     C*
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** DEFAULT FOLDER REQUIRED
     C     SCFOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** VALIDATE FOLDER:
     C                   MOVE      ' '           QQRTCD            1
     C                   CALL      'SVALFLR'
     C                   PARM                    SCFOLD
     C                   PARM                    QQRTCD
     C*** THAT IT EXISTS:
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*** THAT USER HAS AUTHORITY TO IT
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS RETURN SELECTED FOLDER PATH
     C*
     C                   MOVEL     SCFOLD        QQFOLD
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
