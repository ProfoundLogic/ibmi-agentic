     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS222R - DISPLAY BATCH DETAIL
     H******************************************************************
      * REVISIONS:
      *
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  40       CHAIN TO IMBML                                       *
     H******************************************************************
     FIS222F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMBML1    IF   E           K DISK
     FITBSL     IF   E           K DISK
     FIMDTL     IF   E           K DISK
      *
      *

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
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQRTCD            1
     C*
     C*
     C     BMKY          KLIST
     C                   KFLD                    QQBCHI
     C*
     C*
     C     BSKY          KLIST
     C                   KFLD                    BMBSTS
     C*
     C*
     C     DTKY          KLIST
     C                   KFLD                    BMDOCT
     C*
     C* INITIALIZATION SECTION
     C*
     C                   TIME                    DSDTTM
     C*
      /Free
       scPrDs = 'Batch Details';
      /End-Free
     C*
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     BMKY          CHAIN     PBM01                              40
     C     *IN40         IFEQ      '0'
     C*
     C*** THE BATCH MASTER FILE HERE ***
     C*** GET BATCH STATUS DESCRIPTION
     C*
     C     BSKY          CHAIN     PBS01                              41
     C     *IN41         IFEQ      *OFF
     C                   MOVEL     BSLDSC        SCSDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSDSC
     C                   ENDIF
     C*
     C*** GET DOC TYPE DESCRIPTION ***
     C*
     C     DTKY          CHAIN     PDT01                              41
     C     *IN41         IFEQ      *OFF
     C                   MOVEL     DTDESC        SCDDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCDDSC
     C                   ENDIF
     C*
     C*
     C                   MOVE      BMSDT         XXSDT             8
     C*
     C                   MOVEL     XXSDT         XXSY              4
     C                   MOVE      XXSDT         XXSD              2
     C     2             SUBST     XXSDT:5       XXSM              2
     C     XXSM          CAT       XXSD          XXMD              4
     C     XXMD          CAT       XXSY          XXSDT
     C*
     C                   MOVE      XXSDT         SCSDT
     C*
     C*
     C                   MOVE      BMSTM         SCSTM
     C*
     C*
     C                   ELSE
     C                   GOTO      TERM
     C*
     C                   END
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
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*  F7 = BATCH NOTES DISPLAY           *
     C****************************************
     C     *INKG         IFEQ      '1'
     C*
     C                   MOVEL     'IS222R  '    #7PNAM           10
     C                   CALL      'IS014R'
     C                   PARM                    QQBCHI
     C                   PARM                    #7PNAM
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'B'           QQRTCD
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
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C********************************************
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
     C*
     C*
     C                   GOTO      STRT                                         REDISPLAY
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
