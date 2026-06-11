     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC855R - OPTICAL PURGE/DELETE CPY TO OPTICAL
     H*****************************************************************
      ****************************************************************
      **   PROGRAM DESCRIPTION:                                      **
      **       PROMPT                                                **
      **   CALLED FROM: Menu                                         **
      **                                                             **
      *  CALLS TO:                                                    *
      *   SC855C                                                      *
      **   PROGRAMMER: C:12                                          **
      **   CREATED:    05/17/93     PROJECT: OP2                     **
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H******************************************************************
     H******************************************************************
     FSC855F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMOCL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*  OPTICAL CONTROL FILE
     C*
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANKS       UPOUTQ
     C                   END
      *
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C*
     C                   MOVE      'G'           QQOSYS            1
     C                   MOVEL     'OPTMLB01'    QQLIBN           10
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
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
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
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
     C*
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
      *
     C*   NO SCREEN EDITS TO PRCESS
     C* Submit
     C***** MAKE OPTICAL CONTROL SAY IN PROGRESS ***
     C     OCKY          CHAIN     POC01                              62
     C     *IN62         CABEQ     '1'           TERM
     C                   MOVE      'Y'           OCPPFL
     C                   UPDATE    POC01
     C*
     C                   CALL      'SC855C'
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
