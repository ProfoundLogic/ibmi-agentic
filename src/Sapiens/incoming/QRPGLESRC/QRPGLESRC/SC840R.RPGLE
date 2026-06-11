     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *TITLE SC840R - OPTICAL PURGE/DELETE BATCH/BACKUP PROMPT PGM    *
      *****************************************************************
      *    PROGRAM DESCRIPTION:                                       *
      *        BACKUP AND BATCH - Optical Batch and Backup User       *
      *                           Options Prompt Program              *
      *          Allows user to decide on following:                  *
      *            1.)  Type of batch job to run - Select and copy    *
      *                  or select, copy, and delete                  *
      *            2.)  Number of days documents should reside on     *
      *                  DASD                                         *
      *            3.)  Option to save all documents and files to     *
      *                  *SAVF before running batch job               *
      *            4.)  Option to save to backup copy to optical      *
      *                                                               *
      *  CALLED FROM: Menu                                            *
      *                                                               *
      *  CALLS TO: SC840C                                             *
      *                                                               *
      *  CREATED: 03/31/03    PROGROGRAMMER: C:145    PROJECT: 124IAA *
      *****************************************************************
      ** REVISIONS:                                                   *
      *  CREATED:             PROGRAMMER:             PROJECT:        *
      *                                                               *
      ******************************************************************
      * INDICATOR USAGE                                                *
      *                                                                *
      ******************************************************************
      *
     FSC840F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
      *
      /Free
       scPrDs = 'Optical Purge/Delete';
      /End-free

     C     STRT          TAG
      *
     C                   EXFMT     SCR01
      *
      **********************************************************************
      *  Process all command keys
      *
     C     *IN11         IFEQ      '1'
      ********************************************
      *   F3 = EXIT                              *
      ********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  END *INKC
     C                   END                                                    END CMDKEYS
      *****************************************************************
      *  Process screen entries
      *
      *  Check to make sure user enters a '1' or leaves field blank
      *  with all fields except for number of retention days
      *
     C     SCBATS        IFNE      '1'
     C     SCBATS        ANDNE     *BLANK
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCBATD        IFNE      '1'
     C     SCBATD        ANDNE     *BLANK
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCBATB        IFNE      '1'
     C     SCBATB        ANDNE     *BLANK
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCBAK1        IFNE      '1'
     C     SCBAK1        ANDNE     *BLANK
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCBAK2        IFNE      '1'
     C     SCBAK2        ANDNE     *BLANK
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  End error checking for valid field values
      *
      *    Check to make sure only one backup option is selected
      *
     C     SCBAK1        IFEQ      '1'
     C     SCBAK2        ANDEQ     '1'
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Check to make sure the only combinations of batch mode
      *  are select and copy, or select, copy and delete.  Can not
      *  have both types selected.
      *
     C     SCBATS        IFEQ      '1'
     C     SCBATD        ANDEQ     '1'
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
      *
      *    Check to make sure batch type is not blank
      *
     C     SCBATS        IFEQ      *BLANKS
     C     SCBATD        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF
      *
      *    Check to make sure backup type is not blank
      *
     C     SCBAK1        IFEQ      *BLANKS
     C     SCBAK2        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  End backup option checking
      *  End processing all screen entries
      *****************************************************************
      *
     C                   MOVE      *BLANKS       QQBATS
     C                   MOVE      *BLANKS       QQBATD
     C                   MOVE      *BLANKS       QQBATB
     C                   MOVE      *BLANKS       QQBAK1
     C                   MOVE      *BLANKS       QQBAK2
     C                   MOVE      SCBATS        QQBATS
     C                   MOVE      SCBATD        QQBATD
     C                   MOVE      SCBATR        QQBATR
     C                   MOVE      SCBATB        QQBATB
     C                   MOVE      SCBAK1        QQBAK1
     C                   MOVE      SCBAK2        QQBAK2
      *
     C                   CALL      'SC840C'
     C                   PARM                    QQBATS            1
     C                   PARM                    QQBATD            1
     C                   PARM                    QQBATR            3
     C                   PARM                    QQBATB            1
     C                   PARM                    QQBAK1            1
     C                   PARM                    QQBAK2            1
      *  End the program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
