     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC610R - Key Type Selection                              *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  07/26/13  C:277  SUPPORT: 82880
     H*                   CHANGE THE
     H*
     H*****************************************************************
     H* Indicator Usage                                               *
     H* SCR01:                                                        *
     H*  20    Invalid option used to indicate a selected record.     *
     H*  50    Indicator for subfile change.                          *
     H*****************************************************************
     FSC610F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FSMKTL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *LIKE         DEFINE    Q2RECF        SVSCRN
      *
      *   P List for the Delete Object Attachment Program
      *
     C     PGMLST        PLIST
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
      *
      *   P List for the Select Program
      *
     C     SELLST        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
      *
      *   Key List for the Key Type file
      *
     C     KTKY          KLIST
     C                   KFLD                    SCCODE
      *------------------------*
      *** Reset Point in Program
      *------------------------*
     C     TOP           TAG
     C*** Beg Add *** 07/26/13 ****************************************
      /free
         SCPRDS = 'ATTACHMENT DELETION';
      /End-Free
     C*** End Add *** 07/26/13 ****************************************
      *
      *   Initialize Routine * * *
     C                   EXSR      INIT
      *
     C     KTKY          SETLL     PKT01                              60
     C*
      *   Clear Subfile
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
      *
     C                   EXSR      LOAD                                         Load rcds
      *
     C     STRT          TAG
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
      *
      *******************************************************************
      *   PROCESS ALL COMMAND KEYS
      *******************************************************************
      *
     C     *IN11         IFEQ      '1'
      ****************************************
      *   F3 = EXIT                          *
      ****************************************
     C     *INKC         IFEQ      '1'
     C                   EXSR      ENDPGM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   EXSR      ENDPGM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
      *
     C                   GOTO      STRT
     C                   END                                                    *IN11
      **************************************************************************
      *   Check for user selection to reposition
      **************************************************************************
     C     SCCODE        IFNE      *BLANKS
     C                   GOTO      TOP
     C                   END                                                    END SCCODE NE BLKS
      *
      *  Check for search selection
      *
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
      *** If the subfile has changed and the user has selected a record,
      *** save that data to be passed back to the calling program
     C     SCSEL         IFEQ      '1'
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      SCSRC4        SELLST
      *
     C     QQRTCD        IFEQ      *BLANK                                       F3 in called
     C                   MOVEL     SCPNAM        QQPNAM
     C                   CALL      'SC611R'      PGMLST
     C                   MOVE      *BLANK        SCSEL
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'C'                                          F3 in called
     C                   EXSR      ENDPGM                                       Program
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'L'                                          F12 in called
     C                   MOVE      *BLANK        SCSEL                          Program
     C                   ENDIF
      *
     C                   ELSE                                                   ELSE EQ '1'
      *   if the subfile has changed but the user did not use a '1' to
      *   select a record, send an error message back to the screen.
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
      *   If the subfile has not been changed, return to the calling
      *   program without a selected record
     C                   ELSE
     C                   EXSR      ENDPGM
     C                   END                                                    END 0 *IN50
      *
      *===============================================================
      * * *   ENDPGM SUBROUTINE
      *===============================================================
     C     ENDPGM        BEGSR
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSR
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
     C                   DO        13            X                 5 0
     C*
     C                   READ      PKT01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C                   MOVEL     KTKTYP        SCKTYP
     C                   MOVEL     KTPNAM        SCPNAM
     C                   MOVEL     KTPDES        SCPDES
     C                   MOVEL     KTSTAT        SCSTAT
     C                   MOVEL     KTSRC4        SCSRC4
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANKS       SCKTYP
     C                   MOVEL     *BLANKS       SCPNAM
     C                   MOVEL     *BLANKS       SCPDES
     C                   MOVEL     *BLANKS       SCSTAT
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   MOVEL     *BLANK        SCCODE
     C*
     C                   ENDSR
