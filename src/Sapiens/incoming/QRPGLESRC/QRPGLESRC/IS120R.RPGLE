     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS120R - INDEX QUEUE SELECT                              *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON OPT FLAG FIELDS                                     *
     H*  20    INVALID OPTION ENTERED                                 *
     H*  21    INVALID INDEX QUEUE                                    *
     H*  22    INVALID OPTION ENTERED                                 *
     H*  32    ROLL UP INDICATOR                                      *
     H*  40    GENERAL CHAIN                                          *
     H*  50    READ ON SCR01                                          *
     H*  60    READ ON PDT01                                          *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  05/09/03 - C:087  Project: support
     H*             Change pgm to use the DARTS security file, SMSLP,
     H*             to condition records
     H*
     H*  04/18/01 - C:117  PROJECT 114IBZ
     H*             CONDITION THE RECORDS THAT WILL BE DISPLAYED
     H*             AGAINST THE USER'S SECURITY LEVEL
     H*
     H*****************************************************************
     FIS120F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FIMIQL     IF   E           K DISK
     FIMIQL1    IF   E           K DISK
     F                                     RENAME(PIQ01:PIQ02)
     F*** BEG ADD *** 04/18/01 ****************************************
     F***  begin chg  ***  05/09/03  **********************************
     F***SMUPL   IF  E           K        DISK
     FSMSLL2    IF   E           K DISK
     F***  end   chg  ***  05/09/03  **********************************
     F*** END ADD *** 04/18/01 ****************************************

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
     C*   KEY LIST FOR THE DOCUMENT TYPE  FILE
     C*
     C     IQKY          KLIST
     C                   KFLD                    SCID
     C*** BEG ADD *** 04/18/01 ****************************************
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C***  begin chg  ***  05/09/03  **********************************
     C********** UPKY      KLIST
     C     SLKY          KLIST
     C***  end   chg  ***  05/09/03  **********************************
     C                   KFLD                    Q1USER
     C*** END ADD *** 04/18/01 ****************************************
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQID             10
     C                   PARM                    QQDESC           40
     C                   PARM                    QQRTCD            1
     C*
      /Free
       scPrDs = 'Select Index Queue';
      /End-Free
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    RRN2          SRRN2
     C*
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
     C     IQKY          SETLL     PIQ01                              60
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
     C                   MOVEL     *BLANK        SCPDCT
     C*
     C                   MOVEL     Q2FILE        SCFILE           10            SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF            8             FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
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
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = PREVIOUS                     *
     C****************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C***********************************
     C     SCR01T        TAG
     C***********************************
     C*
     C*** IF THE SCPSEL IS SELECTED, THE SCPDCT MUST NOT BE BLANKS
     C*** , MUST BE VALID, THEN AUTO SELECT THIS ONE!
     C     SCPSEL        IFNE      *BLANKS
     C     SCPSEL        CABNE     '1'           STRT                 2222
     C                   ENDIF
     C*** EDIT THE SCPDCT FIELD TO SEE IF IT'S VALID, THEN SELECT IT
     C     SCPSEL        IFNE      *BLANKS
     C     SCPDCT        CHAIN     PIQ02                              67
     C     *IN67         CABEQ     *ON           STRT                     21
     C                   MOVEL     IQID          QQID
     C                   MOVEL     IQDESC        QQDESC
     C                   MOVE      *BLANKS       QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C***** IF POSITION FIELDS ENTERED, PERFORM RE POSITION
     C     SCPDCT        IFNE      *BLANKS
     C*
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C*
     C     SCPDCT        SETLL     PIQ01
     C                   EXSR      LOAD
     C*
     C                   Z-ADD     1             #@$2
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C**
     C* * * EDIT FIELDS
     C                   MOVE      '0'           ERR
     C*
     C*** SAVE RRN SO THAT ROLLING CAN PROPERLY FUNCTION IF COMPLETION IS MARKED
     C                   MOVE      RRN2          SRRN2
     C*
     C                   READC     SCR01S                                 50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*** PROCESS 1=SELECT DOCUMENTS
     C     SCSEL         IFEQ      '1'
     C                   MOVEA     '000'         *IN(20)
     C*
     C*
     C                   MOVEL     SCID          QQID
     C                   MOVEL     SCDESC        QQDESC
     C                   MOVEL     ' '           QQRTCD
     C                   GOTO      TERM
     C*
     C*
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C*
     C                   END                                                    END SEL EQ '5'
     C*
     C*
     C*
     C                   MOVEA     '000'         *IN(20)
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR               1
     C                   UPDATE    SCR01S
     C                   END
     C                   END
     C*
     C     NXT1          TAG
     C*
     C                   READC     SCR01S                                 50
     C                   END
     C*
     C*** RESTORE RRN SO THAT ROLLING CAN PROPERLY FUNCTION
     C                   MOVE      SRRN2         RRN2
     C*
     C     ERR           CABEQ     '1'           STRT
     C*
     C*
     C                   GOTO      STRT
     C*
     C*** END OF PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF           10
     C                   MOVEL     'SCR01'       Q2RECF
     C*                                                            TOGGLE
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN20
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      PIQ01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C*** BEG ADD *** 04/18/01 ****************************************
     C***  begin chg  ***  05/09/03  **********************************
     C********** UPKY      CHAINPUP01                59
     C     SLKY          CHAIN     PSL01                              59
     C***  end   chg  ***  05/09/03  **********************************
     C                   MOVE      IQSECV        XXSECV            3 0
     C*** END ADD *** 04/18/01 ****************************************
     C     *IN60         IFEQ      '0'
     C*** BEG ADD *** 04/18/01 ****************************************
     C     *IN59         ANDEQ     '0'
     C***  begin chg  ***  05/09/03  **********************************
     C********** UPLVL     ANDGEXXSECV
     C     SLSECL        ANDGE     XXSECV
     C***  end   chg  ***  05/09/03  **********************************
     C*** END ADD *** 04/18/01 ****************************************
     C                   ADD       1             RRN2
     C                   MOVEL     IQID          SCID
     C                   MOVEL     IQDESC        SCDESC
     C                   MOVEL     IQFL01        SCFL01
     C                   MOVEL     IQFL02        SCFL02
     C                   MOVEL     IQFL03        SCFL03
     C                   MOVEL     *BLANKS       SCSEL
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C*** 02 01/23/03 ADD *****************************
     C     *IN60         IFEQ      *OFF
     C     *IN59         ANDEQ     *OFF
     C     X             ANDGE     1
     C     X             SUB       1             X
     C                   ENDIF
     C*** 02 01/23/03 END ADD *************************
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCID
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCFL01
     C                   MOVE      *BLANKS       SCFL02
     C                   MOVE      *BLANKS       SCFL03
     C*
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   ENDSR
