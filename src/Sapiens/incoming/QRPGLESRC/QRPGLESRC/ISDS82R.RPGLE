     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *                                                               *
      *****************************************************************
      *
      *
     H/TITLE ISDS82R - Direct Scan Prompt - POLICY WITH YEAR (SUMPL1)
      *
      *
      *****************************************************************
      * REVISIONS:
      *
      * 07/08/05 C:002 PROJ: SUPP
      *                FIX DATE (SCCDAT) TO BE FORMATTED CORRECTLY (MDY) AFTER
      *                RETURN FROM DATCHK ROUTINE.
      *
      *
      * 12/23/03 c:128 Proj: 129IEW
      *                Added comment date to screen to allow dates
      *                other than today's date.
      *
      *****************************************************************
      **INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL USE INDICATOR                                      PGM
      *
      *****************************************************************
      *
     H/EJECT
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      *
     FISDS82F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMDTL     IF   E           K DISK
     F/EJECT
      *****************************************************************
      * EXTENTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D/EJECT
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMMDD                 7     10  0
     D  DSYY                  11     12  0
     D  DSDATE                 7     12  0
     D                 DS
     D  DRTKEY                 1     40
     D  YYDFT                  1      3
     D  YYCARR                 4      6
     D  YYCLNT                 7     11
     D  YYYEAR                12     14
     D  YYPAD2                15     40
     D                 DS
     D  @KEY                   1     14
     D  @DFT                   1      3
     D  @CARR                  4      6
     D  @CLNT                  7     11
     D  @YEAR                 12     14
      *
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I/EJECT
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*** DEFINE FEILDS
     C*          *LIKE     DEFN AAAAAA    BBBBBB
     C*
     C/EJECT
     C*** KEY LISTS
      *
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
      *
     C/EJECT
      *****************************************************************
      *  MAINLINE CODE  *
      *****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SCBDSC           40
     C                   PARM                    SCBCHI           12
     C                   PARM                    QQDLIM            3
     C                   PARM                    QQ#DCS            5
     C                   PARM                    QQ#PGS            5
     C                   PARM                    QQDOCT            8
     C                   PARM                    QQBSTS            2
     C                   PARM                    QQDTDS           40
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
      *** Begin Add 12/23/03 ******************************************
     C                   PARM                    QQCDAT            6            mmddyy(out)
     C** Date check/conversion parameters
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *** End   Add 12/23/03 ******************************************
     C*
     C                   TIME                    DSDTTM
      *
      *** Begin Add 12/23/03 ******************************************
     C                   MOVE      DSDATE        SCCDAT
      *** End   Add 12/23/03 ******************************************
     C                   MOVE      *ZEROS        SCDLIM
     C                   MOVE      *ZEROS        SC#DCS
     C                   MOVE      *ZEROS        SC#PGS
     C                   MOVE      *BLANKS       SCDOCT
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
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
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN07         IFEQ      '1'
     C********************************************
     C*   F3 = Exit                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C*
     C                   MOVEL     'C'           QQRTCD
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F5 = SUMMIT Claim number search        *
     C********************************************
     C     *INKE         IFEQ      '1'
     C                   MOVE      *BLANKS       @CLASS
     C                   CALL      'MBR215'
     C                   PARM      '8'           @CLASS            1
     C                   PARM      *BLANKS       ZCARR             4
     C                   PARM      *BLANKS       @CLNT             5
     C                   PARM      *BLANKS       @YEAR             3
     C                   MOVE      ZCARR         @CARR
      *
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C*** ERROR IF YEAR NOT RETURNED
     C     @YEAR         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C**** ERROR *********
     C                   ENDIF
     C*
     C                   MOVE      '001'         SCDFT
     C                   MOVE      @CARR         SCCARR
     C                   MOVE      *ALL'0'       SCCLNT
     C                   MOVE      @CLNT         SCCLNT
     C                   MOVE      @YEAR         SCYEAR
      *
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C********************************************
     C*   F10 = Document type search             *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       QQDOCT            8
     C                   MOVEL     *BLANKS       QQDESC           40
     C                   MOVEL     *BLANK        QQRTCD            1
     C*
     C                   CALL      'IS100R'
     C                   PARM                    QQDOCT
     C                   PARM                    QQDESC
     C                   PARM                    QQRTCD
     C     QQRTCD        IFNE      ' '
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     QQDOCT        SCDOCT
     C                   MOVEL     QQDESC        QQDTDS
     C                   GOTO      STRT
     C                   ENDIF
     C                   END                                                    END *INKJ
     C********************************************
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      *
      * Transform user entered key to a DARTS key
      *
     C                   MOVE      SCDFT         YYDFT
     C                   MOVE      SCCARR        YYCARR
     C                   MOVE      SCCLNT        YYCLNT
     C                   MOVE      SCYEAR        YYYEAR
     C                   MOVE      *BLANKS       YYPAD2
     C                   MOVE      *BLANK        YYRTCD            1
      *
      * Validate claim key
      *
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   MOVEL     DRTKEY        QQKEY
     C*
     C                   CALL      'ISIV82C'
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD
     C     QQRTCD        IFNE      ' '
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SC#DCS        IFLT      1
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C     SC#PGS        IFLT      1
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*****      SCBDSC    IFEQ *BLANKS
     C*                    MOVE *ON       *IN22
     C*                    GOTO STRT
     C*****                ENDIF
     C*
     C     SCDOCT        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
      *** Begin Add 12/23/03 ******************************************
     C*
     C*  Date entered must be valid.
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C***DEL 7/8/05        MOVELSRDTO     SCCDAT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN52
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      SCCDAT        QQCDAT
     C                   ENDIF
      *** End   Add 12/23/03 ******************************************
     C*
     C     DTKY          CHAIN     PDT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVEL     DTBSTS        QQBSTS
     C                   MOVEL     DTDESC        QQDTDS
     C                   MOVE      SCDLIM        QQDLIM
     C                   MOVE      SC#DCS        QQ#DCS
     C                   MOVE      SC#PGS        QQ#PGS
     C                   MOVE      SCDOCT        QQDOCT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
