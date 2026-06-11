     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*
     H*
     H/TITLE ISDS69R - Direct Scan - Prompt
     H*
     H*
     H*****************************************************************
     H* REVISIONS:
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
      * 02/10/03 c:87  Proj: DR180
      *                Default batch ID description to "Policy
      *                Document".  Changed screen prompt "Document
      *                Type" to "Default Doc Type".
     H*
     H*****************************************************************
     H**INDICATOR USAGE
     H* ALL/DIFFERENT SCREENS AND FUNCTIONS:
     H*  60      GENERAL USE INDICATOR                                      PGM
     H*
     H*****************************************************************
     H*
     H/EJECT
     F*****************************************************************
     F* FILE SPECIFICATIONS  * * *
     F*****************************************************************
     F*
     FISDS69F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMDTL     IF   E           K DISK
     F/EJECT

     D*****************************************************************
     D* EXTENTION SPECIFICATIONS * * *
     D*****************************************************************
     D*
     D/EJECT
     D*****************************************************************
     D* INPUT SPECIFICATIONS  * * *
     D*****************************************************************
     D*
     D* SIS System wide parms data structure
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMMDD                 7     10  0
     D  DSYY                  11     12  0
     D  DSDATE                 7     12  0
     D*
     D*
     D* * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D* * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D***  begin add  ***  02/10/03  **********************************
     D ZZBDSC          C                   CONST('Non-SIS Document')

     I***  end   add  ***  02/10/03  **********************************
     I*
     I/EJECT

     C*****************************************************************
     C* CALCULATION SPECIFICATIONS  * * *
     C*****************************************************************
     C*
     C*** DEFINE FEILDS
     C*          *LIKE     DEFN AAAAAA    BBBBBB
     C*
     C/EJECT
     C*** KEY LISTS
     C*
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
     C*
     C/EJECT
     C*****************************************************************
     C*  MAINLINE CODE  *
     C*****************************************************************
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
     C*
      *** Begin Add 12/23/03 ******************************************
     C                   MOVE      DSDATE        SCCDAT
      *** End   Add 12/23/03 ******************************************
     C                   MOVE      *ZEROS        SCDLIM
     C                   MOVE      *ZEROS        SC#DCS
     C                   MOVE      *ZEROS        SC#PGS
     C                   MOVE      *BLANKS       SCDOCT
     C*
     C***  begin add  ***  02/10/03  **********************************
     C     SCBDSC        IFEQ      *BLANKS
     C                   MOVEL     ZZBDSC        SCBDSC
     C                   ENDIF
     C***  end   add  ***  02/10/03  **********************************
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
     C*
     C* Validate policy key
     C*
     C                   MOVEL     *BLANK        QQRTCD            1
     C                   MOVEL     SCNSRF        QQKEY
     C*
     C                   CALL      'ISIV69R'
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD
     C*
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
     C***DEL 7/8/05 ***    MOVELSRDTO     SCCDAT
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
     C                   MOVEL     DTDESC        QQDTDS
     C                   MOVEL     DTBSTS        QQBSTS
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
