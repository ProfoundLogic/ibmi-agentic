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
     H/TITLE IS600R - BATCH SCANNING - PROMPT
      *
      *
      *****************************************************************
      * REVISIONS:
      * XX/XX/XX NNN    - @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      *
     F* 07/09/07 C:191 PROJECT: 10IJI                                *
     F*               -CAPTURE RECEIVED DATE (SCRCVD) WHEN SYSTEM    *
     F*                CONTROL FLAG IS SET TO "Y".                   *
      *****************************************************************
      **INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL USE INDICATOR                                      PGM
      *  23      INVALID RECIEVED DATE                                     DSPF
      *  98      TO CONTROL DISPLAY OF RECEIVED DATE                       DSPF
      *****************************************************************
      *
     H/EJECT
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      *
     F*** BEG ADD ********** 07/09/07 ********************************
     F*** LOGICAL OF SDRPF
     FSSYLF     IF   E           K DISK
     F*** END ADD ********** 07/09/07 ********************************
     FIS600F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
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
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     C*
     *** BEG ADD ********** 07/09/07 ********************************
     * KEY LIST FOR THE SYSTEM CONTROL FILE
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     *** END ADD ********** 07/09/07 ********************************
     C*
     C/EJECT
      *****************************************************************
      *  MAINLINE CODE  *
      *****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SCDCTP            8
     C                   PARM                    SCDDSC           40
     C                   PARM                    SCBDSC           40
     C                   PARM                    SCBCHI           12
     C                   PARM                    QQDLIM            3
     C                   PARM                    QQ#DCS            5
     C                   PARM                    QQ#PGS            5
     C                   PARM                    QQRTCD            1
     *** BEG ADD ********** 07/09/07 ********************************
     C                   PARM                    QQRCVD            8
     *** END ADD ********** 07/09/07 ********************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQDLIM        SCDLIM
     C                   MOVE      QQ#DCS        SC#DCS
     C                   MOVE      *ZEROS        SC#PGS
     C                   MOVEL     SCDDSC        SCBDSC
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     *** BEG ADD ********** 07/09/07 ********************************
     C                   MOVEL     'SC'          XXSYK
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      *OFF
     C     DRFL15        IFEQ      'Y'
     C                   SETON                                        98
     C                   ENDIF                                                  END DRFL15
     C                   ENDIF                                                  END *IN60
     *** END ADD ********** 07/09/07 ********************************
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
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C*
     C                   MOVEL     'C'           QQRTCD
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
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
     C     SCBDSC        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     *** BEG ADD ********** 07/09/07 ********************************
     C     *IN98         IFEQ      *ON
     C     SCRCVD        IFEQ      0
     * DISPLAY RECEIVED DATE FIELD IN REVERSE ORDER AND AN ERROR MESSAGE
     * TO ENTER ANY VALID DATE.
     C                   SETON                                        23
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      SCRCVD        ##RCVD            8 0
     C                   CALL      'ISCVTDT'
     C                   PARM                    ##RCVD
     C                   PARM      *BLANKS       ##FFMT            8
     C                   PARM      '*YYMD'       ##TFMT            8
     C                   PARM                    ##DTO            10
     C                   PARM      *BLANKS       ##ERR             1
     C     ##ERR         IFEQ      '1'
     * DISPLAY RECEIVED DATE FIELD IN REVERSE ORDER AND AN ERROR MESSAGE
     * TO ENTER ANY VALID DATE.
     C                   SETON                                        23
     C                   GOTO      STRT
     C                   ENDIF                                                  END ##ERR
     C                   ENDIF                                                  END SRRCVD
     C                   ENDIF                                                  END *IN98
     *
     C*
     C                   MOVEL(P)  ##DTO         QQRCVD
     *** END ADD ********** 07/09/07 ********************************
     C                   MOVE      SCDLIM        QQDLIM
     C                   MOVE      SC#DCS        QQ#DCS
     C                   MOVE      SC#PGS        QQ#PGS
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
