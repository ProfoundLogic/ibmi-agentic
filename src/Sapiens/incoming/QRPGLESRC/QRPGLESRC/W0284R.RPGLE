     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0284R - EMPLOYER / CONTACT - SEARCH
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H*  08/17/99  C:118  PROJECT: WC577                              *
     H*                   Made V3 screen fixes.                       *
     H*                                                               *
     H*****************************************************************
     FW0284F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
     F*******************************************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*******************************************************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *

     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER           10
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQNAME           40
     C                   PARM                    QQMAD1           40
     C                   PARM                    QQMAD2           40
     C                   PARM                    QQMCTY           25
     C                   PARM                    QQMST             2
     C                   PARM                    QQMZIP            9
     C                   PARM                    QQCONT           40
     C                   PARM                    QQEFND            3
     C                   PARM                    QQEMP#            5
     C                   PARM                    QQDIV             3
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C**************************************************************
     C* DEFINITIONS:
     C     *LIKE         DEFINE    S2EMP#        SVEMP#
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C                   MOVE      Q1USER        XXUSER
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
     C                   ELSE
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPCO#         XXCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVEL     QQKEY         XXEMP#
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   EXSR      INIT
     C*
     C     TOP           TAG
     C     EMKEY         SETLL     PEM01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD
     C*
     C     STRT          TAG
     C*
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
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = EXIT                         *
     C****************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   Z-ADD     POS2          RRN2
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C******************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     S2EMP#        IFNE      SVEMP#
     C                   MOVE      S2EMP#        SVEMP#
     C                   MOVE      *BLANKS       XXEMP#
     C                   MOVEL     S2EMP#        XXEMP#
     C                   Z-ADD     0             RRN2
     C                   GOTO      TOP
     C                   END
     C*
     C     TAGRC         TAG
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C     SCSEL         IFEQ      '1'
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVE      EMNAME        QQNAME
     C                   MOVE      EMMAD1        QQMAD1
     C                   MOVE      EMMAD2        QQMAD2
     C                   MOVE      EMMCTY        QQMCTY
     C                   MOVE      EMMST         QQMST
     C                   MOVE      EMMZIP        QQMZIP
     C                   MOVE      EMCONT        QQCONT
     C                   MOVE      EMFND         QQEFND
     C                   MOVE      EMEMP#        QQEMP#
     C                   MOVE      EMDIV         QQDIV
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C                   END
     C                   GOTO      STRT
     C*
     C***
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Name
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              61
      *
     C     *IN61         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             SCRRN
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
     C                   READ      PEM01                                  60
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C     X             IFEQ      1
     C                   Z-ADD     RRN2          POS2              5 0
     C                   END
     C                   MOVE      EMCO#         SCCO#
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVEL     EMNAME        SCNAME
     C                   MOVEL     EMCONT        SCCONT
     C                   MOVE      *BLANKS       SCSEL
     C                   Z-ADD     RRN2          SCRRN
     C                   MOVE      '0'           *IN06
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01S
     C                   MOVE      '0'           *IN01
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *ZEROS        SCCO#
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVE      *BLANKS       SCDIV
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCCONT
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             POS2
     C                   Z-ADD     1             SCRRN
     C                   MOVE      '1'           *IN06
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01S
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     POS2          #@$2
     C                   Z-ADD     RRN2          POS2
     C*
     C                   ENDSR
