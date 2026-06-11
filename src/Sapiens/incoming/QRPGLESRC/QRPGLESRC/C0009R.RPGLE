     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0009R - PROMPT FOR BINDER
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program submits the print binder job to batch.    **
     H**   CALLED FROM:                                             **
     H**     MENU                                                   **
     H**   CALLS TO:                                                **
     H**     NONE                                                   **
     H**   PROGRAMMER: C:31                                         **
     H**   CREATED: 08/23/95         PROJECT: 67CDB                 **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      10       Screen help key                              **
     H**      11       Screen command key                           **
     H**      20       Screen error - Invalid date                  **
     H**      50       General usage                                **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**    CDF2L (PF201): System Company Auxiliary Master File(CO#)**
     H**    CMUPL (PUP01): User Parameter File (USR)                **
     H****************************************************************
     H*
     FC0009F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FCDF2L     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK

     D*
     D* -----------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* -----------------------------------------------------
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* -----------------------------------------------------
     D*        TIME AND DATE
     D* -----------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* -----------------------------------------------------
     D*        HELP REQUEST WORK AREA
     D* -----------------------------------------------------
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      /EJECT

     C* -----------------------------------------------------
     C*        ENTRY PARM LIST
     C* -----------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*
     C* -----------------------------------------------------
     C*        KEY LISTS
     C* -----------------------------------------------------
     C*
     C*          COMPANY FILE
     C*          --------------------------------------------
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*          USER PARAMETER FILE
     C*          --------------------------------------------
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*          ------------------------------------------------------
     C*                    M A I N L I N E
     C*          ------------------------------------------------------
     C*
     C*          GET THE USER PARAMETER FILE
     C*          ------------------------------------------------------
     C     UPKY          CHAIN     PUP01                              50
     C     *IN50         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*          GET THE COMPANY RECORD
     C*          ------------------------------------------------------
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              50
     C     *IN50         CABEQ     *ON           TERM
     C     COKY          CHAIN     PF201                              50
     C     *IN50         CABEQ     *ON           TERM
     C*
     C                   EXCEPT    PF201E
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   Z-ADD     F2PRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
     C*
     C*          SET THE TIME AND DATE, LOAD INFORMATION
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C*
     C*          LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*          ------------------------------------------------------
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*          DISPLAY ALL FORMATS
     C*          ------------------------------------------------------
     C     STRT          TAG
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*          PROCESS ALL FUNCTION KEYS
     C*          ------------------------------------------------------
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*         FUNCTION 01 = END OF JOB
     C*          ------------------------------------------------------
     C     *INKC         IFEQ      '1'
     C*                    MOVE '1'       *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*          PROCESS HELP REQUESTS
     C*          ------------------------------------------------------
     C*
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
     C                   END
     C*
     C*          PROCESS CHANGE OF USER PARAMETERS
     C*          ------------------------------------------------------
     C     *INKM         IFEQ      '1'
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*                    GET THE USER PARAMETER FILE
     C*                    --------------------------------------------
     C     UPKY          CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C                   END
     C*
     C*          PROCESS THE REQUEST SCREEN
     C*          ------------------------------------------------------
     C     SCR01T        TAG
     C*
     C*          VALIDATE DATE
     C*          ------------------------------------------------------
     C     SCPRDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*          UPDATE THE COMPANY FILE WITH PROCESS DATE
     C*          ------------------------------------------------------
     C     COKY          CHAIN     PF201                              50
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2PRDT
     C                   UPDATE    PF201
     C                   END
     C*
     C*          END THE PROGRAM
     C*          ------------------------------------------------------
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     O*
     O*          OUTPUT EXCEPTION SPECS
     O*          ------------------------------------------------------
     OPF201     E            PF201E
