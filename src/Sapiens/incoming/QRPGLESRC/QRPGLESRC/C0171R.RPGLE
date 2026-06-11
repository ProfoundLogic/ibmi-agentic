     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0171R - GLOBAL SEARCH DRIVER DATABASE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program searches the Driver Master file for all   **
     H**     hits on presented driver license number. Duplicate     **
     H**     occurances are displayed.                              **
     H**   CALLED FROM:                                             **
     H**     C0170R - Driver Master Maintenance                     **
     H**   CALLS TO:                                                **
     H**     M0006C - Help Routine                                  **
     H**   PROGRAMMER: 31                                           **
     H**   CREATED: 02/01/95         PROJECT: 67CBO                 **
     H****************************************************************
     H*
     H****************************************************************
     H**   REVISIONS                                                **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**                                                            **
     H**      10       Screen Help key                              **
     H**      11       Screen Valid command key                     **
     H**      21       Another screen is needed                     **
     H**      50       General usage                                **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H** FILES:                                                     **
     H**    CMUPL   : User Parameter File (USR)                     **
     H**    CMDRL1  : Driver Master File (LIC#)                     **
     H**    SMCOL   : System Company Master File (CO#)              **
     H****************************************************************
     H*
     FC0171F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMDRL1    IF   E           K DISK

     D*
     D LNC             S              6    DIM(5)                               DRIVER DOB
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
     D*
     D* -----------------------------------------------------
     D*        DRIVER LINE 1 WORK AREA
     D* -----------------------------------------------------
     D                 DS
     D  SCLA                   1    385
     D                                     DIM(5)                               DRIVER LINE 1
     D  SCLA01                 1     77
     D  SCLA02                78    154
     D  SCLA03               155    231
     D  SCLA04               232    308
     D  SCLA05               309    385
     D*
     D                 DS
     D  LNA                    1     77
     D  LNALN                  1     30
     D  LNALNX                31     32
     D  LNAFN                 33     47
     D  LNAFNX                48     52
     D  LNAMI                 53     53
     D  LNAMIX                54     58
     D  LNAPN                 59     71
     D  LNAPNX                72     76
     D  LNAST                 77     77
     D*
     D* -----------------------------------------------------
     D*        DRIVER LINE 2 WORK AREA
     D* -----------------------------------------------------
     D                 DS
     D  SCLB                   1    345
     D                                     DIM(5)                               DRIVER LINE 2
     D  SCLB01                 1     69
     D  SCLB02                70    138
     D  SCLB03               139    207
     D  SCLB04               208    276
     D  SCLB05               277    345
      /EJECT

     C* -----------------------------------------------------
     C*        ENTRY PARM LIST
     C* -----------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQLIC#           14
     C                   PARM                    QQLSN            30
     C                   PARM                    QQFSN            15
     C                   PARM                    QQMID             1
     C                   PARM                    QQDOB             6
     C                   PARM                    QQRTCD            1
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
     C*          PUP01 - USER PARAMETER
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*          COMPANY FILE
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*          DRIVER MASTER FILE
     C     DRKEY         KLIST
     C                   KFLD                    SCLIC#
      /EJECT
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          M A I N L I N E
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C                   EXSR      SR01                                         1ST PASS
     C*
     C     *INLR         IFEQ      *OFF
     C     *INLR         DOWEQ     *OFF
     C*
     C     Q2RECF        CASEQ     'SCR01'       SR02                           SCREEN 1
     C                   ENDCS
     C*
     C                   ENDDO
     C                   END
      /EJECT
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          S U B R O U T I N E S
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C*---------------------------------------------------------------
     C*  SR01    INITIALIZATION SUBROUTINE
     C*---------------------------------------------------------------
     C*
     C*          GET COMPANY # FROM USER PARAMETER FILE
     C*          -----------------------------------------------------
     C     SR01          BEGSR
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*          GET COMPANY NAME FROM COMPANY RECORD
     C*          -----------------------------------------------------
     C*
     C     COKEY         CHAIN     PCO01                              50
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*          MOVE SYSTEM VALUES TO SCREEN FIELD
     C*          -----------------------------------------------------
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*          RESET PARAMETERS
     C*          -----------------------------------------------------
     C                   MOVE      QQLIC#        SCLIC#
     C                   MOVE      *BLANKS       QQLIC#           14
     C                   MOVE      *BLANKS       QQLSN            30
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQDOB             6
     C                   MOVE      *BLANK        QQRTCD            1
     C*
     C*          DETERMINE IF LICENSE IS ALREADY ON FILE
     C*          - IF NO : RETURN TO SENDER
     C*          - IF YES: FILL THE SCREEN
     C*          -----------------------------------------------------
     C     DRKEY         CHAIN     PDR01                              50
     C     *IN50         IFEQ      *ON
     C                   SETON                                        LR
     C                   GOTO      SR01X
     C                   ELSE
     C                   EXSR      SR12
     C                   END
     C*
     C*          MOVE IN VALUES FOR FIRST PASS
     C*          -----------------------------------------------------
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*          END OF SR01
     C*          -----------------------------------------------------
     C     SR01X         ENDSR
     C*
     C*---------------------------------------------------------------
     C*  SR02    PROCESS SCREEN 1
     C*---------------------------------------------------------------
     C*
     C*          WRITE/READ SCREEN 1
     C*          -----------------------------------------------------
     C     SR02          BEGSR
     C                   EXFMT     SCR01
     C*
     C*          CHECK FUNCTION KEYS
     C*          IF FUNCTION KEY STILL ON - SKIP TO END
     C*          -----------------------------------------------------
     C     *IN11         IFEQ      *ON
     C                   EXSR      SR11
     C                   GOTO      SR02X
     C                   END
     C*
     C*          CHECK IF CURSOR ON VALID LINE
     C*          -----------------------------------------------------
     C                   Z-ADD     0             I1                3 0
     C     Q2CPOS        DIV       256           LIN               2 0
     C*
     C     LIN           IFEQ      8
     C     LIN           OREQ      9
     C                   Z-ADD     1             I1
     C                   END
     C*
     C     LIN           IFEQ      11
     C     LIN           OREQ      12
     C                   Z-ADD     2             I1
     C                   END
     C*
     C     LIN           IFEQ      14
     C     LIN           OREQ      15
     C                   Z-ADD     3             I1
     C                   END
     C*
     C     LIN           IFEQ      17
     C     LIN           OREQ      18
     C                   Z-ADD     4             I1
     C                   END
     C*
     C     LIN           IFEQ      20
     C     LIN           OREQ      21
     C                   Z-ADD     5             I1
     C                   END
     C*
     C*          CURSOR ON VALID LINE - PASS DATA BACK
     C*          -----------------------------------------------------
     C     I1            IFGE      1
     C                   MOVE      SCLA(I1)      LNA
     C                   MOVE      SCLIC#        QQLIC#
     C                   MOVE      LNALN         QQLSN
     C                   MOVE      LNAFN         QQFSN
     C                   MOVE      LNAMI         QQMID
     C                   MOVE      LNC(I1)       QQDOB
     C                   MOVE      '*'           QQRTCD
     C                   END
     C*
     C*          SET ON LAST RECORD
     C*          -----------------------------------------------------
     C                   SETON                                        LR
     C*
     C*          END OF SR02
     C*          -----------------------------------------------------
     C     SR02X         ENDSR
     C*
     C*---------------------------------------------------------------
     C*  SR11    PROCESS FUNCTION KEYS
     C*---------------------------------------------------------------
     C*
     C*         FUNCTION 02 = RETURN TO SCREEN 1
     C*          -----------------------------------------------------
     C     SR11          BEGSR
     C     *INKL         IFEQ      *ON
     C                   MOVE      'B'           QQRTCD
     C                   SETON                                        LR
     C                   GOTO      SR11X
     C                   END
     C*
     C*          HELP REQUEST
     C*          -----------------------------------------------------
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
     C                   GOTO      SR11X
     C                   END
     C*
     C*          ROLL UP - SCREEN 1 AND MORE DATA
     C*          -----------------------------------------------------
     C     *IN18         IFEQ      *ON
     C     SCROLL        ANDEQ     'MORE ...'
     C                   EXSR      SR12
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C*
     C*          ROLL DOWN - SCREEN 1
     C*          -----------------------------------------------------
     C     *IN19         IFEQ      *ON
     C     CURRCD        ANDGT     5
     C*
     C*                    DETERMINE NUMBER OF RECORDS TO BACKSPACE
     C*                    -------------------------------------------
     C     SCROLL        IFEQ      'MORE ...'
     C                   Z-ADD     10            IBACK             5 0
     C                   SUB       IBACK         CURRCD
     C                   END
     C     SCROLL        IFEQ      'BOTTOM  '
     C     CURRCD        DIV       5             IWORK             5 0
     C                   MVR                     IBACK
     C                   ADD       5             IBACK
     C                   SUB       IBACK         CURRCD
     C     DRKEY         SETGT     PDR01                              50
     C                   END
     C*
     C*                    REPOSITION FILE
     C*                    -------------------------------------------
     C                   Z-ADD     1             I
     C     I             DOUGT     IBACK
     C     DRKEY         READPE    PDR01                                  50
     C                   ADD       1             I
     C                   ENDDO
     C*
     C*                    RESTART AT NEW LOCATION
     C*                    -------------------------------------------
     C                   SETON                                        21
     C                   EXSR      SR12
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C*
     C*          END OF SUBROUTINE
     C*          -----------------------------------------------------
     C     SR11X         ENDSR
     C*
     C*---------------------------------------------------------------
     C*  SR12    FILL SCREEN ROUTINE
     C*---------------------------------------------------------------
     C*
     C*          INITIALIZE FIELDS
     C*          -----------------------------------------------------
     C     SR12          BEGSR
     C     *IN21         IFEQ      *OFF
     C     DRKEY         CHAIN     PDR01                              50
     C                   ELSE
     C                   SETOFF                                       2150
     C                   END
     C                   MOVEL     'BOTTOM  '    SCROLL            8
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     1             I                 3 0
     C                   CLEAR                   SCLA
     C                   CLEAR                   SCLB
     C                   CLEAR                   LNC
     C*
     C*          LOOP TO FILL SCREEN
     C*          -----------------------------------------------------
     C     *IN50         DOUEQ     *ON
     C     *IN21         OREQ      *ON
     C                   CLEAR                   LNA
     C*
     C*                    ONLY 6 LINES ON THE SCREEN
     C*                    -------------------------------------------
     C     I             IFLT      6
     C                   MOVE      DRLSN         LNALN
     C                   MOVE      DRFSN         LNAFN
     C                   MOVE      DRMID         LNAMI
     C                   MOVE      DRSTAT        LNAST
     C                   MOVE      DRPOL#        LNAPN
     C                   MOVE      LNA           SCLA(I)
     C     DRCOMM        IFNE      *BLANKS
     C                   MOVEL     'COMMENT:'    SCLB(I)
     C                   MOVE      DRCOMM        SCLB(I)
     C                   END
     C                   MOVE      DRDOB         LNC(I)
     C                   ADD       1             CURRCD            5 0
     C                   ADD       1             I
     C                   ELSE
     C                   MOVEL     'MORE ...'    SCROLL
     C                   SETON                                        21
     C                   END
     C*
     C*                    BOTTOM OF READ LOOP
     C*                    -------------------------------------------
     C     *IN21         IFEQ      *OFF
     C     DRKEY         READE     PDR01                                  50
     C                   END
     C                   ENDDO
     C                   END
     C     SR12X         ENDSR
