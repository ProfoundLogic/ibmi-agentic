     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0065R - DAILY VEHICLE DETAILS
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program allows entry of a coverage date for       **
     H**     vehicles using the daily coverage option.              **
     H**   CALLED FROM:                                             **
     H**     NONE                                                   **
     H**   CALLS TO:                                                **
     H**     NONE                                                   **
     H**   PROGRAMMER: C:31                                         **
     H**   CREATED: 01/06/95         PROJECT: 67CBQ                 **
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
     H**      50       General Usage - Controls read loop           **
     H**      51       General Usage                                **
     H**      21       Another screen is needed                     **
     H**      22       Date error - redisplay screen SCR02          **
     H**      24       Error CCR0535 - Invalid policy number.       **
     H**      61       Error CCR0502 - Invalid date on line  1      **
     H**      62       Error CCR0502 - Invalid date on line  2      **
     H**      63       Error CCR0502 - Invalid date on line  3      **
     H**      64       Error CCR0502 - Invalid date on line  4      **
     H**      65       Error CCR0502 - Invalid date on line  5      **
     H**      66       Error CCR0502 - Invalid date on line  6      **
     H**      67       Error CCR0502 - Invalid date on line  7      **
     H**      68       Error CCR0502 - Invalid date on line  8      **
     H**      69       Error CCR0502 - Invalid date on line  9      **
     H**      70       Error CCR0502 - Invalid date on line 10      **
     H**      71       Error CCR0502 - Invalid date on line 11      **
     H**      72       Error CCR0502 - Invalid date on line 12      **
     H**      73       Error CCR0502 - Invalid date on line 13      **
     H**      74       Error CCR0502 - Invalid date on line 14      **
     H**      75       Error CCR0502 - Invalid date on line 15      **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**    SMCOL   (PC001): System Company Master (CO#)            **
     H**    CMUPL   (PUP01): User Parameter (USR)                   **
     H**    CMPML   (PPM01): Policy Master (CO#/POL#)               **
     H**    CMA1L4  (PA101): Auto Liability Cov Det (CO#/POL#/ITM#) **
     H**    CMVSL   (PVS01): Vehicle Schedule  (CO#/POL#/ITM#)      **
     H**    CMDUP   (PDU01): Daily Vehicle Detail                   **
     H****************************************************************
     FC0065F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMA1L4    IF   E           K DISK
     FCMVSL     IF   E           K DISK
     FCMDUP     O  A E           K DISK
     F*

     D ITM             S              5    DIM(15)                              ITEM NUMBERS
     D*
     D* -----------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* -----------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D* -----------------------------------------------------
     D* SYSTEM DATE AND TIME
     D* -----------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D* -----------------------------------------------------
     D* HELP REQUEST WORK AREA
     D* -----------------------------------------------------
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* -----------------------------------------------------
     D* VEHICLE DESCRIPTION LINES
     D* -----------------------------------------------------
     D                 DS
     D  LN                     1   1050
     D                                     DIM(15)                              VEHICLE DESC
     D  SCLN01                 1     68
     D  SCLN02                69    136
     D  SCLN03               137    204
     D  SCLN04               205    272
     D  SCLN05               273    340
     D  SCLN06               341    408
     D  SCLN07               409    476
     D  SCLN08               477    544
     D  SCLN09               545    612
     D  SCLN10               613    680
     D  SCLN11               681    748
     D  SCLN12               749    816
     D  SCLN13               817    884
     D  SCLN14               885    952
     D  SCLN15               953   1020
     D*
     D* -----------------------------------------------------
     D* COVERAGE DATE LINES
     D* -----------------------------------------------------
     D                 DS
     D  DT                     1    120  0
     D                                     DIM(15)                              COVERAGE DATE
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D  SCDT12               089    096  0
     D  SCDT13               097    104  0
     D  SCDT14               105    112  0
     D  SCDT15               113    120  0
     D*
     D* -----------------------------------------------------
     D* VEHICLE DESCRIPTION LINE CONSTRUCT AREA
     D* -----------------------------------------------------
     D                 DS
     D  LINE                   1     70
     D  LITM#                  2      6
     D  LCC                   11     12
     D  LYR                   20     21
     D  LMAKE                 25     36
     D  LMODEL                39     50
     D  LID                   53     69
      /EJECT

     C* -----------------------------------------------------
     C* PARAMETER LIST
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
     C* SEARCH KEYS
     C* -----------------------------------------------------
     C*          PA101 - AUTO LIABILITY COVERAGE DETAIL
     C     A1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*          PCO01 - COMPANY MASTER
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*          PPM01 - POLICY MASTER
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#             3 0
     C                   KFLD                    SCPOL#           13
     C*
     C*          PUP01 - USER PARAMETER
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*          PVS01 - VEHICLE SCHEDULE
     C     VSKEY         KLIST
     C                   KFLD                    A1CO#
     C                   KFLD                    A1POL#
     C                   KFLD                    A1ITM#
      /EJECT
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          M A I N L I N E
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C                   EXSR      SR01                                         1ST PASS
     C*
     C     *INLR         DOWEQ     *OFF
     C*
     C     Q2RECF        CASEQ     'SCR01'       SR02                           SCREEN 1
     C                   ENDCS
     C*
     C     Q2RECF        CASEQ     'SCR02'       SR03                           SCREEN 2
     C                   ENDCS
     C*
     C                   ENDDO
      /EJECT
     C*
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          S U B R O U T I N E S
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C*---------------------------------------------------------------
     C*  SR01    INITIAL1ZATION SUBROUTINE
     C*---------------------------------------------------------------
     C*
     C*          GET COMPANY AND USER RECORDS
     C*          -----------------------------------------------------
     C     SR01          BEGSR
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      *OFF
     C     COKEY         CHAIN     PCO01                              50
     C                   END
     C     *IN50         IFEQ      *ON
     C                   SETON                                        LR
     C                   GOTO      SR01X
     C                   END
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*          MOVE SYSTEM VALUES TO SCREEN FIELD
     C*          -----------------------------------------------------
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*          MOVE IN VALUES FOR FIRST PASS
     C*          -----------------------------------------------------
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
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
     C                   Z-ADD     0             CURRCD
     C                   SETOFF                                       21
     C*
     C*          CHECK FUNCTION KEYS
     C*          IF FUNCTION KEY STILL ON - SKIP TO END
     C*          -----------------------------------------------------
     C     *IN11         IFEQ      *ON
     C                   EXSR      SR11
     C                   END
     C     *IN11         IFEQ      *ON
     C                   GOTO      SR02X
     C                   END
     C*
     C*          IF POLICY NOT FOUND - SKIP TO END
     C*          -----------------------------------------------------
     C     PMKEY         CHAIN     PPM01                              24
     C     *IN24         CABEQ     *ON           SR02X
     C*
     C*          FILL FIRST SCREEN
     C*          -----------------------------------------------------
     C                   EXSR      SR12
     C*
     C*          COMPLETE SETTING UP SCREEN 2
     C*          -----------------------------------------------------
     C                   MOVEL     PMFIN         SCFIN
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C*          END OF SR02
     C*          -----------------------------------------------------
     C     SR02X         ENDSR
     C*
     C*---------------------------------------------------------------
     C*  SR03    PROCESS SCREEN 2
     C*---------------------------------------------------------------
     C*
     C*          WRITE/READ SCREEN 2
     C*          -----------------------------------------------------
     C     SR03          BEGSR
     C                   EXFMT     SCR02
     C*
     C*          CHECK FUNCTION KEYS
     C*          IF FUNCTION KEY STILL ON - SKIP TO END
     C*          -----------------------------------------------------
     C     *IN11         IFEQ      *ON
     C                   EXSR      SR11
     C                   END
     C     *IN11         IFEQ      *ON
     C                   GOTO      SR03X
     C                   END
     C*
     C*          CHECK DATES
     C*          -----------------------------------------------------
     C                   SETOFF                                       22
     C                   DO        15            I
     C     I             ADD       80            I2                3 0
     C     I             ADD       60            I3                3 0
     C     DT(I)         IFNE      *ZEROS
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     DT(I)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(I)
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN(I3)
     C                   MOVE      *ON           *IN22
     C                   END
     C                   END
     C                   ENDDO
     C*
     C     *IN22         CABEQ     *ON           SR03X
     C*
     C*          WRITE RECORDS WITH COVERAGE DATES
     C*          -----------------------------------------------------
     C                   DO        15            I
     C     DT(I)         IFNE      *ZEROS
     C                   MOVE      UPCO#         DUCO#
     C                   MOVE      SCPOL#        DUPOL#
     C                   MOVE      ITM(I)        DUITM#
     C                   Z-ADD     DT(I)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDATE
     C                   MOVE      Q1USER        DUUSER
     C                   MOVE      Q1PGM         DUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSTIME        PMTIME
     C                   WRITE     PDU01
     C                   END
     C                   ENDDO
     C*
     C*          IF NO MORE RECORDS - RETURN TO SCREEN 1
     C*          -----------------------------------------------------
     C     *IN21         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCPOL#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C*
     C*          END OF SR03
     C*          -----------------------------------------------------
     C     SR03X         ENDSR
     C*
     C*---------------------------------------------------------------
     C*  SR11    PROCESS FUNCTION KEYS
     C*---------------------------------------------------------------
     C*
     C*         FUNCTION 01 = END OF JOB
     C*          -----------------------------------------------------
     C     SR11          BEGSR
     C     *INKC         IFEQ      *ON
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      *ON           *INLR
     C                   GOTO      SR11X
     C                   END
     C*
     C*         FUNCTION 02 = RETURN TO SCREEN 1
     C*          -----------------------------------------------------
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCPOL#
     C                   GOTO      SR11X
     C                   END
     C*
     C*         FUNCTION 05 = POLICY NUMBER SEARCH
     C*          -----------------------------------------------------
     C     *INKE         IFEQ      '1'
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   SETOFF                                       11
     C                   GOTO      SR11X
     C                   END
     C                   END
     C*
     C*         FUNCTION 07 = SEARCH POLICY MASTER BY NAME
     C*          -----------------------------------------------------
     C     *INKG         IFEQ      '1'
     C                   MOVE      *BLANKS       QQPOL#
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   SETOFF                                       11
     C                   GOTO      SR11X
     C                   END
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
     C*         FUNCTION 13 - CHANGE USER PARAMETERS
     C*          -----------------------------------------------------
     C     *INKM         IFEQ      '1'
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C                   EXSR      SR01
     C                   GOTO      SR11X
     C                   END
     C*
     C*          ROLL UP - SCREEN 2 AND MORE DATA
     C*          -----------------------------------------------------
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C     *IN18         IFEQ      *ON
     C     SCROLL        ANDEQ     'MORE ...'
     C                   EXSR      SR12
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C*
     C*          ROLL DOWN - SCREEN 2
     C*          -----------------------------------------------------
     C     *IN19         IFEQ      *ON
     C     CURRCD        ANDGT     15
     C*
     C*                    DETERMINE NUMBER OF RECORDS TO BACKSPACE
     C*                    -------------------------------------------
     C     SCROLL        IFEQ      'MORE ...'
     C                   Z-ADD     30            IBACK             5 0
     C                   SUB       IBACK         CURRCD
     C                   END
     C     SCROLL        IFEQ      'BOTTOM  '
     C     CURRCD        DIV       15            IWORK             5 0
     C                   MVR                     IBACK
     C                   ADD       15            IBACK
     C                   SUB       IBACK         CURRCD
     C     A1KEY         SETGT     PA101                              50
     C                   END
     C*
     C*                    REPOSITION FILE
     C*                    -------------------------------------------
     C                   Z-ADD     1             I
     C     I             DOUGT     IBACK
     C     A1KEY         READPE    PA101                                  50
     C                   ADD       1             I
     C                   ENDDO
     C*
     C*                    RESTART AT NEW LOCATION
     C*                    -------------------------------------------
     C                   SETON                                        21
     C                   EXSR      SR12
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C*
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
     C*          READ ALL DAILY COVERAGE VEHICLES
     C*          -----------------------------------------------------
     C     SR12          BEGSR
      *
     C     *IN21         IFEQ      *OFF
     C     A1KEY         CHAIN     PA101                              50
     C                   ELSE
     C                   SETOFF                                       2150
     C                   END
      *
     C                   MOVEL     'BOTTOM  '    SCROLL            8
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     1             I                 3 0
     C                   CLEAR                   DT
     C                   CLEAR                   ITM
     C                   CLEAR                   LN
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C                   MOVEA     '00000'       *IN(91)
     C     *IN50         DOUEQ     *ON
     C     *IN21         OREQ      *ON
     C*
     C*                    LIST ONLY DAILY COVERAGE VEHICLES
     C*                    -------------------------------------------
     C                   CLEAR                   LINE
     C                   MOVE      A1ITM#        LITM#
     C                   MOVE      A1CC          LCC
     C*
     C*                    GET VEHICLE DESCRIPTION
     C*                    -------------------------------------------
     C     VSKEY         CHAIN     PVS01                              51
     C     *IN51         IFEQ      *OFF
     C                   MOVE      VSYR          LYR
     C                   MOVE      VSMAKE        LMAKE
     C                   MOVE      VSMODL        LMODEL
     C                   MOVE      VSVIN         LID
     C                   END
     C*
     C*                    ONLY 15 LINES ON THE SCREEN
     C*                    -------------------------------------------
     C     I             IFLT      16
     C                   MOVE      LINE          LN(I)
     C                   MOVE      LITM#         ITM(I)
     C     I             ADD       80            I2                3 0
     C                   MOVE      '1'           *IN(I2)
     C                   ADD       1             CURRCD            5 0
     C                   ADD       1             I
     C                   ELSE
     C                   MOVEL     'MORE ...'    SCROLL
     C                   SETON                                        21
     C                   END
     C*
     C*                    BOTTOM OF READ LOOP
     C*                    -------------------------------------------
     C  N21A1KEY         READE     PA101                                  50
     C                   ENDDO
     C                   END
     C     SR12X         ENDSR
