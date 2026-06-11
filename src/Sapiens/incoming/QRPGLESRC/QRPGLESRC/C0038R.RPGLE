     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0038R - PURGE INACTIVE RECORDS
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program allows the user to purge inactive         **
     H**     records from a selected file. Files that can be        **
     H**     purged are:                                            **
     H**        . Agent Producer File                               **
     H**        . Auto Coverage File                                **
     H**        . Vehicle Schedule File                             **
     H**   CALLED FROM:                                             **
     H**     MENU                                                   **
     H**   CALLS TO:                                                **
     H**     C0530R  - Policy Master Search by Policy Number        **
     H**     C0531R  - Policy Master Search by Named Insured        **
     H**     C0620R  - Agent Number Search                          **
     H**     C0621R  - Agent Alpha Search                           **
     H**     C0624R  - Agent Code Number Search                     **
     H**     M0006C  - Help Subroutine                              **
     H**   PROGRAMMER: C:31                                         **
     H**   CREATED: 05/20/96         PROJECT: 67CFO                 **
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
     H**      10       Screen Help key.                             **
     H**      11       Screen Valid command key.                    **
     H**      21       Invalid selection code 1.                    **
     H**      22       Invalis selection code 2.                    **
     H**      31       Invalid agent.                               **
     H**      32       Invalid policy.                              **
     H**      50       General usage.                               **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   CDPRL  (PPR01): Agent Producer File (CO#/FEIN/MOD)       **
     H**   CMAGL  (PAG01): Agent Master File (CO#/FEIN/MOD)         **
     H**   CMA1L  (PA101): Auto Coverage Detail File (CO#/POL#)     **
     H**   CMPML  (PPM01): Policy Master File (CO#/POL#)            **
     H**   CMUPL  (PUP01): User Parameter File (USR)                **
     H**   CMVSL  (PVS01): Vehicle Schedule File (CO#/POL#)         **
     H**   SMCOL  (PCO01): System Company Master File (CO#)         **
     H****************************************************************
     H*
     FC0038F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCDPRL     UF   E           K DISK
     FCMAGL     IF   E           K DISK
     FCMA1L     UF   E           K DISK
     FCMVSL     UF   E           K DISK
     FCMPML     IF   E           K DISK

     D*
     D* ---------------------------------------------------------------
     D* TABLES AND ARRAYS
     D* ---------------------------------------------------------------
     D CNT             S              1    DIM(6)                               COUNT ARRAY
     D*
     D* ---------------------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* ---------------------------------------------------------------
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* ---------------------------------------------------------------
     D*        TIME AND DATE
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSMM                   7      8  0
     D  DSDD                   9     10  0
     D  DSYY                  11     12  0
     D*
     D* ---------------------------------------------------------------
     D*        HELP REQUEST WORK AREA
     D* ---------------------------------------------------------------
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*----------------------------------------------------------------
     D*        MESSAGE WORK AREA
     D*----------------------------------------------------------------
     D*
     D                 DS
     D  XXMS1                  1     50
     D  XXMS11                 1      6
     D  XXMS12                 8     27    INZ('RECORDS REMOVED FROM')
     D  XXMS13                29     33    INZ('AGENT')
     D  XXMS14                35     45
     D  XXMS15                46     46    INZ('/')
     D  XXMS16                47     49
     D                 DS
     D  XXMS2                  1     50
     D  XXMS21                 1      6
     D  XXMS22                 8     27    INZ('RECORDS REMOVED FROM')
     D  XXMS23                29     34    INZ('POLICY')
     D  XXMS24                36     48
      /EJECT

     C*----------------------------------------------------------------
     C*        ENTRY PARM LIST
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*        KEY LISTS
     C*----------------------------------------------------------------
     C*
     C*          PAG01 - AGENT MASTER FILE
     C     AGKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*          PA101 - AUTO LIABILITY COVERAGE DETAIL FILE
     C     A1KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*          PVS01 - VEHICLE SCHEDULE FILE
     C     VSKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*          PCO01 - COMPANY FILE
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*          PPM01 - POLICY MASTER FILE
     C     PMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*          PPR01 - AGENT PRODUCER FILE
     C     PRKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*          PUP01 - USER PARAMETER FILE
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      /EJECT
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          M A I N L I N E
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C                   EXSR      SR00                                         1ST PASS
     C*
     C     *INLR         DOWEQ     *OFF
     C*
     C     Q2RECF        CASEQ     'SCR01'       SR01T                          SCREEN 1
     C                   ENDCS
     C*
     C                   ENDDO
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          S U B R O U T I N E S
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C*  SR00    INITIAL1ZATION SUBROUTINE
     C*----------------------------------------------------------------
     C*
     C*          GET COMPANY # FROM USER PARAMETER FILE
     C*          ------------------------------------------------------
     C     SR00          BEGSR
     C     UPKY          CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#             3
     C*
     C*          GET COMPANY NAME FROM COMPANY RECORD
     C*          ------------------------------------------------------
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              50
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*          MOVE SYSTEM VALUES TO SCREEN FIELD
     C*          ------------------------------------------------------
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*          MOVE IN VALUES FOR FIRST PASS
     C*          ------------------------------------------------------
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     SR00X         ENDSR
     C*
     C*----------------------------------------------------------------
     C*  SR01T   PROCESS SCREEN 1
     C*----------------------------------------------------------------
     C*
     C*          WRITE/READ SCREEN 1
     C*          ------------------------------------------------------
     C     SR01T         BEGSR
     C                   EXFMT     SCR01
     C                   MOVE      *BLANKS       SCMESG
     C*
     C*          CHECK COMMAND KEYS
     C*          ------------------------------------------------------
     C     *IN11         IFEQ      '1'
     C                   EXSR      SR11
     C                   END
     C*
     C*          IF COMMAND KEY STILL ON - SKIP TO END
     C*          ------------------------------------------------------
     C     *IN11         IFEQ      '1'
     C                   GOTO      SR01X
     C                   END
     C*
     C*          WAS AGENT PURGE SELECTED?
     C*          ------------------------------------------------------
     C     SCFL01        IFNE      *BLANK
     C*
     C*             CHECK FOR VALID SELECTION CODE
     C*             ---------------------------------------------------
     C     SCFL01        IFNE      'X'
     C                   SETON                                        21
     C                   GOTO      SR01X
     C                   END
     C*
     C*             CHECK FOR VALID AGENT
     C*             ---------------------------------------------------
     C     AGKY          CHAIN     PAG01                              50
     C     *IN50         IFEQ      '1'
     C                   SETON                                        31
     C                   GOTO      SR01X
     C                   END
     C*
     C*             EDITS PASSED - PURGE AGENT PRODUCER FILE
     C*             ---------------------------------------------------
     C     PRKY          CHAIN     PPR01                              50
     C     *IN50         DOWEQ     *OFF
     C     PRSTAT        IFEQ      'I'
     C                   DELETE    PPR01
     C                   ADD       1             XXCNT1            6 0
     C                   END
     C     PRKY          READE     PPR01                                  50
     C                   ENDDO
     C*
     C*             CLEAR LEADING ZEROS
     C*             ---------------------------------------------------
     C                   MOVE      XXCNT1        XXCNT2            6
     C                   MOVEA     XXCNT2        CNT
     C                   DO        6             X                 3 0
     C                   SETOFF                                       50
     C     CNT(X)        IFEQ      '0'
     C     *IN50         ANDEQ     *OFF
     C                   MOVE      ' '           CNT(X)
     C                   ELSE
     C                   SETON                                        50
     C                   END
     C                   ENDDO
     C*
     C*             INITIALIZE MESSAGE
     C*             ---------------------------------------------------
     C                   MOVEA     CNT           XXMS11
     C     XXMS11        IFEQ      *BLANKS
     C                   MOVE      'NO'          XXMS11
     C                   END
     C                   MOVE      SCFEIN        XXMS14
     C                   MOVE      SCMOD         XXMS16
     C                   MOVE      XXMS1         SCMESG
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*             CLEAR SCREEN FIELDS
     C*             ---------------------------------------------------
     C                   MOVE      *BLANKS       SCFL01
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCMOD
     C                   GOTO      SR01X
     C                   END
     C*
     C*          WAS POLICY PURGE SELECTED?
     C*          ------------------------------------------------------
     C     SCFL02        IFNE      *BLANK
     C*
     C*             CHECK FOR VALID SELECTION CODE
     C*             ---------------------------------------------------
     C     SCFL02        IFNE      'X'
     C                   SETON                                        22
     C                   GOTO      SR01X
     C                   END
     C*
     C*             CHECK FOR VALID POLICY
     C*             ---------------------------------------------------
     C     PMKY          CHAIN     PPM01                              50
     C     *IN50         IFEQ      '1'
     C                   SETON                                        32
     C                   GOTO      SR01X
     C                   END
     C*
     C*             EDITS PASSED - PURGE AUTO COVERAGE FILE
     C*             ---------------------------------------------------
     C     A1KY          CHAIN     PA101                              50
     C     *IN50         DOWEQ     *OFF
     C     A1STAT        IFEQ      'I'
     C                   DELETE    PA101
     C                   ADD       1             XXCNT1            6 0
     C                   END
     C     A1KY          READE     PA101                                  50
     C                   ENDDO
     C*
     C*             EDITS PASSED - PURGE VEHICLE SCHEDULE FILE
     C*             ---------------------------------------------------
     C     VSKY          CHAIN     PVS01                              50
     C     *IN50         DOWEQ     *OFF
     C     VSSTAT        IFEQ      'I'
     C                   DELETE    PVS01
     C                   ADD       1             XXCNT1            6 0
     C                   END
     C     VSKY          READE     PVS01                                  50
     C                   ENDDO
     C*
     C*             CLEAR LEADING ZEROS
     C*             ---------------------------------------------------
     C                   MOVE      XXCNT1        XXCNT2            6
     C                   MOVEA     XXCNT2        CNT
     C                   DO        6             X                 3 0
     C                   SETOFF                                       50
     C     CNT(X)        IFEQ      '0'
     C     *IN50         ANDEQ     *OFF
     C                   MOVE      ' '           CNT(X)
     C                   ELSE
     C                   SETON                                        50
     C                   END
     C                   ENDDO
     C*
     C*             INITIALIZE MESSAGE
     C*             ---------------------------------------------------
     C                   MOVEA     CNT           XXMS21
     C     XXMS21        IFEQ      *BLANKS
     C                   MOVE      'NO'          XXMS21
     C                   END
     C                   MOVE      SCPOL#        XXMS24
     C                   MOVE      XXMS2         SCMESG
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*             CLEAR SCREEN FIELDS
     C*             ---------------------------------------------------
     C                   MOVE      *BLANKS       SCFL02
     C                   MOVE      *BLANKS       SCPOL#
     C                   GOTO      SR01X
     C                   END
     C*
     C*          END OF SUBROUTINE
     C*          ------------------------------------------------------
     C     SR01X         ENDSR
     C*
     C*----------------------------------------------------------------
     C*  SR11    PROCESS FUNCTION KEYS
     C*----------------------------------------------------------------
     C*
     C*         FUNCTION 01 = END OF JOB
     C*          ------------------------------------------------------
     C     SR11          BEGSR
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C*
     C*         FUNCTION 05 = POLICY NUMBER SEARCH
     C*          ------------------------------------------------------
     C     *INKE         IFEQ      '1'
     C                   CALL      'C0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCPOL#
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      '*'
     C     SCFL02        IFNE      'X'
     C                   MOVE      *BLANK        SCFL02
     C                   SETON                                        22
     C                   ELSE
     C                   SETOFF                                       11
     C                   END
     C                   GOTO      SR11X
     C                   END
     C                   END
     C*
     C*         FUNCTION 06 = SEARCH POLICY MASTER BY NAME
     C*          ------------------------------------------------------
     C     *INKF         IFEQ      '1'
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCPOL#
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      '*'
     C     SCFL02        IFNE      'X'
     C                   MOVE      *BLANK        SCFL02
     C                   SETON                                        22
     C                   ELSE
     C                   SETOFF                                       11
     C                   END
     C                   GOTO      SR11X
     C                   END
     C                   END
     C*
     C*         FUNCTION 07 = SEARCH AGENT MASTER BY FEIN
     C*          ------------------------------------------------------
     C     *INKG         IFEQ      '1'
     C                   CALL      'C0620R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCFEIN        OREQ      0
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      '*'
     C     SCFL01        IFNE      'X'
     C                   MOVE      *BLANK        SCFL01
     C                   SETON                                        21
     C                   ELSE
     C                   SETOFF                                       11
     C                   END
     C                   GOTO      SR11X
     C                   END
     C                   END
     C*
     C*         FUNCTION 08 = SEARCH AGENT MASTER BY ALPHA
     C*          ------------------------------------------------------
     C     *INKH         IFEQ      '1'
     C                   CALL      'C0621R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCFEIN        OREQ      0
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      '*'
     C     SCFL01        IFNE      'X'
     C                   MOVE      *BLANK        SCFL01
     C                   SETON                                        21
     C                   ELSE
     C                   SETOFF                                       11
     C                   END
     C                   GOTO      SR11X
     C                   END
     C                   END
     C*
     C*         FUNCTION 09 = SEARCH AGENT MASTER BY CODE
     C*          ------------------------------------------------------
     C     *INKI         IFEQ      '1'
     C                   CALL      'C0624R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   MOVE      '1'           *INLR
     C                   GOTO      SR11X
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCFEIN        OREQ      0
     C                   GOTO      SR11X
     C                   END
     C     QQRTC         IFEQ      '*'
     C     SCFL01        IFNE      'X'
     C                   MOVE      *BLANK        SCFL01
     C                   SETON                                        21
     C                   ELSE
     C                   SETOFF                                       11
     C                   END
     C                   GOTO      SR11X
     C                   END
     C                   END
     C*
     C*          HELP REQUEST
     C*          ------------------------------------------------------
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
     C*          END OF SUBROUTINE
     C*          ------------------------------------------------------
     C     SR11X         ENDSR
