     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E7526R - EXPECTED LOSS RATES & 'D' RATIO LISTING
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES PRINT CABABILITIES FOR THE        **
     H**     EXPECTED LOSS DSCNT RATIO FILE (EMEXP)                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     E7526C                                                  **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    02/21/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**  09/04/01  C:136 Project: WC577                             **
     H**            Emp_Exp_Mod Rewrite - Enhanced functionality by  **
     H**            adding selections for effective date and payroll **
     H**            class range.                                     **
     H**                                                             **
     H**  08/12/98  C:90  PROJECT:  WC853                            **
     H**            MODIFIED TO VERSION 3 STANDARDS                  **
     H**                                                             **
     H**  11/07/94  C:30  PROJECT:  WC356                            **
     H**            Add field State ID Code.                         **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      KC      Command 03 - EOJ                               **
     H**      10      HELP                                           **
     H**  SCR01:                                                     **
     H**                                                             **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E7526F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO. FILE                                  **
     H**    SMUPL (PUP01): USER FILE                                 **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE7526F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
      *** BEG CHG *** 09/04/01 **********************************************
     F****************EMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
      *** END CHG *** 09/04/01 **********************************************
     F*

     D*
     D* Table for Verifying State
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D*
     D* Data Structure for System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Data Structure for Screen Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* external Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Define Work Fields
     C*
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Entry Parameters
     C*
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
     C* Initialize and Set-Up
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Retrieve the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Retrieve the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* Load Screen Record Format for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Screen Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
      *** BEG ADD 09/04/01 **********************************************
     C                   SETOFF                                       232425
      *** END ADD 09/04/01 **********************************************
     C                   END
     C*
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
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
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C* Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C* Process Requested Screen
     C*
     C     SCR01T        TAG
     C*
     C* Check for valid state code.
     C*
     C     SCST          IFGT      *BLANK
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
      *** BEG ADD 09/04/01 **********************************************
     C     SCEFDT        IFNE      *ZEROS
     C                   MOVEL     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        24
     C                   GOTO      STRT
     C                   ENDIF                                                  IF SRERR 1
     C                   ENDIF                                                  SCEFDT<>ZEROS
     C*
     C     SCFRCL        IFGT      SCTOCL
     C                   SETON                                        25
     C                   GOTO      STRT
     C                   ENDIF
      *** END ADD 09/04/01 **********************************************
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
      *** BEG ADD 09/04/01 **********************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         QQEFDT
     C                   MOVEL(P)  SCFRCL        QQFRCL
     C                   MOVEL(P)  SCTOCL        QQTOCL
      *** END ADD 09/04/01 **********************************************
      *
     C                   CALL      'E7526C'
     C                   PARM                    QQCO#
     C                   PARM                    SCST
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
      *** BEG ADD 09/004/01 **************************************************
     C                   PARM                    QQEFDT            7
     C                   PARM                    QQFRCL            5
     C                   PARM                    QQTOCL            5
      *** END ADD 09/004/01 **************************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
