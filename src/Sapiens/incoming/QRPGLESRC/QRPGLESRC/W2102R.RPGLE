     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2102R - CLEAR BILLING REQUEST FILE PROMPT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM PROMPTS FOR A FUND YEAR OF WHICH ALL BILLING *
     H*     REQUEST RECORDS ARE TO BE DELETED.                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     MENU OPTION                                               *
     H*  CALLS TO:                                                    *
     H*     W2102C - SUBMITTING CL                                    *
     H*                                                               *
     H*  PROGRAMMER: C:24                                             *
     H*  CREATED:    04/06/95               PROJECT:WC407             *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   20   POLICY FROM DATE IS INVALID                            *
     H*   21   POLICY TO DATE IS INVALID                              *
     H*   22   WARNING - POLICY RANGE CROSSES FUND YEARS              *
     H*   23   POLICY DATE RANGE IS INVALID                           *
     H*   60   GENERAL READ/CHAIN                                     *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   WMFNL   PFN01: CO/FND                           Fund Master *
     H*   WDF1L   PF101: CO/FND/FYR                       Fund Detail *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  11/06/03 C:65 PROJECT: SUPP#34947/#39803                     *
     H*                POLICY EFFECTIVE DATE RANGE IS VALID IF BOTH   *
     H*                DATES ARE THE SAME.                            *
     H*                                                               *
     H* 10/09/03 C:61  PROJECT WC1150                                 *
     H*                Add selection for "Type of records to clear"   *
     H*                ( Estimated, Audited, Blank for all ).         *
     H*                                                               *
     H* 02/25/00  C:35   PROJECT: SUPPORT                             *
     H*                  CORRECT DATE EDIT FOR CROSSING POLICY YEARS. *
     H*                                                               *
     H*  12/01/99 C:118 PROJECT: WC577                                *
     H*                 Made V3 changes to display file.              *
     H*                                                               *
     H*  03/03/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  11/21/96 C:65 PROJECT: CM023                                 *
     H*                CHANGED FYR TO A RANGE OF POLICY EFFECTIVE     *
     H*                DATES.  REMOVED CMD-5 FOR FYR SEARCH.  ALSO    *
     H*                ADDED WARNING MESSAGE IF DATE RANGE CROSSES    *
     H*                FYR'S.                                         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW2102F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      **** BEGIN CHG **** 02/25/00 *********************************************
     F********** WDF1L   IF  E           K        DISK
     FWDF1L1    IF   E           K DISK
      **** END   CHG **** 02/25/00 *********************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXMDY                  1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   7      8  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
      **** BEGIN CHG **** 02/25/00 *********************************************
     C******************** KFLD           XXFYR   30
     C                   KFLD                    XXDT1
      **** END   CHG **** 02/25/00 *********************************************
      ***** BEG DEL ******* 10/09/03 **********************************
     C*********
     C***PARM*LIST TO CALL SUBMITTER CL
     C*********
     C*********  PL2100    PLIST
     C*********            PARM           QQCO#   3
     C*********            PARM           QQFND   3
     C*********            PARM           QQEFDT  8
     C*********            PARM           QQTODT  8
     C*********            PARM           UPOUTQ
     C*********            PARM 'D'       QQMODE  1
      ***** END DEL ******* 10/09/03 **********************************
     C*
     C     *LIKE         DEFINE    FNDT          EFDT1
     C     *LIKE         DEFINE    FNDT          TODT1
     C     *LIKE         DEFINE    FNDT          XXDT1
     C     *LIKE         DEFINE    FNDT          XXDT2
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C***** BEGIN CHG **** 12/01/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   CHG **** 12/01/99 ***********************************
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN CHG **** 12/01/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFND
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   CHG **** 12/01/99 ***********************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FROM AND TO DATES.
     C*
     C* FROM DATE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR == 1
     C* TO DATE
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR == 1
     C* CHECK IF TO DATE IS LESS THAN OR EQUAL TO THE FROM DATE.
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EFDT1
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TODT1
     C*
     C*** BEGIN CHG ***  11/06/03  ************************************
     C*********  TODT1     IFLE EFDT1
     C     TODT1         IFLT      EFDT1
     C*** END CHG *****  11/06/03  ************************************
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF                                                  TODT <= EFDT
     C* CHECK IF FROM AND TO DATES CROSS FUND YEARS
     C     SCEFDT        IFNE      SVEFDT
     C     SCTODT        ORNE      SVTODT
      **** BEGIN DEL ***** 02/25/00 ********************************************
     C**********           MOVE SCEFDT    XXMDY
     C**********           Z-ADDXXYY      XXFYR
     C********** F1KY      CHAINPF101                60
      **** END   DEL ***** 02/25/00 ********************************************
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT1
      **** BEGIN ADD ***** 02/25/00 ********************************************
     C     F1KY          SETGT     PF101
     C     F1KY          READPE    PF101                                  60
      **** END   ADD ***** 02/25/00 ********************************************
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT2
     C*
     C     XXDT1         IFLT      F1STPO
     C     XXDT2         ORGT      F1ENPO
     C                   MOVE      *ON           *IN22
     C                   MOVE      SCEFDT        SVEFDT            8 0
     C                   MOVE      SCTODT        SVTODT            8 0
     C                   GOTO      STRT
     C                   ENDIF                                                  EFDT < STPO
     C                   ENDIF                                                  SC != SV
      ***** BEG ADD ******* 10/09/03 **********************************
      *
      *  "Type of records to clear" must be (E)stimated or (A)udited,
      *   if entered.
      *
     C     SCEAB         IFNE      *BLANK
     C     SCEAB         ANDNE     'E'
     C     SCEAB         ANDNE     'A'
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEAB
      *
      ***** END ADD ******* 10/09/03 **********************************
      ***** BEG DEL ******* 10/09/03 **********************************
     C*********
     C***ALL*EDITS PASSED. CALL CL TO SUMBIT REQUEST.
     C*********
     C*********            MOVE UPCO#     QQCO#
     C*********            MOVE UPFND     QQFND
     C*********            MOVE SCEFDT    QQEFDT
     C*********            MOVE SCTODT    QQTODT
     C*********            CALL 'W2102C'  PL2100
      ***** END DEL ******* 10/09/03 **********************************
      ***** BEG ADD ******* 10/09/03 **********************************
      *
      *  ALL EDITS PASSED. CALL CL TO SUMBIT REQUEST.
      *
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      UPFND         ##FND
     C                   MOVE      SCEFDT        ##EFDT
     C                   MOVE      SCTODT        ##TODT
     C                   MOVE      UPOUTQ        ##OUTQ
     C                   MOVE      SCEAB         ##EAB
      *
     C                   CALL      'W2102C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##EFDT            8
     C                   PARM                    ##TODT            8
     C                   PARM                    ##OUTQ           10
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##EAB             1
      *
      ***** END ADD ******* 10/09/03 **********************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
