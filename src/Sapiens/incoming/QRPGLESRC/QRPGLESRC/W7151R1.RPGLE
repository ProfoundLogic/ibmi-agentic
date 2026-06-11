     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE W7151R1 CLAIM PROFILE BY CLAIMANT RANGE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program allows entry of a claim number range that **
     H**     will be used to select and run batch claim profile     **
     H**   CALLED FROM:                                             **
     H**     CLM-RPT Menu                                           **
     H**   CALLS TO:                                                **
     H**     M0006C     Process Help Requests                       **
     H**     W0006R     Change User Parameters                      **
     H**     W0570R     Claim MAster Search By CAse                 **
     H**     W0571R     Claim MAster Search By SS#                  **
     H**     W0572R     Claimant Alpha/SS# Search                   **
     H**     W7151C1    Claims Profile By RAnge - Submit            **
     H**   PROGRAMMER: Ed Lane                                      **
     H**   CREATED: 09/28/94         PROJECT: WC370                 **
     H****************************************************************
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7151F1 IN SCREEN.    *
     H*                                                               *
     H*  05/29/96 C:33 PROJECT WC420                                  *
     H*                IF THIS REPORT IS CALLED ON DEMAND IT SHOULD   *
     H*                BE ROUTED TO THE USER PARM OUTQ INSTEAD OF THE *
     H*                OVERRIDE OUTQ.                                 *
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**                                                            **
     H**      KM       Command 13 - Change User Parameters          **
     H**      10       Help Key Selected                            **
     H**      11       A Command Key Selected                       **
     H**      20       Error WCR0547 - Value LT or EQ Zero          **
     H**      21       Error WCR0502 - Valus LT Zero                **
     H**      22       Error WCR0702 - From Range not GT To Range   **
     H**      23       Error WCR0517 - Fund Year Invalid For Fund   **
     H**      24       Error WCR0808 - Cursor Not in RAnge Field    **
     H**      35       Position Cursor in 1st (From) Range Field    **
     H**      36       Position Cursor in 2nd (To) Range Field      **
     H**      60       General Usage                                **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H** FILES:                                                     **
     H**    SMCOL (PC001): System Company File For Work Comp        **
     H**    WDF1L (PF101): Fund Detail Record                       **
     H**    WMFNL (PFN01): Fund Master File                         **
     H**    WMUPL (PUP01): User Parameters                          **
     H****************************************************************
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMCMP     IF   E             DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FW7151F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C     *LIKE         DEFINE    CMCASE        SCCASE
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*
     C* BEGIN PROCESSING
     C*
     C     STRT01        TAG
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  GET THE FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCNAME
     C                   GOTO      STRT
     C                   END
     C                   MOVE      FNNAME        SCNAME
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
     C     *IN11         IFEQ      '1'                                          VALID CMD KEYS
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F10 = CLAIM NUMBER SEARCH--TO GET CASE #
     C*
     C                   MOVE      *OFF          *IN20
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   Z-ADD     *ZERO         LIN               2 0
     C                   Z-ADD     *ZERO         COL               2 0
     C     Q2CPOS        DIV       256           LIN
     C                   MVR                     COL               2 0
     C*
     C* To search for Case #, the cursor must be at a Case # field.
     C*
     C     LIN           IFNE      10
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM      SCFYR         @@FYR             3 0
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C*
     C                   SELECT
     C*
     C     COL           WHENGE    23
     C     COL           ANDLE     30
     C                   MOVE      '1'           *IN35                           POS CURSOR @ FR CAS
     C*
     C     COL           WHENGE    33
     C     COL           ANDLE     40
     C                   MOVE      '1'           *IN36                           POS CURSOR @ TO CAS
     C*
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C*
     C     COL           IFGE      23
     C     COL           ANDLE     30
     C                   Z-ADD     CMCASE        SCFCAS
     C                   MOVE      '1'           *IN36                          POS CURSOR @ TO CASE
     C                   ELSE
     C                   Z-ADD     CMCASE        SCTCAS
     C                   END
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   GOTO      STRT01
     C                   END
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* VALIDATE FUND YEAR
     C*
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     F1KEY         CHAIN     PF101                              23
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE CASE NUMBER RANGE
     C*
     C*  FROM CASE # MUST BE GREATER OR EQUAL TO ZERO
     C*
     C     SCFCAS        IFLT      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  TO CASE # MUST BE GREATER THAN ZERO
     C*
     C     SCTCAS        IFLE      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  RANGE MUST BE IN ASCENDING ORDER
     C*
     C     SCFCAS        IFGT      SCTCAS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS PASSED, CALL CLAIM SERVICE REGISTER PROGRAM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   MOVE      XXDATE        QQDATE            8
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      SCFCAS        QQFCAS            7
     C                   MOVE      SCTCAS        QQTCAS            7
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W7151C1'
     C                   PARM                    QQCO#
     C                   PARM                    QQDATE
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQFCAS
     C                   PARM                    QQTCAS
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
