     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7935R - PROMPT FOR DEDUCTABLE DETAIL REPORT
     H*****************************************************************
     H*  CREATED: C:53  PROJECT: 71WEE                                *
     H*                 NEW PROMPT                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 02/01/10  C:33  Project: 25WRB                                *
     H*                -Change to use group select processing         *
     H*                                                               *
     H* 01/11/10  C:33  Project: WC1838                               *
     H*                 Process Large & Small deductibles             *
     H*                                                               *
     H*  01/28/04  C:53  Support#: 40830
     H*                  Add post date to the prompt.
     H*
     H*****************************************************************
     FW7935F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F***  Begin Chg  ***  01/11/10  **************************************
     FSSYLF     IF   E           K DISK

     D*
     D OPTNM           C                   CONST('Deductible Detail -
     D                                     Report')
     D***  End   Chg  ***  01/11/10  **************************************
     D*
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
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
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
     C*================================================================
     C*                    *LIKE DEFN STATEMENTS
     C*================================================================
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C                   CLEAR                   WCFL43
     C     'WC'          CHAIN     SWC01                              60
     C*
     C* Check for Large Deductible Module
     C                   SELECT
     C     WCFL43        WHENEQ    'Y'
     C                   MOVE      X'20'         AT1                            Normal
     C                   MOVE      X'26'         AT2                            Underscore, HI
     C                   MOVE      *ON           LRGDED            1
     C                   OTHER
     C                   MOVE      X'27'         AT1                            Non-display
     C                   MOVE      X'A7'         AT2                            Protect,Non-display
     C                   MOVE      *OFF          LRGDED
     C                   MOVE      'S'           SCDDTP
     C                   ENDSL
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C***  Begin Add  ***  02/01/10  **************************************
     C                   CLEAR                   SCFND
     C***  End   Add  ***  02/01/10  **************************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNPZIP        SCZP              3 0
     C*
     C                   TIME                    DSDTTM
     C***  Begin Chg  ***  01/11/10  **************************************
     C**********           MOVE QQPRDS    SCPRDS
     C                   SELECT
     C     Q1PRMS        WHENGE    1
     C                   MOVEL     QQPRDS        ##DATA
     C                   OTHER
     C                   MOVEL     OPTNM         ##DATA
     C                   ENDSL
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  End   Chg  ***  01/11/10  **************************************
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
     C                   ENDIF                                                  Q2RECF IFEQ
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ
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
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10 IFEQ
     C*
     C*   F10 = FUND SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C***  Begin Add  ***  02/01/10  **************************************
     C* Don't search on groups
     C     HFLD          IFEQ      'SCFND'
     C                   CLEAR                   HFLD
     C                   ENDIF
     C***  End   Add  ***  02/01/10  **************************************
     C*
     C     HFLD          IFEQ      'SCFND'
     C                   MOVEL     XXCO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@FND         ANDNE     *ZERO
     C                   MOVE      @@FND         SCFND
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFND IFEQ
     C                   ENDIF                                                  *INKJ IFEQ
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C                   ENDIF                                                  *IN11 IFEQ
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  LOSSES AS OF DATE MUST BE VALID
     C*
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLSDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEG ADD *** 01/28/04 ****************************
     C*
     C*  A/R POST DATE MUST BE VALID
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C***  Begin Add  ***  01/11/10  **************************************
     C* Deductible Billing Type
     C     LRGDED        IFEQ      *ON
     C*
     C     SCDDTP        IFNE      'B'
     C     SCDDTP        ANDNE     'L'
     C     SCDDTP        ANDNE     'S'
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C     *LIKE         DEFINE    SCDDTP        QQDDTP
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C*** END ADD *** 01/28/04 ****************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
     C                   MOVE      SCLSDT        QQLSDT            8
     C                   MOVE      SCPODT        QQPODT            8
     C                   CALL      'W7935C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    QQLSDT
     C                   PARM                    QQPODT
     C***  Begin Add  ***  01/11/10  **************************************
     C                   PARM      SCDDTP        QQDDTP
     C***  End   Add  ***  01/11/10  **************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
