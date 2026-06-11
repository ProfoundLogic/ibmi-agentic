     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7130R - PROMPT FOR UNDERWRITERS REPORT BY FUND
     H*****************************************************************
     H* REVISIONS                                                     *
     H*                                                               *
     H*  08/04/05  C:132 Support: WC133  48415                        *
     H*                  The division field is passing in Alpha into  *
     H*                  the report, (*ALL) which should either be    *
     H*                  a number or blanks.                          *
     H*                                                               *
     H*  08/07/02  C:136 Support: WC114  32095                        *
     H*                  The division field is passing in zeros into  *
     H*                  the report, even when left blank. This is    *
     H*                  causing the report to err.                   *
     H*                                                               *
     H*  05/16/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*****************************************************************
     FW7130F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK

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
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
      *** BEG CHG **** 08/07/02 ************************************************
     C******************** KFLD           SCDIV
     C                   KFLD                    XXDIV             5 0
      *** END CHG **** 08/07/02 ************************************************
     C*
      *** BEG ADD **** 08/07/02 ************************************************
     C     EMKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
      *** END ADD **** 08/07/02 ************************************************
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFYR
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKEY         CHAIN     PFN01                              21
     C*********  *IN21     CABEQ'1'       STRT
     C                   END
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
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
     C*
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/16/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     'PDSFIL'      CAT(P)    QQSUFX        XXRPTL           10
     C                   CAT(P)    '.':0         XXRPTL
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/16/01 ********************************************
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKEY         CHAIN     PFN01                              21
     C********** *IN21     CABEQ'1'       STRT
     C                   END
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*    IF FUND NUMBER = 0, PROCESS ALL FUNDS
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*
     C                   MOVE      SCFYR         XXFYR             3 0
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101                              25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
      ***** BEG ADD ***** 08/04/05 *********************************************
      * If SCDIV not equal BLANKS or NUMBERS, Change to blanks
     C                   TESTN                   SCDIV                69
     C     *IN69         IFEQ      *OFF
     C     SCDIV         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE EMPLOYER IF ENTERED
     C*
      ***** END ADD ***** 08/04/05 *********************************************
     C     SCEMP#        IFNE      *ZERO
      *** BEG ADD **** 08/07/02 ************************************************
     C     SCDIV         IFNE      *BLANKS
     C                   MOVE(P)   SCDIV         XXDIV
      *** END ADD **** 08/07/02 ************************************************
     C     EMKEY         CHAIN     PEM01                              26
      *** BEG ADD **** 08/07/02 ************************************************
     C                   ELSE
     C     EMKY1         CHAIN     PEM01                              26
     C                   END
      *** END ADD **** 08/07/02 ************************************************
     C     *IN26         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE SELECTION CRITERIA
     C*
     C     SCSELO        IFNE      'Y'
     C     SCSELO        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE SELECTION CRITERIA
     C*
     C     SCSELC        IFNE      'Y'
     C     SCSELC        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKEY         CHAIN     PFN01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE PRINT SELECTION
     C*
     C     SCPRNT        IFNE      'S'
     C     SCPRNT        ANDNE     'D'
     C     SCPRNT        ANDNE     'B'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SORT SELECTION
     C*
     C     SCSEQ         IFNE      'C'
     C     SCSEQ         ANDNE     'N'
     C     SCSEQ         ANDNE     'H'
     C     SCSEQ         ANDNE     'O'
     C     SCSEQ         ANDNE     'A'
     C     SCSEQ         ANDNE     'R'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SUMMARY SORT SELECTION
     C*
     C     SCSSRT        IFNE      '1'
     C     SCSSRT        ANDNE     '2'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#             3
     C                   MOVE      SCFND         X@FND             3
     C                   MOVE      SCDTE         X@DTE             8
     C                   Z-ADD     SCFYR         X@FYR3            3 0
     C                   MOVE      X@FYR3        X@FYR             3
     C                   MOVE      SCEMP#        X@EMP#            9
     C                   MOVE      SCDIV         X@DIV             5
     C                   MOVE      SCSELC        X@SELC            1
     C                   MOVE      SCSELO        X@SELO            1
     C                   MOVE      SCMNTH        X@MNTH            3
     C                   MOVE      SCPRNT        X@PRNT            1
     C                   MOVE      SCSEQ         X@SEQ             1
     C                   MOVE      SCSSRT        X@SSRT            1
     C                   CALL      'W7130C'
     C                   PARM                    X@CO#
     C                   PARM                    X@FND
     C                   PARM                    X@DTE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C                   PARM                    X@FYR
     C                   PARM                    X@EMP#
     C                   PARM                    X@DIV
     C                   PARM                    X@SELC
     C                   PARM                    X@SELO
     C                   PARM                    X@MNTH
     C                   PARM                    X@PRNT
     C                   PARM                    X@SEQ
     C                   PARM                    X@SSRT
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
