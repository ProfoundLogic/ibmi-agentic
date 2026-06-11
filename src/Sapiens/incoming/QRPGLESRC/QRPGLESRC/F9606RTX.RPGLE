     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9606RTX                                          *
     H*    DESCRIPTION: PROMPT FOR EOMB  MAGNETIC TAPE SUBMISSION     *
     H*                 EDIT REPORT                                   *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      F9605CTX - CREATE WORKFILE                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  SCR01:                                                       *
     H*  25     INVALID INPUT                                         *
     H*  SCR02:                                                       *
     H*  20     INVALID FROM DATE                                     *
     H*  21     INVALID TO DATE                                       *
     H*  22     DATES MUST BE IN ASCENDING ORDER                      *
     H*  23     INVALID FUND 1                                        *
     H*  24     INVALID FUND 2                                        *
     H*  25     INVALID FUND 3                                        *
     H*  26     INVALID FUND 4                                        *
     H*  27     INVALID FUND 5                                        *
     H*  28     INVALID FUND 6                                        *
     H*  29     INVALID FUND 7                                        *
     H*  30     INVALID FUND 8                                        *
     H*  31     INVALID FUND 9                                        *
     H*  32     INVALID FUND 10                                       *
     H*  33     INVALID FUND 11                                       *
     H*  34     INVALID FUND 12                                       *
     H*  35     INVALID FUND 13                                       *
     H*  36     INVALID FUND 14                                       *
     H*  37     INVALID FUND 15                                       *
     H*  38     INVALID FUND 16                                       *
     H*  39     INVALID FUND 17                                       *
     H*  40     INVALID FUND 18                                       *
     H*  41     INVALID FUND 19                                       *
     H*  42     INVALID FUND 20                                       *
     H*  43     INVALID FUND 21                                       *
     H*  44     INVALID FUND 22                                       *
     H*  45     INVALID FUND 23                                       *
     H*  46     INVALID FUND 24                                       *
     H*  47     INVALID FUND 25                                       *
     H*  48     INVALID FUND 26                                       *
     H*  49     INVALID FUND 27                                       *
     H*  50     INVALID FUND 28                                       *
     H*  51     INVALID FUND 29                                       *
     H*  52     INVALID FUND 30                                       *
     H*  60     GENERAL CHAIN                                         *
     H*****************************************************************
     FF9606FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK

     D LDA             DS
     D  FND                    1     90  0
     D                                     DIM(30)                              FUND
     D  SCFN01                 1      3  0
     D  SCFN02                 4      6  0
     D  SCFN03                 7      9  0
     D  SCFN04                10     12  0
     D  SCFN05                13     15  0
     D  SCFN06                16     18  0
     D  SCFN07                19     21  0
     D  SCFN08                22     24  0
     D  SCFN09                25     27  0
     D  SCFN10                28     30  0
     D  SCFN11                31     33  0
     D  SCFN12                34     36  0
     D  SCFN13                37     39  0
     D  SCFN14                40     42  0
     D  SCFN15                43     45  0
     D  SCFN16                46     48  0
     D  SCFN17                49     51  0
     D  SCFN18                52     54  0
     D  SCFN19                55     57  0
     D  SCFN20                58     60  0
     D  SCFN21                61     63  0
     D  SCFN22                64     66  0
     D  SCFN23                67     69  0
     D  SCFN24                70     72  0
     D  SCFN25                73     75  0
     D  SCFN26                76     78  0
     D  SCFN27                79     81  0
     D  SCFN28                82     84  0
     D  SCFN29                85     87  0
     D  SCFN30                88     90  0
     D* CURRENT DATE
     D                 DS
     D  XXDT                   1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D*
     D* Year prior to current date's year; used to calc Julian date
     D                 DS
     D  ZZDT                   1      8  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZYY                   5      8  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C* Key List for the Group Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    XXFND
     C*
     C     FNKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C* Key List for the SUBCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*  Entry Parameters
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
     C*  INITIALIZE WORKFIELDS
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *DTAARA       DEFINE    *LDA          LDA
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C* Get the Julian Date
     C                   Z-ADD     DSDATE        XXDT
     C                   Z-ADD     12            ZZMM
     C                   Z-ADD     31            ZZDD
     C     XXYY          IFNE      0
     C     XXYY          SUB       1             ZZYY
     C                   ELSE
     C                   Z-ADD     99            ZZYY
     C                   END
     C*
     C* Call sub-program SRDDFF to Calc Julian Date
     C                   MOVE      ZZDT          SRFRDC            8 0
     C                   MOVE      XXDT          SRTODC            8 0
     C                   Z-ADD     0             SRDAYS            5 0
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     SRDAYS        XXDAT             3 0
     C*
     C*  GET COMPANY NAME
     C*
     C     STRT01        TAG
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
     C*  GET SUBCRIBER NAME
     C*
     C                   MOVE      UPSUB#        SCSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCSBNM
     C                   ELSE
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display Screen Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C******************************************************************
     C* Process All Command Keys
     C     *IN11         IFEQ      '1'
     C************************************
     C* F3 = EXIT                        *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C************************************
     C* CMD 13 - CHG USER PARMS          *
     C************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    END *INKM
     C******
     C                   END                                                    END CMDKEYS
     C*********************************************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C****************************
     C* CHOOSE PROCESSING        *
     C****************************
     C     SCR01T        TAG
     C*
     C     SCPROC        IFNE      'P'
     C     SCPROC        ANDNE     'H'
     C     SCPROC        ANDNE     'B'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   Z-ADD     *ZERO         SCFRDT
     C                   Z-ADD     *ZERO         SCTODT
     C                   Z-ADD     *ZERO         FND
     C                   GOTO      STRT
     C*
     C****************************
     C* PROCESS FUNDS            *
     C****************************
     C*
     C     SCR02T        TAG
     C*
     C* Validate the Entered "FROM DATE"
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the Entered "TO DATE"
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the Entered Dates to Verify Ascending Order
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT             7 0
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C     XXFDT         CABGT     XXTDT         STRT                 22
     C*
     C* Validate the FUND, if entered.
     C*
     C                   MOVE      'N'           SEPFND            1            SEPARATE FUNDS FLAG
     C                   Z-ADD     0             X
     C                   DO        30            X                 2 0
     C     X             ADD       22            Y                 2 0
     C*
     C     FND(X)        IFNE      0
     C                   MOVE      'Y'           SEPFND
     C*
     C                   Z-ADD     FND(X)        XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END                                                    END '1' *IN60
     C*
     C                   END                                                    END NE 0
     C*
     C                   END                                                    END DO 30 X
     C*
     C     LDA           IFNE      *ZEROS
     C                   OUT       LDA
     C                   ENDIF                                                              0
     C*
     C* Call the CLP to Submit the Program for creating tape workfiles
     C* If user does not enter a fund then submit program
     C* for each valid fund within CO#
     C*
     C     SEPFND        IFEQ      'N'
     C     FNKEY1        CHAIN     PFN01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      FNFND         QQFND             3
     C                   MOVE      SCFRDT        QQFDT             8
     C                   MOVE      SCTODT        QQTDT             8
     C                   MOVE      XXDAT         QQJDT             3
     C                   MOVE      *BLANKS       QQOUTQ           10
     C                   MOVE      ' '           QQMODE            1
     C     SCPROC        IFEQ      'B'
     C     SCPROC        OREQ      'P'
     C                   MOVE      'P'           QQPROC            1
     C                   CALL      'F9605CTX'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQJDT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    QQPROC
     C                   END
     C     SCPROC        IFEQ      'B'
     C     SCPROC        OREQ      'H'
     C                   MOVE      'H'           QQPROC            1
     C                   CALL      'F9605CTX'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQJDT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    QQPROC
     C                   END
     C     FNKEY1        READE     PFN01                                  60
     C                   END                                                    END DOWEQ *IN60
     C*
     C                   ELSE                                                   ELSE SEPFND = N
     C*
     C                   Z-ADD     0             X
     C                   DO        30            X
     C     FND(X)        IFNE      0
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      FND(X)        QQFND             3
     C                   MOVE      SCFRDT        QQFDT             8
     C                   MOVE      SCTODT        QQTDT             8
     C                   MOVE      XXDAT         QQJDT             3
     C                   MOVE      *BLANKS       QQOUTQ           10
     C                   MOVE      ' '           QQMODE            1
     C     SCPROC        IFEQ      'B'
     C     SCPROC        OREQ      'H'
     C                   MOVE      'H'           QQPROC            1
     C                   CALL      'F9605CTX'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQJDT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    QQPROC
     C                   END
     C     SCPROC        IFEQ      'B'
     C     SCPROC        OREQ      'P'
     C                   MOVE      'P'           QQPROC            1
     C                   CALL      'F9605CTX'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQJDT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    QQPROC
     C                   END
     C                   END                                                    END FND,X NE 0
     C                   END                                                    END DO 30 X
     C*
     C                   END                                                    SEPFND = N
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
