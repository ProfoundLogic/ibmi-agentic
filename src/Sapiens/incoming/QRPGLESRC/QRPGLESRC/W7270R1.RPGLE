     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7270R1 - PROMPT FOR LOSS RATIO BY JOB CLASS W/ WORKFILE
     H*
     H****************************************************************
     H* Description:  Workfile for the Loss Ratio by Job Class       *
     H*               Report.                                        *
     H*                                                              *
     H* Created: 06/24/04    Programmer: C:15    Project: WC1063     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FW7270F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDS)
     FWDF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D*
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D*
     D OPTSEL          C                   CONST(', if selected.')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    F1FYR         XXFYR
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
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCASOF
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         SCFND
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
     C*---------------------------------
     C*  Process Function Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  F3 = Exit
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F10 = Search
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C*     Group search
     C*
     C     HFLD          WHENEQ    'SCFND'
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM      1             ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##FND         IFNE      *BLANKS
     C                   MOVE      '1'           *IN31
     C                   ENDIF
     C*
     C*     Group year search
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C*
     C     SCFND         IFNE      0
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   CALL      'W0011R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      FNRRN         ##RRN             9 0
     C                   PARM      *ZERO         ##YR              3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##YR          IFEQ      *ZERO
     C     *INKL         OREQ      *ON
     C                   SETON                                        31
     C                   ELSE
     C                   Z-ADD     ##YR          SCFYR
     C                   SETON                                        32
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*   F13 = Change User Parms
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C     UPKY          CHAIN     PUP01                              60
     C                   MOVE      UPCO#         XXCO#
     C*
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*-----------------------------------
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE GROUP
     C*
     C     SCFND         IFNE      0
     C                   MOVE      SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE FUND YEAR FOR THE GROUP
     C*
     C     SCFND         IFNE      0
     C     SCFYR         ANDNE     0
     C                   MOVE      SCFYR         XXFYR
     C     F1KY          CHAIN     PF101                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED RUN DATE
     C*
     C                   Z-ADD     SCASOF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCASOF
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE OUTPUT TYPE IS P/F/B
     C*
     C     SCTYPO        IFNE      'P'
     C     SCTYPO        ANDNE     'F'
     C     SCTYPO        ANDNE     'B'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTYPO<>P/F/
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCASOF        QQASOF
     C                   CALL      'W7270C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQASOF            8
     C                   PARM      SCTYPO        QQTYPO            1
     C                   PARM                    UPOUTQ           10
     C                   PARM      'D'           MODE              1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
