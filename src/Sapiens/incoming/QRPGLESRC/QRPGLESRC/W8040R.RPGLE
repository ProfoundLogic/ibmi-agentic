     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W8040R - PROMPT FOR BULK RERATING PROGRAM
     H*****************************************************************
     H*  PROGRAMMER: C65                                              *
     H*  CREATED:    07/16/99                                         *
     H*  PROJECT#:   WC850                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/19/24  C:291 Project: WC2961                              *
     H*                  Add popup to display message about Bulk      *
     H*                  Rating Process not supported.                *
     H*                                                               *
     H*  12/09/99  C:65  PROJECT:  WC850                              *
     H*                  ALLOW A ZERO PERCENTAGE SINCE A STATE MAY    *
     H*                  NOT BE PASSING A SINGLE PERCENTAGE RATE CHG  *
     H*                                                               *
     H*****************************************************************
      *
     FW8040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMRSL     IF   E           K DISK

     D*
     D OPTNM           C                   CONST('Bulk Re-Rating')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      //*** Beg Add ***** 04/19/24 **************************************
      /copy sPrGenPop
      //*** End Add ***** 04/19/24 **************************************
      *****************************************************************
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
      *
      *  RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCRTCD
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
      *
      *****************************************************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        SCPRDS
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
      //*** Beg Chg ***** 04/19/24 **************************************
     C***                EXFMT     SCR01
        Write SCR01;
        PopOneBtn('Bulk Re-Rating Process is no longer supported. +
         Will develop this if clients request.':'Information':'Ok');
     C                   Goto      TERM
      //*** End Chg ***** 04/19/24 **************************************
     C                   ENDIF                                                  Q2RECF IFEQ
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
      *   COMMAND 03 = END OF JOB
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKA IFEQ
     C*
     C*  CMD 10 - TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     HFLD          IFEQ      'SCBCM1'
     C     HFLD          OREQ      'SCBCM2'
     C     HFLD          OREQ      'SCBCM3'
     C     HFLD          OREQ      'SCICM1'
     C     HFLD          OREQ      'SCICM2'
     C     HFLD          OREQ      'SCICM3'
     C*
     C                   MOVEL(P)  *BLANKS       QQKEY
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY            10
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBCM1'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        51
     C                   ELSE
     C                   SETON                                        52
     C                   MOVEL     QQKEY         SCBCM1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCBCM2'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        52
     C                   ELSE
     C                   MOVEL     QQKEY         SCBCM2
     C                   SETON                                        53
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCBCM3'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        53
     C                   ELSE
     C                   MOVEL     QQKEY         SCBCM3
     C                   SETON                                        54
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCICM1'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        54
     C                   ELSE
     C                   SETON                                        55
     C                   MOVEL     QQKEY         SCICM1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCICM2'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        55
     C                   ELSE
     C                   MOVEL     QQKEY         SCICM2
     C                   SETON                                        56
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCICM3'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        56
     C                   ELSE
     C                   MOVEL     QQKEY         SCICM3
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C     HFLD          IFEQ      'SCRTCD'
      *
     C                   CALL      'W2471R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@KEY            10
      *
     C     @@KEY         IFNE      *BLANKS
     C                   MOVE      *ON           *IN50
     C                   MOVEL     @@KEY         SCRTCD
     C                   ENDIF                                                  @@KEY IFNE  KS
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ IFEQ
     C*
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      *ON
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
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM IFEQ
      *
     C                   ENDIF                                                  *IN11 IFEQ
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
     C*
     C* VALIDATE RATE SET CODE
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* VALIDATE PERCENTAGE
     C*
     C*** BEGIN CHG ***  12/09/99  ************************************
     C*******    SCPCT     IFEQ *ZERO
     C     SCPCT         IFLT      *ZERO
     C*** END CHG *****  12/09/99  ************************************
     C                   MOVE      *ON           *IN57
     C                   GOTO      STRT
     C                   ENDIF
      *
      * VALIDATE EFFECTIVE DATE
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
     C*
     C*  VALIDATE THE BILLING COMMENTS
     C*
     C     SCBCM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBCM1        XXKEY
     C     TBKY          CHAIN     PBC01                              42
     C  N42BCSTAT        COMP      'I'                                    42
     C     *IN42         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCBCM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBCM2        XXKEY
     C     TBKY          CHAIN     PBC01                              43
     C  N43BCSTAT        COMP      'I'                                    43
     C     *IN43         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCBCM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBCM3        XXKEY
     C     TBKY          CHAIN     PBC01                              44
     C  N44BCSTAT        COMP      'I'                                    44
     C     *IN44         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE INSTALLMENT COMMENTS
     C*
     C     SCICM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCICM1        XXKEY
     C     TBKY          CHAIN     PBC01                              45
     C  N45BCSTAT        COMP      'I'                                    45
     C     *IN45         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCICM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCICM2        XXKEY
     C     TBKY          CHAIN     PBC01                              46
     C  N46BCSTAT        COMP      'I'                                    46
     C     *IN46         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCICM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCICM3        XXKEY
     C     TBKY          CHAIN     PBC01                              47
     C  N47BCSTAT        COMP      'I'                                    47
     C     *IN47         CABEQ     '1'           STRT
     C                   END
      *
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPOUTQ        QQQUE            10
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCRTCD        QQRTCD            3
     C                   MOVE      SCPCT         QQPCT             5
     C                   MOVE      SCEFDT        QQEFDT            8
     C                   MOVE      SCBCM1        QQBCM1            2
     C                   MOVE      SCBCM2        QQBCM2            2
     C                   MOVE      SCBCM3        QQBCM3            2
     C                   MOVE      SCICM1        QQICM1            2
     C                   MOVE      SCICM2        QQICM2            2
     C                   MOVE      SCICM3        QQICM3            2
     C*
     C                   CALL      'W8040C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQRTCD
     C                   PARM                    QQPCT
     C                   PARM                    QQEFDT
     C                   PARM                    QQQUE
     C                   PARM                    QQBCM1
     C                   PARM                    QQBCM2
     C                   PARM                    QQBCM3
     C                   PARM                    QQICM1
     C                   PARM                    QQICM2
     C                   PARM                    QQICM3
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
