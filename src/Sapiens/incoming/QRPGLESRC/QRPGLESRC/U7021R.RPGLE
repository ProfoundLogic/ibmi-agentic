     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U7021R - C/R EDIT CODE REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROMPT PROGRAM FOR C/R EDIT REPORT                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    UTILIZATION REVIEW MENU                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    F0545R         SUBSCRIBER SEARCH                           *
     H*    F0508R         GROUP SEARCH                                *
     H*    U0520R         EDIT CODE SEARCH                            *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 10/07/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/15/98 C:91  Project: MC181                                *
     H*                 COMBINED F6 & F7 KEYS INTO ONE F10 SEARCH KEY *
     H*                 SWAP F0545R TO F0055R ON SUBSCRIBER SEARCH    *
     H*                 SWAP F0508R TO F0060R ON GROUP SEARCH         *
     H*                 DELETED CHG USER PARMS                        *
     H*                 CHANGED SCREEN DESIGN TO CURRENT STANDARDS    *
     H*                                                               *
     H*  07/08/93   C:20     PROJ: URMNT                              *
     H*                      1. SEND MESSAGE IF EDIT CODE IS INACTIVE *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H*  20        POSIT CURSOR @ SUB# IF NO SELECTION FROM SEARCH    *
     H*  21        POSIT CURSOR @ GROUP IF SELECTION FROM SUB# SEARCH *
     H*            POSIT CURSOR @ GROUP IF NO SELECTION FROM SEARCH   *
     H*  22        POSIT CURSOR @ EDIT CODE IF SELECTION FROM GRP SRC *
     H*            POSIT CURSOR @ EDIT CODE IF NO SELECTION FROM SRCH *
     H*  25        SUBSCRIBER NUMBER EQUALS BLANKS                    *
     H*  26        GROUP NUMBER EQUALS BLANKS                         *
     H*  27        EDIT CODE EQUALS BLANKS                            *
     H*  28        GROUP AND EDIT CODE MUST BE BLANK                  *
     H*  29        EDIT CODE MUST BE BLANK                            *
     H*  30        INVALID SUBSCRIBER NUMBER                          *
     H*  31        INVALID GROUP NUMBER                               *
     H*  32        INVALID EDIT CODE                                  *
     H*  33        MC40004 - INVALID FIELD FOR SEARCH                 *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FU7021F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FUMCRL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 05/06/98 ************************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** END   ADD **** 05/06/98 ************************************

     D*
      **** BEGIN DEL **** 05/15/98 ************************************
     D**********          CK1A    1   3 78               CMD KEYS FOR SCR01
      **** END   DEL **** 05/15/98 ************************************
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
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR THE RETOSPECTIVE/CONCURRENT EDIT FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEDIT
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
     C                   MOVE      *ZERO         UPSUB#
     C                   ELSE
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   END
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
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 05/15/98 ************************************
     C**********           MOVE Q1USER    SCUSER
      **** END   DEL **** 05/15/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN22
      **** BEGIN DEL **** 05/15/98 ************************************
     C**********           Z-ADD1         CK
     C**********           MOVELCK1A,CK   SCCMD1
      **** END   DEL **** 05/15/98 ************************************
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
     C****************
     C* F3 = EXIT    *
     C****************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
      **** BEGIN CHG **** 05/15/98 ************************************
     C**********
     C*   F6   *
     C**********
     C********** *INKF     IFEQ '1'
     C**********
     C* SUB    *
     C**********
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C**********           MOVE '1'       *IN20
     C**********           GOTO STRT
     C**********           END                             END EQUAL B
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN21
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *
     C**********           END                             END *INKF
     C**********
     C*  F7    *
     C**********
     C********** *INKG     IFEQ '1'
     C**********
     C* GROUP  *
     C**********
     C**********
     C********** SCSUB#    IFEQ *ZEROS
     C**********           MOVE UPSUB#    QQSUB   50
     C**********           ELSE
     C**********           MOVE SCSUB#    QQSUB
     C**********           END
     C**********           CALL 'F0508R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQSUB
     C**********           PARM           SCFND
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C**********           MOVE '1'       *IN21
     C**********           GOTO STRT
     C**********           END                             END EQUAL B
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QQSUB     SCSUB#
     C**********           MOVE '1'       *IN22
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *
     C**********
     C**********           END                             END *INKG
     C**********
     C*  F8    *
     C**********
     C********** *INKH     IFEQ '1'
     C*****************************************************************
     C*
     C     *INKJ         IFEQ      *ON
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C*
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     20     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN21                          PC AT SCFND
     C                   ELSE
     C                   MOVE      *ON           *IN20                          PC AT SCSUB#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB#
     C*
      ****************
      * GROUP SEARCH *
      ****************
     C     HFLD          IFEQ      'SCFND'
     C*
     C                   MOVE      ##RRN         ##RRN2
     C*
     C                   CALL      'F0060R'
     C*
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN2            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     21     PC AT SCFND
     C*
     C     ##RRN2        CHAIN     PFN02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   MOVE      *ON           *IN22                          PC AT SCEMP#
     C                   ELSE
     C                   MOVE      *ON           *IN21                          PC AT SCFND
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
     C***************
     C* EDIT SEARCH *
     C***************
     C     HFLD          IFEQ      'SCEDIT'
     C*
     C                   MOVE      UPCO#         Q1CO#
     C                   MOVE      *ZEROS        Q1SUB#
     C                   MOVE      *ZEROS        Q1FND
     C                   MOVE      *BLANKS       Q1EDIT
     C                   MOVE      *ZEROS        Q1EXT
     C*
     C                   CALL      'U0520R'
     C*
     C                   PARM                    QQPRDS
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C                   PARM                    Q1EDIT            5
     C                   PARM                    Q1EXT             3 0
     C                   PARM                    QQRTCD            1
     C*
     C                   MOVE      Q1SUB#        SCSUB#
     C                   MOVE      Q1FND         SCFND
     C                   MOVE      Q1EDIT        SCEDIT
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN22
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EQUAL B
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ *
     C*
     C                   ENDIF                                                  END SCEDIT
     C*
     C                   ENDIF                                                  END *INKJ
     C                   ENDIF                                                  END *IN11
     C*
     C*****************************************************************
     C**********
     C* MORE   *
     C**********
     C********** *INKY     IFEQ '1'
     C**********           MOVELQ2RECF    HLRECF 10
     C********** CK        ADD  1         CK      50
     C**********
     C********** HLRECF    IFEQ 'SCR01'
     C********** CK        IFGT 2
     C**********           Z-ADD1         CK
     C**********           END                             END CK GT 2
     C**********           MOVELCK1A,CK   SCCMD1
     C**********           END                             END SCR01
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVELHLRECF    Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *INKY
     C**********
     C* PARMS  *
     C**********
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'F0006R'
     C**********
     C**********   GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           ELSE
     C**********           MOVE UPSUB#    SCSUB#
     C**********           MOVE UPFND     SCFND
     C**********           END
     C**********           GOTO STRT01
     C**********           END
     C**********
     C**********           END
      **** END   CHG **** 05/15/98 ************************************
     C*
     C***************************
     C*   PROCESS REQUEST SCREEN*
     C***************************
     C     SCR01T        TAG
     C                   MOVEA     '000'         *IN(25)
     C*
     C*  DETERMINE WHAT WAS ENTERED
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN25
     C                   END
     C     SCFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN26
     C                   END
     C     SCEDIT        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN27
     C                   END
     C*
     C*   VALIDATE ENTRYS KEYED IN CORRECT ORDER
     C*
     C*   IF SUBSCRIBER IS BLANK, GROUP AND EDIT CODE MUST BE BLANK
     C     *IN25         IFEQ      '1'
     C     *IN26         ANDEQ     '0'
     C     *IN25         OREQ      '1'
     C     *IN27         ANDEQ     '0'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   IF GROUP IS BLANK, THEN EDIT CODE MUST BE BLANK
     C     *IN26         IFEQ      '1'
     C     *IN27         ANDEQ     '0'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE SUBSCRIBER
     C*
     C     *IN25         IFEQ      '0'
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN30                          INVALID SUB#
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C                   END                                                    END IF *IN25
     C*
     C*  VALIDATE THE GROUP
     C*
     C     *IN26         IFEQ      '0'
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN31                          INVALID FUND
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C                   END                                                    END IF *IN26
     C*
     C*   VALIDATE THE EDIT CODE
     C*
     C     *IN27         IFEQ      '0'
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   ELSE
     C     CRSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END                                                    END CRSTAT
     C                   END                                                    END IF *IN60
     C                   END                                                    END IF *IN27
     C*
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCEDIT        QQEDIT            5
     C                   CALL      'U7021C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQEDIT
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      **** BEGIN DEL **** 05/15/98 ************************************
      ******  CK1A - COMMAND KEYS for SCR01
      ****** F3=Exit F6=Subscriber Search F7=Group Search F24=More keys
      ****** F8=Edit Code Search F13=Chg User Parms F24=More keys
      **** END   DEL **** 05/15/98 ************************************
