     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7911R - PARTICIPATING PROVIDER NETWORK REPORT - SCRN PGM
     H*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H* SCR01                                                         *
     H*                                                               *
     H*  26 - POSITION CURSOR AT SUBCRIBER NUMBER                     *
     H*  27 - POSITION CURSOR AT GROUP NUMBER                         *
     H*  28 - POSITION CURSOR AT FROM DATE                            *
     H*  29 - ERROR MESSAGE FOR INVALID FUNCTION KEY FOR SEARCH       *
     H*****************************************************************
     H*                                                               *
      *****************************************************************
     H*                 R E V I S I O N S                             *
     H*                                                               *
     H* 1/08/90  LJD - MADE FROM/TO DATES REQUIRED ON PROMPT SCREEN   *
     H*                                                               *
     H*  04/29/98 C:91  PROJ: MC181                                   *
     H*                 COMBINED F6 & F7 KEYS INTO ONE F10 KEY        *
     H*                 ADDED GROUP SEARCH TO F10 KEY                 *
     H*                 CHANGED SCREEN DESIGN TO CURRENT STANDARDS    *
     H*                                                               *
      ****************************************************************
     FN7911F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 04/29/98 ***********************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
      **** END   ADD **** 04/29/98 ***********************************

     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
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
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C*
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
     C                   Z-ADD     0             SCTDT
     C                   Z-ADD     0             SCFDT
     C                   MOVE      *BLANKS       SCRTYP
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
     C     *IN11         IFEQ      '1'
     C********************************
     C*   COMMAND 03 = EXIT          *
     C********************************
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
      **** BEGIN CHG **** 04/29/98 ************************************
     C********************************
     C*   COMMAND 06 = SUBSCRIBER SEARCH
     C********************************
     C**********
     C********** *INKF     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#  50
     C**********           PARM           QQRTC   1
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTC     CABEQ'*'       STRT
     C**********           END
     C**********           GOTO STRT
     C**********           END                             *INKF
     C**********
     C********************************
     C*   COMMAND 07 = GROUP SEARCH
     C********************************
     C**********
     C********** *INKG     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'F0508R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           SCFND
     C**********           PARM           QQRTC   1
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTC     CABEQ'*'       STRT
     C**********           END
     C**********
     C**********           END                             *INKG
     C**********
     C*
     C************************
     C*   F10 = SEARCH       *
     C************************
     C*
     C     *INKJ         IFEQ      *ON
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     26     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN27                          PC AT SCFND
     C                   ELSE
     C                   MOVE      *ON           *IN26                          PC AT SCSUB#
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
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN2            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     27     PC AT SCFND
     C*
     C     ##RRN2        CHAIN     PFN02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   MOVE      *ON           *IN28                          PC AT SCFRDT
     C                   ELSE
     C                   MOVE      *ON           *IN27                          PC AT SCFRDT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ IFEQ ON
      **** END   CHG **** 04/29/98 ************************************
     C********************************
     C*   PROCESS HELP REQUESTS
     C********************************
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
      **** BEGIN DEL **** 04/29/98 ************************************
     C********************************
     C*   PROCESS CHANGE OF USER PARAMETERS
     C********************************
     C**********
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'F0006R'
     C**********
     C*  GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           MOVE *ZERO     UPFND
     C**********           END
     C**********           MOVE UPSUB#    SCSUB#
     C**********           MOVE UPFND     SCFND
     C**********
     C* GET SUBSCRIBER NAME
     C**********
     C**********           GOTO STRT01
     C**********           END
      **** END   DEL **** 04/29/98 ************************************
     C*
     C                   END                                                    *IN11
     C**********END OF COMMAND KEY SECTION ***************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* SUBSCRIBER NUMBER MUST BE VALID, IF NOT BLANK
     C*
     C     SCSUB#        IFNE      *ZEROS
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20                           INVLD SB#
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* GROUP MUST BE VALID, IF NOT BLANK
     C*
     C     SCFND         IFNE      *ZEROS
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                          INVLD GRP#
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE THE DATE ENTERED
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     22
     C*
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     23
     C*
     C                   MOVE      SCFDT         SRFRDC            8 0
     C                   MOVE      SCTDT         SRTODC            8 0
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*   THE REPORT TYPE MUST BE 'D', 'S', OR 'B'
     C*
     C     SCRTYP        IFNE      'D'
     C     SCRTYP        ANDNE     'S'
     C     SCRTYP        ANDNE     'B'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFDT         QQFDT             8
     C                   MOVE      SCTDT         QQTDT             8
     C                   MOVE      SCRTYP        QQRTYP            1
     C                   CALL      'N7911C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQRTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
