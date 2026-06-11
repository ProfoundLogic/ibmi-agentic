     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F0766R                                            *
     H*    PROGRAM DESCRIPTION: REQ LTR (REPRINT) - PROMPT PGM        *
     H*****************************************************************
     H** REVISIONS:
     H*  09/21/99 C:31  Project: MC212
     H*                 Rollup of MC PGMR_DEV
     H*                 1. Correct searches
     H*
     H*  07/22/94 C:11  PROJECT WC113
     H*               1.CHANGE QQFLAG PASED TO F0766C FROM 'I' TO 'R'.
     H*               2.TAKE OFF EDIT FOR STATUS (*IN23).
     H*
     H*  07/22/92 KRR       -ADDED SUB# TO ALL PROCESSING
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H* 20  -  INVALID FUND                                           *
     H* 21  -  INVALID CASE                                           *
     H* 22  -  INVALID LETTER #                                       *
     H* 23  -  NON-PENDED LETTER                                      *
     H* 24  -  POSITION CURSOR AT FUND                                *
     H* 25  -  POSITION CURSOR AT FUND YEAR                           *
     H* 26  -  POSITION CURSOR AT LETTER #                            *
     H* 27  -  POSITION CURSOR AT SUBSCRIBER                          *
     H* 28  -  INVALID SUBSCRIBER NUMBER                              *
     H*****************************************************************
     FF0766F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFALTL1    IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 09/21/99 **************************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     F*** END ADD *** 09/21/99 ****************************************

     D*
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
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR THE CASE MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*   KEY LIST FOR THE REQUEST LETTER FILE
     C*
     C     LTKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCLTR#
     C*
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPSUB#        SCSUB#
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCLTR#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN25
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   COMMAND 03 = END OF JOB      *
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C*** BEGIN DEL *** 09/21/99 **************************************
     C****************************************
     C****COMMAND 06 = SUBSCRIBER  SEARCH    *
     C****************************************
     C********   *INKF     IFEQ '1'
     C********   Q2RECF    IFEQ 'SCR01'
     C********             CALL 'F0545R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCSUB#
     C********             PARM           QQRTC   1
     C********   QQRTC     CABEQ'A'       TERM
     C********
     C********   QQRTC     IFEQ 'B'
     C********   SCSUB#    OREQ *ZERO
     C********             WRITESCR99
     C********             MOVE *BLANK    Q2RECF
     C********             MOVEL'SCR01'   Q2RECF
     C********             MOVE '1'       *IN27
     C********             GOTO STRT
     C********             END                             END EQUAL B
     C********
     C********   QQRTC     IFEQ '*'
     C********             MOVE '1'       *IN24
     C********             GOTO STRT
     C********             END
     C********
     C********             END                             END SCR01
     C********             END                             END *INKF
     C**********************************
     C****COMMAND 07 = FUND  SEARCH    *
     C**********************************
     C********   *INKG     IFEQ '1'
     C********
     C********   SCSUB#    IFEQ *ZEROS
     C********             MOVE UPSUB#    SCSUB#
     C********             END
     C********             Z-ADDSCSUB#    X1SUB#  50
     C********             Z-ADDSCFND     X1FND   30
     C********             CALL 'F0508R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           X1SUB#
     C********             PARM           X1FND
     C********             PARM           QQRTCD
     C********   QQRTCD    CABEQ'A'       TERM
     C********
     C********             WRITESCR99
     C********             MOVE *BLANK    Q2RECF
     C********             MOVEL'SCR01'   Q2RECF
     C********
     C********   QQRTCD    IFEQ 'B'
     C********   X1FND     OREQ *ZERO
     C********             MOVE '1'       *IN24            POS CUR FUND
     C********             GOTO STRT
     C********             END
     C********
     C********   QQRTCD    IFEQ '*'
     C********             MOVE '1'       *IN25            POS CUR FYR
     C********             MOVE X1SUB#    SCSUB#
     C********             MOVE X1FND     SCFND
     C********             GOTO STRT
     C********             END
     C********
     C********             END                             ENDIF *INKG
     C**********************************
     C****COMMAND 08 = CASE # SEARCH   *
     C**********************************
     C********   *INKH     IFEQ '1'
     C********   SCFND     IFEQ *ZEROS
     C********             MOVE UPFND     SCFND
     C********             END
     C********             Z-ADDSCFND     X1FND
     C********             CALL 'F0570R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCFYR
     C********             PARM           SCCASE
     C********             PARM           XXSS#   9
     C********             PARM           QQRTCD  1
     C********             PARM           X1FND
     C********             MOVE X1FND     SCFND
     C********
     C********   QQRTCD    CABEQ'A'       TERM
     C********
     C********             WRITESCR99
     C********             MOVE *BLANK    Q2RECF
     C********             MOVEL'SCR01'   Q2RECF
     C********
     C********   QQRTCD    IFEQ 'B'
     C********   SCFYR     OREQ *ZERO
     C********             MOVE '1'       *IN25            POS CUR FYR
     C********             GOTO STRT
     C********             END                             END IF QQRTC
     C********
     C********   QQRTCD    IFEQ '*'
     C********             MOVE '1'       *IN26            POS CUR LTR
     C********             GOTO STRT
     C********             END
     C********
     C********             END                             END IF *INKH
     C***************************************
     C***COMMAND 09 - REQUEST LETTER SEARCH *
     C***************************************
     C********   *INKI     IFEQ '1'
     C********
     C********             MOVE SCSUB#    QQSUB#  5
     C********             MOVE SCFND     QQFND   3
     C********             MOVE SCFYR     QQFYR   3
     C********             MOVE SCCASE    QQCASE  7
     C********             MOVE SCLTR#    QQLTR#  9
     C********             CALL 'F0775R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           QQSUB#
     C********             PARM           QQFND
     C********             PARM           QQFYR
     C********             PARM           QQCASE
     C********             PARM           QQLTR#
     C********             PARM           QQRTCD
     C********   QQRTCD    CABEQ'A'       TERM
     C********
     C********             WRITESCR99
     C********             MOVE *BLANK    Q2RECF
     C********             MOVEL'SCR01'   Q2RECF
     C********
     C********   QQRTCD    IFEQ 'B'
     C********   QQLTR#    OREQ *ZERO
     C********             MOVE '1'       *IN26            POS CUR LTR
     C********             GOTO STRT
     C********             END                             ENDIF 'B'
     C********
     C********   QQRTCD    IFEQ '*'
     C********             MOVE QQSUB#    SCSUB#
     C********             MOVE QQFND     SCFND
     C********             MOVE QQFYR     SCFYR
     C********             MOVE QQCASE    SCCASE
     C********             MOVE QQLTR#    SCLTR#
     C********             GOTO STRT
     C********             END                             ENDIF '*'
     C********
     C********
     C********             END                             ENDIF *INKI
     C*** END DEL *** 09/21/99 ****************************************
     C*** BEGIN ADD *** 09/21/99 **************************************
     C     *INKJ         IFEQ      '1'
     C                   DO
     C*
     C*          Subscriber search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   SETON                                        LR
     C                   LEAVE
     C                   ENDIF                                                  *INKC = On
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     ##RRN         CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   SETON                                        24
     C                   ELSE                                                   *IN60 = Off
     C                   SETON                                        27
     C                   ENDIF                                                  *IN60 = Off
     C                   LEAVE
     C                   ENDIF                                                  HFLD = SCSUB#
     C*
     C*          Group search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCFND'
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C     *INKC         IFEQ      *ON
     C                   SETON                                        LR
     C                   LEAVE
     C                   ENDIF                                                  *INKC = On
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     ##RRN         CHAIN     PFN02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   SETON                                        25
     C                   ELSE                                                   *IN60 = Off
     C                   SETON                                        24
     C                   ENDIF                                                  *IN60 = Off
     C                   LEAVE
     C                   ENDIF                                                  HFLD = SCFND
     C*
     C*          Case search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCCASE'
     C                   CALL      'F0269R'
     C                   PARM      '2'           Q#CALL            1
     C                   PARM                    Q#SUB#            5 0
     C                   PARM                    Q#FND             3 0
     C                   PARM                    Q#FYR             3 0
     C                   PARM      0             Q#CASE            7 0
     C                   PARM                    Q#SS#             9
     C                   PARM                    Q#ACDT            8 0
     C                   PARM                    Q#RTCD            1
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     Q#RTCD        IFEQ      '*'                                          RTCD = '*'
     C                   SETON                                        26
     C                   MOVE      Q#SUB#        SCSUB#
     C                   MOVE      Q#FND         SCFND
     C                   MOVE      Q#FYR         SCFYR
     C                   MOVE      Q#CASE        SCCASE
     C                   ELSE                                                   RTCD = '*'
     C                   SETON                                        25
     C                   ENDIF                                                  RTCD = '*'
     C                   LEAVE
     C                   ENDIF                                                  HFLD = SCSS#
     C*
     C*          Request Letter Search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCLTR#'
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      SCLTR#        QQLTR#            9
     C                   CALL      'F0775R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQLTR#
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQLTR#        OREQ      *ZERO
     C                   MOVE      '1'           *IN26                          POS CUR LTR
     C                   ENDIF                                                  RTCD = 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQLTR#        SCLTR#
     C                   ENDIF                                                  TRCD = '*'
     C                   ENDIF                                                  HFLD = 'SCLTR#'
     C                   ENDDO                                                  DO Loop
     C   LR              GOTO      TERM
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKJ = On
     C*** END ADD *** 09/21/99 ****************************************
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
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
     C                   END                                                    ENDIF *IN10
     C**********************************
     C*   COMMAND 13 - CHG USER PAMRS  *
     C**********************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    ENDIF *INKM
     C*
     C                   END                                                    END CMD KEYS
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE SUBSCRIBER
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN28                          INVALID SUB#
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*  VALIDATE THE GROUP
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20                          INVALID FUND
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*   VALIDATE THE CASE
     C*
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                          INVALID CASE
     C                   GOTO      STRT
     C                   END                                                    ENDIF *IN60
     C*
     C*   VALIDATE THE LETTER #
     C*
     C     LTKEY         CHAIN     PLT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22                          INVALID LTR#
     C                   GOTO      STRT
     C                   ELSE                                                   ELSIF *IN60
     C                   END                                                    ENDIF *IN60
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      SCLTR#        QQLTR#            9
     C                   CALL      'F0766C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQLTR#
     C                   PARM      'R'           QQFLAG            1
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
