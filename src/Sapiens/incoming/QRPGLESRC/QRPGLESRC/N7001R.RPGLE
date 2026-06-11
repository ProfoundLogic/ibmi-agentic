     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7001R - UNBILLED TIME REPORT - PROMPT PROGRAM
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/28/98  C:91  PROJECT:MC181                                *
     H*                  CHANGED CALLED FILE WITHIN SUBSCRIBER SEARCH *
     H*                  TO F0055R                                    *
     H*                                                               *
     H*  04/23/98  C:89  PROJECT:MC181                                *
     H*                  CHANGED F6 TO F10 FOR SEARCH                 *
     H*                  VERSION 4 STANDARDS                          *
     H*                                                               *
     H*****************************************************************
     FN7001F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 04/28/98 ************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** END   ADD **** 04/28/98 ************************************

     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSYR                  11     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
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
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPSUB#        XXSUB#            5 0
     C*
     C*  GET THE COMPANY RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 04/23/98 ************************************
     C******************** MOVE Q1USER    USER
      **** END   DEL **** 04/23/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*****************************************************************
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
     C*********************************
     C*   COMMAND 03 = END OF JOB     *
     C*********************************
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
     C***********************************
     C*  F10=SEARCH                     *
     C***********************************
     C*
      **** BEGIN CHG **** 04/23/98 ************************************
     C********** *INKF     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#  50
     C******************** PARM           QQRTC   1
     C********************
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C********************
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END
     C********************
     C********** QQRTC     CABEQ'*'       STRT
     C******************** END
     C******************** GOTO STRT
     C******************** END                             *INKF
     C*
     C     *INKJ         IFEQ      '1'
     C*
      **** BEGIN CHG **** 04/28/98 ************************************
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     28     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN27                          PC AT SCFLAG
     C                   ELSE
     C                   MOVE      *ON           *IN28                          PC AT SCSUB#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB#
     C*
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ IFEQ 1
     C*
     C********** HFLD      IFEQ 'SCSUB#'
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QXSUB#  50
     C**********           PARM           QQRTC   1
     C**********
     C**********
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** QXSUB#    OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF                           IFEQ 'B'
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QXSUB#    SCSUB#
     C**********           MOVE *ON       *IN27
     C**********           GOTO STRT
     C**********           ENDIF                           IFEQ '*'
     C**********
     C**********           ENDIF                           IFEQ SCSUB#
      **** END   CHG **** 04/28/98 ************************************
     C*
      **** END   CHG **** 04/23/98 ************************************
     C*
     C***********************************
     C*   PROCESS HELP REQUESTS
     C***********************************
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
      **** BEGIN DEL **** 04/28/98 ************************************
     C***************************************
     C*   PROCESS CHANGE OF USER PARAMETERS *
     C***************************************
     C**********
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'W0006R'
     C**********
     C*  GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           MOVE *ZERO     UPFND
     C**********           END
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO TOP
     C**********           END
     C**********
      **** END   DEL **** 04/28/98 ************************************
     C*
     C                   END                                                    *IN11
     C*
     C********** END OF COMMAND KEY SECTION *************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*****************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C***  SUBSCRIBER NUMBER MUST BE VALID, IF NOT BLANK
     C*
     C     SCSUB#        IFNE      *ZEROS
     C                   Z-ADD     SCSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20                           INVLD SB#
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C***  FLAG MUST EITHER BE 'H', 'R', OR BLANK ...
     C*
     C     SCFLAG        IFNE      *BLANKS
     C     SCFLAG        ANDNE     'H'
     C     SCFLAG        ANDNE     'R'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE THE DATE ENTERED IF DATE RANGE IS REQUESTED
     C*
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ORNE      *ZEROS
     C*
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     22
     C*
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     23
     C*
     C                   MOVE      SCFRDT        SRFRDC            8 0
     C                   MOVE      SCTODT        SRTODC            8 0
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
     C                   END                                                    USE DATE
     C*                                                    RANGE?
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
     C* ALL EDITS HAVE BEEN PASSED -
     C*
     C*
     C*****************************************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE STATEMENT.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCRTYP        QQRTYP            1
     C                   MOVE      SCFLAG        QQFLAG            1
     C                   CALL      'N7001C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQRTYP
     C                   PARM                    QQFLAG
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*****************************************************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
