     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7127R - PBR/HBA BILLING PRINT ONLY - PROMPT PROGRAM
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/21/02 C:62 PROJECT:AR033                                  *
     H*                -ADD PARM TO N7127C TO PASS INVOICE TYPE       *
     H*                -ADD LOGIC TO HANDLE INVOICE TYPE.             *
     H*                                                               *
     H*****************************************************************
     FN7127F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 01/21/02 **************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     F*** END   ADD *** 01/21/02 **************************************

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
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQMODE            1
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
     C                   MOVE      XXSUB#        SCSUB#
     C*
     C*  GET THE COMPANY RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
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
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'                                          ADD 11/18/92
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN (SCREEN 1)
     C*
     C     *INKB         IFEQ      '1'                                          ADD 11/18/92
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*******************************
     C*   F-10=SEARCH               *
     C*******************************
     C*
     C     *INKJ         IFEQ      '1'                                          ADD 11/18/92
     C     Q2RECF        IFEQ      'SCR01'
     C*** BEGIN CHG *** 01/21/02 **************************************
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           Q1SUB#  50
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** Q1SUB#    OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C*
     C********** QQRTC     IFEQ '*'
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE Q1SUB#    SCSUB#
     C**********           GOTO STRT
     C**********           END
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   SETON                                        31
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   SETON                                        32
     C                   ENDIF
     C*** END   CHG *** 01/21/02 **************************************
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
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
     C*
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
     C                   CALL      'M0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** SUBSCRIBER NUMBER CANNOT BE BLANKS
     C     SCSUB#        CABEQ     *ZERO         STRT                     20     NEED A SUB#
     C                   MOVE      SCSUB#        XXSUB#
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                           INVLD SUB#
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD *** 01/21/02 **************************************
     C* VALID INVOICE TYPE MUST BE ENTERED
     C*
     C     SCINVT        IFNE      'B'
     C     SCINVT        ANDNE     'N'
     C     SCINVT        ANDNE     ' '
     C                   SETON                                        22
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 01/21/02 **************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE STATEMENT.
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C*
     C                   CALL      'N7127C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    UPOUTQ
     C                   PARM                    MODE              1
     C                   PARM                    QQMODE
     C*** BEGIN ADD *** 01/21/02 **************************************
     C                   PARM      SCINVT        QQINVT            1
     C*** END   ADD *** 01/21/02 **************************************
     C*****************************************************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
