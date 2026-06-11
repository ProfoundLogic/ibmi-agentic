     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W9200R - PROMPT FOR A/R BATCH EDIT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM PROMPTS THE USER FOR ONE OR ALL BATCH NUMBERS    *
     H* TO RUN AN A/R BATCH EDIT REPORT LISTING DETAIL INFORMATION    *
     H* FOR ONE OR ALL BATCHES THAT HAVE NOT YET BEEN TRANSFERRED.    *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   ACCOUNTING REPORT MENU                                      *
     H* CALLS TO:                                                     *
     H*   M0002C                                                      *
     H*   W0006R                                                      *
     H*   W9200C                                                      *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 07/11/91               PROJECT: MD021                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   20  VALIDATE APPROVED FLAG                                  *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/29/96 C:33 PROJECT WC420                                  *
     H*                IF THIS REPORT IS CALLED ON DEMAND IT SHOULD   *
     H*                BE ROUTED TO THE USER PARM OUTQ INSTEAD OF THE *
     H*                OVERRIDE OUTQ.                                 *
     H*****************************************************************
     FW9200F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDEAL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXBAT#
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EABAT#        XXBAT#
     C     *LIKE         DEFINE    EASTAT        XXSTAT
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
     C     STRT01        TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
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
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 13 = CHANGE USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   VALIDATE THE BATCH NUMBER ENTERED
     C*
     C     SCBAT#        IFNE      *ZEROS
     C*
     C*  CHECK IF BATCH NUMBER EXISTS IN NON-TRANFERRED BATCH ENTRIES
     C*
     C                   MOVE      *BLANKS       XXSTAT
     C                   Z-ADD     SCBAT#        XXBAT#
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFNE      '0'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK IF BATCH NUMBER EXISTS IN TRANFERRED BATCH ENTRIES
     C*
     C                   MOVE      *BLANKS       XXSTAT
     C                   Z-ADD     SCBAT#        XXBAT#
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFNE      '0'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    SCBAT# <> 0
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCBAT#        QQBAT#            9
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W9200C'
     C                   PARM                    QQCO#
     C                   PARM                    QQBAT#
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
