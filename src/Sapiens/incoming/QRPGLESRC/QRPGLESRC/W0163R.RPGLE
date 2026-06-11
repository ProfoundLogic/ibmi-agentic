     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0163R - PROMPT FOR PREMIUM GENERATION
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  01/24/93 C:11 PROJECT WC229                                  *
     H*                DON'T DEFAULT THE CANC EFF DATE TO USER'S PARM *
     H*                DATE SINCE IT'S ALWAYS A FUTURE DATE. LEAVE 0. *
     H*                                                               *
     H*  02/16/95 C:11 PROJECT: WC396                                 *
     H*             1. PROMPT FOR "GENERATE CERTIFICATE HOLDER" IF    *
     H*                OV/400 IS INSTALLED.                           *
     H*             2. REMOVE FYR FROM PROMPT. FYR WILL BE CALCULATED.*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H**    20      (WCR0500) - INVALID NOTICE DATE
     H**    21      (WCR0500) - INVALID EFFECTIVE DATE
     H**    22      (WCR0502) - INVALID FUND YEAR
     H**    23      (WCR0501) - INVALID CERT HOLDER FLAG
     H**    99      OV/400 IS INSTALLED (SYOFOF = Y)
     H**    60      GENERAL CHAINING
     H*****************************************************************
     FW0163F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTXCL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSYEAR                11     12  0
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
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C*  SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C     F1KY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXRTCD
     C*
     C*  CANCELLATION/REINSTATEMENT STATE FILE
     C*
     C     XCKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     XCKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXST
     C*
     C     *LIKE         DEFINE    XCST          XXST
     C     *LIKE         DEFINE    XCEFDT        XXEFDT
     C     *LIKE         DEFINE    F1RTCD        XXRTCD
     C     *LIKE         DEFINE    F1FYR         XXFYR
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
     C*   PULL "OV/400 INSTALLED?" FLAG FROM APPLICATION CONTROL FILE.
     C*
     C                   MOVEL     'SY'          ##KEY
     C                   MOVE      *BLANK        SYOFIF
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C     SYOFIF        ANDEQ     'Y'
     C                   MOVE      '1'           *IN99
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   MOVE      'N'           SCCCRT
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C                   Z-ADD     *ZERO         SCCDAT
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
     C                   MOVE      Q1USER        SCUSER
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
     C                   WRITE     SCR99
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
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   MOVE      *ZERO         SCCDAT
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* IF THE CANCEL NOTICE DATE IS ZERO, THEN DEFAULT.
     C*
     C     SCDATE        IFEQ      *ZERO
     C                   Z-ADD     DSYEAR        XXFYR
     C     F1KY          SETGT     PF101
     C     F1KY2         READPE    PF101                                  60
     C                   MOVE      F1RTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     XCKY          SETGT     PXC01
     C     XCKY2         READPE    PXC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DSDATE        XXEFDT
     C                   Z-ADD     XCNOTC        QQDAYS            5 0
     C                   CALL      'SRFUTR'
     C                   PARM                    XXEFDT
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR             1
     C     QQERR         IFNE      '1'
     C                   MOVE      XXEFDT        SCDATE
     C                   ENDIF                                                  QQERR IFNE
     C*
     C     XCADDM        IFNE      *ZERO
     C                   Z-ADD     XCADDM        QQDAYS            5 0
     C                   CALL      'SRFUTR'
     C                   PARM                    XXEFDT
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR             1
     C     QQERR         IFNE      '1'
     C                   MOVE      XXEFDT        SCDATE
     C                   ENDIF                                                  QQERR IFNE
     C                   ENDIF                                                  XCADDM IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDATE IFEQ
     C*
     C*
     C*  VALIDATE THE CANCEL NOTICE DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE EFFECTIVE DATE
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  GENERATE CERTIFICATE HOLDERS LETTERS MUST BE 'Y' OR 'N'
     C*
     C     SYOFIF        IFEQ      'Y'
     C     SCCCRT        ANDNE     'Y'
     C     SCCCRT        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPFND         QQFND             3
     C                   MOVE      SCDATE        QQDATE            8
     C                   MOVE      SCCDAT        QQCDAT            8
     C                   MOVE      SCCCRT        QQCCRT            1
     C                   MOVE      *BLANKS       QQOUTQ           10
     C                   MOVEL     UPOUTQ        QQOUTQ
     C                   CALL      'W0163C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQDATE
     C                   PARM                    QQCDAT
     C                   PARM                    QQCCRT
     C                   PARM                    QQOUTQ
     C                   PARM      *BLANK        QQMODE            1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
