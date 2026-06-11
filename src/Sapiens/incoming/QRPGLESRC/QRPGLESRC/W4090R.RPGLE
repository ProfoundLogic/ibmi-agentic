     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4090R - PROMPT FOR FINANCIAL EARNED PREMIUM REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/14/24  C:300 Project: WC2948                               *
     H*                 This program is no longer in use.             *
     H*                 Pass-through this program to new scree n.     *
     H*                 Add extra entry parm to avoid Pass-through of old screen
     H*                                                               *
     H* 07/01/98  C:35  PROJECT: SUPPORT WA033 12494                  *
     H*                 THE MESSAGE FOR DATE PREVIOUSLY RUN WAS       *
     H*                 NOT ABLE TO BE OVERRIDDEN, BECAUSE THE SAVED  *
     H*                 DATE WAS NEVER EQUAL TO THE SCREEN DATE,      *
     H*                 SINCE THEY WRE NOT THE SAME SIZE.             *
     H*                                                               *
     H*****************************************************************
     FW4090F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWAFPL     IF   E           K DISK

     D MSG             S             70    DIM(2) CTDATA PERRCD(1)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  MY                     1     72  0
     D                                     DIM(12)
     D  SCMY01                 1      6  0
     D  SCMY02                 7     12  0
     D  SCMY03                13     18  0
     D  SCMY04                19     24  0
     D  SCMY05                25     30  0
     D  SCMY06                31     36  0
     D  SCMY07                37     42  0
     D  SCMY08                43     48  0
     D  SCMY09                49     54  0
     D  SCMY10                55     60  0
     D  SCMY11                61     66  0
     D  SCMY12                67     72  0
     D                 DS
     D  YM                     1     60  0
     D                                     DIM(12)
     D  FPYM01                 1      5  0
     D  FPYM02                 6     10  0
     D  FPYM03                11     15  0
     D  FPYM04                16     20  0
     D  FPYM05                21     25  0
     D  FPYM06                26     30  0
     D  FPYM07                31     35  0
     D  FPYM08                36     40  0
     D  FPYM09                41     45  0
     D  FPYM10                46     50  0
     D  FPYM11                51     55  0
     D  FPYM12                56     60  0
     D                 DS
     D  AM                     1    132  2
     D                                     DIM(12)
     D  SCAM01                 1     11  2
     D  SCAM02                12     22  2
     D  SCAM03                23     33  2
     D  SCAM04                34     44  2
     D  SCAM05                45     55  2
     D  SCAM06                56     66  2
     D  SCAM07                67     77  2
     D  SCAM08                78     88  2
     D  SCAM09                89     99  2
     D  SCAM10               100    110  2
     D  SCAM11               111    121  2
     D  SCAM12               122    132  2
     D                 DS
     D  FA                     1    108  2
     D                                     DIM(12)
     D  FPAM01                 1      9  2
     D  FPAM02                10     18  2
     D  FPAM03                19     27  2
     D  FPAM04                28     36  2
     D  FPAM05                37     45  2
     D  FPAM06                46     54  2
     D  FPAM07                55     63  2
     D  FPAM08                64     72  2
     D  FPAM09                73     81  2
     D  FPAM10                82     90  2
     D  FPAM11                91     99  2
     D  FPAM12               100    108  2
     D                 DS
     D  F1STPO                 1      7  0
     D  XXSTYM                 1      5  0
     D                 DS
     D  F1ENPO                 1      7  0
     D  XXENYM                 1      5  0
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
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE FINANCIAL EARNED PREM WORK FILE
     C*
     C     FPKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    FPEMP#        XXEMP#
     C     *LIKE         DEFINE    FPDIV         XXDIV
     C     *LIKE         DEFINE    FPFYR         XXFYR
     C     *LIKE         DEFINE    FPYM01        XXYRMO
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      **** Begin Add **** 05/14/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 05/14/24 ******************************************
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

      **** Begin Add **** 05/14/24 ******************************************
      * Do not Pass-Through to new screen, if 3rd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 3
     c                   Call      'WTFEPRPT'
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 05/14/24 ******************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCYR
     C*
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
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
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
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
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
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
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*
     C                   Z-ADD     0             XXFYR
     C                   MOVE      SCYR          XXFYR
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  GET SUMMARY RECORD FROM WORK FILE & LOAD SCR02
     C*
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   MOVE      *ZEROS        MY
     C                   MOVE      *ZEROS        AM
     C     FPKEY         CHAIN     PFP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLT     13
     C*
     C     YM(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     YM(X)         SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         MY(X)
     C*
     C                   Z-ADD     FA(X)         AM(X)
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      XXFYR         SCYEAR
     C                   MOVE      *BLANKS       SCDATE
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXYRMO
     C*
     C     XXYRMO        IFLT      XXSTYM
     C     XXYRMO        ORGE      XXENYM
     C*
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*
     C     SCMSG1        IFEQ      *BLANKS
      **** BEGIN CHG **** 07/01/98 *********************************************
     C******************** MOVE SCDATE    SVDATE  40
     C                   MOVE      SCDATE        SVDATE            6 0
      **** END   CHG **** 07/01/98 *********************************************
     C                   END
     C*
     C*  SEND A MESSAGE IF ENTERED MONTH/YEAR HAS ALREADY BEEN
     C*  PROCESSED
     C*
     C     SCMSG1        IFEQ      *BLANKS
     C     SCDATE        ORNE      SVDATE
     C                   MOVE      SCDATE        XXDATE            6 0
     C                   Z-ADD     1             X
     C     XXDATE        LOOKUP    MY(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     MSG(1)        SCMSG1
     C                   MOVEL     MSG(2)        SCMSG2
     C                   MOVE      SCDATE        SVDATE
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCYR          QQFYR             3
     C                   MOVE      SCDATE        QQDATE            6
     C                   CALL      'W4090C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQDATE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**
MONTH/YEAR has already been processed for this fund and fund year,
and using this date will cause the data to be overlaid.
