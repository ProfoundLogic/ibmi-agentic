     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7400R - PROMPT FOR AGENT LATE NOTICE REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/10/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                                                               *
     H*    08/05/91  TAA   LOAD BLANKS FOR A/R TRANSACTION CODES.     *
     H*    PROJ: AJC       USER CAN SELECT DESIRE CODES OR LEAVE      *
     H*                    BLANK FOR ALL CODES.                       *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW7400F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTABL     IF   E           K DISK

     D SCD             S             10    DIM(30) CTDATA PERRCD(1) ASCEND      A/R CODES
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CD                     1     60
     D                                     DIM(30) ASCEND                       A/R CODES
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D  SCCD05                 9     10
     D  SCCD06                11     12
     D  SCCD07                13     14
     D  SCCD08                15     16
     D  SCCD09                17     18
     D  SCCD10                19     20
     D  SCCD11                21     22
     D  SCCD12                23     24
     D  SCCD13                25     26
     D  SCCD14                27     28
     D  SCCD15                29     30
     D  SCCD16                31     32
     D  SCCD17                33     34
     D  SCCD18                35     36
     D  SCCD19                37     38
     D  SCCD20                39     40
     D  SCCD21                41     42
     D  SCCD22                43     44
     D  SCCD23                45     46
     D  SCCD24                47     48
     D  SCCD25                49     50
     D  SCCD26                51     52
     D  SCCD27                53     54
     D  SCCD28                55     56
     D  SCCD29                57     58
     D  SCCD30                59     60
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
     C                   KFLD                    XXFND             3 0
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVE      *ZERO         UPCO#
     C*
     C                   ENDIF                                                  *IN60
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
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNPZIP        SCZP              3 0
     C*
     C                   MOVE      *BLANK        CD
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDATE
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*     ERROR INDICATORS ARE *IN20 - *IN26, *IN65 - *IN70
     C*     POSITION CURSOR INDICATORS ARE *IN27 - *IN56
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   EXFMT     SCR01
     C*
     C                   ENDIF                                                  Q2RECF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  *INKC
     C*
     C*  TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C     HFLD          LOOKUP    SCD(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C                   MOVEL     SCKEY         CD(X)
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANK
     C*
     C     X             ADD       26            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     X             ADD       27            X1                2 0
     C     X1            IFNE      57
     C                   MOVE      '1'           *IN(X1)
     C*
     C                   ENDIF                                                  X1 NE 57
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ
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
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10
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
     C*
     C                   ENDIF                                                  *IN40
     C*
     C                   GOTO      STRT01
     C*
     C                   ENDIF                                                  *INKM
     C*
     C                   ENDIF                                                  *IN11
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE A/R CODES
     C*
     C                   Z-ADD     1             X
     C     X             DOUEQ     31
     C*
     C     CD(X)         IFNE      *BLANKS
     C*
     C                   MOVEA     '0000'        *IN(65)
     C                   MOVE      '0'           *IN70
     C     X             DIV       5.1           XXROW             2 0
     C     XXROW         ADD       65            X2                2 0
     C                   MOVE      '1'           *IN(X2)
     C                   MULT      5             XXROW
     C     X             SUB       XXROW         XXCOL             2 0
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     CD(X)         XXKEY
     C     TBKEY         CHAIN     PTT01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C     XXCOL         ADD       21            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN40
     C*
     C     TTSTAT        IFEQ      'I'
     C*
     C     XXCOL         ADD       21            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  TTSTAT
     C*
     C                   ENDIF                                                  CD,X
     C*
     C                   ADD       1             X
     C*
     C                   ENDDO
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCDATE        QQDATE            8
     C                   MOVEA     CD            QQCODE           60
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W7400C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQDATE
     C                   PARM                    QQCODE
     C                   PARM                    QQOUTQ
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
** SCD
SCCD01
SCCD02
SCCD03
SCCD04
SCCD05
SCCD06
SCCD07
SCCD08
SCCD09
SCCD10
SCCD11
SCCD12
SCCD13
SCCD14
SCCD15
SCCD16
SCCD17
SCCD18
SCCD19
SCCD20
SCCD21
SCCD22
SCCD23
SCCD24
SCCD25
SCCD26
SCCD27
SCCD28
SCCD29
SCCD30
