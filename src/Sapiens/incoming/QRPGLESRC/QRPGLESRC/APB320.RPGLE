     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE APB320 - MONTH END PROMPT
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    REMARK  TYPE REQUIRED                                   *
     H*  21    REMARK  FROM REQUIRED                                   *
     H*  22    REMARK  TO REQUIRED                                     *
     H* SCR02:                                                         *
     H*  20    HEADING REQUIRED                                        *
     H*  21    Y/N REQUIRED FOR DIARY TYPE FLAG                        *
     H*  64    DELETE NOT ALLOWED                                      *
     H******************************************************************
     FAPB320F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKDTL1    IF   E           K DISK
     FHMTBL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
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
     C     TBLKEY        KLIST
     C                   KFLD                    TBLID             2
     C                   KFLD                    TBLVAL           10
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C*
     C*** GET G/L DATE
     C*
     C     'AP'          CHAIN     PDT01                              60
     C*
     C                   Z-ADD     DTAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE OUTPUT QUEUE DEFAULT
     C*
     C                   MOVE      *BLANK        TBLVAL
     C                   MOVEL     Q1JOB         TBLVAL
     C                   MOVEL     'WS'          TBLID             2
     C     TBLKEY        CHAIN     PWS01                              60
     C  N60              MOVEL     WSOUT3        SCQUE
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*************************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*          'GL'      CHAINPSY01                60
     C*          SYDEV     IFEQ *BLANKS
     C*                    MOVE '1'       *IN20
     C*                    GOTO STRT
     C*                    END
     C*
     C*  VALIDATE OUTPUT QUEUE
     C*
     C                   MOVE      *BLANKS       XXRTC             1
     C                   CALL      'BKPRMTC'
     C                   PARM                    XXRTC             1
     C                   PARM                    SCQUE
     C     XXRTC         IFEQ      'X'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  SUBMIT PROGRAM
     C                   MOVE      SCDATE        QQDATE
     C                   MOVEL     SCQUE         QQQUE
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'APB320S'
     C                   PARM                    QQKEY            40
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    SYDEV             8
     C                   PARM                    QQDATE            6
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
