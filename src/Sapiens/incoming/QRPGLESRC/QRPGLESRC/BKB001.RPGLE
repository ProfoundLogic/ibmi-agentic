     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKB001 - PRINT GENERAL LEDGER CHART OF ACCOUNTS PROMPT
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
     FBKB001F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
     D                 DS
     D  S1ACCT                 1     15
     D  S1CO#                  1      3  0
     D  S1FAC#                 4      6  0
     D  S1GLD#                 7      9  0
     D  S1ACC#                10     12  0
     D  S1SUB#                13     15  0
     D                 DS
     D  S2ACCT                 1     15
     D  S2CO#                  1      3  0
     D  S2FAC#                 4      6  0
     D  S2GLD#                 7      9  0
     D  S2ACC#                10     12  0
     D  S2SUB#                13     15  0
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
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ZEROS        S1ACCT
     C                   MOVE      *ZEROS        S2ACCT
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
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKD010R'
     C                   PARM                    SCTYPE
     C                   PARM                    SCCO#             3 0
     C                   PARM                    SCFAC#            3 0
     C                   PARM                    SCGLD#            3 0
     C                   PARM                    SCACC#            3 0
     C                   PARM                    SCSUB#            3 0
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      ' '
     C     QQRTC         OREQ      '*'
     C                   WRITE     SCR99
     C     S1CO#         IFGT      *ZERO
     C                   MOVE      SCCO#         S2CO#
     C                   MOVE      SCFAC#        S2FAC#
     C                   MOVE      SCGLD#        S2GLD#
     C                   MOVE      SCACC#        S2ACC#
     C                   MOVE      SCSUB#        S2SUB#
     C                   ELSE
     C                   MOVE      SCCO#         S1CO#
     C                   MOVE      SCFAC#        S1FAC#
     C                   MOVE      SCGLD#        S1GLD#
     C                   MOVE      SCACC#        S1ACC#
     C                   MOVE      SCSUB#        S1SUB#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKE
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*************************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C     S2ACCT        IFLT      S1ACCT
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C*  CHECK THE TYPE-' ',1,2,3,4,B,P
     C*
     C     SCTYPE        IFNE      *BLANKS
     C     SCTYPE        ANDNE     '1'
     C     SCTYPE        ANDNE     '2'
     C     SCTYPE        ANDNE     '3'
     C     SCTYPE        ANDNE     '4'
     C     SCTYPE        ANDNE     'B'
     C     SCTYPE        ANDNE     'P'
     C                   MOVE      '1'           *IN22
     C                   END
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
     C*
     C                   MOVEL     SCQUE         QQQUE            10
     C*
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'BKB001S'
     C                   PARM                    QQKEY            40
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    S1ACCT
     C                   PARM                    S2ACCT
     C                   PARM                    SCTYPE
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
