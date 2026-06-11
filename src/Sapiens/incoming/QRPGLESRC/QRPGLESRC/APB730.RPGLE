     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE APB730 - PRINT HISTORY DATA
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
     FAPB730F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPVML1    IF   E           K DISK

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
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C                   MOVE      'B'           SCTYPE            1
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
     C                   MOVE      '1'           X@CODE
     C                   CALL      'APD010R'
     C                   PARM                    SCVDR
     C                   PARM                    X@CODE            1
     C                   PARM      '1'           QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
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
     C*  FROM DATE MUST BE A VALID DATE.
     C*
     C     SCFROM        IFEQ      *ZERO
     C                   ELSE
     C*
     C                   Z-ADD     SCFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFROM
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCTO          IFEQ      *ZERO
     C                   ELSE
     C*
     C                   Z-ADD     SCTO          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTO
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
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
     C*
     C     SCVDR         IFNE      *ZERO
     C     SCVDR         CHAIN     PVM01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  SUBMIT PROGRAM
     C                   MOVE      SCFROM        XXFROM
     C                   MOVE      SCTO          XXTO
     C                   MOVEL     SCQUE         QQQUE
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFAC#        XXFAC#
     C                   MOVE      SCVDR         XXVDR
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'APB730S'
     C                   PARM                    QQKEY            40
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    XXFROM            8
     C                   PARM                    XXTO              8
     C                   PARM                    XXCO#             3
     C                   PARM                    XXFAC#            3
     C                   PARM                    XXVDR             7
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
