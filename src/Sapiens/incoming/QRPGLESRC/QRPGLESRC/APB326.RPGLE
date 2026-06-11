     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H******************************************************************
     H/TITLE APB326 - ON DEMAND G/L FEED
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/25/00  C:33  SEQ#: GL065/22504                            *
     H*                  USE TRACTM CORRECTLY                         *
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
     FAPB326F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKDTL1    IF   E           K DISK

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
     C*
     C***  BEGIN CHG  ***  04/25/00  *********************************
     C****BEG*CHG * 11/10/97 * V3
     C**********           MOVELSCMY      MO      20
     C**********           MOVELSCMO      MO      20
     C****END*CHG * 11/10/97 * V3
     C********** MO        IFLT 1
     C********** MO        ORGT 12
     C**********           MOVE '1'       *IN20
     C**********           GOTO STRT
     C**********           END
     C**********
     C***SUBMIT*PROGRAM
     C****BEG*CHG * 11/10/97 * V3
     C**********           MOVE SCMY      QQMY
     C**********           MOVELSCMO      QQMY
     C**********           MOVE SCYR      QQMY
     C****END*CHG * 11/10/97 * V3
     C                   Z-ADD     SCMY          SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCMY
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   MOVE      SCMY          QQMY
     C***  END   CHG  ***  04/25/00  *********************************
     C                   MOVE      'D'           QQMODE            1
     C*
     C                   CALL      'APB326S'
     C                   PARM                    QQKEY            40
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
      *** BEG CHG * 11/10/97 * V3
     C*****                PARM           QQMY    4
     C                   PARM                    QQMY              6
      *** END CHG * 11/10/97 * V3
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
