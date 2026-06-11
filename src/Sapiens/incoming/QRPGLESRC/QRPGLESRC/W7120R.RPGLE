     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7120R - PROMPT FOR LOSS RATIO REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/19/99 C:118 PROJECT: WC898                                *
     H*                 Add 'Region' option and F10=Search on prompt  *
     H*                 screen and associated code in RPG.            *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7120F IN SCREEN.     *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*
     H*  20 - Invalid Date
     H*  21 - Invalid Fund Year
     H*  22 - Invalid Policy Year
     H*  60 - General Chain
     H*  81 - Invalid search field
     H*  82 - Invalid search field
     H*  83 - Invalid Region
     H*  84 - PC on SCFYR
     H*  86 - PC on SCRGN
     H*
     H*****************************************************************
     FW7120F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F***** BEGIN ADD **** 07/19/99 ***********************************
     F                                     INFDS(DBFDS)
     F***** END   ADD **** 07/19/99 ***********************************
     FWDF1L     IF   E           K DISK
     F* * * Fund Detail File
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*
     F***** BEGIN ADD **** 07/19/99 ***********************************
     F* * * Region Master Logical File
     FWMREL     IF   E           K DISK
     F***** END   ADD **** 07/19/99 ***********************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D***** BEGIN ADD **** 07/19/99 ***********************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D***** END   ADD **** 07/19/99 ***********************************
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
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C*
     C*   KEY LIST FOR REGION FILE
     C*
     C     RGKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRGN
     C*
     C***** END   ADD **** 07/19/99 ***********************************
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
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C*================================================================
     C*
     C     *LIKE         DEFINE    RECO#         KKCO#
     C     *LIKE         DEFINE    REFND         KKFND
     C     *LIKE         DEFINE    RERGN#        KKRGN
     C     *LIKE         DEFINE    RERGN#        XXRGN
     C*
     C*================================================================
     C***** END ADD ****** 07/19/99 ***********************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKEY         CHAIN     PFN01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*PUIMOD
     C*                  MOVEL     *DATE         SCDTE
     C                   EVAL      SCDTE=%Int(%Char(%Date():*USA0))
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
     C***** BEGIN DEL **** 07/19/99 ***********************************
     C*********            MOVE Q1USER    SCUSER
     C***** BEGIN DEL **** 07/19/99 ***********************************
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C                   MOVE      *ON           *IN81
     C*
     C     HFLD          WHENEQ    'SCRGN'
     C                   Z-ADD     *ZEROS        XXRGN
     C                   CALL      'W0520R'
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    XXRGN
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     XXRGN         IFNE      *ZEROS
     C                   Z-ADD     XXRGN         SCRGN
     C                   ELSE
     C                   Z-ADD     0             SCRGN
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDTE'
     C                   MOVE      *ON           *IN82
     C*
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C***** END   ADD **** 07/19/99 ***********************************
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
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKEY         CHAIN     PFN01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE
     C*
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*
     C                   MOVE      SCFYR         XXFYR             3 0
     C**         F1KEY ----------------- CO#, FND, FYR
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101                              22
     C     *IN22         CABEQ     '1'           STRT
     C                   END
     C**
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C*
     C*  VALIDATE REGION IF ENTERED OR SELECTED FROM SEARCH
     C*
     C     SCRGN         IFNE      *ZEROS
     C*
     C                   MOVE      UPCO#         KKCO#
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCRGN         KKRGN
     C*
     C     RGKEY         CHAIN     PRE01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      KKRGN         SCRGN
     C                   ELSE
     C                   MOVE      *ON           *IN83
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C***** END   ADD **** 07/19/99 ***********************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKEY         CHAIN     PFN01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCDTE         QQDTE             8
     C                   MOVE      SCFYR         QQFYR             3
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C                   MOVE      SCRGN         QQRGN             3
     C***** END   ADD **** 07/19/99 ***********************************
     C                   CALL      'W7120C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQDTE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C                   PARM                    QQFYR
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C                   PARM                    QQRGN
     C***** END   ADD **** 07/19/99 ***********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
