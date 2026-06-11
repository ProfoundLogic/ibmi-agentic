     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7260R - PROMPT FOR LOSS RATIO W/SELECTION PERCENTAGE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/15/99  C:15  Project: WC577                                *
     H*                 Y2K - Chgd FYR parm to 3A.                    *
     H*                                                               *
     H* 03/02/99  C:118 PROJECT:  WC577                               *
     H*                 MADE POLICY YEAR A REQUIRED FIELD             *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7260F IN SCREEN.     *
     H*                                                               *
     H* 12/31/97  C:35  PROJECT: WC577                                *
     H*                 CHANGED TO DEFAULT RUN DATE TO CURRENT DATE.  *
     H*                                                               *
     H* 08/12/97  C:78  PROJECT: WC577                                *
     H*                 EDIT FOR LOSS RATIO AND FREQUENCY RATIO       *
     H*                 DELETED. THEY CAN BE BLANK.                   *
     H*                                                               *
     H* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
     H* !!  ADD NEW REVISIONS AT THE TOP OF THE REVISIONS SECTION  !! *
     H* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
     H*                                                               *
     H*****************************************************************
     FW7260F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     F* * * Fund Detail File
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
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
      **** BEGIN DEL **** 12/31/97 ************************************
     C******************** Z-ADDUPPRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCDTE
      **** END   DEL **** 12/31/97 ************************************
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
      **** BEGIN ADD **** 12/31/97 ************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
      **** END   ADD **** 12/31/97 ************************************
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
      **** BEGIN DEL **** 12/31/97 ************************************
     C******************** Z-ADDUPPRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCDTE
      **** END   DEL **** 12/31/97 ************************************
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
     C*
     C***** BEGIN CHG **** 03/02/99 ***********************************
     C                   ELSE
     C                   MOVE      *ON           *IN29
     C     *IN29         CABEQ     '1'           STRT
     C***** END CHG ****** 03/02/99 ***********************************
     C*
     C                   END
     C**
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C**
     C*  VALIDATE LOSS RATIO SELECTION FLAG
     C*
     C     SCLOSS        IFNE      'L'
     C     SCLOSS        ANDNE     *BLANK
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C**
     C*  VALIDATE FREQUENCY RATIO SELECTION FLAG
     C*
     C     SCFREQ        IFNE      'F'
     C     SCFREQ        ANDNE     *BLANK
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
      ******* BEGIN DLT ***** 08/12/97 ********************************
     C*
     C*  BOTH LOSS AND FREQUENCY CANNOT BE BLANK           JJJ 8-8-88
     C*                                                    |
     C***********SCLOSS    IFEQ *BLANK                     |
     C***********SCFREQ    ANDEQ*BLANK                     |
     C*********************MOVE '1'       *IN23            |
     C*********************GOTO STRT                       |
     C*********************END                             |
      ******* END   DLT ***** 08/12/97 ********************************
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
     C*** BEGIN CHG *** 12/15/99 **************************************
     C******************** MOVE SCFYR     QQFYR   2
     C                   MOVE      SCFYR         QQFYR             3
     C*** END CHG ***** 12/15/99 **************************************
     C                   MOVE      SCDTE         QQDTE             8
     C                   MOVE      SCRPCT        QQRPCT            7
     C                   MOVE      SCLOSS        QQLOSS            1
     C                   MOVE      SCFREQ        QQFREQ            1
     C                   CALL      'W7260C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQDTE
     C                   PARM                    QQRPCT
     C                   PARM                    QQLOSS
     C                   PARM                    QQFREQ
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
