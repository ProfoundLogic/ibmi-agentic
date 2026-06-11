     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U0535R - LOS PAS GROUP SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROMPT PROGRAM FOR LOS PAS GROUP SERACH                    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    U0010R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    U0536R         LOS REGION CODE SEARCH                      *
     H*    U0537R         LOS CATEGORY CODE SEARCH                    *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAA                *
     H*  CREATED: 12/18/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  10/05/93  C:20    REQ/PROJ: URMNT                            *
     H*                    1. ADDED CODE TO SEND ERROR MESSAGE IF LOS *
     H*                       INFORMATION ENTERED IS INVALID.         *
     H*  03/01/94  C:20    REQ/PROJ: URMNT                            *
     H*                    1. IF LOS TYPE IS 'O' EDIT CATEGORY CODE   *
     H*                       FOR 1,2,4,5,8,9 ONLY.                   *
     H*  02/23/96  C:32  PROJECT# MC131                               *
     H*                  CHG TO USE NEW LOS DIAG OR OPER GRP - LSLGRP,*
     H*                  DILGRP                                       *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FU0535F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FUMLSL     IF   E           K DISK
     FUMLSL1    IF   E           K DISK
     F                                     RENAME(PLS01:PLS02)
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FFMDIL     IF   E           K DISK

     D*
     D RGN             S              1    DIM(7) CTDATA PERRCD(1)
     D CATD            S              1  0 DIM(10) CTDATA PERRCD(1)
     D CATO            S              1  0 DIM(6) CTDATA PERRCD(1)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXPASG                 1      6
     D  LSRGN                  1      1  0
     D  X1LGRP                 2      6
     D                 DS
     D  FL                     1      7
     D                                     DIM(7)                               SELECTION FLAG
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D                 DS
     D  PT                     1     14
     D                                     DIM(7)                               PERCENTAGES
     D  SCPT01                 1      2
     D  SCPT02                 3      4
     D  SCPT03                 5      6
     D  SCPT04                 7      8
     D  SCPT05                 9     10
     D  SCPT06                11     12
     D  SCPT07                13     14
     D                 DS
     D  P1                     1      7
     D                                     DIM(7)                               + PERCENTILE
     D  SCP101                 1      1
     D  SCP102                 2      2
     D  SCP103                 3      3
     D  SCP104                 4      4
     D  SCP105                 5      5
     D  SCP106                 6      6
     D  SCP107                 7      7
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  USER PARAMETER FILE
     C*
     C     LSKEY         KLIST
     C                   KFLD                    XXRGN
     C                   KFLD                    XXLGRP
     C                   KFLD                    SCCAT
     C                   KFLD                    SCAGE
     C                   KFLD                    XXEFDT
     C*
     C*  DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXICD             5
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C     *LIKE         DEFINE    LSDT          XXEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQPASG            6
     C                   PARM                    QQAGE             1
     C                   PARM                    QQCAT             1
     C                   PARM                    QQICD             5
     C                   PARM                    QQCERT            3
     C                   PARM                    QQRTCD            1
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
     C     *LIKE         DEFINE    LSRGN         XXRGN
     C     *LIKE         DEFINE    LSGRP         XXGRP
     C     *LIKE         DEFINE    LSLGRP        XXLGRP                         ADD 02/23/96
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C                   PARM      *BLANKS       ##DIFI            1            DIARY FUNCTION INST
     C                   PARM      *BLANKS       ##APRM            1            PROVIDER MAINT
     C                   PARM      *BLANKS       ##CLED            1            CLAIM EDIT INST
     C                   PARM      *BLANKS       ##DMEI            1            DME INSTALLED
     C                   PARM      *BLANKS       ##DLOS            1            DEFAULT LOS RGN
     C                   PARM      *BLANKS       ##PHPR            1            PHARMACY PROCESSING
     C*** END   ADD *** 08/15/00 **************************************
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQICD         XXICD
     C                   MOVE      QQAGE         SCAGE
     C                   MOVE      QQCAT         SCCAT
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** FBFL08    IFNE *BLANKS
     C**********           MOVE FBFL08    SCRGN
     C     ##DLOS        IFNE      *BLANKS
     C                   MOVE      ##DLOS        SCRGN
     C*** END   CHG *** 08/15/00 **************************************
     C                   MOVE      '1'           *IN22
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   MOVE      *ZEROS        SCRGN
     C                   END
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           ELSE
     C**********           MOVE '1'       *IN20
     C**********           MOVE *ZEROS    SCRGN
     C**********           END
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C* CHAIN TO THE DIAGNOSIS CODE FILE TO GET THE GROUP NUMBER
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  DILGRP        XXLGRP
     C                   MOVEL     DILOST        SCGRP
     C                   ELSE
     C                   MOVE      *BLANKS       XXLGRP
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C                   EXFMT     WDW01
     C                   END
     C*
     C     Q2RECF        IFEQ      'WDW02'
     C                   EXFMT     WDW02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C************************************
     C*   F6                             *
     C************************************
     C     *INKF         IFEQ      '1'
     C************************************
     C*   SCR01 = LOS REGION CODE SEARCH *
     C************************************
     C                   CALL      'U0536R'
     C                   PARM                    QQPRDS
     C                   PARM                    SCRGN
     C                   PARM                    Q1RTCD            1
     C     Q1RTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99                                        ADD 02/23/96
     C                   MOVEL(P)  'WDW01'       Q2RECF
     C*
     C     Q1RTCD        IFEQ      'B'
     C     SCRGN         OREQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    END EQUAL B
     C*
     C     Q1RTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    END *
     C                   END                                                    END *INKF
     C***********************************
     C*   F7                            *
     C***********************************
     C     *INKG         IFEQ      '1'
     C************************************
     C*   SCR01 = LOS CATEGORY SEARCH    *
     C************************************
     C*
     C                   CALL      'U0537R'
     C                   PARM                    QQPRDS
     C                   PARM                    Q2CAT             1
     C                   PARM                    Q2RTCD            1
     C                   MOVE      Q2CAT         SCCAT
     C     Q2RTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99                                        ADD 02/23/96
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C     Q2RTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END EQUAL B
     C*
     C     Q2RTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END                                                    END *
     C*
     C                   END                                                    END *INKG
     C*
     C                   END
     C***********************************
     C*  F12 = PREVIOUS                 *
     C***********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'WDW01'
     C                   MOVE      '1'           *INLR
     C                   MOVEL     'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'WDW02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END *INKL
     C*
     C* PROCESS REQUESTED SCREEN
     C*
     C     Q2RECF        CABEQ     'WDW01'       SCR01T
     C     Q2RECF        CABEQ     'WDW02'       SCR02T
     C*
     C***************************
     C*   PROCESS REQUEST SCREEN*
     C***************************
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE ENTERED INFORMATION
     C*
     C                   Z-ADD     1             X                 5 0
     C     SCRGN         LOOKUP    RGN(X)                                 60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C     SCGRP         IFEQ      'D'
     C*
     C                   Z-ADD     1             X
     C     SCCAT         LOOKUP    CATD(X)                                60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C*
     C     SCGRP         IFEQ      'O'
     C*
     C                   Z-ADD     1             X
     C     SCCAT         LOOKUP    CATO(X)                                60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END SCGRP = 'O'
     C                   END                                                    END SCGRP = 'D'
     C*
     C     SCGRP         IFNE      'O'
     C     SCGRP         ANDNE     'D'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C     SCAGE         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      SCRGN         XXRGN
     C*
     C     SCGRP         IFEQ      'O'
     C*
     C     LSKEY         SETGT     PLS02
     C     LSREAD        TAG
     C                   READP     PLS02                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     XXRGN         ANDEQ     LSRGN
     C     XXLGRP        ANDEQ     LSLGRP
     C     SCCAT         ANDEQ     LSCAT
     C     SCAGE         ANDEQ     LSAGE
     C     LSSTAT        CABEQ     'I'           LSREAD
     C                   MOVEL     LSLGRP        X1LGRP
     C                   MOVE      XXPASG        QQPASG
     C                   MOVE      SCCAT         QQCAT
     C                   MOVE      SCAGE         QQAGE
     C                   MOVE      '*'           QQRTCD
     C                   ELSE
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCGRP         IFEQ      'D'
     C*
     C     LSKEY         SETGT     PLS01
     C     LSRED1        TAG
     C                   READP     PLS01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     XXRGN         ANDEQ     LSRGN
     C     XXLGRP        ANDEQ     LSLGRP
     C     SCCAT         ANDEQ     LSCAT
     C     SCAGE         ANDEQ     LSAGE
     C     LSSTAT        CABEQ     'I'           LSRED1
     C                   MOVEL     LSLGRP        X1LGRP            5
     C                   MOVE      XXPASG        QQPASG
     C                   MOVE      SCCAT         QQCAT
     C                   MOVE      SCAGE         QQAGE
     C                   MOVE      '*'           QQRTCD
     C                   ELSE
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* ALL EDIT HAVE BEEN PASSED GOTO NEXT SCREEN
     C*
     C                   MOVE      *BLANKS       FL
     C                   MOVE      LS10P         SCPT01
     C                   MOVE      LS10P1        SCP101
     C                   MOVE      LS25P         SCPT02
     C                   MOVE      LS25P1        SCP102
     C                   MOVE      LS50P         SCPT03
     C                   MOVE      LS50P1        SCP103
     C                   MOVE      LS75P         SCPT04
     C                   MOVE      LS75P1        SCP104
     C                   MOVE      LS90P         SCPT05
     C                   MOVE      LS90P1        SCP105
     C                   MOVE      LS95P         SCPT06
     C                   MOVE      LS95P1        SCP106
     C                   MOVE      LS99P         SCPT07
     C                   MOVE      LS99P1        SCP107
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW02'       Q2RECF
     C                   GOTO      STRT
     C*
     C***************************
     C*   PROCESS REQUEST SCREEN*
     C***************************
     C     SCR02T        TAG
     C*
     C* VERIFY THE SELECTION FIELD IF ENTERED
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C                   DO        7             X                 5 0
     C     X             ADD       19            A1                2 0          POSIT CURSOR
     C     FL(X)         IFNE      *BLANKS
     C     FL(X)         IFNE      '1'
     C                   MOVE      '1'           *IN(A1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C* CHECK FOR THE USER SELECTION
     C*
     C                   Z-ADD     1             X
     C     '1'           LOOKUP    FL(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVE      PT(X)         QQCERT
     C                   ELSE
     C                   MOVE      *ZEROS        QQCERT
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  REGION CODE TABLE
2
3
4
5
6
6
6
**  CAT D CODE TABLE
0
1
2
3
4
5
6
7
8
9
**  CAT O CODE TABLE
1
2
4
5
8
9
