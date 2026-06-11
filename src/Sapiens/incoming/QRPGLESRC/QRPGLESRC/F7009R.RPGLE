     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F7009R - PROMPT FOR TABLE FILE STATE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H* 04/23/98 C:90 PROJECT: MC181                                  *
     H*               CHANGED F10 TO CALL F0601R INSTEAD              *
     H*               OF SY600R                                       *
     H*                                                               *
     H* 04/08/98 C:89 PROJECT: MC181                                  *
     H*               CHANGED F6 TO F10 FOR SEARCH                    *
     H*               VERSION 4 STANDARDS                             *
     H*                                                               *
     H* 10/22/96 C:59 Req/Proj: 203/FS134                             *
     H*               Call to program CHKSTR to verify: if            *
     H*               State is installed.  Returns '1' If found       *
     H*               and '0' if not found.                           *
     H* INDICATOR USAGE                                               *
     H*  20    MCR0504-INVALID STATE                                  *
     H*  21    MCR0766 - INVALID ADJUDICATION STATE FOR SYSTEM        *
     H*****************************************************************
     FF7009F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
      **** BEGIN ADD **** 04/23/98 ************************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      **** END   ADD **** 04/23/98 ************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** INSTALLED ADJUDICATION STATES FROM SFBPF
     D                 DS
     D*** BEGIN CHG *** 08/15/00 **************************************
     D**********                              1  20 ST
     D**********                              1   2 FBST01
     D**********                              3   4 FBST02
     D**********                              5   6 FBST03
     D**********                              7   8 FBST04
     D**********                              9  10 FBST05
     D**********                             11  12 FBST06
     D**********                             13  14 FBST07
     D**********                             15  16 FBST08
     D**********                             17  18 FBST09
     D**********                             19  20 FBST10
     D  ST                     1     20
     D                                     DIM(10)                              ADJ ST FROM SFBPF
     D  ##ST01                 1      2
     D  ##ST02                 3      4
     D  ##ST03                 5      6
     D  ##ST04                 7      8
     D  ##ST05                 9     10
     D  ##ST06                11     12
     D  ##ST07                13     14
     D  ##ST08                15     16
     D  ##ST09                17     18
     D  ##ST10                19     20
     D*** END   CHG *** 08/15/00 **************************************
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
     C*  KEY LIST FOR STATE FILE
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*
     C*  FS BASE CONTROL FILE
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQST              2
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
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C*** INVALID ADJUDICATION STATE - NO FMPRP FILE OUT THERE FOR THIS STATE
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      ' '           QQRTCD
     C                   END                                                    END LOKUP ST
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
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
      **** BEGIN DEL **** 04/08/98 ************************************
     C******************** MOVE Q1USER    SCUSER
      **** END   DEL **** 04/08/98 ************************************
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
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*************************
     C*   F3 = EXIT           *
     C*************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*************************
     C*  F10 = STATE SEARCH   *
     C*************************
      **** BEGIN CHG **** 04/08/98 ************************************
     C********** *INKF     IFEQ '1'
     C     *INKJ         IFEQ      '1'
      **** END   CHG **** 04/08/98 ************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
      **** BEGIN CHG **** 04/23/98 ************************************
     C                   CALL      'F0601R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM                    QQRRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     22
      *
     C     QQRRN         CHAIN     PST02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     STKEY         SCST
     C                   ENDIF                                                  IFEQ *IN60
     C                   GOTO      STRT
      *
     C******************** CALL 'SY600R'
     C******************** PARM           SCST
     C******************** PARM           QQRTC   1
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCST      OREQ *BLANKS
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTC     IFEQ '*'
     C******************** GOTO STRT
     C******************** END
      **** END   CHG **** 04/23/98 ************************************
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END *INKJ
     C*************************
     C*  *IN10 = HELP         *
     C*************************
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
     C                   GOTO      STRT
     C                   END
      **** BEGIN DEL **** 04/23/98 ************************************
     C*************************
     C**********  F13 = CHG USER PARMS *
     C*************************
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
     C******************** WRITESCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** END
     C******************** GOTO STRT01
     C******************** END
      **** END   DEL **** 04/23/98 ************************************
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   STATE MUST BE VALID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** VALID INSTALLED STATE MUST BE SELECTED BEFORE YOU CAN DO SEARCH
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C*
     C                   MOVEL     SCST          QQSTT
     C                   CALL      'CHKSTR'                             99
     C                   PARM                    QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C     *IN99         IFEQ      '0'
     C     QQSTRT        IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  END LOKUP ST
     C                   ELSE
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C                   Z-ADD     1             X                 2 0
     C     SCST          LOOKUP    ST(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    END LOKUP ST
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           END                             END 0 *IN60
     C*** END   DEL *** 08/15/00 **************************************
     C                   ENDIF                                                  END *IN99
     C*
     C                   MOVEL     SCST          QQST
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
