     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F7040R - PROMPT FOR SERVICE CODE FILE LISTING
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/15/00 C:62 PROJECT: MC215                                  *
     H*               REPLACE SFBPF REFERENCES WITH APIS.             *
     H*                                                               *
     H* 04/15/98 C:90 Proj: MC181                                     *
     H*               COMBINED F6 & F7 SEARCH KEYS INTO F10 KEY       *
     H*               CHANGED SCREEN TO CURRENT STANDARDS             *
     H*                                                               *
     H* 10/22/96 C:59 Req/Proj: 203/FS134                             *
     H*               Call to program CHKSTR to verify: if            *
     H*               State is installed.  Returns '1' If found       *
     H*               and '0' if not found.                           *
     H* INDICATOR USAGE                                               *
     H*  20    MCR0702-FROM FIELD MUST BE LESS THAN TO FIELD          *
     H*  21    MCR0764-MUST PC AT SERV CODE FIELD FOR CODE SEARCH     *
     H*  91    MC40004-NON-SEARCHABLE FIELD                           *
     H*****************************************************************
     H* REVISIONS                                                     *
     H*   04/21/92 KRR   PROJ:FS001                                   *
     H*                  1. ADDED OVERRIDE COSE FOR SERVICE CODE FILE *
     H*                  2. CHANGED SVCD(4N) TO CMSV(11A)             *
     H*****************************************************************
     FF7040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FSTABL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN

     D*
     D QCA             S             55    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCAEXEC
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** QCAEXEC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  40     41
     D*
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
     D                                     DIM(10)
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      '0'           *IN99
     C***** THIS STATMENT IS PURPOSELY NEVER EXECUTED******************
     C   99
     CANN99UPCO#         CHAIN     PSV01                              60
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
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    INKC
     C***********************************
     C*   F10=SEARCH                    *
     C***********************************
     C***********************************
      **** BEGIN CHG **** 04/14/98 ************************************
     C****F6=NUMERIC SEARCH**************
     C***********************************
     C********** *INKF     IFEQ '1'
      *
     C     *INKJ         IFEQ      '1'
      **** END   CHG **** 04/14/98 ************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
      **** BEGIN ADD **** 04/14/98 ************************************
     C     HFLD          IFEQ      'SCFROM'
     C     HFLD          OREQ      'SCTO'
      **** END   ADD **** 04/14/98 ************************************
     C*** STATE MUST BE ENTERED
     C     SCST          IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLANKS
     C*
     C*** VALID INSTALLED STATE MUST BE SELECTED BEFORE YOU CAN DO SEARCH
     C*** BEGIN ADD *** 10/22/96 **************************************
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C*
     C                   MOVEL     SCST          QQSTT
     C                   CALL      'CHKSTR'                             99
     C                   PARM                    QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C     *IN99         IFEQ      '0'
     C     QQSTRT        IFEQ      '0'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  END LOKUP ST
     C                   ELSE
     C*** END ADD ***** 10/22/96 **************************************
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C                   Z-ADD     1             X
     C     SCST          LOOKUP    ST(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                    END LOKUP ST
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           END                             END 0 *IN60
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN ADD *** 10/22/96 **************************************
     C                   ENDIF                                                  END *IN99
     C*** END ADD ***** 10/22/96 **************************************
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMSVL1
     C                   MOVE      ' '           STOPN             1
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     SCST          @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           STOPN
     C                   END
     C*
      **** BEGIN DEL **** 04/14/98 ************************************
     C******************** Z-ADD0         LIN     20
     C******************** Z-ADD0         COL     20
     C********** Q2CPOS    DIV  256       LIN
     C******************** MVR            COL     20
     C*
     C********** LIN       IFNE 11
     C******************** MOVE '1'       *IN21
     C******************** GOTO STRT
     C******************** END
      **** END   DEL **** 04/14/98 ************************************
     C*
     C                   MOVE      *ZEROS        QQCMSV           11
      **** BEGIN DEL **** 04/15/98 ************************************
     C******************** MOVE SCFROM    QQCMSV 11
      **** END   DEL **** 04/15/98 ************************************
     C                   MOVE      *ZEROS        Q1EFDT            8 0
     C*
     C                   MOVE      SCST          QQST
     C                   CALL      'F0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCMSV
     C                   PARM                    QQST
     C                   PARM                    Q1EFDT
     C                   PARM                    QQRTCD            1
     C*
      *
      **** BEGIN DEL **** 04/15/98 ************************************
     C******************** WRITESCR99
      **** END   DEL **** 04/15/98 ************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQCMSV        OREQ      *ZEROS
      **** BEGIN CHG **** 04/14/98 ************************************
     C********** COL       IFGE 24
     C********** COL       ANDLE37
      *
     C******************** MOVE '1'       *IN24
     C******************** ELSE
     C*
     C********** COL       IFGE 45
     C********** COL       ANDLE58
      *
     C******************** MOVE '1'       *IN25
     C******************** END                             END GE/LE
     C******************** END                             END GE/LE
     C*
     C     HFLD          CABEQ     'SCFROM'                               24
     C     HFLD          CABEQ     'SCTO'                                 25
      **** END   CHG **** 04/14/98 ************************************
     C                   GOTO      STRT
     C                   END                                                    END EQ B/ZERO
     C*
     C     QQRTCD        IFEQ      '*'
      **** BEGIN CHG **** 04/14/98 ************************************
     C********** COL       IFGE 24
     C********** COL       ANDLE37
      *
     C     HFLD          IFEQ      'SCFROM'
      **** END   CHG **** 04/14/98 ************************************
     C                   MOVE      QQCMSV        SCFROM
     C                   MOVE      '1'           *IN25
     C                   ELSE
      **** BEGIN CHG **** 04/14/98 ************************************
     C********** COL       IFGE 45
     C********** COL       ANDLE58
      *
     C     HFLD          IFEQ      'SCTO'
      **** END   CHG **** 04/14/98 ************************************
     C                   MOVE      QQCMSV        SCTO
     C                   MOVE      '1'           *IN28
      **** BEGIN CHG **** 04/15/98 ************************************
     C                   ENDIF                                                  HFLD 'SCTO'
     C                   ENDIF                                                  HFLD 'SCFROM'
      *
     C******************** END                             ENDIFCOL
     C******************** END                             ENDIFCOL
      **** END   CHG **** 04/15/98 ************************************
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD
      **** BEGIN CHG **** 04/15/98 ************************************
     C                   ENDIF                                                  HFLD
     C*
     C******************** END                             END 'SCR01'
     C*
     C******************** END                             END *INKH
      **** END   CHG **** 04/15/98 ************************************
     C*************************
     C***F7 = STATE SEARCH*****
     C*************************
      **** BEGIN CHG **** 04/15/98 ************************************
     C********** *INKG     IFEQ '1'
     C*
     C     HFLD          IFEQ      'SCST'
      *
     C********** Q2RECF    IFEQ 'SCR01'
      **** END   CHG **** 04/15/98 ************************************
     C                   CALL      'SY600R'
     C                   PARM                    SCST
     C                   PARM                    QQRTC             1
     C*
      **** BEGIN CHG **** 04/15/98 ************************************
     C******************** WRITESCR99
     C                   MOVE      *ON           *IN24
      **** END   CHG **** 04/15/98 ************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCST          OREQ      *BLANKS
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   GOTO      STRT
      **** BEGIN CHG **** 04/15/98 ************************************
     C                   MOVE      *ON           *IN24
     C                   ENDIF                                                  QQRTC
     C                   ENDIF                                                  HFLD 'SCST'
     C                   MOVE      *ON           *IN91
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  INKJ
     C*
     C******************** END                             END SCR01
     C******************** END                             END *INKF
      **** END   CHG **** 04/15/98 ************************************
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
      **** BEGIN DEL **** 04/15/98 ************************************
     C******************** WRITESCR99
      **** END   DEL **** 04/15/98 ************************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
      **** BEGIN DEL **** 04/28/98 ************************************
     C***********   PROCESS CHANGE OF USER PARAMETERS
     C*
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
      **** BEGIN DEL **** 04/15/98 ************************************
     C******************** WRITESCR99
      **** ADD   DEL **** 04/15/98 ************************************
     C*
     C**********  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** END
     C******************** GOTO STRT01
     C******************** END
      **** END   DEL **** 04/28/98 ************************************
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   SCFROM MUST BE LESS THAN OR EQUAL TO  SCTO
     C*
     C     SCTO          CABLT     SCFROM        STRT                   20
     C*
     C*   STATE MUST BE VALID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*** VALID INSTALLED STATE MUST BE SELECTED BEFORE YOU CAN DO SEARCH
     C*** BEGIN ADD *** 10/22/96 **************************************
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C*
     C                   MOVEL     SCST          QQSTT
     C                   CALL      'CHKSTR'                             99
     C                   PARM                    QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C     *IN99         IFEQ      '0'
     C     QQSTRT        IFEQ      '0'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  END LOKUP ST
     C                   ELSE
     C*** END ADD ***** 10/22/96 **************************************
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C                   Z-ADD     1             X                 2 0
     C     SCST          LOOKUP    ST(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                    END LOKUP ST
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           END                             END 0 *IN60
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN ADD *** 10/22/96 **************************************
     C                   ENDIF                                                  END *IN99
     C*** END ADD ***** 10/22/96 **************************************
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C     SCEFDT        IFNE      *ZERO
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   MOVE      SCEFDT        QQEFDT            6
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#             3
     C                   MOVE      SCFROM        QQFROM           11
     C                   MOVE      SCTO          QQTO             11
     C                   MOVE      SCST          QQST              2
     C                   MOVE      SCEFDT        QQEFDT            6
     C                   CALL      'F7040C'
     C                   PARM                    X@CO#
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQST
     C                   PARM                    QQEFDT
     C                   PARM      ' '           QQRTCD            1
     C*
     C* CHECK FOR SUCCESSFUL RETURN--SELECTOR IS VALID & LIST SUBMITTED
     C*
     C     QQRTCD        IFNE      '*'                                          UNSUCCESSFUL RETURN
     C                   MOVE      '1'           *IN21                          FLAG ERROR
     C                   GOTO      STRT                                         TRY AGAIN
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
**  QCMDEXC TABLE
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
