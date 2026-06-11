     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7380R5 - PROMPT FOR EMPLOYER A/R DETAIL LISTING
     H****************************************************************
     H**    PROGRAM DESCRIBTION:
     H**       THIS PROGRAM PROMPTS FOR FUND YEAR, AGENT #, EMPL#,
     H**       DIVISION, DATE RANGE, SORT SEQUENCE, AND VALID A/R
     H**       CODES. THIS PROGRAM PRODUCES ONE OR ALL AGENTS,
     H**       ONE OR ALL EMPLOYERS, SELECTED A/R CODES OR ALL CODES.
     H**
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/19/99 C:33 PROJECT WC899                                  *
     H*                ADD ENTRY OF POLICY COMMISSION PLAN TYPE -     *
     H*                A=ALL POLICIES, E=EXCLUDE ACCT CURRENT, C=ACCT *
     H*                CURRENT ONLY                                   *
     H*                                                               *
     H* 03/24/99  C:87  PROJECT: WC890                                *
     H*                 Add processing for flag to print only those   *
     H*                 emps w/in date range.  Add parm to pass to    *
     H*                 CL program.                                   *
     H*                                                               *
     H*  05/04/93 C04  PROJECT WC233                                  *
     H*                ALLOW INACTIVE CODES TO BE SELECTED FROM PROMPT*
     H*                                                               *
     H*  09/12/97 C78  PROJECT WC577                                  *
     H*                SCR01 CHANGED TO MEET PDS NEW SCREEN STANDARDS *
     H*                                                               *
     H****************************************************************
     FW7380F5   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWMAAL1    IF   E           K DISK
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)
     FWTABL     IF   E           K DISK
     F***  BEGIN ADD  ***  07/19/99  *********************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F***  END   ADD  ***  07/19/99  *********************************

     D SCD             S             10    DIM(30) CTDATA PERRCD(1) ASCEND      A/R SCRN FLDS
     D DBFDS           DS
     D  DBRRN                397    400B 0
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
      **** BEGIN CHG **** 09/12/97 ************************************
     C*********************KFLD           XXFYR   30
     C                   KFLD                    XXFYR             3 0
      **** END   CHG **** 09/12/97 ************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  AGENT ASSIGNMENT FILE BY FUND/AGENT
     C*
     C     AAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCAGT#
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    EMDT          XXFDT
     C     *LIKE         DEFINE    EMDT          XXTDT
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
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   CLEAR                   WCFL15
     C     'WC'          CHAIN     SWC01                              60
     C     WCFL15        COMP      'Y'                                    96
     C  N96              MOVE      'A'           SCPLAN
     C***  END   ADD  ***  07/19/99  *********************************
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
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C                   MOVE      *ZEROS        SCAGT#
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C                   MOVE      *ZEROS        SCFDT
     C                   MOVE      *ZEROS        SCTDT
     C                   MOVE      *BLANKS       SCSORT
     C                   MOVE      *BLANKS       CD
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*     ERROR INDICATORS FOR CODES ARE *IN20 - *IN26, *IN65 - *IN70
     C*     POSITION CURSOR INDICATORS ARE *IN27 - *IN56
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
     C                   GOTO      STRT
     C                   END
      *** BEGIN CHG **** 09/12/97 *************************************
     C***
     C*** COMMAND 05:  AGENT SEARCH
     C***
     C********   *INKE     IFEQ '1'
     C********   Q2RECF    IFEQ 'SCR01'
      *
      *   F10 = SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
      *   AGENT SEARCH
      *
     C     HFLD          IFEQ      'SCAGT#'
     C*
      *** END   CHG **** 09/12/97 *************************************
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PAA02                              99
     C*
     C                   SETON                                        71
     C                   MOVE      AAAGT#        SCAGT#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
      **** BEGIN DEL ***** 09/12/97 ***********************************
     C*****************    END
      **** END   DEL ***** 09/12/97 ***********************************
     C*
     C                   GOTO      STRT
     C                   END                                                    HFLD IFEQ
     C*
      *** BEGIN CHG **** 09/12/97 *************************************
     C*
     C*  EMPLOYER SEARCH
     C*
     C*
     C********AND 07: EMPLOYER SEARCH
     C*
     C*********  *INKG     IFEQ '1'
     C*********  Q2RECF    ANDEQ'SCR01'
     C     HFLD          IFEQ      'SCEMP#'
      *** END   CHG **** 09/12/97 *************************************
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    HFLD
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
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
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
     C     X             ADD       26            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     X             ADD       27            X1                2 0
     C*
     C     X1            IFNE      57
     C*
     C                   MOVE      '1'           *IN(X1)
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
      **** BEGIN ADD ***** 09/12/97 ***********************************
     C                   END                                                    F10 SEARCH
      **** END   ADD ***** 09/12/97 ***********************************
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
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C*********  F1KEY     CHAINPF101                20
     C*********  *IN20     CABEQ'1'       STRT
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C********   F1KEY     CHAINPF101                20
     C********   *IN20     CABEQ'1'       STRT
     C*
     C*  VALIDATE THE AGENT# FOR THE FUND
     C*
     C     SCAGT#        IFNE      0
     C     AAKEY         CHAIN     PAA01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE EMPLOYER/DIVISION IF ENTERED
     C*
     C     SCEMP#        IFNE      *ZERO
     C     EMKEY         CHAIN     PEM02                              57
     C     *IN57         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED FROM DATE
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN58
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
     C     SCTDT         IFEQ      *ZERO
     C                   MOVE      UPPRDT        SCTDT
     C                   END
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN63
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATES FOR ASCENDING ORDER
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C     XXFDT         CABGT     XXTDT         STRT                 59
     C*
     C***  BEGIN ADD  ***  03/24/99  **********************************
     C*  VALIDATE PRINT FLAG IS 'Y' OR *BLANK
     C     SCPRNT        IFNE      'Y'
     C     SCPRNT        ANDNE     *BLANK
     C                   MOVE      *ON           *IN74
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPRNT<>'Y'
     C***  END   ADD  ***  03/24/99  **********************************
     C*
     C*  VALIDATE SORT SELECTION - MUST BE "A" OR "E"
     C*
     C     SCSORT        IFNE      'E'
     C     SCSORT        ANDNE     'A'
     C     SCSORT        ANDNE     'N'
     C                   MOVE      '1'           *IN64
     C                   GOTO      STRT
     C                   END
     C**** BEGIN ADD **** 07/19/99 ************************************
     C*
     C*  VALIDATE PLAN SELECTION - MUST BE "A","E", OR "C"
     C*
     C     SCPLAN        IFNE      'E'
     C     SCPLAN        ANDNE     'A'
     C     SCPLAN        ANDNE     'C'
     C                   MOVE      *ON           *IN74
     C                   GOTO      STRT
     C                   ENDIF
     C**** END   ADD **** 07/19/99 ************************************
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
     C     *IN40         IFEQ      '1'
     C     XXCOL         ADD       21            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCAGT#        QQAGT#            5
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCFDT         QQFDT             8
     C                   MOVE      SCTDT         QQTDT             8
     C                   MOVE      SCSORT        QQSORT            1
     C                   MOVEA     CD            QQCODE           60
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C***  BEGIN ADD  ***  03/24/99  **********************************
     C                   MOVE      SCPRNT        QQPRNT            1
     C***  END   ADD  ***  03/24/99  **********************************
     C*
     C                   CALL      'W7380C5'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQAGT#
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQSORT
     C                   PARM                    QQCODE
     C                   PARM                    QQOUTQ
     C***  BEGIN ADD  ***  03/24/99  **********************************
     C                   PARM                    QQPRNT
     C***  END   ADD  ***  03/24/99  **********************************
     C**** BEGIN ADD **** 07/19/99 ************************************
     C                   PARM      SCPLAN        QQPLAN            1
     C**** END   ADD **** 07/19/99 ************************************
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
