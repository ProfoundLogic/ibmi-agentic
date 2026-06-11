     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0611R - WORKERS COMP A/R DETAIL SEARCH
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM IS USED TO DISPLAY THE A/R DETAIL INFORMATION    *
     H* FOR THE DATE RANGE SPECIFIED IN THE A/R SUMMARY SEARCH        *
     H* (W0610R).                                                     *
     H* ONLY TRANSACTIONS WITH A POSTING DATE WITHIN THE SPECIFIED    *
     H* DATE RANGE ARE SHOWN.                                         *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W0611R                                                      *
     H* CALLS TO:                                                     *
     H*   W0550R                                                      *
     H*   W0551R                                                      *
     H*   W0006R                                                      *
     H*   M0006C                                                      *
     H*   SRDCK                                                       *
     H*                                                               *
     H* PROGRAMMER: C:09                                              *
     H* CREATED: 10/12/94               PROJECT: WC326                *
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*   10       HELP KEY                                           *
     H*   11       VALID COMMAND KEY                                  *
     H*   12       PROGRAM NOT CALLED FROM THE MENU                   *
     H*   18       ROLL UP                                            *
     H*   19       ROLL DOWN                                          *
     H*   21       INITIAL LOAD OF DETAIL SCREEN                      *
     H*   60       TEMPORARY WORK INDICATOR                           *
     H*   61       ARRAY LOAD CONTROL INDICATOR                       *
     H*                                                               *
     H*  SCR01:                                                       *
     H*   20       INVALID ROLL REQUEST                               *
     H*   78       CANCEL DATE                                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/09/95 C:38 PROJECT WC440                                   *
     H*               ALLOW ACCESS TO DARTS USING FUNCTION KEY 14 AT  *
     H*               DETAIL SCREEN (CO,FND,EMP,DIV CHANGES).         *
     H* 07/24/96 C:59 PROJECT WC541                                   *
     H*               ADDED CANCEL  DATE TO DISPLAY FILE              *
     H*****************************************************************
     FW0611F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMEAL7    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D FL              S              1    DIM(11)
     D TM              S              6  0 DIM(11)
     D KEYD            S              8  0 DIM(2)
     D KEYT            S              6  0 DIM(2)
     D KEYR            S              7  0 DIM(2)
     D TR              S              7  0 DIM(11)
     D RR              S              9    DIM(3) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  TD                     1    088  0
     D                                     DIM(11)
     D  SCTD01               001    008  0
     D  SCTD02               009    016  0
     D  SCTD03               017    024  0
     D  SCTD04               025    032  0
     D  SCTD05               033    040  0
     D  SCTD06               041    048  0
     D  SCTD07               049    056  0
     D  SCTD08               057    064  0
     D  SCTD09               065    072  0
     D  SCTD10               073    080  0
     D  SCTD11               081    088  0
     D                 DS
     D  DT                     1    088  0
     D                                     DIM(11)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D                 DS
     D  AR                     1     22
     D                                     DIM(11)
     D  SCAR01                 1      2
     D  SCAR02                 3      4
     D  SCAR03                 5      6
     D  SCAR04                 7      8
     D  SCAR05                 9     10
     D  SCAR06                11     12
     D  SCAR07                13     14
     D  SCAR08                15     16
     D  SCAR09                17     18
     D  SCAR10                19     20
     D  SCAR11                21     22
     D                 DS
     D  TP                     1     11
     D                                     DIM(11)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D  SCTP10                10     10
     D  SCTP11                11     11
     D                 DS
     D  CK                     1     77  0
     D                                     DIM(11)
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D  SCCK04                22     28  0
     D  SCCK05                29     35  0
     D  SCCK06                36     42  0
     D  SCCK07                43     49  0
     D  SCCK08                50     56  0
     D  SCCK09                57     63  0
     D  SCCK10                64     70  0
     D  SCCK11                71     77  0
     D                 DS
     D  DS                     1    297
     D                                     DIM(11)
     D  SCDS01                 1     27
     D  SCDS02                28     54
     D  SCDS03                55     81
     D  SCDS04                82    108
     D  SCDS05               109    135
     D  SCDS06               136    162
     D  SCDS07               163    189
     D  SCDS08               190    216
     D  SCDS09               217    243
     D  SCDS10               244    270
     D  SCDS11               271    297
     D                 DS
     D  AM                     1     99  2
     D                                     DIM(11)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*                                      40  40 QCQTE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  EMPLOYER A/R MASTER FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDATE
     C                   KFLD                    XXTIME
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EADT          XXDT
     C     *LIKE         DEFINE    EADT          XXTRDT
     C     *LIKE         DEFINE    EADT          XXDATE
     C     *LIKE         DEFINE    EATIME        XXTIME
     C     *LIKE         DEFINE    EADT          XXTODT
     C     *LIKE         DEFINE    EADT          XXFRDT
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQYR              3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQAM              9
     C                   PARM                    QQCHK#            7
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
     C     QQCALL        IFEQ      '2'
     C                   MOVE      '1'           *IN12
     C                   END
     C*
     C                   Z-ADD     0             XXFYR
     C                   MOVE      QQYR          XXFYR
     C                   Z-ADD     0             SCEMP#
     C                   Z-ADD     0             SCDIV
     C                   Z-ADD     0             SCYR
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
     C                   MOVE      QQYR          SCYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFRDT        SCFRDT
     C                   MOVE      QQTODT        SCTODT
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      QQFRDT        SCDT
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     XXCO#         IFEQ      *ZERO
     C                   Z-ADD     UPCO#         XXCO#
     C                   ENDIF                                                  XXCO#=*ZERO
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     XXFND         IFEQ      *ZERO
     C                   Z-ADD     UPFND         XXFND
     C                   ENDIF                                                  XXFND=*ZERO
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C     XXFYR         IFEQ      *ZERO
     C                   Z-ADD     UPFYR         XXFYR
     C                   END
     C*
     C*  GET THE EMPLOYER RECORD
     C*
     C     EMKY          CHAIN     WMEML                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
     C*
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    CD(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C     Y             IFEQ      0
     C     CD(Y)         ORLT      ED(X)
     C                   MOVE      '0'           *IN78                          ACTIVE
     C                   ELSE
     C                   MOVE      '1'           *IN78                          CANCELLED
     C*
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C*
     C                   END
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      *ON           *IN21
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVEL     RR(3)         SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQFND
     C                   MOVE      *BLANKS       QQYR
     C                   MOVE      *BLANKS       QQFRDT
     C                   MOVE      *BLANKS       QQTODT
     C                   MOVE      *BLANKS       QQEMP#
     C                   MOVE      *BLANKS       QQDIV
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQFND
     C                   MOVE      *BLANKS       QQYR
     C                   MOVE      *BLANKS       QQFRDT
     C                   MOVE      *BLANKS       QQTODT
     C                   MOVE      *BLANKS       QQEMP#
     C                   MOVE      *BLANKS       QQDIV
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C*  PROCESS HELP REQUESTS
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  CMD 14 - COMMENTS SYSTEM           *
     C*
     C     *INKN         IFEQ      '1'
     C*
     C*  RETAIN SCREEN NUMBER
     C*
     C                   MOVE      Q2RECF        HLRECF           10
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C*                    MOVE *BLANKS   QCQTE
     C*          QQQTE     IFEQ 'Q'
     C*                    MOVE 'Q'       QCQTE
     C*                    END
     C*
     C*  CALL COMMENT PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      HLRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKN
     C                   END
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYD(2)       X1DATE            8 0
     C                   Z-ADD     KEYT(2)       XXTIME
     C                   Z-ADD     KEYR(2)       XXTRDT
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     EAKY2         CHAIN     PEA01                              60
     C     EAKY1         READE     PEA01                                  60
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDGT     XXTODT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYD(1)       X1DATE
     C                   Z-ADD     KEYT(1)       XXTIME
     C                   Z-ADD     KEYR(1)       XXTRDT
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     EAKY2         CHAIN     PEA01                              60
     C     EAKY1         READPE    PEA01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDLT     XXFRDT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCDT          IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        KEYD
     C                   MOVE      *BLANK        KEYT
     C                   MOVE      *BLANK        KEYR
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   MOVE      *ZEROS        SCDT
     C     XXDT          IFLE      XXFRDT
     C                   Z-ADD     XXFRDT        XXDT
     C                   MOVE      '1'           XXDOWN
     C                   ENDIF
     C     EAKY          SETLL     PEA01
     C     EAKY1         READE     PEA01                                  60
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDGT     XXTODT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       10            I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     11
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      AM(I1)        QQAM
     C                   MOVE      CK(I1)        QQCHK#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQEMP#
     C                   MOVE      *BLANKS       QQDIV
     C                   MOVE      *BLANKS       QQFND
     C                   MOVE      *BLANKS       QQYR
     C                   MOVE      *BLANKS       QQFRDT
     C                   MOVE      *BLANKS       QQTODT
     C                   MOVE      *BLANKS       QQAM
     C                   MOVE      *BLANKS       QQCHK#
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       TD
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       AR
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *BLANKS       CK
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       TM
     C                   MOVE      *BLANKS       TR
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TD(I1)
     C*
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   MOVE      EAARCD        AR(I1)
     C                   MOVE      EAARTY        TP(I1)
     C                   MOVE      EACHK#        CK(I1)
     C                   MOVEL     EADESC        DS(I1)
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C                   SUB       EAAMT         AM(I1)
     C                   ELSE
     C                   MOVE      EAAMT         AM(I1)
     C                   END
     C                   Z-ADD     EATIME        TM(I1)
     C                   Z-ADD     EATRDT        TR(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     REREAD        TAG
     C     EAKY1         READE     PEA01                                  60
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDGT     XXTODT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      11                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     DT(11)        KEYD(2)
     C                   Z-ADD     TM(11)        KEYT(2)
     C                   Z-ADD     TR(1)         KEYR(1)
     C                   Z-ADD     TR(11)        KEYR(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     12            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C     REREDP        TAG
     C     EAKY1         READPE    PEA01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDLT     XXFRDT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     DT(11)        KEYD(2)
     C                   Z-ADD     TM(11)        KEYT(2)
     C                   Z-ADD     TR(1)         KEYR(1)
     C                   Z-ADD     TR(11)        KEYR(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     XXFRDT        XXDT
     C*
     C     EAKY          SETLL     PEA01
     C     EAKY1         READE     PEA01                                  60
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDGT     XXTODT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     REREDB        TAG
     C     EAKY1         READE     PEA01                                  60
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDGT     XXTODT
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      11                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
  More...

   Bottom
