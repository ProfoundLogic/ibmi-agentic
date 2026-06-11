     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0613R - EMPLOYER A/R SUMMARY SEARCH
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/30/14 C:62 Support: OP140 86039                           *
     H*                Update only when ready to update               *
     H*                                                               *
     H*  01/21/98 C:61 PROJECT: 91WBD                                 *
     H*                MODIFIED FROM V3.                              *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
      *
      *
     FW0613F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
      *
     FWMEAL7    UF   E           K DISK
      *
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
      *
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
      *
     FWDELL1    IF   E           K DISK
      *
     FSMCOL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FSSYLF     IF   E           K DISK
      *
      *

     D TM              S              6  0 DIM(9)
     D KEYD            S              8  0 DIM(2)
     D KEYT            S              6  0 DIM(2)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D OPTNM           C                   CONST('A/R Inquiry (late st-
     D                                     atus)')
     D AGTAPI          C                   CONST('WA002R')
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  TD                     1     72  0
     D                                     DIM(9)
     D  SCTD01               001    008  0
     D  SCTD02               009    016  0
     D  SCTD03               017    024  0
     D  SCTD04               025    032  0
     D  SCTD05               033    040  0
     D  SCTD06               041    048  0
     D  SCTD07               049    056  0
     D  SCTD08               057    064  0
     D  SCTD09               065    072  0
     D                 DS
     D  DT                     1     72  0
     D                                     DIM(9)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D                 DS
     D  AR                     1     18
     D                                     DIM(9)
     D  SCAR01                 1      2
     D  SCAR02                 3      4
     D  SCAR03                 5      6
     D  SCAR04                 7      8
     D  SCAR05                 9     10
     D  SCAR06                11     12
     D  SCAR07                13     14
     D  SCAR08                15     16
     D  SCAR09                17     18
     D                 DS
     D  SL                     1     18
     D                                     DIM(9)
     D  SCSL01                 1      2
     D  SCSL02                 3      4
     D  SCSL03                 5      6
     D  SCSL04                 7      8
     D  SCSL05                 9     10
     D  SCSL06                11     12
     D  SCSL07                13     14
     D  SCSL08                15     16
     D  SCSL09                17     18
     D                 DS
     D  TP                     1      9
     D                                     DIM(9)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D                 DS
     D  LF                     1      9
     D                                     DIM(9)
     D  SCLF01                 1      1
     D  SCLF02                 2      2
     D  SCLF03                 3      3
     D  SCLF04                 4      4
     D  SCLF05                 5      5
     D  SCLF06                 6      6
     D  SCLF07                 7      7
     D  SCLF08                 8      8
     D  SCLF09                 9      9
     D                 DS
     D  DS                     1    225
     D                                     DIM(9)
     D  SCDS01                 1     25
     D  SCDS02                26     50
     D  SCDS03                51     75
     D  SCDS04                76    100
     D  SCDS05               101    125
     D  SCDS06               126    150
     D  SCDS07               151    175
     D  SCDS08               176    200
     D  SCDS09               201    225
     D                 DS
     D  AM                     1     81  2
     D                                     DIM(9)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  KEY LIST FOR THE GROUP MASTER FILE
      *

     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  EMPLOYER A/R FILE
      *
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT
      *
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDATE
     C                   KFLD                    XXTIME
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
      *
      *
      *
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    EADT          XXDT
     C     *LIKE         DEFINE    EADT          XXDATE
     C     *LIKE         DEFINE    EADT          XXDAT
     C     *LIKE         DEFINE    EATIME        XXTIME
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    EAARTY        XXARTY
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EAPGM         QQDPGM
     C     *LIKE         DEFINE    EACO#         ##CO#
     C     *LIKE         DEFINE    EAFND         ##FND
     C     *LIKE         DEFINE    EAEMP#        ##EMP#
     C     *LIKE         DEFINE    EADIV         ##DIV
     C     *LIKE         DEFINE    DSTIME        SVTIME
      *
      *  DEFINE DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
      *
      *  INITIALIZE VARIABLES
      *
     C                   Z-ADD     0             SCCNDT                         CANCEL DATE
      *
      *
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    QQCALL            1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM      XXCO#         ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *
      *
     C                   Z-ADD     *ZERO         SCYR
     C                   Z-ADD     *ZERO         SCEMP#
     C                   Z-ADD     *ZERO         SCDIV
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
      *
      *  GET THE USER PARAMETER FILE
      *
     C     TOP           TAG
      *
     C     UPKY          CHAIN     PUP01                              60
     C   60              Z-ADD     *ZERO         UPCO#
     C   60              Z-ADD     *ZERO         UPFND
      *
     C     XXCO#         IFEQ      *ZERO
     C                   Z-ADD     UPCO#         XXCO#
     C                   ENDIF                                                   XCO#=*ZERO
      *
     C     XXFND         IFEQ      *ZERO
     C                   Z-ADD     UPFND         XXFND
     C                   ENDIF                                                  XXFND=*ZERO
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
     C     SCYR          IFEQ      *ZERO
     C                   Z-ADD     UPFYR         SCYR
     C                   ENDIF                                                  SCYR=*ZERO
      *
     C     SCEMP#        IFEQ      *ZERO
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
     C                   ENDIF                                                  SCEMP#=*ZERO
      *
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL06        COMP      'Y'                                    75
      *
      *  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *  DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR02'
      *
     C     XXUP          IFEQ      '1'
     C                   MOVEL     RR(2)         SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
      *
     C                   EXFMT     SCR02
     C                   END
      *
      *  PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *  F3=EXIT
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *  F12=CANCEL
      *
     C     *INKL         IFEQ      *ON
      *
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
      *
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSL
     C                   ENDIF                                                  INKL
      *
      *  F10=EMPLOYER NUMERIC SEARCH
      *
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
      * ZZZ
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   SELECT
      *
     C     *INKC         WHENEQ    *ON
     C                   GOTO      TERM
      *
     C     *INKL         WHENEQ    *OFF
     C     @@RRN         ANDNE     *ZERO
     C     @@RRN         CHAIN     WMEMP                              60
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
      *
     C                   OTHER
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  Q2RECF
     C                   ENDIF                                                  INKJ
     C*
     C*  F13=PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   Z-ADD     *ZERO         XXEMP#
     C                   Z-ADD     *ZERO         XXDIV
     C                   Z-ADD     *ZERO         XXFYR
     C                   Z-ADD     *ZERO         SCEMP#
     C                   Z-ADD     *ZERO         SCDIV
     C                   Z-ADD     *ZERO         SCYR
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
      *
      *  ROLL UP REQUEST
      *
     C     *IN18         CABEQ     '1'           SCR02T
      *
      *  ROLL DOWN REQUEST
      *
     C     *IN19         CABEQ     '1'           SCR02T
      *
      *  PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      '1'
      *
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
      *
     C                   END
      *
      *  GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      *
      *  PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  YEAR CANNOT BE LESS THAN OR EQUAL TO ZERO
      *
     C     SCYR          CABLE     0             STRT                   2020
      *
      *  EMPLOYER AND DIVISION NUMBERS MUST BE VALID
      *
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C     EMKY          CHAIN     WMEML                              21
     C     *IN21         CABEQ     '1'           STRT
      *
      *  ALL EDITS PASSED, LOAD EMP NAME, CANCEL DATE(IF APPLICABLE),
      *  & CALC CURRENT DUE FOR SCR02
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
      *  IF EMPLOYER CANCELLED, GET CANCELLATION DATE
      *
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
      *
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    CD(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
      *
     C     Y             IFEQ      0
     C     CD(Y)         ORLT      ED(X)
      *
      *
     C                   MOVE      *OFF          *IN71                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN71                          CANCELLED
      *
      *
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C                   END
     C*
     C                   Z-ADD     SCYR          XXFYR                          JJJ 1-31-89
      *
      *  BEGIN CALCULATION FOR DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 = OFF
      *
      *  END CALCULATION FOR DIVISION COUNT
      *
      *
      *  GET AGENT/PRODUCER INFORMATION
      *
     C                   Z-ADD     *ZEROS        ##FYR
      *
     C                   CALL      AGTAPI        AGTPL
      *
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      *
      *
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN21
     C                   Z-ADD     0             SCDT
     C                   GOTO      REPOS
      *
     C     SCR02T        TAG
      *
      *  INVALID ROLL REQUEST
      *
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
      *  ROLL UP REQUEST
      *
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYD(2)       X1DATE            8 0
     C                   Z-ADD     KEYT(2)       XXTIME
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY2         CHAIN     PEA01                              60
     C*****EAKY1         READE     PEA01                                  60
     C     EAKY2         CHAIN(N)  PEA01                              60
     C     EAKY1         READE(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
      *
      *  ROLL DOWN REQUEST
      *
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYD(1)       X1DATE
     C                   Z-ADD     KEYT(1)       XXTIME
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY2         CHAIN     PEA01                              60
     C**N60              READP     PEA01                                  60
     C     EAKY2         CHAIN(N)  PEA01                              60
     C  N60              READP(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C  N60UPCO#         COMP      EACO#                              6060
     C  N60UPFND         COMP      EAFND                              6060
     C  N60XXEMP#        COMP      EAEMP#                             6060
     C  N60XXDIV         COMP      EADIV                              6060
     C  N60XXFYR         COMP      EAFYR                              6060
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
      *
      *  REPOSITION FILE AT REQUESTED RECORD
      *
     C     SCDT          IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        KEYD
     C                   MOVE      *BLANK        KEYT
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   MOVE      *ZEROS        SCDT
     C     EAKY          SETLL     PEA01
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY1         READE     PEA01                                  60
     C     EAKY1         READE(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
      *
      *  UPDATE LATE STATUS
      *
     C                   MOVEL(P)  'N'           OPTION            1
     C                   DO        9             I3                3 0
     C     SL(I3)        IFEQ      '7 '
     C     SL(I3)        OREQ      ' 7'
     C     SL(I3)        OREQ      '07'
     C                   EXSR      OPT7
     C                   MOVEL     'Y'           OPTION
     C                   ENDIF
     C                   ENDDO
      *
     C     OPTION        IFEQ      'Y'
      *
      *  REDISPLAY SCREEN
      *
     C                   MOVEL     *BLANKS       XXDOWN
     C                   Z-ADD     KEYD(1)       X1DATE            8 0
     C                   Z-ADD     KEYT(1)       XXTIME
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     EAKY2         SETLL     PEA01
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY1         READE     PEA01                                  60
     C     EAKY1         READE(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C                   EXSR      LOAD
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *  CLEAR SCREEN ARRAYS
      *
     C     CLR           BEGSR
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   MOVE      *BLANKS       TD
     C                   MOVE      *BLANKS       LF
     C                   MOVE      *BLANKS       SL
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       AR
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       TM
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
      *
      *  FILL SCREEN ARRAYS
      *
     C     FILL          BEGSR
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TD(I1)
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVE      EAARCD        AR(I1)
     C                   MOVE      EAARTY        TP(I1)
     C                   MOVE      EALATE        LF(I1)
     C                   MOVEL(P)  EADESC        DS(I1)
     C                   MOVE      EAAMT         AM(I1)
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'
     C     EAARTY        OREQ      'I'
     C     AM(I1)        MULT      -1            AM(I1)
     C                   END
     C                   Z-ADD     EATIME        TM(I1)
     C     I1            ADD       30            I2                3 0
     C                   MOVE      '1'           *IN(I2)
     C                   ENDSR
      *
      *  LOAD FORWARD SUBROUTINE
      *
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
      *
      *  END OF FILE
      *
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
      *
      *  FILL SCREEN AND GET NEXT RECORD
      *
     C                   ADD       1             I1
     C                   EXSR      FILL
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY1         READE     PEA01                                  60
     C     EAKY1         READE(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      9                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
      *
     C                   Z-ADD     DT(1)         KEYD(1)
      *
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     DT(9)         KEYD(2)
     C                   Z-ADD     TM(9)         KEYT(2)
     C                   ENDSR
      *
      *  LOAD BACKWARD SUBROUTINE
      *
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     10            I1
     C     OVERP         TAG
      *
      *  BEGINNING OF FILE
      *
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
      *
      *  FILL SCREEN AND GET NEXT RECORD
      *
     C                   SUB       1             I1
     C                   EXSR      FILL
     C*** Begin Chg *** 06/30/14 **************************************************
     C**********         READP     PEA01                                  60
     C                   READP(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C  N60EACO#         COMP      UPCO#                              6060
     C  N60EAFND         COMP      UPFND                              6060
     C  N60EAEMP#        COMP      XXEMP#                             6060
     C  N60EADIV         COMP      XXDIV                              6060
     C  N60EAFYR         COMP      XXFYR                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     DT(9)         KEYD(2)
     C                   Z-ADD     TM(9)         KEYT(2)
     C                   ENDSR
      *
      *  LOAD FROM BEGINNING OF FILE SUBROUTINE
      *
     C     LOADB         BEGSR
     C                   EXSR      CLR
      *
      *  POSITION AT BEGINNING OF FILE
      *
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C     EAKY1         SETLL     PEA01
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY1         READE     PEA01                                  60
     C     EAKY1         READE(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
      *
      *  FILL SCREEN AND GET NEXT RECORD
      *
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY1         READE     PEA01                                  60
     C     EAKY1         READE(N)  PEA01                                  60
     C*** End   Chg *** 06/30/14 **************************************************
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      9                                    61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
      *
      *
      *
      ****************************************************************
      *  OPT7 - CHANGE LATE STATUS AND UPDATE RECORD
      ****************************************************************
      *
     C     OPT7          BEGSR
      *
      *  GET POSTING DATE FOR RECORD TO BE UPDATED
      *
     C                   Z-ADD     DT(I3)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
      *  ENSURE UNIQUE FILE KEY FOR RECORD TO BE UPDATED
      *  GET UNIQUE TIME FOR GIVEN EAPODT
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
      *
     C     *IN60         DOUEQ     '1'
      *
     C*** Begin Chg *** 06/30/14 **************************************************
     C*****EAKY2         CHAIN     PEA01                              60
     C     EAKY2         CHAIN(N)  PEA01                              60
     C*** End   Chg *** 06/30/14 **************************************************
      *
     C     *IN60         IFEQ      '1'
     C                   LEAVE
     C                   ELSE
     C                   ADD       1             XXTIME
     C     XXTIME        IFEQ      999999
     C                   Z-ADD     *ZERO         XXTIME
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   Z-ADD     XXTIME        SVTIME
      *
      *  GET RECORD TO BE UPDATED
      *
     C                   Z-ADD     TM(I3)        XXTIME
     C     EAKY2         CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
      *
      *  TOGGLE LATE FLAG
      *
     C     EALATE        IFEQ      'Y'
     C                   MOVEL(P)  *BLANK        EALATE
     C                   ELSE
     C                   MOVEL(P)  'Y'           EALATE
     C                   ENDIF
      *
      *  UPDATE RECORD
      *
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     SVTIME        EATIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
      *
     C                   UPDATE    PEA01
      *
     C                   ENDIF                                                  EAKY2
      *
     C                   ENDSR
**
  More...
   Bottom
