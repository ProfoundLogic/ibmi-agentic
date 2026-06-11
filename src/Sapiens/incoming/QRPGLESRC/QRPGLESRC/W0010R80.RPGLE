     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H****************************************************************
     H/TITLE W0010R80 - Client Control File Maintenance
     H****************************************************************
     H* DESCRIPTION:                                                 *
     H*   This pgm allows the user to enter control file information *
     H*   for a WCMS Group.  The field usage is defined using WMHXP  *
     H*   (Client Control Heading File).                             *
     H*                                                              *
     H* CALLED FROM:                                                 *
     H*   Menu (for default value)                                   *
     H*   W0010R - Work with Groups                                  *
     H*                                                              *
     H* CALLS TO:                                                    *
     H*                                                              *
     H*                                                              *
     H* CREATED:  10/22/97   C:33   Project:  60APC                  *
     H*                                                              *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  10/11/06  C:33  Seq#: WA099/41801                           *
     H*                  Use SC305R instead of SC580R.               *
     H*                                                              *
     H*  10/27/97  C:33  PROJECT: 60APC                              *
     H*                 -Use KLIST on header file to prior to using  *
     H*                  default CO# record                          *
     H*                 -Add edits on security levels                *
     H****************************************************************
     H*  INDICATORS USED:                                            *
     H*                                                              *
     H*   21 - 40  Edit Field Indicators                             *
     H*   41       Errors                                            *
     H*   42       Position Cursor                                   *
     H*   43       Security Levels not entered                       *
     H*   44       Security Types invalid                            *
     H*   60       WMCXL record found/not found                      *
     H*   68       Add mode                                          *
     H*   69       Defualt record allow F13                          *
     H*   71 - 90  Display  Indicators                               *
     H*   99       Record previously updated                         *
     H*                                                              *
     H****************************************************************
     H* FILE USAGE:                                                  *
     H*                                                              *
     H*    SMCOL  (PCO01): CO#                                       *
     H*    WMUPL  (PUP01): USR                                       *
     H*    WMHXL  (PHX01): CO#                                       *
     H*    WMXCL  (PXC01): CO#/FND                                   *
     H*    WMFNL  (PFN01): CO#/FND                                   *
     H*    STCTL1 (PCT01): RMCD                                      *
     H*                                                              *
     H****************************************************************
     FW0010F80  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter File
     FWMUPL     IF   E           K DISK    USROPN
     F*
     F* Client Control Heading File
     FWMHXL     IF   E           K DISK
     F*
     F* Client Control File
     FWMXCL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMXCP     UF A E             DISK
     F                                     RENAME(PXC01:DBRCDF)
     F*
     F* WCMS Group Master File
     FWMFNP     IF   E             DISK
     F*
     F* DARTS Remark Code File
     FSTCTL1    IF   E           K DISK
     F*_______________________________________________________________

     D*
     D SCD             S              1    DIM(36) CTDATA PERRCD(36) ASCEND     Security Codes
     D***  BEGIN ADD  ***  10/27/97  *********************************
     D***  END   ADD  ***  10/27/97  *********************************
     D*_______________________________________________________________
     D*
     D OPTNM           C                   CONST('Client Control Infor-
     D                                     mation')
     D DFLT            C                   CONST('Default CO# Record')
     D NOHDR           C                   CONST('There are no header -
     D                                     records for this Com-
     D                                     pany')
     D DOTS            C                   CONST(' . . . . . . . . . -
     D                                     . . . . . . . . .')
     D*_______________________________________________________________
     D*
     D                 DS
     D  STY                    1     20
     D                                     DIM(20)                              Security Type
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D  SCST11                11     11
     D  SCST12                12     12
     D  SCST13                13     13
     D  SCST14                14     14
     D  SCST15                15     15
     D  SCST16                16     16
     D  SCST17                17     17
     D  SCST18                18     18
     D  SCST19                19     19
     D  SCST20                20     20
     D***  BEGIN ADD  ***  10/27/97  *********************************
     D                 DS
     D  SLV                    1     60  0
     D                                     DIM(20)                              Security Level
     D  SCSL01                 1      3  0
     D  SCSL02                 4      6  0
     D  SCSL03                 7      9  0
     D  SCSL04                10     12  0
     D  SCSL05                13     15  0
     D  SCSL06                16     18  0
     D  SCSL07                19     21  0
     D  SCSL08                22     24  0
     D  SCSL09                25     27  0
     D  SCSL10                28     30  0
     D  SCSL11                31     33  0
     D  SCSL12                34     36  0
     D  SCSL13                37     39  0
     D  SCSL14                40     42  0
     D  SCSL15                43     45  0
     D  SCSL16                46     48  0
     D  SCSL17                49     51  0
     D  SCSL18                52     54  0
     D  SCSL19                55     57  0
     D  SCSL20                58     60  0
     D***  END   ADD  ***  10/27/97  *********************************
     D                 DS
     D  REM                    1    100
     D                                     DIM(20)                              DARTS Remark CD
     D  SCRM01                 1      5
     D  SCRM02                 6     10
     D  SCRM03                11     15
     D  SCRM04                16     20
     D  SCRM05                21     25
     D  SCRM06                26     30
     D  SCRM07                31     35
     D  SCRM08                36     40
     D  SCRM09                41     45
     D  SCRM10                46     50
     D  SCRM11                51     55
     D  SCRM12                56     60
     D  SCRM13                61     65
     D  SCRM14                66     70
     D  SCRM15                71     75
     D  SCRM16                76     80
     D  SCRM17                81     85
     D  SCRM18                86     90
     D  SCRM19                91     95
     D  SCRM20                96    100
     D                 DS
     D  DT8                    1     80  0
     D                                     DIM(10)                              Dates (SCREEN)
     D  SC6N01                 1      8  0
     D  SC6N02                 9     16  0
     D  SC6N03                17     24  0
     D  SC6N04                25     32  0
     D  SC6N05                33     40  0
     D  SC6N06                41     48  0
     D  SC6N07                49     56  0
     D  SC6N08                57     64  0
     D  SC6N09                65     72  0
     D  SC6N10                73     80  0
     D                 DS
     D  DT7                    1     70  0
     D                                     DIM(10)                              Dates (FILE)
     D  XC6N01                 1      7  0
     D  XC6N02                 8     14  0
     D  XC6N03                15     21  0
     D  XC6N04                22     28  0
     D  XC6N05                29     35  0
     D  XC6N06                36     42  0
     D  XC6N07                43     49  0
     D  XC6N08                50     56  0
     D  XC6N09                57     63  0
     D  XC6N10                64     70  0
     D                 DS
     D  SL                     1    500
     D                                     DIM(20)                              Sec Level Headers
     D  XHSL01                 1     25
     D  XHSL02                26     50
     D  XHSL03                51     75
     D  XHSL04                76    100
     D  XHSL05               101    125
     D  XHSL06               126    150
     D  XHSL07               151    175
     D  XHSL08               176    200
     D  XHSL09               201    225
     D  XHSL10               226    250
     D  XHSL11               251    275
     D  XHSL12               276    300
     D  XHSL13               301    325
     D  XHSL14               326    350
     D  XHSL15               351    375
     D  XHSL16               376    400
     D  XHSL17               401    425
     D  XHSL18               426    450
     D  XHSL19               451    475
     D  XHSL20               476    500
     D                 DS
     D  RM                     1    500
     D                                     DIM(20)                              Remark Code Headers
     D  XHRM01                 1     25
     D  XHRM02                26     50
     D  XHRM03                51     75
     D  XHRM04                76    100
     D  XHRM05               101    125
     D  XHRM06               126    150
     D  XHRM07               151    175
     D  XHRM08               176    200
     D  XHRM09               201    225
     D  XHRM10               226    250
     D  XHRM11               251    275
     D  XHRM12               276    300
     D  XHRM13               301    325
     D  XHRM14               326    350
     D  XHRM15               351    375
     D  XHRM16               376    400
     D  XHRM17               401    425
     D  XHRM18               426    450
     D  XHRM19               451    475
     D  XHRM20               476    500
     D                 DS
     D  FL                     1    500
     D                                     DIM(20)                              Flag Headers
     D  XHFL01                 1     25
     D  XHFL02                26     50
     D  XHFL03                51     75
     D  XHFL04                76    100
     D  XHFL05               101    125
     D  XHFL06               126    150
     D  XHFL07               151    175
     D  XHFL08               176    200
     D  XHFL09               201    225
     D  XHFL10               226    250
     D  XHFL11               251    275
     D  XHFL12               276    300
     D  XHFL13               301    325
     D  XHFL14               326    350
     D  XHFL15               351    375
     D  XHFL16               376    400
     D  XHFL17               401    425
     D  XHFL18               426    450
     D  XHFL19               451    475
     D  XHFL20               476    500
     D                 DS
     D  D10                    1    100
     D                                     DIM(5)                               10A Desc Headers
     D  XH10A1                 1     20
     D  XH10A2                21     40
     D  XH10A3                41     60
     D  XH10A4                61     80
     D  XH10A5                81    100
     D                 DS
     D  D40                    1    100
     D                                     DIM(5)                               40A Desc Headers
     D  XH40A1                 1     20
     D  XH40A2                21     40
     D  XH40A3                41     60
     D  XH40A4                61     80
     D  XH40A5                81    100
     D                 DS
     D  DTH                    1    250
     D                                     DIM(10)                              Date Headers
     D  XH6N01                 1     25
     D  XH6N02                26     50
     D  XH6N03                51     75
     D  XH6N04                76    100
     D  XH6N05               101    125
     D  XH6N06               126    150
     D  XH6N07               151    175
     D  XH6N08               176    200
     D  XH6N09               201    225
     D  XH6N10               226    250
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*  Work Field for Field Cursor Position (DARTS Remark Type)
     D                 DS
     D  WRKCSR                 1     10
     D  WRKPOS                 5      6  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0

     C*
     C     *LIKE         DEFINE    Q2RECF        SVRECF
     C     *LIKE         DEFINE    XCCO#         XXCO#
     C     *LIKE         DEFINE    XCFND         XXFND
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    *IN60         SECFLG
     C     *LIKE         DEFINE    *IN60         REMFLG
     C     *LIKE         DEFINE    *IN60         FLGFLG
     C     *LIKE         DEFINE    *IN60         DESFLG
     C     *LIKE         DEFINE    *IN60         DATFLG
     C*
     C*_______________________________________________________________
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C***  BEGIN ADD  ***  10/27/97  *********************************
     C*
     C*  Key list for Client Control Header File
     C*
     C     HXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  END   ADD  ***  10/27/97  *********************************
     C*
     C*  Key list for Client Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Key list for Group File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Key list for DARTS Remark Code File
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
     C*_______________________________________________________________
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
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
     C*_______________________________________________________________
     C*
     C     Q1PRMS        IFGE      4
     C*
     C*  GET THE FUND RECORD PASSED IN
     C*
     C     @@RRN         CHAIN     WMFNP                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      'Y'           XXDFLT
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      'N'           XXDFLT            1
     C                   MOVE      *OFF          *IN69
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ELSE
     C*
     C                   MOVE      'Y'           XXDFLT
     C                   MOVE      *ON           *IN69
     C                   ENDIF                                                  Q1PRMS>=3
     C*
     C     TOP           TAG
     C*
     C                   CLEAR                   SL
     C                   CLEAR                   RM
     C                   CLEAR                   FL
     C                   CLEAR                   D10
     C                   CLEAR                   D40
     C                   CLEAR                   DTH
     C                   MOVE      'Y'           HDRS              1
     C*
     C     XXDFLT        IFEQ      'Y'
     C*
     C*                    D e f a u l t    C O #    R e c o r d
     C*
     C                   OPEN      WMUPL
     C     Q1USER        CHAIN     PUP01                              60
     C                   Z-ADD     UPCO#         XXCO#
     C                   CLOSE     WMUPL
     C*
     C                   Z-ADD     *ZERO         XXFND
     C*
     C                   MOVEL(P)  DFLT          SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      52            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        SCFNAM
     C*
     C                   ELSE
     C*
     C                   Z-ADD     FNCO#         XXCO#
     C                   Z-ADD     FNFND         XXFND
     C*
     C                   ENDIF                                                  XXDFLT=Y
     C*
     C                   Z-ADD     XXFND         SCFND
     C*
     C*  Access Client Control Heading file
     C*
     C***  BEGIN CHG  ***  10/27/97  *********************************
     C********** XXCO#     CHAINPXH01                60
     C     HXKY          CHAIN     PXH01                              60
     C*
     C   60XXCO#         CHAIN     PXH01                              60
     C***  END   CHG  ***  10/27/97  *********************************
     C*
     C     ' '           CHECK     SL(1)                                  60
     C                   MOVE      *IN60         SECFLG
     C*
     C     ' '           CHECK     RM(1)                                  60
     C                   MOVE      *IN60         REMFLG
     C*
     C     ' '           CHECK     FL(1)                                  60
     C                   MOVE      *IN60         FLGFLG
     C*
     C     ' '           CHECK     D10(1)                                 60
     C                   MOVE      *IN60         DESFLG
     C*
     C  N60' '           CHECK     D40(1)                                 60
     C                   MOVE      *IN60         DESFLG
     C*
     C     ' '           CHECK     DTH(1)                                 60
     C                   MOVE      *IN60         DATFLG
     C*
     C*  Access Client Control file
     C*
     C                   CLEAR                   DBRCDF
     C                   MOVE      *OFF          *IN68
     C*
     C     CXKY          CHAIN     PXC01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZEROS        DBRRN
     C                   MOVE      *ON           *IN68
     C                   ENDIF
     C*
     C                   EXSR      MVDBSC
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*  Get the Company record
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        SCCONM
     C*
     C*  Load the record format info for the First Time
     C*
     C                   SELECT
     C     SECFLG        WHENEQ    *ON
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     REMFLG        WHENEQ    *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C     FLGFLG        WHENEQ    *ON
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C     DESFLG        WHENEQ    *ON
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C     DATFLG        WHENEQ    *ON
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C                   OTHER
     C                   MOVE      *ON           *IN69
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   MOVEL(P)  NOHDR         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      52            SRDLEN            3 0
     C                   MOVEL(P)  SRDFLD        SCFNAM
     C                   MOVE      'N'           HDRS
     C                   ENDSL
     C*
     C*   Display ALL Formats
     C*
     C     STRT          TAG
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        SAVETM            6 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SAVEDT            7 0
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C*
     C                   DO        20            X
     C*
     C     SL(X)         IFNE      *BLANK
     C*
     C     70            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C*
     C     ' '           CHECKR    SL(X)         XXLEN             3 0
     C*
     C     XXLEN         IFLT      22
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     SL(X)         CAT       DOTS:1        SL(X)
     C                   ELSE
     C     SL(X)         CAT       DOTS:0        SL(X)
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<25
     C*
     C                   ENDIF                                                  SL,X<>*BLANK
     C*
     C                   ENDDO                                                  X->20
     C*
     C                   EXFMT     SCR01
     C**
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C*
     C                   DO        20            X
     C*
     C     RM(X)         IFNE      *BLANK
     C*
     C     70            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C*
     C     ' '           CHECKR    RM(X)         XXLEN             3 0
     C*
     C     XXLEN         IFLT      22
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     RM(X)         CAT       DOTS:1        RM(X)
     C                   ELSE
     C     RM(X)         CAT       DOTS:0        RM(X)
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<25
     C*
     C                   ENDIF                                                  RM,X<>*BLANK
     C*
     C                   ENDDO                                                  X->20
     C*
     C                   EXFMT     SCR02
     C**
     C*
     C     Q2RECF        WHENEQ    'SCR03'
     C*
     C                   DO        20            X
     C*
     C     FL(X)         IFNE      *BLANK
     C*
     C     70            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C*
     C     ' '           CHECKR    FL(X)         XXLEN             3 0
     C*
     C     XXLEN         IFLT      22
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     FL(X)         CAT       DOTS:1        FL(X)
     C                   ELSE
     C     FL(X)         CAT       DOTS:0        FL(X)
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<25
     C*
     C                   ENDIF                                                  FL,X<>*BLANK
     C*
     C                   ENDDO                                                  X->20
     C*
     C                   EXFMT     SCR03
     C**
     C*
     C     Q2RECF        WHENEQ    'SCR04'
     C*
     C                   DO        5             X
     C*
     C     D10(X)        IFNE      *BLANK
     C*
     C     70            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C*
     C     ' '           CHECKR    D10(X)        XXLEN             3 0
     C*
     C     XXLEN         IFLT      17
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     D10(X)        CAT       DOTS:0        D10(X)
     C                   ELSE
     C     D10(X)        CAT       DOTS:1        D10(X)
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<25
     C*
     C                   ENDIF                                                  D10,X<>*BLANK
     C*
     C                   ENDDO                                                  X->5
     C*
     C                   DO        5             X
     C*
     C     D40(X)        IFNE      *BLANK
     C*
     C     80            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C*
     C     ' '           CHECKR    D40(X)        XXLEN             3 0
     C*
     C     XXLEN         IFLT      17
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     D40(X)        CAT       DOTS:0        D40(X)
     C                   ELSE
     C     D40(X)        CAT       DOTS:1        D40(X)
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<25
     C*
     C                   ENDIF                                                  D40,X<>*BLANK
     C*
     C                   ENDDO                                                  X->5
     C*
     C                   EXFMT     SCR04
     C**
     C*
     C     Q2RECF        WHENEQ    'SCR05'
     C*
     C                   DO        10            X
     C*
     C     DTH(X)        IFNE      *BLANK
     C*
     C     70            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C*
     C     ' '           CHECKR    DTH(X)        XXLEN             3 0
     C*
     C     XXLEN         IFLT      22
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     DTH(X)        CAT       DOTS:1        DTH(X)
     C                   ELSE
     C     DTH(X)        CAT       DOTS:0        DTH(X)
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<25
     C*
     C                   ENDIF                                                  DTH,X<>*BLANK
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   EXFMT     SCR05
     C*
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C*   Process ALL Function Keys
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*===============================================================
     C*  F3=Exit
     C*===============================================================
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*===============================================================
     C*  F5=Refresh
     C*===============================================================
     C*
     C     *INKE         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C     DBRRN         CHAIN(N)  DBRCDF                             60
     C*
     C                   EXSR      MVDBSC
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKE=*ON
     C*
     C*===============================================================
     C*  F10=Search                                      KJDO
     C*===============================================================
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C*  D A R T S    R e m a r k    S e a r c h
     C*
     C     HFLD          WHENEQ    'SCRM01'
     C     HFLD          OREQ      'SCRM02'
     C     HFLD          OREQ      'SCRM03'
     C     HFLD          OREQ      'SCRM04'
     C     HFLD          OREQ      'SCRM05'
     C     HFLD          OREQ      'SCRM06'
     C     HFLD          OREQ      'SCRM07'
     C     HFLD          OREQ      'SCRM08'
     C     HFLD          OREQ      'SCRM09'
     C     HFLD          OREQ      'SCRM10'
     C     HFLD          OREQ      'SCRM11'
     C     HFLD          OREQ      'SCRM12'
     C     HFLD          OREQ      'SCRM13'
     C     HFLD          OREQ      'SCRM14'
     C     HFLD          OREQ      'SCRM15'
     C     HFLD          OREQ      'SCRM16'
     C     HFLD          OREQ      'SCRM17'
     C     HFLD          OREQ      'SCRM18'
     C     HFLD          OREQ      'SCRM19'
     C     HFLD          OREQ      'SCRM20'
     C*
     C                   MOVEL(P)  HFLD          WRKCSR
     C                   Z-ADD     WRKPOS        X
     C*
     C     RM(X)         IFNE      *BLANK
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM *BLANKS   QQRMCD  5
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *LIKE         DEFINE    ##RMCD        QQRMCD
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C********** QQRTC     CABEQ'A'       TERM
     C     *INKC         CABEQ     'A'           TERM
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C                   MOVE      *ON           *IN42                          DSPATR(PC)
     C     20            ADD       X             Y1                2 0
     C*
     C     QQRMCD        IFEQ      *BLANK
     C                   MOVE      *ON           *IN(Y1)                        DSPATR(PC)
     C                   ELSE
     C                   MOVEL(P)  QQRMCD        REM(X)
     C     RM(X)         DOUNE     *BLANK
     C     Y1            OREQ      40
     C                   ADD       1             Y1                2 0
     C                   ENDDO                                                  RM,X<>*BLANK
     C                   MOVE      *ON           *IN(Y1)                        DSPATR(PC)
     C                   ENDIF                                                  QQRMCD=*BLANK
     C*
     C                   ENDIF                                                  RM,X<>*BLANK
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ=*ON
     C*
     C*===============================================================
     C*  F12=Cancel
     C*===============================================================
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     SECFLG        CABEQ     *OFF          TERM
     C**
     C*
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C*
     C     REMFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     SECFLG        CABEQ     *OFF          TERM
     C                   ENDIF                                                  REMFLG=*OFF
     C**
     C*
     C     Q2RECF        WHENEQ    'SCR04'
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C*
     C     FLGFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C*
     C     REMFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     SECFLG        CABEQ     *OFF          TERM
     C                   ENDIF                                                  REMFLG=*OFF
     C*
     C                   ENDIF                                                  FLGFLG=*OFF
     C**
     C*
     C     Q2RECF        WHENEQ    'SCR05'
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C*
     C     DESFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C*
     C     FLGFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C*
     C     REMFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C     SECFLG        CABEQ     *OFF          TERM
     C                   ENDIF                                                  REMFLG=*OFF
     C*
     C                   ENDIF                                                  FLGFLG=*OFF
     C*
     C                   ENDIF                                                  DESFLG=*OFF
     C**
     C*
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*===============================================================
     C*  F13=Change User Parameters
     C*===============================================================
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'W0006R'
     C*
     C                   MOVE      *ON           *IN60
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  *INKM=*ON
     C*
     C*===============================================================
     C*   Process HELP Requests
     C*===============================================================
     C*
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        XXRECF           10
     C                   MOVEL(P)  'QHLP'        FILNAM           10
     C                   MOVEL(P)  '*LIBL'       LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10=*ON
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C*  GOTO the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C*_______________________________________________________________
     C*
     C*  Process Request Screen 01
     C*
     C     SCR01T        TAG
     C*
     C*  IF THERE ARE NO HEADER RECORDS, END PROGRAM
     C*
     C     HDRS          CABEQ     'N'           TERM
     C*
     C***  BEGIN ADD  ***  10/27/97  *********************************
     C                   MOVE      *ON           *IN43
     C*
     C*  Security Levels must be entered
     C*
     C                   DO        20            X                 3 0
     C*
     C     SL(X)         IFNE      *BLANK
     C     SLV(X)        ANDLE     *ZEROS
     C     20            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C                   GOTO      STRT
     C                   ENDIF                                                  SL,X<>*BLANK<>STY,X
     C*
     C                   ENDDO                                                  X->20
     C*
     C                   MOVE      *OFF          *IN43
     C                   MOVE      *ON           *IN44
     C***  END   ADD  ***  10/27/97  *********************************
     C*
     C*  Edit Security Type Code, if Entered
     C*
     C                   DO        20            X                 3 0
     C*
     C     SL(X)         IFNE      *BLANK
     C     STY(X)        ANDNE     *BLANK
     C*
     C                   Z-ADD     1             N                 3 0
     C     STY(X)        LOOKUP    SCD(N)                                 60
     C*
     C     *IN60         IFEQ      *OFF
     C     20            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  SL,X<>*BLANK<>STY,X
     C*
     C                   ENDDO                                                  X->20
     C*
     C***  BEGIN ADD  ***  10/27/97  *********************************
     C                   MOVE      *OFF          *IN44
     C***  END   ADD  ***  10/27/97  *********************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     DBRRN         CHAIN(N)  DBRCDF                             60
     C*
     C     XCDT          IFGT      SAVEDT
     C     XCDT          OREQ      SAVEDT
     C     XCTIME        ANDGT     SAVETM
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
     C                   ENDIF                                                  XCDT>SAVEDT
     C*
     C*  Passed Edits
     C*
     C     DBRRN         IFNE      *ZERO
     C                   EXSR      UPDATR
     C                   ENDIF                                                  DBRRN<>*ZERO
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C*
     C     REMFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C     FLGFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C     DESFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C     DATFLG        IFEQ      *OFF
     C     DBRRN         IFEQ      *ZERO
     C                   EXSR      WRITER
     C                   ENDIF                                                  DBRRN=*ZERO
     C                   GOTO      TERM
     C                   ENDIF                                                  DATFLG=*OFF
     C                   ENDIF                                                  DESFLG=*OFF
     C                   ENDIF                                                  FLGFLG=*OFF
     C                   ENDIF                                                  REMFLG=*OFF
     C*
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*   Process Request Screen 02
     C*
     C     SCR02T        TAG
     C*
     C*  Edit DARTS Remark Code, if Entered
     C*
     C                   DO        20            X
     C*
     C     RM(X)         IFNE      *BLANK
     C     REM(X)        ANDNE     *BLANK
     C*
     C                   MOVEL     REM(X)        XXRMCD
     C*
     C     CTKY          CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN41
     C     20            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  RM,X<>*BLANK<>REM,X
     C*
     C                   ENDDO                                                  X->20
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     DBRRN         CHAIN(N)  DBRCDF                             60
     C*
     C     XCDT          IFGT      SAVEDT
     C     XCDT          OREQ      SAVEDT
     C     XCTIME        ANDGT     SAVETM
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
     C                   ENDIF                                                  XCDT>SAVEDT
     C*
     C*  Passed Edits
     C*
     C     DBRRN         IFNE      *ZERO
     C                   EXSR      UPDATR
     C                   ENDIF                                                  DBRRN<>*ZERO
     C*
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C*
     C     FLGFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C     DESFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C     DATFLG        IFEQ      *OFF
     C     DBRRN         IFEQ      *ZERO
     C                   EXSR      WRITER
     C                   ENDIF                                                  DBRRN=*ZERO
     C                   GOTO      TERM
     C                   ENDIF                                                  DATFLG=*OFF
     C                   ENDIF                                                  DESFLG=*OFF
     C                   ENDIF                                                  FLGFLG=*OFF
     C*
     C                   GOTO      STRT
     C*
     C*_______________________________________________________________
     C*
     C*   Process Request Screen 03
     C*
     C     SCR03T        TAG
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     DBRRN         CHAIN(N)  DBRCDF                             60
     C*
     C     XCDT          IFGT      SAVEDT
     C     XCDT          OREQ      SAVEDT
     C     XCTIME        ANDGT     SAVETM
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
     C                   ENDIF                                                  XCDT>SAVEDT
     C*
     C*  Passed Edits
     C*
     C     DBRRN         IFNE      *ZERO
     C                   EXSR      UPDATR
     C                   ENDIF                                                  DBRRN<>*ZERO
     C*
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C*
     C     DESFLG        IFEQ      *OFF
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C     DATFLG        IFEQ      *OFF
     C     DBRRN         IFEQ      *ZERO
     C                   EXSR      WRITER
     C                   ENDIF                                                  DBRRN=*ZERO
     C                   GOTO      TERM
     C                   ENDIF                                                  DATFLG=*OFF
     C                   ENDIF                                                  DESFLG=*OFF
     C*
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*   Process Request Screen 04
     C*
     C     SCR04T        TAG
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     DBRRN         CHAIN(N)  DBRCDF                             60
     C*
     C     XCDT          IFGT      SAVEDT
     C     XCDT          OREQ      SAVEDT
     C     XCTIME        ANDGT     SAVETM
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
     C                   ENDIF                                                  XCDT>SAVEDT
     C*
     C*  Passed Edits
     C*
     C     DBRRN         IFNE      *ZERO
     C                   EXSR      UPDATR
     C                   ENDIF                                                  DBRRN<>*ZERO
     C*
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C*
     C     DATFLG        IFEQ      *OFF
     C     DBRRN         IFEQ      *ZERO
     C                   EXSR      WRITER
     C                   ENDIF                                                  DBRRN=*ZERO
     C                   GOTO      TERM
     C                   ENDIF                                                  DATFLG=*OFF
     C*
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*   Process Request Screen 05
     C*
     C     SCR05T        TAG
     C*
     C*   Edit Date fields
     C*
     C                   DO        10            X
     C*
     C     DTH(X)        IFNE      *BLANK
     C     DT8(X)        ANDGT     *ZERO
     C*
     C                   Z-ADD     DT8(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT8(X)
     C*
     C     SRERR         IFEQ      *ON
     C     20            ADD       X             Y1
     C                   MOVE      *ON           *IN(Y1)
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  DTH<>*BLANK
     C*
     C                   ENDDO                                                  X->10
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     DBRRN         CHAIN(N)  DBRCDF                             60
     C*
     C     XCDT          IFGT      SAVEDT
     C     XCDT          OREQ      SAVEDT
     C     XCTIME        ANDGT     SAVETM
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
     C                   ENDIF                                                  XCDT>SAVEDT
     C*
     C*  Passed Edits
     C*
     C*  Update/Write the Client Control file
     C*
     C     DBRRN         IFEQ      *ZERO
     C                   EXSR      WRITER
     C                   ELSE
     C                   EXSR      UPDATR
     C                   ENDIF                                                  DBRRN=*ZERO
     C*_______________________________________________________________
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C*===============================================================
     C*  MVDBSC - Assign All Client Control Fields to Screen Fields
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      XCSL01        SCSL01
     C                   MOVE      XCSL02        SCSL02
     C                   MOVE      XCSL03        SCSL03
     C                   MOVE      XCSL04        SCSL04
     C                   MOVE      XCSL05        SCSL05
     C                   MOVE      XCSL06        SCSL06
     C                   MOVE      XCSL07        SCSL07
     C                   MOVE      XCSL08        SCSL08
     C                   MOVE      XCSL09        SCSL09
     C                   MOVE      XCSL10        SCSL10
     C                   MOVE      XCSL11        SCSL11
     C                   MOVE      XCSL12        SCSL12
     C                   MOVE      XCSL13        SCSL13
     C                   MOVE      XCSL14        SCSL14
     C                   MOVE      XCSL15        SCSL15
     C                   MOVE      XCSL16        SCSL16
     C                   MOVE      XCSL17        SCSL17
     C                   MOVE      XCSL18        SCSL18
     C                   MOVE      XCSL19        SCSL19
     C                   MOVE      XCSL20        SCSL20
     C                   MOVE      XCST01        SCST01
     C                   MOVE      XCST02        SCST02
     C                   MOVE      XCST03        SCST03
     C                   MOVE      XCST04        SCST04
     C                   MOVE      XCST05        SCST05
     C                   MOVE      XCST06        SCST06
     C                   MOVE      XCST07        SCST07
     C                   MOVE      XCST08        SCST08
     C                   MOVE      XCST09        SCST09
     C                   MOVE      XCST10        SCST10
     C                   MOVE      XCST11        SCST11
     C                   MOVE      XCST12        SCST12
     C                   MOVE      XCST13        SCST13
     C                   MOVE      XCST14        SCST14
     C                   MOVE      XCST15        SCST15
     C                   MOVE      XCST16        SCST16
     C                   MOVE      XCST17        SCST17
     C                   MOVE      XCST18        SCST18
     C                   MOVE      XCST19        SCST19
     C                   MOVE      XCST20        SCST20
     C                   MOVE      XCRM01        SCRM01
     C                   MOVE      XCRM02        SCRM02
     C                   MOVE      XCRM03        SCRM03
     C                   MOVE      XCRM04        SCRM04
     C                   MOVE      XCRM05        SCRM05
     C                   MOVE      XCRM06        SCRM06
     C                   MOVE      XCRM07        SCRM07
     C                   MOVE      XCRM08        SCRM08
     C                   MOVE      XCRM09        SCRM09
     C                   MOVE      XCRM10        SCRM10
     C                   MOVE      XCRM11        SCRM11
     C                   MOVE      XCRM12        SCRM12
     C                   MOVE      XCRM13        SCRM13
     C                   MOVE      XCRM14        SCRM14
     C                   MOVE      XCRM15        SCRM15
     C                   MOVE      XCRM16        SCRM16
     C                   MOVE      XCRM17        SCRM17
     C                   MOVE      XCRM18        SCRM18
     C                   MOVE      XCRM19        SCRM19
     C                   MOVE      XCRM20        SCRM20
     C                   MOVE      XCFL01        SCFL01
     C                   MOVE      XCFL02        SCFL02
     C                   MOVE      XCFL03        SCFL03
     C                   MOVE      XCFL04        SCFL04
     C                   MOVE      XCFL05        SCFL05
     C                   MOVE      XCFL06        SCFL06
     C                   MOVE      XCFL07        SCFL07
     C                   MOVE      XCFL08        SCFL08
     C                   MOVE      XCFL09        SCFL09
     C                   MOVE      XCFL10        SCFL10
     C                   MOVE      XCFL11        SCFL11
     C                   MOVE      XCFL12        SCFL12
     C                   MOVE      XCFL13        SCFL13
     C                   MOVE      XCFL14        SCFL14
     C                   MOVE      XCFL15        SCFL15
     C                   MOVE      XCFL16        SCFL16
     C                   MOVE      XCFL17        SCFL17
     C                   MOVE      XCFL18        SCFL18
     C                   MOVE      XCFL19        SCFL19
     C                   MOVE      XCFL20        SCFL20
     C                   MOVE      XC10A1        SC10A1
     C                   MOVE      XC10A2        SC10A2
     C                   MOVE      XC10A3        SC10A3
     C                   MOVE      XC10A4        SC10A4
     C                   MOVE      XC10A5        SC10A5
     C                   MOVE      XC40A1        SC40A1
     C                   MOVE      XC40A2        SC40A2
     C                   MOVE      XC40A3        SC40A3
     C                   MOVE      XC40A4        SC40A4
     C                   MOVE      XC40A5        SC40A5
     C*
     C                   DO        10            X
     C*
     C                   Z-ADD     DT7(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT8(X)
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C*  MVSCDB - Assign Screen Fields to Client Control Fields
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         XCCO#
     C                   Z-ADD     SCFND         XCFND
     C*
     C                   MOVE      SCSL01        XCSL01
     C                   MOVE      SCSL02        XCSL02
     C                   MOVE      SCSL03        XCSL03
     C                   MOVE      SCSL04        XCSL04
     C                   MOVE      SCSL05        XCSL05
     C                   MOVE      SCSL06        XCSL06
     C                   MOVE      SCSL07        XCSL07
     C                   MOVE      SCSL08        XCSL08
     C                   MOVE      SCSL09        XCSL09
     C                   MOVE      SCSL10        XCSL10
     C                   MOVE      SCSL11        XCSL11
     C                   MOVE      SCSL12        XCSL12
     C                   MOVE      SCSL13        XCSL13
     C                   MOVE      SCSL14        XCSL14
     C                   MOVE      SCSL15        XCSL15
     C                   MOVE      SCSL16        XCSL16
     C                   MOVE      SCSL17        XCSL17
     C                   MOVE      SCSL18        XCSL18
     C                   MOVE      SCSL19        XCSL19
     C                   MOVE      SCSL20        XCSL20
     C                   MOVE      SCST01        XCST01
     C                   MOVE      SCST02        XCST02
     C                   MOVE      SCST03        XCST03
     C                   MOVE      SCST04        XCST04
     C                   MOVE      SCST05        XCST05
     C                   MOVE      SCST06        XCST06
     C                   MOVE      SCST07        XCST07
     C                   MOVE      SCST08        XCST08
     C                   MOVE      SCST09        XCST09
     C                   MOVE      SCST10        XCST10
     C                   MOVE      SCST11        XCST11
     C                   MOVE      SCST12        XCST12
     C                   MOVE      SCST13        XCST13
     C                   MOVE      SCST14        XCST14
     C                   MOVE      SCST15        XCST15
     C                   MOVE      SCST16        XCST16
     C                   MOVE      SCST17        XCST17
     C                   MOVE      SCST18        XCST18
     C                   MOVE      SCST19        XCST19
     C                   MOVE      SCST20        XCST20
     C                   MOVE      SCRM01        XCRM01
     C                   MOVE      SCRM02        XCRM02
     C                   MOVE      SCRM03        XCRM03
     C                   MOVE      SCRM04        XCRM04
     C                   MOVE      SCRM05        XCRM05
     C                   MOVE      SCRM06        XCRM06
     C                   MOVE      SCRM07        XCRM07
     C                   MOVE      SCRM08        XCRM08
     C                   MOVE      SCRM09        XCRM09
     C                   MOVE      SCRM10        XCRM10
     C                   MOVE      SCRM11        XCRM11
     C                   MOVE      SCRM12        XCRM12
     C                   MOVE      SCRM13        XCRM13
     C                   MOVE      SCRM14        XCRM14
     C                   MOVE      SCRM15        XCRM15
     C                   MOVE      SCRM16        XCRM16
     C                   MOVE      SCRM17        XCRM17
     C                   MOVE      SCRM18        XCRM18
     C                   MOVE      SCRM19        XCRM19
     C                   MOVE      SCRM20        XCRM20
     C                   MOVE      SCFL01        XCFL01
     C                   MOVE      SCFL02        XCFL02
     C                   MOVE      SCFL03        XCFL03
     C                   MOVE      SCFL04        XCFL04
     C                   MOVE      SCFL05        XCFL05
     C                   MOVE      SCFL06        XCFL06
     C                   MOVE      SCFL07        XCFL07
     C                   MOVE      SCFL08        XCFL08
     C                   MOVE      SCFL09        XCFL09
     C                   MOVE      SCFL10        XCFL10
     C                   MOVE      SCFL11        XCFL11
     C                   MOVE      SCFL12        XCFL12
     C                   MOVE      SCFL13        XCFL13
     C                   MOVE      SCFL14        XCFL14
     C                   MOVE      SCFL15        XCFL15
     C                   MOVE      SCFL16        XCFL16
     C                   MOVE      SCFL17        XCFL17
     C                   MOVE      SCFL18        XCFL18
     C                   MOVE      SCFL19        XCFL19
     C                   MOVE      SCFL20        XCFL20
     C                   MOVE      SC10A1        XC10A1
     C                   MOVE      SC10A2        XC10A2
     C                   MOVE      SC10A3        XC10A3
     C                   MOVE      SC10A4        XC10A4
     C                   MOVE      SC10A5        XC10A5
     C                   MOVE      SC40A1        XC40A1
     C                   MOVE      SC40A2        XC40A2
     C                   MOVE      SC40A3        XC40A3
     C                   MOVE      SC40A4        XC40A4
     C                   MOVE      SC40A5        XC40A5
     C*
     C                   DO        10            X
     C*
     C                   Z-ADD     DT8(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DT7(X)
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   MOVE      *BLANK        XCSTAT
     C                   MOVEL(P)  Q1USER        XCUSER
     C                   MOVEL(P)  Q1PGM         XCPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        XCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XCDT
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C*  UPDATR - Update existing record to Client Control Fields
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     DBRRN         CHAIN     DBRCDF                             60
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C*  WRITER - Write a new record to Client Control Fields
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     XCUSER        XCIUSR
     C                   Z-ADD     XCDT          XCIDT
     C*
     C                   WRITE     DBRCDF
     C*
     C                   ENDSR                                                  WRITER
     C*
**  TABLE SCD - Valid Security Type Codes
ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
