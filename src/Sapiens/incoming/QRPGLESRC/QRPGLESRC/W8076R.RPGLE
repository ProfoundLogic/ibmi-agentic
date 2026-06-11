     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8076R -  MANUAL ACKNOWLEDGEMENT PROCESSING
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: 57WCC
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/05/01 C:15  Project: WC468                                *
     H*                 Changed code to correct processing of manual  *
     H*                 entries.                                      *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  08/05/99  C:118  PROJECT:  WC468                             *
     H*                   Made V3 changes. Made Jurisdiction State a  *
     H*                   required field.  Added search on SCJRST.    *
     H*                   Made V3 display file changes.               *
     H*****************************************************************
     FW8076F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR02S:RRN2)
     F                                     SFILE(SCR03S:RRN2)
     FSMCOL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FWMUPL     IF   E           K DISK
     FWTEML     IF   E           K DISK
     FWMM2L1    IF   E           K DISK
     FWMLGL5    IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FWXA1L2    UF A E           K DISK
     FWXA1L     UF A E           K DISK
     F                                     IGNORE(PA101)

     D DN              S            150    DIM(999)                             Data elements
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  KEYC                   1     23
     D  XXSENT                 1      7  0
     D  XXTSET                 8     10
     D  XXFND                 11     13  0
     D  XXFYR                 14     16  0
     D  XXCASE                17     23  0
     D*
     D                 DS
     D  KEYP                   1     23
     D*
     D                 DS
     D  KEYN                   1     23
     D*
     D*   Element Numbers
     D*
     D                 DS           396
     D  A1EL01                 1      4
     D  A1EL02                 5      8
     D  A1EL03                 9     12
     D  A1EL04                13     16
     D  A1EL05                17     20
     D  A1EL06                21     24
     D  A1EL07                25     28
     D  A1EL08                29     32
     D  A1EL09                33     36
     D  A1EL10                37     40
     D  A1EL11                41     44
     D  A1EL12                45     48
     D  A1EL13                49     52
     D  A1EL14                53     56
     D  A1EL15                57     60
     D  A1EL16                61     64
     D  A1EL17                65     68
     D  A1EL18                69     72
     D  A1EL19                73     76
     D  A1EL20                77     80
     D  A1EL21                81     84
     D  A1EL22                85     88
     D  A1EL23                89     92
     D  A1EL24                93     96
     D  A1EL25                97    100
     D  A1EL26               101    104
     D  A1EL27               105    108
     D  A1EL28               109    112
     D  A1EL29               113    116
     D  A1EL30               117    120
     D  A1EL31               121    124
     D  A1EL32               125    128
     D  A1EL33               129    132
     D  A1EL34               133    136
     D  A1EL35               137    140
     D  A1EL36               141    144
     D  A1EL37               145    148
     D  A1EL38               149    152
     D  A1EL39               153    156
     D  A1EL40               157    160
     D  A1EL41               161    164
     D  A1EL42               165    168
     D  A1EL43               169    172
     D  A1EL44               173    176
     D  A1EL45               177    180
     D  A1EL46               181    184
     D  A1EL47               185    188
     D  A1EL48               189    192
     D  A1EL49               193    196
     D  A1EL50               197    200
     D  A1EL51               201    204
     D  A1EL52               205    208
     D  A1EL53               209    212
     D  A1EL54               213    216
     D  A1EL55               217    220
     D  A1EL56               221    224
     D  A1EL57               225    228
     D  A1EL58               229    232
     D  A1EL59               233    236
     D  A1EL60               237    240
     D  A1EL61               241    244
     D  A1EL62               245    248
     D  A1EL63               249    252
     D  A1EL64               253    256
     D  A1EL65               257    260
     D  A1EL66               261    264
     D  A1EL67               265    268
     D  A1EL68               269    272
     D  A1EL69               273    276
     D  A1EL70               277    280
     D  A1EL71               281    284
     D  A1EL72               285    288
     D  A1EL73               289    292
     D  A1EL74               293    296
     D  A1EL75               297    300
     D  A1EL76               301    304
     D  A1EL77               305    308
     D  A1EL78               309    312
     D  A1EL79               313    316
     D  A1EL80               317    320
     D  A1EL81               321    324
     D  A1EL82               325    328
     D  A1EL83               329    332
     D  A1EL84               333    336
     D  A1EL85               337    340
     D  A1EL86               341    344
     D  A1EL87               345    348
     D  A1EL88               349    352
     D  A1EL89               353    356
     D  A1EL90               357    360
     D  A1EL91               361    364
     D  A1EL92               365    368
     D  A1EL93               369    372
     D  A1EL94               373    376
     D  A1EL95               377    380
     D  A1EL96               381    384
     D  A1EL97               385    388
     D  A1EL98               389    392
     D  A1EL99               393    396
     D  D#                     1    396
     D                                     DIM(99)                              Data element #
     D*
     D*   Error Numbers
     D*
     D                 DS           297
     D  A1ER01                 1      3
     D  A1ER02                 4      6
     D  A1ER03                 7      9
     D  A1ER04                10     12
     D  A1ER05                13     15
     D  A1ER06                16     18
     D  A1ER07                19     21
     D  A1ER08                22     24
     D  A1ER09                25     27
     D  A1ER10                28     30
     D  A1ER11                31     33
     D  A1ER12                34     36
     D  A1ER13                37     39
     D  A1ER14                40     42
     D  A1ER15                43     45
     D  A1ER16                46     48
     D  A1ER17                49     51
     D  A1ER18                52     54
     D  A1ER19                55     57
     D  A1ER20                58     60
     D  A1ER21                61     63
     D  A1ER22                64     66
     D  A1ER23                67     69
     D  A1ER24                70     72
     D  A1ER25                73     75
     D  A1ER26                76     78
     D  A1ER27                79     81
     D  A1ER28                82     84
     D  A1ER29                85     87
     D  A1ER30                88     90
     D  A1ER31                91     93
     D  A1ER32                94     96
     D  A1ER33                97     99
     D  A1ER34               100    102
     D  A1ER35               103    105
     D  A1ER36               106    108
     D  A1ER37               109    111
     D  A1ER38               112    114
     D  A1ER39               115    117
     D  A1ER40               118    120
     D  A1ER41               121    123
     D  A1ER42               124    126
     D  A1ER43               127    129
     D  A1ER44               130    132
     D  A1ER45               133    135
     D  A1ER46               136    138
     D  A1ER47               139    141
     D  A1ER48               142    144
     D  A1ER49               145    147
     D  A1ER50               148    150
     D  A1ER51               151    153
     D  A1ER52               154    156
     D  A1ER53               157    159
     D  A1ER54               160    162
     D  A1ER55               163    165
     D  A1ER56               166    168
     D  A1ER57               169    171
     D  A1ER58               172    174
     D  A1ER59               175    177
     D  A1ER60               178    180
     D  A1ER61               181    183
     D  A1ER62               184    186
     D  A1ER63               187    189
     D  A1ER64               190    192
     D  A1ER65               193    195
     D  A1ER66               196    198
     D  A1ER67               199    201
     D  A1ER68               202    204
     D  A1ER69               205    207
     D  A1ER70               208    210
     D  A1ER71               211    213
     D  A1ER72               214    216
     D  A1ER73               217    219
     D  A1ER74               220    222
     D  A1ER75               223    225
     D  A1ER76               226    228
     D  A1ER77               229    231
     D  A1ER78               232    234
     D  A1ER79               235    237
     D  A1ER80               238    240
     D  A1ER81               241    243
     D  A1ER82               244    246
     D  A1ER83               247    249
     D  A1ER84               250    252
     D  A1ER85               253    255
     D  A1ER86               256    258
     D  A1ER87               259    261
     D  A1ER88               262    264
     D  A1ER89               265    267
     D  A1ER90               268    270
     D  A1ER91               271    273
     D  A1ER92               274    276
     D  A1ER93               277    279
     D  A1ER94               280    282
     D  A1ER95               283    285
     D  A1ER96               286    288
     D  A1ER97               289    291
     D  A1ER98               292    294
     D  A1ER99               295    297
     D  E#                     1    297
     D                                     DIM(99)                              Error #
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   WORK FIELDS
     C*
     C     *LIKE         DEFINE    *IN60         @LDF
     C     *LIKE         DEFINE    *IN60         X@SF01
     C     *LIKE         DEFINE    *IN60         X@SF02
     C     *LIKE         DEFINE    *IN60         X@SF03
     C     *LIKE         DEFINE    *IN60         X@RDS3
     C     *LIKE         DEFINE    *IN60         KEYPF
     C     *LIKE         DEFINE    *IN60         KEYNF
     C     *LIKE         DEFINE    *IN60         @ROLLK
     C     *LIKE         DEFINE    *INKF         X@INKF
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    A1OTDT        X8OTDT
     C     *LIKE         DEFINE    M2OTDT        XXOTDT
     C     *LIKE         DEFINE    M2OTTM        XXOTTM
     C     *LIKE         DEFINE    LGMTC         XXMTC
     C     *LIKE         DEFINE    A1ACLC        XXACLC
     C     *LIKE         DEFINE    EMSET         XXESET
     C     *LIKE         DEFINE    EMMTC         XXEMTC
     C     *LIKE         DEFINE    EMDN#         XXDN#
     C     *LIKE         DEFINE    EMMSG#        XXMSG#
     C     *LIKE         DEFINE    M2OTDT        XSOTDT
     C     *LIKE         DEFINE    M2OTTM        XSOTTM
     C     *LIKE         DEFINE    SCTSTF        XSTSTF
     C     *LIKE         DEFINE    SCJST         XSJST
     C     *LIKE         DEFINE    SCARCV        XSARCV
     C*
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *ZEROS        RRN2              5 0
     C                   MOVE      *ZEROS        XRN2              5 0
     C                   MOVE      *ZEROS        P                 5 0
     C                   MOVE      *ZEROS        Q                 5 0
     C                   MOVE      *ZEROS        B                 5 0
     C                   MOVE      *ZEROS        L                 5 0
     C                   MOVE      *BLANKS       @PGD              1
     C                   MOVE      *BLANKS       XCOTDT            8
     C                   MOVE      *BLANKS       XCOTTM            6
     C                   MOVE      *OFF          X@SF01
     C                   MOVE      *OFF          X@SF02
     C                   MOVE      *OFF          X@SF03
     C                   MOVE      *OFF          X@RDS3
     C                   MOVE      *OFF          @ROLLK
     C*
     C*   KEY LISTS
     C*
     C*          SMCOL - Company Master
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCXFND
     C                   KFLD                    SCXFYR
     C                   KFLD                    SCXCAS
     C*
     C*          WMUPL - User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE STATE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*          WTEML - Edit Matrix Table
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXDN#
     C*
     C*          WMM1L2 - Transmission Management File
     C*
     C     M2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C*          WMLGL3 - EDI Log File
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXSENT
     C                   KFLD                    XXTSET
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     LGK2          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XXSENT
     C*
     C*          WXA1L2 - Acknowledgement Detail
     C*
     C     A1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    X8OTDT
     C                   KFLD                    XCOTTM
     C                   KFLD                    XXACLC
     C                   KFLD                    XXMTC
     C*
     C     A1K2          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XCOTDT
     C                   KFLD                    XCOTTM
     C*
     C*   ENTRY PARM LIST
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C*
     C     STRT01        TAG
     C*
     C*   Retrieve User Parameters
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         UPCO#
     C                   ENDIF
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*   Retrieve Company Master
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/05/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*   Initialize Record Format
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display all formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXSR      SRLSC1
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02C'
     C                   WRITE     SCR02K
     C                   EXFMT     SCR02C
     C                   END
     C*
     C*   Process command keys
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*          Command 03 = EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*          Command 05 = TRANSMISSION MGMT SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C***** BEGIN ADD **** 08/05/99 ***********************************
     C*  JURISDICTION STATE TABLE SEARCH
     C*
     C     HFLD          IFEQ      'SCJST'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'JRST'        QQTBID            4
     C                   PARM      *BLANK        QQKEY            10
     C                   PARM      ' '           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   ENDIF                                                  QQRTCD='A'
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='B'
     C*
     C     QQKEY         IFEQ      *BLANKS
     C                   GOTO      STRT
     C                   ENDIF                                                  QQKEY=BLKS
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCJST
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='*'
     C*
     C                   ENDIF                                                  HFLD=SCJST
     C*
     C***** END   ADD **** 08/05/99 ***********************************
     C*          The jurisdiction state MUST be entered and valid, because
     C*          this search only displays a list for one jurisdiction
     C*          state at a time.
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCJST         QQJST             2
     C                   MOVE      SCOTDT        QQOTDT            8
     C                   MOVE      SCOTTM        QQOTTM            6
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'W0873R'
     C                   PARM                    QQCO#
     C                   PARM                    QQJST
     C                   PARM                    QQOTDT
     C                   PARM                    QQOTTM
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQJST         CABEQ     ' '           STRT
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQJST         SCJST
     C                   MOVE      QQOTDT        SCOTDT
     C                   MOVE      QQOTTM        SCOTTM
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*          Help Request
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C*********            WRITESCR99
     C***** END   DEL **** 08/05/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*          Previous Screen
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR02C  '
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C*
     C     Q2RECF        WHENEQ    'SCR03C  '
     C                   MOVEL     'SCR02C  '    Q2RECF
     C                   GOTO      STRT
     C*
     C                   OTHER
     C                   GOTO      TERM
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*          Change User Parameters
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/05/99 ***********************************
     C                   GOTO      STRT01
     C                   ENDIF
     C*
      ****************************************
     C*   *IN17 -  ROLLDOWN REQUEST          *
      ****************************************
     C*
     C     *IN17         IFEQ      *ON                                          ROLLDOWN
     C                   READC     SCR02S                                 61
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           @ROLLK
     C                   GOTO      SCR02T
     C                   ENDIF
     C     KEYPF         IFEQ      *OFF                                         NO ROLL IF EOF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVEL     KEYP          KEYC
     C     LGKY          SETLL     PLG01
     C                   MOVE      'B'           @PGD
     C                   EXSR      SRLSC2
     C                   EXSR      LOAD
     C                   MOVEL     'SCR02C  '    Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLDOWN
     C*
      ****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
      ****************************************
     C*
     C     *IN18         IFEQ      *ON                                          ROLLUP
     C                   READC     SCR02S                                 61
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           @ROLLK
     C                   GOTO      SCR02T
     C                   ENDIF
     C     KEYNF         IFEQ      *OFF                                         NO ROLL IF EOF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVEL     KEYN          KEYC
     C     LGKY          SETGT     PLG01
     C                   MOVE      'F'           @PGD
     C                   EXSR      SRLSC2
     C                   EXSR      LOAD
     C                   MOVEL     'SCR02C  '    Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   END
     C*
      *****************************************************************
     C*
     C*   Process Current Screen
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      SCR01T
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02C'
     C                   GOTO      SCR02T
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03C'
     C                   GOTO      SCR03T
     C                   END
     C*
     C*   Prompt Screen
     C*
     C     SCR01T        TAG
     C*
     C*          Validate Test Flag
     C*
     C     SCTSTF        IFNE      *BLANKS
     C     SCTSTF        ANDNE     'Y'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C*
      ***        Validate Jurisdiction State
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
      ***        Validate Transmission Date
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOTDT
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
      ***        Validate Acknowledgement Received Date
     C*
     C                   Z-ADD     SCARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCARCV
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
      ***        Validate that the Transmission Management Record Exists.
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXOTDT
     C                   MOVE      SCOTTM        XXOTTM
     C                   MOVE      XXOTDT        XCOTDT
     C                   MOVE      XXOTTM        XCOTTM
     C*
     C     M2KY          CHAIN     PM201                              22
     C     *IN22         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      SCTSTF        XSTSTF
     C                   MOVE      SCJST         XSJST
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSOTDT
     C                   MOVE      SCOTTM        XSOTTM
     C                   MOVE      SCARCV        XSARCV
     C*
      ***        Display the list of log records in transmission date
      ***        order.
     C*
     C                   MOVE      XXOTDT        XXSENT
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      M2TSET        XXTSET
     C     LGKY          SETLL     PLG01
     C                   MOVE      'F'           @PGD
     C                   EXSR      SRLSC2
     C                   EXSR      LOAD
     C                   CLEAR                   Q2RECF
     C                   MOVEL     'SCR02C'      Q2RECF
     C                   GOTO      STRT
     C*
      *** Process List of Log Records
     C*
     C     SCR02T        TAG
     C*
      ***        Reposition list if requested.
     C*
     C     SCPSET        IFNE      *BLANKS
     C     SCPFND        ORNE      *ZEROS
     C     SCPFYR        ORNE      *ZEROS
     C     SCPCAS        ORNE      *ZEROS
     C                   MOVE      SCPSET        XXTSET
     C                   MOVE      SCPFND        XXFND
     C                   MOVE      SCPFYR        XXFYR
     C                   MOVE      SCPCAS        XXCASE
     C                   MOVE      *BLANKS       SCPSET
     C                   MOVE      *ZEROS        SCPFND
     C                   MOVE      *ZEROS        SCPFYR
     C                   MOVE      *ZEROS        SCPCAS
     C     LGKY          SETLL     PLG01
     C                   MOVE      'F'           @PGD
     C                   EXSR      SRLSC2
     C                   EXSR      LOAD
     C                   MOVEL     'SCR02C  '    Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
      ***        Retrieve Next Record from List
     C*
     C                   MOVE      *OFF          X@INKF
     C                   Z-ADD     1             XRN2
     C     *INKF         IFEQ      *ON
     C                   MOVE      *ON           X@INKF
     C     XRN2          CHAIN     SCR02S                             61
     C                   ELSE
     C     @ROLLK        IFEQ      *OFF
     C                   READC     SCR02S                                 61
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          @ROLLK
     C*
     C     *IN61         DOWEQ     *OFF
     C                   MOVE      SCXCO#        QQCO#
     C                   MOVE      SCXFND        QQFND
     C                   MOVE      SCXFYR        QQFYR
     C                   MOVE      SCXCAS        QQCASE
     C                   MOVE      SCXSET        QQTSET
     C                   MOVE      SCXMTC        QQMTC
     C                   MOVE      SCJST         QQJST
     C                   CLEAR                   DN
     C                   CALL      'W8070R6'
     C                   PARM                    QQMTCD            6
     C                   PARM                    QQCO#             3            Key info
     C                   PARM                    QQFND             3            Key info
     C                   PARM                    QQFYR             3            Key info
     C                   PARM                    QQCASE            7            Key info
     C                   PARM                    QQTSET            3            Req data
     C                   PARM                    QQMTC             2            Req data
     C                   PARM                    QQJST             2            Req data
     C                   PARM                    DN
     C*
     C     SCXSEL        IFNE      *BLANKS
     C*
     C     SCXSEL        IFEQ      'TR'
     C     SCXSEL        OREQ      'TE'
     C*
     C                   MOVE      *OFF          X@SF03
     C                   EXSR      SRLSC3
     C                   EXSR      LOAD3
     C*
      ***        Process List of Errors
     C*
     C     SCR03T        TAG
     C*
     C                   MOVE      *ON           X@RDS3
     C     X@RDS3        DOWEQ     *ON
     C                   MOVE      *ON           *IN05
     C                   WRITE     SCR03K
     C                   EXFMT     SCR03C
     C                   MOVE      *OFF          X@RDS3
     C     *IN11         CABEQ     *ON           STRT
     C*
      ***        If any record was changed, execute edits and re-
      ***        display the list.
     C*
     C                   READC     SCR03S                                 64
     C     *IN64         IFEQ      *OFF
     C                   MOVE      *ON           X@RDS3
     C                   ENDIF
     C     1             DO        22            RRN2
     C     RRN2          CHAIN     SCR03S                             64
     C                   EXSR      SREDS3
     C                   UPDATE    SCR03S
     C                   ENDDO
     C                   ENDDO
     C                   ENDIF
     C*
      ***        Write/Update the acknowledgement detail record.
     C*
     C                   MOVEL     DN(2)         XXMTC
     C     A1KY          CHAIN     PA101                              62
     C                   MOVEL     'AK1'         A1TAK1
     C                   MOVE      XXCO#         A1CO#
     C                   MOVE      XCOTDT        A1OTDT
     C                   MOVE      XCOTTM        A1OTTM
     C                   MOVEL     SCJST         A1JST
     C                   MOVE      SCARCV        XXDT4             4               YEAR
     C                   MOVEL     XXDT4         A1PDT                               FIRST
     C                   MOVEL     SCARCV        XXDT4                             MMDD
     C                   MOVE      XXDT4         A1PDT                               LAST
     C                   MOVE      DSTIME        A1PTM
     C                   MOVE      DN(6)         A1IEIN
     C                   MOVE      DN(8)         A1AEIN
     C                   MOVEL     DN(1)         A1TSET
     C                   MOVEL     DN(15)        A1ACLC
     C*** BEGIN CHG *** 01/05/01 **************************************
     C******************** MOVELDN,5      A1ACL#
     C                   MOVEL     SCXACL        A1ACL#
     C*** END CHG ***** 01/05/01 **************************************
     C                   MOVEL     DN(2)         A1MTC
     C                   MOVE      DN(3)         A1TDT
     C                   MOVE      SCXSEL        A1ACKC
     C*** BEGIN ADD *** 01/05/01 **************************************
     C                   MOVE      'Y'           A1STAT
     C*** END ADD ***** 01/05/01 **************************************
     C                   CLEAR                   E#
     C                   CLEAR                   D#
     C                   MOVE      *ZEROS        P
     C*
     C     SCXSEL        IFEQ      'TR'
     C     SCXSEL        OREQ      'TE'
     C     1             DO        22            RRN2
     C     RRN2          CHAIN     SCR03S                             63
     C     SCXER#        IFNE      *ZEROS
     C                   ADD       1             P
     C                   MOVE      SCXEL#        D#(P)
     C                   MOVE      SCXER#        E#(P)
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C     *IN62         IFEQ      *OFF
     C                   UPDATE    PA101
     C                   ELSE
     C                   WRITE     PA101
     C                   ENDIF
     C*
     C                   ELSE
     C*
      ***        If the selection was changed to blanks, change it
      ***        back if there is an existing acknowledgement record.
     C*
     C                   MOVEL     DN(15)        XXACLC
     C                   MOVEL     DN(2)         XXMTC
     C     A1KY          CHAIN(N)  PA101                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     A1ACKC        SCXSEL
     C                   UPDATE    SCR02S
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SCXACL        IFNE      *BLANK
     C     SCXACL        ANDNE     SFXACL
     C*
     C     CMKY          CHAIN     PCM01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     SCXACL        CMACL#
     C                   UPDATE    PCM01
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   MOVE      *OFF          *IN05
     C*
      ***        Continue with List of Log Records
     C*
     C     X@INKF        IFEQ      *ON
     C                   ADD       1             XRN2
     C     XRN2          IFLE      6
     C     XRN2          CHAIN     SCR02S                             61
     C                   ELSE
     C                   MOVE      *ON           *IN61
     C                   ENDIF
     C                   ELSE
     C                   READC     SCR02S                                 61
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   CLEAR                   Q2RECF
     C*** BEGIN CHG *** 01/05/01 **************************************
     C******************** MOVEL'SCR02C'  Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   EXSR      SRUHDR
     C*** END CHG ***** 01/05/01 **************************************
     C                   GOTO      STRT
     C*
      *** End Program
     C*
     C     TERM          TAG
     C*** BEGIN DLT *** 01/05/01 **************************************
     C******************** EXSR SRUHDR
     C*** END DLT ***** 01/05/01 **************************************
     C                   MOVE      *ON           *INLR
     C/EJECT
      **************************************************************************
     C*
      *** ROUTINE - SRLSC1
     C*
     CSR   SRLSC1        BEGSR
     C*
      *** Initialize Screen Fields
     C*
     CSR                 TIME                    DSDTTM
     C*
     CSR   X@SF01        IFEQ      *OFF
     CSR                 MOVE      *ON           X@SF01
     C*
     CSR                 CLEAR                   SCR01
     C                   MOVEL     CONAME        SCCONM
     C                   MOVEL     Q1PGM         SCPGM
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**SR**********         MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/05/99 ***********************************
     CSR                 MOVE      QQPRDS        SCPRDS
     CSR                 MOVE      *BLANKS       SCTSTF
     CSR                 MOVE      *BLANKS       SCJST
     CSR                 MOVE      *ZEROS        SCOTDT
     CSR                 MOVE      *ZEROS        SCOTTM
     C                   Z-ADD     DSDATE        SCARCV
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      **************************************************************************
      *** ROUTINE - SRLSC2
     C*
     CSR   SRLSC2        BEGSR
     C*
      *** Initialize Screen Fields
     C*
     CSR                 TIME                    DSDTTM
     C*
     CSR   X@SF02        IFEQ      *OFF
     CSR                 MOVE      *ON           X@SF02
     C*
     CSR                 CLEAR                   SCR02C
     C                   MOVEL     CONAME        SCCONM
     C                   MOVE      Q1PGM         SCPGM
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**SR********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/05/99 ***********************************
     CSR                 MOVE      QQPRDS        SCPRDS
     CSR                 MOVE      XSJST         SCJRST
     CSR                 MOVE      XSTSTF        SCTSTF
     C                   Z-ADD     XSOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOTDT
     CSR                 MOVE      XSOTTM        SCOTTM
     CSR                 MOVE      XXTSET        SCTSET
     C*
     CSR                 ENDIF
     C*
     CSR                 ENDSR
     C/EJECT
      **************************************************************************
      *** ROUTINE - SRLSC3
     C*
     CSR   SRLSC3        BEGSR
     C*
      *** Initialize Screen Fields
     C*
     CSR                 TIME                    DSDTTM
     C*
     CSR   X@SF03        IFEQ      *OFF
     CSR                 MOVE      *ON           X@SF03
     C*
     CSR                 CLEAR                   SCR03C
     C                   MOVEL     CONAME        SCCONM
     C                   MOVE      Q1PGM         SCPGM
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**SR********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/05/99 ***********************************
     CSR                 MOVE      QQPRDS        SCPRDS
     CSR                 MOVE      XSJST         SCJRST
     CSR                 MOVE      XSTSTF        SCTSTF
     C                   Z-ADD     XSOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOTDT
     CSR                 MOVE      XSOTTM        SCOTTM
     CSR                 MOVE      SCXSET        SCTSET
     CSR                 MOVE      SCXMTC        SCMTC
     CSR                 MOVE      SCXSEL        SCSELS
     CSR                 MOVE      SCXACD        SCACDT
     CSR                 MOVEL     QQCO#         SCCLM
     CSR                 CAT       '-':0         SCCLM
     CSR                 CAT       QQFND:0       SCCLM
     CSR                 CAT       '-':0         SCCLM
     CSR                 CAT       QQFYR:0       SCCLM
     CSR                 CAT       '-':0         SCCLM
     CSR                 CAT       QQCASE:0      SCCLM
     CSR                 MOVEL     SCXCLN        SCCLNM
     C*
     CSR                 ENDIF
     C*
     CSR                 ENDSR
     C*
     C*****************************************************************
     C* LOAD SUBROUTINE                ** Subroutine **   L O A D     *
     C*****************************************************************
     CSR   LOAD          BEGSR
     C*
     CSR                 MOVE      *ON           @LDF
     CSR   @LDF          DOWEQ     *ON
     CSR                 MOVE      *OFF          @LDF
     C*
     CSR                 MOVE      *OFF          *IN05
     CSR                 MOVE      *OFF          *IN32
     CSR                 MOVE      *BLANK        KEYP
     CSR                 MOVE      *BLANK        KEYN
     CSR                 MOVE      *OFF          KEYPF
     CSR                 MOVE      *OFF          KEYNF
     C*
     CSR                 MOVE      *OFF          *IN01
     CSR                 MOVE      *ON           *IN04
     CSR                 WRITE     SCR02C
     CSR                 MOVE      *OFF          *IN04
     CSR                 MOVE      *ON           *IN01
     C*
     CSR   @PGD          IFEQ      'F'
     CSR                 MOVE      *ZEROS        RRN2
     CSR                 ELSE
     CSR                 Z-ADD     5             RRN2
     CSR                 ENDIF
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UP TO 4 LINES
     C*
     CSR                 DO        4
     C*
     CSR   @PGD          IFEQ      'F'
     CSR   LGK2          READE     PLG01                                  60
     CSR                 ELSE
     CSR   LGK2          READPE    PLG01                                  60
     CSR                 ENDIF
     C*
     CSR   *IN60         IFEQ      *ON
     CSR                 LEAVE
     CSR                 END
     C*
     CSR   @PGD          IFEQ      'F'
     CSR                 ADD       1             RRN2
     CSR                 ELSE
     CSR                 SUB       1             RRN2
     CSR                 ENDIF
     C*
     CSR                 MOVE      LGCO#         QQCO#
     CSR                 MOVE      LGFND         QQFND             3
     CSR                 MOVE      LGFYR         QQFYR             3
     CSR                 MOVE      LGCASE        QQCASE            7
     CSR                 MOVE      LGTSET        QQTSET            3
     CSR                 MOVE      LGMTC         QQMTC             2
     CSR                 MOVE      LGMTCD        QQMTCD            6
     CSR                 MOVE      SCJST         QQJST             2
     CSR                 CLEAR                   DN
     CSR                 CALL      'W8070R6'
     CSR                 PARM                    QQMTCD            6
     CSR                 PARM                    QQCO#             3            Key info
     CSR                 PARM                    QQFND             3            Key info
     CSR                 PARM                    QQFYR             3            Key info
     CSR                 PARM                    QQCASE            7            Key info
     CSR                 PARM                    QQTSET            3            Req data
     CSR                 PARM                    QQMTC             2            Req data
     CSR                 PARM                    QQJST             2            Req data
     CSR                 PARM                    DN
     C*
     CSR                 MOVE      *BLANKS       SCXSEL
     CSR                 MOVEL     DN(5)         SCXACL
     CSR                 MOVEL     SCXACL        SFXACL
     CSR                 MOVEL     LGTSET        SCXSET
     CSR                 MOVEL     LGMTC         SCXMTC
     CSR                 MOVE      LGCO#         SCXCO#
     CSR                 MOVE      LGFND         SCXFND
     CSR                 MOVE      LGFYR         SCXFYR
     CSR                 MOVE      LGCASE        SCXCAS
     CSR   DN(31)        IFNE      *BLANKS
     C                   MOVEL     DN(31)        SCXACD
     CSR                 ENDIF
     CSR                 MOVEL     DN(43)        SCXCLN
     CSR   SCXCLN        IFNE      *BLANKS
     CSR                 CAT       ',':0         SCXCLN
     CSR                 CAT       DN(44):1      SCXCLN
     CSR                 CAT       DN(45):1      SCXCLN
     CSR                 ENDIF
     C                   Z-ADD     XXOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X8OTDT
     CSR                 MOVEL     DN(15)        XXACLC
     CSR                 MOVEL     DN(2)         XXMTC
     CSR   A1KY          CHAIN     PA101                              62
     CSR   *IN62         IFEQ      *OFF
     CSR                 MOVE      A1ACKC        SCXSEL
     CSR                 ENDIF
     CSR                 MOVE      *OFF          *IN06
     CSR                 WRITE     SCR02S
     C*
     C                   Z-ADD     LGSENT        XXSENT
     CSR                 MOVE      LGTSET        XXTSET
     CSR                 MOVE      LGFND         XXFND
     CSR                 MOVE      LGFYR         XXFYR
     CSR                 MOVE      LGCASE        XXCASE
     C*
     CSR   RRN2          IFEQ      1
     CSR                 MOVE      KEYC          KEYP
     CSR                 Z-ADD     RRN2          #@$2
     CSR                 ENDIF
     C*
     CSR   RRN2          IFEQ      4
     CSR                 MOVE      KEYC          KEYN
     CSR                 ENDIF
     C*
     CSR                 ENDDO
     C*
     CSR   @PGD          IFNE      'F'
     CSR   RRN2          ANDGT     1
     CSR                 CLEAR                   KEYC
     CSR                 MOVE      XXOTDT        XXSENT
     CSR                 MOVE      *ZEROS        XXFND
     CSR                 MOVE      *ZEROS        XXFYR
     CSR                 MOVE      *ZEROS        XXCASE
     CSR   LGKY          SETLL     PLG01
     CSR                 MOVE      *ON           @LDF
     CSR                 MOVE      'F'           @PGD
     CSR                 ENDIF
     C*
     CSR                 ENDDO
     C*
     C*  Set Page Indicators
     C*
     CSR   KEYP          IFGT      *BLANKS
     CSR                 MOVE      KEYP          KEYC
     CSR   LGKY          CHAIN     PLG01                              60
     CSR   LGK2          READPE    PLG01                                  60
     C*
     CSR   *IN60         IFEQ      *OFF
     CSR                 MOVE      *ON           KEYPF
     CSR                 ENDIF
     CSR                 ENDIF
     C*
     CSR   KEYN          IFGT      *BLANKS
     CSR                 MOVE      KEYN          KEYC
     CSR   LGKY          CHAIN     PLG01                              60
     CSR   LGK2          READE     PLG01                                  60
     C*
     CSR   *IN60         IFEQ      *OFF
     CSR                 MOVE      *ON           KEYNF
     CSR                 ENDIF
     CSR                 ELSE
     CSR                 MOVE      *ON           *IN05
     CSR                 ENDIF
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     CSR   RRN2          IFEQ      0
     CSR   *IN60         ANDEQ     *ON
     CSR                 CLEAR                   SCR02S
     CSR                 Z-ADD     1             RRN2
     CSR                 Z-ADD     RRN2          #@$2
     CSR                 MOVE      *ON           *IN06
     CSR                 WRITE     SCR02S
     CSR                 END
     C*
     CSR                 ENDSR
     C*
     C/EJECT
     C*
     C*****************************************************************
     C* LOAD SUBROUTINE FOR SCR03      ** Subroutine **   L O A D 3   *
     C*****************************************************************
     CSR   LOAD3         BEGSR
     C*
     CSR                 MOVE      *OFF          *IN05
     CSR                 MOVE      *OFF          *IN32
     C*
     CSR                 MOVE      *OFF          *IN01
     CSR                 MOVE      *ON           *IN02
     CSR                 WRITE     SCR03C
     CSR                 MOVE      *OFF          *IN02
     CSR                 MOVE      *ON           *IN01
     CSR                 Z-ADD     1             #@$2
     C*
     C*          If an Acknowledgement already exists, retrieve the
     C*          detail.
     C*
     CSR                 CLEAR                   XXMTC
     CSR                 MOVEL     DN(2)         XXMTC
     CSR                 CLEAR                   XXACLC
     CSR                 MOVEL     DN(15)        XXACLC
     CSR   A1KY          CHAIN(N)  PA101                              62
     CSR   *IN62         IFEQ      *OFF
     CSR                 MOVE      *ZEROS        RRN2
     CSR   1             DO        22            P
     CSR   E#(P)         IFNE      *BLANKS
     CSR                 ADD       1             RRN2
     CSR   RRN2          CHAIN     SCR03S                             63
     CSR   ' ':'0'       XLATE     D#(P)         D#(P)
     CSR                 MOVE      D#(P)         SCXEL#
     CSR                 Z-ADD     SCXEL#        Q
     CSR   ' ':'0'       XLATE     E#(P)         E#(P)
     CSR                 MOVE      E#(P)         SCXER#
     CSR   ' '           CHECK     DN(Q)         B
     CSR   30            SUB       B             L
     CSR   L             IFGT      25
     CSR                 Z-ADD     25            L
     CSR                 ENDIF
     CSR   B             IFEQ      *ZEROS
     CSR                 Z-ADD     1             B
     CSR                 ENDIF
     CSR   L             SUBST     DN(Q):B       SCXELV
     CSR                 EXSR      SREDS3
     CSR                 UPDATE    SCR03S
     CSR                 ENDIF
     CSR                 ENDDO
     CSR                 ENDIF
     C*
     CSR                 ENDSR
     C*
     C/EJECT
     C*
     C*****************************************************************
     C* SREDS3 SUBROUTINE              ** Subroutine **   SREDS3      *
     C*****************************************************************
     C*
     CSR   SREDS3        BEGSR
     C*
     CSR                 MOVE      *OFF          *IN20
     CSR                 MOVE      *OFF          *IN21
     CSR                 CLEAR                   SCXELV
     CSR                 CLEAR                   SCXERD
     C*
     C*          Edit the element #
     C*
     CSR   SCXEL#        IFNE      *ZEROS
     C*
     CSR   SCXEL#        IFLT      1
     CSR   SCXEL#        ORGT      99
     CSR                 MOVE      *ON           *IN20
     CSR                 ELSE
     CSR                 Z-ADD     SCXEL#        Q
     CSR   ' '           CHECK     DN(Q)         B
     CSR   30            SUB       B             L
     CSR   L             IFGT      25
     CSR                 Z-ADD     25            L
     CSR                 ENDIF
     CSR   B             IFEQ      *ZEROS
     CSR                 Z-ADD     1             B
     CSR                 ENDIF
     CSR   L             SUBST     DN(Q):B       SCXELV
     CSR                 ENDIF
     C*
     CSR                 ELSE
     C*
     CSR   SCXER#        IFNE      *ZEROS
     CSR                 MOVE      *ON           *IN20
     CSR                 ENDIF
     C*
     CSR                 ENDIF
     C*
     CSR   SCXEL#        IFGT      *ZEROS
     CSR   SCXER#        ANDGT     *ZEROS
     C*
     CSR                 Z-ADD     SCXEL#        XXDN#
     CSR                 Z-ADD     SCXER#        XXMSG#
     CSR                 MOVE      SCXSET        XXESET
     CSR                 MOVE      SCXMTC        XXEMTC
     CSR   EMKY          CHAIN     PEM01                              21
     CSR   *IN21         IFEQ      *ON
     CSR                 MOVE      *BLANKS       XXEMTC
     CSR   EMKY          CHAIN     PEM01                              21
     CSR                 ENDIF
     CSR   *IN21         IFEQ      *ON
     CSR                 MOVE      *BLANKS       XXESET
     CSR   EMKY          CHAIN     PEM01                              21
     CSR                 ENDIF
     CSR   *IN21         IFEQ      *OFF
     CSR                 MOVEL     EMDESC        SCXERD
     CSR                 ELSE
     CSR                 MOVE      *ALL'*'       SCXERD
     CSR                 ENDIF
     C*
     CSR                 ENDIF
     C*
     CSR   *IN20         IFEQ      *ON
     CSR   *IN21         OREQ      *ON
     CSR                 MOVE      *ON           X@RDS3
     CSR                 ENDIF
     C*
     CSR   SCXEL#        IFEQ      *ZEROS
     CSR   SCXER#        ANDEQ     *ZEROS
     CSR                 CLEAR                   SCR03S
     CSR                 ENDIF
     C*
     CSR                 ENDSR
     C/EJECT
     C*
     C*****************************************************************
     C* SRUHDR SUBROUTINE              ** Subroutine **   SRUHDR      *
     C*****************************************************************
     C*
     CSR   SRUHDR        BEGSR
     C*
     CSR   A1K2          CHAIN     PHA01                              62
     CSR                 MOVE      *ZEROS        XXRCNT            9 0
     CSR   A1K2          CHAIN(N)  PA101                              61
     CSR   *IN61         DOWEQ     *OFF
     CSR                 ADD       1             XXRCNT
     CSR   A1K2          READE     PA101                                  61
     CSR                 ENDDO
     C*
     C*          Write/Update the Header
     C*
     CSR                 MOVEL     'HD1'         HATHDR
     C                   MOVE      SCARCV        XXDT4             4               YEAR
     C                   MOVEL     XXDT4         XXDATE                              FIRST
     C                   MOVEL     SCARCV        XXDT4                             MMDD
     C                   MOVE      XXDT4         XXDATE                              LAST
     CSR                 MOVE      XXDATE        HALTDT
     CSR                 MOVE      DSTIME        HALTTM
     CSR                 MOVE      M2OTDT        HAOTDT
     CSR                 MOVE      M2OTTM        HAOTTM
     CSR                 MOVE      XXRCNT        HARCNT
     CSR                 MOVE      SCTSTF        HATPFG
     CSR                 MOVE      XXCO#         HACO#
     CSR                 MOVE      SCJST         HAJST
     CSR                 MOVEL     'P'           HASTAT
     CSR                 MOVEL     'MANUAL'      HASEIN
     C*
     CSR   *IN62         IFEQ      *OFF
     CSR                 UPDATE    PHA01
     CSR                 ELSE
     CSR                 WRITE     PHA01
     CSR                 ENDIF
     C*
     CSR                 ENDSR
