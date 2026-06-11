     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8078R3 -  EDITS FOR EDI ACKNOWLEDGMENT PROCESSING
     H*
     H*****************************************************************
     H*
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H* REVISIONS:
     H*                                                                                    *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 09/07/19 C:62  Project: 152WAU                                                     *
     H*                Use Optacomp process.                                               *
     H*                                                                                    *
     H* 03/31/14 C:53  Project: 114WBF                                                     *
     H*                Change key field sequence.                                          *
     H*
     H* 11/27/13 C:180 SUPPORT OP114 84300 - PROJECT: 147WBF          *
     H*                Identify OPTACOMP Admin Claim Number           *
     H*                as part of 147WBF.
     H*
     H* 04/05/10 C:189 Project: 139WLM
     H*                 -EDI-Create diary when EDI Log History
     H*                  PROJECT RE-JAD , RE-DESIGNED
     H*                                                               *
     H*
     H* 02/01/10 C:235 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H* 10/09/09 C:253 Project: 139WLM
     H*                 -EDI-Create diary when EDI Log History
     H*
     H*
     H* 05/29/09 C:189 Project: 139WLM
     H*                 -EDI-Create diary when EDI Log History
     H*
     H*
     H* 12/07/07 C:53 Project: WC1177
     H*              Created from W8078R1 to process the R3 ack file.
     H*
     H* 06/20/07 C:53 114WMF  BACK TO BASE-114WJE                     *
     H* 03/15/05 C:15 Project: 114WJE                                 *
     H*              -Use CMASCL instead of CMACL#.                   *
     H*
     H* 03/16/07 C:53 Support: 57473                                  *
     H*               For client 132 don't update log status from     *
     H*               ROES error if an AN transmission.               *
     H*                                                               *
     H* 07/24/06 C:53 PROJECT: WC1434
     H*               ROES ASP integration (RAK1)
     H*
     H* 08/03/00 C:15 Support: WC082/23790                            *
     H*               Added code to validate the claim # in the 824   *
     H*               file.  If not valid, print on error report.     *
     H*                                                               *
     H* 04/04/00 C:15 Project: WC468                                  *
     H*               Changed pgm to use WTERP instead of WTEMP to    *
     H*               retrieve the error messages.                    *
     H*                                                               *
     H* 05/20/99 C:15 Support: WC096/18132                            *
     H*               Fixed the "Accepted with Errors" report to print*
     H*               the claimant name instead of the employer name. *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMLGL     UF   E           K DISK
     F                                     INFDS(FIDS01)
     FWMM1L1    UF   E           K DISK
     FWMM2L     UF   E           K DISK
     FWXA4L     UF   E           K DISK
     F*** BEGIN CHG *** 04/04/00 **************************************
     F*****WTEML   IF  E           K        DISK
     FWTERL     IF   E           K DISK
     F*** END CHG ***** 04/04/00 **************************************
     FWTDCL     IF   E           K DISK
     F*** BEG ADD ***** 05/29/09 **************************************
     FSTCTL1    IF   E           K DISK
     F                                     RENAME(PCT01:PCT03)
     FWMIXL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F*** END ADD ***** 05/29/09 **************************************
     F*** Begin add *** 11/27/13 **************************************
     FWMFNL     IF   E           K DISK
     FWMCML17   IF   E           K DISK    RENAME(PCM01:PCM17)
     F*** End   add *** 11/27/13 **************************************
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOB)
     F                                     USROPN
     FQSYSPRT3  O    F  132        PRINTER OFLIND(*INOC)
     F                                     USROPN
     FQSYSPRT4  O    F  132        PRINTER OFLIND(*INOD)
     F                                     USROPN

     D*________________________________________________________________
     D DN              S            150    DIM(999)                             Data elements
     D*** BEG ADD ***** 12/07/07 **************************************
     D TX              S             50    DIM(99)                              Error text
     D*** END ADD ***** 12/07/07 **************************************
     D*** BEGIN ADD *** 11/27/13 **************************************
     D XXPDS#          S                   LIKE(CMPDS#) INZ
     D XXFND           S                   LIKE(CMFND) INZ
     D*** END   ADD *** 11/27/13 **************************************
     D*================================================================
     D*
     D*** Define Constants
     D*
     D XCERR1          C                   CONST('NO MATCHING LOG RECO-
     D                                     RD FOUND')
     D*
     D XCERR2          C                   CONST('NO EDI/NOI INFORMATI-
     D                                     ON FOUND')
     D*
     D*** BEGIN ADD *** 08/03/00 **************************************
     D XCERR3          C                   CONST('CLAIM CAN NOT BE DET-
     D                                     ERMINED ')
     D*** END ADD ***** 08/03/00 **************************************
     D*** BEG ADD *** 07/24/06 **************************************
     D XATITL          C                   CONST('             ROES AC-
     D                                     KNOWLEDGEMENT ERROR -
     D                                     REPORT')
     D*** END ADD *** 07/24/06 **************************************
     D*
     D*** Date/Time Data Structure
     D*
     D                 DS            12
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*** Claim Administrator Claim Number
     D*
     D                 DS            25
     D  A4ACLC                 1     25
     D  X1CO#                  1      3  0
     D  X1FND                  4      6  0
     D  X1FYR                  7      9  0
     D  X1CASE                10     16  0
     D*
     D*** BEGIN ADD *** 08/03/00 **************************************
     D                 DS            25
     D  X2ACLC                 1     17
     D  X2CO#                  1      3
     D  X2FND                  4      6
     D  X2FYR                  7      9
     D  X2CASE                10     16
     D*** END ADD ***** 08/03/00 **************************************
     D*** BEGIN ADD *** 11/27/13 **************************************
     D                 DS            25
     D  CMACLC                 1     17
     D    CMCO#                            OVERLAY(CMACLC:1)
     D    CMFND                            OVERLAY(CMACLC:4)
     D    CMFYR                            OVERLAY(CMACLC:7)
     D    CMCASE                           OVERLAY(CMACLC:10)
     D*** END ADD ***** 11/27/13 **************************************
     D*
     D*** Element Numbers
     D*
     D                 DS           396
     D  A4EL01                 1      4
     D  A4EL02                 5      8
     D  A4EL03                 9     12
     D  A4EL04                13     16
     D  A4EL05                17     20
     D  A4EL06                21     24
     D  A4EL07                25     28
     D  A4EL08                29     32
     D  A4EL09                33     36
     D  A4EL10                37     40
     D  A4EL11                41     44
     D  A4EL12                45     48
     D  A4EL13                49     52
     D  A4EL14                53     56
     D  A4EL15                57     60
     D  A4EL16                61     64
     D  A4EL17                65     68
     D  A4EL18                69     72
     D  A4EL19                73     76
     D  A4EL20                77     80
     D  A4EL21                81     84
     D  A4EL22                85     88
     D  A4EL23                89     92
     D  A4EL24                93     96
     D  A4EL25                97    100
     D  A4EL26               101    104
     D  A4EL27               105    108
     D  A4EL28               109    112
     D  A4EL29               113    116
     D  A4EL30               117    120
     D  A4EL31               121    124
     D  A4EL32               125    128
     D  A4EL33               129    132
     D  A4EL34               133    136
     D  A4EL35               137    140
     D  A4EL36               141    144
     D  A4EL37               145    148
     D  A4EL38               149    152
     D  A4EL39               153    156
     D  A4EL40               157    160
     D  A4EL41               161    164
     D  A4EL42               165    168
     D  A4EL43               169    172
     D  A4EL44               173    176
     D  A4EL45               177    180
     D  A4EL46               181    184
     D  A4EL47               185    188
     D  A4EL48               189    192
     D  A4EL49               193    196
     D  A4EL50               197    200
     D  A4EL51               201    204
     D  A4EL52               205    208
     D  A4EL53               209    212
     D  A4EL54               213    216
     D  A4EL55               217    220
     D  A4EL56               221    224
     D  A4EL57               225    228
     D  A4EL58               229    232
     D  A4EL59               233    236
     D  A4EL60               237    240
     D  A4EL61               241    244
     D  A4EL62               245    248
     D  A4EL63               249    252
     D  A4EL64               253    256
     D  A4EL65               257    260
     D  A4EL66               261    264
     D  A4EL67               265    268
     D  A4EL68               269    272
     D  A4EL69               273    276
     D  A4EL70               277    280
     D  A4EL71               281    284
     D  A4EL72               285    288
     D  A4EL73               289    292
     D  A4EL74               293    296
     D  A4EL75               297    300
     D  A4EL76               301    304
     D  A4EL77               305    308
     D  A4EL78               309    312
     D  A4EL79               313    316
     D  A4EL80               317    320
     D  A4EL81               321    324
     D  A4EL82               325    328
     D  A4EL83               329    332
     D  A4EL84               333    336
     D  A4EL85               337    340
     D  A4EL86               341    344
     D  A4EL87               345    348
     D  A4EL88               349    352
     D  A4EL89               353    356
     D  A4EL90               357    360
     D  A4EL91               361    364
     D  A4EL92               365    368
     D  A4EL93               369    372
     D  A4EL94               373    376
     D  A4EL95               377    380
     D  A4EL96               381    384
     D  A4EL97               385    388
     D  A4EL98               389    392
     D  A4EL99               393    396
     D  D#                     1    396
     D                                     DIM(99)                              Data element #
     D*
     D*** Error Numbers
     D*
     D                 DS           297
     D  A4ER01                 1      3
     D  A4ER02                 4      6
     D  A4ER03                 7      9
     D  A4ER04                10     12
     D  A4ER05                13     15
     D  A4ER06                16     18
     D  A4ER07                19     21
     D  A4ER08                22     24
     D  A4ER09                25     27
     D  A4ER10                28     30
     D  A4ER11                31     33
     D  A4ER12                34     36
     D  A4ER13                37     39
     D  A4ER14                40     42
     D  A4ER15                43     45
     D  A4ER16                46     48
     D  A4ER17                49     51
     D  A4ER18                52     54
     D  A4ER19                55     57
     D  A4ER20                58     60
     D  A4ER21                61     63
     D  A4ER22                64     66
     D  A4ER23                67     69
     D  A4ER24                70     72
     D  A4ER25                73     75
     D  A4ER26                76     78
     D  A4ER27                79     81
     D  A4ER28                82     84
     D  A4ER29                85     87
     D  A4ER30                88     90
     D  A4ER31                91     93
     D  A4ER32                94     96
     D  A4ER33                97     99
     D  A4ER34               100    102
     D  A4ER35               103    105
     D  A4ER36               106    108
     D  A4ER37               109    111
     D  A4ER38               112    114
     D  A4ER39               115    117
     D  A4ER40               118    120
     D  A4ER41               121    123
     D  A4ER42               124    126
     D  A4ER43               127    129
     D  A4ER44               130    132
     D  A4ER45               133    135
     D  A4ER46               136    138
     D  A4ER47               139    141
     D  A4ER48               142    144
     D  A4ER49               145    147
     D  A4ER50               148    150
     D  A4ER51               151    153
     D  A4ER52               154    156
     D  A4ER53               157    159
     D  A4ER54               160    162
     D  A4ER55               163    165
     D  A4ER56               166    168
     D  A4ER57               169    171
     D  A4ER58               172    174
     D  A4ER59               175    177
     D  A4ER60               178    180
     D  A4ER61               181    183
     D  A4ER62               184    186
     D  A4ER63               187    189
     D  A4ER64               190    192
     D  A4ER65               193    195
     D  A4ER66               196    198
     D  A4ER67               199    201
     D  A4ER68               202    204
     D  A4ER69               205    207
     D  A4ER70               208    210
     D  A4ER71               211    213
     D  A4ER72               214    216
     D  A4ER73               217    219
     D  A4ER74               220    222
     D  A4ER75               223    225
     D  A4ER76               226    228
     D  A4ER77               229    231
     D  A4ER78               232    234
     D  A4ER79               235    237
     D  A4ER80               238    240
     D  A4ER81               241    243
     D  A4ER82               244    246
     D  A4ER83               247    249
     D  A4ER84               250    252
     D  A4ER85               253    255
     D  A4ER86               256    258
     D  A4ER87               259    261
     D  A4ER88               262    264
     D  A4ER89               265    267
     D  A4ER90               268    270
     D  A4ER91               271    273
     D  A4ER92               274    276
     D  A4ER93               277    279
     D  A4ER94               280    282
     D  A4ER95               283    285
     D  A4ER96               286    288
     D  A4ER97               289    291
     D  A4ER98               292    294
     D  A4ER99               295    297
     D  E#                     1    297
     D                                     DIM(99)                              Error #
     D*** BEG ADD ***** 05/29/09 **************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D*** END ADD ***** 05/29/09 **************************************
     D*
     D*   Override printer file command
     D*
     D XXCMD           DS           512
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  Q2RRN#               397    400B 0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   Define Key lists
     C*
     C*          SMCOL - Company Master
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*                                                                   SRINIT
     C*          SMORL - Output Routing File                              SRINIT
     C*                                                                   SRINIT
     C     ORKY          KLIST                                                                 SRINI
     C                   KFLD                    Q1PGM                                         SRINI
     C                   KFLD                    XXFILE                                        SRINI
     C*
     C*          WXA4L - Acknowledgment Work File
     C*
     C     A4KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQJST
     C                   KFLD                    XXOTDC
     C                   KFLD                    XXOTTC
     C*
     C*          WMLGL - EDI Log File
     C*
     C     LGKY          KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1CASE
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*       WMM2L - Transmission Management File
     C*
     C     M2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    A4JST
     C                   KFLD                    A4TSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM            6 0
     C*
     C*       WMM1L - Extract Management File
     C*
     C     M1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    A4JST
     C                   KFLD                    A4TSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C*** BEGIN DLT *** 04/04/00 **************************************
     C********** WTEML - Error Table
     C**********
     C********** EMKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXJST
     C**********           KFLD           XXDN#
     C**********
     C********** EMK2      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXJST
     C**********           KFLD           XXDN#
     C*** END DLT ***** 04/04/00 **************************************
     C*** BEGIN ADD *** 04/04/00 **************************************
     C*          WTERL - Error Table
     C*
     C     ERKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXMSG#
     C*** END ADD ***** 04/04/00 **************************************
     C*
     C*          WTDCL - Data Elements
     C*
     C     DCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C*
     C*** BEG ADD ***** 05/29/09 **************************************
     C     IXKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGTSET
     C*** END ADD ***** 05/29/09 **************************************
     C*** BEG ADD ***** 11/27/13 **************************************
     C     CMKY17        KLIST
     C                   KFLD                    XXCO#
     C*** BEG DEL ***** 03/13/14 **************************************
     C****************** KFLD ****************** XXJST
     C*** END DEL ***** 03/13/14 **************************************
     C                   KFLD                    XXPDS#
     C*** BEG ADD ***** 03/13/14 **************************************
     C                   KFLD                    XXJST
     C*** END ADD ***** 03/13/14 **************************************
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END ADD ***** 11/27/13 **************************************
     C*   Define PLISTS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQOTDT            8
     C                   PARM                    QQOTTM            6
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
     C*   Define work fields
     C*
     C     *LIKE         DEFINE    LGCO#         XXCO#
     C     *LIKE         DEFINE    A4JST         XXJST
     C     *LIKE         DEFINE    A4TSET        XXSET
     C     *LIKE         DEFINE    A4MTC         XXMTC
     C     *LIKE         DEFINE    ORFIL#        XXFILE                                        SRINI
     C     *LIKE         DEFINE    ORTITL        XXTITL                                        SRINI
     C     *LIKE         DEFINE    ORTITL        X2TITL
     C     *LIKE         DEFINE    ORTITL        X3TITL
     C     *LIKE         DEFINE    ORTITL        X4TITL
     C*** BEGIN DLT *** 04/04/00 **************************************
     C********** *LIKE     DEFN EMDN#     XXDN#
     C********** *LIKE     DEFN EMDN#     SVDN#
     C********** *LIKE     DEFN EMMSG#    XXMSG#
     C*** END DLT ***** 04/04/00 **************************************
     C*** BEGIN DLT *** 04/04/00 **************************************
     C     *LIKE         DEFINE    DCDN#         XXDN#
     C     *LIKE         DEFINE    DCDN#         SVDN#
     C     *LIKE         DEFINE    ERMSG#        XXMSG#
     C*** END DLT ***** 04/04/00 **************************************
     C     *LIKE         DEFINE    *INLR         XXLGRF                                        SRINI
     C     *LIKE         DEFINE    M2OTDT        XXOTDT
     C*** BEG ADD *** 07/24/06 ****************************
     C     *LIKE         DEFINE    *IN99         XX1ST
     C*** END ADD *** 07/24/06 ****************************
     C*** BEG ADD *** 05/29/09 ****************************
     C     *LIKE         DEFINE    LGSTS         SVSTS#
     C*** END ADD *** 05/29/09 ****************************
     C*** BEG DEL *** 04/05/10 ****************************
     C*** BEG ADD *** 02/01/10 ****************************
     C************           *LIKE     DEFN LGMTC     SVMTC#
     C*** END ADD *** 02/01/10 ****************************
     C*** END DEL *** 04/05/10 ****************************
     C*
     C*** BEGIN DLT *** 05/20/99 **************************************
     C*******************  MOVE *BLANKS   XXENAM 30
     C*** END DLT ***** 05/20/99 **************************************
     C                   MOVE      *BLANKS       XXCLNM           30
     C                   MOVE      *BLANKS       XXDAT8            8
     C                   MOVE      *BLANKS       XXOTDC            8
     C                   MOVE      *ZEROS        XXOTTM
     C                   MOVE      *BLANKS       XXOTTC            6
     C                   MOVE      *ZEROS        XXDTIN            8 0
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *ZEROS        ARDATE            8 0
     C                   MOVE      *BLANKS       XXERDS           30
     C                   MOVE      *BLANKS       XXDCDS           30
     C                   MOVE      *ZEROS        P                 5 0
     C                   MOVE      *ZEROS        XXLEN            15 5
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQOTDT        XXOTDC
     C                   MOVE      QQOTDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXOTDT
     C                   MOVE      QQOTTM        XXOTTC
     C                   MOVE      QQOTTM        XXOTTM
     C*** BEG ADD *** 07/24/06 ****************************
     C                   MOVE      *OFF          XX1ST
     C*** END ADD *** 07/24/06 ****************************
     C*** BEG ADD *** 03/16/07 ****************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD *** 03/16/07 ****************************
     C*
     C*          Retrieve the Company Master
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*===== MAINLINE =================================================
     C*
     C*   Read the acknowledgment records.  For each record found, run
     C*   the edits, print on report, and update the error flag in the
     C*   acknowledgment detail record.
     C*
     C*
     C*          Retrieve the Header Record
     C*
     C     A4KY          CHAIN     PHA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     HALTDT        XXDAT4            4              YEAR
     C                   MOVE      XXDAT4        ARDATE
     C                   MOVE      HALTDT        XXDAT4                           MO DAY
     C                   MOVEL     XXDAT4        ARDATE
     C                   MOVE      ' '           HASTAT
     C                   UPDATE    PHA01
     C                   ELSE
     C                   MOVE      *ZEROS        ARDATE
     C                   ENDIF
     C*
     C*          Override printer files and print distribution pages.
     C*
     C*** BEG DEL *** 07/24/06 ****************************
     C***********          EXSR SRPDST
     C*** END DEL *** 07/24/06 ****************************
     C*
     C*          Retrieve the First Detail Acknowledgment Record
     C*
     C     A4KY          CHAIN     PA401                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 07/24/06 ****************************
     C     XX1ST         IFEQ      *OFF
     C                   EXSR      SRPDST
     C                   MOVE      *ON           XX1ST
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C*
     C*** BEGIN ADD *** 08/03/00 **************************************
     C*          Determine if the Admin Claim # is valid
     C*
     C                   MOVEL     A4ACLC        X2ACLC
     C                   TESTN                   X2CO#                9999
     C   99              TESTN                   X2FND                9999
     C   99              TESTN                   X2FYR                9999
     C   99              TESTN                   X2CASE               9999
     C*
     C     *IN99         IFEQ      *OFF
      ******* Begin Add ********* 11/27/13 *********************************
     C                   EXSR      OptaCmpAcn
     C                   ENDIF
     C     *IN99         IFEQ      *OFF
      ******* End   Add ********* 11/27/13 *********************************
     C                   MOVEL     XCERR3        XXERDS
     C   OA              EXCEPT    RPHDR1
     C                   EXCEPT    RPDT1A
     C*
     C                   ELSE
     C*
     C*** END ADD ***** 08/03/00 **************************************
     C*
     C*          Retrieve the open EDI Log Record
     C*
     C                   EXSR      SRRLOG
     C     XXLGRF        IFEQ      *OFF
     C                   MOVE      '1'           A4ERRF
     C                   EXSR      SRPDTL
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       DN
     C                   Z-ADD     Q2RRN#        QQRRN#            9 0
     C                   Z-ADD     LGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     C                   MOVE      XXDATE        QQEXDT            8
     C                   MOVE      X1CO#         QQCO#
     C                   MOVE      X1FND         QQFND
     C                   MOVE      X1FYR         QQFYR
     C                   MOVE      X1CASE        QQCASE
     C                   MOVE      LGTSET        QQTSET
     C                   MOVE      LGMTC         QQMTC
     C                   CALL      'W8070R6'
     C                   PARM                    QQEXDT
     C                   PARM                    QQCO#             3            Key info
     C                   PARM                    QQFND             3            Key info
     C                   PARM                    QQFYR             3            Key info
     C                   PARM                    QQCASE            7            Key info
     C                   PARM                    QQTSET            3
     C                   PARM                    QQMTC             2            Req data
     C                   PARM                    QQJST                          Req data
     C                   PARM                    DN
     C*
     C*** BEGIN DLT *** 05/20/99 **************************************
     C******************** MOVELDN,17     XXENAM
     C*** END DLT ***** 05/20/99 **************************************
     C*
     C*       Retrieve the Claim Master
     C*
     C     CMKY          CHAIN     PCM01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      '2'           A4ERRF
     C                   EXSR      SRPDTL
     C*
     C                   ELSE
     C*
     C*       Get Accident date
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTIN
     C*
     C*  GET THE CLAIMANT'S NAME
     C                   MOVE      *BLANKS       CDLSN
     C                   MOVE      *BLANKS       CDFSN
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVEL     CDLSN         XXCLNM
     C                   CAT       ',':0         XXCLNM
     C                   CAT       CDFSN:1       XXCLNM
     C*
     C                   MOVE      *BLANKS       A4ERRF
     C                   MOVE      'C'           A4STAT
     C                   EXSR      SRPDTL
     C*
     C*          Update the Log and Claim Records that were
     C*          retrieved.
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEG ADD *** 07/24/06 ****************************
     C     A4TAK1        IFNE      'AR1'
     C*** END ADD *** 07/24/06 ****************************
     C                   MOVEL     SRDTO         LGARCV
     C                   MOVEL     SRDTO         LGAENT
     C                   MOVEL     A4ACKC        LGSTS
     C*** BEG ADD *** 07/24/06 ****************************
     C                   ELSE
     C*** BEG ADD *** 03/16/07 ****************************
     C     CLTID         IFEQ      '132'
     C     LGMTC         ANDEQ     'AN'
     C                   ELSE
     C*** END ADD *** 03/16/07 ****************************
     C                   MOVEL     'RE'          LGSTS
     C                   Z-ADD     *ZEROS        LGSENT
     C*** BEG ADD *** 03/16/07 ****************************
     C                   ENDIF
     C*** END ADD *** 03/16/07 ****************************
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C                   MOVE      QQDSQG        LGDSQG
     C*** BEG ADD ***** 05/29/09 **************************************
     C*** BEG DEL *** 04/05/10 ****************************
     C***********CMCLEX    CHAINPUP01                60
     C************IN60     IFEQ *OFF
     C***********          MOVE UPUSR     Q#USER 10 P
     C***********          ENDIF
     C***********
     C*** BEG ADD ***** 02/01/10 **************************************
     C***********LGTSET    IFNE 'A49'
     C***** END ADD ***** 02/01/10 **************************************
     C***********LGSTS     IFNE SVSTS#
     C********** BEG ADD ***** 02/01/10 **************************************
     C*********  LGMTC     ORNE SVMTC#
     C********* END ADD ***** 02/01/10 **************************************
     C***********UPUSR     ANDNE*BLANKS
     C*** END DEL *** 04/05/10 ****************************
     C*** BEG ADD *** 04/05/10 ****************************
     C     LGSTS         IFNE      SVSTS#
     C*** END ADD *** 04/05/10 ****************************
     C*
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C*** BEG DEL *** 04/05/10 ****************************
     C*** BEG ADD ***** 02/01/10 **************************************
     C***********          ENDIF
     C***********LGTSET    IFEQ 'A49'
     C***********LGSTS     IFNE SVSTS#
     C***********LGMTC     ORNE SVMTC#
     C***********UPUSR     ANDNE*BLANKS
     C***********LGSTS     IFEQ 'EI'
     C***********LGSTS     OREQ 'EC'
     C***********          EXSR DARTS
     C***********          ENDIF
     C***********          ENDIF
     C***********          ENDIF
     C*** END ADD ***** 02/01/10 **************************************
     C*** END DEL *** 04/05/10 ****************************
     C*** END ADD ***** 05/29/09 **************************************
     C                   UPDATE    PLG01
     C*
     C*** Begin add *** 03/15/05 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVEL     A4ACL#        CMASCL
     C                   ELSE
     C*** End add ***** 03/15/05 **************************************
     C                   MOVEL     A4ACL#        CMACL#
     C*** Begin add *** 03/15/05 **************************************
     C                   ENDIF
     C*** End add ***** 03/15/05 **************************************
     C*** BEG ADD ***** 12/07/07 **************************************
     C                   MOVEL(P)  A4JURB        CMJURO
     C*** END ADD ***** 12/07/07 **************************************
     C                   UPDATE    PCM01
     C*
     C*
     C*          Generate the Diary Remark, IF THERE WERE ERRORS.
     C*
     C     A4ACKC        IFNE      'TA'
     C                   MOVE      LGFND         QQFND
     C                   MOVE      LGFYR         QQFYR
     C                   MOVE      LGCASE        QQCASE
     C                   MOVE      LGTSET        QQTSET
     C                   MOVE      *BLANKS       QQDSQG
     C*
     C                   CALL      'W0071R6'
     C                   PARM                    QQCO#                          COMPANY
     C                   PARM                    QQJST                          JURISTDICTION
     C                   PARM                    QQFND             3            FUND ID
     C                   PARM                    QQFYR             3            FUND YR
     C                   PARM                    QQCASE            7            CASE/CLAIM
     C                   PARM                    QQTSET            3            TX SET
     C                   PARM                    QQDSQG            5            DARTS SEQ#
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/03/00 **************************************
     C                   ENDIF
     C*** END ADD ***** 08/03/00 **************************************
     C*
     C     A4KY          READE     PA401                                  60
     C                   ENDDO
     C*
     C*** BEG ADD *** 07/24/06 ****************************
     C     A4TAK1        IFNE      'AR1'
     C*** END ADD *** 07/24/06 ****************************
     C*
     C*   Update the management records
     C*
     C     M2KY          CHAIN     PM201                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M2ARCV
     C                   MOVEL     SRDTO         M2AENT
     C                   UPDATE    PM201
     C                   ENDIF
     C*
     C     M1KY          CHAIN     PM101                              60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1ARCV
     C                   MOVEL     SRDTO         M1AENT
     C                   UPDATE    PM101
     C     M1KY          READE     PM101                                  60
     C                   ENDDO
     C*
     C*** BEG ADD *** 07/24/06 ****************************
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C*
     C*   End Program
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C*   Routine - SRRLOG
     C*
     C*   Find the associated log record.  The match must be on:
     C*      - Transaction Set Code
     C*      - Maintenance Type Code
     C*      - STATUS CODE MUST BE 'SN'
     C*
-BSR CSR   SRRLOG        BEGSR                                                                 SRINI
     C*                                                                   SRINIT
     CSR                 MOVE      *OFF          XXLGRF
     C*                                                                   SRINIT
     CSR   LGKY          CHAIN     PLG01                              61
     CSR   *IN61         DOWEQ     *OFF
     CSR   LGTSET        IFEQ      A4TSET
     CSR   LGMTC         ANDEQ     A4MTC
     CSR   LGSTS         ANDEQ     'SN'
     C*** BEG ADD ***** 05/29/09 **************************************
     C                   MOVEL     LGSTS         SVSTS#
     C*** END ADD ***** 05/29/09 **************************************
     CSR                 MOVE      *ON           XXLGRF
     CSR                 LEAVE
     CSR                 ENDIF
     CSR   LGKY          READE     PLG01                                  61
     CSR                 ENDDO
     C*                                                                   SRINIT
-ESR CSR   ENRLOG        ENDSR                                                                 SRINI
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C*   Routine - SRPDST
     C*
     C*   Print the Distribution and Header pages for each of the
     C*   reports.
     C*
-BSR CSR   SRPDST        BEGSR                                                                 SRINI
     C*
     C*** BEG ADD *** 07/24/06 ****************************
     C     A4TAK1        IFNE      'AR1'
     C*** END ADD *** 07/24/06 ****************************
     C*                                                                   SRINIT
     C*          File 1 - Fatal Error Report.                             SRINIT
     C*                                                                   SRINIT
     C*
     CSR                 OPEN      QSYSPRT1
     CSR                 MOVE      '1'           XXFILE                                        SRINI
     C*                                                                   SRINIT
     CSR   ORKY          CHAIN     POR01                              61                       SRINI
     CSR   *IN61         IFEQ      '0'                                                         SRINI
     CSR                 MOVEL     ORTITL        XXTITL                                        SRINI
     CSR                 EXCEPT    RPDST1                                                      SRINI
     CSR                 ENDIF                                                                 SRINI
     C*
     CSR                 EXCEPT    RPHDR1
     CSR                 MOVE      *OFF          *INOA
     C*                                                                   SRINIT
     C*          File 2 - Accepted with no errors report.                 SRINIT
     C*                                                                   SRINIT
     CSR                 OPEN      QSYSPRT2
     CSR                 MOVE      '2'           XXFILE                                        SRINI
     C*                                                                   SRINIT
     CSR   ORKY          CHAIN     POR01                              61                       SRINI
     CSR   *IN61         IFEQ      '0'                                                         SRINI
     C                   MOVEL     ORTITL        X2TITL                                        SRINI
     CSR                 EXCEPT    RPDST2                                                      SRINI
     CSR                 ENDIF                                                                 SRINI
     C*
     CSR                 EXCEPT    RPHDR2
     CSR                 MOVE      *OFF          *INOB
     C*                                                                   SRINIT
     C*          File 3 - Accepted with errors report.                    SRINIT
     C*                                                                   SRINIT
     CSR                 OPEN      QSYSPRT3
     CSR                 MOVE      '3'           XXFILE                                        SRINI
     C*                                                                   SRINIT
     CSR   ORKY          CHAIN     POR01                              61                       SRINI
     CSR   *IN61         IFEQ      '0'                                                         SRINI
     C                   MOVEL     ORTITL        X3TITL                                        SRINI
     CSR                 EXCEPT    RPDST3                                                      SRINI
     CSR                 ENDIF                                                                 SRINI
     C*
     CSR                 EXCEPT    RPHDR3
     CSR                 MOVE      *OFF          *INOC
     C*                                                                   SRINIT
     C*** BEG ADD *** 07/24/06 ****************************
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C*                                                                   SRINIT
     C*          File 4 - Rejected with errors report.                    SRINIT
     C*                                                                   SRINIT
     CSR                 OPEN      QSYSPRT4
     CSR                 MOVE      '4'           XXFILE                                        SRINI
     C*                                                                   SRINIT
     CSR   ORKY          CHAIN     POR01                              61                       SRINI
     CSR   *IN61         IFEQ      '0'                                                         SRINI
     C                   MOVEL     ORTITL        X4TITL                                        SRINI
     CSR                 EXCEPT    RPDST4                                                      SRINI
     CSR                 ENDIF                                                                 SRINI
     C*
     C*** BEG ADD *** 07/24/06 ****************************
     C     A4TAK1        IFEQ      'AR1'
     C                   MOVEL     XATITL        X4TITL
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C*
     CSR                 EXCEPT    RPHDR4
     CSR                 MOVE      *OFF          *INOD
     C*                                                                   SRINIT
-ESR CSR   ENPDST        ENDSR                                                                 SRINI
     C*
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C*   Routine - SRPDTL
     C*
     C*   Print the Edit Reports.
     C*
-BSR CSR   SRPDTL        BEGSR                                                                 SRINI
     C*                                                                   SRINIT
     CSR                 SELECT
     C*
     C*          Acknowledgments not processed, not in Claim Data
     C*          Base.
     C*
     CSR   A4ERRF        WHENEQ    '1'
     CSR   A4ERRF        OREQ      '2'
     CSR                 MOVE      *BLANKS       XXERDS
     CSR                 MOVEL     XCERR1        XXERDS
     CSR   A4ERRF        IFEQ      '2'
     CSR                 MOVE      *BLANKS       XXERDS
     CSR                 MOVEL     XCERR2        XXERDS
     CSR                 ENDIF
     C*** BEG ADD *** 07/24/06 ****************************
     C     A4TAK1        IFNE      'AR1'
     C*** END ADD *** 07/24/06 ****************************
     CSR                 EXCEPT    RPDTL1
     C*** BEG ADD *** 07/24/06 ****************************
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C*
     CSR   *INOA         IFEQ      *ON
     CSR                 EXCEPT    RPHDR1
     CSR                 MOVE      *OFF          *INOA
     CSR                 ENDIF
     C*                                                                   SRINIT
     C*          Acknowledgments accepted, no errors.
     C*
     CSR   A4ACKC        WHENEQ    'TA'
     C*** BEG ADD *** 07/24/06 ****************************
     C     A4TAK1        ANDNE     'AR1'
     C*** END ADD *** 07/24/06 ****************************
     CSR                 EXCEPT    RPDTL2
     C*
     CSR   *INOB         IFEQ      *ON
     CSR                 EXCEPT    RPHDR2
     CSR                 MOVE      *OFF          *INOB
     CSR                 ENDIF
     C*                                                                   SRINIT
     C*          Acknowledgments accepted with errors.
     C*
     CSR   A4ACKC        WHENEQ    'TE'
     C*** BEG ADD *** 07/24/06 ****************************
     C     A4TAK1        ANDNE     'AR1'
     C*** END ADD *** 07/24/06 ****************************
     CSR                 EXCEPT    RPDTL3
     C*
     CSR   *INOC         IFEQ      *ON
     CSR                 EXCEPT    RPHDR3
     CSR                 MOVE      *OFF          *INOC
     CSR                 ENDIF
     C*                                                                   SRINIT
     CSR   1             DO        99            P
     CSR   ' ':'0'       XLATE     E#(P)         E#(P)
     CSR   E#(P)         IFGT      *ZEROS
     CSR   ' ':'0'       XLATE     D#(P)         D#(P)
     CSR                 MOVE      D#(P)         XXDN#
     CSR                 MOVE      A4TSET        XXSET
     CSR                 MOVE      *BLANKS       XXMTC
     CSR                 MOVE      E#(P)         XXMSG#
     C*** BEG CHG ***** 12/07/07 **************************************
     C***********          MOVE *ALL'*'   XXERDS
     C                   EXSR      SRTEXT
     C*** END CHG ***** 12/07/07 **************************************
     C*
     C     XXDN#         IFNE      SVDN#
     C     DCKY          CHAIN     PDC01                              67
     C     *IN67         IFEQ      *ON
     C                   MOVE      *ALL'*'       DCDESC
     C                   ENDIF
     C                   Z-ADD     XXDN#         SVDN#
     C                   MOVEL     DCDESC        XXDCDS           30
     C                   ENDIF
     C                   MOVE      *OFF          *IN62
     C*
     C*** BEGIN DLT *** 04/04/00 **************************************
     C********** EMKY      SETLLPEM01
     C********** EMK2      READEPEM01                    62
     C**********
     C********** *IN62     DOWEQ*OFF
     C********** EMSET     IFEQ XXSET
     C********** EMSET     OREQ *BLANKS
     C**********
     C********** EMMTC     IFEQ A4MTC
     C********** EMMTC     OREQ *BLANKS
     C**********
     C********** EMMSG#    IFEQ XXMSG#
     C**********           MOVELEMDESC    XXERDS
     C**********           LEAVE
     C**********           ENDIF
     C**********           ENDIF
     C**********           ENDIF
     C**********
     C********** EMK2      READEPEM01                    62
     C**********           ENDDO
     C*** END DLT ***** 04/04/00 **************************************
     C*** BEGIN ADD *** 04/04/00 **************************************
     C     ERKY          CHAIN     PER01                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     ERDESC        XXERDS
     C                   ENDIF
     C*** END ADD ***** 04/04/00 **************************************
     C*
     CSR                 EXCEPT    RPERR3
     CSR   *INOC         IFEQ      *ON
     CSR                 EXCEPT    RPHDR3
     CSR                 MOVE      *OFF          *INOC
     CSR                 ENDIF
     CSR                 ENDIF
     CSR                 ENDDO
     C*                                                                   SRINIT
     C*          Acknowledgments rejected with errors.
     C*
     CSR   A4ACKC        WHENEQ    'TR'
     C*** BEG ADD *** 07/24/06 ****************************
     CSR   A4ACKC        OREQ      'TE'
     C     A4TAK1        ANDEQ     'AR1'
     C*** END ADD *** 07/24/06 ****************************
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     CSR                 EXCEPT    RPDTL4
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*
     CSR   *INOD         IFEQ      *ON
     CSR                 EXCEPT    RPHDR4
     CSR                 MOVE      *OFF          *INOD
     CSR                 ENDIF
     C*                                                                   SRINIT
     CSR   1             DO        99            P
     CSR   ' ':'0'       XLATE     E#(P)         E#(P)
     CSR   E#(P)         IFGT      *ZEROS
     CSR   ' ':'0'       XLATE     D#(P)         D#(P)
     CSR                 MOVE      A4TSET        XXSET
     C                   MOVE      D#(P)         XXDN#
     CSR                 MOVE      *BLANKS       XXMTC
     CSR                 MOVE      E#(P)         XXMSG#
     C*** BEG CHG ***** 12/07/07 **************************************
     C***********          MOVE *ALL'*'   XXERDS
     C                   EXSR      SRTEXT
     C*** END CHG ***** 12/07/07 **************************************
     C*
     C     XXDN#         IFNE      SVDN#
     C     DCKY          CHAIN     PDC01                              67
     C     *IN67         IFEQ      *ON
     C                   MOVE      *ALL'*'       DCDESC
     C                   ENDIF
     C                   Z-ADD     XXDN#         SVDN#
     C                   MOVEL     DCDESC        XXDCDS           30
     C                   ENDIF
     C*
     C*** BEGIN DLT *** 04/04/00 **************************************
     C********** EMKY      SETLLPEM01
     C********** EMK2      READEPEM01                    62
     C**********
     C********** *IN62     DOWEQ*OFF
     C********** EMSET     IFEQ XXSET
     C********** EMSET     OREQ *BLANKS
     C**********
     C********** EMMTC     IFEQ A4MTC
     C********** EMMTC     OREQ *BLANKS
     C**********
     C********** EMMSG#    IFEQ XXMSG#
     C**********           MOVELEMDESC    XXERDS
     C**********           LEAVE
     C**********           ENDIF
     C**********           ENDIF
     C**********           ENDIF
     C**********
     C********** EMK2      READEPEM01                    62
     C**********           ENDDO
     C*** END DLT ***** 04/04/00 **************************************
     C*** BEGIN ADD *** 04/04/00 **************************************
     C     ERKY          CHAIN     PER01                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     ERDESC        XXERDS
     C                   ENDIF
     C*** END ADD ***** 04/04/00 **************************************
     C*
     CSR                 EXCEPT    RPERR4
     CSR   *INOD         IFEQ      *ON
     CSR                 EXCEPT    RPHDR4
     CSR                 MOVE      *OFF          *INOD
     CSR                 ENDIF
     CSR                 ENDIF
     CSR                 ENDDO
     C*                                                                   SRINIT
     CSR                 OTHER
     C*                                                                   SRINIT
     CSR                 ENDSL
     C*                                                                   SRINIT
-ESR CSR   ENPDTL        ENDSR                                                                 SRINI
     C*** BEG ADD ***** 12/07/07 **************************************
     CSR   SRTEXT        BEGSR                                                                 SRINI
     C*
     C*
     C*** Error Text
     C*
     C                   SELECT
     C     P             WHENEQ    1
     C                   MOVEL(P)  A4TX01        XXERDS
     C     P             WHENEQ    2
     C                   MOVEL(P)  A4TX02        XXERDS
     C     P             WHENEQ    3
     C                   MOVEL(P)  A4TX03        XXERDS
     C     P             WHENEQ    4
     C                   MOVEL(P)  A4TX04        XXERDS
     C     P             WHENEQ    5
     C                   MOVEL(P)  A4TX05        XXERDS
     C     P             WHENEQ    6
     C                   MOVEL(P)  A4TX06        XXERDS
     C     P             WHENEQ    7
     C                   MOVEL(P)  A4TX07        XXERDS
     C     P             WHENEQ    8
     C                   MOVEL(P)  A4TX08        XXERDS
     C     P             WHENEQ    9
     C                   MOVEL(P)  A4TX09        XXERDS
     C     P             WHENEQ    10
     C                   MOVEL(P)  A4TX10        XXERDS
     C     P             WHENEQ    11
     C                   MOVEL(P)  A4TX11        XXERDS
     C     P             WHENEQ    12
     C                   MOVEL(P)  A4TX12        XXERDS
     C     P             WHENEQ    13
     C                   MOVEL(P)  A4TX13        XXERDS
     C     P             WHENEQ    14
     C                   MOVEL(P)  A4TX14        XXERDS
     C     P             WHENEQ    15
     C                   MOVEL(P)  A4TX15        XXERDS
     C     P             WHENEQ    16
     C                   MOVEL(P)  A4TX16        XXERDS
     C     P             WHENEQ    17
     C                   MOVEL(P)  A4TX17        XXERDS
     C     P             WHENEQ    18
     C                   MOVEL(P)  A4TX18        XXERDS
     C     P             WHENEQ    19
     C                   MOVEL(P)  A4TX19        XXERDS
     C     P             WHENEQ    20
     C                   MOVEL(P)  A4TX20        XXERDS
     C     P             WHENEQ    21
     C                   MOVEL(P)  A4TX21        XXERDS
     C     P             WHENEQ    22
     C                   MOVEL(P)  A4TX22        XXERDS
     C     P             WHENEQ    23
     C                   MOVEL(P)  A4TX23        XXERDS
     C     P             WHENEQ    24
     C                   MOVEL(P)  A4TX24        XXERDS
     C     P             WHENEQ    25
     C                   MOVEL(P)  A4TX25        XXERDS
     C     P             WHENEQ    26
     C                   MOVEL(P)  A4TX26        XXERDS
     C     P             WHENEQ    27
     C                   MOVEL(P)  A4TX27        XXERDS
     C     P             WHENEQ    28
     C                   MOVEL(P)  A4TX28        XXERDS
     C     P             WHENEQ    29
     C                   MOVEL(P)  A4TX29        XXERDS
     C     P             WHENEQ    30
     C                   MOVEL(P)  A4TX30        XXERDS
     C     P             WHENEQ    31
     C                   MOVEL(P)  A4TX31        XXERDS
     C     P             WHENEQ    32
     C                   MOVEL(P)  A4TX32        XXERDS
     C     P             WHENEQ    33
     C                   MOVEL(P)  A4TX33        XXERDS
     C     P             WHENEQ    34
     C                   MOVEL(P)  A4TX34        XXERDS
     C     P             WHENEQ    35
     C                   MOVEL(P)  A4TX35        XXERDS
     C     P             WHENEQ    36
     C                   MOVEL(P)  A4TX36        XXERDS
     C     P             WHENEQ    37
     C                   MOVEL(P)  A4TX37        XXERDS
     C     P             WHENEQ    38
     C                   MOVEL(P)  A4TX38        XXERDS
     C     P             WHENEQ    39
     C                   MOVEL(P)  A4TX39        XXERDS
     C     P             WHENEQ    40
     C                   MOVEL(P)  A4TX40        XXERDS
     C     P             WHENEQ    41
     C                   MOVEL(P)  A4TX41        XXERDS
     C     P             WHENEQ    42
     C                   MOVEL(P)  A4TX42        XXERDS
     C     P             WHENEQ    43
     C                   MOVEL(P)  A4TX43        XXERDS
     C     P             WHENEQ    44
     C                   MOVEL(P)  A4TX44        XXERDS
     C     P             WHENEQ    45
     C                   MOVEL(P)  A4TX45        XXERDS
     C     P             WHENEQ    46
     C                   MOVEL(P)  A4TX46        XXERDS
     C     P             WHENEQ    47
     C                   MOVEL(P)  A4TX47        XXERDS
     C     P             WHENEQ    48
     C                   MOVEL(P)  A4TX48        XXERDS
     C     P             WHENEQ    49
     C                   MOVEL(P)  A4TX49        XXERDS
     C     P             WHENEQ    50
     C                   MOVEL(P)  A4TX50        XXERDS
     C     P             WHENEQ    51
     C                   MOVEL(P)  A4TX51        XXERDS
     C     P             WHENEQ    52
     C                   MOVEL(P)  A4TX52        XXERDS
     C     P             WHENEQ    53
     C                   MOVEL(P)  A4TX53        XXERDS
     C     P             WHENEQ    54
     C                   MOVEL(P)  A4TX54        XXERDS
     C     P             WHENEQ    55
     C                   MOVEL(P)  A4TX55        XXERDS
     C     P             WHENEQ    56
     C                   MOVEL(P)  A4TX56        XXERDS
     C     P             WHENEQ    57
     C                   MOVEL(P)  A4TX57        XXERDS
     C     P             WHENEQ    58
     C                   MOVEL(P)  A4TX58        XXERDS
     C     P             WHENEQ    59
     C                   MOVEL(P)  A4TX59        XXERDS
     C     P             WHENEQ    60
     C                   MOVEL(P)  A4TX60        XXERDS
     C     P             WHENEQ    61
     C                   MOVEL(P)  A4TX61        XXERDS
     C     P             WHENEQ    62
     C                   MOVEL(P)  A4TX62        XXERDS
     C     P             WHENEQ    63
     C                   MOVEL(P)  A4TX63        XXERDS
     C     P             WHENEQ    64
     C                   MOVEL(P)  A4TX64        XXERDS
     C     P             WHENEQ    65
     C                   MOVEL(P)  A4TX65        XXERDS
     C     P             WHENEQ    66
     C                   MOVEL(P)  A4TX66        XXERDS
     C     P             WHENEQ    67
     C                   MOVEL(P)  A4TX67        XXERDS
     C     P             WHENEQ    68
     C                   MOVEL(P)  A4TX68        XXERDS
     C     P             WHENEQ    69
     C                   MOVEL(P)  A4TX69        XXERDS
     C     P             WHENEQ    70
     C                   MOVEL(P)  A4TX70        XXERDS
     C     P             WHENEQ    71
     C                   MOVEL(P)  A4TX71        XXERDS
     C     P             WHENEQ    72
     C                   MOVEL(P)  A4TX72        XXERDS
     C     P             WHENEQ    73
     C                   MOVEL(P)  A4TX73        XXERDS
     C     P             WHENEQ    74
     C                   MOVEL(P)  A4TX74        XXERDS
     C     P             WHENEQ    75
     C                   MOVEL(P)  A4TX75        XXERDS
     C     P             WHENEQ    76
     C                   MOVEL(P)  A4TX76        XXERDS
     C     P             WHENEQ    77
     C                   MOVEL(P)  A4TX77        XXERDS
     C     P             WHENEQ    78
     C                   MOVEL(P)  A4TX78        XXERDS
     C     P             WHENEQ    79
     C                   MOVEL(P)  A4TX79        XXERDS
     C     P             WHENEQ    80
     C                   MOVEL(P)  A4TX80        XXERDS
     C     P             WHENEQ    81
     C                   MOVEL(P)  A4TX81        XXERDS
     C     P             WHENEQ    82
     C                   MOVEL(P)  A4TX82        XXERDS
     C     P             WHENEQ    83
     C                   MOVEL(P)  A4TX83        XXERDS
     C     P             WHENEQ    84
     C                   MOVEL(P)  A4TX84        XXERDS
     C     P             WHENEQ    85
     C                   MOVEL(P)  A4TX85        XXERDS
     C     P             WHENEQ    86
     C                   MOVEL(P)  A4TX86        XXERDS
     C     P             WHENEQ    87
     C                   MOVEL(P)  A4TX87        XXERDS
     C     P             WHENEQ    88
     C                   MOVEL(P)  A4TX88        XXERDS
     C     P             WHENEQ    89
     C                   MOVEL(P)  A4TX89        XXERDS
     C     P             WHENEQ    90
     C                   MOVEL(P)  A4TX90        XXERDS
     C     P             WHENEQ    91
     C                   MOVEL(P)  A4TX91        XXERDS
     C     P             WHENEQ    92
     C                   MOVEL(P)  A4TX92        XXERDS
     C     P             WHENEQ    93
     C                   MOVEL(P)  A4TX93        XXERDS
     C     P             WHENEQ    94
     C                   MOVEL(P)  A4TX94        XXERDS
     C     P             WHENEQ    95
     C                   MOVEL(P)  A4TX95        XXERDS
     C     P             WHENEQ    96
     C                   MOVEL(P)  A4TX96        XXERDS
     C     P             WHENEQ    97
     C                   MOVEL(P)  A4TX97        XXERDS
     C     P             WHENEQ    98
     C                   MOVEL(P)  A4TX98        XXERDS
     C     P             WHENEQ    99
     C                   MOVEL(P)  A4TX99        XXERDS
     C                   ENDSL
     CSR                 ENDSR                                                                 SRINI
     C*** END ADD ***** 12/07/07 **************************************
     C*** BEG ADD ***** 05/29/09 **************************************
     C     DARTS         BEGSR
     C***** BEG ADD ***** 04/05/10 **************************************
     C     LGTSET        IFEQ      '148'
     C     LGTSET        OREQ      'A49'
     C***** END ADD ***** 04/05/10 **************************************
     C*
     C     IXKY          CHAIN     PIX01                              60
     C*
     C                   SELECT
     C     LGSTS         WHENEQ    'RE'
     C     IXCRER        ANDNE     *BLANKS
     C                   MOVE      IXCRER        Q#RMCD            5
     C*
     C*** BEG CHG ***** 10/09/09 **************************************
     C     LGSTS         WHENEQ    'EI'
     C*********  IXCECR    ANDNE*BLANKS
     C**********           MOVE IXCECR    Q#RMCD  5
     C     IXCEIR        ANDNE     *BLANKS
     C                   MOVE      IXCEIR        Q#RMCD            5
     C*** END CHG ***** 10/09/09 **************************************
     C*
     C     LGSTS         WHENEQ    'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE      IXCECR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE      IXCTAR        Q#RMCD
     C*
     C*** BEG CHG ***** 10/09/09 **************************************
     C     LGSTS         WHENEQ    'TR'
     C******     IXCTAR    ANDNE*BLANKS
     C*******              MOVE IXCTAR    Q#RMCD
     C     IXCTRR        ANDNE     *BLANKS
     C                   MOVE      IXCTRR        Q#RMCD
     C*** END CHG ***** 10/09/09 **************************************
     C*
     C                   OTHER
     C                   GOTO      END
     C                   ENDSL
     C*
     C                   MOVE      LGCO#         QCCO#
     C                   MOVE      LGFND         QCFND
     C                   MOVE      LGFYR         QCFYR
     C                   MOVE      LGCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY
     C*
     C     Q#RMCD        CHAIN     PCT03                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  CTDESC        Q#COMM
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C*
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   UPUSR         Q#USER           10
     C                   ENDIF
     C*** BEG ADD ***** 04/05/10 **************************************
     C     Q#USER        IFEQ      *BLANKS
     C                   GOTO      END
     C                   ENDIF
     C*** END ADD ***** 04/05/10 **************************************
     C*
     C******BEG ADD*****10/09/09***************************************
     C     LGSTS         IFEQ      'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C     LGSTS         IFEQ      'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C******END ADD*****10/09/09***************************************
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD           80
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   ENDIF
     C*
     C     CTDPRG        IFGT      0
     C                   MOVEL(P)  'Y'           Q#FL02            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   ENDIF
     C*
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C***BEG ADD*******10/09/09****************************************
     C                   MOVEL     *BLANKS       Q#COMM
     C     LGMTC         CAT       '-':0         Q#COMM
     C                   CAT       CTDESC:0      Q#COMM
     C***END ADD*******10/09/09****************************************
     C*
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C*** BEG ADD ***** 04/05/10 **************************************
     C                   ENDIF
     C*** END ADD ***** 04/05/10 **************************************
     C     END           ENDSR
     C*** END ADD ***** 05/29/09 **************************************
     C***** BEG ADD ***** 11/27/13 ************************************
      *----------------------------------------------------------------
      * OptaCmpAcn - Determine if OptaComp Admin Claim Number
      *----------------------------------------------------------------
     C     OptaCmpAcn    BEGSR
     C                   MOVEL(P)  A4ACLC        XXPDS#
     C     CMKY17        CHAIN     PCM17

     C                   IF        %FOUND
     C                   MOVE      CMFND         XXFND
     C     FNKY          CHAIN     PFN01

     C*** Begin Chg *** 09/07/19 **************************************
     C**********         IF        %FOUND and FNGRPASC = 'OPTACOMP'
     C                   IF        (%Found and FNgrpAsc = 'OPTACOMP')
     C                             Or cltID = '152'
     C*** End   Chg *** 09/07/19 **************************************
     C                   MOVEL(P)  CMACLC        X2ACLC
     C                   TESTN                   X2CO#                9999
     C   99              TESTN                   X2FND                9999
     C   99              TESTN                   X2FYR                9999
     C   99              TESTN                   X2CASE               9999
     C   99              MOVEL(P)  CMACLC        A4ACLC
     C                   ENDIF
     C                   ENDIF

     C                   ENDSR
      *** END ADD ***** 11/27/13 **************************************
     OQSYSPRT1  E            RPDST1         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDST1         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDST1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST1         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST1         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDST1         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST1         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST1         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDST1         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDST1         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST1         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDST1         2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            RPDST1         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDST1         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            RPDST1           60
     O                                           23 '(C) COPYRIGHT PROGRESSI'
     O                                           46 'VE DATA SOLUTIONS. INC,'
     O                                           51 '.1999'
     O          E            RPHDR1         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              89
     O                       DSDATE        Y    128
     O          E            RPHDR1         2
     O                       Q1PGM               10
     O                       XXTITL              99
     O                                          123 'PAGE:'
     O                       PAGE1         Z    128
     O          E            RPHDR1         2
     O                                           19 'JURISDICTION STATE:'
     O                       XXJST               22
     O                                           47 'ACKNOWLEDGMENT RECEIPT'
     O                                           53 'DATE:'
     O                       ARDATE              64 '  /  /    '
     O          E            RPHDR1         2
     O                                            4 'FUND'
     O                                            8 'YR'
     O                                           17 'CLAIM #'
     O                                           35 'AGENCY CLAIM #'
     O                                           81 'REASON FOR REJECT'
     O          E            RPDTL1         1
     O                       X1FND                4
     O                       X1FYR                8
     O                       X1CASE              17
     O                       A4ACL#              46
     O                       XXERDS              94
     O*** BEGIN ADD *** 08/03/00 ****************************************
     O          E            RPDT1A         1
     O                       X2ACLC              17
     O                       A4ACL#              46
     O                       XXERDS              94
     O*** END ADD ***** 08/03/00 ****************************************
     OQSYSPRT2  E            RPDST2         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDST2         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDST2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST2         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST2         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDST2         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST2         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST2         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDST2         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDST2         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST2         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDST2         2
     O                                           19 'REPORT TITLE:'
     O                       X2TITL              80
     O          E            RPDST2         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDST2         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            RPDST2           60
     O                                           23 '(C) COPYRIGHT PROGRESSI'
     O                                           46 'VE DATA SOLUTIONS. INC,'
     O                                           51 '.1999'
     O          E            RPHDR2         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              89
     O                       DSDATE        Y    128
     O          E            RPHDR2         2
     O                       Q1PGM               10
     O                       X2TITL              99
     O                                          123 'PAGE:'
     O                       PAGE2         Z    128
     O          E            RPHDR2         2
     O                                           19 'JURISDICTION STATE:'
     O                       XXJST               22
     O                                           47 'ACKNOWLEDGMENT RECEIPT'
     O                                           53 'DATE:'
     O                       ARDATE              64 '  /  /    '
     O          E            RPHDR2         2
     O                                            4 'FUND'
     O                                            8 'YR'
     O                                           17 'CLAIM #'
     O                                           32 'CLAIMANT NAME'
     O                                           65 'DATE INJURY'
     O                                           81 'AGENCY CLAIM #'
     O          E            RPDTL2         1
     O                       X1FND                4
     O                       X1FYR                8
     O                       X1CASE              17
     O*** BEGIN DLT *** 05/20/99 **************************************
     O************************ XXENAM    49
     O                       XXCLNM              49
     O*** END DLT ***** 05/20/99 **************************************
     O                       XXDTIN        Y     64
     O                       A4ACL#              92
     O          E            RPDTL2         1
     O                       A4TXT               78
     OQSYSPRT3  E            RPDST3         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDST3         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST3         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDST3         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST3         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST3         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDST3         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST3         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST3         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDST3         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDST3         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST3         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDST3         2
     O                                           19 'REPORT TITLE:'
     O                       X3TITL              80
     O          E            RPDST3         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDST3         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            RPDST3           60
     O                                           23 '(C) COPYRIGHT PROGRESSI'
     O                                           46 'VE DATA SOLUTIONS. INC,'
     O                                           51 '.1999'
     O          E            RPHDR3         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              89
     O                       DSDATE        Y    128
     O          E            RPHDR3         2
     O                       Q1PGM               10
     O                       X3TITL              99
     O                                          123 'PAGE:'
     O                       PAGE3         Z    128
     O          E            RPHDR3         2
     O                                           19 'JURISDICTION STATE:'
     O                       XXJST               22
     O                                           47 'ACKNOWLEDGMENT RECEIPT'
     O                                           53 'DATE:'
     O                       ARDATE              64 '  /  /    '
     O          E            RPHDR3         2
     O                                            4 'FUND'
     O                                            8 'YR'
     O                                           17 'CLAIM #'
     O                                           32 'CLAIMANT NAME'
     O                                           60 'DT OF ACC'
     O                                           72 'SOC SEC #'
     O                                           89 'AGENCY CLAIM #'
     O          E            RPHDR3         0
     O                                           70 'DATA ELEMENT AND'
     O                                           82 'DESCRIPTION'
     O                                          111 'ERROR CODE AND'
     O                                          123 'DESCRIPTION'
     O          E            RPHDR3         1
     O                                           69 '_______________'
     O                                           93 '________________________'
     O                                          108 '___________'
     O                                          132 '________________________'
     O          E            RPDTL3      1  1
     O                       X1FND         3      4
     O                       X1FYR         3      8
     O                       X1CASE        3     17
     O                       XXCLNM              49
     O                       XXDTIN        Y     61
     O                       A4ACL#             100
     O          E            RPDTL3         1
     O                       A4TXT               78
     O          E            RPERR3         1
     O                       XXDN#               59
     O                       XXDCDS              91
     O                       XXMSG#             100
     O                       XXERDS             132
     O*
     O*   REPORT 4 - REJECTED WITH ERRORS
     OQSYSPRT4  E            RPDST4         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            RPDST4         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST4         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            RPDST4         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST4         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            RPDST4         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            RPDST4         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST4         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            RPDST4         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            RPDST4         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            RPDST4         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            RPDST4         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            RPDST4         2
     O                                           19 'REPORT TITLE:'
     O                       X4TITL              80
     O          E            RPDST4         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            RPDST4         2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            RPDST4           60
     O                                           23 '(C) COPYRIGHT PROGRESSI'
     O                                           46 'VE DATA SOLUTIONS. INC,'
     O                                           51 '.1999'
     O*
     O*   REPORT 4 - REJECTED WITH ERRORS
     O          E            RPHDR4         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              89
     O                       DSDATE        Y    128
     O          E            RPHDR4         2
     O                       Q1PGM               10
     O                       X4TITL              99
     O                                          123 'PAGE:'
     O                       PAGE4         Z    128
     O          E            RPHDR4         2
     O                                           19 'JURISDICTION STATE:'
     O                       XXJST               22
     O                                           47 'ACKNOWLEDGMENT RECEIPT'
     O                                           53 'DATE:'
     O                       ARDATE              64 '  /  /    '
     O                                           90 'ORIGINAL TRANSMISSION '
     O                                           95 'DATE:'
     O                       XXOTDT             106 '  /  /    '
     O          E            RPHDR4         0
     O                                            4 'FUND'
     O                                            8 'YR'
     O                                           17 'CLAIM #'
     O                                           32 'CLAIMANT NAME'
     O                                           60 'DT OF ACC'
     O                                           72 'SOC SEC #'
     O                                           89 'AGENCY CLAIM #'
     O          E            RPHDR4         1
     O                                            4 '____'
     O                                            8 '___'
     O                                           17 '_______'
     O                                           40 '_____________________'
     O                                           49 '_________'
     O                                           61 '__________'
     O                                           72 '_________'
     O                                           89 '______________'
     O          E            RPHDR4         0
     O                                           70 'DATA ELEMENT AND'
     O                                           82 'DESCRIPTION'
     O                                          111 'ERROR CODE AND'
     O                                          123 'DESCRIPTION'
     O          E            RPHDR4         1
     O                                           69 '_______________'
     O                                           93 '________________________'
     O                                          108 '___________'
     O                                          132 '________________________'
     O          E            RPDTL4      1  1
     O                       X1FND         3      3
     O                       X1FYR         3      8
     O                       X1CASE        3     17
     O                       XXCLNM              49
     O                       XXDTIN        Y     61
     O                       CMSS#               72
     O                       A4ACL#             100
     O          E            RPERR4         1
     O                       XXDN#               59
     O                       XXDCDS              91
     O                       XXMSG#             100
     O                       XXERDS             132
