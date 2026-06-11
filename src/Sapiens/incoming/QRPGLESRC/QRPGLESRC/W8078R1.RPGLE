     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8078R1 -  EDITS FOR EDI ACKNOWLEDGMENT PROCESSING
     H*
     H*****************************************************************
     H*
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 12/11/19 C:53  Support: 85868                                                      *
     H*                Use Optacomp process for CLTID 153                                  *
     H*                                                                                    *
     H* 03/31/14 C:53  Project: 114WBF                                                     *
     H*                Change key field sequence.                                          *
     H*                                                                                    *
     H* 11/27/13 C:180 SUPPORT OP114 84300  - PROJECT : 147WBF        *
     H*                Identify OPTACOMP Admin Claim Number           *
     H*                as part of 147WBF.
     H*
     H* 05/21/12 C:180 SUPPORT 79960
     H*                Further qualify Accepted With Errors Report
     H*                for codes 53 and 60 to specify FL & CA only.
     H*
     H*
     H* 03/08/11 C:180 SUPPORT 76142
     H*                Report error code 060 from state of CA
     H*                and rec status of 'TE' in the Accepted with
     H*                errors report.
     H*
     H* 04/05/10 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                  PROJECT RE-JAD , RE-DESIGNED                 *
     H*                                                               *
     H* 02/26/10 C:180 SUPPORT SEQ71782                               *
     H*                - Donot update with an 'RE' when error code    *
     H*                  is either 053 or 060.                        *
     H*                                                               *
     H* 02/01/10 C:235 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 10/09/09 C:235 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*
     H*                                                               *
     H* 05/29/09 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
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
     FWXA1L     UF   E           K DISK
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
     D*================================================================
     D*** BEGIN ADD *** 11/27/13 **************************************
     D XXPDS#          S                   LIKE(CMPDS#) INZ
     D XXFND           S                   LIKE(CMFND) INZ
     D*** END   ADD *** 11/27/13 **************************************
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
     D  A1ACLC                 1     25
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
     D*** Error Numbers
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
     D*
     D*** BEG ADD ***** 05/29/09 **************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D*** END ADD ***** 05/29/09 **************************************
     D*
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
     C*          WXA1L - Acknowledgment Work File
     C*
     C     A1KY          KLIST
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
     C                   KFLD                    A1JST
     C                   KFLD                    A1TSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM            6 0
     C*
     C*       WMM1L - Extract Management File
     C*
     C     M1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    A1JST
     C                   KFLD                    A1TSET
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
     C     *LIKE         DEFINE    A1JST         XXJST
     C     *LIKE         DEFINE    A1TSET        XXSET
     C     *LIKE         DEFINE    A1MTC         XXMTC
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
     C*********  *LIKE     DEFN LGMTC     SVMTC#
     C*** END ADD *** 02/01/10 ****************************
     C*
     C*** END DEL *** 04/05/10 ****************************
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
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
     C     A1KY          CHAIN     PHA01                              60
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
     C     A1KY          CHAIN     PA101                              60
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
     C                   MOVEL     A1ACLC        X2ACLC
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
      *
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
     C                   MOVE      '1'           A1ERRF
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
     C                   MOVE      '2'           A1ERRF
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
     C                   MOVE      *BLANKS       A1ERRF
     C                   MOVE      'C'           A1STAT
     C                   EXSR      SRPDTL
     C*
     C*          Update the Log and Claim Records that were
     C*          retrieved.
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEG ADD *** 07/24/06 ****************************
     C     A1TAK1        IFNE      'AR1'
     C*** END ADD *** 07/24/06 ****************************
     C                   MOVEL     SRDTO         LGARCV
     C                   MOVEL     SRDTO         LGAENT
     C                   MOVEL     A1ACKC        LGSTS
     C*** BEG ADD *** 07/24/06 ****************************
     C                   ELSE
     C*** BEG ADD *** 03/16/07 ****************************
     C     CLTID         IFEQ      '132'
     C     LGMTC         ANDEQ     'AN'
     C                   ELSE
     C**** BEGIN ADD **** 02/26/10 ************************
     C                   SELECT
     C     A1TAK1        WHENEQ    'AR1'
     C     A1JST         ANDEQ     'FL'
     C     A1TSET        ANDEQ     '148'
     C     A1ACKC        ANDEQ     'TE'
     C     A1#ERR        ANDEQ     '01'
     C     A1ER01        ANDEQ     '053'
      *
     C     A1TAK1        WHENEQ    'AR1'
     C     A1JST         ANDEQ     'FL'
     C     A1TSET        ANDEQ     '148'
     C     A1ACKC        ANDEQ     'TE'
     C     A1#ERR        ANDEQ     '01'
     C     A1ER01        ANDEQ     '060'
      *
     C**** BEGIN ADD **** 03/08/11 ************************
     C     A1TAK1        WHENEQ    'AR1'
     C     A1JST         ANDEQ     'CA'
     C     A1TSET        ANDEQ     'A49'
     C     A1ACKC        ANDEQ     'TE'
     C     A1#ERR        ANDEQ     '01'
     C     A1ER01        ANDEQ     '060'
     C**** END   ADD **** 03/08/11 ************************
     C                   OTHER
     C**** END ADD  ***** 02/26/10 ************************
     C*** END ADD *** 03/16/07 ****************************
     C                   MOVEL     'RE'          LGSTS
     C                   Z-ADD     *ZEROS        LGSENT
     C*** BEG ADD *** 03/16/07 ****************************
     C**** BEGIN ADD***** 02/26/10 ************************
     C                   ENDSL
     C**** END ADD  ***** 02/26/10 ************************
     C                   ENDIF
     C*** END ADD *** 03/16/07 ****************************
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C                   MOVE      QQDSQG        LGDSQG
     C*** BEG ADD ***** 05/29/09 **************************************
     C*** BEG DEL *** 04/05/10 ****************************
     C********   CMCLEX    CHAINPUP01                60
     C********   *IN60     IFEQ *OFF
     C********             MOVE UPUSR     Q#USER 10 P
     C********             ENDIF
     C********
     C*** BEG ADD ***** 02/01/10 **************************************
     C********   LGTSET    IFNE 'A49'
     C*** END ADD ***** 02/01/10 **************************************
     C********   LGSTS     IFNE SVSTS#
     C*** BEG ADD ***** 02/01/10 **************************************
     C******     LGMTC     ORNE SVMTC#
     C*** END ADD ***** 02/01/10 **************************************
     C********   UPUSR     ANDNE*BLANKS
     C*** END DEL *** 04/05/10 ****************************
     C*** BEG ADD *** 04/05/10 ****************************
     C     LGSTS         IFNE      SVSTS#
     C*** END ADD *** 04/05/10 ****************************
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C*** BEG DEL *** 04/05/10 ****************************
     C*** BEG ADD ***** 02/01/10 **************************************
     C**********           ENDIF
     C********** LGTSET    IFEQ 'A49'
     C********** LGSTS     IFNE SVSTS#
     C********** LGMTC     ORNE SVMTC#
     C********** UPUSR     ANDNE*BLANKS
     C********** LGSTS     IFEQ 'EI'
     C********** LGSTS     OREQ 'EC'
     C**********           EXSR DARTS
     C**********           ENDIF
     C**********           ENDIF
     C**********           ENDIF
     C*** END ADD ***** 02/01/10 **************************************
     C*** END DEL *** 04/05/10 ****************************
     C*** END ADD ***** 05/29/09 **************************************
     C                   UPDATE    PLG01
     C*
     C*** Begin add *** 03/15/05 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVEL     A1ACL#        CMASCL
     C                   ELSE
     C*** End add ***** 03/15/05 **************************************
     C                   MOVEL     A1ACL#        CMACL#
     C*** Begin add *** 03/15/05 **************************************
     C                   ENDIF
     C*** End add ***** 03/15/05 **************************************
     C                   UPDATE    PCM01
     C*
     C*
     C*          Generate the Diary Remark, IF THERE WERE ERRORS.
     C*
     C     A1ACKC        IFNE      'TA'
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
     C     A1KY          READE     PA101                                  60
     C                   ENDDO
     C*
     C*** BEG ADD *** 07/24/06 ****************************
     C     A1TAK1        IFNE      'AR1'
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
     CSR   LGTSET        IFEQ      A1TSET
     CSR   LGMTC         ANDEQ     A1MTC
     CSR   LGSTS         ANDEQ     'SN'
     CSR                 MOVE      *ON           XXLGRF
     C*** BEG ADD ***** 05/29/09 **************************************
     C                   MOVEL     LGSTS         SVSTS#
     C*** END ADD ***** 05/29/09 **************************************
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
     C*** BEGIN ADD ****** 02/26/10 ******************************
     C                   MOVE      *OFF          X3FILE            1
     C*** END ADD   ****** 02/26/10 ******************************
     C*** BEG ADD *** 07/24/06 ****************************
     C     A1TAK1        IFNE      'AR1'
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
     C*** BEGIN ADD ****** 02/26/10 ******************************
     C                   MOVE      *ON           X3FILE
     C*** END ADD   ****** 02/26/10 ******************************
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
     C     A1TAK1        IFEQ      'AR1'
     C                   MOVEL     XATITL        X4TITL
     C                   ENDIF
     C*** END ADD *** 07/24/06 ****************************
     C*
     CSR                 EXCEPT    RPHDR4
     CSR                 MOVE      *OFF          *INOD
     C*                                                                   SRINIT
     C**** BEGIN ADD **** 02/26/10 ************************
     C     A1TAK1        IFEQ      'AR1'
     C     A1JST         ANDEQ     'FL'
     C     X3FILE        ANDEQ     *OFF
     C**** BEGIN ADD **** 03/08/11 ************************
     C     A1TAK1        OREQ      'AR1'
     C     A1JST         ANDEQ     'CA'
     C     X3FILE        ANDEQ     *OFF
     C**** BEGIN ADD **** 03/08/11 ************************
     C                   OPEN      QSYSPRT3
     C                   MOVE      '3'           XXFILE                                        SRINI
     C                   MOVE      *ON           X3FILE
     C     ORKY          CHAIN     POR01                              61                       SRINI
     C     *IN61         IFEQ      '0'                                                         SRINI
     C                   MOVEL     ORTITL        X3TITL                                        SRINI
     C                   EXCEPT    RPDST3                                                      SRINI
     C                   ENDIF                                                                 SRINI
     C                   EXCEPT    RPHDR3
     C                   MOVE      *OFF          *INOC
     C                   ENDIF
     C**** END ADD ****** 02/26/10 ************************
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
     CSR   A1ERRF        WHENEQ    '1'
     CSR   A1ERRF        OREQ      '2'
     CSR                 MOVE      *BLANKS       XXERDS
     CSR                 MOVEL     XCERR1        XXERDS
     CSR   A1ERRF        IFEQ      '2'
     CSR                 MOVE      *BLANKS       XXERDS
     CSR                 MOVEL     XCERR2        XXERDS
     CSR                 ENDIF
     C*** BEG ADD *** 07/24/06 ****************************
     C     A1TAK1        IFNE      'AR1'
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
     CSR   A1ACKC        WHENEQ    'TA'
     C*** BEG ADD *** 07/24/06 ****************************
     C     A1TAK1        ANDNE     'AR1'
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
     CSR   A1ACKC        WHENEQ    'TE'
     C*** BEG ADD *** 07/24/06 ****************************
     C     A1TAK1        ANDNE     'AR1'
     C*** END ADD *** 07/24/06 ****************************
     C**** BEGIN ADD *****  02/26/10 **********************
     CSR   A1ACKC        OREQ      'TE'
     CSR   A1TAK1        ANDEQ     'AR1'
     CSR   A1#ERR        ANDEQ     '01'
     CSR   A1ER01        ANDEQ     '053'
     ***** BEGIN ADD ***** 05/21/12 ************************
     CSR   A1JST         ANDEQ     'FL'
     C*
     CSR   A1ACKC        OREQ      'TE'
     CSR   A1TAK1        ANDEQ     'AR1'
     CSR   A1#ERR        ANDEQ     '01'
     CSR   A1ER01        ANDEQ     '060'
     CSR   A1JST         ANDEQ     'FL'
     ***** END   ADD ***** 05/21/12 ************************
     C*
     CSR   A1ACKC        OREQ      'TE'
     CSR   A1TAK1        ANDEQ     'AR1'
     CSR   A1#ERR        ANDEQ     '01'
     CSR   A1ER01        ANDEQ     '060'
     C**** END ADD *******  02/26/10 **********************
     ***** BEGIN ADD ***** 05/21/12 ************************
     CSR   A1JST         ANDEQ     'CA'
     ***** END   ADD ***** 05/21/12 ************************
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
     CSR                 MOVE      A1TSET        XXSET
     CSR                 MOVE      *BLANKS       XXMTC
     CSR                 MOVE      E#(P)         XXMSG#
     CSR                 MOVE      *ALL'*'       XXERDS
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
     C********** EMMTC     IFEQ A1MTC
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
     CSR   A1ACKC        WHENEQ    'TR'
     C*** BEG ADD *** 07/24/06 ****************************
     CSR   A1ACKC        OREQ      'TE'
     C     A1TAK1        ANDEQ     'AR1'
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
     CSR                 MOVE      A1TSET        XXSET
     C                   MOVE      D#(P)         XXDN#
     CSR                 MOVE      *BLANKS       XXMTC
     CSR                 MOVE      E#(P)         XXMSG#
     C                   MOVE      *ALL'*'       XXERDS
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
     C********** EMMTC     IFEQ A1MTC
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
     C*************        MOVE IXCECR    Q#RMCD  5
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
     C*********  IXCTAR    ANDNE*BLANKS
     C**************       MOVE IXCTAR    Q#RMCD
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
     C                   MOVEL(P)  A1ACLC        XXPDS#
     C     CMKY17        CHAIN     PCM17

     C                   IF        %FOUND
     C                   MOVE      CMFND         XXFND
     C     FNKY          CHAIN     PFN01

     C*** Begin Chg *** 12/11/19 **************************************
     C*****************  IF        %FOUND and FNGRPASC = 'OPTACOMP'
     C                   IF        (%Found and FNgrpAsc = 'OPTACOMP')
     C                             Or cltID = '152'
     C*** End   Chg *** 12/11/19 **************************************
     C                   MOVEL(P)  CMACLC        X2ACLC
     C                   TESTN                   X2CO#                9999
     C   99              TESTN                   X2FND                9999
     C   99              TESTN                   X2FYR                9999
     C   99              TESTN                   X2CASE               9999
     C   99              MOVEL(P)  CMACLC        A1ACLC
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           RPDST1
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1999'
     O          E            RPDST1         1 59
     O                       #BRND1
     O          E            RPDST1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
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
     O                       A1ACL#              46
     O                       XXERDS              94
     O*** BEGIN ADD *** 08/03/00 ****************************************
     O          E            RPDT1A         1
     O                       X2ACLC              17
     O                       A1ACL#              46
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           RPDST2
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1999'
     O          E            RPDST2         1 59
     O                       #BRND1
     O          E            RPDST2         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
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
     O                       A1ACL#              92
     O          E            RPDTL2         1
     O                       A1TXT               78
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           RPDST3
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1999'
     O          E            RPDST3         1 59
     O                       #BRND1
     O          E            RPDST3         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
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
     O                       A1ACL#             100
     O          E            RPDTL3         1
     O                       A1TXT               78
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           RPDST4
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1999'
     O          E            RPDST4         1 59
     O                       #BRND1
     O          E            RPDST4         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
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
     O                       A1ACL#             100
     O          E            RPERR4         1
     O                       XXDN#               59
     O                       XXDCDS              91
     O                       XXMSG#             100
     O                       XXERDS             132
