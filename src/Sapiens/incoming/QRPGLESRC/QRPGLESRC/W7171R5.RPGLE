     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7171R - Claims Listing by Selection Criteria
     H*****************************************************************
     H*
     H* REVISIONS
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*
     H*  12/03/98 C:61 PROJECT WC577
     H*                Copied from V2 and modified for V3.
     H*
     H* PLEASE ADD TO TOP
     H*****************************************************************
      *
      *  Claims File
     FWMCML5    IF   E           K DISK
      *
      *  Claimant Demographics File
     FWMCDL     IF   E           K DISK
      *
      *  Employer File
     FWMEML     IF   E           K DISK
      *
      *  Batch Routing File
     FSMORL     IF   E           K DISK
      *
      *  Company File
     FSMCOL     IF   E           K DISK
      *
      *  Fund Detail File
     FWDF1L     IF   E           K DISK
      *
      *  Claim History File
     FWMCHL     IF   E           K DISK
      *
      *  Table File
     FWTABL     IF   E           K DISK
      *
      *  User Parameter File
     FWMUPL     IF   E           K DISK
      *
      *  Output File
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
      *
      *

     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  DATA                   1    225
     D  FFND                   1      3  0
     D  TFND                   4      6  0
     D  FCAS                   7     13  0
     D  TCAS                  14     20  0
     D  FYR                   21     23  0
     D  TYR                   24     26  0
     D  FCLA                  27     31  0
     D  TCLA                  32     36  0
     D  FRGN                  37     39  0
     D  TRGN                  40     42  0
     D  FAWW                  43     49  2
     D  TAWW                  50     56  2
     D  FEMP                  57     65  0
     D  TEMP                  66     74  0
     D  FRAT                  75     81  2
     D  TRAT                  82     88  2
     D  FDIV                  89     93  0
     D  TDIV                  94     98  0
     D  FACC                  99    106  0
     D  TACC                 107    114  0
     D  FLT                  115    115
     D  TLT                  116    116
     D  FREC                 117    124  0
     D  TREC                 125    132  0
     D  FNCC                 133    134
     D  TNCC                 135    136
     D  FENT                 137    144  0
     D  TENT                 145    152  0
     D  FNAT                 153    154
     D  TNAT                 155    156
     D  FCLO                 157    164  0
     D  TCLO                 165    172  0
     D  FPAR                 173    174
     D  TPAR                 175    176
     D  FPAY                 177    187  2
     D  TPAY                 188    198  2
     D  FCAU                 199    200
     D  TCAU                 201    202
     D  FRES                 203    213  2
     D  TRES                 214    224  2
     D  FSTAT                225    225
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXAT15          C                   CONST('*NO AUTHORITY* ')
      *** END ADD ****** 05/25/07 **********************************
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE OVERRIDE FILE
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *   KEY LIST FOR THE CLAIMS FILE
      *
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
      *
     C     CMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     CMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
      *
     C     CMKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXEMPR
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCASE
      *
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
      *
      *   KEY LIST FOR THE HISTORY FILE
      *
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDATE            7 0
      *
      *   KEY LIST FOR THE EMPLOYER FILE
      *
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMPR
     C                   KFLD                    XXDIV
      *
      *   KEY LIST FOR THE FUND DETAIL FILE
      *
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *   KEY LIST FOR THE TABLE FILE
      *
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRES            10
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXYR
     C     *LIKE         DEFINE    CMEMP#        XXEMPR
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMACDT        FACC1
     C     *LIKE         DEFINE    CMACDT        TACC1
     C     *LIKE         DEFINE    CMRCVD        FREC1
     C     *LIKE         DEFINE    CMRCVD        TREC1
     C     *LIKE         DEFINE    CMENTD        FENT1
     C     *LIKE         DEFINE    CMENTD        TENT1
     C     *LIKE         DEFINE    CHEFDT        FCLO1
     C     *LIKE         DEFINE    CHEFDT        TCLO1
      *
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@DATA          225
     C                   PARM                    @@QUE            10
     C                   PARM                    @@CO#             3
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
      *
     C                   MOVE      @@DATA        DATA
      *
      * Save dates to
      * print on routing page.
      *
     C                   Z-ADD     FACC          XFACC             8 0
     C                   Z-ADD     TACC          XTACC             8 0
     C                   Z-ADD     FREC          XFREC             8 0
     C                   Z-ADD     TREC          XTREC             8 0
     C                   Z-ADD     FENT          XFENT             8 0
     C                   Z-ADD     TENT          XTENT             8 0
     C                   Z-ADD     FCLO          XFCLO             8 0
     C                   Z-ADD     TCLO          XTCLO             8 0
      *
      * FIRST CYCLE PROCESSING
      *
     C  N11              EXSR      SR1ST
      *
     C                   MOVE      FFND          XXFND
     C                   MOVE      FEMP          XXEMPR
     C                   MOVE      FDIV          XXDIV
     C                   MOVE      FYR           XXYR
     C                   MOVE      FCAS          XXCASE
      *
      * Indicators 30-32 are used to determine which key list to use
      * when reading the claim file
      *
     C                   MOVEA     '000'         *IN(30)
      *
      * NEED TO FLIP DATES
      *
     C     FACC          IFNE      *ZEROS
     C                   Z-ADD     FACC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FACC1
     C                   ELSE
     C                   Z-ADD     0             FACC1
     C                   ENDIF
      *
     C     TACC          IFNE      99999999
     C                   Z-ADD     TACC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TACC1
     C                   ELSE
     C                   Z-ADD     9999999       TACC1
     C                   ENDIF
      *
     C     FREC          IFNE      *ZEROS
     C                   Z-ADD     FREC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FREC1
     C                   ELSE
     C                   Z-ADD     0             FREC1
     C                   ENDIF
      *
     C     TREC          IFNE      99999999
     C                   Z-ADD     TREC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TREC1
     C                   ELSE
     C                   Z-ADD     9999999       TREC1
     C                   ENDIF
      *
     C     FENT          IFNE      *ZEROS
     C                   Z-ADD     FENT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FENT1
     C                   ELSE
     C                   Z-ADD     0             FENT1
     C                   ENDIF
      *
     C     TENT          IFNE      99999999
     C                   Z-ADD     TENT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TENT1
     C                   ELSE
     C                   Z-ADD     9999999       TENT1
     C                   ENDIF
      *
     C     FCLO          IFNE      *ZEROS
     C                   Z-ADD     FCLO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FCLO1
     C                   ELSE
     C                   Z-ADD     0             FCLO1
     C                   ENDIF
      *
     C     TCLO          IFNE      99999999
     C                   Z-ADD     TCLO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TCLO1
     C                   ELSE
     C                   Z-ADD     9999999       TCLO1
     C                   ENDIF
      *
      *****************************************************************
      *****************************************************************
      *
      *  READ CLAIM FILE
      *
      * Determine if we can use partial keys to get requested data
      *
     C     CMKEY3        SETLL     PCM01
      *
      * If from & to fund years are equal use CMKEY2
      *
     C     FYR           IFEQ      TYR
     C     FFND          ANDEQ     TFND
     C                   MOVEA     '00'          *IN(30)
     C                   MOVE      '1'           *IN32
     C     CMKEY2        READE     PCM01                                  66
     C                   ELSE
      *
      * If from & to fund are equal use CMKEY1
      *
     C     FFND          IFEQ      TFND
     C                   MOVE      '0'           *IN30
     C                   MOVE      '0'           *IN32
     C                   MOVE      '1'           *IN31
     C     CMKEY1        READE     PCM01                                  66
     C                   ELSE
      *
      * If no specifics requested read entire claim file
      *
     C                   MOVEA     '00'          *IN(31)
     C                   MOVE      '1'           *IN30
     C     CMKEY         READE     PCM01                                  66
     C                   END
     C                   END
      *
     C     *IN66         DOWEQ     '0'
      *
      *
      *    DETERMINE IF COMPANY IS WITHIN RANGE SELECTED
      *
     C     CMCO#         IFNE      XXCO#
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF FUND IS WITHIN RANGE SELECTED
      *
     C     CMFND         IFLT      FFND
     C     CMFND         ORGT      TFND
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF YEAR IS WITHIN RANGE SELECTED
      *
     C     CMFYR         IFLT      FYR
     C     CMFYR         ORGT      TYR
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF EMPLOYER IS WITHIN RANGE SELECTED
      *
     C     CMEMP#        IFLT      FEMP
     C     CMEMP#        ORGT      TEMP
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF DIVISION IS WITHIN RANGE SELECTED
      *
     C     CMDIV         IFLT      FDIV
     C     CMDIV         ORGT      TDIV
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF CASE IS WITHIN RANGE SELECTED
      *
     C     CMCASE        IFLT      FCAS
     C     CMCASE        ORGT      TCAS
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF CLASS IS WITHIN RANGE SELECTED
      *
     C     CMCLAS        IFLT      FCLA
     C     CMCLAS        ORGT      TCLA
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF NCC CODE IS WITHIN RANGE SELECTED
      *
     C     CMNCCI        IFLT      FNCC
     C     CMNCCI        ORGT      TNCC
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF NATURE IS WITHIN RANGE SELECTED
      *
     C     CMNATR        IFLT      FNAT
     C     CMNATR        ORGT      TNAT
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF PART IS WITHIN RANGE SELECTED
      *
     C     CMPART        IFLT      FPAR
     C     CMPART        ORGT      TPAR
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF CAUSE IS WITHIN RANGE SELECTED
      *
     C     CMCAUS        IFLT      FCAU
     C     CMCAUS        ORGT      TCAU
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF LOST TIME FLAG IS WITHIN RANGE SELECTED
      *
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      'L'           LFLAG             1
     C                   ELSE
     C                   MOVE      'N'           LFLAG             1
     C                   END
      *
     C     LFLAG         IFLT      FLT
     C     LFLAG         ORGT      TLT
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF AVERAGE WEEKLY WAGE IS WITHIN RANGE SELECTED
      *
     C     CMAWW         IFLT      FAWW
     C     CMAWW         ORGT      TAWW
     C                   GOTO      TAG04
     C                   END
      *
      ****************************************************************
      *
      *  CHAIN TO FUND DETAIL FILE TO PCT AND MAX WK CAP
      *
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFYR         XXYR
     C     F1KEY         CHAIN     WDF1L                              60
     C     *IN60         IFEQ      '1'
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF RATE IS WITHIN RANGE SELECTED
      *
     C     F1REPC        DIV       100           XXREPC            5 4
     C     CMAWW         MULT(H)   XXREPC        RATE              9 2
      *
     C     F1PCP2        IFNE      0
     C     F1EFDT        ANDNE     0
     C     CMACDT        ANDGE     F1EFDT
      *
     C     RATE          IFGT      F1PCP2
     C                   Z-ADD     F1PCP2        RATE
     C                   END
      *
     C                   ELSE
      *
     C     RATE          IFGT      F1PACP
     C                   Z-ADD     F1PACP        RATE
     C                   END
      *
     C                   END
      *
     C     RATE          IFLT      FRAT
     C     RATE          ORGT      TRAT
     C                   GOTO      TAG04
     C                   END
      *
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ACCDTE            6 0
      *
      *    DETERMINE IF ACCIDENT DATE IS WITHIN RANGE SELECTED
      *
     C     CMACDT        IFLT      FACC1
     C     CMACDT        ORGT      TACC1
     C                   GOTO      TAG04
     C                   END
      *
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDTE            6 0
      *
      *    DETERMINE IF RECEIVED DATE IS WITHIN RANGE SELECTED
      *
     C     CMRCVD        IFLT      FREC1
     C     CMRCVD        ORGT      TREC1
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF ENTERED DATE IS WITHIN RANGE SELECTED
      *
     C     CMENTD        IFLT      FENT1
     C     CMENTD        ORGT      TENT1
     C                   GOTO      TAG04
     C                   END
      *
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFYR         XXYR
     C                   MOVE      CMCASE        XXCASE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
      *  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
      *
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60     BOF
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         CLODTE
     C                   GOTO      ARND05
     C                   END
     C     CHCO#         IFNE      CMCO#
     C     CHFND         ORNE      CMFND
     C     CHFYR         ORNE      CMFYR
     C     CHCASE        ORNE      CMCASE
     C                   Z-ADD     *ZERO         CLODTE
     C                   GOTO      ARND05
     C                   END
     C     ARND06        TAG
      *
      *  If claim is closed, effective date is closed date
      *  If claim is open, -no closed date-
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         CLODTE            6 0
     C                   ELSE
     C                   Z-ADD     *ZERO         CLODTE
     C                   END
     C     ARND05        TAG
      *
      *    DETERMINE IF CLOSED DATE IS WITHIN RANGE SELECTED
      *
     C     CHEFDT        IFLT      FCLO1
     C     CHEFDT        ORGT      TCLO1
     C                   GOTO      TAG04
     C                   END
      *
     C                   Z-ADD     *ZERO         PCOMP
     C                   Z-ADD     *ZERO         PMED
     C                   Z-ADD     *ZERO         POTHR
     C                   Z-ADD     *ZERO         RCOMP
     C                   Z-ADD     *ZERO         RMED
     C                   Z-ADD     *ZERO         ROTHR
     C                   Z-ADD     *ZERO         TOTPMT
     C                   Z-ADD     *ZERO         TOTRES
      *
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXRES
      *
      *  GET RESERVE TYPE CODE FILE
      *
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'
     C                   MOVE      '1'           *IN20
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   MOVE      '1'           *IN21
     C                   END
     C                   END
      *
      *   ACCUMULATE PAYMENT AMOUNTS
      *
     C   20              ADD       CMA(R)        PCOMP             9 2
     C   21              ADD       CMA(R)        PMED              9 2
     C  N20
     CANN21              ADD       CMA(R)        POTHR             9 2
      *
      *   ACCUMULATE RESERVE AMOUNTS, PAYMENT AMOUNTS
      *
     C   20              ADD       CMR(R)        RCOMP             9 2
     C   21              ADD       CMR(R)        RMED              9 2
     C  N20
     CANN21              ADD       CMR(R)        ROTHR             9 2
     C   20              ADD       CMA(R)        RCOMP
     C   21              ADD       CMA(R)        RMED
     C  N20
     CANN21              ADD       CMA(R)        ROTHR
      *
     C                   END
      *
     C     PCOMP         ADD       PMED          TOTPMT            9 2
     C                   ADD       POTHR         TOTPMT
     C     RCOMP         ADD       RMED          TOTRES            9 2
     C                   ADD       ROTHR         TOTRES
      *
      *    DETERMINE IF PAYMENT AMOUNT IS WITHIN RANGE SELECTED
      *
     C     TOTPMT        IFLT      FPAY
     C     TOTPMT        ORGT      TPAY
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF RESERVE AMOUNT IS WITHIN RANGE SELECTED
      *
     C     TOTRES        IFLT      FRES
     C     TOTRES        ORGT      TRES
     C                   GOTO      TAG04
     C                   END
      *
      *  CHAIN TO EMPLOYER FILE TO CHECK REGION
      *
     C                   MOVE      CMEMP#        XXEMPR
     C                   MOVE      CMDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   GOTO      TAG04
     C                   END
      *
      *    DETERMINE IF REGION IS WITHIN RANGE SELECTED
      *
     C     EMRGN#        IFLT      FRGN
     C     EMRGN#        ORGT      TRGN
     C                   GOTO      TAG04
     C                   END
      *
      * DETERMINE IF CLAIM STATUS IS EQUAL TO SELECTED STATUS
      *
     C     FSTAT         IFNE      *BLANK
     C     CMSTAT        IFNE      FSTAT
     C                   GOTO      TAG04
     C                   END
     C                   END
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          XXAUTH            1
     C                   MOVE      *BLANKS       XXSS#
     C                   ELSE
     C                   MOVE      *ON           XXAUTH
     C                   MOVE      CMSS#         XXSS#
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C                   MOVEL     XXSS#         SS4               4
     C                   MOVE      '-'           SS4
     C                   MOVE      XXSS#         SS6               6
     C                   MOVEL     SS6           SS2               2
     C                   MOVE      SS6           SS5               5
     C                   MOVEL     '-'           SS5
      *
     C                   ADD       1             LRCASE            5 0
      *
      *  GET CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKEY         CHAIN     PCD01                              60
      *
     C                   MOVEL     CDLSN         CNAME            15
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        CNAME
     C                   END
     C*** BEG ADD ****** 05/25/07 **********************************************
     C     XXAUTH        IFEQ      *OFF
     C                   MOVE      *BLANK        CNAME
     C                   MOVEL     XXAT15        CNAME
     C                   ENDIF
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C     RCOMP         ADD       RMED          TOTALR            9 2
     C                   ADD       ROTHR         TOTALR
      *
     C     PCOMP         ADD       PMED          TOTALP            9 2
     C                   ADD       POTHR         TOTALP
      *
     C                   ADD       RCOMP         LRRCMP           11 2
     C                   ADD       RMED          LRRMED           11 2
     C                   ADD       ROTHR         LRROTH           11 2
     C                   ADD       TOTRES        LRRTOT           11 2
      *
     C                   ADD       PCOMP         LRPCMP           11 2
     C                   ADD       PMED          LRPMED           11 2
     C                   ADD       POTHR         LRPOTH           11 2
     C                   ADD       TOTPMT        LRPTOT           11 2
      *
     C   22              EXCEPT    HEAD
     C                   SETOFF                                       22
     C                   EXCEPT    DETAIL
     C                   SETON                                        71
     C     *INOF         IFEQ      '1'
     C                   SETON                                        22
     C                   END
      *
     C     TAG04         TAG
      *
     C   30CMKEY         READE     PCM01                                  66
     C   31CMKEY1        READE     PCM01                                  66
     C   32CMKEY2        READE     PCM01                                  66
      *
     C                   END
      *
     C     END           TAG
     C                   SETON                                        LR
      *
     CLR 22              EXCEPT    HEAD
     CLR 71              EXCEPT    TOTALS
     CLRN71              EXCEPT    NOREC
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain orKey POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
      *
      *
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
      *
     CSR   SR1ST         BEGSR
      *
      *   SET-UP AND INITIALIZE
      *
     C     @@QUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
      *  INITIALIZE FIELDS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      UPPRDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      *  OPEN PRINT FILE
      *
     C                   OPEN      QSYSPRT
      *
      *   GET THE DISTRIBUTION INFORMATION
      *
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Text in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      *   PRINT THE DISTRIBUTION INFORMATION
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
     C                   END
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *
      *
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *IN22
     C                   SETOFF                                       71
      *
     C                   ENDSR
      *
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      **********
      **********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      *
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
      *
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      *
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
     O          E            ROUTE          2 35
     O                                           56 'SELECTION CRITERIA:'
      *
     O          E            ROUTE          2
     O                                           26 'FROM'
     O                                           38 'TO'
     O                                           84 'FROM'
     O                                          101 'TO'
      *
     O          E            ROUTE          1
     O                                           16 'GROUP  . . . . :'
     O                       FFND          3     26
     O                       TFND          3     38
     O                                           70 'CASE  . . . . :'
     O                       FCAS          3     84
     O                       TCAS          3    101
      *
     O          E            ROUTE          1
     O                                           16 'POLICY YEAR  . :'
     O                       FYR           3     26
     O                       TYR           3     38
     O                                           70 'CLASS . . . . :'
     O                       FCLA          3     84
     O                       TCLA          3    101
      *
     O          E            ROUTE          1
     O                                           16 'REGION . . . . :'
     O                       FRGN          3     26
     O                       TRGN          3     38
     O                                           70 'AWW . . . . . :'
     O                       FAWW          J     85
     O                       TAWW          J    102
      *
     O          E            ROUTE          1
     O                                           16 'EMPLOYER . . . :'
     O                       FEMP          3     26
     O                       TEMP          3     38
     O                                           70 'RATE  . . . . :'
     O                       FRAT          J     85
     O                       TRAT          J    102
      *
     O          E            ROUTE          1
     O                                           16 'DIVISION . . . :'
     O                       FDIV          3     26
     O                       TDIV          3     38
     O                                           70 'ACCIDENT DATE :'
     O                       XFACC               84 '  /  /    '
     O                       XTACC              101 '  /  /    '
      *
     O          E            ROUTE          1
     O                                           16 'LOST TIME (L/N):'
     O                       FLT                 26
     O                       TLT                 38
     O                                           70 'DATE RECEIVED :'
     O                       XFREC               84 '  /  /    '
     O                       XTREC              101 '  /  /    '
      *
     O          E            ROUTE          1
     O                                           16 'NCCI CODE  . . :'
     O                       FNCC                26
     O                       TNCC                38
     O                                           70 'DATE ENTERED  :'
     O                       XFENT               84 '  /  /    '
     O                       XTENT              101 '  /  /    '
      *
     O          E            ROUTE          1
     O                                           16 'NATURE CODE  . :'
     O                       FNAT                26
     O                       TNAT                38
     O                                           70 'DATE CLOSED . :'
     O                       XFCLO               84 '  /  /    '
     O                       XTCLO              101 '  /  /    '
      *
     O          E            ROUTE          1
     O                                           16 'PART CODE  . . :'
     O                       FPAR                26
     O                       TPAR                38
     O                                           70 'PAYMENT AMOUNT:'
     O                       FPAY          J     85
     O                       TPAY          J    102
      *
     O          E            ROUTE          1
     O                                           16 'CAUSE CODE . . :'
     O                       FCAU                26
     O                       TCAU                38
     O                                           70 'TOTAL INCURRED:'
     O                       FRES          J     85
     O                       TRES          J    102
      *
     O          E            ROUTE          1
     O                                           16 'CLAIM STATUS . :'
     O                       FSTAT             +001
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS, INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             120
     O                       DSDATE        Y    198
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             130
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E            HEAD           1
     O                                         +000 'GRP-PYR-CASE'
     O                                         +006 'EMPLOYER'
     O                                         +003 'DIV'
     O                                         +001 'RGN'
     O                                         +002 'CLAIMANT'
     O                                         +009 'SOC SEC #'
     O                                         +003 'JB CLS'
     O                                         +010 'AWW'
     O                                         +011 'RATE'
     O                                         +004 'ACC DTE'
     O                                         +002 'RCV DATE'
     O                                         +002 'CLS DTE'
     O                                         +003 'NC NT PT CS'
     O                                         +003 'STATUS'
      *  ZZZ
     O          E            HEAD           2
     O                                          101 'COMPENSATION'
     O                                         +011 'MEDICAL'
     O                                         +013 'OTHER'
     O                                         +011 'TOTAL'
      *
     O          E            DETAIL         1
     O                       CMFND         X   +000
     O                                         +000 '-'
     O                       CMFYR         X   +000
     O                                         +000 '-'
     O                       CMCASE            +000
     O                       CMEMP#        Z   +002
     O                       CMDIV         Z   +001
     O                       EMRGN#        Z   +001
     O                       CNAME             +002
     O                       SS4               +002
     O                       SS2               +000
     O                       SS5               +000
     O                       CMCLAS        Z   +002
     O                       CMAWW         J   +004
     O                       RATE          J   +002
     O                       ACCDTE            +002 '  /  /  '
     O                       RECDTE            +002 '  /  /  '
     O                       CLODTE            +002 '  /  /  '
     O                       CMNCCI            +002
     O                       CMNATR            +001
     O                       CMPART            +001
     O                       CMCAUS            +001
     O                       CMSTAT            +005
      * ZZZ
     O          E            DETAIL         1
     O                                           84 'INCURRED:'
     O                       RCOMP         KB  +005
     O                       RMED          KB  +005
     O                       ROTHR         KB  +005
     O                       TOTALR        JB  +003
      *
     O          E            DETAIL         3
     O                                           84 'PAYMENTS:'
     O                       PCOMP         KB  +005
     O                       PMED          KB  +005
     O                       POTHR         KB  +005
     O                       TOTALP        JB  +003
      *
     O          E    LR      TOTALS      2  1
     O                                           38 'TOTAL CLAIM COUNT:'
     O                       LRCASE        1     44
     O                                           84 'TOTAL INCURRED:'
     O                       LRRCMP        JB  +003
     O                       LRRMED        JB  +003
     O                       LRROTH        JB  +003
     O                       LRRTOT        JB  +001
      *
     O          E    LR      TOTALS         2
     O                                           84 'TOTAL PAYMENTS:'
     O                       LRPCMP        JB  +003
     O                       LRPMED        JB  +003
     O                       LRPOTH        JB  +003
     O                       LRPTOT        JB  +001
      *
     O          E            NOREC       2  1
     O                                         +010 '**** NO RECORDS MEET'
     O                                         +001 'THE SELECTED CRITERIA'
     O                                         +001 '****'
      *
