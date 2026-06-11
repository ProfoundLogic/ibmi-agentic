     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H**   PROGRAM: N0724R                                           **
     H**   DESCRIPTION: INVOICE DETAIL SEARCH                        **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  01/07/99 C:94  PROJECT: MC152                               **
     H*                 CHANGED THE LENGTH OF INV# FROM 5 TO 9       **
     H*                                                              **
     H*  08/18/93 C:20   REQ/PROJ: ARMNT                             **
     H*                  1. CHANGED KEY FOR NMARL8 TO BE:            **
     H*                     CO/SUB/INV/FND/FYR/CASE/EOB  FROM        **
     H*                     CO/INV/SUB/FND/FYR/CASE/EOB              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATOR USAGE:                                            **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FN0724F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FNMARL8    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK

     D FL              S              1    DIM(14)                              POSITION FLAG
     D*** BEG CHG *** 01/07/99 ***************************************
     D**********          IN         14  5 0             INVOICE #
     D*** END CHG *** 01/07/99 ***************************************
     D*** BEG CHG *** 01/07/99 ***************************************
     D**********          NM         14 15               CLAIMANT NAME
     D*** END CHG *** 01/07/99 ***************************************
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*** BEG CHG *** 01/07/99 ***************************************
     D KEY1            S              9  0 DIM(2)                               INV #
     D**********          KEY1        2  5 0             INV #
     D*** END CHG *** 01/07/99 ***************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXFNUM                 1     14
     D  XXFUND                 1      3
     D  XXSEP1                 4      4
     D  XXYEAR                 5      6
     D  XXSEP2                 7      7
     D  XXCAS#                 8     14
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEG CHG *** 01/07/99 ***************************************
     D                 DS
     D  IN                     1    126  0
     D                                     DIM(14)                              INVOICE #
     D  SCIN01                 1      9  0
     D  SCIN02                10     18  0
     D  SCIN03                19     27  0
     D  SCIN04                28     36  0
     D  SCIN05                37     45  0
     D  SCIN06                46     54  0
     D  SCIN07                55     63  0
     D  SCIN08                64     72  0
     D  SCIN09                73     81  0
     D  SCIN10                82     90  0
     D  SCIN11                91     99  0
     D  SCIN12               100    108  0
     D  SCIN13               109    117  0
     D  SCIN14               118    126  0
     D**********  DS
     D**********                              1  70 IN
     D**********                              1   50SCIN01
     D**********                              6  100SCIN02
     D**********                             11  150SCIN03
     D**********                             16  200SCIN04
     D**********                             21  250SCIN05
     D**********                             26  300SCIN06
     D**********                             31  350SCIN07
     D**********                             36  400SCIN08
     D**********                             41  450SCIN09
     D**********                             46  500SCIN10
     D**********                             51  550SCIN11
     D**********                             56  600SCIN12
     D**********                             61  650SCIN13
     D**********                             66  700SCIN14
     D*** END CHG *** XX/XX/XX ***************************************
     D                 DS
     D  EO                     1    126  0
     D                                     DIM(14)                              EOB #
     D  SCEO01                 1      9  0
     D  SCEO02                10     18  0
     D  SCEO03                19     27  0
     D  SCEO04                28     36  0
     D  SCEO05                37     45  0
     D  SCEO06                46     54  0
     D  SCEO07                55     63  0
     D  SCEO08                64     72  0
     D  SCEO09                73     81  0
     D  SCEO10                82     90  0
     D  SCEO11                91     99  0
     D  SCEO12               100    108  0
     D  SCEO13               109    117  0
     D  SCEO14               118    126  0
     D                 DS
     D  SY                     1     14
     D                                     DIM(14)                              SYSTEM ID
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D  SCSY03                 3      3
     D  SCSY04                 4      4
     D  SCSY05                 5      5
     D  SCSY06                 6      6
     D  SCSY07                 7      7
     D  SCSY08                 8      8
     D  SCSY09                 9      9
     D  SCSY10                10     10
     D  SCSY11                11     11
     D  SCSY12                12     12
     D  SCSY13                13     13
     D  SCSY14                14     14
     D                 DS
     D  DT                     1    112  0
     D                                     DIM(14)                              TRANS DATE
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
     D  SCDT12               089    096  0
     D  SCDT13               097    104  0
     D  SCDT14               105    112  0
     D                 DS
     D  CD                     1     28
     D                                     DIM(14)                              TRANS CODE
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
     D                 DS
     D  FN                     1    196
     D                                     DIM(14)                              FILE NUMBER
     D  SCFN01                 1     14
     D  SCFN02                15     28
     D  SCFN03                29     42
     D  SCFN04                43     56
     D  SCFN05                57     70
     D  SCFN06                71     84
     D  SCFN07                85     98
     D  SCFN08                99    112
     D  SCFN09               113    126
     D  SCFN10               127    140
     D  SCFN11               141    154
     D  SCFN12               155    168
     D  SCFN13               169    182
     D  SCFN14               183    196
     D*** BEG CHG *** 01/07/99 ***************************************
     D                 DS
     D  NM                     1    168
     D                                     DIM(14)                              CLAIMANT NAME
     D  SCNM01                 1     12
     D  SCNM02                13     24
     D  SCNM03                25     36
     D  SCNM04                37     48
     D  SCNM05                49     60
     D  SCNM06                61     72
     D  SCNM07                73     84
     D  SCNM08                85     96
     D  SCNM09                97    108
     D  SCNM10               109    120
     D  SCNM11               121    132
     D  SCNM12               133    144
     D  SCNM13               145    156
     D  SCNM14               157    168
     D**********  DS
     D**********                              1 210 NM
     D**********                              1  15 SCNM01
     D**********                             16  30 SCNM02
     D**********                             31  45 SCNM03
     D**********                             46  60 SCNM04
     D**********                             61  75 SCNM05
     D**********                             76  90 SCNM06
     D**********                             91 105 SCNM07
     D**********                            106 120 SCNM08
     D**********                            121 135 SCNM09
     D**********                            136 150 SCNM10
     D**********                            151 165 SCNM11
     D**********                            166 180 SCNM12
     D**********                            181 195 SCNM13
     D**********                            196 210 SCNM14
     D*** END CHG *** 01/07/99 ***************************************
     D                 DS
     D  AM                     1    126  2
     D                                     DIM(14)                              AMOUNT
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
     D  SCAM12               100    108  2
     D  SCAM13               109    117  2
     D  SCAM14               118    126  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST #1 FOR THE A/R FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#                         ADD 08/18/93
     C*** BEG CHG *** XX/XX/XX ***************************************
     C                   KFLD                    XXINV#            9 0
     C**********           KFLD           XXINV#  50
     C*** END CHG *** XX/XX/XX ***************************************
     C*
     C*  KEY LIST #2 FOR THE A/R FILE
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C*
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** BEG CHG *** XX/XX/XX ***************************************
     C                   PARM                    QQINV#            9
     C**********           PARM           QQINV#  5
     C*** END CHG *** XX/XX/XX ***************************************
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFND         SCFND             3 0
     C                   MOVE      QQFYR         SCFYR             3 0
     C                   MOVE      QQCASE        SCCASE            7 0
     C                   MOVE      QQINV#        SCINV#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         XXSUB#
     C                   ELSE
     C                   Z-ADD     UPSUB#        XXSUB#            5 0
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C                   MOVEL     UPSUB#        SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C* GET GROUP NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        SCFNNM
     C                   ELSE
     C                   MOVEL     FNNAME        SCFNNM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     XXINV#        COMP      *ZERO                                  21
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
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
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *ZERO         QQINV#
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = PREVIOUS
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
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
     C                   Z-ADD     KEY1(2)       XXINV#
     C*** BEGIN OF DELETE *** 7/25/89 *** CONNIE/I0008
     C******************** Z-ADDKEY2,2    XKFUND  30
     C******************** Z-ADDKEY3,2    XKYEAR  30
     C******************** Z-ADDKEY4,2    XKCAS#  70
     C******************** Z-ADDKEY5,2    XXSUB#
     C*** END OF DELETE *** 7/25/89 *** CONNIE/I0008
     C     ARKEY         CHAIN     PAR01                              60
     C*
     C*** SAVE KEY FIELDS FOR CHAIN TO CLAIM FILE - CMKEY ...
     C*
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ARSUB#        XXSUB#
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARCASE        XXCASE
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C                   END
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEY1(1)       XXINV#
     C*** BEGIN OF DELETE *** 7/25/89 *** CONNIE/I0008
     C******************** Z-ADDKEY2,1    XKFUND
     C******************** Z-ADDKEY3,1    XKYEAR
     C******************** Z-ADDKEY4,1    XKCAS#
     C******************** Z-ADDKEY5,1    XXSUB#
     C*** END OF DELETE *** 7/25/89 *** CONNIE/I0008
     C     ARKEY         CHAIN     PAR01                              60
     C*
     C*** SAVE KEY FIELDS FOR CHAIN TO CLAIM FILE - CMKEY ...
     C*
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ARSUB#        XXSUB#
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARCASE        XXCASE
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C                   END
     C*
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCINV#        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY1
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCINV#        XXINV#
     C                   Z-ADD     *ZEROS        SCINV#
     C     ARKEY         SETLL     PAR01
     C     ARKEY2        READE     PAR01                                  60
     C*
     C*** SAVE KEY FIELDS FOR CHAIN TO CLAIM FILE - CMKEY ...
     C*
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ARSUB#        XXSUB#
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARCASE        XXCASE
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C                   END
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  RETURN TO CALLING PROGRAM
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      FN(I1)        XXFNUM
     C                   MOVE      XXFUND        QQFND
     C                   MOVE      XXYEAR        QQFYR
     C                   MOVE      XXCAS#        QQCASE
     C                   MOVE      IN(I1)        QQINV#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     *BLANK        QQINV#
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C*  CLEAR SCREEN ARRAYS       ** SUBROUTINE **              C L R   *
     C********************************************************************
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       IN
     C                   MOVE      *BLANKS       EO
     C                   MOVE      *BLANKS       SY
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       FN
     C                   MOVE      *BLANKS       AM
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
     C********************************************************************
     C*  FILL SCREEN ARRAYS      ** SUBROUTINE **                F I L L *
     C********************************************************************
     C     FILL          BEGSR
     C                   Z-ADD     ARINV#        IN(I1)
     C                   Z-ADD     AREOB#        EO(I1)
     C                   MOVE      ARSYS         SY(I1)
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   MOVE      ARTRCD        CD(I1)
     C                   MOVE      XXCLNM        NM(I1)
     C                   MOVE      XXFNUM        FN(I1)
     C                   Z-ADD     ARAMT         AM(I1)
     C*
     C     ENDFIL        ENDSR
     C********************************************************************
     C*  LOAD FORWARD           ** SUBROUTINE **                 L O A D *
     C********************************************************************
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
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCLNM           15
     C                   ELSE                                                   ELSE - CDKEY
     C                   MOVE      *BLANKS       XXCLNM
     C                   END                                                    END - CDKEY
     C*
     C*** ADD NEXT 2 LINES *** 7/25/89 *** CONNIE/I0008
     C                   ELSE                                                   ELSE - CMKEY
     C                   MOVE      *BLANKS       XXCLNM
     C                   END                                                    END - CMKEY
     C*
     C                   MOVE      *BLANKS       XXFNUM
     C                   MOVE      ARFND         XXFUND
     C                   MOVE      '-'           XXSEP1
     C                   MOVE      ARFYR         XXYEAR
     C                   MOVE      '-'           XXSEP2
     C                   MOVE      ARCASE        XXCAS#
     C                   EXSR      FILL
     C     ARKEY2        READE     PAR01                                  60
     C*
     C*** SAVE KEY FIELDS FOR CHAIN TO CLAIM FILE - CMKEY ...
     C*
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARCASE        XXCASE
     C                   Z-ADD     ARSUB#        XXSUB#
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C                   END
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C*
     C                   MOVE      *BLANKS       XXFNUM
     C                   MOVE      FN(1)         XXFNUM
     C                   Z-ADD     IN(1)         KEY1(1)
     C*** BEGIN OF DELETE *** 7/25/89 *** CONNIE/I0008
     C******************** MOVE XXFUND    KEY2,1
     C******************** MOVE XXYEAR    KEY3,1
     C******************** MOVE XXCAS#    KEY4,1
     C******************** MOVE XXSUB#    KEY5,1
     C*** END OF DELETE *** 7/25/89 *** CONNIE/I0008
     C*
     C                   MOVE      *BLANKS       XXFNUM
     C                   MOVE      FN(14)        XXFNUM
     C                   Z-ADD     IN(14)        KEY1(2)
     C*** BEGIN OF DELETE *** 7/25/89 *** CONNIE/I0008
     C******************** MOVE XXFUND    KEY2,2
     C******************** MOVE XXYEAR    KEY3,2
     C******************** MOVE XXCAS#    KEY4,2
     C******************** MOVE XXSUB#    KEY5,2
     C*** END OF DELETE *** 7/25/89 *** CONNIE/I0008
     C                   ENDSR
     C********************************************************************
     C*  LOAD BACKWARD          ** SUBROUTINE **               L O A D P *
     C********************************************************************
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
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
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCLNM
     C                   ELSE                                                   ELSE - CDKEY
     C                   MOVE      *BLANKS       XXCLNM
     C                   END                                                    END - CDKEY
     C*
     C*** ADD NEXT 2 LINES *** 7/25/89 *** CONNIE/I0008
     C                   ELSE                                                   ELSE - CMKEY
     C                   MOVE      *BLANKS       XXCLNM
     C                   END                                                    END - CMKEY
     C*
     C                   MOVE      *BLANKS       XXFNUM
     C                   MOVE      ARFND         XXFUND
     C                   MOVE      '-'           XXSEP1
     C                   MOVE      ARFYR         XXYEAR
     C                   MOVE      '-'           XXSEP2
     C                   MOVE      ARCASE        XXCAS#
     C                   EXSR      FILL
     C                   READP     PAR01                                  60
     C*
     C*** SAVE KEY FIELDS FOR CHAIN TO CLAIM FILE - CMKEY ...
     C*
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARCASE        XXCASE
     C                   Z-ADD     ARSUB#        XXSUB#
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C                   END
     C*
     C     UPCO#         IFNE      ARCO#
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C*
     C                   MOVE      *BLANKS       XXFNUM
     C                   MOVE      FN(1)         XXFNUM
     C                   Z-ADD     IN(1)         KEY1(1)
     C*** BEGIN OF DELETE *** 7/25/89 *** CONNIE/I0008
     C******************** MOVE XXFUND    KEY2,1
     C******************** MOVE XXYEAR    KEY3,1
     C******************** MOVE XXCAS#    KEY4,1
     C******************** MOVE XXSUB#    KEY5,1
     C*** END OF DELETE *** 7/25/89 *** CONNIE/I0008
     C*
     C                   MOVE      *BLANKS       XXFNUM
     C                   MOVE      FN(14)        XXFNUM
     C                   Z-ADD     IN(14)        KEY1(2)
     C*** BEGIN OF DELETE *** 7/25/89 *** CONNIE/I0008
     C******************** MOVE XXFUND    KEY2,2
     C******************** MOVE XXYEAR    KEY3,2
     C******************** MOVE XXCAS#    KEY4,2
     C******************** MOVE XXSUB#    KEY5,2
     C*** END OF DELETE *** 7/25/89 *** CONNIE/I0008
     C                   ENDSR
     C********************************************************************
     C*  LOAD BEGINNING         ** SUBROUTINE **               L O A D B *
     C********************************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXINV#
     C     ARKEY         SETLL     PAR01
     C     ARKEY2        READE     PAR01                                  60
     C*
     C*** SAVE KEY FIELDS FOR CHAIN TO CLAIM FILE - CMKEY ...
     C*
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ARSUB#        XXSUB#
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARCASE        XXCASE
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C                   END
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCLNM
     C                   ELSE                                                   ELSE - CDKEY
     C                   MOVE      *BLANKS       XXCLNM
     C                   END                                                    END - CDKEY
     C*
     C*** ADD NEXT 2 LINES *** 7/25/89 *** CONNIE/I0008
     C                   ELSE                                                   ELSE - CMKEY
     C                   MOVE      *BLANKS       XXCLNM
     C                   END                                                    END - CMKEY
     C*
     C                   MOVE      *BLANKS       XXFNUM
     C                   MOVE      ARFND         XXFUND
     C                   MOVE      '-'           XXSEP1
     C                   MOVE      ARFYR         XXYEAR
     C                   MOVE      '-'           XXSEP2
     C                   MOVE      ARCASE        XXCAS#
     C                   EXSR      FILL
     C     ARKEY2        READE     PAR01                                  60
     C*
     C*** SAVE KEY FIELDS FOR CHAIN TO CLAIM FILE - CMKEY ...
     C*
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ARSUB#        XXSUB#
     C                   Z-ADD     ARFND         XXFND
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARCASE        XXCASE
     C*** ADD NEXT LINE *** 7/25/89 *** CONNIE/I0008
     C                   END
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
