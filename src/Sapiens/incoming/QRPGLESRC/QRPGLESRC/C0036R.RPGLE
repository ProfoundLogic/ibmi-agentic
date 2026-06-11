     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0036R - UNDERWRITER REVIEW - LOC/BLDG SEARCH
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the underwriter review Loc/Bldg Search **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   _____  (_____): __________________________ (___)         **
     H****************************************************************
     H*
     FC0036F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMLOL     IF   E           K DISK
     FCMFAL1    IF   E           K DISK
     FCMGLL1    IF   E           K DISK
     FCMLLL1    IF   E           K DISK
     FCMIML1    IF   E           K DISK
     FCMGAL1    IF   E           K DISK
     FCMBTL1    IF   E           K DISK
     FCMFIL1    IF   E           K DISK
     FCMCCL1    IF   E           K DISK
     FCMAIL     IF   E           K DISK
     FCTABL     IF   E           K DISK

     D SEC             S              2    DIM(20)
     D SL              S             78    DIM(24) CTDATA PERRCD(1)
     D CMDL            S             78    DIM(5) CTDATA PERRCD(1)
     D CNCD            S              1    DIM(10) CTDATA PERRCD(1)             FIRE CONSTRUCTION
     D CNDS            S             60    DIM(10) ALT(CNCD)
     D FCCD            S              1    DIM(8) CTDATA PERRCD(1)              FIRE COVERAGE
     D FCDS            S             41    DIM(8) ALT(FCCD)
     D GCCD            S              1    DIM(5) CTDATA PERRCD(1)              GL COVERAGE
     D GCDS            S             76    DIM(5) ALT(GCCD)
     D GCVC            S              6    DIM(7) CTDATA PERRCD(1)              GLASS CLASS
     D CVDS            S             35    DIM(7) ALT(GCVC)
     D GECD            S              1    DIM(10) CTDATA PERRCD(1)             GL EXCLUSIONS
     D GEDS            S             30    DIM(10) ALT(GECD)
     D ATCD            S              1    DIM(5) CTDATA PERRCD(1)              ADDL INT TYPES
     D ATDS            S             18    DIM(5) ALT(ATCD)
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CC                     1     40
     D                                     DIM(20)
     D  PMCC01                 1      2
     D  PMCC02                 3      4
     D  PMCC03                 5      6
     D  PMCC04                 7      8
     D  PMCC05                 9     10
     D  PMCC06                11     12
     D  PMCC07                13     14
     D  PMCC08                15     16
     D  PMCC09                17     18
     D  PMCC10                19     20
     D  PMCC11                21     22
     D  PMCC12                23     24
     D  PMCC13                25     26
     D  PMCC14                27     28
     D  PMCC15                29     30
     D  PMCC16                31     32
     D  PMCC17                33     34
     D  PMCC18                35     36
     D  PMCC19                37     38
     D  PMCC20                39     40
     D                 DS
     D  EXCL                   1     10
     D                                     DIM(10)
     D  LLEC01                 1      1
     D  LLEC02                 2      2
     D  LLEC03                 3      3
     D  LLEC04                 4      4
     D  LLEC05                 5      5
     D  LLEC06                 6      6
     D  LLEC07                 7      7
     D  LLEC08                 8      8
     D  LLEC09                 9      9
     D  LLEC10                10     10
     D                 DS
     D  SCEC                   1    600
     D                                     DIM(10)
     D  SCEC01                 1     60
     D  SCEC02                61    120
     D  SCEC03               121    180
     D  SCEC04               181    240
     D  SCEC05               241    300
     D  SCEC06               301    360
     D  SCEC07               361    420
     D  SCEC08               421    480
     D  SCEC09               481    540
     D  SCEC10               541    600
     D                 DS
     D  SCAI                   1    300
     D                                     DIM(5)
     D  SCAI01                 1     60
     D  SCAI02                61    120
     D  SCAI03               121    180
     D  SCAI04               181    240
     D  SCAI05               241    300
     D                 DS
     D  SCTY                   1     90
     D                                     DIM(5)
     D  SCTY01                 1     18
     D  SCTY02                19     36
     D  SCTY03                37     54
     D  SCTY04                55     72
     D  SCTY05                73     90
     D                 DS
     D  AI                     1     10
     D                                     DIM(5)
     D  IMAI01                 1      2
     D  IMAI02                 3      4
     D  IMAI03                 5      6
     D  IMAI04                 7      8
     D  IMAI05                 9     10
     D*
     D*   DATA STRUCTURES LINE FIELDS
     D*
     D                 DS
     D  CF13                   1     78
     D  COVF                  11     50
     D                 DS
     D  CF14                   1     78
     D  CLASF                  9     77
     D                 DS
     D  CF15                   1     78
     D  FARCBF                20     20
     D  FARCPF                30     30
     D  FARCSF                38     38
     D  CONSF                 49     78
     D                 DS
     D  CF16                   1     78
     D  XXPRTC                13     14
     D  FAMLIN                34     36  0
     D  FAMXIF                55     55
     D  FAEXPI                75     77  0
     D                 DS
     D  CF17                   1     78
     D  AMTIF                 19     29
     D  FACOIN                40     42  0
     D  DEDF                  50     61
     D  FAINFG                75     77  0
     D                 DS
     D  CF18                   1     78
     D  FARTVA                15     15
     D  FAOUTS                32     32
     D  FAECZN                48     49
     D  FAWDEX                68     68
     D                 DS
     D  CF19                   1     78
     D  CONST                  1     15
     D  SUBL                  18     77
     D                 DS
     D  GL13                   1     78
     D  SUBG                  11     77
     D                 DS
     D  GL14                   1     78
     D  CLASG                  9     68
     D                 DS
     D  GL15                   1     78
     D  COVG                  12     77
     D                 DS
     D  GL16                   1     78
     D  MNT                   14     15
     D  DAY                   17     18
     D  YR                    20     21
     D  LLCMMD                40     42  0
     D  LLEXTL                64     64
     D                 DS
     D  GL17                   1     78
     D  BILT                  21     30
     D  PDLT                  55     64
     D                 DS
     D  GL18                   1     78
     D  FDLT                  21     30
     D  MDPL                  55     64
     D                 DS
     D  GL19                   1     78
     D  GAG                   21     30
     D  PCPA                  61     70
     D                 DS
     D  GL20                   1     78
     D  BIDD                  16     27
     D  PDDD                  48     59
     D                 DS
     D  GL21                   1     78
     D  GLPRBS                14     14
     D  EXPG                  30     40
     D  LLBFGL                62     62
     D                 DS
     D  IM14                   1     78
     D  CLASI                  9     77
     D                 DS
     D  IM16                   1     78
     D  EXPI                  23     33
     D  IMCOIN                45     47  0
     D                 DS
     D  IM18                   1     78
     D  DEDI                  14     25
     D                 DS
     D  GS14                   1     78
     D  CLASGS                 9     77
     D                 DS
     D  GS16                   1     78
     D  GARPPL                22     22
     D  GAVACB                41     41
     D  GAGLOC                58     58
     D                 DS
     D  GS18                   1     78
     D  EXPGS                 23     33
     D  DEDGS                 54     65
     D                 DS
     D  BF14                   1     78
     D  CLASBF                 9     77
     D                 DS
     D  BT16                   1     78
     D  BTFRM                 13     14
     D  BTRTGP                34     35
     D  BTSAFE                53     53
     D                 DS
     D  FT16                   1     78
     D  FIFRM                 13     14
     D  FIRTGP                34     35
     D  FISAFE                53     53
     D                 DS
     D  BF18                   1     78
     D  BFEXP                 23     33
     D  BFDED                 54     65
     D*
     D*   DATA STRUCTURES FOR FILE FIELDS (EDITING)
     D*
     D                 DS
     D  EXP                    1     11
     D  EXP1                   1      3
     D  COMMA1                 4      4
     D  EXP2                   5      7
     D  COMMA2                 8      8
     D  EXP3                   9     11
     D                 DS
     D  DED                    1     12
     D  DED1                   1      1
     D  COMMA3                 2      2
     D  DED2                   3      5
     D  COMMA4                 6      6
     D  DED3                   7      9
     D  PERIOD                10     10
     D  DED4                  11     12
     D                 DS
     D  FAEXP                  1      9  0
     D  FEXP1                  1      3
     D  FEXP2                  4      6
     D  FEXP3                  7      9
     D                 DS
     D  DEDEDA                 1      8  0
     D  DEDA1                  1      2
     D  DEDA2                  3      5
     D  DEDA3                  6      8
     D                 DS
     D  LLECMD                 1      7  0
     D  YEAR                   2      3
     D  MONT                   4      5
     D  DAYS                   6      7
     D                 DS
     D  LLBLMT                 1      8  0
     D  BLT1                   1      2
     D  BLT2                   3      5
     D  BLT3                   6      8
     D                 DS
     D  LLPLMT                 1      8  0
     D  PLT1                   1      2
     D  PLT2                   3      5
     D  PLT3                   6      8
     D                 DS
     D  LLFDML                 1      8  0
     D  FLT1                   1      2
     D  FLT2                   3      5
     D  FLT3                   6      8
     D                 DS
     D  LLMDPL                 1      8  0
     D  MLT1                   1      2
     D  MLT2                   3      5
     D  MLT3                   6      8
     D                 DS
     D  LLGAG                  1      8  0
     D  GAG1                   1      2
     D  GAG2                   3      5
     D  GAG3                   6      8
     D                 DS
     D  LLPCPA                 1      8  0
     D  PCA1                   1      2
     D  PCA2                   3      5
     D  PCA3                   6      8
     D                 DS
     D  GLEXP                  1      9  0
     D  GEXP1                  1      3
     D  GEXP2                  4      6
     D  GEXP3                  7      9
     D                 DS
     D  IMEXP                  1      9  0
     D  IEXP1                  1      3
     D  IEXP2                  4      6
     D  IEXP3                  7      9
     D                 DS
     D  GAEXP                  1      9  0
     D  GAEXP1                 1      3
     D  GAEXP2                 4      6
     D  GAEXP3                 7      9
     D                 DS
     D  BTEXP                  1      9  0
     D  BEXP1                  1      3
     D  BEXP2                  4      6
     D  BEXP3                  7      9
     D                 DS
     D  FIEXP                  1      9  0
     D  TEXP1                  1      3
     D  TEXP2                  4      6
     D  TEXP3                  7      9
     D*
     D LDA            UDS
     D  CDDESC                 1     60
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *DTAARA       DEFINE    *LDA          LDA
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE KEY LISTS
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  POLICY MASTER FILE & POLICY AUXILLARY FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  LOCATION MASTER FILE
     C*
     C     LOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXLOC#
     C                   KFLD                    XXPRM#
     C*
     C     LOKEYA        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C*
     C     LOKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  KEY LIST FOR ALL SECTIONS
     C*
     C     SCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXCC              2
     C                   KFLD                    LOLOC#
     C                   KFLD                    LOPRM#
     C*
     C*  GENERAL LIABILITY LIMITS FILE KEY LISTS
     C*
     C     LLKEY         KLIST
     C                   KFLD                    GLCO#
     C                   KFLD                    GLPOL#
     C                   KFLD                    GLCC
     C                   KFLD                    GLTRDR
     C*
     C     LLKEY1        KLIST
     C                   KFLD                    GLCO#
     C                   KFLD                    GLPOL#
     C                   KFLD                    GLCC
     C*
     C*   GL CLASS CODE FILE
     C*
     C     CCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C*
     C*   ADDITIONAL INTEREST FILE - FOR INLAND MARINE AI CODES
     C*
     C     AIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXADI             2
     C*
     C*  TYPE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQLOC#            3
     C                   PARM                    QQPRM#            3
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
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQLOC#        XXLOC#            3 0
     C                   MOVE      QQPRM#        XXPRM#            3 0
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              50
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   CHAIN OUT TO POLICY MASTER AND GET SECTION FOR LOC/BLDG
     C*
     C     PMKEY         CHAIN     PPM01                              60
     C*
     C                   MOVEL     PMFIN         SCFIN
     C*
     C*   FLIP ON CANCELATION INDICATOR (17) IF THERE IS A CANCELATION DATE
     C*
     C     PMCNDT        IFNE      *ZERO
     C                   MOVE      '1'           *IN17
     C                   ELSE
     C                   MOVE      '0'           *IN17
     C                   END
     C*
     C*   ADJUST ARRAY WITH ONLY LOC/BLDG SECTIONS
     C*
     C                   Z-ADD     1             Y                 5 0
     C                   DO        20            X                 5 0
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     CC(X)         XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C     CVVAL         IFEQ      'L'
     C     CVVAL         OREQ      'I'
     C     CVVAL         OREQ      'T'
     C     CVVAL         OREQ      'G'
     C     CVVAL         OREQ      'C'
     C     CVVAL         OREQ      'F'
     C     CVVAL         OREQ      'O'
     C                   MOVEL     CC(X)         SEC(Y)
     C                   ADD       1             Y
     C                   END
     C                   END
     C                   END
     C*
     C*   GET THE APPROPIATE LOC/BLDG AND LOAD SCREEN
     C*
     C     LOKEY         SETLL     PLO01
     C                   MOVE      'I'           LOSTAT
     C                   MOVE      '0'           *IN60
     C     LOSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     LOKEY1        READE     PLO01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     LOKEY1        CHAIN     PLO01                              60
     C                   END
     C     *IN60         IFEQ      '0'
     C                   EXSR      LOLOAD
     C                   END
     C*
     C*   GET THE FIRST SECTION FOR THAT LOCATION AND BUILDING
     C*
     C                   EXSR      GETSC
     C*
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR03'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 05 = LOCATION / BUILDING SEARCH
     C*
     C     *INKE         IFEQ      '1'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   CALL      'C0640R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQRTCD
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCLOC#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           STRT
     C*
     C                   END
     C*
     C*  FUNCTION 07 = ITEM SEARCH BY SECTION LOCATION BUILDING CLASS
     C*
     C     *INKG         IFEQ      '1'
     C*
     C*   INLAND MARINE ITEM SEARCH BY CLASS
     C*
     C     CVVAL         IFEQ      'I'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      XXCC          QQCC              2
     C                   MOVE      XXLOC#        QQLOC#            3
     C                   MOVE      XXPRM#        QQPRM#            3
     C                   MOVE      IMCLAS        QYCLAS            6
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'C0575R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QYCLAS
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           STRT
     C*
     C                   END
     C*
     C*   GLASS ITEM SEARCH BY CLASS
     C*
     C     CVVAL         IFEQ      'G'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      XXCC          QQCC              2
     C                   MOVE      XXLOC#        QQLOC#            3
     C                   MOVE      XXPRM#        QQPRM#            3
     C                   MOVE      GACLAS        QYCLAS            6
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'C0585R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QYCLAS
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           STRT
     C*
     C                   END
     C*
     C*   GL EXCLUSION SCREEN
     C*
     C     CVVAL         IFEQ      'L'
     C*
     C*   LOAD EXCLUSIONS INTO SCREEN 2
     C*
     C                   MOVEL     XXCC          SCCC1
     C                   MOVE      *BLANK        SCEC
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y
     C                   DO        10            X
     C*
     C     EXCL(X)       IFNE      *BLANK
     C                   Z-ADD     1             Z                 5 0
     C     EXCL(X)       LOOKUP    GECD(Z)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVEL     GEDS(Z)       SCEC(Y)
     C                   ADD       1             Y
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 9 = LOAD SCREEN 3 WITH IM ADDITIONAL INTERESTS
     C*
     C     *INKI         IFEQ      '1'
     C*
     C                   MOVEL     XXCC          SCCC1
     C                   MOVEL     CLASI         SCCLAS
     C                   MOVE      *BLANK        SCAI
     C                   MOVE      *BLANK        SCTY
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y
     C                   DO        5             X
     C*
     C*   CHAIN OUT TO ADDITION INTEREST FILE IF THERE IS A CODE
     C*
     C     AI(X)         IFNE      *BLANK
     C                   MOVEL     AI(X)         XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C                   MOVEL     AINAME        SCAI(Y)
     C*
     C*   GET ADDITIONAL INTEREST TYPE
     C*
     C                   Z-ADD     1             Z                 5 0
     C     AIIN          LOOKUP    ATCD(Z)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVEL     ATDS(Z)       SCTY(Y)
     C                   ELSE
     C                   MOVE      *BLANK        SCTY(Y)
     C                   END
     C                   ADD       1             Y
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  FUNCTION 10 = TABLE SEARCH FOR TRANSACTION CODE AND
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     SCCC          IFEQ      '? '
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      *BLANK        QQCC              2
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'C0650R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQRTCD
     C                   MOVE      QQCC          SCCC
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQCC          OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           STRT
     C                   END
     C*
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   CHECK FOR ROLL KEYS
     C*
     C     *IN18         IFEQ      '1'
     C     *IN19         OREQ      '1'
     C*
     C*   GET CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C*
     C*   DEPENDING ON CURSOR POSITION; EXSR  TO ROLL LOC/BLDG, SC, OR SECTION
     C*
     C     LIN           IFLT      12
     C                   EXSR      RLLOC                                        ROLL LOC/BLDG
     C                   END
     C*
     C     LIN           IFEQ      12
     C                   EXSR      RLSEC                                        ROLL THRU SEC
     C                   END
     C*
     C     LIN           IFGT      12
     C     CVVAL         CASEQ     'L'           LIAB
     C     CVVAL         CASEQ     'I'           INLMAR
     C     CVVAL         CASEQ     'T'           BURGT
     C     CVVAL         CASEQ     'G'           GLASS
     C     CVVAL         CASEQ     'C'           FIRE
     C     CVVAL         CASEQ     'F'           FIDFOR
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK SEARCH FIELDS AND EXSR
     C*
     C     SCLOC#        IFNE      *ZERO
     C     SCPRM#        ORNE      *ZERO
     C                   EXSR      SRCHLO
     C     SCCC          IFEQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCCC          IFNE      *BLANK
     C                   EXSR      SRCHCC
     C                   GOTO      STRT
     C                   END
     C*
     C*   GOTO TERM IF ENTER PRESSED WITH NO VALUES IN FIELDS
     C*
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C*
     C*   PROCESS REQUEST SCREEN 2
     C*
     C     SCR02T        TAG
     C*
     C*   GO BACK TO SCREEN 1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   PROCESS REQUEST SCREEN 3
     C*
     C     SCR03T        TAG
     C*
     C*   GO BACK TO SCREEN 1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C****************************************************************
     C*   SUBROUTINES                                                *
     C****************************************************************
     C*
     C*
     C*   SUBROUTINE TO LOAD LOCATION/BUILDING FIELDS ON SCREEN
     C*
     CSR   LOLOAD        BEGSR
     C*
     C                   MOVEL     LOADR1        SCADR1
     C                   MOVEL     LOADR2        SCADR2
     C                   MOVEL     LOCTY         SCCTY
     C                   MOVEL     LOST          SCST
     C                   MOVEL     LOZIP         SCZIP1
     C                   MOVE      LOZIP         SCZIP2
     C                   MOVE      LOOCAS        SCOCAS
     C                   MOVE      LOLOC#        XXLOC#
     C                   MOVE      LOPRM#        XXPRM#
     C*
     CSR                 ENDSR                                                  LOLOAD
     C*
     C*   SUBROUTINE TO CLEAR SCREEN FIELDS FOR EACH SECTION(LINE OF BUS)
     C*
     CSR   CLSCR         BEGSR
     C*
     C                   MOVEL     *BLANKS       SCLN13
     C                   MOVEL     *BLANKS       SCLN14
     C                   MOVEL     *BLANKS       SCLN15
     C                   MOVEL     *BLANKS       SCLN16
     C                   MOVEL     *BLANKS       SCLN17
     C                   MOVEL     *BLANKS       SCLN18
     C                   MOVEL     *BLANKS       SCLN19
     C                   MOVEL     *BLANKS       SCLN20
     C                   MOVEL     *BLANKS       SCLN21
     C                   MOVEL     *BLANKS       SCLN22
     C                   MOVEL     *BLANKS       SCCML1
     C                   MOVEL     *BLANKS       SCCML2
     C*
     C                   ENDSR                                                  CLSCR
     C*
     C*   SUBROUTINE TO LOAD FIRE SECTION ONTO SCREEN
     C*
     CSR   CFLOAD        BEGSR
     C*
     C*   EXECUTE SUBROUTINE TO CLEAR SCREEN
     C*
     C                   EXSR      CLSCR
     C*
     C*   GET EFFECTIVE DATE OF COVERAGE
     C*
     C                   Z-ADD     FATRDR        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*   GET COVERAGE CODE DESCRIPTION
     C*
     C                   Z-ADD     1             X
     C     FACOVF        LOOKUP    FCCD(X)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVEL     FCDS(X)       COVF
     C                   ELSE
     C                   MOVE      *BLANKS       COVF
     C                   END
     C*
     C*   GET CLASS CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        QQPRDS           40
     C                   MOVE      *BLANK        QQCALL            1
     C                   MOVE      FACLAS        QQCLAS           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0740R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCLAS
     C                   PARM                    QQRTCD
     C*
     C                   IN        LDA
     C                   MOVEL     CDDESC        CLASF
     C                   OUT       LDA
     C*
     C*   GET BUILDING CONSTRUCTION CODE DESCRIPTION
     C*
     C                   Z-ADD     1             X
     C     LOCONS        LOOKUP    CNCD(X)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVEL     CNDS(X)       CONSF
     C                   ELSE
     C                   MOVE      *BLANK        CONSF
     C                   END
     C*
     C*   MOVE PROTECTION CLASS INTO DATA STRUCTURE
     C*
     C                   MOVE      LOPRTC        XXPRTC
     C*
     C*   EDIT FIRE EXPOSURE
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      FEXP1         EXP1
     C                   MOVE      FEXP2         EXP2
     C                   MOVE      FEXP3         EXP3
     C     FEXP1         IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     FEXP2         IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           AMTIF
     C*
     C*   GET DEDUCTIBLE AMOUNT AND EDIT FIELD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     FADECD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C                   MOVE      *BLANK        DED
     C                   MOVE      DEDA1         DED1
     C                   MOVE      DEDA2         DED2
     C                   MOVE      DEDA3         DED3
     C                   MOVE      '00'          DED4
     C                   MOVE      '.'           PERIOD
     C     DEDA1         IFNE      *ZERO
     C                   MOVE      ','           COMMA3
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED1
     C     DEDA2         IFNE      *ZERO
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED2
     C                   END
     C                   END
     C                   MOVE      DED           DEDF
     C*
     C*   GET EACH SUBLINE DESCRIPTION
     C*
     C     FASUB1        IFNE      *BLANK
     C                   MOVE      *BLANK        QYPRDS           40
     C                   MOVE      *BLANK        QYCALL            1
     C                   MOVE      FASUB1        QQSUBL           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0750R'
     C                   PARM                    QYPRDS
     C                   PARM                    QYCALL
     C                   PARM                    QQSUBL
     C                   PARM                    QQRTCD
     C*
     C                   MOVEL     QYPRDS        SUBL
     C                   END
     C*
     C                   MOVEL     CF19          SCLN19
     C                   MOVE      *BLANK        CONST
     C*
     C*
     C     FASUB2        IFNE      *BLANK
     C                   MOVE      *BLANK        QYPRDS           40
     C                   MOVE      *BLANK        QYCALL            1
     C                   MOVE      FASUB2        QQSUBL           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0750R'
     C                   PARM                    QYPRDS
     C                   PARM                    QYCALL
     C                   PARM                    QQSUBL
     C                   PARM                    QQRTCD
     C*
     C                   MOVEL     QYPRDS        SUBL
     C                   MOVEL     CF19          SCLN20
     C                   END
     C*
     C*
     C     FASUB3        IFNE      *BLANK
     C                   MOVE      *BLANK        QYPRDS           40
     C                   MOVE      *BLANK        QYCALL            1
     C                   MOVE      FASUB3        QQSUBL           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0750R'
     C                   PARM                    QYPRDS
     C                   PARM                    QYCALL
     C                   PARM                    QQSUBL
     C                   PARM                    QQRTCD
     C*
     C                   MOVEL     QYPRDS        SUBL
     C                   MOVEL     CF19          SCLN21
     C                   END
     C*
     C*
     C     FASUB4        IFNE      *BLANK
     C                   MOVE      *BLANK        QYPRDS           40
     C                   MOVE      *BLANK        QYCALL            1
     C                   MOVE      FASUB4        QQSUBL           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0750R'
     C                   PARM                    QYPRDS
     C                   PARM                    QYCALL
     C                   PARM                    QQSUBL
     C                   PARM                    QQRTCD
     C*
     C                   MOVEL     QYPRDS        SUBL
     C                   MOVEL     CF19          SCLN22
     C                   END
     C*
     C*   FILL THE SCREEN WITH THE FILLED DATA STRUCTURES
     C*
     C                   MOVEL     CF13          SCLN13
     C                   MOVEL     CF14          SCLN14
     C                   MOVEL     CF15          SCLN15
     C                   MOVEL     CF16          SCLN16
     C                   MOVEL     CF17          SCLN17
     C                   MOVEL     CF18          SCLN18
     C                   MOVEL     CMDL(1)       SCCML1
     C                   MOVEL     CMDL(2)       SCCML2
     C                   MOVE      '0'           *IN41
     C                   MOVE      '0'           *IN42
     C*
     C                   MOVE      '0'           *IN60
     C*
     CSR                 ENDSR                                                  CFLOAD
     C*
     C*************************************************************************
     C*   SUBROUTINE TO LOAD GL SECTION ONTO SCREEN
     C*
     CSR   GLLOAD        BEGSR
     C*
     C*   EXECUTE SUBROUTINE TO CLEAR SCREEN
     C*
     C                   EXSR      CLSCR
     C*
     C*   GET EFFECTIVE DATE OF COVERAGE
     C*
     C                   Z-ADD     GLTRDR        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*   GET SUBLINE CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        QQPRDS           40
     C                   MOVE      *BLANK        QQCALL            1
     C                   MOVE      GLSUBL        QQSUBL           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0751R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQSUBL
     C                   PARM                    QQRTCD
     C*
     C                   IN        LDA
     C                   MOVEL     CDDESC        SUBG
     C                   OUT       LDA
     C*
     C*   GET CLASS CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      GLCLAS        XXKEY
     C     CCKEY         CHAIN     PCC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CCDESC        CLASG
     C                   ELSE
     C                   MOVE      *BLANK        CLASG
     C                   END
     C*
     C*   GET COVERAGE CODE DESCRIPTION
     C*
     C                   Z-ADD     1             X
     C     LLCOVG        LOOKUP    GCCD(X)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVEL     GCDS(X)       COVG
     C                   ELSE
     C                   MOVE      *BLANKS       COVG
     C                   END
     C*
     C*   EDIT DATE FIELD
     C*
     C                   MOVE      YEAR          YR
     C                   MOVE      MONT          MNT
     C                   MOVE      DAYS          DAY
     C*
     C*   EDIT BI LIMIT
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      BLT1          EXP1
     C                   MOVE      BLT2          EXP2
     C                   MOVE      BLT3          EXP3
     C     BLT1          IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     BLT2          IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           BILT
     C*
     C*   EDIT PD LIMIT
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      PLT1          EXP1
     C                   MOVE      PLT2          EXP2
     C                   MOVE      PLT3          EXP3
     C     PLT1          IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     PLT2          IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           PDLT
     C*
     C*   EDIT FIRE DAMAGE LIMIT
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      FLT1          EXP1
     C                   MOVE      FLT2          EXP2
     C                   MOVE      FLT3          EXP3
     C     FLT1          IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     FLT2          IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           FDLT
     C*
     C*   EDIT MED PAY LIMIT
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      MLT1          EXP1
     C                   MOVE      MLT2          EXP2
     C                   MOVE      MLT3          EXP3
     C     MLT1          IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     MLT2          IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           MDPL
     C*
     C*   EDIT GENERAL AGGAGATE LIMIT
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      GAG1          EXP1
     C                   MOVE      GAG2          EXP2
     C                   MOVE      GAG3          EXP3
     C     GAG1          IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     GAG2          IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           GAG
     C*
     C*   EDIT PROP/COOP AGGAGATE LIMIT
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      PCA1          EXP1
     C                   MOVE      PCA2          EXP2
     C                   MOVE      PCA3          EXP3
     C     PCA1          IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     PCA2          IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           PCPA
     C*
     C*   GET BI DEDUCTIBLE AMOUNT AND EDIT FIELD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     LLBIDC        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C                   MOVE      *BLANK        DED
     C                   MOVE      DEDA1         DED1
     C                   MOVE      DEDA2         DED2
     C                   MOVE      DEDA3         DED3
     C                   MOVE      '00'          DED4
     C                   MOVE      '.'           PERIOD
     C     DEDA1         IFNE      *ZERO
     C                   MOVE      ','           COMMA3
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED1
     C     DEDA2         IFNE      *ZERO
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED2
     C                   END
     C                   END
     C                   MOVE      DED           BIDD
     C*
     C*   GET PD DEDUCTIBLE AMOUNT AND EDIT FIELD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     LLPDDC        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C                   MOVE      *BLANK        DED
     C                   MOVE      DEDA1         DED1
     C                   MOVE      DEDA2         DED2
     C                   MOVE      DEDA3         DED3
     C                   MOVE      '00'          DED4
     C                   MOVE      '.'           PERIOD
     C     DEDA1         IFNE      *ZERO
     C                   MOVE      ','           COMMA3
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED1
     C     DEDA2         IFNE      *ZERO
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED2
     C                   END
     C                   END
     C                   MOVE      DED           PDDD
     C*
     C*   EDIT GL EXPOSURE
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      GEXP1         EXP1
     C                   MOVE      GEXP2         EXP2
     C                   MOVE      GEXP3         EXP3
     C     GEXP1         IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     GEXP2         IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           EXPG
     C*
     C*   FILL THE SCREEN WITH THE FILLED DATA STRUCTURES
     C*
     C                   MOVEL     GL13          SCLN13
     C                   MOVEL     GL14          SCLN14
     C                   MOVEL     GL15          SCLN15
     C                   MOVEL     GL16          SCLN16
     C                   MOVEL     GL17          SCLN17
     C                   MOVEL     GL18          SCLN18
     C                   MOVEL     GL19          SCLN19
     C                   MOVEL     GL20          SCLN20
     C                   MOVEL     GL21          SCLN21
     C                   MOVEL     CMDL(1)       SCCML1
     C                   MOVEL     CMDL(3)       SCCML2
     C                   MOVE      '1'           *IN41
     C                   MOVE      '0'           *IN42
     C*
     C                   MOVE      '0'           *IN60
     C*
     CSR                 ENDSR                                                  GLLOAD
     C*
     C*************************************************************************
     C*   SUBROUTINE TO LOAD IM SECTION ONTO SCREEN
     C*
     CSR   IMLOAD        BEGSR
     C*
     C*   EXECUTE SUBROUTINE TO CLEAR SCREEN
     C*
     C                   EXSR      CLSCR
     C*
     C*   GET EFFECTIVE DATE OF COVERAGE
     C*
     C                   Z-ADD     IMTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*   GET CLASS CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        QQPRDS           40
     C                   MOVE      *BLANK        QQCALL            1
     C                   MOVE      IMCLAS        QQCLAS           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0744R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCLAS
     C                   PARM                    QQRTCD
     C*
     C                   IN        LDA
     C                   MOVEL     CDDESC        CLASI
     C                   OUT       LDA
     C*
     C*   EDIT EXPOSURE
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      IEXP1         EXP1
     C                   MOVE      IEXP2         EXP2
     C                   MOVE      IEXP3         EXP3
     C     IEXP1         IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     IEXP2         IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           EXPI
     C*
     C*   GET DEDUCTIBLE AMOUNT AND EDIT FIELD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     IMDEDC        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C                   MOVE      *BLANK        DED
     C                   MOVE      DEDA1         DED1
     C                   MOVE      DEDA2         DED2
     C                   MOVE      DEDA3         DED3
     C                   MOVE      '00'          DED4
     C                   MOVE      '.'           PERIOD
     C     DEDA1         IFNE      *ZERO
     C                   MOVE      ','           COMMA3
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED1
     C     DEDA2         IFNE      *ZERO
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED2
     C                   END
     C                   END
     C                   MOVE      DED           DEDI
     C*
     C*   FILL THE SCREEN WITH THE FILLED DATA STRUCTURES
     C*
     C                   MOVEL     IM14          SCLN14
     C                   MOVEL     IM16          SCLN16
     C                   MOVEL     IM18          SCLN18
     C                   MOVEL     CMDL(1)       SCCML1
     C                   MOVEL     CMDL(4)       SCCML2
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN42
     C*
     C                   MOVE      '0'           *IN60
     C*
     CSR                 ENDSR                                                  IMLOAD
     C*
     C*************************************************************************
     C*   SUBROUTINE TO LOAD GLASS SECTION ONTO SCREEN
     C*
     CSR   GSLOAD        BEGSR
     C*
     C*   EXECUTE SUBROUTINE TO CLEAR SCREEN
     C*
     C                   EXSR      CLSCR
     C*
     C*   GET EFFECTIVE DATE OF COVERAGE
     C*
     C                   Z-ADD     GATRDR        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*   GET CLASS CODE DESCRIPTION
     C*
     C                   Z-ADD     1             X
     C     GACLAS        LOOKUP    GCVC(X)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVEL     CVDS(X)       CLASGS
     C                   ELSE
     C                   MOVE      *BLANKS       CLASGS
     C                   END
     C*
     C*   EDIT EXPOSURE
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      GAEXP1        EXP1
     C                   MOVE      GAEXP2        EXP2
     C                   MOVE      GAEXP3        EXP3
     C     GAEXP1        IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     GAEXP2        IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           EXPGS
     C*
     C*   GET DEDUCTIBLE AMOUNT AND EDIT FIELD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     GADECD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C                   MOVE      *BLANK        DED
     C                   MOVE      DEDA1         DED1
     C                   MOVE      DEDA2         DED2
     C                   MOVE      DEDA3         DED3
     C                   MOVE      '00'          DED4
     C                   MOVE      '.'           PERIOD
     C     DEDA1         IFNE      *ZERO
     C                   MOVE      ','           COMMA3
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED1
     C     DEDA2         IFNE      *ZERO
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED2
     C                   END
     C                   END
     C                   MOVE      DED           DEDGS
     C*
     C*   FILL THE SCREEN WITH THE FILLED DATA STRUCTURES
     C*
     C                   MOVEL     GS14          SCLN14
     C                   MOVEL     GS16          SCLN16
     C                   MOVEL     GS18          SCLN18
     C                   MOVEL     CMDL(1)       SCCML1
     C                   MOVEL     CMDL(5)       SCCML2
     C                   MOVE      '1'           *IN41
     C                   MOVE      '0'           *IN42
     C*
     C                   MOVE      '0'           *IN60
     C*
     CSR                 ENDSR                                                  GSLOAD
     C*
     C*************************************************************************
     C*   SUBROUTINE TO LOAD BURGLARY AND THEFT SECTION ONTO SCREEN
     C*
     CSR   BTLOAD        BEGSR
     C*
     C*   EXECUTE SUBROUTINE TO CLEAR SCREEN
     C*
     C                   EXSR      CLSCR
     C*
     C*   GET EFFECTIVE DATE OF COVERAGE
     C*
     C                   Z-ADD     BTTRDR        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*   GET CLASS CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        QQPRDS           40
     C                   MOVE      *BLANK        QQCALL            1
     C                   MOVE      BTCLAS        QQCLAS           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0745R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCLAS
     C                   PARM                    QQRTCD
     C*
     C                   IN        LDA
     C                   MOVEL     CDDESC        CLASBF
     C                   OUT       LDA
     C*
     C*   EDIT EXPOSURE
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      BEXP1         EXP1
     C                   MOVE      BEXP2         EXP2
     C                   MOVE      BEXP3         EXP3
     C     BEXP1         IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     BEXP2         IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           BFEXP
     C*
     C*   GET DEDUCTIBLE AMOUNT AND EDIT FIELD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     IMDEDC        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C                   MOVE      *BLANK        DED
     C                   MOVE      DEDA1         DED1
     C                   MOVE      DEDA2         DED2
     C                   MOVE      DEDA3         DED3
     C                   MOVE      '00'          DED4
     C                   MOVE      '.'           PERIOD
     C     DEDA1         IFNE      *ZERO
     C                   MOVE      ','           COMMA3
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED1
     C     DEDA2         IFNE      *ZERO
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED2
     C                   END
     C                   END
     C                   MOVE      DED           BFDED
     C*
     C*   FILL THE SCREEN WITH THE FILLED DATA STRUCTURES
     C*
     C                   MOVEL     BF14          SCLN14
     C                   MOVEL     BT16          SCLN16
     C                   MOVEL     BF18          SCLN18
     C                   MOVEL     CMDL(1)       SCCML1
     C                   MOVEL     CMDL(2)       SCCML2
     C                   MOVE      '0'           *IN41
     C                   MOVE      '0'           *IN42
     C*
     C                   MOVE      '0'           *IN60
     C*
     CSR                 ENDSR                                                  BTLOAD
     C*
     C*************************************************************************
     C*   SUBROUTINE TO LOAD FIDELITY AND FORGERY SECTION ONTO SCREEN
     C*
     CSR   FFLOAD        BEGSR
     C*
     C*   EXECUTE SUBROUTINE TO CLEAR SCREEN
     C*
     C                   EXSR      CLSCR
     C*
     C*   GET EFFECTIVE DATE OF COVERAGE
     C*
     C                   Z-ADD     FITRDR        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*   GET CLASS CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        QQPRDS           40
     C                   MOVE      *BLANK        QQCALL            1
     C                   MOVE      FICLAS        QQCLAS           10
     C                   MOVE      'A'           QQRTCD            1
     C                   CALL      'C0745R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCLAS
     C                   PARM                    QQRTCD
     C*
     C                   IN        LDA
     C                   MOVEL     CDDESC        CLASBF
     C                   OUT       LDA
     C*
     C*   EDIT EXPOSURE
     C*
     C                   MOVEL     *BLANK        EXP
     C                   MOVE      TEXP1         EXP1
     C                   MOVE      TEXP2         EXP2
     C                   MOVE      TEXP3         EXP3
     C     TEXP1         IFNE      *ZERO
     C                   MOVE      ','           COMMA1
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP1
     C     TEXP2         IFNE      *ZERO
     C                   MOVE      ','           COMMA2
     C                   ELSE
     C                   MOVE      *BLANK        EXP2
     C                   END
     C                   END
     C                   MOVE      EXP           BFEXP
     C*
     C*   GET DEDUCTIBLE AMOUNT AND EDIT FIELD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     FIDECD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C                   MOVE      *BLANK        DED
     C                   MOVE      DEDA1         DED1
     C                   MOVE      DEDA2         DED2
     C                   MOVE      DEDA3         DED3
     C                   MOVE      '00'          DED4
     C                   MOVE      '.'           PERIOD
     C     DEDA1         IFNE      *ZERO
     C                   MOVE      ','           COMMA3
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED1
     C     DEDA2         IFNE      *ZERO
     C                   MOVE      ','           COMMA4
     C                   ELSE
     C                   MOVE      *BLANK        DED2
     C                   END
     C                   END
     C                   MOVE      DED           BFDED
     C*
     C*   FILL THE SCREEN WITH THE FILLED DATA STRUCTURES
     C*
     C                   MOVEL     BF14          SCLN14
     C                   MOVEL     FT16          SCLN16
     C                   MOVEL     BF18          SCLN18
     C                   MOVEL     CMDL(1)       SCCML1
     C                   MOVEL     CMDL(2)       SCCML2
     C                   MOVE      '0'           *IN41
     C                   MOVE      '0'           *IN42
     C*
     C                   MOVE      '0'           *IN60
     C*
     CSR                 ENDSR                                                  FFLOAD
     C*
     C*
     C*************************************************************************
     C*   SUBROUTINE TO GET THE APPROPIATE RECORD  FOR GL
     C*
     CSR   LIAB          BEGSR
     C*
     C*   CLEAR ALL DATA STRUCTURES TO FILL SCREEN
     C*
     C                   MOVEL     *BLANKS       GL13
     C                   MOVEL     *BLANKS       GL14
     C                   MOVEL     *BLANKS       GL15
     C                   MOVEL     *BLANKS       GL16
     C                   MOVEL     *BLANKS       GL17
     C                   MOVEL     *BLANKS       GL18
     C                   MOVEL     *BLANKS       GL19
     C                   MOVEL     *BLANKS       GL20
     C                   MOVEL     *BLANKS       GL21
     C*
     C*   MOVE CONSTANTS INTO EACH LINE
     C*
     C                   MOVEL     SL(8)         GL13
     C                   MOVEL     SL(9)         GL14
     C                   MOVEL     SL(10)        GL15
     C                   MOVEL     SL(11)        GL16
     C                   MOVEL     SL(12)        GL17
     C                   MOVEL     SL(13)        GL18
     C                   MOVEL     SL(14)        GL19
     C                   MOVEL     SL(15)        GL20
     C                   MOVEL     SL(16)        GL21
     C*
     C*   IF NEW SECTION GET FIRST ACTIVE RECORD
     C*
     C     XXBEGN        IFEQ      'Y'
     C*
     C     SCKEY         CHAIN     PGL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     GLSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PGL01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'N'           XXBEGN
     C*
     C*   GET NEXT OR PREVIOUS RECORD FOR THIS SECTION
     C*
     C                   ELSE
     C*
     C*   ROLL UP GET NEXT ACTIVE RECORD
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           GLSTAT
     C                   MOVE      '0'           *IN60
     C     GLSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PGL01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         SETGT     PGL01
     C                   READP     PGL01                                  60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN GET PREVIOUS RECORD
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           GLSTAT
     C                   MOVE      '0'           *IN60
     C     GLSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PGL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      GLCO#
     C     SCPOL#        ORNE      GLPOL#
     C     XXCC          ORNE      GLCC
     C     XXLOC#        ORNE      GLLOC#
     C     XXPRM#        ORNE      GLPRM#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         CHAIN     PGL01                              60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN47
     C                   END
     C*
     C*   GET THE APPROPIATE LIMITS RECORD FOR POLICY AT THAT DATE
     C*
     C     LLKEY         SETGT     PLL01
     C                   MOVE      'I'           LLSTAT
     C                   MOVE      '0'           *IN60
     C     LLSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PLL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     GLCO#         IFNE      LLCO#
     C     GLPOL#        ORNE      LLPOL#
     C     GLCC          ORNE      LLCC
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN31
     C                   END
     C*
     C*   EXECUTE SUBROUTINE TO FILL SCREEN
     C*
     C                   EXSR      GLLOAD
     C*
     CSR                 ENDSR
     C*
     C********************************************************************
     C*   SUBROUTINE TO GET THE APPROPIATE RECORD  FOR IM
     C*
     CSR   INLMAR        BEGSR
     C*
     C*   CLEAR ALL DATA STRUCTURES TO FILL SCREEN
     C*
     C                   MOVEL     *BLANKS       IM14
     C                   MOVEL     *BLANKS       IM16
     C                   MOVEL     *BLANKS       IM18
     C*
     C*   MOVE CONSTANTS INTO EACH LINE
     C*
     C                   MOVEL     SL(9)         IM14
     C                   MOVEL     SL(17)        IM16
     C                   MOVEL     SL(18)        IM18
     C*
     C*   IF NEW SECTION GET FIRST ACTIVE RECORD
     C*
     C     XXBEGN        IFEQ      'Y'
     C*
     C     SCKEY         CHAIN     PIM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     IMSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PIM01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'N'           XXBEGN
     C*
     C*   GET NEXT OR PREVIOUS RECORD FOR THIS SECTION
     C*
     C                   ELSE
     C*
     C*   ROLL UP GET NEXT ACTIVE RECORD
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           IMSTAT
     C                   MOVE      '0'           *IN60
     C     IMSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PIM01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         SETGT     PIM01
     C                   READP     PIM01                                  60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN GET PREVIOUS RECORD
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           IMSTAT
     C                   MOVE      '0'           *IN60
     C     IMSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PIM01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      IMCO#
     C     SCPOL#        ORNE      IMPOL#
     C     XXCC          ORNE      IMCC
     C     XXLOC#        ORNE      IMLOC#
     C     XXPRM#        ORNE      IMPRM#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         CHAIN     PIM01                              60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN47
     C                   END
     C*
     C*   EXECUTE SUBROUTINE TO FILL SCREEN
     C*
     C                   EXSR      IMLOAD
     C*
     CSR                 ENDSR
     C*
     C********************************************************************
     C*   SUBROUTINE TO GET THE APPROPIATE RECORD  FOR BURGLARY & THEFT
     C*
     CSR   BURGT         BEGSR
     C*
     C*   CLEAR ALL DATA STRUCTURES TO FILL SCREEN
     C*
     C                   MOVEL     *BLANKS       BF14
     C                   MOVEL     *BLANKS       BT16
     C                   MOVEL     *BLANKS       BF18
     C*
     C*   MOVE CONSTANTS INTO EACH LINE
     C*
     C                   MOVEL     SL(22)        BF14
     C                   MOVEL     SL(23)        BT16
     C                   MOVEL     SL(24)        BF18
     C*
     C*   IF NEW SECTION GET FIRST ACTIVE RECORD
     C*
     C     XXBEGN        IFEQ      'Y'
     C*
     C     SCKEY         CHAIN     PBT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     BTSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PBT01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'N'           XXBEGN
     C*
     C*   GET NEXT OR PREVIOUS RECORD FOR THIS SECTION
     C*
     C                   ELSE
     C*
     C*   ROLL UP GET NEXT ACTIVE RECORD
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           BTSTAT
     C                   MOVE      '0'           *IN60
     C     BTSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PBT01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         SETGT     PBT01
     C                   READP     PBT01                                  60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN GET PREVIOUS RECORD
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           BTSTAT
     C                   MOVE      '0'           *IN60
     C     BTSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PBT01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      BTCO#
     C     SCPOL#        ORNE      BTPOL#
     C     XXCC          ORNE      BTCC
     C     XXLOC#        ORNE      BTLOC#
     C     XXPRM#        ORNE      BTPRM#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         CHAIN     PBT01                              40
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN47
     C                   END
     C*
     C*   EXECUTE SUBROUTINE TO FILL SCREEN
     C*
     C                   EXSR      BTLOAD
     C*
     CSR                 ENDSR
     C*
     C********************************************************************
     C*   SUBROUTINE TO GET THE APPROPIATE RECORD  FOR GLASS
     C*
     CSR   GLASS         BEGSR
     C*
     C*   CLEAR ALL DATA STRUCTURES TO FILL SCREEN
     C*
     C                   MOVEL     *BLANKS       GS14
     C                   MOVEL     *BLANKS       GS16
     C                   MOVEL     *BLANKS       GS18
     C*
     C*   MOVE CONSTANTS INTO EACH LINE
     C*
     C                   MOVEL     SL(19)        GS14
     C                   MOVEL     SL(20)        GS16
     C                   MOVEL     SL(21)        GS18
     C*
     C*   IF NEW SECTION GET FIRST ACTIVE RECORD
     C*
     C     XXBEGN        IFEQ      'Y'
     C*
     C     SCKEY         CHAIN     PGA01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     GASTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PGA01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'N'           XXBEGN
     C*
     C*   GET NEXT OR PREVIOUS RECORD FOR THIS SECTION
     C*
     C                   ELSE
     C*
     C*   ROLL UP GET NEXT ACTIVE RECORD
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           GASTAT
     C                   MOVE      '0'           *IN60
     C     GASTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PGA01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         SETGT     PGA01
     C                   READP     PGA01                                  60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN GET PREVIOUS RECORD
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           GASTAT
     C                   MOVE      '0'           *IN60
     C     GASTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PGA01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      GACO#
     C     SCPOL#        ORNE      GAPOL#
     C     XXCC          ORNE      GACC
     C     XXLOC#        ORNE      GALOC#
     C     XXPRM#        ORNE      GAPRM#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         CHAIN     PGA01                              60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN47
     C                   END
     C*
     C*************************************************************************
     C*   EXECUTE SUBROUTINE TO FILL SCREEN
     C*
     C                   EXSR      GSLOAD
     C*
     CSR                 ENDSR
     C*
     C********************************************************************
     C*   SUBROUTINE TO GET THE APPROPIATE RECORD  FOR FIRE
     C*
     CSR   FIRE          BEGSR
     C*
     C*   CLEAR ALL DATA STRUCTURES TO FILL SCREEN
     C*
     C                   MOVEL     *BLANKS       CF13
     C                   MOVEL     *BLANKS       CF14
     C                   MOVEL     *BLANKS       CF15
     C                   MOVEL     *BLANKS       CF16
     C                   MOVEL     *BLANKS       CF17
     C                   MOVEL     *BLANKS       CF18
     C                   MOVEL     *BLANKS       CF19
     C*
     C*   MOVE CONSTANTS INTO EACH LINE
     C*
     C                   MOVEL     SL(1)         CF13
     C                   MOVEL     SL(2)         CF14
     C                   MOVEL     SL(3)         CF15
     C                   MOVEL     SL(4)         CF16
     C                   MOVEL     SL(5)         CF17
     C                   MOVEL     SL(6)         CF18
     C                   MOVEL     SL(7)         CF19
     C*
     C*   IF NEW SECTION GET FIRST ACTIVE RECORD
     C*
     C     XXBEGN        IFEQ      'Y'
     C*
     C     SCKEY         CHAIN     PFA01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     FASTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PFA01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'N'           XXBEGN
     C*
     C*   GET NEXT OR PREVIOUS RECORD FOR THIS SECTION
     C*
     C                   ELSE
     C*
     C*   ROLL UP GET NEXT ACTIVE RECORD
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           FASTAT
     C                   MOVE      '0'           *IN60
     C     FASTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PFA01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         SETGT     PFA01
     C                   READP     PFA01                                  60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN GET PREVIOUS RECORD
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           FASTAT
     C                   MOVE      '0'           *IN60
     C     FASTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PFA01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      FACO#
     C     SCPOL#        ORNE      FAPOL#
     C     XXCC          ORNE      FACC
     C     XXLOC#        ORNE      FALOC#
     C     XXPRM#        ORNE      FAPRM#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         CHAIN     PFA01                              60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN47
     C                   END
     C*
     C*   EXECUTE SUBROUTINE TO FILL SCREEN
     C*
     C                   EXSR      CFLOAD
     C*
     CSR                 ENDSR
     C*
     C********************************************************************
     C*   SUBROUTINE TO GET THE APPROPIATE RECORD  FOR FIDELITY & FORGERY
     C*
     CSR   FIDFOR        BEGSR
     C*
     C*   CLEAR ALL DATA STRUCTURES TO FILL SCREEN
     C*
     C                   MOVEL     *BLANKS       BF14
     C                   MOVEL     *BLANKS       FT16
     C                   MOVEL     *BLANKS       BF18
     C*
     C*   MOVE CONSTANTS INTO EACH LINE
     C*
     C                   MOVEL     SL(22)        BF14
     C                   MOVEL     SL(23)        FT16
     C                   MOVEL     SL(24)        BF18
     C*
     C*   IF NEW SECTION GET FIRST ACTIVE RECORD
     C*
     C     XXBEGN        IFEQ      'Y'
     C*
     C     SCKEY         CHAIN     PFI01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     FISTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PFI01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      'N'           XXBEGN
     C*
     C*   GET NEXT OR PREVIOUS RECORD FOR THIS SECTION
     C*
     C                   ELSE
     C*
     C*   ROLL UP GET NEXT ACTIVE RECORD
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           FISTAT
     C                   MOVE      '0'           *IN60
     C     FISTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PFI01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         SETGT     PFI01
     C                   READP     PFI01                                  60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN GET PREVIOUS RECORD
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           FISTAT
     C                   MOVE      '0'           *IN60
     C     FISTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PFI01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      FICO#
     C     SCPOL#        ORNE      FIPOL#
     C     XXCC          ORNE      FICC
     C     XXLOC#        ORNE      FILOC#
     C     XXPRM#        ORNE      FIPRM#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         CHAIN     PFI01                              60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN47
     C                   END
     C*
     C*   EXECUTE SUBROUTINE TO FILL SCREEN
     C*
     C                   EXSR      FFLOAD
     C*
     CSR                 ENDSR
     C*
     C*************************************************************************
     C*   SUBROUTINE TO GET THE FIRST SECTION FOR THAT LOCATION AND BUILDING
     C*
     CSR   GETSC         BEGSR
     C*
     C                   Z-ADD     1             SC                5 0
     C                   MOVE      '1'           *IN60
     C                   MOVE      'Y'           XXBEGN            1
     C*
     C     *IN60         DOWEQ     '1'
     C     SC            ANDLE     20
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C                   MOVEL     SEC(SC)       XXCC
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CVVAL         IFEQ      'L'
     C     SCKEY         CHAIN     PGL01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      LIAB
     C                   ELSE
     C                   MOVE      *BLANK        CVDESC
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'I'
     C     SCKEY         CHAIN     PIM01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      INLMAR
     C                   ELSE
     C                   MOVE      *BLANK        CVDESC
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'T'
     C     SCKEY         CHAIN     PBT01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      BURGT
     C                   ELSE
     C                   MOVE      *BLANK        CVDESC
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'G'
     C     SCKEY         CHAIN     PGA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      GLASS
     C                   ELSE
     C                   MOVE      *BLANK        CVDESC
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'C'
     C     SCKEY         CHAIN     PFA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIRE
     C                   ELSE
     C                   MOVE      *BLANK        CVDESC
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'F'
     C     SCKEY         CHAIN     PFI01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIDFOR
     C                   ELSE
     C                   MOVE      *BLANK        CVDESC
     C                   END
     C                   END
     C*
     C*
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   ADD       1             SC
     C                   END
     C*
     C                   END
     C                   MOVEL     CVDESC        SCSCDS
     C     CVDESC        IFEQ      *BLANK
     C                   EXSR      CLSCR
     C                   END
     C*
     CSR                 ENDSR
     C*
     C*
     C*************************************************************************
     C*   SUBROUTINE TO ROLL THROUGH LOCATION/BUILDING
     C*
     CSR   RLLOC         BEGSR
     C*
     C*   ROLL UP
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           LOSTAT
     C                   MOVE      '0'           *IN60
     C     LOSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     LOKEY1        READE     PLO01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     LOKEY         CHAIN     PLO01                              60
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           LOSTAT
     C                   MOVE      '0'           *IN60
     C     LOSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PLO01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      LOCO#
     C     SCPOL#        ORNE      LOPOL#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     LOKEY         CHAIN     PLO01                              60
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   LOAD SCREEN FOR LOCATION AND BUILDING
     C*
     C                   EXSR      LOLOAD
     C*
     C                   MOVE      'Y'           XXBEGN
     C*
     C*   GET FIRST SECTION FOR LOCATION AND BUILDING
     C*
     C                   EXSR      GETSC
     C                   MOVE      '1'           *IN45
     C*
     CSR                 ENDSR                                                  RLLOC
     C*
     C*************************************************************************
     C*   SUBROUTINE TO ROLL THROUGH LOCATION/BUILDING
     C*
     CSR   RLSEC         BEGSR
     C*
     C*   SET FLAG FOR BEGINNING A NEW SECTION
     C*
     C                   MOVE      'Y'           XXBEGN
     C                   MOVE      CVVAL         XXVAL             1
     C*
     C*   ROLL UP
     C*
     C     *IN18         IFEQ      '1'
     C                   Z-ADD     SC            SAVESC            5 0
     C                   ADD       1             SC
     C                   MOVE      '1'           *IN60
     C     SC            DOWLE     20
     C     *IN60         ANDEQ     '1'
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C                   MOVEL     SEC(SC)       XXCC
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CVVAL         IFEQ      'L'
     C     SCKEY         CHAIN     PGL01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      LIAB
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'I'
     C     SCKEY         CHAIN     PIM01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      INLMAR
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'T'
     C     SCKEY         CHAIN     PBT01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      BURGT
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'G'
     C     SCKEY         CHAIN     PGA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      GLASS
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'C'
     C     SCKEY         CHAIN     PFA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIRE
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'F'
     C     SCKEY         CHAIN     PFI01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIDFOR
     C                   END
     C                   END
     C*
     C*
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   ADD       1             SC
     C                   END
     C*
     C                   END
     C*
     C*   ROLLED UP TO FAR
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     SAVESC        SC
     C                   MOVEL     SEC(SC)       XXCC
     C                   MOVE      'N'           XXBEGN
     C                   MOVE      '1'           *IN22
     C                   MOVE      XXVAL         CVVAL
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     CVDESC        SCSCDS
     C                   END
     C*
     C*   ROLL DOWN
     C*
     C     *IN19         IFEQ      '1'
     C                   Z-ADD     SC            SAVESC            5 0
     C                   SUB       1             SC
     C                   MOVE      '1'           *IN60
     C     SC            DOWGT     *ZERO
     C     *IN60         ANDEQ     '1'
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C                   MOVEL     SEC(SC)       XXCC
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CVVAL         IFEQ      'L'
     C     SCKEY         CHAIN     PGL01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      LIAB
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'I'
     C     SCKEY         CHAIN     PIM01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      INLMAR
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'T'
     C     SCKEY         CHAIN     PBT01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      BURGT
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'G'
     C     SCKEY         CHAIN     PGA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      GLASS
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'C'
     C     SCKEY         CHAIN     PFA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIRE
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'F'
     C     SCKEY         CHAIN     PFI01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIDFOR
     C                   END
     C                   END
     C*
     C*
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   SUB       1             SC
     C                   END
     C*
     C                   END
     C*
     C*   ROLLED DOWN TO FAR
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     SAVESC        SC
     C                   MOVEL     SEC(SC)       XXCC
     C                   MOVE      'N'           XXBEGN
     C                   MOVE      '1'           *IN22
     C                   MOVE      XXVAL         CVVAL
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     CVDESC        SCSCDS
     C                   END
     C                   MOVE      '1'           *IN46
     C*
     CSR                 ENDSR
     C*
     C*************************************************************************
     C*   SUBROUTINE TO SEARCH FOR LOCATION/BUILDING
     C*
     CSR   SRCHLO        BEGSR
     C*
     C*   CHECK FOR VALID LOCATION AND BUILDING
     C*
     C     LOKEYA        CHAIN     PLO01                              60
     C     *IN60         IFEQ      '1'
     C     LOKEY         CHAIN     PLO01                              60
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C*   LOAD SCREEN
     C*
     C                   EXSR      LOLOAD
     C*
     C*   BLANK OUT SCREEN FIELDS
     C*
     C                   MOVE      *BLANK        SCLOC#
     C                   MOVE      *BLANK        SCPRM#
     C                   MOVE      '1'           *IN45
     C*
     C*   GET FIRST SECTION IF SCREEN SECTION CODE IS BLANK
     C*
     C     SCCC          IFEQ      *BLANK
     C                   EXSR      GETSC
     C                   END
     C*
     CSR                 ENDSR                                                  SCRHLO
     C*
     C*************************************************************************
     C*   SUBROUTINE TO SEARCH FOR SECTION
     C*
     CSR   SRCHCC        BEGSR
     C*
     C*   CHECK THAT WE HAVE A VALID SECTION CODE FOR POLICY AND LOC/BLDG
     C*
     C                   Z-ADD     1             X
     C     SCCC          LOOKUP    SEC(X)                                 50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C*   SET FLAG FOR BEGINNING A NEW SECTION
     C*
     C                   MOVE      'Y'           XXBEGN
     C*
     C*   GET THAT SECTION ELSE GET FIRST SECTION FOR LOCATION BUILDING
     C*
     C                   Z-ADD     X             SC
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C                   MOVEL     SEC(SC)       XXCC
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CVVAL         IFEQ      'L'
     C     SCKEY         CHAIN     PGL01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      LIAB
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'I'
     C     SCKEY         CHAIN     PIM01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      INLMAR
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'T'
     C     SCKEY         CHAIN     PBT01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      BURGT
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'G'
     C     SCKEY         CHAIN     PGA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      GLASS
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'C'
     C     SCKEY         CHAIN     PFA01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIRE
     C                   END
     C                   END
     C*
     C     CVVAL         IFEQ      'F'
     C     SCKEY         CHAIN     PFI01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      FIDFOR
     C                   END
     C                   END
     C*
     C*
     C*
     C*   IF *IN60 IS "1" SECTION IS NOT VALID FOR LOC/BLDG
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN36
     C                   EXSR      CLSCR
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     CVDESC        SCSCDS
     C                   MOVEL     *BLANK        SCCC
     C                   MOVE      '1'           *IN46
     C                   END
     C*
     C                   END
     C*
     C*
     CSR                 ENDSR
     C*
     C*
     C*
     C*
**  COMPILE TIME ARRAY FOR EACH LINE'S CONSTANTS                              
Coverage:                                                                    
Class:                                                                       
Replacement  Bldg:  Conts:  Stk:  Constr:                              
Prot Class:     Max Lim Indem:     Max Per Indem:    Ext Per Indem:    
Amt of Insurance:            Co-Ins:    Ded:            Infl Guard:    
Radio/TV Ant:  Outside Sign:   Grp II Sym:    Wind Excluded: 
Causes of Loss:                                                              
Subline:                                                                     
Class:                                                                       
Coverage:                                                                    
Retro Date:   /  /     Claims Made:        Extended Tail:  
Limit BI/CSL:                       Limit PD:                
Fire Damage Limit:                  Med Pay Limit:           
General Aggragate:                  Prop/CoOp Aggragate:            
Deductible BI:               Deductible PD:               
Prem Basis:     Exposure:               Broad Form CGL:   
Amount of Insurance:             Co-Ins:     
Deductible:              
Class:                                                                       
Replacement Plates:      Vacant Chg:       Loc Use:    
Amount of Insurance:                  Deductible:               
Class:                                                                       
Form Code:        Rate Group:       Safe Type:    
Amount of Insurance:                  Deductible:              
** COMPILE TIME ARRAYS FOR THE COMMAND LINES
F1=END OF JOB                F2=PREVIOUS SCREEN               F3=RESTART
F5=LOC#/BLDG# SEARCH                                          F10=TABLE SEARCH
F5=LOC#/BLDG# SEARCH         F7=EXCLUSIONS                    F10=TABLE SEARCH
F5=LOC/BLDG SRCH         F7=ITEM SRCH      F9=ADDL INT SRCH     F10=TABLE SRCH
F5=LOC#/BLDG SRCH               F7=ITEM SEARCH                F10=TABLE SEARCH
** COMPILE TIME ARRAYS FOR FIRE CONSTRUCTION CODES
1Frame
2Joisted Masonry
3Non-Combustible
4Masonry Non-Combustible
5Modified Fire Resistive
6Fire Resistive
7Heavy Timber Joisted Masonry
8Superior Non-Combustible
9Superior Masonry Non Combustible
0Not Applicable
** COMPILE TIME ARRAYS FOR FIRE COVERAGE CODE
1Building
2Personal Property
3Building and/or Personal Property Blanket
4Business Income - Not Rental
5Business Income - Rental
6Extra Expense
7Time Element _ All Other
9Pollution Clean-Up Endt
** COMPILE TIME ARRAY FOR GL COVERAGE CODE
1Claims made coverage - Basic - with retroactive date
2Claims made - Supplemental Extended reporting period - With retroactive date
3Occurrence Coverage
4Claims made coverage - Basic - no retroactive date
5Claims made - Supplemental Extended reporting period - No retroactive date
** COMPILE TIME ARRAY FOR GLASS COVERAGE
    91Covertible Glass
    98Cumulative Deductible Loss Coverage
    01All Other Glass-Class E
    00All Other Glass-All Other
    99Composite Rate Basis
    88Retro Rating
    96"A" Rated Risks
** COMPILE TIME ARRAY FOR GL EXCLUSIONS
IPersonal Injury
CContractual
FBroad Form Propery Damage
EExtended Bodily Injury
AAdvertising Injury
NMedical Malpractice
HHost Liquor
TOwners Contractors Protective
WWorld Wide Coverage
DAdditional Insured
**  COMPILE TIME ARRAY FOR ADDITION INTERESTS TYPE
CCertificate Holder
LLoss Payee
MMorgage
AAdditional Insured
OOther
