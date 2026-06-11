     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0037R - REVIEW COVERAGE BY VEHICLE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program reviews coverage by vehicle.              **
     H**   CALLED FROM:                                             **
     H**     C0035R - Underwriters Review.                          **
     H**   CALLS TO:                                                **
     H**     C0561R - Vehicle Number Search.                        **
     H**     C0650R - Policy Coverage Search.                       **
     H**     C0170R - Driver Master Maintenance                     **
     H**     M0006C - Help Routine.                                 **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H**                                                            **
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
     H**   ______ (_____): ___________________ (___/___)            **
     H****************************************************************
     FC0037F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMVSL     IF   E           K DISK
     FCMASL1    IF   E           K DISK
     FCMA1L1    IF   E           K DISK
     FCMAIL     IF   E           K DISK
     FCTABL     IF   E           K DISK

     D SEC             S              2    DIM(20)
     D CON             S             25    DIM(11) CTDATA PERRCD(1)
     D MPCD            S              1    DIM(7) CTDATA PERRCD(1)              ADDL INT TYPES
     D MPLT            S              8    DIM(7) ALT(MPCD)
     D PDCD            S              1    DIM(8) CTDATA PERRCD(1)              ADDL INT TYPES
     D PDAM            S              7    DIM(8) ALT(PDCD)
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
     D  SCAI                   1    600
     D                                     DIM(10)
     D  SCAI01                 1     60
     D  SCAI02                61    120
     D  SCAI03               121    180
     D  SCAI04               181    240
     D  SCAI05               241    300
     D  SCAI06               301    360
     D  SCAI07               361    420
     D  SCAI08               421    480
     D  SCAI09               481    540
     D  SCAI10               541    600
     D                 DS
     D  SCTY                   1    180
     D                                     DIM(10)
     D  SCTY01                 1     18
     D  SCTY02                19     36
     D  SCTY03                37     54
     D  SCTY04                55     72
     D  SCTY05                73     90
     D  SCTY06                91    108
     D  SCTY07               109    126
     D  SCTY08               127    144
     D  SCTY09               145    162
     D  SCTY10               163    180
     D                 DS
     D  AI                     1     20
     D                                     DIM(10)
     D  VSADI1                 1      2
     D  VSADI2                 3      4
     D  VSADI3                 5      6
     D  VSADI4                 7      8
     D  VSADI5                 9     10
     D  VSADI6                11     12
     D  VSADI7                13     14
     D  VSADI8                15     16
     D  VSADI9                17     18
     D  VSADI0                19     20
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*  VEHICLE MASTER FILE
     C*
     C     VSKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXITM#
     C*
     C     VSKEYA        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCITM#
     C*
     C     VSKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  KEY LIST FOR ALL SECTIONS
     C*
     C     SCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXCC              2
     C                   KFLD                    VSITM#
     C*
     C*  AUTO LIMITS FILE KEY LISTS
     C*
     C     ASKEY         KLIST
     C                   KFLD                    A1CO#
     C                   KFLD                    A1POL#
     C                   KFLD                    A1CC
     C                   KFLD                    A1TRDT
     C*
     C     ASKEY1        KLIST
     C                   KFLD                    A1CO#
     C                   KFLD                    A1POL#
     C                   KFLD                    A1CC
     C*
     C*   ADDITIONAL INTEREST FILE - FOR AUTO AI CODES
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
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQITM#            5
     C                   PARM                    QQDLDT            8 0
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
     C                   MOVE      QQITM#        XXITM#            5 0
     C     QQDLDT        IFNE      0
     C                   Z-ADD     QQDLDT        SCDLDT
     C                   END
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
     C*   CHAIN OUT TO POLICY MASTER AND GET SECTION FOR AUTO
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
     C     PMCNDT        IFNE      0
     C     SCDLDT        ANDEQ     0
     C                   Z-ADD     PMCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT
     C                   END
     C*
     C*   ADJUST ARRAY WITH ONLY VEHICLE SECTIONS
     C*
     C                   Z-ADD     1             Y                 5 0
     C                   DO        20            X                 5 0
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     CC(X)         XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C     CVVAL         IFEQ      'A'
     C                   MOVEL     CC(X)         SEC(Y)
     C                   ADD       1             Y
     C                   END
     C                   END
     C                   END
     C*
     C*   GET THE APPROPIATE VEHICLE AND LOAD SCREEN
     C*
     C     VSKEY         SETLL     PVS01
     C                   MOVE      'I'           VSSTAT
     C                   MOVE      '0'           *IN60
     C     VSSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     VSKEY1        READE     PVS01                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     VSKEY1        CHAIN     PVS01                              60
     C                   END
     C     *IN60         IFEQ      '0'
     C                   EXSR      VSLOAD
     C                   END
     C*
     C*   GET THE FIRST SECTION FOR THAT VEHICLE
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
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 05 = VEHICLE SEARCH
     C*
     C     *INKE         IFEQ      '1'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   Z-ADD     SCITM#        QXITM#            5 0
     C                   Z-ADD     0             QQDLDT            8 0
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQCLAS            6
     C                   CALL      'C0561R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QXITM#
     C                   PARM                    QQDLDT
     C                   PARM                    QQCLAS
     C                   PARM                    QQRTCD
     C                   MOVE      QXITM#        SCITM#
     C     QQDLDT        IFNE      0
     C                   Z-ADD     QQDLDT        SCDLDT                         ALREADY MDYY
     C                   ELSE
     C     PMCNDT        IFNE      0
     C                   Z-ADD     PMCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT
     C                   END
     C                   END
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCITM#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           STRT
     C*
     C                   END
     C*
     C*  FUNCTION 7 = LOAD SCREEN 2 WITH AUTO ADDITIONAL INTERESTS
     C*
     C     *INKG         IFEQ      '1'
     C*
     C                   MOVEL     XXCC          SCCC1
     C                   MOVE      *BLANK        SCAI
     C                   MOVE      *BLANK        SCTY
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y
     C                   DO        10            X
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
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  FUNCTION 8 - CALL DRIVER MAINTENANCE  - SCREEN 1
     C*
     C     *INKH         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      '1'           QQCALL
     C                   CALL      'C0170R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    SCPOL#
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
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
     C*   DEPENDING ON CURSOR POSITION; EXSR  TO ROLL VEHICLE, SC, OR SECTION
     C*
     C     LIN           IFLT      9
     C                   EXSR      RLVEH                                        ROLL VEHICLE
     C                   END
     C*
     C     LIN           IFEQ      9
     C                   EXSR      RLSEC                                        ROLL THRU SEC
     C                   END
     C*
     C     LIN           IFGT      9
     C                   EXSR      AUTO
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK SEARCH FIELDS AND EXSR
     C*
     C     SCITM#        IFNE      *ZERO
     C                   EXSR      SRCHVE
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
     C*   SUBROUTINE TO LOAD VEHICLE FIELDS ON SCREEN
     C*
     CSR   VSLOAD        BEGSR
     C*
     C                   MOVEL     VSYR          SCYR
     C                   MOVEL     VSMAKE        SCMAKE
     C                   MOVEL     VSMODL        SCMODL
     C                   MOVEL     VSBODY        SCBODY
     C                   MOVEL     VSVIN         SCVIN
     C                   MOVE      VSAMNW        SCAMNW
     C                   MOVE      VSITM#        XXITM#
     C*
     CSR                 ENDSR                                                  VSLOAD
     C*
     C*   SUBROUTINE TO CLEAR SCREEN FIELDS FOR EACH SECTION(LINE OF BUS)
     C*
     CSR   CLSCR         BEGSR
     C*
     C                   MOVE      *BLANKS       SCCL
     C                   MOVE      *BLANKS       SCCLAS
     C                   MOVE      *BLANKS       SCLL
     C                   MOVE      *BLANKS       SCLLT1
     C                   MOVE      *BLANKS       SCSL1
     C                   MOVE      *BLANKS       SCLLT2
     C                   MOVE      *BLANKS       SCUM
     C                   MOVE      *BLANKS       SCSL2
     C                   MOVE      *BLANKS       SCUML1
     C                   MOVE      *BLANKS       SCUML2
     C                   MOVE      *BLANKS       SCMP
     C                   MOVE      *BLANKS       SCMPLT
     C                   MOVE      *BLANKS       SCPL
     C                   MOVE      *BLANKS       SCPILT
     C                   MOVE      *BLANKS       SCPD
     C                   MOVE      *BLANKS       SCPIDE
     C                   MOVE      *BLANKS       SCAPL
     C                   MOVE      *BLANKS       SCAPLT
     C                   MOVE      *BLANKS       SCCPD
     C                   MOVE      *BLANKS       SCCMDE
     C                   MOVE      *BLANKS       SCCLD
     C                   MOVE      *BLANKS       SCCLDE
     C                   MOVE      *BLANKS       SCCOV1
     C                   MOVE      *BLANKS       SCCOV2
     C                   MOVE      *BLANKS       SCCOV3
     C                   MOVE      *BLANKS       SCCOV4
     C*
     C                   ENDSR                                                  CLSCR
     C*
     C*   SUBROUTINE TO LOAD AUTO SECTION ONTO SCREEN
     C*
     CSR   ATLOAD        BEGSR
     C*
     C*   EXECUTE SUBROUTINE TO CLEAR SCREEN
     C*
     C                   EXSR      CLSCR
     C*
     C*   GET EFFECTIVE DATE OF COVERAGE
     C*
     C                   Z-ADD     A1TRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*   GET CLASS CODE
     C*
     C                   MOVEL     CON(1)        SCCL
     C                   MOVEL     A1CLAS        SCCLAS
     C*
     C*   DOES VEHICLE HAVE LIABILITY COVERAGE
     C*
     C     A1LIBF        IFEQ      'Y'
     C*
     C*   MOVE OVER CONSTANTS
     C*
     C                   MOVEL     CON(2)        SCLL
     C                   MOVEL     CON(3)        SCUM
     C                   MOVEL     CON(4)        SCMP
     C*
     C*   GET LIABILITY LIMITS
     C*
     C     ASLPP         IFNE      *ZERO
     C     ASLPO         ANDNE     *ZERO
     C                   Z-ADD     ASLPP         SCLLT1
     C                   Z-ADD     ASLPO         SCLLT2
     C                   MOVE      '/'           SCSL1
     C                   ELSE
     C                   Z-ADD     *ZERO         SCLLT2
     C                   MOVE      *BLANK        SCSL1
     C                   Z-ADD     ASLPO         SCLLT1
     C                   END
     C*
     C*   GET UM LIMITS
     C*
     C     ASUMPC        IFNE      *ZERO
     C     ASUPAO        ANDNE     *ZERO
     C                   Z-ADD     ASUMPC        SCUML1
     C                   Z-ADD     ASUPAO        SCUML2
     C                   MOVE      '/'           SCSL2
     C                   ELSE
     C                   Z-ADD     *ZERO         SCUML2
     C                   MOVE      *BLANK        SCSL2
     C                   Z-ADD     ASUPAO        SCUML1
     C                   END
     C*
     C*   GET UM LIMITS
     C*
     C                   Z-ADD     1             X
     C     ASMDPC        LOOKUP    MPCD(X)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVE      MPLT(X)       SCMPLT
     C                   ELSE
     C                   Z-ADD     *ZERO         SCMPLT
     C                   END
     C*
     C                   END
     C*
     C*   CHECK FOR PIP COVERAGE ON VEHICLE
     C*
     C     A1PIPF        IFEQ      'Y'
     C*
     C*   MOVE CONSTANTS
     C*
     C                   MOVEL     CON(5)        SCPL
     C                   MOVEL     CON(6)        SCPD
     C*
     C*   CHAIN OUT PIP LIMIT FOR STATE
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     VSST          XXKEY
     C     TBKEY         CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     PSLIMT        SCPILT
     C                   ELSE
     C                   Z-ADD     *ZERO         SCPILT
     C                   END
     C*
     C*   GET PIP DEDUCTIBLE
     C*
     C                   Z-ADD     1             X
     C     ASPIDD        LOOKUP    PDCD(X)                                50
     C     *IN50         IFEQ      '1'
     C                   MOVE      PDAM(X)       SCPIDE
     C                   ELSE
     C                   Z-ADD     *ZERO         SCPIDE
     C                   END
     C*
     C*   GET ADDITIONAL PIP LIMIT
     C*
     C                   MOVEL     CON(7)        SCAPL
     C                   Z-ADD     A1APLM        SCAPLT
     C*
     C                   END
     C*
     C*   CHECK IF PHYSICAL DAMAGE ON VEHICLE
     C*
     C     A1PDF         IFEQ      'Y'
     C*
     C*   MOVE IN CONSTANTS
     C*
     C                   MOVEL     CON(8)        SCCPD
     C                   MOVEL     CON(9)        SCCLD
     C*
     C*   GET COMPREHSIVE DEDUCTIBLE
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     A1DECD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DEDEDA        SCCMDE
     C                   ELSE
     C                   Z-ADD     *ZERO         SCCMDE
     C                   END
     C*
     C*   GET COLLISION DEDUCTIBLE
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     A1COLD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DEDEDA        SCCLDE
     C                   ELSE
     C                   Z-ADD     *ZERO         SCCLDE
     C                   END
     C*
     C                   END
     C*
     C*   SET ADDITIONAL COVERAGES
     C*
     C     ASHBCL        IFNE      *BLANK
     C                   MOVEL     CON(10)       SCCOV1
     C                   END
     C*
     C     ASEMCL        IFNE      *BLANK
     C                   MOVEL     CON(11)       SCCOV2
     C                   END
     C*
     C                   MOVE      '0'           *IN60
     C*
     CSR                 ENDSR                                                  ATLOAD
     C*
     C*************************************************************************
     C*   SUBROUTINE TO GET THE APPROPIATE AUTO RECORD
     C*
     CSR   AUTO          BEGSR
     C*
     C*
     C*   IF NEW SECTION GET FIRST ACTIVE RECORD
     C*
     C     XXBEGN        IFEQ      'Y'
     C*
     C     SCKEY         CHAIN     PA101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     A1STAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PA101                                  60
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
     C                   MOVE      'I'           A1STAT
     C                   MOVE      '0'           *IN60
     C     A1STAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     SCKEY         READE     PA101                                  60
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         SETGT     PA101
     C                   READP     PA101                                  60
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN GET PREVIOUS RECORD
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           A1STAT
     C                   MOVE      '0'           *IN60
     C     A1STAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PA101                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      A1CO#
     C     SCPOL#        ORNE      A1POL#
     C     XXCC          ORNE      A1CC
     C     XXITM#        ORNE      A1ITM#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     SCKEY         CHAIN     PA101                              60
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
     C     ASKEY         SETGT     PAS01
     C                   MOVE      'I'           ASSTAT
     C                   MOVE      '0'           *IN60
     C     ASSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PAS01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     A1CO#         IFNE      ASCO#
     C     A1POL#        ORNE      ASPOL#
     C     A1CC          ORNE      ASCC
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
     C                   EXSR      ATLOAD
     C*
     CSR                 ENDSR
     C*
     C********************************************************************
     C*   SUBROUTINE TO GET THE FIRST SECTION FOR THAT VEHICLE
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
     C                   MOVEL     SEC(SC)       XXCC
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     SCKEY         CHAIN     PA101                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      AUTO
     C                   ELSE
     C                   MOVE      *BLANK        CVDESC
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
     C*   SUBROUTINE TO ROLL THROUGH VEHICLE
     C*
     CSR   RLVEH         BEGSR
     C*
     C*   ROLL UP
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      'I'           VSSTAT
     C                   MOVE      '0'           *IN60
     C     VSSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C     VSKEY1        READE     PVS01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     VSKEY         CHAIN     PVS01                              60
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ROLL DOWN
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      'I'           VSSTAT
     C                   MOVE      '0'           *IN60
     C     VSSTAT        DOWEQ     'I'
     C     *IN60         ANDEQ     '0'
     C                   READP     PVS01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      VSCO#
     C     SCPOL#        ORNE      VSPOL#
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   END
     C     *IN60         IFEQ      '1'
     C     VSKEY         CHAIN     PVS01                              60
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   LOAD SCREEN FOR LOCATION AND BUILDING
     C*
     C                   EXSR      VSLOAD
     C*
     C                   MOVE      'Y'           XXBEGN
     C*
     C*   GET FIRST SECTION FOR LOCATION AND BUILDING
     C*
     C                   EXSR      GETSC
     C                   MOVE      '1'           *IN45
     C*
     CSR                 ENDSR                                                  RLVEH
     C*
     C*************************************************************************
     C*   SUBROUTINE TO ROLL THROUGH LOCATION/BUILDING
     C*
     CSR   RLSEC         BEGSR
     C*
     C*   SET FLAG FOR BEGINNING A NEW SECTION
     C*
     C                   MOVE      'Y'           XXBEGN
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
     C                   MOVEL     SEC(SC)       XXCC
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     SCKEY         CHAIN     PA101                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      AUTO
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
     C                   MOVEL     SEC(SC)       XXCC
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     SCKEY         CHAIN     PA101                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      AUTO
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
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     CVDESC        SCSCDS
     C                   END
     C                   MOVE      '1'           *IN46
     C*
     CSR                 ENDSR
     C*
     C*************************************************************************
     C*   SUBROUTINE TO SEARCH FOR VEHICLE
     C*
     CSR   SRCHVE        BEGSR
     C*
     C*   CHECK FOR VALID LOCATION AND BUILDING
     C*
     C     VSKEYA        CHAIN     PVS01                              60
     C     *IN60         IFEQ      '1'
     C     VSKEY         CHAIN     PVS01                              60
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C*   LOAD SCREEN
     C*
     C                   EXSR      VSLOAD
     C*
     C*   BLANK OUT SCREEN FIELDS
     C*
     C                   MOVE      *BLANK        SCITM#
     C                   MOVE      '1'           *IN45
     C*
     C*   GET FIRST SECTION IF SCREEN SECTION CODE IS BLANK
     C*
     C     SCCC          IFEQ      *BLANK
     C                   EXSR      GETSC
     C                   END
     C*
     CSR                 ENDSR                                                  SRCHVE
     C*
     C*************************************************************************
     C*   SUBROUTINE TO SEARCH FOR SECTION
     C*
     CSR   SRCHCC        BEGSR
     C*
     C*   CHECK THAT WE HAVE A VALID SECTION CODE FOR POLICY AND VEHICLE
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
     C*   GET THAT SECTION ELSE GET FIRST SECTION FOR VEHICLE
     C*
     C                   Z-ADD     X             SC
     C                   MOVEL     SEC(SC)       XXCC
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SEC(SC)       XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     SCKEY         CHAIN     PA101                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      AUTO
     C                   END
     C*
     C*
     C*   IF *IN60 IS "1" SECTION IS NOT VALID FOR VEHICLE
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
     C*
     C*
     CSR                 ENDSR
     C*
     C*
     C*
     C*
**  COMPILE TIME ARRAY FOR CONSTANTS                              
Class:
Liability Limit:
UM Limit:
Med Pay:
PIP Limit:
Deductible:
Additional PIP Limit:
Comprehensive Deductible:
Collision Deductible:
Hired / Borrowed
Employer / Non-Owned
**  COMPILE TIME ARRAY FOR MED PAY LIMITS
100000500
200000750
300001000
400002000
500003000
600005000
900000000
**  COMPILE TIME ARRAY FOR PIP DEDUCTIBLE
10000000
20025000
30050000
40100000
60025000
70050000
80100000
00000000
**  COMPILE TIME ARRAY FOR ADDITION INTERESTS TYPE
CCertificate Holder
LLoss Payee
MMorgage
AAdditional Insured
OOther
