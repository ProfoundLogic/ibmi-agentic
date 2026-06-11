     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE C0060R - AUTO COVERAGE MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program maintains auto coverage                   **
     H**   CALLED FROM:                                             **
     H**     C0030R - Policy Master File Maintenance                **
     H**   CALLS TO:                                                **
     H**     SRCOPR - Prorate Premium                               **
     H**     SRDCK  - Date Check                                    **
     H**     C0006R - Change User Parameters                        **
     H**     C0130R - Additional Insured Maintenance                **
     H**     C0170R - Driver Master Maintenance                     **
     H**     C0281R - Premium Bearing Endorse Policy Change Page    **
     H**     C0530R - Search Policy Master by Policy Number         **
     H**     C0531R - Search Policy Master By Named Insured         **
     H**     C0560R - Auto Coverage Detail Search                   **
     H**     C0561R - Vehicle Number Search                         **
     H**     C0630R - Casuality Additional Interest Search          **
     H**     C0650R - Policy Coverage Search                        **
     H**     C0733R - Territory Code Search For Auto                **
     H**     C0771R - Liability Limit Codes For Auto                **
     H**     C0790R - OTC Coverage Code Search                      **
     H**     C0791R - Collision Coverage Code Search                **
     H**     C2001R - Coverage Code Search                          **
     H**     C2011R - Deductable Code Table File Search             **
     H**     C2041R - Transaction Type Code Search                  **
     H**     C2190R - Primary Rating Maintenance                    **
     H**     C2291R - Primary Rating Search                         **
     H**     C2221R - County Code Search                            **
     H**     M0006C - Help Routine                                  **
     H**   PROGRAMMER:                                              **
     H**   CREATED:                  PROJECT:                       **
     H****************************************************************
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 08/25/01 C:15  Project: 94CAX                                 *
     H*               -Removed various edits to allow easier entry of *
     H*                policy.                                        *
     H*                                                               *
     H**  08/16/99 *** C:107 *** V2000 ***
     H**           1. Compile only
     H**
     H**  11/18/96 *** C:107 *** V2000 ***
     H**           1. Version 2 format changes.
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      03       Policy is cancelled                          **
     H**      04       Policy is non-renewed                        **
     H**      05       Policy is cancellation pending status.       **
     H**      06       Policy is reinstatement pending status.      **
     H**      20       Produce error message on screen              **
     H**      63       SCR05 Error - Invalid Med Pay Code.          **
     H**      64       SCR05 Error - Invalid UM Code.               **
     H**      67       SCR03 Error - Trans date NE policy eff date. **
     H**      60       General Usage                                **
     H**      85       SCR05 Msg - Vehicle has A/R records.         **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H** FILES:                                                     **
     H**    SMCOL  (PC001): System Company File For Work Comp       **
     H**    CMUPL  (PUP01): Casulty User Parameter File             **
     H**    CMPML  (PPM01): Policy Master File                      **
     H**    CMA1L  (PA101): Auto Coverage Detail File               **
     H**    CMA1L1 (PA102): Auto Coverage Detail File               **
     H**    CMASL  (PAS01): Covered Auto Symbols File               **
     H**    CMASL1 (PAS02): Covered Auto Symbols File               **
     H**    CMAUL  (PAU01): Auto Master Section File                **
     H**    CMVSL  (PVS01): Vehicle Schedule File                   **
     H**    CMAIL  (PAI01): Additional Interest File                **
     H**    CTABL  (     ): Table File Logical                      **
     H**    CDARL  (PAR01): Accounts Receivable                     **
     H**    CDFRL  (PFR01): PCMS Forms Master   (CO#/FORM)          **
     H****************************************************************
     FC0060F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMA1L     UF A E           K DISK
     FCMA1L1    IF   E           K DISK
     F                                     RENAME(PA101:PA102)
     FCMASL     UF A E           K DISK
     FCMASL1    IF   E           K DISK
     F                                     RENAME(PAS01:PAS02)
     FCMAUL     UF A E           K DISK
     FCMVSL     UF A E           K DISK
     FCMAIL     IF   E           K DISK
     FCDARL     IF   E           K DISK
     FCDFRL     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F                                     IGNORE(PPC01)

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D L524            S             78    DIM(6)                               SCR05 LINE 24
     D TABL24          S              2    DIM(8) CTDATA PERRCD(1)              LINE 24 TEXT
     D TABL2D          S             78    DIM(8) ALT(TABL24)
     D FRM             S              8    DIM(20)                              FORM INPUT AREA
     D FRD             S              5    DIM(20)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  XXDATE                 1      8  0
     D  XDMM                   1      2  0
     D  XDYY                   5      8  0
     D  XDYY2                  7      8  0
      *
     D                 DS
     D  FREFDT                 1      5  0
     D  FMM                    2      3  0
     D  FYY                    4      5  0
      *
     D                 DS
     D  XFEFDT                 1      5  0
     D  FXYY                   2      3  0
     D  FXMM                   4      5  0
      *
     D                 DS
     D  DSACMY                 1      5  0
     D  DSACCN                 1      1  0
     D  DSACYY                 2      3  0
     D  DSACMM                 4      5  0
     D                 DS
     D  SCACMY                 1      6  0
     D  SCACMM                 1      2  0
     D  SCACYY                 3      6  0
     D  SCACY2                 5      6  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  AI                     1     20
     D                                     DIM(10) ASCEND
     D  SCADI1                 1      2
     D  SCADI2                 3      4
     D  SCADI3                 5      6
     D  SCADI4                 7      8
     D  SCADI5                 9     10
     D  SCADI6                11     12
     D  SCADI7                13     14
     D  SCADI8                15     16
     D  SCADI9                17     18
     D  SCADI0                19     20
     D                 DS
     D  SC                     1     40
     D                                     DIM(20) ASCEND
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
     D*
     D* -----------------------------------------------------
     D* SCREEN 3 - LINE 24 ENTRIES
     D* -----------------------------------------------------
     D                 DS
     D  L324                   1    312
     D                                     DIM(4)                               SCR03 LINE 24
     D  L32401                 1     78    INZ('F1=End of job       -
     D                                     F2=Prior screen     -
     D                                     F5=Detail search    -
     D                                             F24=More  ')
     D  L32402                79    156    INZ('F7=Vehicle number se-
     D                                     arch   F10=Table sea-
     D                                     rch                 -
     D                                             F24=More  ')
     D  L32403               157    234    INZ('F11=Driver maintenan-
     D                                     ce     F13=Change us-
     D                                     er parms            -
     D                                             F24=More  ')
     D*
     D L324X1          C                   CONST('F1=Return to initial-
     D                                      program   F2=Prior -
     D                                     screen   F5=Detail s-
     D                                     earch   F24=More  ')
     D*
     D*           POLICY FORMS WORK AREA
     D*
     D                 DS
     D  SCFR                   1    200
     D                                     DIM(20)                              FORMS ARRAY
     D  SCFR01                 1     10
     D  SCFR02                11     20
     D  SCFR03                21     30
     D  SCFR04                31     40
     D  SCFR05                41     50
     D  SCFR06                51     60
     D  SCFR07                61     70
     D  SCFR08                71     80
     D  SCFR09                81     90
     D  SCFR10                91    100
     D  SCFR11               101    110
     D  SCFR12               111    120
     D  SCFR13               121    130
     D  SCFR14               131    140
     D  SCFR15               141    150
     D  SCFR16               151    160
     D  SCFR17               161    170
     D  SCFR18               171    180
     D  SCFR19               181    190
     D  SCFR20               191    200
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCPOL#                 4     16
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  AUTO DETAIL COVERAGE FILE KEY LISTS
     C*
     C     A1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCITM#
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCTRNS
     C                   KFLD                    XXTRDT            7 0
     C*
     C     A1KY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCITM#
     C                   KFLD                    SCCLAS
     C                   KFLD                    XXTRDT
     C*
     C     A1KY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCITM#
     C*
     C*  COVERED AUTO SYMBOLS FILE KEY LISTS
     C*
     C     ASKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCTRNS
     C                   KFLD                    XXTRDT
     C*
     C     ASKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C*
     C     ASKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    XXTRDT
     C*
     C*  AUTO COVERAGE MASTER FILE KEY LIST
     C*
     C     AUKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  VEHICLE SCHEDULE FILE KEY LIST
     C*
     C     VSKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCITM#
     C*
     C*  ACCOUNTS RECEIVABLE KEY LIST
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  VEHICLE SCHEDULE FILE KEY LIST 2
     C*
     C     VSKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*** END ADD ** 12/23/94 **************************************************
     C*
     C*  ADDITIONAL INTEREST MASTER FILE
     C*
     C     AIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXADI             2
     C*
     C*  TABLE FILE  KEY LIST
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     TBKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C     TBKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    ZZKEY
     C                   KFLD                    ZZEFDT            7 0
     C*
     C*  PCMS FORMS MASTER FILE KEY LIST
     C*
     C     FRKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCC
     C     *LIKE         DEFINE    FRCC          XXCC
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQCC              2
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
     C                   MOVE      QQCC          SCCC
     C                   Z-ADD     *ZERO         SCACMY
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME - IF CALLED FROM
     C*   POLICY MAINT PGM, GO TO POLICY FORM MAINT & NEVER ALLOW SCR01
     C*   TO EXECUTED.
     C*
     C     SCPOL#        IFEQ      *BLANK
     C     SCCC          OREQ      *BLANK
     C                   MOVE      '0'           *IN08
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      611           SCALSL
     C                   MOVE      9             SCSTXL
     C                   MOVE      0             SCZONL
     C                   MOVE      9             SCRIDL
     C                   MOVE      100           SCRTML
     C                   MOVE      194           SCASLL
     C     SCCC          IFEQ      'AU'
     C                   MOVE      615           SCPISL
     C                   MOVE      0             SCSTXP
     C                   MOVE      0             SCZONP
     C                   MOVE      9             SCRIDP
     C                   MOVE      100           SCRTMP
     C                   MOVE      193           SCASLP
     C                   END
     C                   MOVE      'T'           SCREIN
     C                   MOVE      0             SCCMNR
     C*
     C*          SET UP TO LOAD FORMS TABLE
     C*          ------------------------------------------------------
     C                   CLEAR                   SCFR
     C                   CLEAR                   FRM
     C                   CLEAR                   FRD
     C                   MOVE      SCCC          XXCC
     C     FRKY          CHAIN     PFR01                              50
     C                   Z-ADD     1             X
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     *DATE         XXDATE
     C     XDYY          IFGE      2000
     C                   Z-ADD     100           XYY3              3 0
     C                   ELSE
     C                   Z-ADD     0             XYY3
     C                   END
     C                   MOVE      XDYY2         XYY3
     C                   MOVE      XDMM          XXMMYY            5 0
     C                   MOVEL     XYY3          XXMMYY
     C*
     C*          READ FORMS MASTER FILE
     C*          ------------------------------------------------------
     C     *IN50         DOWEQ     *OFF
     C                   MOVE      FYY           FXYY
     C                   MOVE      FMM           FXMM
     C     XFEFDT        IFLE      XXMMYY
     C                   Z-ADD     1             V                 3 0
     C*
     C*             FORM DATE IS ACTIVE
     C*             ---------------------------------------------------
     C                   MOVE      FRFORM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FRFRM8            8
      *
     C     FRFRM8        LOOKUP    FRM(V)                                 50
     C     *IN50         IFEQ      *ON
     C*
     C*                IF ALREADY ON TABLE - UPDATE DATE
     C*                ------------------------------------------------
     C                   MOVE      FREFDT        FRD(V)
     C                   ELSE
     C                   Z-ADD     1             V                 3 0
     C*
     C*                NOT ON TABLE - ADD IT
     C*                ------------------------------------------------
     C     '        '    LOOKUP    FRM(V)                                 50
     C     *IN50         IFEQ      *ON
     C                   MOVE      FRFRM8        FRM(V)
     C                   MOVE      FREFDT        FRD(V)
     C                   ADD       1             X
     C                   END
     C                   END
     C                   END
     C*
     C*             PROCESS UNTIL TABLE FULL OR ALL FORMS READ
     C*             ---------------------------------------------------
     C     X             COMP      20                                 50
     C     *IN50         IFEQ      *OFF
     C     FRKY          READE     PFR01                                  50
     C                   END
     C                   ENDDO
     C*
     C*             LOAD FORMS TO SCREEN
     C*             ---------------------------------------------------
     C                   DO        20            V
     C                   MOVEL     FRM(V)        SCFR(V)
     C     FRM(V)        IFNE      *BLANKS
     C                   MOVEL     FRD(V)        XXYYMM            5 0
     C                   MOVEL     XXYYMM        XXYY              3 0
     C                   MOVE      XXYYMM        XXMM              2 0
     C                   MOVE      XXYY          XXMMYY            5 0
     C                   MOVEL     XXMM          XXMMYY
     C                   MOVE      XXMMYY        SCFR(V)
     C                   END
     C                   ENDDO
     C                   ELSE
     C                   MOVE      '1'           *IN08
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
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
     C*          LOAD SCREEN 3 - LINE 24 TABLES
     C*          ------------------------------------------------------
     C     *IN08         IFEQ      *ON
     C                   MOVE      *BLANKS       L324(1)
     C                   MOVEL     L324X1        L324(1)
     C                   END
     C                   MOVE      L324(1)       SC324
     C                   Z-ADD     1             I3                3 0
     C*
     C*   LOAD LINE 24 TABLES
     C*
     C                   CLEAR                   L524
      *
     C     *IN08         IFEQ      *OFF
     C     *IN09         ANDEQ     *ON
     C     '5A'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L524(1)
     C     *IN08         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C     '5B'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L524(1)
     C     *IN08         IFEQ      *ON
     C     *IN09         ANDEQ     *ON
     C     '5C'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L524(1)
     C     *IN08         IFEQ      *ON
     C     *IN09         ANDEQ     *OFF
     C     '51'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L524(1)
     C     *IN09         IFEQ      *OFF
     C     '52'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L524(2)
     C     '53'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L524(3)
     C     '54'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L524(4)
     C     '55'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L524(5)
     C                   END
     C                   MOVE      L524(1)       SC524
     C                   Z-ADD     1             I5                3 0
     C*                                                                *********
     C     UPLVL         IFGE      99
     C                   MOVE      *ON           *IN14
     C                   END
     C*
     C*   IF CALLED FROM ANOTHER PROGRAM GOTO SCR01T
     C*
     C     *IN08         IFEQ      '1'
     C                   GOTO      SCR01T
     C                   END
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
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
     C                   END
     C*
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C     *IN08         IFEQ      '1'
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   GOTO      SCR01T
     C                   END
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVE      L324(1)       SC324
     C                   Z-ADD     1             I3
     C                   MOVEL     'SCR03'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR05'
     C     SCUPRV        IFEQ      'Y'
     C                   GOTO      SCR03T
     C                   ELSE
     C                   MOVEL     'SCR03'       Q2RECF
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 03 = RESTART - IF CALLED FROM POL. MAINT. GO TO
     C*                          POL.FORMS, OTHERWISE GOTO SCR01
     C*
     C     *INKC         IFEQ      '1'
     C     *IN08         IFEQ      '0'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      SCR01T
     C                   END
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 05 = COVERAGE DETAIL SEARCH -  OR SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVEL     Q2RECF        XXHLD             5
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      SCITM#        QQITM#            5 0
     C                   MOVE      SCCLAS        QQCLAS            6
     C                   MOVE      *BLANKS       QQTRNS            2
     C                   MOVE      *BLANKS       QQTRDT            8 0
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0560R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQITM#
     C                   PARM                    QQCLAS
     C                   PARM                    QQTRNS
     C                   PARM                    QQTRDT
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQCC          SCCC
     C                   MOVE      QQITM#        SCITM#
     C                   MOVE      QQCLAS        SCCLAS
     C                   MOVE      QQTRNS        SCTRNS
     C     QQTRDT        IFNE      *ZEROS
     C                   MOVE      QQTRDT        SCTRDT
     C                   END
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQITM#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*         FUNCTION 13 = CHANGE USER PARAMETERS
     C*          -----------------------------------------------------
     C     *INKM         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR03'
     C                   CALL      'C0006R'
     C     UPKEY         CHAIN     PUP01                              50
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   LIABILITY LIMIT CODE SEARCH - SCREEN 4
     C*
     C*
     C*   TERRITORY CODE SEARCH - SCREEN 5
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERA        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0733R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCTERA
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN76
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN77
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 06 - COPY VEHICLE
     C*---------------------------------------------------------------
     C     *INKF         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR05'
     C     Q2CPOS        DIV       256           #ROW
     C                   MVR                     #COL
     C*
     C*          MOVE DATA TO WINDOW
     C*          -----------------------------------------------------
     C     *IN11         DOUEQ     *ON
     C                   Z-ADD     0             WNYR
     C                   MOVE      *BLANKS       WNMAKE
     C                   MOVE      *BLANKS       WNMODL
     C                   MOVE      *BLANKS       WNBODY
     C                   MOVE      *BLANKS       WNVIN
     C                   EXFMT     WIN05B
     C*
     C*         FUNCTION 01 = END OF JOB
     C*          -----------------------------------------------------
     C     *INKA         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*         FUNCTION 02 = RETURN TO SCREEN SCR05
     C*          -----------------------------------------------------
     C     *INKB         IFEQ      *ON
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   MOVE      L524(1)       SC524
     C                   Z-ADD     1             I5
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
     C*
     C*          GET THE NEXT ITEM NUMBER
     C*          -----------------------------------------------------
     C     VSKEY2        SETGT     PVS01                              60
     C     *IN60         IFEQ      *OFF
     C                   READP     PVS01                                  60
     C                   END
     C     *IN60         IFEQ      *OFF
     C     VSCO#         ANDEQ     UPCO#
     C     VSPOL#        ANDEQ     SCPOL#
     C     VSITM#        ADD       1             WNITM#
     C     VSKEY         CHAIN     PVS01                              60
     C*
     C*          WRITE THE NEW RECORDS
     C*          -----------------------------------------------------
     C                   Z-ADD     WNITM#        VSITM#
     C                   Z-ADD     WNYR          VSYR
     C                   MOVE      WNMAKE        VSMAKE
     C                   MOVE      WNMODL        VSMODL
     C                   MOVE      WNBODY        VSBODY
     C                   MOVE      WNVIN         VSVIN
     C                   WRITE     PVS01
     C                   Z-ADD     WNITM#        A1ITM#
     C                   WRITE     PA101
     C                   END
     C*
     C*          GET THE RECORD RELATED TO SCREEN SCR05
     C*          -----------------------------------------------------
     C     VSKEY         CHAIN     PVS01                              60
     C                   EXCEPT    PVS01E
     C*
     C*          RESTORE AUTO LIABILITY ITEM NUMBER
     C*          -----------------------------------------------------
     C                   Z-ADD     SCITM#        A1ITM#
     C                   ENDDO
     C*
     C*          COPIES DONE - RETURN TO SCREEN SCR05
     C*          -----------------------------------------------------
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME, SCR01
     C*                CALL ADDITIONAL LOCATION PREMISIS, SCR02
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       QQPOL#
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   VEHICLE SEARCH
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   Z-ADD     0             QQDLDT            8 0
     C                   Z-ADD     SCITM#        QQITM#
     C                   MOVE      *BLANKS       QQCLAS            6
     C                   CALL      'C0561R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCPOL#
     C                   PARM                    QQITM#
     C                   PARM                    QQDLDT
     C                   PARM                    QQCLAS
     C                   PARM                    QQRTC             1
     C                   MOVE      QQITM#        SCITM#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQITM#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  CALL ADDITIONAL INTERESTS  - SCREEN 5
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   CALL      'C0130R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   Z-ADD     1             Z                 2 0
     C     *BLANK        LOOKUP    AI(Z)                                  50
     C     *IN50         IFEQ      '1'
     C     Z             ADD       89            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      '1'           *IN37
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 09 = POLICY COVERAGE SEARCH -SCR01
     C*                ADDITIONAL INTEREST SEARCH -SCR04
     C     *INKI         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     SCPOL#        IFEQ      *BLANKS
     C                   GOTO      SCR01T
     C                   END
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0650R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQRTC             1
     C                   MOVE      QQCC          SCCC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCC          OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C*   SEARCH ADDITIONAL INTEREST CMD 9
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C*
     C                   Z-ADD     1             Z                 2 0
     C     '? '          LOOKUP    AI(Z)                                  50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      *BLANK        QQADI             2
     C                   MOVE      ' '           QQTYPE            1
     C                   CALL      'C0630R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQADI
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C                   MOVE      QQADI         AI(Z)
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     QQADI         OREQ      *BLANK
     C     Z             ADD       89            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     Z             ADD       90            Y
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* FUNCTION 10
     C*  TABLE FILE SEARCH  - SECTION CODE SCR01;
     C*                       DEDUCTIBLE CODE SCR05.
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVEL     SCCC          XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2001R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCCC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C                   END
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVEL     SCTRNS        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2041R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCTRNS
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C     '?'           SCAN      SCCLAS                                 50
     C     *IN50         IFEQ      *ON
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2191R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCCLAS
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*   SCREEN 04 - TEST BI DEDUCTION CODE
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     *IN41         IFEQ      '0'
     C*
     C     SCDEBI        IFEQ      '? '
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCDEBI
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C                   END
     C*
     C*   DED CODES SEARCH FOR SCREEN 5
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C*
     C     *IN41         IFEQ      '0'
     C*
     C     SCDEPD        IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCDEPD
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN73
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     *IN43         IFEQ      '0'
     C*
     C     SCDECD        IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCDECD
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN74
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN79
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C     SCCOLD        IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCCOLD
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN80
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN75
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   COUNTY CODE SEARCH
     C*
     C     *IN45         IFEQ      '0'
     C     SCCNTY        IFEQ      '?'
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCCNTY        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C2221R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCCNTY
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN53
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 11 - CALL DRIVER MAINTENANCE  - SCREEN 3
     C*
     C     *INKK         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   CALL      'C0170R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    SCPOL#
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 14 - DARTS
     C*
     C     *INKN         IFEQ      '1'
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      UPCO#         QCCO#
     C                   MOVE      SCPOL#        QCPOL#
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 15 - OTC CODVERAGE CODE SEARCH - SCREEN 5
     C*
     C     *INKP         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR05'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCOTCC        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0790R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCOTCC
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN78
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN74
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 17 - COLL COVERAGE CODE SEARCH - SCREEN 5
     C*
     C     *INKR         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR05'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCOLC        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0791R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCCOLC
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN79
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN80
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 19 - A/R INQUIRY
     C*
     C     *INKT         IFEQ      *ON
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0180R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 20 - CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     *IN61         ANDEQ     '0'
     C*
     C     SCSTA4        IFEQ      'A'
     C                   MOVE      'I'           SCSTA4
     C                   ELSE
     C                   MOVE      'A'           SCSTA4
     C                   END
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C     *IN62         ANDEQ     '0'
     C*
     C     SCSTA5        IFEQ      'A'
     C                   MOVE      'I'           SCSTA5
     C                   ELSE
     C                   MOVE      'A'           SCSTA5
     C                   END
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  FUNCTION 21 - DISPLAY PRORATED
     C*---------------------------------------------------------------
     C*
     C     *INKV         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR05'
     C     Q2CPOS        DIV       256           #ROW
     C                   MVR                     #COL
     C*
     C*          MOVE DATA TO WINDOW
     C*          -----------------------------------------------------
     C                   Z-ADD     A1SLBP        WCSLBP
     C                   Z-ADD     A1MEDP        WCMEDP
     C                   Z-ADD     A1UNMP        WCUNMP
     C                   Z-ADD     A1PRDP        WCPRDP
     C                   Z-ADD     A1PIPP        WCPIPP
     C                   Z-ADD     A1ADLP        WCADLP
     C                   Z-ADD     A1COLP        WCCOLP
     C                   Z-ADD     A1NCLP        WCNCLP
     C                   Z-ADD     A1TOWP        WCTOWP
     C                   EXFMT     WIN05C
     C*
     C*         FUNCTION 01 = END OF JOB
     C*          -----------------------------------------------------
     C     *INKA         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*         FUNCTION 02 = RETURN TO SCREEN SCR05
     C*          -----------------------------------------------------
     C     *INKB         IFEQ      *ON
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   MOVE      L524(1)       SC524
     C                   Z-ADD     1             I5
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
     C*
     C*          RETURN TO SCREEN SCR05
     C*          -----------------------------------------------------
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
     C*
     C*---------------------------------------------------------------
     C*
     C*  FUNCTION 23 - DELETE VEHICLE
     C*---------------------------------------------------------------
     C*
     C     *INKX         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR05'
     C     Q2CPOS        DIV       256           #ROW
     C                   MVR                     #COL
     C*
     C*          IF A/R RECORD EXISTS POST MESSAGE
     C*          -----------------------------------------------------
     C     ARKEY         CHAIN     PAR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PMEXDT        WRK70             7 0
     C*
     C     PMCNDT        IFLT      PMEXDT
     C                   Z-ADD     PMCNDT        WRK70
     C                   END
     C*
     C     *IN60         DOUEQ     *ON
     C     ARPODT        IFGE      PMEFDT
     C     ARPODT        ANDLE     WRK70
     C                   MOVE      *ON           *IN85
     C                   GOTO      STRT
     C                   END
     C*
     C     ARKEY         READE     PAR01                                  60
     C                   ENDDO
     C                   END
     C*
     C*          MOVE DATA TO WINDOW
     C*          -----------------------------------------------------
     C                   MOVE      *BLANK        WNFLAG
     C                   Z-ADD     SCITM#        WNITM#
     C                   Z-ADD     SCYR          WNYR
     C                   MOVE      SCMAKE        WNMAKE
     C                   MOVE      SCMODL        WNMODL
     C                   MOVE      SCBODY        WNBODY
     C                   MOVE      SCVIN         WNVIN
     C                   EXFMT     WIN05A
     C*
     C*         FUNCTION 01 = END OF JOB
     C*          -----------------------------------------------------
     C     *INKA         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*         FUNCTION 02 = RETURN TO SCREEN SCR05
     C*          -----------------------------------------------------
     C     *INKB         IFEQ      *ON
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   MOVE      L524(1)       SC524
     C                   Z-ADD     1             I5
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
     C*
     C*          IF CONFORMATION IS YES - DELETE RECORD
     C*          -----------------------------------------------------
     C     WNFLAG        IFEQ      'Y'
     C     VSKEY         CHAIN     PVS01                              60
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PVS01
     C                   END
     C     A1KEY         CHAIN     PA101                              60
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PA101
     C                   END
     C                   MOVE      *BLANKS       SCITM#
     C                   MOVE      *BLANKS       SCCLAS
     C                   MOVE      *BLANKS       SCTRNS
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*          IF CONFORMATION IS NO  - RETURN TO SCREEN SCR05
     C*          -----------------------------------------------------
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 24 - CHANGE FUNCTION LINE
     C*---------------------------------------------------------------
     C*
     C     *INKY         IFEQ      *ON
     C                   MOVEL     Q2RECF        SCHOLD            5
     C     Q2CPOS        DIV       256           #ROW
     C                   MVR                     #COL
     C*
     C*          SCREEN 3 LINE 24
     C*          -----------------------------------------------------
     C     Q2RECF        IFEQ      'SCR03'
     C                   ADD       1             I3
     C     L324(I3)      IFEQ      *BLANKS
     C                   MOVE      L324(1)       SC324
     C                   Z-ADD     1             I3
     C                   ELSE
     C                   MOVE      L324(I3)      SC324
     C                   END
     C                   END
     C*          -----------------------------------------------------
     C*          SCREEN 5 LINE 24
     C*          -----------------------------------------------------
     C     Q2RECF        IFEQ      'SCR05'
     C                   ADD       1             I5
     C     L524(I5)      IFEQ      *BLANKS
     C                   MOVE      L524(1)       SC524
     C                   Z-ADD     1             I5
     C                   ELSE
     C                   MOVE      L524(I5)      SC524
     C                   END
     C                   END
     C*
     C*          COMMMAND 24 EXIT
     C*          -----------------------------------------------------
     C     C24X          TAG
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     SCHOLD        Q2RECF
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
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
     C     Q2RECF        IFNE      'SCR03'
     C*
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   ELSE
     C                   CALL      'C0006R'
     C     UPKEY         CHAIN     PUP01                              50
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE POLICY NUMBER
     C*
     C     PMKEY         CHAIN     PPM01                              20
     C     PMKEY         CHAIN     PPM01                              60
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C     PMCANC        IFEQ      'P'
     C                   MOVE      '1'           *IN05
     C                   ELSE
     C                   MOVE      '0'           *IN05
     C                   END
     C     PMCANC        IFEQ      'Q'
     C                   MOVE      '1'           *IN06
     C                   ELSE
     C                   MOVE      '0'           *IN06
     C                   END
     C*   SET INDICATOR TO DISPLAY POLICY NON-RENEWED IF NON-RENEW FLAG
     C*
     C     PMNRDT        IFNE      *ZEROS
     C     PMNRDT        ANDGT     PMRSDT
     C                   MOVE      *ON           *IN04
     C                   ELSE
     C                   MOVE      *OFF          *IN04
     C                   END
     C*
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     PMFIN         SCFIN
     C*
     C*  COVERAGE CODE MUST BE - VALID CODE, FIRE TYPE, & ACTIVE
     C*
     C                   Z-ADD     1             X                 2 0
     C     SCCC          LOOKUP    SC(X)                                  21
     C     *IN21         IFNE      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCC          XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     CVVAL         IFNE      'A'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     CVSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CVDESC        SCCCDS
     C*
     C*  ALL EDITS PASSED ON SCR01 GO TO AUTO COVERAGE MASTER AND LOAD
     C*
     C     AUKEY         CHAIN     PAU01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PAU01E
     C*
     C     *IN09         IFEQ      *OFF
     C                   MOVE      UA(2)         SCMOD2           10
     C                   END
     C                   MOVE      AUALSL        SCALSL
     C                   MOVE      AUSTXL        SCSTXL
     C                   MOVE      AUZONL        SCZONL
     C                   MOVE      AURIDL        SCRIDL
     C                   MOVE      AURTML        SCRTML
     C                   MOVE      AUASLL        SCASLL
     C                   MOVE      AUPISL        SCPISL
     C                   MOVE      AUSTXP        SCSTXP
     C                   MOVE      AUZONP        SCZONP
     C                   MOVE      AURIDP        SCRIDP
     C                   MOVE      AURTMP        SCRTMP
     C                   MOVE      AUASLP        SCASLP
     C                   MOVE      AUPDSL        SCPDSL
     C                   MOVE      AUSTXD        SCSTXD
     C                   MOVE      AUZOND        SCZOND
     C                   MOVE      AURIDD        SCRIDD
     C                   MOVE      AURTMD        SCRTMD
     C                   MOVE      AUASLD        SCASLD
     C                   MOVE      AUCMNR        SCCMNR
     C                   MOVE      AUREIN        SCREIN
     C                   MOVE      AUFR01        SCFR01
     C                   MOVE      AUFR02        SCFR02
     C                   MOVE      AUFR03        SCFR03
     C                   MOVE      AUFR04        SCFR04
     C                   MOVE      AUFR05        SCFR05
     C                   MOVE      AUFR06        SCFR06
     C                   MOVE      AUFR07        SCFR07
     C                   MOVE      AUFR08        SCFR08
     C                   MOVE      AUFR09        SCFR09
     C                   MOVE      AUFR10        SCFR10
     C                   MOVE      AUFR11        SCFR11
     C                   MOVE      AUFR12        SCFR12
     C                   MOVE      AUFR13        SCFR13
     C                   MOVE      AUFR14        SCFR14
     C                   MOVE      AUFR15        SCFR15
     C                   MOVE      AUFR16        SCFR16
     C                   MOVE      AUFR17        SCFR17
     C                   MOVE      AUFR18        SCFR18
     C                   MOVE      AUFR19        SCFR19
     C                   MOVE      AUFR20        SCFR20
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C     *IN09         IFEQ      *OFF
     C                   MOVE      UA(1)         SCMOD2
     C                   END
     C                   MOVE      *BLANK        SCALSL
     C                   MOVE      *BLANK        SCSTXL
     C                   MOVE      *BLANK        SCZONL
     C                   MOVE      *BLANK        SCRIDL
     C                   MOVE      *BLANK        SCRTML
     C                   MOVE      *BLANK        SCASLL
     C                   MOVE      *BLANK        SCPISL
     C                   MOVE      *BLANK        SCSTXP
     C                   MOVE      *BLANK        SCZONP
     C                   MOVE      *BLANK        SCRIDP
     C                   MOVE      *BLANK        SCRTMP
     C                   MOVE      *BLANK        SCASLP
     C                   MOVE      *BLANK        SCPDSL
     C                   MOVE      *BLANK        SCSTXD
     C                   MOVE      *BLANK        SCZOND
     C                   MOVE      *BLANK        SCRIDD
     C                   MOVE      *BLANK        SCRTMD
     C                   MOVE      *BLANK        SCASLD
     C                   Z-ADD     PMCMNR        SCCMNR
     C                   MOVE      *BLANK        SCREIN
     C                   MOVE      *BLANK        SCFR01
     C                   MOVE      *BLANK        SCFR02
     C                   MOVE      *BLANK        SCFR03
     C                   MOVE      *BLANK        SCFR04
     C                   MOVE      *BLANK        SCFR05
     C                   MOVE      *BLANK        SCFR06
     C                   MOVE      *BLANK        SCFR07
     C                   MOVE      *BLANK        SCFR08
     C                   MOVE      *BLANK        SCFR09
     C                   MOVE      *BLANK        SCFR10
     C                   MOVE      *BLANK        SCFR11
     C                   MOVE      *BLANK        SCFR12
     C                   MOVE      *BLANK        SCFR13
     C                   MOVE      *BLANK        SCFR14
     C                   MOVE      *BLANK        SCFR15
     C                   MOVE      *BLANK        SCFR16
     C                   MOVE      *BLANK        SCFR17
     C                   MOVE      *BLANK        SCFR18
     C                   MOVE      *BLANK        SCFR19
     C                   MOVE      *BLANK        SCFR20
     C                   MOVE      611           SCALSL
     C                   MOVE      9             SCSTXL
     C                   MOVE      0             SCZONL
     C                   MOVE      9             SCRIDL
     C                   MOVE      100           SCRTML
     C                   MOVE      194           SCASLL
     C     SCCC          IFEQ      'AU'
     C                   MOVE      615           SCPISL
     C                   MOVE      0             SCSTXP
     C                   MOVE      0             SCZONP
     C                   MOVE      9             SCRIDP
     C                   MOVE      100           SCRTMP
     C                   MOVE      193           SCASLP
     C                   END
     C                   MOVE      'T'           SCREIN
     C                   MOVE      0             SCCMNR
     C*
     C*          SET UP TO LOAD FORMS TABLE
     C*          ------------------------------------------------------
     C                   CLEAR                   SCFR
     C                   CLEAR                   FRM
     C                   CLEAR                   FRD
     C                   MOVE      SCCC          XXCC
     C     FRKY          CHAIN     PFR01                              50
     C                   Z-ADD     1             X
      *
     C                   Z-ADD     *DATE         XXDATE
     C     XDYY          IFGE      2000
     C                   Z-ADD     100           XYY3              3 0
     C                   ELSE
     C                   Z-ADD     0             XYY3
     C                   END
     C                   MOVE      XDYY2         XYY3
     C                   MOVE      XDMM          XXMMYY            5 0
     C                   MOVEL     XYY3          XXMMYY
     C*
     C*          READ FORMS MASTER FILE
     C*          ------------------------------------------------------
     C     *IN50         DOWEQ     *OFF
     C                   MOVE      FYY           FXYY
     C                   MOVE      FMM           FXMM
     C     XFEFDT        IFLE      XXMMYY
     C                   Z-ADD     1             V                 3 0
     C*
     C*             FORM DATE IS ACTIVE
     C*             ---------------------------------------------------
     C                   MOVE      FRFORM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FRFRM8            8
      *
     C     FRFRM8        LOOKUP    FRM(V)                                 50
     C     *IN50         IFEQ      *ON
     C*
     C*                IF ALREADY ON TABLE - UPDATE DATE
     C*                ------------------------------------------------
     C                   MOVE      FREFDT        FRD(V)
     C                   ELSE
     C                   Z-ADD     1             V                 3 0
     C*
     C*                NOT ON TABLE - ADD IT
     C*                ------------------------------------------------
     C     '        '    LOOKUP    FRM(V)                                 50
     C     *IN50         IFEQ      *ON
     C                   MOVE      FRFRM8        FRM(V)
     C                   MOVE      FREFDT        FRD(V)
     C                   ADD       1             X
     C                   END
     C                   END
     C                   END
     C*
     C*             PROCESS UNTIL TABLE FULL OR ALL FORMS READ
     C*             ---------------------------------------------------
     C     X             COMP      20                                 50
     C     *IN50         IFEQ      *OFF
     C     FRKY          READE     PFR01                                  50
     C                   END
     C                   ENDDO
     C*
     C*             LOAD FORMS TO SCREEN
     C*             ---------------------------------------------------
     C                   DO        20            V
     C                   MOVEL     FRM(V)        SCFR(V)
     C     FRM(V)        IFNE      *BLANKS
     C                   MOVEL     FRD(V)        XXYYMM            5 0
     C                   MOVEL     XXYYMM        XXYY              3 0
     C                   MOVE      XXYYMM        XXMM              2 0
     C                   MOVE      XXYY          XXMMYY            5 0
     C                   MOVEL     XXMM          XXMMYY
     C                   MOVE      XXMMYY        SCFR(V)
     C                   MOVE      XXMMYY        SCFR(V)
     C                   END
     C                   ENDDO
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR02B
     C*
     C*   AT LEAST ONE SUBLINE MUST BE ENTERED
     C*
     C     SCALSL        IFEQ      *BLANK
     C     SCPISL        ANDEQ     *BLANK
     C     SCPDSL        ANDEQ     *BLANK
     C                   MOVE      '1'           *IN19
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEA     '000'         *IN(41)
     C*
     C*   VALIDATE SUBLINES IF NOT BLANK
     C*
     C     SCALSL        IFNE      *BLANK
     C     SCALSL        IFNE      '611'
     C*** Begin add *** 10/12/01 **************************************
     C     SCALSL        ANDNE     '501'
     C     SCALSL        ANDNE     '502'
     C     SCALSL        ANDNE     '508'
     C     SCALSL        ANDNE     '509'
     C*** End add ***** 10/12/01 **************************************
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN41
     C                   END
     C*
     C     SCPISL        IFNE      *BLANK
     C     SCPISL        IFNE      '615'
     C     SCPISL        ANDNE     '635'
     C*** Begin add *** 10/12/01 **************************************
     C     SCPISL        ANDNE     '504'
     C     SCPISL        ANDNE     '505'
     C*** End add ***** 10/12/01 **************************************
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C     SCPDSL        IFNE      *BLANK
     C     SCPDSL        IFNE      '618'
     C*** Begin add *** 10/12/01 **************************************
     C     SCPDSL        ANDNE     '530'
     C     SCPDSL        ANDNE     '531'
     C*** End add ***** 10/12/01 **************************************
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN43
     C                   END
     C*
     C*   SET IND. 44 ON IF THERE IS NO LIAB AND NO PHY. DAM.
     C*
     C     *IN41         IFEQ      '1'
     C     *IN43         ANDEQ     '1'
     C                   MOVE      '1'           *IN44
     C                   ELSE
     C                   MOVE      '0'           *IN44
     C                   END
     C*
     C*   VALIDATE ONLY CODES THAT HAVE A SUBLINE
     C*
     C     *IN41         IFEQ      '0'                                          LIABILITY
     C*
     C*   VALIDATE STATE EXCEPTION CODE
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCSTXL        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCSTXL        IFNE      '1'
     C     SCSTXL        ANDNE     '2'
     C     SCSTXL        ANDNE     '3'
     C     SCSTXL        ANDNE     '4'
     C     SCSTXL        ANDNE     '5'
     C     SCSTXL        ANDNE     '6'
     C     SCSTXL        ANDNE     '9'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE RATE ID
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCRIDL        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCRIDL        IFNE      '1'
     C     SCRIDL        ANDNE     '9'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE RATE MOD
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCRTML        IFNE      0
     C*** End add ***** 09/10/01 **************************************
     C     SCRTML        IFLE      0
     C     SCRTML        ORGT      999
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE ANNUAL STATEMENT LINE OF BUSINESS
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCASLL        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCASLL        IFNE      '192'
     C     SCASLL        ANDNE     '194'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C                   END
     C*
     C     *IN42         IFEQ      '0'                                          NO-FAULT PIP
     C*
     C*   VALIDATE STATE EXCEPTION CODE
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCSTXP        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCSTXP        IFNE      *ZERO                                        |
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE RATE ID
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCRIDP        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCRIDP        IFNE      '1'
     C     SCRIDP        ANDNE     '9'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE RATE MOD
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCRTMP        IFNE      0
     C*** End add ***** 09/10/01 **************************************
     C     SCRTMP        IFLE      0
     C     SCRTMP        ORGT      999
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE ANNUAL STATEMENT LINE OF BUSINESS
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCASLP        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCASLP        IFNE      '191'
     C     SCASLP        ANDNE     '193'
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C                   END
     C*
     C     *IN43         IFEQ      '0'                                          PHY. DAMAGE
     C*
     C*   VALIDATE STATE EXCEPTION CODE
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCSTXD        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCSTXD        IFNE      *ZERO                                        |
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE RATE ID
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCRIDD        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCRIDD        IFNE      '1'
     C     SCRIDD        ANDNE     '9'
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE RATE MOD
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCRTMD        IFNE      0
     C*** End add ***** 09/10/01 **************************************
     C     SCRTMD        IFLE      0
     C     SCRTMD        ORGT      999
     C                   MOVE      '1'           *IN55
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C*   VALIDATE ANNUAL STATEMENT LINE OF BUSINESS
     C*
     C*** Begin add *** 09/10/01 **************************************
     C     SCASLD        IFNE      *BLANKS
     C*** End add ***** 09/10/01 **************************************
     C     SCASLD        IFNE      '211'
     C     SCASLD        ANDNE     '212'
     C                   MOVE      '1'           *IN57
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/10/01 **************************************
     C                   END
     C*** End add ***** 09/10/01 **************************************
     C*
     C                   END
     C*
     C*   RE INSURANCE MUST BE "F", "T", OR BLANK
     C*
     C     SCREIN        IFNE      'F'
     C     SCREIN        ANDNE     'T'
     C     SCREIN        ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COMMISSION RATE
     C*
     C*** Begin chg *** 09/10/01 **************************************
     C********** SCCMNR    IFLT 0.01
     C     SCCMNR        IFLT      0.00
     C*** End chg ***** 09/10/01 **************************************
     C     SCCMNR        ORGT      999.99
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*   ALL EDITS HAVE BEEN PASSED - WRITE OR UPDATE FILE
     C*
     C     AUKEY         CHAIN     PAU01                              40
     C*
     C                   MOVE      UPCO#         AUCO#
     C                   MOVE      SCPOL#        AUPOL#
     C                   MOVE      SCCC          AUCC
     C*
     C     SCALSL        IFEQ      *BLANK
     C                   MOVE      *BLANK        SCSTXL
     C                   MOVE      *BLANK        SCZONL
     C                   MOVE      *BLANK        SCRIDL
     C                   MOVE      *BLANK        SCRTML
     C                   MOVE      *BLANK        SCASLL
     C                   END
     C*
     C                   MOVE      SCALSL        AUALSL
     C                   MOVE      SCSTXL        AUSTXL
     C                   MOVE      SCZONL        AUZONL
     C                   MOVE      SCRIDL        AURIDL
     C                   MOVE      SCRTML        AURTML
     C                   MOVE      *ZEROS        AURTDL
     C                   MOVE      SCASLL        AUASLL
     C*
     C     SCPISL        IFEQ      *BLANK
     C                   MOVE      *BLANK        SCSTXP
     C                   MOVE      *BLANK        SCZONP
     C                   MOVE      *BLANK        SCRIDP
     C                   MOVE      *BLANK        SCRTMP
     C                   MOVE      *BLANK        SCASLP
     C                   END
     C*
     C                   MOVE      SCPISL        AUPISL
     C                   MOVE      SCSTXP        AUSTXP
     C                   MOVE      SCZONP        AUZONP
     C                   MOVE      SCRIDP        AURIDP
     C                   MOVE      SCRTMP        AURTMP
     C                   MOVE      *BLANKS       AURTDP
     C                   MOVE      SCASLP        AUASLP
     C*
     C     SCPDSL        IFEQ      *BLANK
     C                   MOVE      *BLANK        SCSTXD
     C                   MOVE      *BLANK        SCZOND
     C                   MOVE      *BLANK        SCRIDD
     C                   MOVE      *BLANK        SCRTMD
     C                   MOVE      *BLANK        SCASLD
     C                   END
     C*
     C                   MOVE      SCPDSL        AUPDSL
     C                   MOVE      SCSTXD        AUSTXD
     C                   MOVE      SCZOND        AUZOND
     C                   MOVE      SCRIDD        AURIDD
     C                   MOVE      SCRTMD        AURTMD
     C                   MOVE      *ZEROS        AURTDD
     C                   MOVE      SCASLD        AUASLD
     C*
     C                   MOVE      SCREIN        AUREIN
     C                   MOVE      SCCMNR        AUCMNR
     C                   MOVE      SCFR01        AUFR01
     C                   MOVE      SCFR02        AUFR02
     C                   MOVE      SCFR03        AUFR03
     C                   MOVE      SCFR04        AUFR04
     C                   MOVE      SCFR05        AUFR05
     C                   MOVE      SCFR06        AUFR06
     C                   MOVE      SCFR07        AUFR07
     C                   MOVE      SCFR08        AUFR08
     C                   MOVE      SCFR09        AUFR09
     C                   MOVE      SCFR10        AUFR10
     C                   MOVE      SCFR11        AUFR11
     C                   MOVE      SCFR12        AUFR12
     C                   MOVE      SCFR13        AUFR13
     C                   MOVE      SCFR14        AUFR14
     C                   MOVE      SCFR15        AUFR15
     C                   MOVE      SCFR16        AUFR16
     C                   MOVE      SCFR17        AUFR17
     C                   MOVE      SCFR18        AUFR18
     C                   MOVE      SCFR19        AUFR19
     C                   MOVE      SCFR20        AUFR20
     C                   MOVE      'A'           AUSTAT
     C                   MOVE      Q1USER        AUUSER
     C                   MOVE      Q1PGM         AUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C                   Z-ADD     DSTIME        AUTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PAU01
     C                   ELSE
     C                   WRITE     PAU01
     C                   END
     C*
     C     SCR02B        TAG
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   MOVE      *BLANK        SCITM#
     C                   MOVE      *BLANK        SCCLAS
     C*** Begin chg *** 09/10/01 **************************************
     C******************** MOVE *BLANK    SCTRNS
     C                   MOVE      'NB'          SCTRNS
     C*** End chg ***** 09/10/01 **************************************
     C                   Z-ADD     DSDATE        SCTRDT
     C                   MOVE      'N'           SCUPRV
     C                   GOTO      STRT
     C*
     C*   EDIT SCREEN 3
     C*
     C     SCR03T        TAG
     C*
     C*   IF ALL FIELDS ARE BLANK ON PROMPT SCREEN(SCR02) RETURN TO
     C*   SCR01 OR POLICY MASTER MAINTENANCE.
     C*
     C     SCITM#        IFEQ      *ZERO
     C     SCCLAS        ANDEQ     *BLANK
     C     SCTRNS        ANDEQ     *BLANK
     C*          SCTRDT    ANDEQ*ZERO
     C*
     C     *IN08         IFEQ      '1'                                          GO BACK TO POL.
     C                   GOTO      TERM                                         MAINT. PGM
     C                   ELSE
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  CLASS CODE MUST HAVE ENTRY IN PRIMARY RATING TABLE
     C*
     C     SCCLAS        IFNE      *BLANKS
     C                   MOVEL     SCCLAS        ZZKEY            10
     C                   Z-ADD     *HIVAL        ZZEFDT
     C     TBKEY2        SETGT     PPR01                              50
     C                   READP     PPR01                                  50
     C     *IN50         IFEQ      *OFF
     C                   MOVE      SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TEST1             7 0
     C                   MOVEL     PRKEY         PRCODE            6
     C                   Z-ADD     PREFDT        TEST2             7 0
     C                   END
     C     *IN50         IFEQ      *ON
     C     SCCLAS        ORNE      PRCODE
     C     TEST1         ORLT      TEST2
     C                   CALL      'C2190R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCCLAS
     C                   PARM                    QQRTC
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Chain to find out if record exists; if the record exists
     C* do not edit screen information.  User wants to see data that
     C* may have an inactive  codes (data entered prior to inactivating
     C* codes)                                             LJQ 8-07-89
     C*                                                    |
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT            7 0
     C     A1KEY         CHAIN     PA101                              40        |
     C     *IN40         IFNE      '0'                                          |
     C*
     C*   IF ITEM# IS ZERO SET INDICATOR 45 ON ('1')
     C*
     C     SCITM#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN45
     C                   ELSE
     C                   MOVE      '0'           *IN45
     C                   END
     C*
     C*  TRANSACTION CODE MUST BE VALID AND ACTIVE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     TTSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  TRANS. DATE MUST BE A VALID DATE & BETWEEN POL. FROM & TO DATE
     C*
     C     SCTRDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*          IF NEW BUSINESS, TRANS DATE MUST EQUAL POLICY EFF DATE
     C*          ------------------------------------------------------
     C     SCTRNS        IFEQ      'NB'
     C     XXTRDT        ANDNE     PMEFDT
     C                   MOVE      *ON           *IN67
     C                   GOTO      STRT
     C                   END
     C*
     C     PMCNDT        IFNE      0
     C                   Z-ADD     PMCNDT        XXTODT            7 0
     C                   ELSE
     C                   Z-ADD     PMEXDT        XXTODT
     C                   END
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C     XXTRDT        IFLT      PMEFDT
     C     XXTRDT        ORGT      XXTODT
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE UPDATE REVIEW FIELD TO BE "Y" OR "N"
     C*
     C     SCUPRV        IFNE      'Y'
     C     SCUPRV        ANDNE     'N'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C     ASKEY2        CHAIN     PAS01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      'Y'           SCUPRV
     C                   ELSE
     C                   EXCEPT    PAS01E
     C                   END
     C*
     C*
     C                   ELSE                                                   ELSE FOR BYPASS
     C*
     C*   IF ITEM# IS ZERO SET INDICATOR 45 ON ('1')
     C*
     C     SCITM#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN45
     C                   ELSE
     C                   MOVE      '0'           *IN45
     C                   END
     C*                                                    LJQ 8-07-89
     C*  GET TRANSACTION CODE TYPE                         |
     C*                                                    |
     C                   MOVE      *BLANKS       XXKEY                          |
     C                   MOVEL     SCTRNS        XXKEY                          |
     C     TBKEY         CHAIN     PTT01                              60        |
     C*                                                    |
     C                   END                                                    |Edit bypass
     C*
     C*   LOAD INFORMATION FROM THE VEHICLE SCHEDULE FILE
     C*
     C     VSKEY         CHAIN     PVS01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PVS01E
     C     *IN09         IFEQ      *OFF
     C                   MOVE      UA(2)         SCMOD5
     C                   END
     C*
     C                   MOVE      VSYR          SCYR
     C                   MOVE      VSMAKE        SCMAKE
     C                   MOVE      VSMODL        SCMODL
     C                   MOVE      VSBODY        SCBODY
     C                   MOVE      VSVIN         SCVIN
     C                   MOVE      VSAMNW        SCAMNW
     C                   MOVE      VSAMST        SCAMST
     C                   MOVE      VSEXP         SCEXP
     C                   MOVE      VSTERA        SCTERA
     C                   MOVE      VSCTY         SCCTY
     C                   MOVE      VSCNTY        SCCNTY
     C                   MOVE      VSST          SCST
     C                   MOVE      VSNU          SCNU
     C                   MOVE      VSRAD         SCRAD
     C                   MOVE      VSUSE         SCUSE
     C                   MOVE      VSGVW         SCGVW
     C                   MOVE      VSADI1        SCADI1
     C                   MOVE      VSADI2        SCADI2
     C                   MOVE      VSADI3        SCADI3
     C                   MOVE      VSADI4        SCADI4
     C                   MOVE      VSADI5        SCADI5
     C                   MOVE      VSADI6        SCADI6
     C                   MOVE      VSADI7        SCADI7
     C                   MOVE      VSADI8        SCADI8
     C                   MOVE      VSADI9        SCADI9
     C                   MOVE      VSADI0        SCADI0
     C*
     C                   ELSE
     C     SCITM#        IFNE      *ZERO
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C  N09              MOVE      UA(1)         SCMOD5
     C                   MOVE      *BLANK        SCYR
     C                   MOVE      *BLANK        SCMAKE
     C                   MOVE      *BLANK        SCMODL
     C                   MOVE      *BLANK        SCBODY
     C                   MOVE      *BLANK        SCVIN
     C                   MOVE      *BLANK        SCAMNW
     C                   MOVE      *BLANK        SCAMST
     C                   MOVE      *ZERO         SCEXP                          * TMN 4-9-90
     C                   MOVE      *BLANK        SCTERA
     C                   MOVE      *BLANK        SCCTY
     C                   MOVE      *BLANK        SCCNTY
     C                   MOVE      *BLANK        SCST
     C     SCPOL#        IFNE      *BLANKS
     C     PMKEY         CHAIN     PPM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PMCTY         SCCTY
     C                   MOVE      PMCNTY        SCCNTY
     C                   MOVE      PMST          SCST
     C                   END
     C                   END
     C                   MOVE      *BLANKS       SCNU
     C                   MOVE      PRRD          SCRAD
     C                   MOVE      PRBU          SCUSE
     C                   MOVE      PRGW          SCGVW
     C                   MOVE      *BLANK        SCADI1
     C                   MOVE      *BLANK        SCADI2
     C                   MOVE      *BLANK        SCADI3
     C                   MOVE      *BLANK        SCADI4
     C                   MOVE      *BLANK        SCADI5
     C                   MOVE      *BLANK        SCADI6
     C                   MOVE      *BLANK        SCADI7
     C                   MOVE      *BLANK        SCADI8
     C                   MOVE      *BLANK        SCADI9
     C                   MOVE      *BLANK        SCADI0
     C                   END
     C*
     C*   GET THE REQUESTED RECORDS FOR SCREEN 5, LOAD INFORMATION
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C     ASKY1         SETGT     PAS02
     C                   READP     PAS02                                  40
     C     ASSTAT        DOWEQ     'I'
     C     *IN40         ANDEQ     *OFF
     C                   READP     PAS02                                  40
     C                   END
     C     ASCO#         IFNE      UPCO#
     C     ASPOL#        ORNE      SCPOL#
     C     ASCC          ORNE      SCCC
     C     *IN40         OREQ      *ON
     C                   MOVE      *BLANKS       ASDEBI
     C                   ELSE
     C     ASLMID        IFEQ      '1'
     C                   SETOFF                                       66
     C                   ELSE
     C                   SETON                                        66
     C                   END
     C                   END
     C     A1KEY         CHAIN     PA101                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C*** Begin dlt *** 09/10/01 **************************************
     C*   VALIDATE THAT TRANS DATE IS EQUAL TO USER PARM PROCESS DATE
     C*   IF ADDING A RECORD OF COVERAGE
     C**********
     C********** XXTRDT    IFNE UPPRDT
     C**********           MOVE '1'       *IN36
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 09/10/01 **************************************
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN62
     C*
     C*   RECORD MUST EXIST IN INQUIRY MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   GET RECORD FOR LAST TRANS ACTION DATE AND LOAD SCREEN WITH
     C*   THAT RECORDS INFORMATION TO ADD TO FILE
     C*
     C     A1KY1         SETGT     PA102
     C                   MOVE      'I'           A1STAT
     C                   MOVE      '0'           *IN40
     C     A1STAT        DOWEQ     'I'
     C     *IN40         ANDNE     '1'
     C                   READP     PA102                                  40
      *
     C     *IN40         IFEQ      *OFF
     C     A1CO#         IFNE      UPCO#
     C     A1POL#        ORNE      SCPOL#
     C     A1CC          ORNE      SCCC
     C     A1ITM#        ORNE      SCITM#
     C     A1CLAS        ORNE      SCCLAS
     C                   MOVE      *ON           *IN40
     C                   END
     C                   END
      *
     C                   END
     C*
     C                   MOVE      UA(1)         SCMOD5
     C                   MOVE      *BLANK        A1SLBA
     C                   MOVE      *BLANK        A1MEDA
     C                   MOVE      *BLANK        A1UNMA
     C                   MOVE      *BLANK        A1PRDA
     C                   MOVE      *BLANK        A1PIPA
     C                   MOVE      *BLANK        A1ADLA
     C                   MOVE      *BLANK        A1COLA
     C                   MOVE      *BLANK        A1NCLA
     C                   MOVE      *BLANK        A1TOWA
     C                   MOVE      UMONTH        A1ACMY
     C                   MOVEL     UYEAR         A1ACMY
     C                   Z-ADD     PMNEND        A1END#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN62
     C                   EXCEPT    PA101E
     C                   MOVE      UA(2)         SCMOD5
     C                   END
     C*
     C*   LOAD SCREEN 5
     C*
     C     *IN40         IFEQ      '0'
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     A1END#        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      A1SLBA        SCSLBP
     C                   MOVE      A1MEDA        SCMEDP
     C                   MOVE      A1UNMA        SCUNMP
     C                   MOVE      A1PRDA        SCPRDP
     C                   MOVE      A1DEPD        SCDEPD
     C                   MOVE      A1PIPA        SCPIPP
     C                   MOVE      A1ADLA        SCADLP
     C                   MOVE      A1APLM        SCAPLM
     C                   MOVE      A1OTCC        SCOTCC
     C                   MOVE      A1COLC        SCCOLC
     C                   MOVE      A1DECD        SCDECD
     C                   MOVE      A1COLD        SCCOLD
     C                   MOVE      A1AGEC        SCAGEC
     C                   MOVE      A1COLA        SCCOLP
     C                   MOVE      A1NCLA        SCNCLP
     C                   MOVE      A1TOWA        SCTOWP
     C                   MOVE      A1LIBF        SCLIBF
     C                   MOVE      A1PIPF        SCPIPF
     C                   MOVE      A1PDF         SCPDF
     C                   MOVE      A1MEDF        SCMEDF
     C                   MOVE      A1UMF         SCUMF
     C                   MOVE      A1STAT        SCSTA5
     C                   MOVE      A1RTDL        SCRTDL
     C                   MOVE      A1RTDP        SCRTDP
     C                   MOVE      A1RTDD        SCRTDD
     C*
     C                   ELSE
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     A1END#        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      *BLANK        SCSLBP
     C                   MOVE      *BLANK        SCMEDP
     C                   MOVE      *BLANK        SCUNMP
     C                   MOVE      *BLANK        SCPRDP
     C                   MOVE      *BLANK        SCDEPD
     C                   MOVE      *BLANK        SCPIPP
     C                   MOVE      *BLANK        SCADLP
     C                   MOVE      *BLANK        SCAPLM
     C                   MOVE      *BLANK        SCOTCC
     C                   MOVE      *BLANK        SCCOLC
      *
     C     *IN43         IFEQ      *ON
     C                   MOVE      *BLANK        SCDECD
     C                   MOVE      *BLANK        SCCOLD
     C                   ELSE
     C                   MOVEL     DEKEY         SCDECD
     C                   MOVEL     DEKEY         SCCOLD
     C                   END
      *
     C                   MOVE      *BLANK        SCAGEC
     C                   MOVE      *BLANK        SCCOLP
     C                   MOVE      *BLANK        SCNCLP
     C                   MOVE      *BLANK        SCTOWP
     C                   MOVE      *BLANK        SCLIBF
     C                   MOVE      *BLANK        SCPIPF
     C                   MOVE      *BLANK        SCPDF
     C                   MOVE      *BLANK        SCMEDF
     C                   MOVE      *BLANK        SCUMF
     C     *IN41         IFEQ      *OFF
     C                   MOVE      'N'           SCLIBF
     C                   MOVE      'N'           SCMEDF
     C                   MOVE      'N'           SCUMF
     C                   END
     C     *IN42         IFEQ      *OFF
     C                   MOVE      'N'           SCPIPF
     C                   END
     C     *IN43         IFEQ      *OFF
     C                   MOVE      'N'           SCPDF
     C                   END
     C                   MOVE      'A'           SCSTA5
     C                   MOVE      *ZEROS        SCRTDL
     C                   MOVE      *ZEROS        SCRTDP
     C                   MOVE      *ZEROS        SCRTDD
     C                   END
      *
     C                   Z-ADD     PMACMY        DSACMY
     C                   MOVEL     DSACMM        SCACMM
     C                   Z-ADD     DSACYY        SCACYY
     C*** Begin add *** 08/25/01 **************************************
     C     PMACMY        IFNE      *ZERO
     C*** End add ***** 08/25/01 **************************************
     C     DSACCN        IFEQ      1
     C                   ADD       2000          SCACYY
     C                   ELSE
     C                   ADD       1900          SCACYY
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
      *
     C*
     C     SCUPRV        IFEQ      'N'
     C*
     C                   MOVE      L524(1)       SC524
     C                   Z-ADD     1             I5
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C*   GET THE REQUESTED RECORDS FOR SCREEN 4, LOAD INFORMATION
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C     ASKEY         CHAIN     PAS01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN61
     C*
     C*   RECORD MUST EXIST IN INQUIRY MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   GET RECORD FOR LAST TRANS ACTION DATE AND LOAD SCREEN WITH
     C*   THAT RECORDS INFORMATION TO ADD TO FILE
     C*
     C     ASKY1         SETGT     PAS02
     C                   MOVE      'I'           ASSTAT
     C                   MOVE      '0'           *IN40
     C     ASSTAT        DOWEQ     'I'
     C     *IN40         ANDNE     '1'
     C                   READP     PAS02                                  40
      *
     C     *IN40         IFEQ      *OFF
     C     ASCO#         IFNE      UPCO#
     C     ASPOL#        ORNE      SCPOL#
     C     ASCC          ORNE      SCCC
     C                   MOVE      *ON           *IN40
     C                   END
     C                   END
      *
     C                   END
     C*
     C                   MOVE      UA(1)         SCMOD4
     C                   Z-ADD     PMNEND        ASEND#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN61
     C                   EXCEPT    PAS01E
     C                   MOVE      UA(2)         SCMOD4
     C                   END
     C*
     C*   LOAD SCREEN 4
     C*
     C     *IN40         IFEQ      '0'
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     A1END#        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      ASLMID        SCLIMT
     C                   MOVE      ASLPP         SCLPP
     C                   MOVE      ASLPO         SCLPO
     C                   MOVE      ASMDPC        SCMDPC
     C                   MOVE      ASMPSB        SCMPSB
     C                   MOVE      ASUMPC        SCUMPC
     C                   MOVE      ASUPAO        SCUPAO
     C                   MOVE      ASUMPD        SCUMPD
     C                   MOVE      ASUMCV        SCUMCV
     C                   MOVE      ASLIB1        SCLIB1
     C                   MOVE      ASLIB2        SCLIB2
     C                   MOVE      ASLIB3        SCLIB3
     C                   MOVE      ASDEBI        SCDEBI
     C                   MOVE      ASPILM        SCPILM
     C                   MOVE      ASPIDD        SCPIDD
     C                   MOVE      ASPRTB        SCPRTB
     C                   MOVE      ASPIP         SCPIP
     C                   MOVE      ASAPIP        SCAPIP
     C                   MOVE      ASTOWL        SCTOWL
     C                   MOVE      ASCOM1        SCCOM1
     C                   MOVE      ASCOM2        SCCOM2
     C                   MOVE      ASCOM3        SCCOM3
     C                   MOVE      ASSPR1        SCSPR1
     C                   MOVE      ASSPR2        SCSPR2
     C                   MOVE      ASSPR3        SCSPR3
     C                   MOVE      ASCOL1        SCCOL1
     C                   MOVE      ASCOL2        SCCOL2
     C                   MOVE      ASCOL3        SCCOL3
     C                   MOVE      ASHBST        SCHBST
     C                   MOVE      ASHBCL        SCHBCL
     C                   MOVE      ASHBCT        SCHBCT
     C                   MOVE      ASEMST        SCEMST
     C                   MOVE      ASEMCL        SCEMCL
     C                   MOVE      ASEMP#        SCEMP#
     C                   MOVE      ASSTAT        SCSTA4
     C*
     C                   ELSE
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     A1END#        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      *BLANK        SCLIMT
     C                   MOVE      *BLANK        SCLPP
     C                   MOVE      *BLANK        SCLPO
     C                   MOVE      *BLANK        SCMDPC
     C                   MOVE      *BLANK        SCMPSB
     C                   MOVE      *BLANK        SCUMPC
     C                   MOVE      *BLANK        SCUPAO
     C                   MOVE      *BLANK        SCUMPD
     C                   MOVE      *BLANK        SCUMCV
     C                   MOVE      *BLANK        SCLIB1
     C                   MOVE      *BLANK        SCLIB2
     C                   MOVE      *BLANK        SCLIB3
     C                   MOVE      *BLANK        SCDEBI
     C                   MOVE      *BLANK        SCPILM
     C     *IN42         IFEQ      *ON
     C                   MOVE      *BLANK        SCPIDD
     C                   END
     C                   MOVE      *BLANK        SCPRTB
     C     *IN42         IFEQ      *OFF
     C                   MOVE      '7'           SCPIP
     C                   ELSE
     C                   MOVE      *BLANK        SCPIP
     C                   END
      *
     C                   MOVE      *BLANK        SCAPIP
     C                   MOVE      *BLANK        SCTOWL
     C                   MOVE      *BLANK        SCCOM1
     C                   MOVE      *BLANK        SCCOM2
     C                   MOVE      *BLANK        SCCOM3
     C                   MOVE      *BLANK        SCSPR1
     C                   MOVE      *BLANK        SCSPR2
     C                   MOVE      *BLANK        SCSPR3
     C                   MOVE      *BLANK        SCCOL1
     C                   MOVE      *BLANK        SCCOL2
     C                   MOVE      *BLANK        SCCOL3
     C                   MOVE      *BLANK        SCHBST
     C                   MOVE      *BLANK        SCHBCL
     C                   MOVE      *BLANK        SCHBCT
     C                   MOVE      *BLANK        SCEMST
     C                   MOVE      *BLANK        SCEMCL
     C                   MOVE      *BLANK        SCEMP#
     C                   MOVE      'A'           SCSTA4
     C                   MOVE      '1'           SCLIMT
     C                   MOVE      '9'           SCMDPC
     C                   MOVE      '01'          SCPILM
     C                   MOVE      '1'           SCPIDD
     C                   MOVE      '4'           SCPRTB
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*   EDIT SCREEN 4
     C*
     C     SCR04T        TAG
     C*
     C*   DONT VALIDATE IN INQUERY MODE
     C*
     C     *IN09         CABEQ     '1'           SCR04B
     C*
     C*   VALIDATE LIABILITY PART OF SCREEN
     C*
     C     *IN41         IFEQ      '0'
     C*
     C*   VALIDATE LIMIT ID
     C*
     C     SCLIMT        IFNE      '1'
     C     SCLIMT        ANDNE     '2'
     C     SCLIMT        ANDNE     '3'
     C     SCLIMT        ANDNE     '4'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE LIABILITY LIMIT PER PERSON
     C*
     C     SCLIMT        IFEQ      '1'
     C     SCLIMT        OREQ      '2'
     C     SCLPP         IFNE      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C     SCLPP         IFLE      *ZERO
     C                   MOVE      '1'           *IN77
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE OCCURANCE LIMIT
     C*
     C     SCLPO         IFLE      *ZERO
     C                   MOVE      '1'           *IN78
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE LIABILITY COVERAGE 1
     C*
     C     SCLIB1        IFNE      '1'
     C     SCLIB1        ANDNE     '2'
     C     SCLIB1        ANDNE     '3'
     C     SCLIB1        ANDNE     '4'
     C     SCLIB1        ANDNE     '5'
     C     SCLIB1        ANDNE     '6'
     C     SCLIB1        ANDNE     '7'
     C     SCLIB1        ANDNE     '8'
     C     SCLIB1        ANDNE     '9'
     C     SCLIB1        ANDNE     ' '
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE LIABILITY COVERAGE 2
     C*
     C     SCLIB2        IFNE      '1'
     C     SCLIB2        ANDNE     '2'
     C     SCLIB2        ANDNE     '3'
     C     SCLIB2        ANDNE     '4'
     C     SCLIB2        ANDNE     '5'
     C     SCLIB2        ANDNE     '6'
     C     SCLIB2        ANDNE     '7'
     C     SCLIB2        ANDNE     '8'
     C     SCLIB2        ANDNE     '9'
     C     SCLIB2        ANDNE     ' '
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE LIABILITY COVERAGE 3
     C*
     C     SCLIB3        IFNE      '1'
     C     SCLIB3        ANDNE     '2'
     C     SCLIB3        ANDNE     '3'
     C     SCLIB3        ANDNE     '4'
     C     SCLIB3        ANDNE     '5'
     C     SCLIB3        ANDNE     '6'
     C     SCLIB3        ANDNE     '7'
     C     SCLIB3        ANDNE     '8'
     C     SCLIB3        ANDNE     '9'
     C     SCLIB3        ANDNE     ' '
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE BI DED CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDEBI        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE MED PAY CODE
     C*
     C     SCMDPC        IFNE      '1'
     C     SCMDPC        ANDNE     '2'
     C     SCMDPC        ANDNE     '3'
     C     SCMDPC        ANDNE     '4'
     C     SCMDPC        ANDNE     '5'
     C     SCMDPC        ANDNE     '6'
     C     SCMDPC        ANDNE     '9'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE MED PAY CODE
     C*
     C     SCMPSB        IFNE      '1'
     C     SCMPSB        ANDNE     '2'
     C     SCMPSB        ANDNE     '3'
     C     SCMPSB        ANDNE     '4'
     C     SCMPSB        ANDNE     '5'
     C     SCMPSB        ANDNE     '6'
     C     SCMPSB        ANDNE     '7'
     C     SCMPSB        ANDNE     '8'
     C     SCMPSB        ANDNE     '9'
     C     SCMPSB        ANDNE     ' '
     C                   MOVE      '1'           *IN59
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE UM ID CODE 1-SINGLE LIMIT POLICY 2-SPLIT LIMIT POLICY
     C*
     C     SCUMPD        IFNE      '1'
     C     SCUMPD        ANDNE     '2'
     C     SCUMPD        ANDNE     ' '
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE UM LIMIT PER CLAIM
     C*
     C     SCUMPD        IFEQ      '1'
     C     SCUMPC        ANDNE     *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C     SCUMPD        IFEQ      '2'
     C     SCUMPC        ANDEQ     *ZERO
     C                   MOVE      '1'           *IN79
     C                   GOTO      STRT
     C                   END
     C*
     C     SCUMPD        IFEQ      ' '
     C     SCUMPC        ANDNE     *ZERO
     C                   MOVE      '1'           *IN82
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE UM ACCIDENT/OCCURANCE LIMIT
     C*
     C     SCUMPD        IFEQ      ' '
     C     SCUPAO        IFNE      *ZERO
     C                   MOVE      '1'           *IN83
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C     SCUPAO        IFLE      *ZERO
     C                   MOVE      '1'           *IN81
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE UM COVERED AUTO SYMBOL
     C*
     C     SCUMCV        IFNE      '1'
     C     SCUMCV        ANDNE     '2'
     C     SCUMCV        ANDNE     '3'
     C     SCUMCV        ANDNE     '4'
     C     SCUMCV        ANDNE     '5'
     C     SCUMCV        ANDNE     '6'
     C     SCUMCV        ANDNE     '7'
     C     SCUMCV        ANDNE     '8'
     C     SCUMCV        ANDNE     '9'
     C     SCUMCV        ANDNE     ' '
     C                   MOVE      '1'           *IN73
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    LIAB PART
     C*
     C*   VALIDATE PIP PART
     C*
     C     *IN42         IFEQ      '0'
     C*
     C*   VALIDATE PIP LIMIT CD
     C*
     C     SCPILM        IFNE      '01'
     C     SCPILM        ANDNE     '02'
     C     SCPILM        ANDNE     '03'
     C     SCPILM        ANDNE     '04'
     C     SCPILM        ANDNE     '05'
     C     SCPILM        ANDNE     '09'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE PIP DED CD
     C*
     C     SCPILM        IFNE      *BLANK
     C     SCPIDD        IFNE      '1'
     C     SCPIDD        ANDNE     '2'
     C     SCPIDD        ANDNE     '3'
     C     SCPIDD        ANDNE     '4'
     C     SCPIDD        ANDNE     '6'
     C     SCPIDD        ANDNE     '7'
     C     SCPIDD        ANDNE     '8'
     C     SCPIDD        ANDNE     '0'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE PIP RATE BASIS
     C*
     C     SCPRTB        IFNE      '0'
     C     SCPRTB        ANDNE     '1'
     C     SCPRTB        ANDNE     '2'
     C     SCPRTB        ANDNE     '3'
     C     SCPRTB        ANDNE     '4'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE PIP COVERAGE CODE
     C*
     C     SCPIP         IFNE      ' '
     C     SCPIP         ANDNE     '1'
     C     SCPIP         ANDNE     '2'
     C     SCPIP         ANDNE     '3'
     C     SCPIP         ANDNE     '4'
     C     SCPIP         ANDNE     '5'
     C     SCPIP         ANDNE     '6'
     C     SCPIP         ANDNE     '7'
     C     SCPIP         ANDNE     '8'
     C     SCPIP         ANDNE     '9'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE ADDITIONAL PIP COVERAGE CODE
     C*
     C     SCAPIP        IFNE      ' '
     C     SCAPIP        ANDNE     '1'
     C     SCAPIP        ANDNE     '2'
     C     SCAPIP        ANDNE     '3'
     C     SCAPIP        ANDNE     '4'
     C     SCAPIP        ANDNE     '5'
     C     SCAPIP        ANDNE     '6'
     C     SCAPIP        ANDNE     '7'
     C     SCAPIP        ANDNE     '8'
     C     SCAPIP        ANDNE     '9'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    PIP PART
     C*
     C*   VALIDATE PHYSICAL DAMAGE PART
     C*
     C     *IN43         IFEQ      '0'
     C*
     C*   TOWING AND LABOR MUST BE "3", "7", OR "0"
     C*
     C     SCTOWL        IFNE      '3'
     C     SCTOWL        ANDNE     '7'
     C     SCTOWL        ANDNE     '0'
     C     SCTOWL        ANDNE     ' '
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COMPREHENSIVE 1
     C*
     C     SCCOM1        IFNE      '1'
     C     SCCOM1        ANDNE     '2'
     C     SCCOM1        ANDNE     '3'
     C     SCCOM1        ANDNE     '4'
     C     SCCOM1        ANDNE     '5'
     C     SCCOM1        ANDNE     '6'
     C     SCCOM1        ANDNE     '7'
     C     SCCOM1        ANDNE     '8'
     C     SCCOM1        ANDNE     '9'
     C     SCCOM1        ANDNE     ' '
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COMPREHENSIVE 2
     C*
     C     SCCOM2        IFNE      '1'
     C     SCCOM2        ANDNE     '2'
     C     SCCOM2        ANDNE     '3'
     C     SCCOM2        ANDNE     '4'
     C     SCCOM2        ANDNE     '5'
     C     SCCOM2        ANDNE     '6'
     C     SCCOM2        ANDNE     '7'
     C     SCCOM2        ANDNE     '8'
     C     SCCOM2        ANDNE     '9'
     C     SCCOM2        ANDNE     ' '
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COMPREHENSIVE 3
     C*
     C     SCCOM3        IFNE      '1'
     C     SCCOM3        ANDNE     '2'
     C     SCCOM3        ANDNE     '3'
     C     SCCOM3        ANDNE     '4'
     C     SCCOM3        ANDNE     '5'
     C     SCCOM3        ANDNE     '6'
     C     SCCOM3        ANDNE     '7'
     C     SCCOM3        ANDNE     '8'
     C     SCCOM3        ANDNE     '9'
     C     SCCOM3        ANDNE     ' '
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SPECIFIC PERIL 1
     C*
     C     SCSPR1        IFNE      '1'
     C     SCSPR1        ANDNE     '2'
     C     SCSPR1        ANDNE     '3'
     C     SCSPR1        ANDNE     '4'
     C     SCSPR1        ANDNE     '5'
     C     SCSPR1        ANDNE     '6'
     C     SCSPR1        ANDNE     '7'
     C     SCSPR1        ANDNE     '8'
     C     SCSPR1        ANDNE     '9'
     C     SCSPR1        ANDNE     ' '
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SPECIFIC PERIL 2
     C*
     C     SCSPR2        IFNE      '1'
     C     SCSPR2        ANDNE     '2'
     C     SCSPR2        ANDNE     '3'
     C     SCSPR2        ANDNE     '4'
     C     SCSPR2        ANDNE     '5'
     C     SCSPR2        ANDNE     '6'
     C     SCSPR2        ANDNE     '7'
     C     SCSPR2        ANDNE     '8'
     C     SCSPR2        ANDNE     '9'
     C     SCSPR2        ANDNE     ' '
     C                   MOVE      '1'           *IN47
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SPECIFIC PERIL 3
     C*
     C     SCSPR3        IFNE      '1'
     C     SCSPR3        ANDNE     '2'
     C     SCSPR3        ANDNE     '3'
     C     SCSPR3        ANDNE     '4'
     C     SCSPR3        ANDNE     '5'
     C     SCSPR3        ANDNE     '6'
     C     SCSPR3        ANDNE     '7'
     C     SCSPR3        ANDNE     '8'
     C     SCSPR3        ANDNE     '9'
     C     SCSPR3        ANDNE     ' '
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COLLISION 1
     C*
     C     SCCOL1        IFNE      '1'
     C     SCCOL1        ANDNE     '2'
     C     SCCOL1        ANDNE     '3'
     C     SCCOL1        ANDNE     '4'
     C     SCCOL1        ANDNE     '5'
     C     SCCOL1        ANDNE     '6'
     C     SCCOL1        ANDNE     '7'
     C     SCCOL1        ANDNE     '8'
     C     SCCOL1        ANDNE     '9'
     C     SCCOL1        ANDNE     ' '
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COLLISION 2
     C*
     C     SCCOL2        IFNE      '1'
     C     SCCOL2        ANDNE     '2'
     C     SCCOL2        ANDNE     '3'
     C     SCCOL2        ANDNE     '4'
     C     SCCOL2        ANDNE     '5'
     C     SCCOL2        ANDNE     '6'
     C     SCCOL2        ANDNE     '7'
     C     SCCOL2        ANDNE     '8'
     C     SCCOL2        ANDNE     '9'
     C     SCCOL2        ANDNE     ' '
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COLLISION 3
     C*
     C     SCCOL3        IFNE      '1'
     C     SCCOL3        ANDNE     '2'
     C     SCCOL3        ANDNE     '3'
     C     SCCOL3        ANDNE     '4'
     C     SCCOL3        ANDNE     '5'
     C     SCCOL3        ANDNE     '6'
     C     SCCOL3        ANDNE     '7'
     C     SCCOL3        ANDNE     '8'
     C     SCCOL3        ANDNE     '9'
     C     SCCOL3        ANDNE     ' '
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   VALIDATE HIRED/BORROWED AND EMPLOYER PARTS IF LIAB OR PD
     C*
     C     *IN44         IFEQ      '0'
     C*
     C*   MUST HAVE A VALID STATE CODE FOR HIRED/BORROWED
     C*
     C     SCHBST        IFNE      *BLANK
     C*
     C                   Z-ADD     1             X                 2 0
     C     SCHBST        LOOKUP    ST(X)                                  50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN53
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE CLASS CODE FOR HIRED/BORROWED
     C*
     C     SCHBCL        IFNE      '6611'
     C     SCHBCL        ANDNE     '6614'
     C     SCHBCL        ANDNE     '6619'
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   MUST HAVE A VALID STATE CODE FOR EMPLOYER
     C*
     C     SCEMST        IFNE      *BLANK
     C*
     C                   Z-ADD     1             X                 2 0
     C     SCEMST        LOOKUP    ST(X)                                  50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN56
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE CLASS CODE FOR EMPLOYER
     C*
     C     SCEMCL        IFNE      '6670'
     C     SCEMCL        ANDNE     '6671'
     C     SCEMCL        ANDNE     '6672'
     C     SCEMCL        ANDNE     '7000'
     C     SCEMCL        ANDNE     '6601'                                       TMN 04/26/91
     C     SCEMCL        ANDNE     '6602'                                       |
     C     SCEMCL        ANDNE     '6603'                                       |
     C     SCEMCL        ANDNE     '6604'                                       |
     C     SCEMCL        ANDNE     '6605'                                       |
     C                   MOVE      '1'           *IN57
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE NUMBER OF EMPLOYEES CODE
     C*
     C     SCEMP#        IFNE      '1'
     C     SCEMP#        ANDNE     '2'
     C     SCEMP#        ANDNE     '3'
     C                   MOVE      '1'           *IN58
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END                                                    LIAB. & PD
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     ASKEY         CHAIN     PAS01                              40
     C                   MOVE      UPCO#         ASCO#
     C                   MOVE      SCPOL#        ASPOL#
     C                   MOVE      SCCC          ASCC
     C                   MOVE      SCTRNS        ASTRNS
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ASTRDT
     C                   MOVE      SCLIMT        ASLMID
     C                   MOVE      SCLPP         ASLPP
     C                   MOVE      SCLPO         ASLPO
     C                   MOVE      SCMDPC        ASMDPC
     C                   MOVE      SCMPSB        ASMPSB
     C                   MOVE      SCUMPC        ASUMPC
     C                   MOVE      SCUPAO        ASUPAO
     C                   MOVE      SCUMPD        ASUMPD
     C                   MOVE      SCUMCV        ASUMCV
     C                   MOVE      SCLIB1        ASLIB1
     C                   MOVE      SCLIB2        ASLIB2
     C                   MOVE      SCLIB3        ASLIB3
     C                   MOVE      SCDEBI        ASDEBI
     C                   MOVE      SCPILM        ASPILM
     C                   MOVE      SCPIDD        ASPIDD
     C                   MOVE      SCPRTB        ASPRTB
     C                   MOVE      SCPIP         ASPIP
     C                   MOVE      SCAPIP        ASAPIP
     C                   MOVE      SCTOWL        ASTOWL
     C                   MOVE      SCCOM1        ASCOM1
     C                   MOVE      SCCOM2        ASCOM2
     C                   MOVE      SCCOM3        ASCOM3
     C                   MOVE      SCSPR1        ASSPR1
     C                   MOVE      SCSPR2        ASSPR2
     C                   MOVE      SCSPR3        ASSPR3
     C                   MOVE      SCCOL1        ASCOL1
     C                   MOVE      SCCOL2        ASCOL2
     C                   MOVE      SCCOL3        ASCOL3
     C                   MOVE      SCHBST        ASHBST
     C                   MOVE      SCHBCL        ASHBCL
     C                   MOVE      SCHBCT        ASHBCT
     C                   MOVE      SCEMST        ASEMST
     C                   MOVE      SCEMCL        ASEMCL
     C                   MOVE      SCEMP#        ASEMP#
     C                   MOVE      SCEND#        ASEND#
     C                   MOVE      SCSTA4        ASSTAT
     C                   MOVE      Q1USER        ASUSER
     C                   MOVE      Q1PGM         ASPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ASDT
     C                   Z-ADD     DSTIME        ASTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PAS01
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ASRCDT
     C                   MOVE      'A'           ASSTAT                         ## LJD 11/16
     C                   Z-ADD     *ZEROS        ASBUL3                         ## LJD 11/16
     C                   WRITE     PAS01
     C     ASLMID        IFEQ      '1'
     C                   SETOFF                                       66
     C                   ELSE
     C                   SETON                                        66
     C                   END
     C                   END
     C*
     C     SCR04B        TAG
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   EDIT SCREEN 5
     C*
     C     SCR05T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR05B
     C*
     C*   DON'T VALIDATE VEHICLE INFOMATION IF VEH# IS ZERO
     C*
     C     *IN45         IFEQ      '0'
     C*
     C*   MUST HAVE A VALID STATE CODE
     C*
     C                   Z-ADD     1             X                 2 0
     C     SCST          LOOKUP    ST(X)                                  50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*   EXPOSURE CANNOT BE ZERO WHEN CLASS CODE IS 6614  *
     C*
     C     SCCLAS        IFEQ      '  6614'
     C     SCEXP         ANDEQ     *ZERO
     C                   MOVE      '1'           *IN18
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE TERRITORY CODE
     C*
     C*** Begin dlt *** 08/25/01 **************************************
     C********** SCCLAS    IFNE '  6614'
     C********** SCCLAS    OREQ '  6614'
     C********** SCTERA    ANDNE*BLANK
     C*** End dlt ***** 08/25/01 **************************************
     C*** Begin add *** 08/25/01 **************************************
     C     SCTERA        IFNE      *BLANKS
     C*** End add ***** 08/25/01 **************************************
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERA        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0733R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VAILDATE NEW / USED FIELD
     C*
     C     SCNU          IFNE      'N'
     C     SCNU          ANDNE     'U'
     C*** Begin add *** 08/25/01 **************************************
     C     SCNU          ANDNE     *BLANK
     C*** End add ***** 08/25/01 **************************************
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE RADIUS FIELD
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCRAD         IFNE      *BLANK
     C*** End add ***** 08/25/01 **************************************
     C     SCCLAS        IFEQ      '7391'
     C     SCCLAS        OREQ      '7398'
     C     SCRAD         SCAN      'T'           XXSCAN                   60
     C                   ELSE
     C     SCRAD         SCAN      'LID'         XXSCAN                   60
     C                   END
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN49
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   VAILDATE USE FIELD
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCUSE         IFNE      *BLANK
     C*** End add ***** 08/25/01 **************************************
     C     SCCLAS        IFEQ      '7391'
     C     SCCLAS        OREQ      '7398'
     C     SCUSE         SCAN      'P'           XXSCAN            3 0    60
     C                   ELSE
     C*** Begin chg *** 08/25/01 **************************************
     C********** SCUSE     SCAN 'CSR'     XXSCAN         60
     C     SCUSE         SCAN      'CSRA'        XXSCAN                   60
     C*** End chg ***** 08/25/01 **************************************
     C                   END
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN51
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   VAILDATE COUNTY CODE
     C*
     C                   MOVEL     SCCNTY        XXKEY
     C     TBKEY         CHAIN     PCY01                              55
     C     *IN55         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C     CYSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN81
     C                   GOTO      STRT
     C                   END
     C*
     C*   VAILDATE GVW FIELD
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCGVW         IFNE      *BLANK
     C*** End add ***** 08/25/01 **************************************
     C     SCCLAS        IFEQ      '7391'
     C     SCCLAS        OREQ      '7398'
     C     SCGVW         SCAN      'P'           XXSCAN                   60
     C                   ELSE
     C     SCGVW         SCAN      'LMHET'       XXSCAN                   60
     C                   END
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN52
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   VALIDATE ADDITIONAL INTERESTS
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C     *BLANK        LOOKUP    AI(X)                              50
     C     *IN50         IFEQ      '1'
     C     X             ADD       89            Z                 2 0
     C                   MOVEL     AI(X)         XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(Z)
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C     AISTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN(Z)
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     10            X
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C                   END                                                     *IN45 = 0
     C*
     C*   VALIDATE DED CODES FOR LIB IF *IN41 = 0
     C*
     C     *IN41         IFEQ      '0'
     C*
     C*   VALIDATE VEHICLE COVERAGE FLAG FOR LIABILITY
     C*
     C     SCLIBF        IFNE      'Y'
     C     SCLIBF        ANDNE     'N'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE IF FLAG IS YES
     C*
     C     SCLIBF        IFEQ      'Y'
     C*
     C     SCDEPD        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDEPD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*          VALIDATE MEDICAL COVERAGE FLAG
     C*          ------------------------------------------------------
     C     SCMEDF        IFNE      'Y'
     C     SCMEDF        ANDNE     'N'
     C                   SETON                                        63
     C                   GOTO      STRT
     C                   END
     C*
     C*          VALIDATE UNINSURED MOTORIST FLAG
     C*          ------------------------------------------------------
     C     SCUMF         IFNE      'Y'
     C     SCUMF         ANDNE     'N'
     C                   SETON                                        64
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   VALIDATE PIP COVERAGE FLAG  *IN42 = 0
     C*
     C     *IN42         IFEQ      '0'
     C*
     C*   VALIDATE VEHICLE COVERAGE FLAG FOR PIP
     C*
     C     SCPIPF        IFNE      'Y'
     C     SCPIPF        ANDNE     'N'
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   VALIDATE DED CODE FOR PD IF *IN43 = 0
     C*
     C     *IN43         IFEQ      '0'
     C*
     C*   VALIDATE VEHICLE COVERAGE FLAG FOR PD
     C*
     C     SCPDF         IFNE      'Y'
     C     SCPDF         ANDNE     'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE IF FLAG IS YES
     C*
     C     SCPDF         IFEQ      'Y'
     C*
     C*   VALIDATE OTC COV CODE
     C*
     C     SCOTCC        IFEQ      *BLANKS
     C     SCDECD        IFNE      *BLANKS
     C                   MOVE      '1'           *IN82
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCOTCC        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0790R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE DED CODE
     C*
     C     SCOTCC        IFNE      *BLANKS
     C     SCDECD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN82
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDECD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*
     C*   VALIDATE COLL COV CODE
     C*
     C*
     C     SCCOLC        IFEQ      *BLANKS
     C     SCCOLD        IFNE      *BLANKS
     C                   MOVE      '1'           *IN84
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCOLC        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0791R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*    VALIDATE DED CODE
     C*
     C     SCCOLC        IFNE      *BLANKS
     C     SCCOLD        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN84
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOLD        XXKEY
     C     TBKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN47
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*   AGE CODE MUST BE "1" THRU "6"
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCAGEC        IFNE      *BLANK
     C*** End add ***** 08/25/01 **************************************
     C     SCAGEC        IFNE      '1'
     C     SCAGEC        ANDNE     '2'
     C     SCAGEC        ANDNE     '3'
     C     SCAGEC        ANDNE     '4'
     C     SCAGEC        ANDNE     '5'
     C     SCAGEC        ANDNE     '6'
     C*
     C     SCCLAS        IFNE      '  6614'                                     * TMN 4-6-90
     C     SCCLAS        OREQ      '  6614'                                     *
     C     SCAGEC        ANDNE     *BLANK                                       *
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   VAL RATE MUST BE "1" THRU "9"
     C*
     C                   END
     C*
     C                   END
     C*
     C*   MONTH/YEAR MUST BE VALID
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCACMY        IFNE      *ZERO
     C*** End add ***** 08/25/01 **************************************
     C                   MOVEL     SCACMY        XXMNTH            2 0
     C                   MOVE      SCACMY        XXYEAR            4 0
     C     XXMNTH        IFLT      1
     C     XXMNTH        ORGT      12
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C     XXYEAR        IFLT      1
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   VALIDATE RATE DEP
     C*
     C     SCLIBF        IFEQ      'Y'
     C*** Begin chg *** 08/25/01 **************************************
     C********** SCRTDL    IFLE 0
     C     SCRTDL        IFLT      0
     C*** End chg ***** 08/25/01 **************************************
     C     SCRTDL        ORGT      999
     C                   MOVE      '1'           *IN15
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE RATE DEP
     C*
     C     SCPIPF        IFEQ      'Y'
     C*** Begin chg *** 08/25/01 **************************************
     C********** SCRTDP    IFLE 0
     C     SCRTDP        IFLT      0
     C*** End chg ***** 08/25/01 **************************************
     C     SCRTDP        ORGT      999
     C                   MOVE      '1'           *IN16
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE RATE DEP
     C*
     C     SCPDF         IFEQ      'Y'
     C*** Begin chg *** 08/25/01 **************************************
     C********** SCRTDD    IFLE 0
     C     SCRTDD        IFLT      0
     C*** End chg ***** 08/25/01 **************************************
     C     SCRTDD        ORGT      999
     C                   MOVE      '1'           *IN17
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ALL EDITS HAVE BEEN PASSED, UPDATE OR WRITE RECORD
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     A1KEY         CHAIN     PA101                              40
     C*
     C*   CLEAR FIELDS IF COVERAGE FOR VEHICLE IS NOT FLAGED
     C*
     C     *IN41         IFEQ      '0'
     C     SCLIBF        ANDEQ     'N'
     C                   MOVE      *BLANK        SCSLBP
     C                   MOVE      *BLANK        SCDEPD
     C                   MOVE      *BLANK        SCPRDP
     C                   MOVE      *BLANK        SCMEDP
     C                   MOVE      *BLANK        SCUNMP
     C                   END
     C*
     C     *IN42         IFEQ      '0'
     C     SCPIPF        ANDEQ     'N'
     C                   MOVE      *BLANK        SCPIPP
     C                   MOVE      *BLANK        SCADLP
     C                   MOVE      *BLANK        SCAPLM
     C                   END
     C*
     C     *IN43         IFEQ      '0'
     C     SCPDF         ANDEQ     'N'
     C                   MOVE      *BLANK        SCOTCC
     C                   MOVE      *BLANK        SCCOLC
     C                   MOVE      *BLANK        SCDECD
     C                   MOVE      *BLANK        SCCOLD
     C                   MOVE      *BLANK        SCAGEC
     C                   MOVE      *BLANK        SCNCLP
     C                   MOVE      *BLANK        SCCOLP
     C                   MOVE      *BLANK        SCTOWP
     C                   END
     C*
     C                   MOVE      UPCO#         A1CO#
     C                   MOVE      SCPOL#        A1POL#
     C                   MOVE      SCCC          A1CC
     C                   MOVE      SCITM#        A1ITM#
     C                   MOVE      SCCLAS        A1CLAS
     C                   MOVE      SCTRNS        A1TRNS
      *
     C                   CLEAR                   DSACMY
     C                   MOVE      SCACYY        DSACYY
     C                   MOVEL     SCACMM        DSACMM
     C     SCACYY        IFGT      1999
     C                   MOVEL     1             DSACCN
     C                   ELSE
     C                   MOVEL     0             DSACCN
     C                   END
     C                   Z-ADD     DSACMY        A1ACMY
      *
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A1TRDT
     C                   MOVE      SCEND#        A1END#
     C                   MOVE      SCSLBP        A1SLBA
     C                   MOVE      SCMEDP        A1MEDA
     C                   MOVE      SCUNMP        A1UNMA
     C                   MOVE      SCPRDP        A1PRDA
     C     SCLIBF        IFEQ      'Y'
     C                   MOVE      ASDEBI        A1DEBI
     C                   ELSE
     C                   MOVE      *BLANKS       A1DEBI
     C                   END
     C                   MOVE      SCDEPD        A1DEPD
     C                   MOVE      SCPIPP        A1PIPA
     C                   MOVE      SCADLP        A1ADLA
     C                   MOVE      SCAPLM        A1APLM
     C                   MOVE      SCOTCC        A1OTCC
     C                   MOVE      SCCOLC        A1COLC
     C                   MOVE      SCDECD        A1DECD
     C                   MOVE      SCCOLD        A1COLD
     C                   MOVE      SCAGEC        A1AGEC
     C     *IN43         IFEQ      '0'
     C     SCPDF         ANDEQ     'N'
     C                   MOVE      *BLANK        A1VLRU
     C                   ELSE
     C                   MOVE      '9'           A1VLRU
     C                   END
     C                   MOVE      SCCOLP        A1COLA
     C                   MOVE      SCNCLP        A1NCLA
     C                   MOVE      SCTOWP        A1TOWA
     C                   MOVE      SCLIBF        A1LIBF
     C                   MOVE      SCPIPF        A1PIPF
     C                   MOVE      SCPDF         A1PDF
     C                   MOVE      SCMEDF        A1MEDF
     C                   MOVE      SCUMF         A1UMF
     C                   MOVE      SCSTA5        A1STAT
     C                   MOVE      Q1USER        A1USER
     C                   MOVE      Q1PGM         A1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A1DT
     C                   Z-ADD     DSTIME        A1TIME
     C                   Z-ADD     SCRTDL        A1RTDL
     C                   Z-ADD     SCRTDP        A1RTDP
     C                   Z-ADD     SCRTDD        A1RTDD
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     A1SLBA        S6SLBP
     C                   Z-ADD     A1MEDA        S6MEDP
     C                   Z-ADD     A1UNMA        S6UNMP
     C                   Z-ADD     A1PRDA        S6PRDP
     C                   Z-ADD     A1PIPA        S6PIPP
     C                   Z-ADD     A1ADLA        S6ADLP
     C                   Z-ADD     A1COLA        S6COLP
     C                   Z-ADD     A1NCLA        S6NCLP
     C                   Z-ADD     A1TOWA        S6TOWP
     C                   EXFMT     SCR06
     C                   Z-ADD     A1TRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQWORK            8 0
     C                   MOVE      QQWORK        QQSTDT            8
     C                   Z-ADD     PMEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQWORK            8 0
     C                   MOVE      QQWORK        QQNDDT            8
     C     S6YN1         IFEQ      'R'
     C     S6YN1         OREQ      'N'
     C                   Z-ADD     A1SLBA        A1SLBP
     C                   END
     C     S6YN1         IFEQ      'Y'
     C                   Z-ADD     A1SLBA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1SLBP
     C                   END
     C     S6YN2         IFEQ      'R'
     C     S6YN2         OREQ      'N'
     C                   Z-ADD     A1MEDA        A1MEDP
     C                   END
     C     S6YN2         IFEQ      'Y'
     C                   Z-ADD     A1MEDA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1MEDP
     C                   END
     C     S6YN3         IFEQ      'R'
     C     S6YN3         OREQ      'N'
     C                   Z-ADD     A1UNMA        A1UNMP
     C                   END
     C     S6YN3         IFEQ      'Y'
     C                   Z-ADD     A1UNMA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1UNMP
     C                   END
     C     S6YN4         IFEQ      'R'
     C     S6YN4         OREQ      'N'
     C                   Z-ADD     A1PRDA        A1PRDP
     C                   END
     C     S6YN4         IFEQ      'Y'
     C                   Z-ADD     A1PRDA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1PRDP
     C                   END
     C     S6YN5         IFEQ      'R'
     C     S6YN5         OREQ      'N'
     C                   Z-ADD     A1PIPA        A1PIPP
     C                   END
     C     S6YN5         IFEQ      'Y'
     C                   Z-ADD     A1PIPA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1PIPP
     C                   END
     C     S6YN6         IFEQ      'R'
     C     S6YN6         OREQ      'N'
     C                   Z-ADD     A1ADLA        A1ADLP
     C                   END
     C     S6YN6         IFEQ      'Y'
     C                   Z-ADD     A1ADLA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1ADLP
     C                   END
     C     S6YN7         IFEQ      'R'
     C     S6YN7         OREQ      'N'
     C                   Z-ADD     A1COLA        A1COLP
     C                   END
     C     S6YN7         IFEQ      'Y'
     C                   Z-ADD     A1COLA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1COLP
     C                   END
     C     S6YN8         IFEQ      'R'
     C     S6YN8         OREQ      'N'
     C                   Z-ADD     A1NCLA        A1NCLP
     C                   END
     C     S6YN8         IFEQ      'Y'
     C                   Z-ADD     A1NCLA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1NCLP
     C                   END
     C     S6YN9         IFEQ      'R'
     C     S6YN9         OREQ      'N'
     C                   Z-ADD     A1TOWA        A1TOWP
     C                   END
     C     S6YN9         IFEQ      'Y'
     C                   Z-ADD     A1TOWA        QQPRAT            8 0
     C                   CALL      'SRCOPR'
     C                   PARM                    QQSTDT
     C                   PARM                    QQNDDT
     C                   PARM                    QQPRAT
     C                   Z-ADD     QQPRAT        A1TOWP
     C                   END
     C                   ELSE
     C                   Z-ADD     A1SLBA        A1SLBP
     C                   Z-ADD     A1MEDA        A1MEDP
     C                   Z-ADD     A1UNMA        A1UNMP
     C                   Z-ADD     A1PRDA        A1PRDP
     C                   Z-ADD     A1PIPA        A1PIPP
     C                   Z-ADD     A1ADLA        A1ADLP
     C                   Z-ADD     A1COLA        A1COLP
     C                   Z-ADD     A1NCLA        A1NCLP
     C                   Z-ADD     A1TOWA        A1TOWP
     C                   END
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PA101
     C                   MOVE      'U'           QQUPAD            1
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A1RCDT
     C                   MOVE      'A'           A1STAT                         ## LJD 11/15
     C                   Z-ADD     *ZEROS        A1BUL3                         ## LJD 11/15
     C                   WRITE     PA101
     C                   MOVE      'A'           QQUPAD
     C                   END
     C*
     C*   DON'T UPDATE IF VEHICLE #IS ZERO
     C*
     C     *IN45         IFEQ      '0'
     C*
     C     VSKEY         CHAIN     PVS01                              40
     C*
     C                   MOVE      UPCO#         VSCO#
     C                   MOVE      SCPOL#        VSPOL#
     C                   MOVE      SCITM#        VSITM#
     C                   MOVE      SCYR          VSYR
     C                   MOVE      SCMAKE        VSMAKE
     C                   MOVE      SCMODL        VSMODL
     C                   MOVE      SCBODY        VSBODY
     C                   MOVE      SCVIN         VSVIN
     C                   MOVE      SCAMNW        VSAMNW
     C                   MOVE      SCAMST        VSAMST
     C                   MOVE      SCEXP         VSEXP
     C                   MOVE      SCTERA        VSTERA
     C                   MOVE      SCCTY         VSCTY
     C                   MOVE      SCCNTY        VSCNTY
     C                   MOVE      SCST          VSST
     C                   MOVE      SCNU          VSNU
     C                   MOVE      SCRAD         VSRAD
     C                   MOVE      SCUSE         VSUSE
     C                   MOVE      SCGVW         VSGVW
     C                   MOVE      SCADI1        VSADI1
     C                   MOVE      SCADI2        VSADI2
     C                   MOVE      SCADI3        VSADI3
     C                   MOVE      SCADI4        VSADI4
     C                   MOVE      SCADI5        VSADI5
     C                   MOVE      SCADI6        VSADI6
     C                   MOVE      SCADI7        VSADI7
     C                   MOVE      SCADI8        VSADI8
     C                   MOVE      SCADI9        VSADI9
     C                   MOVE      SCADI0        VSADI0
     C                   MOVE      *BLANK        VSNVTF
     C                   MOVE      Q1USER        VSUSER
     C                   MOVE      Q1PGM         VSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VSDT
     C                   Z-ADD     DSTIME        VSTIME
     C*
     C     *IN40         IFEQ      '0'
     C*
     C*   CHECK TO SEE IF ALL COVERAGE RECORDS ARE INACTIVE
     C*   BEFORE INACTIVATING VEHICAL
     C*
     C     SCSTA5        IFNE      VSSTAT
     C                   MOVE      'I'           XXACTV            1
     C     A1KY2         CHAIN     PA102                              60
     C     *IN60         DOWEQ     '0'
     C     A1STAT        IFEQ      'A'
     C                   MOVE      'A'           XXACTV
     C                   END
     C     A1KY2         READE     PA102                                  60
     C                   END
     C                   MOVE      XXACTV        VSSTAT
     C                   END
     C*
     C                   UPDATE    PVS01
     C                   ELSE
     C                   MOVE      'A'           VSSTAT
     C                   WRITE     PVS01
     C                   END
     C*
     C                   END
     C*
     C*   CALL PREMIUM BEARING ENDORSEMENT MAINTENANCE IF AN ENDORSEMENT RECORD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     *ZERO         XXAMT             8 0
     C     A1SLBP        ADD       A1MEDP        XXAMT
     C                   ADD       A1UNMP        XXAMT
     C                   ADD       A1PRDP        XXAMT
     C                   ADD       A1PIPP        XXAMT
     C                   ADD       A1ADLP        XXAMT
     C                   ADD       A1COLP        XXAMT
     C                   ADD       A1NCLP        XXAMT
     C                   ADD       A1TOWP        XXAMT
     C*
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      SCTRDT        QXTRDT            8
     C                   MOVE      SCEND#        QQEND#            3
     C                   MOVE      XXAMT         QQAMT             8 0
     C                   CALL      'C0281R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QXTRDT
     C                   PARM                    QQEND#
     C                   PARM                    QQAMT
     C                   PARM                    QQUPAD
     C*
     C                   END
     C*
     C*
     C     SCR05B        TAG
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   MOVE      *BLANKS       SCITM#
     C                   MOVE      *BLANKS       SCCLAS
     C                   MOVE      *BLANKS       SCTRNS
     C                   Z-ADD     DSDATE        SCTRDT
     C                   MOVE      'N'           SCUPRV
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     OPA101     E            PA101E
     OPAS01     E            PAS01E
     OPAU01     E            PAU01E
     OPVS01     E            PVS01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
**         1         2         3         4         5         6         7
5AF1=End of job       F2=Prior screen       F3=Restart
5BF1=Initial program     F2=Prior screen     F3=Restart
5CF1=End of job          F2=Prior screen          F3=Restart            F24-More
51F1=Initial program         F2=Prior screen         F3=Restart         F24-More
52F5=Territory search       F6=Copy vehicle       F7=Add addl int       F24-More
53F9=Addl int srch        F10=Table srch        F15=OTC cov srch        F24-More
54F17=Coll cov srch       F20=Chg stat       F21=Display prorate        F24-More
55F23=Delete vehicle                                                    F24-More
