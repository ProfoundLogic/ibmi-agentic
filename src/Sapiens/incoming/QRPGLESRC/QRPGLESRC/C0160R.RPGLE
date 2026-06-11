     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0160R - A/R CASH AND ADJUSTMENT POSTING
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program posts cash and/or adjustments to the      **
     H**     A/R File.                                              **
     H**   CALLED FROM:                                             **
     H**     MENU                                                   **
     H**   CALLS TO:                                                **
     H**     C0180R - A/R Inquiry                                   **
     H**     C0530R - Policy Master Policy Number Search            **
     H**     C0531R - Policy Master Named Insured Search            **
     H**     C0650R - Policy Coverage Search                        **
     H**     C2161R - A/R Transaction Code Search                   **
     H**     SRDCK  - Date Check Subroutine                         **
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
     H**      09       Inquiry Only                                 **
     H**      14       SCR02 - Invalid A/R Amount - can't be zero   **
     H**      15       SCR02 - Invalid A/R transaction code         **
     H**      16       SCR02 - Invalid section code                 **
     H**      17       SCR02 - Inactive status                      **
     H**      18       SCR02 - Position cursor                      **
     H**      19       SCR02 - Invalid Date                         **
     H**      20       SCR01 - Invalid Policy Number                **
     H**      20       SCR02 - Invalid line  1 entry                **
     H**      21       SCR02 - Invalid line  2 entry                **
     H**      22       SCR02 - Invalid line  3 entry                **
     H**      23       SCR02 - Invalid line  4 entry                **
     H**      24       SCR02 - Invalid line  5 entry                **
     H**      25       SCR02 - Invalid line  6 entry                **
     H**      26       SCR02 - Invalid line  7 entry                **
     H**      27       SCR02 - Invalid line  8 entry                **
     H**      28       SCR02 - Invalid line  9 entry                **
     H**      29       SCR02 - Invalid line 10 entry                **
     H**      30       SCR02 - 1st error for entry                  **
     H**      31       SCR02 - 2nd error for entry                  **
     H**      50       General Usage                                **
     H**      60       General Usage                                **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   CACTL  (PCT01): Commission Truth Table (CO#)             **
     H**   CDARP  (PAR01): A/R File                                 **
     H**   CMAUL  (PAU01): Auto Coverage Master(CO#/POL#/CC/...     **
     H**   CMPML  (PPM01): Policy Master File (CO#/POL#)            **
     H**   CMP1L  (PPC01): Policy Coverage File (CO#/POL#/CC)       **
     H**   CMUPL  (PUP01): User Paramters File (USR)                **
     H**   CTABL         : Table Files (CO#/KEY)                    **
     H**          (PAC01): A/R Transaction Type Code Table          **  01/15/95
     H**          (PBC01): Billing Code Table                       **  01/15/95
     H**          (PCV01): Section Code Table                       **  01/15/95
     H**   SMCOL  (PC001): System Company Master File (CO#)         **
     H****************************************************************
     FC0160F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCDARP     O    E           K DISK
     FCACTL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMP1L     IF   E           K DISK
     FCMAUL     IF   E           K DISK
     F                                     IGNORE(PPC01)
     FCTABL     IF   E           K DISK
     F                                     IGNORE(PPC01)
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D CT              S              1    DIM(9) CTDATA PERRCD(9)
     D PT              S              1    DIM(10) ASCEND
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  TC                     1     20
     D                                     DIM(10) ASCEND
     D  SCTC01                 1      2
     D  SCTC02                 3      4
     D  SCTC03                 5      6
     D  SCTC04                 7      8
     D  SCTC05                 9     10
     D  SCTC06                11     12
     D  SCTC07                13     14
     D  SCTC08                15     16
     D  SCTC09                17     18
     D  SCTC10                19     20
     D                 DS
     D  CC                     1     20
     D                                     DIM(10) ASCEND
     D  SCCC01                 1      2
     D  SCCC02                 3      4
     D  SCCC03                 5      6
     D  SCCC04                 7      8
     D  SCCC05                 9     10
     D  SCCC06                11     12
     D  SCCC07                13     14
     D  SCCC08                15     16
     D  SCCC09                17     18
     D  SCCC10                19     20
     D                 DS
     D  PD                     1    080  0
     D                                     DIM(10) ASCEND
     D  SCPD01               001    008  0
     D  SCPD02               009    016  0
     D  SCPD03               017    024  0
     D  SCPD04               025    032  0
     D  SCPD05               033    040  0
     D  SCPD06               041    048  0
     D  SCPD07               049    056  0
     D  SCPD08               057    064  0
     D  SCPD09               065    072  0
     D  SCPD10               073    080  0
     D                 DS
     D  DC                     1    300
     D                                     DIM(10) ASCEND
     D  SCDC01                 1     30
     D  SCDC02                31     60
     D  SCDC03                61     90
     D  SCDC04                91    120
     D  SCDC05               121    150
     D  SCDC06               151    180
     D  SCDC07               181    210
     D  SCDC08               211    240
     D  SCDC09               241    270
     D  SCDC10               271    300
     D                 DS
     D  CK                     1     70  0
     D                                     DIM(10) ASCEND
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D  SCCK04                22     28  0
     D  SCCK05                29     35  0
     D  SCCK06                36     42  0
     D  SCCK07                43     49  0
     D  SCCK08                50     56  0
     D  SCCK09                57     63  0
     D  SCCK10                64     70  0
     D                 DS
     D  AM                     1     90  2
     D                                     DIM(10) ASCEND
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
     D                 DS
     D  TTC                    1      9
     D                                     DIM(9)
     D  CTADJC                 1      1
     D  CTBILC                 2      2
     D  CTAUDC                 3      3
     D  CTDPBC                 4      4
     D  CTIDPC                 5      5
     D  CTNSFC                 6      6
     D  CTPAYC                 7      7
     D  CTREFC                 8      8
     D  CTVODC                 9      9
     D                 DS
     D  TTD                    1      9
     D                                     DIM(9)
     D  CTADJD                 1      1
     D  CTBILD                 2      2
     D  CTAUDD                 3      3
     D  CTDPBD                 4      4
     D  CTIDPD                 5      5
     D  CTNSFD                 6      6
     D  CTPAYD                 7      7
     D  CTREFD                 8      8
     D  CTVODD                 9      9
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
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  POLICY FORMS MASTER FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXCC              2
     C*
     C*  POLICY AUTO FORMS MASTER FILE
     C*
     C     AUKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXCC
     C*
     C*  COMMISSION TRUTH TABLE WORK FILE
     C*
     C     CTKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     TBKY1         KLIST
     C                   KFLD                    UPCO#
     C*
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        SCPOL#
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
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#           13
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
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME, SCR01
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
     C     SCPOL#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 09 = A/R INQUERY
     C*
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  THE POLICY NUMBER CANNOT BE BLANK
     C*
     C     PMKEY         CHAIN     PPM01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0180R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 10 = TABLE SEARCH FOR TRANSACTION CODE AND
     C*   COVERAGE CODE - SCREEN 2
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   Z-ADD     1             Z                 2 0
     C     '? '          LOOKUP    TC(Z)                                  50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVE      *BLANK        QQTRCD           10
     C                   CALL      'C2161R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTRCD
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQTRCD        TC(Z)
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     QQTRCD        OREQ      *BLANK
     C     Z             ADD       19            Q                 2 0
     C                   MOVE      '1'           *IN(Q)
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN18
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     Z             ADD       19            Q
     C                   MOVE      '1'           *IN(Q)
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN18
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   Z-ADD     1             Z                 2 0
     C     '? '          LOOKUP    CC(Z)                                  50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      *BLANK        QQCC              2
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0650R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQRTC             1
     C                   MOVE      QQCC          CC(Z)
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     CC(Z)         OREQ      *BLANK
     C     Z             ADD       19            Q                 2 0
     C                   MOVE      '1'           *IN(Q)
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN18
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     Z             ADD       19            Q
     C                   MOVE      '1'           *IN(Q)
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN18
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C*
     C                   GOTO      STRT
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
     C*  THE POLICY NUMBER CANNOT BE BLANK
     C*
     C     PMKEY         CHAIN     PPM01                              20
     C*
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*   LOAD BLANKS ONTO SCREEN
     C*
     C                   MOVE      *BLANKS       TC
     C                   MOVE      *BLANKS       CC
     C                   MOVE      *BLANKS       PD
     C                   MOVE      *BLANKS       DC
     C                   MOVE      *BLANKS       CK
     C                   MOVE      *BLANKS       AM
     C                   Z-ADD     *DATE         SCTRDT
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C     TC(X)         IFNE      *BLANKS
     C     CC(X)         ORNE      *BLANKS
     C     PD(X)         ORNE      0
     C     DC(X)         ORNE      *BLANKS
     C     CK(X)         ORNE      0
     C     AM(X)         ORNE      0
     C*
     C*   CHECK A/R TRANSACTION CODE
     C*
     C     X             ADD       19            A                 2 0
     C                   MOVEL     TC(X)         XXKEY
     C     TBKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN15
     C                   GOTO      STRT
     C                   END
     C*
     C     ACSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN17
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      ACTYPE        PT(X)
     C*
     C*   VALIDATE COVERAGE CODE
     C*
     C                   Z-ADD     1             ZQ                2 0
     C     X             ADD       19            C                 2 0
     C     CC(X)         LOOKUP    SC(ZQ)                                 50
     C     *IN50         IFNE      '1'
     C     CC(X)         ANDNE     COPFSC                                       POLICY FEE SECTION C
     C     CC(X)         OREQ      *BLANKS
     C                   MOVE      '1'           *IN(C)
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN16
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     CC(X)         XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     CVSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN(C)
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN17
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE POSTING DATE
     C*
     C     PD(X)         IFNE      *ZERO
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     PD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         PD(X)
     C     SRERR         IFEQ      '1'
     C     X             ADD       19            E                 2 0
     C                   MOVE      '1'           *IN(E)
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN19
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     DC(X)         IFEQ      *BLANK
     C                   MOVEL     ACDESC        DC(X)
     C                   END
     C*
     C*   CHECK A/R AMOUNT - CAN'T BE ZERO
     C*
     C     X             ADD       19            F                 2 0
     C     AM(X)         IFEQ      *ZERO
     C                   MOVE      '1'           *IN(F)
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN14
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     10            X
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C*  DETERMINE IF ACCOUNT CURRENT
     C*
     C                   MOVEL     PMBLCD        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     BCACDB        IFEQ      'C'
     C                   MOVE      'Y'           XXACYN            1
     C                   ELSE
     C                   MOVE      ' '           XXACYN
     C                   END
     C*
     C*   ONLY WRITE RECORD WITH VALID TRANSACTION CODE
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C     *BLANK        LOOKUP    TC(X)                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      UPCO#         ARCO#
     C                   MOVE      SCPOL#        ARPOL#
     C                   MOVE      CC(X)         ARCC
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C     PD(X)         IFEQ      *ZERO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   ELSE
     C                   Z-ADD     PD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   END
     C*
     C                   MOVE      TC(X)         ARPYCD
     C                   MOVE      PT(X)         ARPYTY
     C                   MOVE      DC(X)         ARDESC
     C                   MOVE      CK(X)         ARCHK#
     C                   MOVE      XXACYN        ARACYN
     C                   MOVE      *ZERO         ARAGPC
     C                   MOVE      PMAGCO        ARACC#
     C*
     C*   CHECK FOR POLICY FEE GENERATE COMMISSION IF NOT
     C*
     C     CC(X)         IFNE      COPFSC
     C*
     C                   Z-ADD     *ZERO         ARAMT
     C                   Z-ADD     *ZERO         ARCOMA
     C*
     C                   Z-ADD     AM(X)         ARGAMT
     C                   MOVE      ARACYN        SRCDF             1
     C                   MOVE      ARPYTY        SRTYPE            1
     C                   EXSR      SRCD
     C     SRCOM         IFEQ      'Y'
     C*   GET POLICY FORMS MASTER FILE OR AUTO FORMS FILE FOR
     C*   COMMISSION RATE
     C                   MOVEL     CC(X)         XXCC
     C                   MOVEL     CC(X)         XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C     CVVAL         IFEQ      'A'
     C     AUKEY         CHAIN     PAU01                              60
     C                   Z-ADD     AUCMNR        ARAGPC
     C                   ELSE
     C     PCKEY         CHAIN     PPC01                              60
     C                   Z-ADD     PCCMNR        ARAGPC
     C                   END
     C     ARAGPC        DIV(H)    100           XXWK44            4 4
     C     ARGAMT        MULT(H)   XXWK44        ARCOMA
     C                   END
     C     SRCDF         IFEQ      'Y'
     C     ARGAMT        SUB       ARCOMA        ARAMT
     C                   ELSE
     C                   Z-ADD     ARGAMT        ARAMT
     C                   END
     C*
     C                   ELSE
     C* POLICY FEE TYPE
     C                   Z-ADD     AM(X)         ARGAMT
     C                   Z-ADD     AM(X)         ARAMT
     C                   Z-ADD     *ZERO         ARCOMA
     C                   END
     C*
     C                   MOVE      'A'           ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   Z-ADD     DSTIME        ARTIME
     C*
     C                   WRITE     PAR01
     C*
     C                   ELSE
     C                   Z-ADD     10            X
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*   SUBROUTINE TO DETERMINE IF COMMISSIONS NEED TO BE GENERATED
     C*
     C     SRCD          BEGSR
     C*
     C*   CHAIN OUT TO COMMISSION TRUTH TABLE AND GET VALUE
     C*
     C     CTKEY         CHAIN     PCT01                              60
     C*
     C                   Z-ADD     1             XZ                1 0
     C     SRTYPE        LOOKUP    CT(XZ)                                 50
     C     *IN50         IFEQ      '1'
     C*
     C     SRCDF         IFEQ      'Y'
     C                   MOVE      TTC(XZ)       SRCOM             1
     C                   ELSE
     C                   MOVE      TTD(XZ)       SRCOM
     C                   END
     C*
     C                   END
     C                   ENDSR
     C*
     C*
** CT A/R TYPE CODES
ABDHINPRV
