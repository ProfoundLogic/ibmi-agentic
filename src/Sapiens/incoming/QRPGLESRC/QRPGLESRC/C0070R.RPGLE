     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0070R - COVERAGE MAINTENANCE - INLAND MARINE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Inland Marine Coverage Maintenence **
     H**   CALLED FROM:                                             **
     H**     C0281R -  PREMIUM ENDORSEMENT ENTRY                    **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H*                                                               *
     H* 10/22/03 C:15  Project:                                       *
     H*               -Removed various edits to allow easier entry of *
     H*                policy.                                        *
     H*                                                               *
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
     H**    SMCOL   (PC001): System Company Master (CO#)            **
     H**    CMUPL   (PUP01): User Parameter (USR)                   **
     H**    CMPML   (PPM01): Policy Master (CO#/POL#)               **
     H**    CMIML   (PIM01): Inland Marine Coverage File            **
     H**    CMIML1  (PIM02)  (IMCO#,IMPOL#,IMCC,IMLOC#,IMPRM,IMTRDT)**
     H**    CMA1L   (PA101): Auto Coverage Detail File              **
     H**    CMLOL   (PLO01): Coverage Location File(CO#/POL#/LOC#/PRM#)
     H**    CDIIL   (PII01): Inland Marine Item File                **
     H**    CTABL   (     ): Table File Logical                     **
     H****************************************************************
     H*
     F*
     FC0070F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMIML     UF A E           K DISK
     FCMIML1    IF   E           K DISK
     F                                     RENAME(PIM01:PIM02)
     FCMAIL     IF   E           K DISK
     FCMLOL     IF   E           K DISK
     FCDIIL     IF   E           K DISK
     FCTABL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D  SCAI01                 1      2
     D  SCAI02                 3      4
     D  SCAI03                 5      6
     D  SCAI04                 7      8
     D  SCAI05                 9     10
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
     C*  INLAND MARINE COV FILE
     C*
     C     IMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS            6
     C                   KFLD                    SCTRNS
     C                   KFLD                    XXTRDT            7 0
     C*
     C     IMKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXTRDT
     C*
     C*  INLAND MARINE ITEM FILE
     C*
     C     IIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  ADDITIONAL INTEREST MASTER FILE
     C*
     C     AIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXADI             2
     C*
     C*  LOCATION - PREMISES FILE
     C*
     C     LOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
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
     C                   MOVE      '0'           *IN78                          TURN OFF COL
     C                   Z-ADD     *ZERO         SCACMY
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME - IF CALLED FROM
     C*   POLICY MAINT PGM, GO TO POLICY FORM MAINT & NEVER ALLOW SCR01
     C*   TO BE EXECUTED.
     C*
     C     SCPOL#        IFEQ      *BLANK
     C     SCCC          OREQ      *BLANK
     C                   MOVE      '0'           *IN76
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   MOVE      '1'           *IN76
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'PLFRM'       Q2RECF
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
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'PLFRM'
     C*
     C*  GET THE POLICY NAME FROM POLICY MASTER FILE
     C*
     C     PMKEY         CHAIN     PPM01                              60
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
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      *BLANK        QQFIN            40
     C                   MOVEL     PMFIN         QQFIN
     C                   MOVEL     PMFIN         SCFIN
     C*
     C                   MOVE      *BLANK        QQCC
     C                   MOVE      SCCC          QQCC
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0150R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQFIN
     C                   PARM                    QQCC
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      'C'
     C*
     C                   WRITE     SCR99
     C     *IN76         IFEQ      '1'                                          GO BACK TO POL.
     C                   GOTO      TERM                                         MAINT. PGM
     C                   ELSE
     C     QQRTC         IFEQ      'C'
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCC          XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C                   MOVEL     CVDESC        SCCODS
     C                   MOVE      *BLANKS       SCLOC#
     C                   MOVE      *BLANKS       SCPRM#
     C                   MOVE      *BLANK        SCCLAS
     C*** Begin dlt *** 10/22/03 **************************************
     C******************** MOVE *BLANKS   SCTRNS
     C******************** Z-ADDUPPRDT    SRDTI
     C*** End dlt ***** 10/22/03 **************************************
     C*** Begin add *** 10/22/03 **************************************
     C                   MOVE      'NB'          SCTRNS
     C                   Z-ADD     PMEFDT        SRDTI
     C*** End add ***** 10/22/03 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
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
     C                   MOVEL     'PLFRM'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 03 = RESTART - IF CALLED FROM POL. MAINT. GO TO
     C*                          POL.FORMS, OTHERWISE GOTO SCR01
     C*
     C     *INKA         IFEQ      '1'
     C     *IN76         IFEQ      '0'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     'PLFRM'       Q2RECF
     C                   GOTO      STRT
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
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 05 = COVERAGE DETAIL SEARCH - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   MOVE      *BLANKS       QQCLAS            6
     C                   MOVE      *BLANKS       QQTRNS            2
     C                   MOVE      *BLANKS       QQTRDT            8
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0570R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQCLAS
     C                   PARM                    QQTRNS
     C                   PARM                    QQTRDT
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQCC          SCCC
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C                   MOVE      QQCLAS        SCCLAS
     C                   MOVE      QQTRNS        SCTRNS
     C                   MOVE      QQTRDT        SCTRDT
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQLOC#        OREQ      *BLANK
     C     QQPRM#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR02T
     C*
     C                   END
     C*
     C*  FUNCTION 05 = ADDITIONAL INTEREST SEARCH- SCR03
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   Z-ADD     1             Z                 2 0
     C     '? '          LOOKUP    AI(Z)                                  50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      *BLANK        QQADI             2
     C                   CALL      'C0630R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQADI
     C                   PARM      ' '           QQTYPE            1
     C                   PARM                    QQRTC             1
     C                   MOVE      QQADI         AI(Z)
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     QQADI         OREQ      *BLANK
     C     Z             ADD       70            Y                 2 0
     C                   SUB       1             Y
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     Z             ADD       70            Y
     C                   MOVE      '1'           *IN(Y)
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
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME, SCR01
     C*                CALL ADDITIONAL LOCATION PREMISIS, SCR02
     C*                CALL ADDITIONAL INTEREST, SCR03
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
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  CALL ADDITIONAL LOCATION PREMISES - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   CALL      'C0140R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  CALL ADDITION INTEREST MAINTENANCE - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
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
     C     Z             ADD       69            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 09 = POLICY COVERAGE SEARCH SCR01
     C*                LOCATION PREMISIS SEARCH SCR02
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0640R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCLOC#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  TABLE FILE SEARCH  - COVERAGE CODE SCR01; TRANS. CODE SCR02;
     C*                       DEDUCTIBLE CODE SCR03.
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
     C                   MOVE      '1'           *IN32
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
     C     Q2RECF        IFEQ      'SCR02'
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
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN34
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
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVEL     SCDEDC        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCDEDC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN30
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
     C                   END
     C                   END
     C*
     C*  FUNCTION 12 = CLASS CODE SEARCH - SCREEN 2
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCLAS        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0744R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCCLAS
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   ITEM MAINTENANCE
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVEL     SCCC          QQCC              2
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   MOVE      SCCLAS        QQCLAS            6
     C                   MOVE      SCTRDT        QQTRDT            8
     C                   CALL      'C0075R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQCLAS
     C                   PARM                    QQTRDT
     C*
     C*   GET EXPOSURE
     C*
     C                   Z-ADD     0             XXEXP             8 0
     C     IIKEY         CHAIN     PII01                              50
     C     *IN50         DOWEQ     '0'
     C     IISTAT        IFEQ      'A'
     C                   ADD       IIVAL         XXEXP
     C                   END
     C     IIKEY         READE     PII01                                  50
     C                   END
     C                   Z-ADD     XXEXP         SCEXP
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 20 - CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     *IN61         ANDEQ     '0'
     C*
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C*
     C                   END
     C*
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
     C*  VALIDATE POLICY NUMBER
     C*
     C     PMKEY         CHAIN     PPM01                              20
     C*
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*  COVERAGE CODE MUST BE - VALID CODE, INLAND MARINE, & ACTIVE
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
     C     CVVAL         IFNE      'I'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     CVSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS PASSED ON SCR01 GO TO POLICY FORM MAINTENANCE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'PLFRM'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*   IF ALL FIELDS ARE BLANK ON PROMPT SCREEN(SCR02) RETURN TO
     C*   SCR01 OR POLICY MASTER MAINTENANCE.
     C*
     C     SCLOC#        IFEQ      *ZERO
     C     SCPRM#        ANDEQ     *ZERO
     C     SCCLAS        ANDEQ     *BLANK
     C     SCTRNS        ANDEQ     *BLANK
     C*          SCTRDT    ANDEQ*ZERO
     C*
     C     *IN76         IFEQ      '1'                                          GO BACK TO POL.
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
     C* Chain to find out if record exists; if the record exists
     C* do not edit screen information.  User wants to see data that
     C* may have an inactive  codes (data entered prior to inactivating
     C* codes)
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      SCCLAS        XXCLAS
     C     IMKEY         CHAIN     PIM01                              40
     C     *IN40         IFNE      '0'
     C*
     C*   VALIDATE LOCATION & PREMISES NUMBER
     C*
     C     LOKEY         CHAIN     PLO01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     LOSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE CLASS CODE
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCLAS        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0744R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
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
     C                   ELSE
     C*
     C*  GET TRANSACTION CODE TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C                   END                                                    Edit bypass
     C*
     C*
     C*   ALL EDITS PASSED, GET THE REQUESTED RECORD, LOAD INFORMATION
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C                   MOVE      SCCLAS        XXCLAS
     C     IMKEY         CHAIN     PIM01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C*** Begin dlt *** 10/22/03 **************************************
     C*   VALIDATE THAT TRANS DATE IS EQUAL TO USER PARM PROCESS DATE
     C*   IF ADDING A RECORD OF COVERAGE
     C**********
     C********** XXTRDT    IFNE UPPRDT
     C**********           MOVE '1'       *IN36
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 10/22/03 **************************************
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN61
     C*
     C*  CASE MUST ALREADY EXIST IF THIS IS IN INQUIRE MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCCLAS        XXCLAS
     C     IMKY1         SETGT     PIM02
     C                   MOVE      'I'           IMSTAT
     C                   MOVE      '0'           *IN40
     C     IMSTAT        DOWEQ     'I'
     C     *IN40         ANDEQ     '0'
     C                   READP     PIM02                                  40
      *
     C     *IN40         IFEQ      *OFF
     C     IMCO#         IFNE      UPCO#
     C     IMPOL#        ORNE      SCPOL#
     C     IMCC          ORNE      SCCC
     C     IMLOC#        ORNE      SCLOC#
     C     IMPRM#        ORNE      SCPRM#
     C     IMCLAS        ORNE      SCCLAS
     C                   MOVE      *ON           *IN40
     C                   END
     C                   END
      *
     C                   END
     C*       SET MODE TO 'ADD'. CLEAR PREMIUM FIELDS & MOVE CORRERT
     C*       END# INTO FAEND# FOR ADD MODE.
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       IMPREM
     C                   MOVE      UMONTH        IMACMY
     C                   MOVEL     UYEAR         IMACMY
     C                   Z-ADD     PMNEND        IMEND#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN61
     C                   EXCEPT    PIM01E
     C                   MOVE      UA(2)         SCMODE
     C                   END
     C*
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      IMSUBL        SCSUBL
     C                   MOVE      IMTERR        SCTERI
     C                   MOVE      IMDEDC        SCDEDC
     C                   MOVE      IMCOIN        SCCOIN
     C                   MOVE      IMEXP         SCEXP
     C                   MOVE      IMPREM        SCPREM
     C****       IMACMY    MULT 100.01    SCACMY
     C                   Z-ADD     IMACMY        DSACMY
     C                   MOVEL     DSACMM        SCACMM
     C                   Z-ADD     DSACYY        SCACYY
     C     DSACCN        IFEQ      1
     C                   ADD       2000          SCACYY
     C                   ELSE
     C                   ADD       1900          SCACYY
     C                   END
      *
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     IMEND#        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      IMAI01        SCAI01
     C                   MOVE      IMAI02        SCAI02
     C                   MOVE      IMAI03        SCAI03
     C                   MOVE      IMAI04        SCAI04
     C                   MOVE      IMAI05        SCAI05
     C                   MOVE      IMSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      '920'         SCSUBL
     C                   MOVE      '999'         SCTERI
     C*
     C*   GET FIRST RECORD IN DEDUCTIBLE TABLE FOR DEFAULT
     C*
     C     TBKY1         CHAIN     PDE01                              60
     C                   MOVEL     DEKEY         SCDEDC
     C                   MOVE      *BLANK        SCCOIN
     C                   MOVE      *BLANK        SCEXP
     C                   MOVE      *BLANK        SCPREM
     C                   MOVEL     UMONTH        SCACMY
     C                   MOVE      *YEAR         SCACYY
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     PMNEND        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      *BLANK        SCAI01
     C                   MOVE      *BLANK        SCAI02
     C                   MOVE      *BLANK        SCAI03
     C                   MOVE      *BLANK        SCAI04
     C                   MOVE      *BLANK        SCAI05
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR03T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR03B
     C*
     C*  VALIDATE DEDUCTIBLE CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDEDC        XXKEY
     C     TBKEY         CHAIN     PDE01                              21
     C     *IN21         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   MONTH/YEAR MUST BE VALID
     C*
     C                   MOVEL     SCACMY        XXMNTH            2 0
     C                   MOVE      SCACMY        XXYEAR            4 0
     C     XXMNTH        IFLT      1
     C     XXMNTH        ORGT      12
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     XXYEAR        IFLT      1
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE ADDITIONAL INTERESTS IF NOT BLANK
     C*
     C     SCAI01        IFNE      *BLANK
     C                   MOVE      SCAI01        XXADI
     C     AIKEY         CHAIN     PAI01                              25
     C     *IN25         CABEQ     '1'           STRT
     C     AISTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCAI02        IFNE      *BLANK
     C                   MOVE      SCAI02        XXADI
     C     AIKEY         CHAIN     PAI01                              26
     C     *IN26         CABEQ     '1'           STRT
     C     AISTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCAI03        IFNE      *BLANK
     C                   MOVE      SCAI03        XXADI
     C     AIKEY         CHAIN     PAI01                              27
     C     *IN27         CABEQ     '1'           STRT
     C     AISTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCAI04        IFNE      *BLANK
     C                   MOVE      SCAI04        XXADI
     C     AIKEY         CHAIN     PAI01                              28
     C     *IN28         CABEQ     '1'           STRT
     C     AISTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCAI05        IFNE      *BLANK
     C                   MOVE      SCAI05        XXADI
     C     AIKEY         CHAIN     PAI01                              29
     C     *IN29         CABEQ     '1'           STRT
     C     AISTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     IMKEY         CHAIN     PIM01                              40
     C                   MOVE      UPCO#         IMCO#
     C                   MOVE      SCPOL#        IMPOL#
     C                   MOVE      SCCC          IMCC
     C                   MOVE      SCLOC#        IMLOC#
     C                   MOVE      SCPRM#        IMPRM#
     C                   MOVE      SCCLAS        IMCLAS
     C                   MOVE      SCTRNS        IMTRNS
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IMTRDT
     C                   MOVE      SCSUBL        IMSUBL
     C                   MOVE      SCTERI        IMTERR
     C                   MOVE      SCAI01        IMAI01
     C                   MOVE      SCAI02        IMAI02
     C                   MOVE      SCAI03        IMAI03
     C                   MOVE      SCAI04        IMAI04
     C                   MOVE      SCAI05        IMAI05
     C                   MOVE      SCSTAT        IMSTAT
     C                   MOVE      Q1USER        IMUSER
     C                   MOVE      Q1PGM         IMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IMDT
     C                   Z-ADD     DSTIME        IMTIME
     C                   MOVE      SCEND#        IMEND#
     C                   MOVE      SCDEDC        IMDEDC
     C                   MOVE      SCCOIN        IMCOIN
     C                   MOVE      SCEXP         IMEXP
     C                   MOVE      SCPREM        IMPREM
      *
     C                   CLEAR                   DSACMY
     C                   MOVE      SCACYY        DSACYY
     C                   MOVEL     SCACMM        DSACMM
     C     SCACYY        IFGT      1999
     C                   MOVEL     1             DSACCN
     C                   ELSE
     C                   MOVEL     0             DSACCN
     C                   END
     C                   Z-ADD     DSACMY        IMACMY
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PIM01
     C                   MOVE      'U'           QQUPAD            1
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IMRCDT
     C                   MOVE      'A'           IMSTAT
     C                   Z-ADD     *ZEROS        IMBUL3
     C                   WRITE     PIM01
     C                   MOVE      'A'           QQUPAD
     C                   END
     C*
     C*   CALL PREMIUM BEARING ENDORSEMENT MAINTENANCE IF AN ENDORSEMENT RECORD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     IMPREM        XXAMT             8 0
     C*
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      SCTRDT        QQTRDT            8
     C                   MOVE      SCEND#        QQEND#            3
     C                   MOVE      XXAMT         QQAMT             8 0
     C                   CALL      'C0281R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQTRDT
     C                   PARM                    QQEND#
     C                   PARM                    QQAMT
     C                   PARM                    QQUPAD
     C*
     C                   END
     C*
     C     SCR03B        TAG
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *BLANKS       SCLOC#
     C                   MOVE      *BLANKS       SCPRM#
     C                   MOVE      *BLANK        SCCLAS
     C*** Begin dlt *** 10/22/03 **************************************
     C******************** MOVE *BLANKS   SCTRNS
     C******************** Z-ADDUPPRDT    SRDTI
     C*** End dlt ***** 10/22/03 **************************************
     C*** Begin add *** 10/22/03 **************************************
     C                   MOVE      'NB'          SCTRNS
     C                   Z-ADD     PMEFDT        SRDTI
     C*** End add ***** 10/22/03 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPIM01     E            PIM01E
**  TABLE UA
-- ADD --
- UPDATE -
