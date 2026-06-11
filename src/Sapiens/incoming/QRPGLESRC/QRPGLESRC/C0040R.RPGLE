     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0040R - COVERAGE MAINTENANCE - FIRE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program _______________________                   **
     H**   CALLED FROM:                                             **
     H**     C0281R - PREMIUM ENDORSEMENT ENTRY                     **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/19/01 C:15  Project: 94CAX                                 *
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
     H**   _____  (_____): __________________________ (___)         **
     H****************************************************************
     H*
     F*
     FC0040F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMFAL     UF A E           K DISK
     FCMFAL1    IF   E           K DISK
     F                                     RENAME(PFA01:PFA02)
     FCMLOL     IF   E           K DISK
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
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  SL                     1     12
     D                                     DIM(4) ASCEND
     D  SCSUB1                 1      3
     D  SCSUB2                 4      6
     D  SCSUB3                 7      9
     D  SCSUB4                10     12
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
     C*  FIRE & ALLIED COV FILE
     C*
     C     FAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#           13
     C                   KFLD                    SCCC              2
     C                   KFLD                    SCLOC#            3 0
     C                   KFLD                    SCPRM#            3 0
     C                   KFLD                    SCCOVF            1
     C                   KFLD                    SCTRNS            2
     C                   KFLD                    XXTRDT            7 0
     C*
     C     FAKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#           13
     C                   KFLD                    SCCC              2
     C                   KFLD                    SCLOC#            3 0
     C                   KFLD                    SCPRM#            3 0
     C                   KFLD                    SCCOVF            1
     C                   KFLD                    XXTRDT
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  LOCATION - PREMISES FILE
     C*
     C     LOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C*
     C*  COVERAGE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
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
     C*                                                    SEP. ON ERR
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME - IF CALLED FROM
     C*   POLICY MAINT PGM, GO TO POLICY FORM MAINT & NEVER ALLOW SCR01
     C*   TO EXECUTED.
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
     C                   MOVE      *BLANK        SCCOVF
     C*** Begin dlt *** 09/19/01 **************************************
     C******************** MOVE *BLANKS   SCTRNS
     C******************** Z-ADDUPPRDT    SRDTI
     C*** End dlt ***** 09/19/01 **************************************
     C*** Begin add *** 09/19/01 **************************************
     C                   MOVE      'NB'          SCTRNS
     C                   Z-ADD     PMEFDT        SRDTI
     C*** End add ***** 09/19/01 **************************************
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
     C                   MOVE      *BLANKS       QQCOVF            1
     C                   MOVE      *BLANKS       QQTRNS            2
     C                   MOVE      *BLANKS       QQTRDT            8
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0540R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQCOVF
     C                   PARM                    QQTRNS
     C                   PARM                    QQTRDT
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQCC          SCCC
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C                   MOVE      QQCOVF        SCCOVF
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
     C*  FUNCTION 05 = TERRITORY CODE SEARCH - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERF        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0730R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCTERF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C*                    MOVE '1'       *IN46
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
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
     C*  FUNCTION 07 = CLASS CODE SEARCH - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCLAS        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0740R'
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
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 09 = POLICY COVERAGE SEARCH -SCR01
     C*                LOCATION PREMISIS SEARCH -SCR02
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
     C*  FUNCTION 09 = SUBLINE SEARCH - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   Z-ADD     1             Q                 2 0
     C     '?  '         LOOKUP    SL(Q)                                  50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0750R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SL(Q)
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C     Q             ADD       79            R                 2 0
     C*                    MOVE '1'       *IN,R
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     Q             ADD       83            R                 2 0
     C                   MOVE      '1'           *IN(R)
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
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVEL     SCDECD        XXQ               2
     C     XXQ           IFEQ      '? '
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
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN36
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
     C*
     C                   GOTO      STRT
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
     C     CVVAL         IFNE      'C'
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
     C     SCCOVF        ANDEQ     *BLANK
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
     C     FAKEY         CHAIN     PFA01                              40
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
     C*   VALIDATE COVERAGE CODE - MUST BE BETWEEN '1' & '9'
     C*
     C     SCCOVF        IFLT      '1'
     C     SCCOVF        ORGT      '9'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  TRANSACTION CODE MUST BE VALID AND ACTIVE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              23
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     TTSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN26
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
     C                   END
     C*
     C*   ALL EDITS PASSED, GET THE REQUESTED RECORD, LOAD INFORMATION
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C     FAKEY         CHAIN     PFA01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C*** Begin dlt *** 09/19/01 **************************************
     C*   VALIDATE THAT TRANS DATE IS EQUAL TO USER PARM PROCESS DATE
     C*   IF ADDING A RECORD OF COVERAGE
     C**********
     C********** XXTRDT    IFNE UPPRDT
     C**********           MOVE '1'       *IN36
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 09/19/01 **************************************
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN61
     C*
     C*  CASE MUST ALREADY EXIST IF THIS IS IN INQUIRE MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     FAKY1         SETGT     PFA02
     C                   MOVE      'I'           FASTAT
     C                   MOVE      '0'           *IN40
     C     FASTAT        DOWEQ     'I'
     C     *IN40         ANDEQ     '0'
     C                   READP     PFA02                                  40
     C     *IN40         IFEQ      *OFF
     C     FACO#         IFNE      UPCO#
     C     FAPOL#        ORNE      SCPOL#
     C     FACC          ORNE      SCCC
     C     FALOC#        ORNE      SCLOC#
     C     FAPRM#        ORNE      SCPRM#
     C     FACOVF        ORNE      SCCOVF
     C                   MOVE      *ON           *IN40
     C                   END
     C                   END
      *
     C                   END
     C*       SET MODE TO 'ADD'. CLEAR PREMIUM FIELDS & MOVE CORRERT
     C*       END# INTO FAEND# FOR ADD MODE.
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       FAPRM1
     C                   MOVE      *BLANK        FAPRM2
     C                   MOVE      *BLANK        FAPRM3
     C                   MOVE      *BLANK        FAPRM4
     C                   MOVE      UMONTH        FAACMY
     C                   MOVE      UYEAR         CYR               3 0
     C     *YEAR         IFGT      1999
     C                   MOVEL     1             CYR
     C                   END
     C                   MOVEL     CYR           FAACMY
     C                   Z-ADD     PMNEND        FAEND#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN61
     C                   EXCEPT    PFA01E
     C                   MOVE      UA(2)         SCMODE
     C                   END
     C*
     C     *IN40         IFEQ      '0'
     C*
      * CYYMM -> MMYYYY
     C                   Z-ADD     FAACMY        DSACMY
     C                   MOVEL     DSACMM        SCACMM
     C                   Z-ADD     DSACYY        SCACYY
     C     DSACCN        IFEQ      1
     C                   ADD       2000          SCACYY
     C                   ELSE
     C                   ADD       1900          SCACYY
     C                   END
      *
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     FAEND#        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      FATERF        SCTERF
     C                   MOVE      FASUB1        SCSUB1
     C                   MOVE      FAPRM1        SCPRM1
     C                   MOVE      FASUB2        SCSUB2
     C                   MOVE      FAPRM2        SCPRM2
     C                   MOVE      FASUB3        SCSUB3
     C                   MOVE      FAPRM3        SCPRM3
     C                   MOVE      FASUB4        SCSUB4
     C                   MOVE      FAPRM4        SCPRM4
     C                   MOVE      FACLAS        SCCLAS
     C                   MOVE      FADECD        SCDECD
     C                   MOVE      FAEXP         SCEXP
     C                   MOVE      FACOIN        SCCOIN
     C                   MOVE      FARCBF        SCRCBF
     C                   MOVE      FARCPF        SCRCPF
     C                   MOVE      FARCSF        SCRCSF
     C                   MOVE      FAINFG        SCINFG
     C                   MOVE      FAMLIN        SCMLIN
     C                   MOVE      FAMXIF        SCMXIF
     C                   MOVE      FAEXPI        SCEXPI
     C                   Z-ADD     FAAGVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAGVD
     C                   MOVE      FARTVA        SCRTVA
     C                   MOVE      FAOUTS        SCOUTS
     C                   MOVE      FAECZN        SCECZN
     C                   MOVE      FAWDEX        SCWDEX
     C                   MOVE      FASTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     PMNEND        SCEND#
     C                   ELSE
     C                   Z-ADD     0             SCEND#
     C                   END
     C                   MOVE      *BLANK        SCCLAS
     C                   MOVE      *BLANK        SCTERF
     C                   MOVE      *BLANK        SCSUB1
     C                   MOVE      *BLANK        SCPRM1
     C                   MOVE      *BLANK        SCSUB2
     C                   MOVE      *BLANK        SCPRM2
     C                   MOVE      *BLANK        SCSUB3
     C                   MOVE      *BLANK        SCPRM3
     C                   MOVE      *BLANK        SCSUB4
     C                   MOVE      *BLANK        SCPRM4
     C                   MOVE      *BLANK        SCEXP
     C                   MOVE      *BLANK        SCAGVD
     C                   MOVE      *BLANK        SCCOIN
     C                   MOVE      '00'          SCDECD
     C                   MOVE      'N'           SCRCBF
     C                   MOVE      'N'           SCRCPF
     C                   MOVE      'N'           SCRCSF
     C                   MOVE      *ZERO         SCINFG
     C                   MOVE      *BLANK        SCMLIN
     C                   MOVE      'N'           SCMXIF
     C                   MOVE      *BLANK        SCEXPI
     C                   MOVE      *BLANK        SCSUB1
     C                   MOVE      *BLANK        SCPRM1
     C                   MOVE      *BLANK        SCSUB2
     C                   MOVE      *BLANK        SCPRM2
     C                   MOVE      *BLANK        SCSUB3
     C                   MOVE      *BLANK        SCPRM3
     C                   MOVE      *BLANK        SCSUB4
     C                   MOVE      *BLANK        SCPRM4
     C                   MOVEL     UMONTH        SCACMM
     C                   MOVE      *YEAR         SCACYY
      *
     C                   MOVE      'N'           SCRTVA
     C                   MOVE      'N'           SCOUTS
     C                   MOVE      *BLANKS       SCECZN
     C                   MOVE      'N'           SCWDEX
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
     C*   VALIDATE TERRITORY CODE
     C*
     C*** Begin add *** 09/19/01 **************************************
     C     SCTERF        IFNE      *BLANKS
     C*** End add ***** 09/19/01 **************************************
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERF        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0730R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN45
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/19/01 **************************************
     C                   END
     C*** End add ***** 09/19/01 **************************************
     C*
     C*   VALIDATE CLASS CODE
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCLAS        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0740R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE AGREED VALIDATION DATE
     C*
     C     SCAGVD        IFNE      *ZERO
     C                   Z-ADD     SCAGVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAGVD
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE DEDUCTIBLE CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDECD        XXKEY
     C     TBKEY         CHAIN     PDE01                              35
     C     *IN35         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C*   REPLACEMENT COST, BUILDING MUST BE Y OR N.
     C*
     C     SCRCBF        IFNE      'Y'
     C     SCRCBF        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   REPLACEMENT COST, PROPERTY MUST BE Y OR N.
     C*
     C     SCRCPF        IFNE      'Y'
     C     SCRCPF        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   REPLACEMENT COST, STOCK MUST BE Y OR N.
     C*
     C     SCRCSF        IFNE      'Y'
     C     SCRCSF        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   INFLATION GUARD % MUST BE BETWEEN 0 & 100
     C*
     C     SCINFG        IFLT      0
     C     SCINFG        ORGT      100
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   MONTHLY LIMIT INDEMNITY % MUST BE BETWEEN 0 & 100
     C*
     C     SCMLIN        IFLT      0
     C     SCMLIN        ORGT      100
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   MAX PER OF INDEMNITY MUST BE Y OR N.
     C*
     C     SCMXIF        IFNE      'Y'
     C     SCMXIF        ANDNE     'N'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*   EXT PER OF INDEMNITY % MUST BE BETWEEN 0 & 100
     C*
     C     SCEXPI        IFLT      0
     C     SCEXPI        ORGT      100
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   RADIO/TV ANTENNA MUST BE "Y" OR "N".
     C*
     C     SCRTVA        IFNE      'Y'
     C     SCRTVA        ANDNE     'N'
     C                   MOVE      '1'           *IN47
     C                   GOTO      STRT
     C                   END
     C*
     C*   OUTSIDE SIGN MUST BE "Y" OR "N".
     C*
     C     SCOUTS        IFNE      'Y'
     C     SCOUTS        ANDNE     'N'
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C*
     C*   EC ZONE MUST BE "A ", " A", "AB", "B ", OR " B";
     C*
     C     SCECZN        IFNE      'A '
     C     SCECZN        ANDNE     ' A'
     C     SCECZN        ANDNE     'AB'
     C     SCECZN        ANDNE     'B '
     C     SCECZN        ANDNE     ' B'
     C*** Begin add *** 09/19/01 **************************************
     C     SCECZN        ANDNE     *BLANKS
     C*** End add ***** 09/19/01 **************************************
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   END
     C*
     C*   WIND EXCLUSION MUST BE "Y" OR "N".
     C*
     C     SCWDEX        IFNE      'Y'
     C     SCWDEX        ANDNE     'N'
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C*   SUBLINE ISO & PREMIUMS ARE MUTUALLY INCLUSIVE
     C*
     C     SCSUB1        IFEQ      *BLANKS
     C     SCPRM1        ANDNE     0
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C     SCSUB2        IFEQ      *BLANKS
     C     SCPRM2        ANDNE     0
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     SCSUB3        IFEQ      *BLANKS
     C     SCPRM3        ANDNE     0
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     SCSUB4        IFEQ      *BLANKS
     C     SCPRM4        ANDNE     0
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SUBLINE
     C*
     C                   Z-ADD     1             Q                 2 0
     C     Q             DOWLE     4
     C     *BLANK        LOOKUP    SL(Q)                              50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SL(Q)         QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0750R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     Q             ADD       40            R                 2 0
     C                   MOVE      '1'           *IN(R)
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     4             Q
     C                   END
     C*
     C                   ADD       1             Q
     C                   END
     C*
     C*   MONTH/YEAR MUST BE VALID
     C*
     C                   MOVE      SCACMM        XXMNTH            2 0
     C                   MOVE      SCACYY        XXYEAR            4 0
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
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     FAKEY         CHAIN     PFA01                              40
     C                   MOVE      UPCO#         FACO#
     C                   MOVE      SCPOL#        FAPOL#
     C                   MOVE      SCCC          FACC
     C                   MOVE      SCLOC#        FALOC#
     C                   MOVE      SCPRM#        FAPRM#
     C                   MOVE      SCTRNS        FATRNS
     C                   MOVE      SCCOVF        FACOVF
     C*****
     C                   CLEAR                   DSACMY
     C                   MOVE      SCACMM        DSACMM
     C                   MOVE      SCACYY        DSACYY
     C     SCACYY        IFGT      1999
     C                   MOVEL     1             DSACCN
     C                   ELSE
     C                   MOVEL     0             DSACCN
     C                   END
     C                   Z-ADD     DSACMY        FAACMY
      *
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FATRDR
     C                   MOVE      SCEND#        FAEND#
     C                   MOVE      SCTERF        FATERF
     C                   MOVE      SCSUB1        FASUB1
     C                   MOVE      SCPRM1        FAPRM1
     C                   MOVE      SCSUB2        FASUB2
     C                   MOVE      SCPRM2        FAPRM2
     C                   MOVE      SCSUB3        FASUB3
     C                   MOVE      SCPRM3        FAPRM3
     C                   MOVE      SCSUB4        FASUB4
     C                   MOVE      SCPRM4        FAPRM4
     C                   MOVE      SCCLAS        FACLAS
     C                   MOVE      SCDECD        FADECD
     C                   MOVE      SCEXP         FAEXP
     C                   MOVE      SCCOIN        FACOIN
     C                   MOVE      SCRCBF        FARCBF
     C                   MOVE      SCRCPF        FARCPF
     C                   MOVE      SCRCSF        FARCSF
     C                   MOVE      SCINFG        FAINFG
     C                   MOVE      SCMLIN        FAMLIN
     C                   MOVE      SCMXIF        FAMXIF
     C                   MOVE      SCEXPI        FAEXPI
     C                   Z-ADD     SCAGVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FAAGVD
     C                   MOVE      SCRTVA        FARTVA
     C                   MOVE      SCOUTS        FAOUTS
     C                   MOVE      SCECZN        FAECZN
     C                   MOVE      SCWDEX        FAWDEX
     C                   MOVE      SCSTAT        FASTAT
     C                   MOVE      Q1USER        FAUSER
     C                   MOVE      Q1PGM         FAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FADT
     C                   Z-ADD     DSTIME        FATIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PFA01
     C                   MOVE      'U'           QQUPAD            1
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FARCDT
     C                   MOVE      'A'           FASTAT                         ## LJD 11/15
     C                   Z-ADD     *ZEROS        FABUL3                         ## LJD 11/15
     C                   WRITE     PFA01
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
     C                   Z-ADD     *ZERO         XXAMT             8 0
     C     FAPRM1        ADD       FAPRM2        XXAMT
     C                   ADD       FAPRM3        XXAMT
     C                   ADD       FAPRM4        XXAMT
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
     C                   MOVE      *BLANK        SCCOVF
     C*** Begin dlt *** 09/19/01 **************************************
     C******************** MOVE *BLANKS   SCTRNS
     C******************** Z-ADDUPPRDT    SRDTI
     C*** End dlt ***** 09/19/01 **************************************
     C*** Begin add *** 09/19/01 **************************************
     C                   MOVE      'NB'          SCTRNS
     C                   Z-ADD     PMEFDT        SRDTI
     C*** End add ***** 09/19/01 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPFA01     E            PFA01E
**  TABLE UA
-- ADD --
- UPDATE -
